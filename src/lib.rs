/*
 * Copyright (c) 2023, Nguyen Duy <wuuyi123@gmail.com> All rights reserved.
 * GitHub repo: https://github.com/nomi-san/parsec-vdd/
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
pub mod display;
use crate::display::ParsecDisplay;
use std::collections::HashMap;
use std::ffi::CStr;
use std::time::SystemTime;
use windows::{
    core::{GUID, PCSTR},
    Win32::{
        Devices::DeviceAndDriverInstallation::{
            CM_Get_DevNode_Status, SetupDiDestroyDeviceInfoList, SetupDiEnumDeviceInfo,
            SetupDiEnumDeviceInterfaces, SetupDiGetClassDevsA, SetupDiGetDeviceInterfaceDetailA,
            SetupDiGetDeviceRegistryPropertyA, CM_DEVNODE_STATUS_FLAGS, CM_PROB, CM_PROB_DISABLED,
            CM_PROB_DISABLED_SERVICE, CM_PROB_FAILED_POST_START, CM_PROB_HARDWARE_DISABLED,
            CM_PROB_NEED_RESTART, CONFIGRET, DIGCF_DEVICEINTERFACE, DIGCF_PRESENT,
            DN_DRIVER_LOADED, DN_HAS_PROBLEM, DN_STARTED, SPDRP_HARDWAREID,
            SP_DEVICE_INTERFACE_DATA, SP_DEVICE_INTERFACE_DETAIL_DATA_A, SP_DEVINFO_DATA,
        },
        Foundation::{CloseHandle, GetLastError, HANDLE, INVALID_HANDLE_VALUE},
        Graphics::Gdi::{EnumDisplayDevicesA, DISPLAY_DEVICEA},
        Storage::FileSystem::{
            CreateFileA, FILE_ATTRIBUTE_NORMAL, FILE_FLAG_NO_BUFFERING, FILE_FLAG_OVERLAPPED,
            FILE_FLAG_WRITE_THROUGH, FILE_SHARE_READ, FILE_SHARE_WRITE, OPEN_EXISTING,
        },
        System::{
            Registry::{
                RegCloseKey, RegOpenKeyExA, RegQueryValueExA, HKEY, HKEY_LOCAL_MACHINE, KEY_READ,
                REG_MULTI_SZ, REG_SZ, REG_VALUE_TYPE,
            },
            Threading::CreateEventA,
            IO::{DeviceIoControl, GetOverlappedResultEx, OVERLAPPED},
        },
    },
};

// Device helper.
//////////////////////////////////////////////////

/// Device status enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum DeviceStatus {
    Ok = 0,          // Ready to use
    Inaccessible,    // Inaccessible
    Unknown,         // Unknown status
    UnknownProblem,  // Unknown problem
    Disabled,        // Device is disabled
    DriverError,     // Device encountered error
    RestartRequired, // Must restart PC to use (could ignore but would have issue)
    DisabledService, // Service is disabled
    NotInstalled,    // Driver is not installed
}

impl From<i32> for DeviceStatus {
    fn from(value: i32) -> Self {
        match value {
            0 => DeviceStatus::Ok,
            1 => DeviceStatus::Inaccessible,
            2 => DeviceStatus::Unknown,
            3 => DeviceStatus::UnknownProblem,
            4 => DeviceStatus::Disabled,
            5 => DeviceStatus::DriverError,
            6 => DeviceStatus::RestartRequired,
            7 => DeviceStatus::DisabledService,
            8 => DeviceStatus::NotInstalled,
            _ => DeviceStatus::Unknown,
        }
    }
}

/// Query the driver status.
///
/// # Arguments
/// * `class_guid` - The GUID of the class.
/// * `device_id` - The device/hardware ID of the driver.
///
/// # Returns
/// DeviceStatus indicating the current status of the device.
pub fn query_device_status(class_guid: &GUID, device_id: &str) -> DeviceStatus {
    let mut status = DeviceStatus::Inaccessible;

    let mut dev_info_data = SP_DEVINFO_DATA {
        cbSize: std::mem::size_of::<SP_DEVINFO_DATA>() as u32,
        ..Default::default()
    };

    let dev_info =
        match unsafe { SetupDiGetClassDevsA(Some(class_guid), None, None, DIGCF_PRESENT) } {
            Ok(handle) => handle,
            Err(_) => return status,
        };

    let mut found_prop = false;
    let mut device_index = 0u32;

    loop {
        if unsafe { SetupDiEnumDeviceInfo(dev_info, device_index, &mut dev_info_data) }.is_err() {
            break;
        }

        let mut required_size = 0u32;
        unsafe {
            let _ = SetupDiGetDeviceRegistryPropertyA(
                dev_info,
                &dev_info_data,
                SPDRP_HARDWAREID,
                None,
                None,
                Some(&mut required_size),
            );
        }

        if required_size > 0 {
            let mut reg_data_type = 0u32;
            let prop_buffer = unsafe {
                let layout = std::alloc::Layout::from_size_align(required_size as usize, 1)
                    .expect("Invalid layout");
                std::alloc::alloc_zeroed(layout)
            };

            if prop_buffer.is_null() {
                status = DeviceStatus::NotInstalled;
                break;
            }

            let success = unsafe {
                SetupDiGetDeviceRegistryPropertyA(
                    dev_info,
                    &dev_info_data,
                    SPDRP_HARDWAREID,
                    Some(&mut reg_data_type),
                    Some(std::slice::from_raw_parts_mut(
                        prop_buffer,
                        required_size as usize,
                    )),
                    Some(&mut required_size),
                )
                .is_ok()
            };

            if success {
                use windows::Win32::System::Registry::REG_SZ;
                let reg_sz_val = REG_SZ.0;
                let reg_multi_sz_val = REG_MULTI_SZ.0;

                if reg_data_type == reg_sz_val || reg_data_type == reg_multi_sz_val {
                    let mut found = false;
                    let mut cp = prop_buffer as *const u8;
                    let end_ptr = unsafe { prop_buffer.add(required_size as usize) };

                    loop {
                        if cp.is_null() || cp >= end_ptr || unsafe { *cp } == 0 {
                            status = DeviceStatus::NotInstalled;
                            break;
                        }

                        let current_str =
                            unsafe { CStr::from_ptr(cp as *const i8).to_string_lossy() };
                        if current_str == device_id {
                            found = true;
                            break;
                        }

                        // Calculate string length manually
                        let mut len = 0;
                        let mut p = cp;
                        while p < end_ptr && unsafe { *p } != 0 {
                            len += 1;
                            p = unsafe { p.add(1) };
                        }
                        cp = unsafe { cp.add(len + 1) };
                    }

                    if found {
                        found_prop = true;
                        let mut dev_status = CM_DEVNODE_STATUS_FLAGS(0);
                        let mut dev_problem_num = CM_PROB(0);

                        let config_ret = unsafe {
                            CM_Get_DevNode_Status(
                                &mut dev_status,
                                &mut dev_problem_num,
                                dev_info_data.DevInst,
                                0,
                            )
                        };
                        if config_ret != CONFIGRET(0) {
                            // CONFIGRET(0) is CR_SUCCESS
                            status = DeviceStatus::NotInstalled;
                            unsafe {
                                let layout =
                                    std::alloc::Layout::from_size_align(required_size as usize, 1)
                                        .expect("Invalid layout");
                                std::alloc::dealloc(prop_buffer, layout);
                            }
                            break;
                        }

                        if (dev_status.0 & (DN_DRIVER_LOADED.0 | DN_STARTED.0)) != 0 {
                            status = DeviceStatus::Ok;
                        } else if (dev_status.0 & DN_HAS_PROBLEM.0) != 0 {
                            status = match dev_problem_num {
                                p if p == CM_PROB_NEED_RESTART => DeviceStatus::RestartRequired,
                                p if p == CM_PROB_DISABLED || p == CM_PROB_HARDWARE_DISABLED => {
                                    DeviceStatus::Disabled
                                }
                                p if p == CM_PROB_DISABLED_SERVICE => DeviceStatus::DisabledService,
                                p if p == CM_PROB_FAILED_POST_START => DeviceStatus::DriverError,
                                _ => DeviceStatus::UnknownProblem,
                            };
                        } else {
                            status = DeviceStatus::Unknown;
                        }
                    }
                }
            }

            unsafe {
                let layout = std::alloc::Layout::from_size_align(required_size as usize, 1)
                    .expect("Invalid layout");
                std::alloc::dealloc(prop_buffer, layout);
            }
        }

        device_index += 1;
    }

    if !found_prop
        && unsafe { windows::Win32::Foundation::GetLastError() }
            != windows::Win32::Foundation::ERROR_SUCCESS
    {
        status = DeviceStatus::NotInstalled;
    }

    unsafe {
        let _ = SetupDiDestroyDeviceInfoList(dev_info);
    }

    status
}

/// Obtain the device handle.
/// Returns None if fails, otherwise Some(handle).
/// Should call close_device_handle to close this handle after use.
///
/// # Arguments
/// * `interface_guid` - The adapter/interface GUID of the target device.
///
/// # Returns
/// Option<HANDLE> - Some(handle) if successful, None otherwise.
pub fn open_device_handle(interface_guid: &GUID) -> Option<HANDLE> {
    let mut handle = INVALID_HANDLE_VALUE;

    let dev_info = match unsafe {
        SetupDiGetClassDevsA(
            Some(interface_guid),
            None,
            None,
            DIGCF_PRESENT | DIGCF_DEVICEINTERFACE,
        )
    } {
        Ok(h) => h,
        Err(e) => {
            eprintln!(
                "SetupDiGetClassDevsA failed: {:?}, LastError: {:?}",
                e,
                unsafe { GetLastError() }
            );
            return None;
        }
    };

    let mut dev_interface = SP_DEVICE_INTERFACE_DATA {
        cbSize: std::mem::size_of::<SP_DEVICE_INTERFACE_DATA>() as u32,
        ..Default::default()
    };

    let mut i = 0u32;
    loop {
        if unsafe {
            SetupDiEnumDeviceInterfaces(dev_info, None, interface_guid, i, &mut dev_interface)
        }
        .is_err()
        {
            break;
        }

        let mut detail_size = 0u32;
        unsafe {
            let _ = SetupDiGetDeviceInterfaceDetailA(
                dev_info,
                &dev_interface,
                None,
                0,
                Some(&mut detail_size),
                None,
            );
        }

        let detail_layout = match std::alloc::Layout::from_size_align(detail_size as usize, 1) {
            Ok(l) => l,
            Err(_) => {
                i += 1;
                continue;
            }
        };

        let detail = unsafe {
            let buf = std::alloc::alloc_zeroed(detail_layout);
            if buf.is_null() {
                i += 1;
                continue;
            }
            let detail_ptr = buf as *mut SP_DEVICE_INTERFACE_DETAIL_DATA_A;
            (*detail_ptr).cbSize = std::mem::size_of::<SP_DEVICE_INTERFACE_DETAIL_DATA_A>() as u32;
            detail_ptr
        };

        let success = unsafe {
            SetupDiGetDeviceInterfaceDetailA(
                dev_info,
                &dev_interface,
                Some(detail),
                detail_size,
                Some(&mut detail_size),
                None,
            )
            .is_ok()
        };

        if success {
            let device_path = unsafe { (*detail).DevicePath.as_ptr() };
            // GENERIC_READ = 0x80000000, GENERIC_WRITE = 0x40000000
            const GENERIC_READ: u32 = 0x80000000;
            const GENERIC_WRITE: u32 = 0x40000000;

            handle = match unsafe {
                CreateFileA(
                    PCSTR(device_path as *const u8),
                    GENERIC_READ | GENERIC_WRITE,
                    FILE_SHARE_READ | FILE_SHARE_WRITE,
                    None,
                    OPEN_EXISTING,
                    FILE_ATTRIBUTE_NORMAL
                        | FILE_FLAG_NO_BUFFERING
                        | FILE_FLAG_OVERLAPPED
                        | FILE_FLAG_WRITE_THROUGH,
                    None,
                )
            } {
                Ok(h) => h,
                Err(e) => {
                    // Log error for debugging
                    eprintln!("CreateFileA failed: {:?}", e);
                    INVALID_HANDLE_VALUE
                }
            };

            if handle != INVALID_HANDLE_VALUE {
                unsafe {
                    std::alloc::dealloc(detail as *mut u8, detail_layout);
                }
                break;
            }
        }

        unsafe {
            std::alloc::dealloc(detail as *mut u8, detail_layout);
        }
        i += 1;
    }

    unsafe {
        let _ = SetupDiDestroyDeviceInfoList(dev_info);
    }

    if handle != INVALID_HANDLE_VALUE {
        Some(handle)
    } else {
        None
    }
}

/// Release the device handle
///
/// # Arguments
/// * `handle` - The device handle to close.
pub fn close_device_handle(handle: HANDLE) {
    if handle != INVALID_HANDLE_VALUE {
        unsafe {
            let _ = CloseHandle(handle);
        }
    }
}

// Parsec VDD core.
//////////////////////////////////////////////////

/// Display name info.
pub const VDD_DISPLAY_ID: &str = "PSCCDD0"; // You will see it in registry (HKLM\SYSTEM\CurrentControlSet\Enum\DISPLAY)
pub const VDD_DISPLAY_NAME: &str = "ParsecVDA"; // You will see it in the [Advanced display settings] tab.
const EDD_GET_DEVICE_INTERFACE_NAME: u32 = 0x1;

/// Adapter GUID to obtain the device handle.
/// {00b41627-04c4-429e-a26e-0265cf50c8fa}
pub const VDD_ADAPTER_GUID: GUID = GUID::from_values(
    0x00b41627,
    0x04c4,
    0x429e,
    [0xa2, 0x6e, 0x02, 0x65, 0xcf, 0x50, 0xc8, 0xfa],
);

pub const VDD_ADAPTER_NAME: &str = "Parsec Virtual Display Adapter";

/// Class and hwid to query device status.
/// {4d36e968-e325-11ce-bfc1-08002be10318}
pub const VDD_CLASS_GUID: GUID = GUID::from_values(
    0x4d36e968,
    0xe325,
    0x11ce,
    [0xbf, 0xc1, 0x08, 0x00, 0x2b, 0xe1, 0x03, 0x18],
);

pub const VDD_HARDWARE_ID: &str = "Root\\Parsec\\VDA";

/// Actually up to 16 devices could be created per adapter
/// so just use a half to avoid plugging lag.
pub const VDD_MAX_DISPLAYS: i32 = 8;

/// Core IoControl codes
#[repr(u32)]
pub enum VddCtlCode {
    Add = 0x0022e004, // CTL_CODE(FILE_DEVICE_UNKNOWN, 0x800 + 1, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
    Remove = 0x0022a008, // CTL_CODE(FILE_DEVICE_UNKNOWN, 0x800 + 2, METHOD_BUFFERED, FILE_WRITE_ACCESS)
    Update = 0x0022a00c, // CTL_CODE(FILE_DEVICE_UNKNOWN, 0x800 + 3, METHOD_BUFFERED, FILE_WRITE_ACCESS)
    Version = 0x0022e010, // CTL_CODE(FILE_DEVICE_UNKNOWN, 0x800 + 4, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS)
}

/// Generic DeviceIoControl for all IoControl codes.
fn vdd_io_control(
    vdd: HANDLE,
    code: VddCtlCode,
    data: Option<&[u8]>,
) -> Result<u32, std::io::Error> {
    if vdd == INVALID_HANDLE_VALUE {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "Invalid handle",
        ));
    }

    let mut in_buffer = [0u8; 32];
    if let Some(data_slice) = data {
        let copy_len = data_slice.len().min(32);
        in_buffer[..copy_len].copy_from_slice(&data_slice[..copy_len]);
    }

    let event = match unsafe { CreateEventA(None, true, false, None) } {
        Ok(e) => e,
        Err(_) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Failed to create event",
            ))
        }
    };

    let mut overlapped = OVERLAPPED {
        Internal: 0,
        InternalHigh: 0,
        Anonymous: windows::Win32::System::IO::OVERLAPPED_0 {
            Anonymous: windows::Win32::System::IO::OVERLAPPED_0_0 {
                Offset: 0,
                OffsetHigh: 0,
            },
        },
        hEvent: event,
    };

    let mut out_buffer: u32 = 0;

    unsafe {
        DeviceIoControl(
            vdd,
            code as u32,
            Some(in_buffer.as_ptr() as *const _),
            in_buffer.len() as u32,
            Some(std::ptr::addr_of_mut!(out_buffer) as *mut _),
            std::mem::size_of::<u32>() as u32,
            None,
            Some(&mut overlapped),
        )
        .ok();
    }

    let mut number_of_bytes_transferred: u32 = 0;
    let result = unsafe {
        GetOverlappedResultEx(
            vdd,
            &overlapped,
            &mut number_of_bytes_transferred,
            5000,
            false,
        )
    };

    unsafe {
        let _ = CloseHandle(event);
    }

    if result.is_ok() {
        Ok(out_buffer)
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            result.err().unwrap().to_string(),
        ))
    }
}

/// Query VDD minor version.
///
/// # Arguments
/// * `vdd` - The device handle of VDD.
///
/// # Returns
/// The number of minor version, or -1 on error.
pub fn vdd_version(vdd: HANDLE) -> i32 {
    match vdd_io_control(vdd, VddCtlCode::Version, None) {
        Ok(minor) => minor as i32,
        Err(_) => -1,
    }
}

/// Update/ping to VDD.
/// Should call this function in a side thread for each
/// less than 100ms to keep all added virtual displays alive.
///
/// # Arguments
/// * `vdd` - The device handle of VDD.
pub fn vdd_update(vdd: HANDLE) -> Result<(), std::io::Error> {
    match vdd_io_control(vdd, VddCtlCode::Update, None) {
        Ok(_) => Ok(()),
        Err(e) => Err(e),
    }
}

/// Add/plug a virtual display.
///
/// # Arguments
/// * `vdd` - The device handle of VDD.
///
/// # Returns
/// The index of the added display, or -1 on error.
pub fn vdd_add_display(vdd: HANDLE) -> Result<i32, std::io::Error> {
    match vdd_io_control(vdd, VddCtlCode::Add, None) {
        Ok(idx) => {
            vdd_update(vdd)?;
            Ok(idx as i32)
        }
        Err(e) => Err(e),
    }
}

/// Add display and identify the added display
///
/// # Arguments
/// * `vdd` - The device handle of VDD.
///
/// # Returns
/// The index of the added display and the virtual display identified
pub fn vdd_add_and_identify_display(vdd: HANDLE) -> Result<(i32, ParsecDisplay), std::io::Error> {
    let current_displays = get_all_displays();
    let index = match vdd_add_display(vdd) {
        Ok(index) => index,
        Err(e) => return Err(e),
    };
    let mut new_display: Option<ParsecDisplay> = None;
    for _ in 0..50 {
        let new_displays = get_all_displays();
        let added_display = new_displays.iter().find(|d| {
            !current_displays
                .iter()
                .any(|c| c.identifier == d.identifier)
        });
        if let Some(added_display) = added_display {
            new_display = Some(added_display.clone());
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    if let Some(new_display) = new_display {
        Ok((index, new_display))
    } else {
        vdd_remove_display(vdd, index)?;
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Failed to identify the added display",
        ))
    }
}

/// Remove/unplug a virtual display.
///
/// # Arguments
/// * `vdd` - The device handle of VDD.
/// * `index` - The index of the display will be removed.
pub fn vdd_remove_display(vdd: HANDLE, index: i32) -> Result<(), std::io::Error> {
    // 16-bit BE index
    let index_data = ((index & 0xFF) << 8) | ((index >> 8) & 0xFF);
    match vdd_io_control(vdd, VddCtlCode::Remove, Some(&index_data.to_le_bytes())) {
        Ok(_) => vdd_update(vdd),
        Err(e) => Err(e),
    }
}

/// Parse display address from path
fn parse_display_address(path: &str) -> i32 {
    if let Some(index) = path.rfind("uid") {
        let addr_str = &path[index + 3..];
        addr_str.parse().unwrap_or(0)
    } else {
        0
    }
}

/// Parse display code from device ID
fn parse_display_code(id: &str) -> String {
    let tokens: Vec<&str> = id.split('#').collect();
    if tokens.len() >= 2 {
        tokens[1].to_string()
    } else {
        tokens[0].to_string()
    }
}

/// Get device instance and related information
/// This is a simplified version - the full implementation would need
/// more device manager APIs
fn get_device_info(_path: &str) -> (SystemTime, String, String, SystemTime) {
    // Simplified - would need full implementation with CM APIs
    let now = SystemTime::now();
    (now, String::new(), String::new(), now)
}

/// Get all Parsec virtual displays
pub fn get_all_displays() -> Vec<ParsecDisplay> {
    let mut display_map: HashMap<String, ParsecDisplay> = HashMap::new();
    let mut clone_groups: Vec<(usize, usize)> = Vec::new();

    let mut paths = get_display_paths();

    let mut dd = DISPLAY_DEVICEA::default();
    dd.cb = std::mem::size_of::<DISPLAY_DEVICEA>() as u32;

    let mut device_index = 0u32;
    loop {
        let success = unsafe { EnumDisplayDevicesA(None, device_index, &mut dd, 0).as_bool() };

        if !success {
            break;
        }

        let device_name = unsafe {
            CStr::from_ptr(dd.DeviceName.as_ptr() as *const i8)
                .to_string_lossy()
                .to_string()
        };
        let state_flags = dd.StateFlags;

        let device_id_first = unsafe {
            CStr::from_ptr(dd.DeviceID.as_ptr() as *const i8)
                .to_string_lossy()
                .to_string()
        };

        // Now try to enumerate interfaces for this device
        let mut dd2 = DISPLAY_DEVICEA::default();
        dd2.cb = std::mem::size_of::<DISPLAY_DEVICEA>() as u32;

        let mut interface_index = 0u32;
        let mut prev_active_display: Option<usize> = None;

        let device_name_ptr = dd.DeviceName.as_ptr() as *const u8;

        loop {
            let success2 = unsafe {
                EnumDisplayDevicesA(
                    PCSTR(device_name_ptr),
                    interface_index,
                    &mut dd2,
                    EDD_GET_DEVICE_INTERFACE_NAME.into(),
                )
                .as_bool()
            };

            if !success2 {
                if interface_index == 0 && !device_id_first.is_empty() {
                    let path_idx = paths.iter().position(|p| {
                        if !p.contains(VDD_DISPLAY_ID) {
                            return false;
                        }
                        let path_normalized = p.replace('\\', "#");
                        device_id_first.contains(&path_normalized)
                    });

                    if let Some(path_idx) = path_idx {
                        let path = paths[path_idx].clone();
                        if !display_map.contains_key(&path) {
                            let is_active = state_flags
                                .contains(windows::Win32::Graphics::Gdi::DISPLAY_DEVICE_ACTIVE);
                            let address = parse_display_address(&path);
                            let display_code = parse_display_code(&device_id_first);

                            let (last_arrival, adapter, adapter_instance, adapter_arrival) =
                                get_device_info(&path);

                            let mut display = ParsecDisplay {
                                active: is_active,
                                identifier: 0,
                                clone_of: 0,
                                address,
                                last_arrival,
                                adapter,
                                adapter_instance,
                                adapter_arrival,
                                device_name: device_name.clone(),
                                display_name: display_code,
                                current_mode: None,
                                current_orientation: display::Orientation::Landscape,
                                mode_list: Vec::new(),
                                supported_resolutions: Vec::new(),
                                hmonitor_id: None,
                            };

                            if is_active {
                                display.fetch_all_modes();
                            }

                            display_map.insert(path.clone(), display);
                            paths.remove(path_idx);
                        }
                    }
                }
                break;
            }

            let state_flags2 = dd2.StateFlags;

            if !state_flags2.contains(windows::Win32::Graphics::Gdi::DISPLAY_DEVICE_ATTACHED) {
                interface_index += 1;
                continue;
            }

            let device_id2 = unsafe {
                CStr::from_ptr(dd2.DeviceID.as_ptr() as *const i8)
                    .to_string_lossy()
                    .to_string()
            };

            let path_idx = paths.iter().position(|p| {
                if !p.contains(VDD_DISPLAY_ID) {
                    return false;
                }
                let path_normalized = p.replace('\\', "#");
                device_id2.contains(&path_normalized)
            });

            if let Some(path_idx) = path_idx {
                let path = paths[path_idx].clone();

                if !display_map.contains_key(&path) {
                    let is_active =
                        state_flags2.contains(windows::Win32::Graphics::Gdi::DISPLAY_DEVICE_ACTIVE);
                    let address = parse_display_address(&path);
                    let display_code = parse_display_code(&device_id2);

                    let (last_arrival, adapter, adapter_instance, adapter_arrival) =
                        get_device_info(&path);

                    let mut display = ParsecDisplay {
                        active: is_active,
                        identifier: 0,
                        clone_of: 0,
                        address,
                        last_arrival,
                        adapter,
                        adapter_instance,
                        adapter_arrival,
                        device_name: device_name.clone(),
                        display_name: display_code,
                        current_mode: None,
                        current_orientation: display::Orientation::Landscape,
                        mode_list: Vec::new(),
                        supported_resolutions: Vec::new(),
                        hmonitor_id: None,
                    };

                    if is_active {
                        display.fetch_all_modes();

                        if let Some(prev_idx) = prev_active_display {
                            clone_groups.push((prev_idx, display_map.len()));
                        } else {
                            prev_active_display = Some(display_map.len());
                        }
                    }

                    display_map.insert(path.clone(), display);
                    paths.remove(path_idx);
                }
            }

            interface_index += 1;
        }

        device_index += 1;
    }

    let mut displays: Vec<ParsecDisplay> = display_map.into_values().collect();

    // Sort displays by adapter arrival time
    displays.sort_by(|a, b| {
        if a.adapter_instance == b.adapter_instance {
            a.device_name.cmp(&b.device_name)
        } else {
            a.adapter_arrival
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default()
                .cmp(
                    &b.adapter_arrival
                        .duration_since(SystemTime::UNIX_EPOCH)
                        .unwrap_or_default(),
                )
        }
    });

    // Fill display identifier
    for (i, display) in displays.iter_mut().enumerate() {
        display.identifier = (i + 1) as i32;
    }

    // Fill clone of identifier
    for (idx1, idx2) in clone_groups {
        if idx1 < displays.len() && idx2 < displays.len() {
            let id1 = displays[idx1].identifier;
            let id2 = displays[idx2].identifier;
            displays[idx1].clone_of = id2;
            displays[idx2].clone_of = id1;
        }
    }

    displays
}

// update config

/// Get display paths from registry
fn get_display_paths() -> Vec<String> {
    let mut paths = Vec::new();

    let key_path = b"SYSTEM\\CurrentControlSet\\Services\\monitor\\Enum\0";
    let mut hkey: HKEY = HKEY::default();

    unsafe {
        let result = RegOpenKeyExA(
            HKEY_LOCAL_MACHINE,
            PCSTR(key_path.as_ptr()),
            None,
            KEY_READ,
            &mut hkey,
        );
        use windows::Win32::Foundation::ERROR_SUCCESS;
        if result == ERROR_SUCCESS {
            let count_name = b"Count\0";
            let mut count_value: u32 = 0;
            let mut count_type = REG_VALUE_TYPE(0);
            let mut count_size = std::mem::size_of::<u32>() as u32;

            let count_result = RegQueryValueExA(
                hkey,
                PCSTR(count_name.as_ptr()),
                None,
                Some(&mut count_type),
                Some(std::ptr::addr_of_mut!(count_value) as *mut u8),
                Some(&mut count_size),
            );
            if count_result == ERROR_SUCCESS {
                for i in 0..count_value {
                    let value_name = format!("{}\0", i);
                    let mut value_data = vec![0u8; 512];
                    let mut value_type = REG_VALUE_TYPE(0);
                    let mut value_size = value_data.len() as u32;

                    let value_result = RegQueryValueExA(
                        hkey,
                        PCSTR(value_name.as_ptr()),
                        None,
                        Some(&mut value_type),
                        Some(value_data.as_mut_ptr()),
                        Some(&mut value_size),
                    );
                    if value_result == ERROR_SUCCESS {
                        if value_type == REG_SZ {
                            if let Ok(path) =
                                CStr::from_ptr(value_data.as_ptr() as *const i8).to_str()
                            {
                                if path.contains(VDD_DISPLAY_ID) {
                                    paths.push(path.to_string());
                                }
                            }
                        }
                    }
                }
            }

            let _ = RegCloseKey(hkey);
        }
    }
    paths
}
