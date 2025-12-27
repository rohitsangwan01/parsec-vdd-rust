use clap::ValueEnum;
use std::collections::{HashMap, HashSet};
use std::time::SystemTime;
use windows::Win32::Foundation::{POINTL, RECT, TRUE};
use windows::Win32::Graphics::Gdi::{GetMonitorInfoW, HDC, MONITORINFOEXW};
use windows::{
    core::{BOOL, PCSTR},
    Win32::Foundation::LPARAM,
    Win32::Graphics::Gdi::{
        ChangeDisplaySettingsExA, EnumDisplayMonitors, EnumDisplaySettingsA, CDS_GLOBAL,
        CDS_UPDATEREGISTRY, DEVMODEA, DEVMODE_FIELD_FLAGS, DISP_CHANGE, ENUM_DISPLAY_SETTINGS_MODE,
        HMONITOR, MONITORINFO,
    },
};

/// Parsec virtual display information
#[derive(Debug, Clone)]
pub struct ParsecDisplay {
    pub active: bool,
    pub identifier: i32,
    pub clone_of: i32,
    pub address: i32,
    pub last_arrival: SystemTime,
    pub adapter: String,
    pub adapter_instance: String,
    pub adapter_arrival: SystemTime,
    pub device_name: String,
    pub display_name: String,
    pub current_mode: Option<Mode>,
    pub current_orientation: Orientation,
    pub mode_list: Vec<Mode>,
    pub supported_resolutions: Vec<ModeSet>,
    pub hmonitor_id: Option<i32>,
}

impl ParsecDisplay {
    pub fn display_index(&self) -> i32 {
        self.address - 0x100
    }

    pub fn get_hmonitor_id(&self) -> Option<i32> {
        if let Some(hmon) = self.hmonitor_id {
            return Some(hmon);
        }
        let target_device_name = self.device_name.clone();
        let h_monitors_mut_ptr: *mut Vec<HMONITOR> = Box::into_raw(Box::default());
        let h_monitors = unsafe {
            EnumDisplayMonitors(
                None,
                None,
                Some(monitor_enum_proc),
                LPARAM(h_monitors_mut_ptr as isize),
            )
            .ok()
            .unwrap_or_default();
            Box::from_raw(h_monitors_mut_ptr)
        };
        for &h_monitor in h_monitors.iter() {
            let mut monitor_info_ex_w = MONITORINFOEXW::default();
            monitor_info_ex_w.monitorInfo.cbSize = std::mem::size_of::<MONITORINFOEXW>() as u32;
            let monitor_info_ex_w_ptr =
                &mut monitor_info_ex_w as *mut MONITORINFOEXW as *mut MONITORINFO;
            let success = unsafe { GetMonitorInfoW(h_monitor, monitor_info_ex_w_ptr).as_bool() };
            if !success {
                continue;
            }
            let sz_device = &monitor_info_ex_w.szDevice;
            let mut len = 0;
            while len < sz_device.len() && sz_device[len] != 0 {
                len += 1;
            }
            let name = String::from_utf16_lossy(&sz_device[..len]);
            let id = h_monitor.0 as u32;
            if name.eq_ignore_ascii_case(&target_device_name) {
                return Some(id as i32);
            }
        }
        None
    }

    /// Change display mode (width, height, refresh rate, position, orientation)
    pub fn change_mode(
        &mut self,
        width: Option<i32>,
        height: Option<i32>,
        hz: Option<i32>,
        position: Option<(i32, i32)>,
        orientation: Option<Orientation>,
    ) -> bool {
        let device_name_cstr = std::ffi::CString::new(self.device_name.clone()).unwrap_or_default();
        let device_name_ptr = device_name_cstr.as_ptr() as *const u8;

        let mut dev_mode = DEVMODEA::default();
        dev_mode.dmSize = std::mem::size_of::<DEVMODEA>() as u16;

        // Get current display settings
        let success = unsafe {
            EnumDisplaySettingsA(
                PCSTR(device_name_ptr),
                ENUM_DISPLAY_SETTINGS_MODE(0xFFFFFFFF), // -1 for current settings
                &mut dev_mode,
            )
            .as_bool()
        };

        if !success {
            return false;
        }

        // Constants for dmFields flags
        const DM_PELSWIDTH: DEVMODE_FIELD_FLAGS = DEVMODE_FIELD_FLAGS(0x80000);
        const DM_PELSHEIGHT: DEVMODE_FIELD_FLAGS = DEVMODE_FIELD_FLAGS(0x100000);
        const DM_DISPLAYFREQUENCY: DEVMODE_FIELD_FLAGS = DEVMODE_FIELD_FLAGS(0x400000);
        const DM_POSITION: DEVMODE_FIELD_FLAGS = DEVMODE_FIELD_FLAGS(0x000020);

        // Update width if provided
        if let Some(w) = width {
            dev_mode.dmPelsWidth = w as u32;
            dev_mode.dmFields = dev_mode.dmFields | DM_PELSWIDTH;
        }

        // Update height if provided
        if let Some(h) = height {
            dev_mode.dmPelsHeight = h as u32;
            dev_mode.dmFields = dev_mode.dmFields | DM_PELSHEIGHT;
        }

        // Update refresh rate if provided
        if let Some(refresh_rate) = hz {
            dev_mode.dmDisplayFrequency = refresh_rate as u32;
            dev_mode.dmFields = dev_mode.dmFields | DM_DISPLAYFREQUENCY;
        }

        // Update position if provided
        if let Some((x, y)) = position {
            dev_mode.Anonymous1.Anonymous2.dmPosition = POINTL {
                x: x as i32,
                y: y as i32,
            };
            dev_mode.dmFields = dev_mode.dmFields | DM_POSITION;
        }

        // Update orientation if provided
        // Note: dmDisplayOrientation may not be available in DEVMODEA
        // We'll skip orientation changes for now as the field doesn't exist
        if let Some(new_orientation) = orientation {
            // If orientation changes from landscape to portrait (or vice versa), swap width/height
            if ((new_orientation as i32 + self.current_orientation as i32) % 2) != 0 {
                let temp = dev_mode.dmPelsWidth;
                dev_mode.dmPelsWidth = dev_mode.dmPelsHeight;
                dev_mode.dmPelsHeight = temp;
            }

            self.current_orientation = new_orientation;
        }

        // Apply the changes
        let result = unsafe {
            ChangeDisplaySettingsExA(
                PCSTR(device_name_ptr),
                Some(&mut dev_mode),
                None,
                CDS_UPDATEREGISTRY | CDS_GLOBAL,
                None,
            )
        };

        // DISP_CHANGE_SUCCESSFUL is 0
        if result == DISP_CHANGE(0) {
            if let (Some(w), Some(h)) = (width, height) {
                let new_hz = hz.unwrap_or(dev_mode.dmDisplayFrequency as i32);
                self.current_mode = Some(Mode::new(w, h, new_hz));
            }
            true
        } else {
            false
        }
    }
}

impl ParsecDisplay {
    pub fn fetch_all_modes(&mut self) {
        let device_name_cstr = std::ffi::CString::new(self.device_name.clone()).unwrap_or_default();
        let device_name_ptr = device_name_cstr.as_ptr() as *const i8;

        let mut dev_mode = DEVMODEA::default();
        dev_mode.dmSize = std::mem::size_of::<DEVMODEA>() as u16;

        let mut set: HashMap<u64, HashSet<i32>> = HashMap::new();
        let mut mode_num: u32 = 0xFFFFFFFF; // -1 as u32 for ENUM_CURRENT_SETTINGS

        loop {
            let mode_enum = ENUM_DISPLAY_SETTINGS_MODE(mode_num);

            let success = unsafe {
                EnumDisplaySettingsA(
                    PCSTR(device_name_ptr as *const u8),
                    mode_enum,
                    &mut dev_mode,
                )
                .as_bool()
            };

            if !success {
                break;
            }

            let mode = Mode {
                width: dev_mode.dmPelsWidth as i32,
                height: dev_mode.dmPelsHeight as i32,
                hz: dev_mode.dmDisplayFrequency as i32,
            };

            if mode_num == 0xFFFFFFFF {
                self.current_mode = Some(mode.clone());
                // Note: dmDisplayOrientation may not be available in DEVMODEA
                // Defaulting to Landscape for now
                self.current_orientation = Orientation::Landscape;
            } else {
                self.mode_list.push(mode.clone());

                let mut mode_without_hz = mode.clone();
                mode_without_hz.hz = 0;
                let bits = mode_without_hz.to_bits();

                set.entry(bits).or_insert_with(HashSet::new).insert(mode.hz);
            }

            if mode_num == 0xFFFFFFFF {
                mode_num = 0;
            } else {
                mode_num += 1;
            }
        }

        // Build supported resolutions
        let mut mode_sets: Vec<ModeSet> = set
            .into_iter()
            .map(|(bits, rrs)| {
                let mode = Mode::from_bits(bits);
                let mut refresh_rates: Vec<i32> = rrs.into_iter().collect();
                refresh_rates.sort();
                ModeSet {
                    width: mode.width,
                    height: mode.height,
                    refresh_rates,
                }
            })
            .collect();

        mode_sets.sort_by(|a, b| {
            if b.width == a.width {
                (b.height).cmp(&a.height)
            } else {
                (b.width).cmp(&a.width)
            }
        });

        self.supported_resolutions = mode_sets;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Orientation {
    Landscape = 0,
    Portrait,
    LandscapeFlipped,
    PortraitFlipped,
}

/// Display mode information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mode {
    pub width: i32,
    pub height: i32,
    pub hz: i32,
}

impl Mode {
    pub fn new(width: i32, height: i32, hz: i32) -> Self {
        Self { width, height, hz }
    }

    pub fn from_bits(bits: u64) -> Self {
        Self {
            width: (bits & 0xFFFF) as i32,
            height: ((bits >> 16) & 0xFFFF) as i32,
            hz: ((bits >> 32) & 0xFFFF) as i32,
        }
    }

    pub fn to_bits(&self) -> u64 {
        ((self.width & 0xFFFF) as u64)
            | (((self.height & 0xFFFF) as u64) << 16)
            | (((self.hz & 0xFFFF) as u64) << 32)
    }

    pub fn resolution(&self) -> String {
        format!("{} × {}", self.width, self.height)
    }

    pub fn refresh_rate(&self) -> String {
        format!("{} Hz", self.hz)
    }
}

/// Display mode set (resolution with multiple refresh rates)
#[derive(Debug, Clone)]
pub struct ModeSet {
    pub width: i32,
    pub height: i32,
    pub refresh_rates: Vec<i32>,
}

extern "system" fn monitor_enum_proc(
    h_monitor: HMONITOR,
    _: HDC,
    _: *mut RECT,
    state: LPARAM,
) -> BOOL {
    unsafe {
        let state = Box::leak(Box::from_raw(state.0 as *mut Vec<HMONITOR>));
        state.push(h_monitor);

        TRUE
    }
}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.resolution(), self.refresh_rate())
    }
}

impl std::fmt::Display for ParsecDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!(
            "[{}] {} ({}{}) - {}x{}@{}Hz",
            self.identifier,
            self.device_name,
            self.display_name,
            self.address,
            self.current_mode.as_ref().map(|m| m.width).unwrap_or(0),
            self.current_mode.as_ref().map(|m| m.height).unwrap_or(0),
            self.current_mode.as_ref().map(|m| m.hz).unwrap_or(0),
        );
        if self.clone_of > 0 && self.clone_of < self.identifier {
            str.push_str(&format!(" (clone of [{}])", self.clone_of));
        }
        write!(f, "{}", str)
    }
}
