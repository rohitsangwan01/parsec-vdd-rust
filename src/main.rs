use clap::{CommandFactory, Parser};
use parsec_vdd_rust::display::{Orientation, ParsecDisplay};
use parsec_vdd_rust::{
    close_device_handle, get_all_displays, open_device_handle, query_device_status,
    vdd_add_and_identify_display, vdd_remove_display, vdd_update, DeviceStatus, VDD_ADAPTER_GUID,
    VDD_CLASS_GUID, VDD_HARDWARE_ID, VDD_MAX_DISPLAYS,
};
use std::collections::HashMap;
use std::io::{self, Write};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

#[derive(Parser, Debug)]
#[command(name = "parsec-vdd-rust", version = "0.1")]
enum CliCommands {
    #[command(about = "Add a new virtual display")]
    Add,
    #[command(about = "Remove a virtual display")]
    Remove {
        #[arg(short, long)]
        index: Option<i32>,
    },
    #[command(about = "Get all virtual displays")]
    Get,
    #[command(about = "Configure a virtual display")]
    Config {
        #[arg(short, long, help = "Index of the display to configure")]
        index: i32,
        #[arg(
            short,
            long,
            help = "Size of the display to configure, format: widthxheight"
        )]
        size: Option<String>,
        #[arg(short, long, help = "Refresh rate of the display to configure")]
        refresh_rate: Option<i32>,
        #[arg(
            short,
            long,
            help = "Position of the display to configure, format: x,y"
        )]
        position: Option<String>,
        #[arg(short, long, help = "Orientation")]
        orientation: Option<Orientation>,
    },
    #[command(about = "Exit the program")]
    Exit,
}

// HANDLE is safe to use across threads on Windows, but Rust doesn't know that.
// We'll use unsafe to assert it's Send and Sync.
struct HandleWrapper(windows::Win32::Foundation::HANDLE);
unsafe impl Send for HandleWrapper {}
unsafe impl Sync for HandleWrapper {}

fn main() {
    let status = query_device_status(&VDD_CLASS_GUID, VDD_HARDWARE_ID);
    if status != DeviceStatus::Ok {
        println!("Parsec VDD device is not OK, got status {:?}.", status);
        std::process::exit(1);
    }
    println!("Parsec VDD is ready.");

    let vdd = match open_device_handle(&VDD_ADAPTER_GUID) {
        Some(handle) => handle,
        None => {
            println!("Failed to obtain the device handle.");
            std::process::exit(1);
        }
    };

    let running = Arc::new(Mutex::new(true));
    let running_clone = Arc::clone(&running);
    let vdd_wrapper = Arc::new(HandleWrapper(vdd));
    let vdd_for_thread = Arc::clone(&vdd_wrapper);
    let updater = thread::spawn(move || {
        while *running_clone.lock().unwrap() {
            if let Err(err) = vdd_update(vdd_for_thread.0) {
                println!("Error updating vdd: {err:?}");
            }
            thread::sleep(Duration::from_millis(100));
        }
    });

    let _ = CliCommands::command().print_help();

    let mut displays = HashMap::<i32, ParsecDisplay>::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            continue;
        }
        let input = input.trim();
        if input.is_empty() {
            continue;
        }
        let args = std::iter::once("").chain(input.split_whitespace());
        let command = match CliCommands::try_parse_from(args) {
            Ok(cli) => cli,
            Err(e) => {
                let _ = e.print();
                continue;
            }
        };
        match command {
            CliCommands::Add => {
                if displays.len() < VDD_MAX_DISPLAYS as usize {
                    let (index, display) = match vdd_add_and_identify_display(vdd_wrapper.0) {
                        Ok(result) => result,
                        Err(e) => {
                            println!("Add virtual display failed: {}", e);
                            continue;
                        }
                    };
                    displays.insert(index, display.clone());
                    println!(
                        "Added a new virtual display, index: {}, hmonitor_id: {}, display_name: {}",
                        index,
                        display.get_hmonitor_id().unwrap_or(-1),
                        display,
                    );
                } else {
                    println!(
                        "Limit exceeded ({}), could not add more virtual displays.",
                        VDD_MAX_DISPLAYS
                    );
                }
            }
            CliCommands::Remove { index } => {
                if let Some(index) = index.or_else(|| displays.keys().last().cloned()) {
                    if let Some(_) = displays.remove(&index) {
                        if let Err(err) = vdd_remove_display(vdd_wrapper.0, index) {
                            println!("Error removing display: {err:?}");
                        }
                        println!("Removed virtual display, index: {}.", index);
                    } else {
                        println!("No added virtual displays.");
                    }
                }
            }
            CliCommands::Get => {
                let latest_displays = get_all_displays();
                for display in latest_displays {
                    let cached_index = displays
                        .iter()
                        .find(|(_, d)| d.identifier == display.identifier)
                        .map(|(index, _)| *index);
                    if let Some(index) = cached_index {
                        displays.insert(index, display.clone());
                    }
                }
                if displays.is_empty() {
                    println!("No added virtual displays.");
                } else {
                    println!("--------------------------------");
                    for (index, display) in displays.iter() {
                        println!("Index: {}, Display: {}", index, display);
                    }
                    println!("--------------------------------");
                }
            }
            CliCommands::Config {
                index,
                size,
                refresh_rate,
                position,
                orientation,
            } => {
                let mut position_value: Option<(i32, i32)> = None;
                let mut width_value: Option<i32> = None;
                let mut height_value: Option<i32> = None;
                if let Some(size) = size {
                    let (width, height) = size.split_once('x').unwrap();
                    width_value = width.parse::<i32>().ok();
                    height_value = height.parse::<i32>().ok();
                }
                if let Some(position) = position {
                    let (x, y) = position.split_once(',').unwrap();
                    let x = x.parse::<i32>().ok();
                    let y = y.parse::<i32>().ok();
                    if let (Some(x), Some(y)) = (x, y) {
                        position_value = Some((x, y));
                    }
                }
                if let Some(display) = displays.get_mut(&index) {
                    if display.change_mode(
                        width_value,
                        height_value,
                        refresh_rate,
                        position_value,
                        orientation,
                    ) {
                        println!("Display configured successfully, index: {}.", index);
                    } else {
                        println!("Failed to configure display, index: {}.", index);
                    }
                }
            }
            CliCommands::Exit => {
                *running.lock().unwrap() = false;
                break;
            }
        }
    }

    {
        for (index, _) in displays.iter() {
            if let Err(err) = vdd_remove_display(vdd_wrapper.0, *index) {
                println!("Error removing display: {err:?}");
            }
        }
    }

    if updater.join().is_err() {
        eprintln!("Failed to join updater thread");
    }

    close_device_handle(vdd_wrapper.0);
}
