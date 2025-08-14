// tauri-overlay.rs - Transparent notification overlay using Tauri
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tauri::{WindowBuilder, WindowUrl};
use std::env;

fn main() {
    // Get command line arguments for the notification
    let args: Vec<String> = env::args().collect();
    let notification_type = args.get(1).unwrap_or(&"info".to_string()).clone();
    let message = args.get(2).unwrap_or(&"NOTIFICATION".to_string()).clone();
    let duration = args.get(3).unwrap_or(&"3000".to_string()).parse::<u64>().unwrap_or(3000);
    
    let encoded_word = urlencoding::encode(&message);
    let overlay_url = format!(
        "http://localhost:8888/overlay-test.html?type={}&word={}&duration={}",
        notification_type,
        encoded_word,
        duration
    );
    
    tauri::Builder::default()
        .setup(move |app| {
            let window = WindowBuilder::new(
                app,
                "overlay",
                WindowUrl::External(overlay_url.parse().unwrap())
            )
            .title("AC Notification Overlay")
            .inner_size(800.0, 600.0)
            .position(100.0, 100.0)
            .resizable(false)
            .decorations(false)
            .transparent(true)
            .always_on_top(true)
            .skip_taskbar(true)
            .build()?;
            
            // Auto-close after duration
            if duration > 0 {
                let window_clone = window.clone();
                std::thread::spawn(move || {
                    std::thread::sleep(std::time::Duration::from_millis(duration));
                    let _ = window_clone.close();
                    std::process::exit(0);
                });
            }
            
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
