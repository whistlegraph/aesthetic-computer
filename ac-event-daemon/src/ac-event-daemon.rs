// ac-event-daemon, 25.05.22.06.44
// UDP notification listener with giant word overlay

use std::{ thread, time::Duration };
use std::net::UdpSocket;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use signal_hook::{consts::SIGTERM, consts::SIGHUP, consts::SIGINT};

// Function to show giant word overlay using Tauri overlay
fn show_giant_word_overlay(notification_type: &str, message: &str) {
    let notification_type = notification_type.to_string();
    let message = message.to_string();
    
    std::thread::spawn(move || {
        let word = if message.is_empty() { 
            notification_type.to_uppercase() 
        } else { 
            message.to_uppercase() 
        };
        
        // Use Tauri overlay only
        create_tauri_overlay(&word, &notification_type);
    });
}

// Create a transparent Tauri overlay
fn create_tauri_overlay(word: &str, notification_type: &str) -> bool {
    // Try to spawn the tauri-overlay binary
    let result = Command::new("target/release/tauri-overlay")
        .arg(notification_type)
        .arg(word)
        .arg("3000") // duration in ms
        .spawn();
    
    match result {
        Ok(_) => {
            if std::env::var("DAEMON_VERBOSE").unwrap_or_default() == "1" {
                println!("🎨 Launched Tauri transparent overlay");
            }
            true
        }
        Err(e) => {
            if std::env::var("DAEMON_VERBOSE").unwrap_or_default() == "1" {
                println!("⚠️ Tauri overlay failed, falling back to browser: {}", e);
            }
            false
        }
    }
}

// Function to handle UDP notifications
fn handle_udp_notification(socket: &UdpSocket) -> std::io::Result<()> {
    let mut buf = [0; 1024];

    // Non-blocking receive
    match socket.recv(&mut buf) {
        Ok(received_size) => {
            if let Ok(message) = std::str::from_utf8(&buf[..received_size]) {
                if message.starts_with("prompt-complete:") {
                    let rest = message.strip_prefix("prompt-complete:").unwrap_or("unknown");
                    let parts: Vec<&str> = rest.splitn(2, ':').collect();
                    
                    let notification_type = parts.get(0).unwrap_or(&"unknown");
                    let notification_message = parts.get(1).unwrap_or(&"");
                    
                    // Only log if verbose mode (set DAEMON_VERBOSE=1)
                    if std::env::var("DAEMON_VERBOSE").unwrap_or_default() == "1" {
                        println!("📡 Notification: {} - {}", notification_type.to_uppercase(), notification_message);
                    }
                    
                    // Show giant word overlay
                    show_giant_word_overlay(notification_type, notification_message);
                }
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
            // No data available, this is normal for non-blocking socket
        }
        Err(e) => {
            eprintln!("❌ UDP receive error: {:?}", e);
        }
    }

    Ok(())
}

fn main() -> std::io::Result<()> {
    println!("🎵 ac-event-daemon - UDP notification listener");
    println!("📡 Listening on 0.0.0.0:9999");
    println!("💡 Press Ctrl+C to exit or close terminal");

    // Set up UDP socket for notifications
    let udp_socket = UdpSocket::bind("0.0.0.0:9999")?;
    udp_socket.set_nonblocking(true)?;

    // Set up signal handler for graceful shutdown
    let running = Arc::new(AtomicBool::new(true));
    
    // Handle multiple signals: SIGINT (Ctrl+C), SIGTERM, SIGHUP (terminal close)
    for &signal in &[SIGINT, SIGTERM, SIGHUP] {
        let r = running.clone();
        unsafe {
            signal_hook::low_level::register(signal, move || {
                r.store(false, Ordering::SeqCst);
            }).expect("Failed to register signal handler");
        }
    }

    // Get parent process ID for orphan detection
    let parent_pid = std::process::id();
    let mut check_counter = 0;

    while running.load(Ordering::SeqCst) {
        // Handle UDP notifications
        if let Err(e) = handle_udp_notification(&udp_socket) {
            eprintln!("UDP notification error: {:?}", e);
        }

        // Every 100 iterations (~5 seconds), check if we're orphaned
        check_counter += 1;
        if check_counter >= 100 {
            check_counter = 0;
            
            // Check if parent process changed (we became orphaned)
            let current_parent = std::process::id();
            if current_parent != parent_pid {
                // Simple orphan check - if our process group changed significantly
                // or if we can't access our original parent, we should probably exit
                if let Ok(output) = std::process::Command::new("ps")
                    .arg("-p")
                    .arg(parent_pid.to_string())
                    .output() 
                {
                    if !output.status.success() {
                        println!("👻 Parent process gone, shutting down");
                        break;
                    }
                }
            }
        }

        thread::sleep(Duration::from_millis(50));
    }

    println!("👋 ac-event-daemon shutting down");
    Ok(())
}
