# AC Event Daemon

A UDP notification listener that displays giant word overlays for aesthetic-computer notifications.

## Features

- 🔄 UDP notification listener (port 9999)
- 🌐 **Web-based overlay system** using aesthetic.computer
- 🖥️ Giant word overlays with multiple fallback methods:
  - Web overlay via aesthetic.computer/overlay.html (primary)
  - Local HTML file fallback
  - Terminal output (disabled by default)
- 🎨 Color-coded notifications: success, error, info, warning
- 🐧 Linux/Wayland friendly with fractional scaling support
- 📦 Container compatible
- 🛡️ Robust signal handling (auto-stops when terminal closes)
- 👤 Root user safety and clean output
- 🎯 Minimal terminal output (use DAEMON_VERBOSE=1 for debug)

## Usage

### Start the daemon:
```bash
cd ac-event-daemon
./start-daemon.sh
# OR
cargo run
```

### Send notifications:
```bash
./ac-notify success "Task completed"
./ac-notify error "Something failed"  
./ac-notify info "Information"
./ac-notify warning "Be careful"

# Verbose mode (shows debug info)
VERBOSE=1 ./ac-notify success "Debug mode"
```

### Test web overlay:
```bash
./test-web-overlay.sh
```

### Daemon verbose mode:
```bash
DAEMON_VERBOSE=1 ./start-daemon.sh
```

### Safe daemon startup (handles root permissions):
```bash
cd ac-event-daemon
./start-daemon-safe.sh
```

## UDP Protocol

The daemon listens for UDP messages on port 9999 with the format:
```
prompt-complete:<type>
```

Where `<type>` can be:
- `success` - Green overlay with ✨ SUCCESS ✨
- `error` - Red overlay with ❌ ERROR ❌  
- `info` - Blue overlay with ℹ️ INFO ℹ️

## System Requirements

- Rust/Cargo
- Linux with UDP networking
- Optional: rofi, gxmessage, notify-send for overlays
- Optional: Browser (firefox, chromium, etc.) for HTML overlays

## Architecture

- **ac-event-daemon**: Rust UDP listener with overlay display
- **ac-notify**: Bash script for sending notifications
- Fallback chain: HTML → rofi → gxmessage → system notification

The system is designed to work in various environments including containers and headless systems.
