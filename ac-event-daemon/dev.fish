#!/usr/bin/env fish

# Script to run the ac-event-daemon (with optional watch mode if cargo-watch available)

echo "🚀 Starting ac-event-daemon..."

# Get the user's HOME directory from the first argument
set USER_HOME $argv[1]

if test -z "$USER_HOME"
    echo "Critical: User HOME directory not passed to script. Exiting."
    exit 1
end

# Explicitly set RUSTUP_HOME and CARGO_HOME for this script's environment
set -x RUSTUP_HOME "$USER_HOME/.rustup"
set -x CARGO_HOME "$USER_HOME/.cargo"

# Ensure the Rust bin directory is in PATH for the current fish session
if test -d "$CARGO_HOME/bin"
    fish_add_path "$CARGO_HOME/bin"
    echo "Added $CARGO_HOME/bin to PATH"
else
    echo "Warning: $CARGO_HOME/bin not found. Rust environment might not be set up correctly."
end

# Navigate to the script's directory to ensure paths are correct
cd (dirname (status --current-filename))

# Try cargo-watch if available, otherwise just run once
if command -q cargo-watch
    echo "Starting cargo watch..."
    cargo watch -c -w src -w Cargo.toml -x "run --release"
else
    echo "⚠ cargo-watch not available (needs Rust 1.85+), running without auto-reload"
    echo "  To enable: rustup update stable"
    cargo run --release
end
