#!/usr/bin/env fish

# Script to watch for changes and automatically recompile/relaunch the ac-event-daemon

echo "🚀 Starting ac-event-daemon in development mode with auto-reload..."

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

# Debugging output
echo "USER_HOME is: $USER_HOME"
echo "RUSTUP_HOME is: $RUSTUP_HOME"
echo "CARGO_HOME is: $CARGO_HOME"
echo "Current PATH is: $PATH"
echo "Attempting to show rustup default toolchain:"
rustup show active-toolchain || echo "rustup show active-toolchain failed"
echo "Attempting to get cargo version:"
cargo --version || echo "cargo --version failed"

# Navigate to the script's directory to ensure paths are correct
cd (dirname (status --current-filename))

# -c: Clear screen before each run
# -w src: Watch the src directory
# -w Cargo.toml: Watch the Cargo.toml file
# -x run: Execute 'cargo run --release' on changes
# The actual cargo command will be run with the modified PATH and RUSTUP/CARGO_HOME
echo "Starting cargo watch..."
cargo watch -c -w src -w Cargo.toml -x 'run --release'