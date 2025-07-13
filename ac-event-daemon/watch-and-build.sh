#!/bin/bash
# watch-and-build.sh - Auto-rebuild Tauri overlay when files change

echo "🔧 Starting auto-build daemon watcher..."
echo "📁 Watching: ac-event-daemon/, system/public/overlay-test.html"
echo "🔨 Will rebuild Tauri overlay on changes"
echo ""

# Build initial versions
echo "🚀 Initial build..."
cd /workspaces/aesthetic-computer/ac-event-daemon

# Build both binaries
cargo build --release --bin ac-event-daemon
cargo build --release --bin tauri-overlay

echo "✅ Initial build complete!"
echo ""
echo "👀 Watching for changes..."

# Watch for changes and rebuild
inotifywait -m -r --format '%w%f %e' \
  -e modify,create,delete,move \
  /workspaces/aesthetic-computer/ac-event-daemon/src/ \
  /workspaces/aesthetic-computer/system/public/overlay-test.html \
  /workspaces/aesthetic-computer/ac-event-daemon/Cargo.toml |
while read file event; do
  echo "📝 Changed: $file ($event)"
  
  # Build daemon if daemon files changed
  if [[ "$file" == *"ac-event-daemon.rs"* ]] || [[ "$file" == *"Cargo.toml"* ]]; then
    echo "🔨 Rebuilding daemon..."
    cargo build --release --bin ac-event-daemon
    echo "✅ Daemon rebuilt!"
  fi
  
  # Build Tauri overlay if overlay files changed
  if [[ "$file" == *"tauri-overlay.rs"* ]] || [[ "$file" == *"overlay-test.html"* ]] || [[ "$file" == *"Cargo.toml"* ]]; then
    echo "🔨 Rebuilding Tauri overlay..."
    cargo build --release --bin tauri-overlay
    echo "✅ Tauri overlay rebuilt!"
  fi
  
  echo ""
done
