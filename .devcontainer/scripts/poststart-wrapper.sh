#!/bin/sh
# Wrapper to run entry.fish in background so VS Code can connect immediately
# The heavy setup continues in background and creates .waiter when done

LOG_DIR="/workspaces/aesthetic-computer/.devcontainer/entry-logs"
mkdir -p "$LOG_DIR"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BG_LOG="$LOG_DIR/background-$TIMESTAMP.log"
WRAPPER_LOG="$LOG_DIR/wrapper-$TIMESTAMP.log"

# Function to log with timestamp
log() {
    MSG="[$(date '+%H:%M:%S.%3N')] $1"
    echo "$MSG"
    echo "$MSG" >> "$WRAPPER_LOG"
    sync 2>/dev/null
}

log "🚀 poststart-wrapper.sh starting"
log "📋 Wrapper log: $WRAPPER_LOG"
log "📋 Background log: $BG_LOG"
log "🖥️  Hostname: $(hostname 2>/dev/null || cat /etc/hostname 2>/dev/null || echo 'unknown')"
log "👤 User: $(whoami)"
log "📁 PWD: $(pwd)"

# Create a marker so we know wrapper was actually used
touch "$LOG_DIR/.wrapper-used-$TIMESTAMP"
log "✅ Created wrapper marker"

# Run entry.fish in background, redirect output to log
log "🔄 Starting entry.fish in background..."
nohup fish /workspaces/aesthetic-computer/.devcontainer/entry.fish > "$BG_LOG" 2>&1 &
BG_PID=$!

log "✅ entry.fish started with PID: $BG_PID"

# Create symlinks to latest logs
ln -sf "$BG_LOG" "$LOG_DIR/latest-background.log"
ln -sf "$WRAPPER_LOG" "$LOG_DIR/latest-wrapper.log"
log "🔗 Created symlinks to latest logs"

# Give a moment for critical early setup (dockerd, basic env)
log "⏳ Waiting 3 seconds for critical setup..."
sleep 3

# Check if process is still running
if kill -0 $BG_PID 2>/dev/null; then
    log "✅ Background process still running (PID: $BG_PID)"
else
    log "⚠️  Background process may have exited early"
fi

log "🎉 Wrapper complete - VS Code can now connect"
log "📖 Monitor progress: tail -f $BG_LOG"
log "🏁 poststart-wrapper.sh finished"

