#!/usr/bin/env bash
# DEPRECATED: Use ac-emacs-crash-monitor (fish) instead.
# The fish monitor has startup-lock awareness, correct config on restart,
# load-aware timeouts, and CPU monitoring. Run: ac-emacs-start-monitor
#
# Emacs Watchdog - monitors emacs daemon health and auto-recovers from hangs
# Run: ./monitor-emacs.sh (foreground) or ./monitor-emacs.sh & (background)
# Stop: kill $(cat /tmp/emacs-watchdog.pid) or ac-watchdog-stop

CHECK_INTERVAL="${WATCHDOG_INTERVAL:-10}"  # Check every N seconds
CPU_THRESHOLD="${WATCHDOG_CPU:-85}"        # CPU % threshold to consider "stuck"
CPU_SAMPLES="${WATCHDOG_SAMPLES:-3}"       # Consecutive high-CPU samples before action
TIMEOUT_THRESHOLD=5                         # Seconds to wait for emacsclient response

LOG_DIR="/workspaces/aesthetic-computer/.emacs-logs"
LOG_FILE="$LOG_DIR/watchdog.log"
PID_FILE="/tmp/emacs-watchdog.pid"
WARNING_FILE="/tmp/emacs-watchdog-warning"

mkdir -p "$LOG_DIR"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Track high CPU samples
high_cpu_count=0

check_emacs_health() {
    local daemon_pid
    daemon_pid=$(pgrep -f "emacs.*daemon" 2>/dev/null | head -1)
    
    if [ -z "$daemon_pid" ]; then
        echo "not_running"
        return
    fi
    
    # Check if responsive (quick eval)
    if ! timeout "$TIMEOUT_THRESHOLD" emacsclient -e "t" >/dev/null 2>&1; then
        echo "unresponsive:$daemon_pid"
        return
    fi
    
    # Check CPU usage
    local cpu
    cpu=$(ps -p "$daemon_pid" -o %cpu= 2>/dev/null | tr -d ' ' | cut -d. -f1)
    
    if [ -n "$cpu" ] && [ "$cpu" -gt "$CPU_THRESHOLD" ]; then
        echo "high_cpu:$cpu:$daemon_pid"
        return
    fi
    
    echo "healthy:$daemon_pid"
}

restart_emacs() {
    local reason="$1"
    log "🔄 WATCHDOG: Restarting emacs daemon (reason: $reason)"
    
    # Kill all emacs processes
    pkill -9 -f "emacs.*daemon" 2>/dev/null
    pkill -9 emacs 2>/dev/null
    pkill -9 emacsclient 2>/dev/null
    sleep 2
    
    # Start fresh daemon
    log "🚀 WATCHDOG: Starting fresh emacs daemon..."
    emacs --daemon 2>&1 | head -10 >> "$LOG_FILE"
    
    sleep 2
    
    # Verify
    if timeout 5 emacsclient -e "t" >/dev/null 2>&1; then
        log "✅ WATCHDOG: Emacs daemon restarted successfully"
        # Trigger aesthetic-backend so tabs/terminals come back after crash
        if timeout 15 emacsclient -e "(aesthetic-backend \"artery\")" >/dev/null 2>&1; then
            log "🧭 WATCHDOG: aesthetic-backend triggered after restart"
        else
            log "⚠️  WATCHDOG: Failed to trigger aesthetic-backend after restart"
        fi
        show_warning "$reason"
        return 0
    else
        log "❌ WATCHDOG: Emacs daemon failed to restart"
        return 1
    fi
}

show_warning() {
    local reason="$1"
    local timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    # Write warning file for artery TUI to detect
    cat > "$WARNING_FILE" << EOF
{
  "timestamp": "$timestamp",
  "reason": "$reason",
  "message": "Emacs daemon was auto-restarted. Restart the '💻 Aesthetic' task to reconnect.",
  "acknowledged": false
}
EOF
    
    # Try ac-notify if available
    if [ -x "/workspaces/aesthetic-computer/ac-notify" ]; then
        /workspaces/aesthetic-computer/ac-notify "⚠️ Emacs Recovered" "Watchdog: $reason. Restart the Aesthetic task." 2>/dev/null &
    fi
    
    # Ring terminal bell
    printf '\a' 2>/dev/null
    
    log "⚠️  WARNING: $reason - user notification sent"
}

show_status() {
    local status
    status=$(check_emacs_health)
    
    echo "=== Emacs Watchdog Status ==="
    echo "PID File: $PID_FILE"
    echo "Log File: $LOG_FILE"
    echo "Check Interval: ${CHECK_INTERVAL}s"
    echo "CPU Threshold: ${CPU_THRESHOLD}%"
    echo "CPU Samples: ${CPU_SAMPLES}"
    echo ""
    
    case "$status" in
        not_running)
            echo "Emacs: 🔴 Not running"
            ;;
        unresponsive:*)
            echo "Emacs: ⚠️  Unresponsive (PID: ${status#unresponsive:})"
            ;;
        high_cpu:*:*)
            local cpu="${status#high_cpu:}"
            cpu="${cpu%%:*}"
            local pid="${status##*:}"
            echo "Emacs: ⚠️  High CPU ${cpu}% (PID: $pid)"
            ;;
        healthy:*)
            echo "Emacs: ✅ Healthy (PID: ${status#healthy:})"
            ;;
    esac
    
    if [ -f "$PID_FILE" ]; then
        local wpid
        wpid=$(cat "$PID_FILE")
        if ps -p "$wpid" >/dev/null 2>&1; then
            echo "Watchdog: 🟢 Running (PID: $wpid)"
        else
            echo "Watchdog: 🔴 Dead (stale PID file)"
        fi
    else
        echo "Watchdog: 🔴 Not running"
    fi
}

main_loop() {
    # Write PID file
    echo $$ > "$PID_FILE"
    
    log "🐕 WATCHDOG: Starting emacs health monitor (PID: $$)"
    log "   Check interval: ${CHECK_INTERVAL}s, CPU threshold: ${CPU_THRESHOLD}%, Samples needed: ${CPU_SAMPLES}"
    
    while true; do
        status=$(check_emacs_health)
        
        case "$status" in
            "not_running")
                # Daemon not running - probably intentional, don't log spam
                high_cpu_count=0
                ;;
            unresponsive:*)
                local pid="${status#unresponsive:}"
                log "⚠️  WATCHDOG: Emacs daemon (PID: $pid) UNRESPONSIVE - restarting"
                high_cpu_count=0
                restart_emacs "unresponsive (timeout after ${TIMEOUT_THRESHOLD}s)"
                ;;
            high_cpu:*:*)
                local cpu="${status#high_cpu:}"
                cpu="${cpu%%:*}"
                local pid="${status##*:}"
                high_cpu_count=$((high_cpu_count + 1))
                log "⚠️  WATCHDOG: High CPU detected (${cpu}%) on PID $pid - sample $high_cpu_count/$CPU_SAMPLES"
                
                if [ "$high_cpu_count" -ge "$CPU_SAMPLES" ]; then
                    log "🔥 WATCHDOG: Sustained high CPU for ${CPU_SAMPLES} checks - restarting"
                    restart_emacs "sustained high CPU (${cpu}% for ${CPU_SAMPLES} samples)"
                    high_cpu_count=0
                fi
                ;;
            healthy:*)
                if [ "$high_cpu_count" -gt 0 ]; then
                    log "✅ WATCHDOG: CPU normalized after $high_cpu_count samples"
                fi
                high_cpu_count=0
                ;;
        esac
        
        sleep "$CHECK_INTERVAL"
    done
}

cleanup() {
    log "🛑 WATCHDOG: Shutting down (signal received)"
    rm -f "$PID_FILE"
    exit 0
}

# Handle signals gracefully
trap cleanup SIGTERM SIGINT SIGHUP

# Parse arguments
case "${1:-}" in
    status|--status|-s)
        show_status
        exit 0
        ;;
    stop|--stop)
        if [ -f "$PID_FILE" ]; then
            wpid=$(cat "$PID_FILE")
            if kill -0 "$wpid" 2>/dev/null; then
                kill "$wpid"
                echo "Watchdog (PID: $wpid) stopped"
                rm -f "$PID_FILE"
            else
                echo "Watchdog not running (stale PID file)"
                rm -f "$PID_FILE"
            fi
        else
            echo "Watchdog not running (no PID file)"
        fi
        exit 0
        ;;
    help|--help|-h)
        echo "Usage: $0 [command]"
        echo ""
        echo "Commands:"
        echo "  (none)    Start watchdog in foreground"
        echo "  status    Show emacs and watchdog status"
        echo "  stop      Stop running watchdog"
        echo "  help      Show this help"
        echo ""
        echo "Environment variables:"
        echo "  WATCHDOG_INTERVAL  Check interval in seconds (default: 10)"
        echo "  WATCHDOG_CPU       CPU threshold percentage (default: 85)"
        echo "  WATCHDOG_SAMPLES   High-CPU samples before restart (default: 3)"
        exit 0
        ;;
esac

# Check if already running
if [ -f "$PID_FILE" ]; then
    existing_pid=$(cat "$PID_FILE")
    if ps -p "$existing_pid" >/dev/null 2>&1; then
        echo "Watchdog already running (PID: $existing_pid)"
        echo "Use '$0 stop' to stop it first, or '$0 status' to check status"
        exit 1
    else
        rm -f "$PID_FILE"
    fi
fi

main_loop