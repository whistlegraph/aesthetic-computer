#!/bin/bash
# Emacs Race Condition Monitor
# 
# This script monitors Emacs startup to catch race conditions and crashes
# that occur intermittently during the aesthetic command initialization.
#
# Usage:
#   ./utilities/emacs-monitor.sh
#
# The script will:
# 1. Start monitoring system resources and Emacs processes
# 2. Wait for you to run `aesthetic-now` in another terminal
# 3. Log all activity to help debug race conditions
#
# Logs are saved to:
# - /tmp/emacs-debug.log (Emacs internal debugging)
# - /tmp/emacs-monitoring/system.log (System resource monitoring)

echo "🔍 Starting Emacs Race Condition Monitor..."
echo "📁 Creating monitoring directories..."
mkdir -p /tmp/emacs-monitoring

# Function to log with timestamp
log_with_time() {
    echo "[$(date '+%H:%M:%S.%3N')] $1" | tee -a /tmp/emacs-monitoring/system.log
}

# Function to monitor system state
monitor_system() {
    while true; do
        log_with_time "=== SYSTEM STATE ==="
        
        # Process monitoring
        emacs_procs=$(ps aux | grep -E "(emacs|aesthetic)" | grep -v grep)
        if [ -n "$emacs_procs" ]; then
            log_with_time "📊 Emacs Processes:"
            echo "$emacs_procs" | while read line; do
                log_with_time "  $line"
            done
        else
            log_with_time "❌ No Emacs processes found"
        fi
        
        # Resource monitoring
        load_avg=$(cat /proc/loadavg 2>/dev/null || echo "unknown")
        memory=$(free -h | grep Mem | awk '{print $3 "/" $2 " used, " $7 " available"}' 2>/dev/null || echo "unknown")
        network_conns=$(ss -tuln 2>/dev/null | wc -l || echo "unknown")
        
        log_with_time "⚡ Load Average: $load_avg"
        log_with_time "💾 Memory: $memory"
        log_with_time "🌐 Network Connections: $network_conns"
        
        # Check for zombie processes
        zombies=$(ps aux | grep -E "Z|<defunct>" | grep -v grep | wc -l)
        if [ "$zombies" -gt 0 ]; then
            log_with_time "🧟 WARNING: $zombies zombie processes detected"
            ps aux | grep -E "Z|<defunct>" | grep -v grep | while read line; do
                log_with_time "  ZOMBIE: $line"
            done
        fi
        
        log_with_time ""
        sleep 2
    done
}

# Function to test race condition
test_race_condition() {
    log_with_time "🧪 Starting Race Condition Test..."
    
    for i in {1..3}; do
        log_with_time "🔄 Test Attempt $i/3"
        
        # Kill any existing Emacs processes
        log_with_time "🔪 Killing existing Emacs processes..."
        pkill -f emacs 2>/dev/null || true
        sleep 2
        
        # Try to start aesthetic-now with timeout
        log_with_time "🚀 Starting aesthetic-now (30s timeout)..."
        start_time=$(date +%s)
        
        if timeout 30s aesthetic-now 2>&1 | tee -a /tmp/emacs-monitoring/aesthetic-output.log; then
            end_time=$(date +%s)
            duration=$((end_time - start_time))
            log_with_time "✅ Attempt $i succeeded in ${duration}s"
        else
            end_time=$(date +%s)
            duration=$((end_time - start_time))
            log_with_time "❌ Attempt $i failed/timeout after ${duration}s"
            
            # Capture any stuck processes
            stuck_procs=$(ps aux | grep -E "(emacs|aesthetic)" | grep -v grep)
            if [ -n "$stuck_procs" ]; then
                log_with_time "🔒 Stuck processes detected:"
                echo "$stuck_procs" | while read line; do
                    log_with_time "  $line"
                done
            fi
        fi
        
        sleep 3
    done
    
    log_with_time "🧪 Race condition test completed"
}

# Start monitoring in background
log_with_time "🎬 Starting system monitor..."
monitor_system &
MONITOR_PID=$!

# Set up cleanup
cleanup() {
    log_with_time "🛑 Stopping monitor (PID: $MONITOR_PID)..."
    kill $MONITOR_PID 2>/dev/null || true
    log_with_time "📋 Monitor logs saved to:"
    log_with_time "  - /tmp/emacs-monitoring/system.log"
    log_with_time "  - /tmp/emacs-debug.log (if Emacs debugging was enabled)"
    log_with_time "  - /tmp/emacs-monitoring/aesthetic-output.log"
    exit 0
}

trap cleanup SIGINT SIGTERM

echo ""
echo "🎯 Monitor is running! Choose an option:"
echo ""
echo "1. Manual Mode: Run 'aesthetic-now' in another terminal"
echo "   Press Ctrl+C when done to stop monitoring"
echo ""
echo "2. Auto Test: Press 't' + Enter to run automated race condition test"
echo ""
echo "📊 Live monitoring data is being logged to /tmp/emacs-monitoring/system.log"
echo ""

read -p "Choice (t for auto test, or just wait for manual): " choice

if [ "$choice" = "t" ] || [ "$choice" = "T" ]; then
    test_race_condition
    cleanup
else
    log_with_time "👀 Manual mode: Waiting for aesthetic-now in another terminal..."
    log_with_time "   Press Ctrl+C to stop monitoring"
    
    # Wait indefinitely
    while true; do
        sleep 10
    done
fi