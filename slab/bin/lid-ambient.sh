#!/bin/bash
# lid-ambient daemon. Long-lived via launchd KeepAlive; polls lid every POLL
# seconds; on transition plays chimes and manages the ambient loop + reactive
# listener + resource monitor. Turns off display on lid close.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}

POLL=${POLL:-0.5}
SOUNDS="$SLAB_HOME/sounds"
wav="$SOUNDS/ambient.wav"
start_wav="$SOUNDS/lid-start.wav"
return_wav="$SOUNDS/lid-return.wav"
open_ding="$SOUNDS/lid-open-ding.wav"
return_dur=1.15

PID_DIR=/tmp
pid_file="$PID_DIR/lidambient.pid"
reactive_pid_file="$PID_DIR/lidreactive.pid"
monitor_pid_file="$PID_DIR/slab-monitor.pid"
log="$SLAB_HOME/logs/lidalive.log"
mkdir -p "$(dirname "$log")"

reactive_py="$SLAB_HOME/venv/bin/python3"
reactive_script="$SLAB_BIN/lid-reactive.py"

log_msg() { echo "$(date '+%Y-%m-%d %H:%M:%S') $*" >> "$log"; }

stop_player() {
    local pid
    if [[ -f "$pid_file" ]]; then
        pid=$(cat "$pid_file" 2>/dev/null)
        if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
            pkill -P "$pid" 2>/dev/null
            kill "$pid" 2>/dev/null
        fi
        rm -f "$pid_file"
    fi
    pkill -x afplay 2>/dev/null
}

start_player() {
    (while true; do /usr/bin/afplay "$wav"; done) > /dev/null 2>&1 &
    echo $! > "$pid_file"
    log_msg "started ambient loop pid $!"
}

start_reactive() {
    if [[ -x "$reactive_py" && -f "$reactive_script" ]]; then
        nohup "$reactive_py" "$reactive_script" > /dev/null 2>&1 &
        echo $! > "$reactive_pid_file"
        log_msg "started reactive listener pid $!"
    fi
}

stop_reactive() {
    local pid
    if [[ -f "$reactive_pid_file" ]]; then
        pid=$(cat "$reactive_pid_file" 2>/dev/null)
        [[ -n "$pid" ]] && kill "$pid" 2>/dev/null
        rm -f "$reactive_pid_file"
    fi
    pkill -f lid-reactive.py 2>/dev/null
}

start_monitor() {
    if [[ -x "$SLAB_BIN/slab-monitor.sh" ]]; then
        nohup "$SLAB_BIN/slab-monitor.sh" 15 > /dev/null 2>&1 &
        echo $! > "$monitor_pid_file"
        log_msg "started resource monitor pid $!"
    fi
}

stop_monitor() {
    local pid
    if [[ -f "$monitor_pid_file" ]]; then
        pid=$(cat "$monitor_pid_file" 2>/dev/null)
        [[ -n "$pid" ]] && kill "$pid" 2>/dev/null
        rm -f "$monitor_pid_file"
    fi
    pkill -f slab-monitor.sh 2>/dev/null
}

cleanup() {
    log_msg "daemon exiting, cleaning up"
    stop_player
    stop_reactive
    stop_monitor
    exit 0
}
trap cleanup SIGTERM SIGINT SIGHUP

log_msg "daemon starting (pid $$)"
stop_player
stop_reactive
stop_monitor
start_monitor

prev=""
while true; do
    state=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
    sleep_disabled=$(pmset -g | awk '/SleepDisabled/{print $2; exit}')
    sleep_disabled=${sleep_disabled:-0}

    # No -> Yes (lid just closed)
    if [[ "$state" == "Yes" && "$prev" == "No" ]]; then
        log_msg "lid CLOSED (sleep_disabled=$sleep_disabled)"
        if [[ "$sleep_disabled" == "1" ]]; then
            /usr/bin/afplay "$start_wav" 2>/dev/null &
            start_player
            start_reactive
            (sleep 0.6; sudo -n /usr/bin/pmset displaysleepnow 2>/dev/null) &
        fi
    fi

    # Yes -> No (lid just opened): ding + return stinger, kill ambient after stinger
    if [[ "$state" == "No" && "$prev" == "Yes" ]]; then
        log_msg "lid OPENED - ding + return stinger"
        /usr/bin/afplay "$open_ding"  2>/dev/null &
        /usr/bin/afplay "$return_wav" 2>/dev/null &
        (sleep "$return_dur"
         cur=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
         if [[ "$cur" == "No" ]]; then
             stop_player
             stop_reactive
             log_msg "ambient + reactive stopped after return stinger"
         fi
        ) &
    fi

    prev="$state"
    sleep "$POLL"
done
