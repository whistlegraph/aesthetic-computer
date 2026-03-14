#!/bin/sh
# upload-log.sh — Upload ac-native.log to machines API
# Called in background on wifi connect. Reads auth from config.json.
# Bundled into /scripts/upload-log.sh in the initramfs.

LOG="/mnt/ac-native.log"
CONFIG="/mnt/config.json"
MACHINE_ID_FILE="/mnt/.machine-id"
API="https://aesthetic.computer/api/machine-logs"
CA="/etc/pki/tls/certs/ca-bundle.crt"

# Need log file and config
[ -f "$LOG" ] || exit 0
[ -f "$CONFIG" ] || exit 0

# Extract user sub from config (simple grep — no jq in initramfs)
SUB=$(sed -n 's/.*"sub"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$CONFIG" | head -1)
[ -z "$SUB" ] && exit 0
echo "[upload-log] sub=$SUB mid=$MID" >> "$LOG"

# Machine ID
MID="unknown"
[ -f "$MACHINE_ID_FILE" ] && MID=$(cat "$MACHINE_ID_FILE" | tr -d '\n')

# Read log lines, escape for JSON, limit to 200 lines
LINES=""
n=0
while IFS= read -r line && [ $n -lt 200 ]; do
    # Escape backslashes, double quotes, and control chars
    escaped=$(printf '%s' "$line" | sed 's/\\/\\\\/g; s/"/\\"/g; s/	/\\t/g')
    if [ -n "$LINES" ]; then
        LINES="$LINES,\"$escaped\""
    else
        LINES="\"$escaped\""
    fi
    n=$((n + 1))
done < "$LOG"

[ -z "$LINES" ] && exit 0

# Build JSON body (include sub for device auth)
BODY="{\"machineId\":\"$MID\",\"sub\":\"$SUB\",\"lines\":[$LINES],\"sessionType\":\"log\"}"

# Write body to temp file (avoid shell escaping issues with curl -d)
echo "$BODY" > /tmp/log-upload.json

# Upload (use sub as a simple auth — the API will validate)
curl -sf -X POST "$API" \
    -H "Content-Type: application/json" \
    --cacert "$CA" \
    -d @/tmp/log-upload.json \
    --max-time 15 > /tmp/log-upload-result.json 2>&1

rc=$?
rm -f /tmp/log-upload.json
echo "[upload-log] rc=$rc mid=$MID lines=$n" >> "$LOG"
