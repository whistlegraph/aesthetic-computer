#!/bin/sh
# tv-sync.sh — read, set, or measure a TV station's audio delay, fast.
#
# The A/V offset is one number (AV_OFFSET seconds, itsoffset on the pulse
# input; positive = delay audio, i.e. push early audio later). Tuning it
# by hand meant ssh + sed + restart every time; this is one line.
#
#   tv-sync.sh <station>              show current offset (+ live encoder arg)
#   tv-sync.sh <station> <ms>         set offset to N milliseconds, restart
#   tv-sync.sh <station> +<ms>        nudge by N ms (e.g. +20, -15)
#   tv-sync.sh <station> measure      rig-side A/B card measurement (no Chrome,
#                                     no YouTube) — prints the true baseline
#
#   station = wgtv | actv    host fixed to jasellite (the rig box)
#
# Setting restarts the one station (a few-second stream blip); measure
# swaps in the test card for ~70s then restores. YouTube adds its own
# viewer-side skew on top of the rig number, so the ear stays the final
# judge — but this makes each guess cost one command, not five.
set -eu
HOST=jasellite
UNIT="${1:?usage: tv-sync.sh <wgtv|actv> [ms|+ms|measure]}"
ENV="$UNIT/$UNIT.env"

# ── read ──────────────────────────────────────────────────────────────
if [ $# -lt 2 ]; then
  ssh "$HOST" "sh -c '
    printf \"env:  \"; grep \"^AV_OFFSET=\" $ENV | tail -1
    printf \"live: \"; ps -eo args | grep -o -- \"itsoffset [0-9.]*\" | sort -u | tail -1'"
  exit 0
fi

ARG="$2"

# ── measure (rig-side A/B card) ───────────────────────────────────────
if [ "$ARG" = "measure" ]; then
  SIZE=$(ssh "$HOST" "grep '^SIZE=' $UNIT/$UNIT.env | tail -1 | cut -d= -f2")
  DISP=$(ssh "$HOST" "grep '^DISPLAY_NUM=' $UNIT/$UNIT.env | tail -1 | cut -d= -f2")
  SINK=$(ssh "$HOST" "grep '^SINK=' $UNIT/$UNIT.env | tail -1 | cut -d= -f2")
  [ -n "$SIZE" ] || SIZE=1280x720
  [ -n "$DISP" ] || DISP=:99
  [ -n "$SINK" ] || SINK=tvsink
  echo "▸ measuring $UNIT ($SIZE $DISP $SINK) via the sync card — ~70s, station restores after"
  ssh "$HOST" "sh -c '
    systemctl --user stop $UNIT-watchdog.timer $UNIT; sleep 2
    grep -v \"^URL=\" $ENV > /tmp/tvsync.env
    echo URL=file:///home/jas/wgtv/sync-test.html >> /tmp/tvsync.env
    echo UNMUTE_CLICK=1 >> /tmp/tvsync.env
    nohup sh $UNIT/rig.sh /tmp/tvsync.env > $UNIT/sync-rig.log 2>&1 &
    sleep 25
    ffmpeg -v error -thread_queue_size 1024 -use_wallclock_as_timestamps 1 \
      -f x11grab -framerate 30 -video_size $SIZE -i $DISP \
      -thread_queue_size 1024 -use_wallclock_as_timestamps 1 \
      -f pulse -fragment_size 1024 -i ${SINK}.monitor \
      -af aresample=async=1 -fps_mode cfr -t 30 \
      -c:v libx264 -preset veryfast -pix_fmt yuv420p -c:a aac -y /tmp/tvsync.mp4
    pkill -f \"user-data-dir=/home/jas/.tv-rig-chrom[e] \" 2>/dev/null || true
    pkill -f \"Xvfb ${DISP%%:*}:${DISP##*:}\" 2>/dev/null || true
    pkill -f -- \"-i ${DISP%%x*} \" 2>/dev/null || true
    sleep 2
    systemctl --user start $UNIT $UNIT-watchdog.timer
    echo ===FLASH===
    ffmpeg -i /tmp/tvsync.mp4 -vf blackdetect=d=0.15:pix_th=0.10 -an -f null - 2>&1 | grep -o \"black_end:[0-9.]*\" | cut -d: -f2 | tr \"\n\" \" \"
    echo; echo ===BEEP===
    ffmpeg -i /tmp/tvsync.mp4 -af silencedetect=n=-25dB:d=0.25 -vn -f null - 2>&1 | grep -o \"silence_end: [0-9.]*\" | awk \"{print \\\$2}\" | tr \"\n\" \" \"
    echo'" 2>/dev/null | FL_BP_READER=1 python3 -c '
import sys
fl=bp=None
for ln in sys.stdin:
    ln=ln.strip()
    if ln=="===FLASH===": mode="f"; continue
    if ln=="===BEEP===": mode="b"; continue
    vals=[float(x) for x in ln.split()] if ln and ln[0].isdigit() else None
    if vals and mode=="f": fl=vals
    if vals and mode=="b": bp=vals
if not fl or not bp:
    print("✗ measurement failed (no card events captured)"); sys.exit(1)
o=sorted((b-min(fl,key=lambda f:abs(f-b)))*1000 for b in bp if abs(b-min(fl,key=lambda f:abs(f-b)))<0.5)
m=o[len(o)//2]
print(f"rig A/V offset: {m:+.0f} ms ({\"audio LATE\" if m>0 else \"audio EARLY\"}); spread {o[0]:+.0f}…{o[-1]:+.0f}")
print(f"suggested: tv-sync.sh {\"'\"$UNIT\"'\"} {int(round(-m))}   (delay audio to cancel it)")
'
  exit 0
fi

# ── set / nudge ───────────────────────────────────────────────────────
case "$ARG" in
  +*|-*) # nudge
    CUR=$(ssh "$HOST" "grep '^AV_OFFSET=' $ENV | tail -1 | cut -d= -f2" || echo 0)
    [ -n "$CUR" ] || CUR=0
    NEW=$(awk "BEGIN{printf \"%.3f\", $CUR + ($ARG)/1000}")
    ;;
  *) NEW=$(awk "BEGIN{printf \"%.3f\", $ARG/1000}") ;;
esac

echo "▸ $UNIT AV_OFFSET → ${NEW}s (audio delayed ${NEW}s) — restarting"
ssh "$HOST" "sh -c '
  grep -q \"^AV_OFFSET=\" $ENV && sed -i \"s/^AV_OFFSET=.*/AV_OFFSET=$NEW/\" $ENV || echo AV_OFFSET=$NEW >> $ENV
  systemctl --user restart $UNIT
  sleep 24
  systemctl --user is-active $UNIT
  ps -eo args | grep -o -- \"itsoffset $NEW\" | head -1'"
