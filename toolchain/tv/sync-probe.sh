#!/bin/bash
# sync-probe.sh — end-to-end AV-sync + latency measurement for a TV station.
# Runs on a residential machine (YouTube bot-checks datacenter IPs out of
# HLS access, so jasellite can't pull its own stream); the rig host airs
# the card, this side records the real viewer feed and reads the numbers.
#
#   toolchain/tv/sync-probe.sh [rig-host] [station] [watch-url]
#   defaults:  jasellite       wgtv       the Whistlegraph TV broadcast
#
# What happens: the station unit + watchdog timer pause, the same rig
# pipeline airs sync-test.html (1kHz beep + white flash on the even-UTC-
# second grid, double flash on :00/:30), yt-dlp records ~45s of the live
# HLS here, the station is restored (EXIT trap — even on failure), and
# blackdetect/silencedetect yield:
#   AV offset   — median(beep − flash); positive = audio LATE.
#   glass delay — double-flash wallclock vs arrival, unambiguous mod 30s.
set -eu
HOST="${1:-jasellite}"
UNIT="${2:-wgtv}"
WATCH="${3:-https://www.youtube.com/watch?v=PhzZS0OEW64}"
DUR=45
CAP=$(mktemp -t sync-cap).mp4

restore() {
  echo "▸ restoring $UNIT on $HOST"
  # Bracketed patterns so pkill can't match this ssh's own command line.
  ssh "$HOST" "sh -c 'pkill -f \"user-data-dir=/home/jas/.tv-rig-chrom[e] \" 2>/dev/null; pkill -f \"Xvfb :9[9]\" 2>/dev/null; pkill -f -- \"-i :9[9] \" 2>/dev/null; sleep 1; systemctl --user start $UNIT $UNIT-watchdog.timer'" || true
}
# EXIT alone doesn't fire on an untrapped signal — a killed probe once left
# the channel dark. Trap the signals and exit through them.
trap restore EXIT
trap 'exit 129' INT TERM

# ⚠️ Known flaw in the YouTube-side capture: the ingest gap during the
# station swap stalls the public HLS edge for several minutes, so the
# recording can wedge at a stale manifest. The rig-side measurement below
# is the reliable A/V number (same inputs, same encoder, no YouTube):
#
#   ssh <host> 'ffmpeg -thread_queue_size 1024 -use_wallclock_as_timestamps 1 \
#     -f x11grab -framerate 30 -video_size <SIZE> -i :99 \
#     -thread_queue_size 1024 -use_wallclock_as_timestamps 1 \
#     -f pulse -fragment_size 1024 -i tvsink.monitor \
#     -af aresample=async=1:first_pts=0 -fps_mode cfr -t 40 \
#     -c:v libx264 -preset veryfast -c:a aac -y /tmp/rig-sync.mp4'
#   …then blackdetect/silencedetect as below. 2026-09-01 baseline measured
#   −65ms (audio early) → AV_OFFSET=0.065 now set in both station envs.

echo "▸ pausing $UNIT and airing the test card…"
ssh "$HOST" "sh -c 'systemctl --user stop $UNIT-watchdog.timer $UNIT; sleep 2;
  grep -v \"^URL=\" $UNIT/$UNIT.env > /tmp/sync.env;
  echo URL=file:///home/jas/wgtv/sync-test.html >> /tmp/sync.env;
  echo UNMUTE_CLICK=1 >> /tmp/sync.env;
  nohup sh $UNIT/rig.sh /tmp/sync.env > $UNIT/sync-rig.log 2>&1 & echo card-rig-up'"
sleep 30

echo "▸ recording ${DUR}s of the live edge…"
HLS=$(yt-dlp -g "$WATCH" | head -1)
T0=$(python3 -c 'import time; print(time.time())')
ffmpeg -v error -i "$HLS" -t "$DUR" -c copy -y "$CAP"

echo "▸ analyzing…"
FL=$(ffmpeg -i "$CAP" -vf blackdetect=d=0.2:pix_th=0.10 -an -f null - 2>&1 |
  grep -o "black_end:[0-9.]*" | cut -d: -f2)
BP=$(ffmpeg -i "$CAP" -af silencedetect=n=-25dB:d=0.3 -vn -f null - 2>&1 |
  grep -o "silence_end: [0-9.]*" | awk '{print $2}')

T0="$T0" FLASHES="$FL" BEEPS="$BP" python3 - <<'EOF'
import os
fl = [float(x) for x in os.environ["FLASHES"].split()]
bp = [float(x) for x in os.environ["BEEPS"].split()]
t0 = float(os.environ["T0"])
if not fl or not bp:
    print(f"✗ not enough events (flashes={len(fl)}, beeps={len(bp)})"); raise SystemExit(1)
offs = []
for b in bp:
    f = min(fl, key=lambda f: abs(f - b))
    if abs(f - b) < 0.6: offs.append(b - f)
offs.sort()
med = offs[len(offs)//2] if offs else None
delay = None
for a, b in zip(fl, fl[1:]):
    if 0.25 < b - a < 0.55:
        delay = (t0 + a) % 30
        break
print(f"flashes={len(fl)} beeps={len(bp)} pairs={len(offs)}")
if med is not None:
    side = "audio LATE (behind picture)" if med > 0 else "audio EARLY (ahead of picture)"
    print(f"AV offset: {med*1000:+.0f} ms — {side}")
    print(f"   spread: {offs[0]*1000:+.0f} … {offs[-1]*1000:+.0f} ms")
if delay is not None:
    print(f"glass-to-glass delay: ~{delay:.1f} s (mod 30; ±2s from capture startup)")
else:
    print("no double-flash captured — extend DUR for a delay reading")
EOF
