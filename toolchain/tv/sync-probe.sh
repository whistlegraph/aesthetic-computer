#!/bin/sh
# sync-probe.sh — end-to-end AV-sync + latency measurement for a TV station.
#
# Airs the sync test card (sync-test.html: 1kHz beep + white flash on the
# even-UTC-second grid, double-flash on :00/:30) through the REAL pipeline
# (same rig, same encoder, same stream key), records the actual YouTube
# HLS a viewer would receive, and reads the numbers back out:
#
#   AV offset   — median(beep onset − flash onset) via silencedetect and
#                 blackdetect. Positive = audio LATE (behind picture).
#   glass delay — wallclock between a double-flash airing and it arriving
#                 at the HLS live edge (unambiguous modulo 30s).
#
#   sync-probe.sh <station-dir> <watch-url>   # e.g. ~/wgtv https://www.youtube.com/watch?v=…
#
# The station unit + its watchdog timer are stopped for the ~90s test and
# restarted after, so the broadcast shows a brief test card — honest TV.
set -eu
STATION_DIR="${1:?usage: sync-probe.sh <station-dir> <watch-url>}"
WATCH_URL="${2:?usage: sync-probe.sh <station-dir> <watch-url>}"
UNIT=$(basename "$STATION_DIR")
CAP="$STATION_DIR/sync-capture.mp4"
DUR=45

echo "▸ probing $UNIT → $WATCH_URL"
systemctl --user stop "$UNIT-watchdog.timer" "$UNIT" 2>/dev/null || true
sleep 2

# Test-card env: the station's own settings, page swapped for the card.
ENVT=$(mktemp)
grep -v "^URL=" "$STATION_DIR/$UNIT.env" > "$ENVT"
echo "URL=file://$HOME/wgtv/sync-test.html" >> "$ENVT"
echo "UNMUTE_CLICK=1" >> "$ENVT"

sh "$STATION_DIR/rig.sh" "$ENVT" > "$STATION_DIR/sync-rig.log" 2>&1 &
RIG_PID=$!
echo "▸ test card rig up (pid $RIG_PID), waiting for ingest…"
sleep 30

HLS=$("$HOME/bin/yt-dlp" -g "$WATCH_URL" 2>/dev/null | head -1)
[ -n "$HLS" ] || { echo "✗ no HLS url"; kill "$RIG_PID" 2>/dev/null; exit 1; }
T0=$(date +%s.%N)
echo "▸ recording ${DUR}s of the live edge…"
ffmpeg -v error -i "$HLS" -t "$DUR" -c copy -y "$CAP"

kill "$RIG_PID" 2>/dev/null || true
pkill -f "user-data-dir=$HOME/.tv-rig-chrome" 2>/dev/null || true
pkill -f "Xvfb :99" 2>/dev/null || true
sleep 2
systemctl --user start "$UNIT" "$UNIT-watchdog.timer"
echo "▸ station restored; analyzing…"

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
# AV offset: each beep pairs with its nearest flash.
offs = []
for b in bp:
    f = min(fl, key=lambda f: abs(f - b))
    if abs(f - b) < 0.6: offs.append(b - f)
offs.sort()
med = offs[len(offs)//2] if offs else None
# Glass-to-glass: find a double flash (two onsets ~0.4s apart) → its first
# onset sat on a :00/:30 boundary when it aired.
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
