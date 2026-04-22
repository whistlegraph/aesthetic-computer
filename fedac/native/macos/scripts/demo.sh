#!/usr/bin/env bash
# demo.sh — record the full ac-native first-demo:
#   boot animation → prompt → type "notepat" → waltz in notepat
# into a single mkv (1280x800, 60 fps, h264 + aac) with synced audio.
#
# Audio is a stereo mix of:
#   - the host's CoreAudio WAV tap (synth voice playback for every
#     keystroke + notepat's waltz notes)
#   - a macOS TTS greeting rendered by `say` and delayed to match the
#     boot animation's f==10 cue (the Linux kernel does the same)
#
# Env overrides (optional):
#   HANDLE=jeffrey    # the greeting handle (passed as AC_SHOT_HANDLE)
#   CITY="Los Angeles"
#   HOUR=13
#   OUT=~/Desktop/ac-boot-shots/demo.mkv
#   KEEP_STAGES=1     # keep /tmp/ac-demo-final after success
#
# Requires: ffmpeg (brew install ffmpeg), /usr/bin/say, and the
# fedac/native/macos/build/ac-native-macos-core binary.

set -euo pipefail

HANDLE="${HANDLE:-jeffrey}"
CITY="${CITY:-Los Angeles}"
HOUR="${HOUR:-13}"
OUT="${OUT:-$HOME/Desktop/ac-boot-shots/demo.mkv}"
STAGE="${STAGE:-/tmp/ac-demo-final}"

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)
BIN="$REPO_ROOT/fedac/native/macos/build/ac-native-macos-core"
PROMPT="$REPO_ROOT/fedac/native/pieces/prompt.mjs"

[[ -x "$BIN"   ]] || { echo "error: binary missing — run make in fedac/native/macos" >&2; exit 1; }
[[ -f "$PROMPT" ]] || { echo "error: prompt.mjs not found at $PROMPT" >&2; exit 1; }

rm -rf "$STAGE"
mkdir -p "$STAGE/frames"

# Typing timeline — all delays are cumulative offsets from prompt load.
# Boot animation runs for ~2s before this, so the earliest keystroke is
# effectively ~2.5s into the final video.
#
# Layout:
#   prompt typing: n o t e p a t <enter>  (quick, ~1s)
#   ~1.4s pause while notepat loads + settles
#   waltz: 4 bars × 3 beats at 500ms/beat = 6s
#     z c e  (Cm bar)
#     z f a  (Fm bar)
#     z g b  (Gm bar)
#     z c e  (Cm bar back home)
SEQ=(
  "n,500"  "o,120"  "t,120"  "e,120"  "p,120"  "a,120"  "t,120"  "enter,400"
  "z,1380" "c,500"  "e,500"
  "z,500"  "f,500"  "a,500"
  "z,500"  "g,500"  "b,500"
  "z,500"  "c,500"  "e,500"
)
# Join with '|' separators (AC_INJECT_SEQUENCE syntax).
IFS='|' SEQ_STR="${SEQ[*]}"; unset IFS

# Generate the TTS greeting. -r 165 slightly slower than default so it
# reads less urgent; -v Samantha is the long-time macOS US voice.
say -v Samantha -r 165 \
    -o "$STAGE/tts.aiff" \
    "good afternoon $HANDLE. enjoy $CITY."

echo "→ running ac-native for ~12 s …"
AC_HEADLESS_MS=12000 \
  AC_BOOT_ANIM=1 \
  AC_WIN_W=1280 AC_WIN_H=800 \
  AC_SHOT_HANDLE="$HANDLE" \
  AC_SHOT_CITY="$CITY" \
  AC_SHOT_HOUR="$HOUR" \
  AC_FRAME_DUMP_DIR="$STAGE/frames" \
  AC_WAV_OUT="$STAGE/synth.wav" \
  AC_INJECT_SEQUENCE="$SEQ_STR" \
  "$BIN" "$PROMPT" 2>&1 | sed 's/^/  [ac] /'

N_FRAMES=$(ls "$STAGE/frames" | wc -l | tr -d ' ')
echo "→ captured $N_FRAMES frames + $(du -h "$STAGE/synth.wav" | cut -f1) synth WAV"

# Mix synth + TTS into one track. TTS starts at 167ms to match f==10 of
# the boot animation (where Linux kicks the greeting off). `-filter_complex`
# delays TTS and mixes equal-loudness; `duration=longest` keeps whichever
# track runs longest as the final length.
echo "→ mixing synth + TTS …"
ffmpeg -y \
  -i "$STAGE/synth.wav" \
  -i "$STAGE/tts.aiff" \
  -filter_complex "[1]adelay=167|167,volume=1.3[tts]; [0:a][tts]amix=inputs=2:duration=longest:dropout_transition=0[out]" \
  -map "[out]" "$STAGE/mix.wav" 2>&1 | tail -2

echo "→ encoding mkv …"
mkdir -p "$(dirname "$OUT")"
ffmpeg -y \
  -framerate 60 -i "$STAGE/frames/frame_%05d.png" \
  -i "$STAGE/mix.wav" \
  -c:v libx264 -pix_fmt yuv420p -crf 18 -preset slow \
  -c:a aac -b:a 192k \
  "$OUT" 2>&1 | tail -2

echo
echo "demo ready: $OUT ($(du -h "$OUT" | cut -f1))"
ffprobe -v error -show_entries format=duration,bit_rate "$OUT" 2>&1 | sed 's/^/  /'

if [[ -z "${KEEP_STAGES:-}" ]]; then
  rm -rf "$STAGE"
fi
