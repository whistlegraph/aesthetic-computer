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
# WALTZ picks a drum pattern from scripts/waltz-seq.py (classic, dark,
# dreamy, baroque, minimal, phonk, viennese, drill). BPM + BARS control
# tempo and length. When WALTZ=melody the legacy oom-pah-pah melody
# sequence plays instead — useful for quick A/B vs the drum grids.
MODE="${MODE:-waltz}"          # waltz | bachbeat
WALTZ="${WALTZ:-classic}"      # classic, dark, dreamy, baroque, minimal, phonk, viennese, drill
BPM="${BPM:-120}"              # waltz default 120; bachbeat default 100
BARS="${BARS:-4}"              # waltz bar count
BACH_EVENTS="${BACH_EVENTS:-48}"  # bachbeat: how many bach notes to play
# GRID chooses which hand plays drums. Default "right" = right-hand kit
# + left-hand melody (classic live-piano setup). "left" reverses it.
GRID="${GRID:-right}"
# MELODY=on interleaves an oom-pah-pah bass+chord line on the OTHER
# grid so notes and drums play simultaneously (waltz mode only).
MELODY="${MELODY:-on}"
# KIT_VOL down-ticks for the perc kit after it's enabled — each tick
# drops ~10% volume (notepat arrowdown). 3 = kit ~70%, 5 = ~50%.
KIT_MIXDOWN="${KIT_MIXDOWN:-3}"
case "$MODE" in
  bachbeat) OUT_DEFAULT="$HOME/Desktop/ac-boot-shots/demo-bachbeat.mkv" ;;
  *)        OUT_DEFAULT="$HOME/Desktop/ac-boot-shots/demo-$WALTZ.mkv" ;;
esac
OUT="${OUT:-$OUT_DEFAULT}"
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
#   0.5s  prompt typing: n o t e p a t <enter>  (quick, ~1s)
#   1.4s  pause while notepat loads + settles
#   +     pageup/pagedown flips LEFT or RIGHT grid into the perc kit
#   +     0.4s breath before the groove kicks in
#   + generated drum sequence (+ optional interleaved melody)

# GRID=right  → pagedown (right grid → perc, left grid stays melodic)
# GRID=left   → pageup   (left grid → perc, right grid stays melodic)
if [[ "$GRID" == "left" ]]; then
  KIT_KEY="pageup"
  SELECT_KEY="arrowleft"   # so arrow-down mixes LEFT grid
  PREVIEW_KEY="c"          # left-grid kick preview
else
  KIT_KEY="pagedown"
  SELECT_KEY="arrowright"  # so arrow-down mixes RIGHT grid
  PREVIEW_KEY="h"          # right-grid kick preview (+c → h)
fi

# Build the kit-mixdown prefix: select the kit side, then fire arrowdown
# $KIT_MIXDOWN times to reduce the perc volume before the beat kicks in.
MIX_SEQ="$KIT_KEY,1400|$SELECT_KEY,200"
for ((i=0; i<KIT_MIXDOWN; i++)); do
  MIX_SEQ+="|arrowdown,100"
done
MIX_SEQ+="|$PREVIEW_KEY,400"

PROMPT_SEQ="n,500|o,120|t,120|e,120|p,120|a,120|t,120|enter,400|$MIX_SEQ"

case "$MODE" in
  bachbeat)
    # Default BPM 100 for bachbeat feels right with the baroque meter.
    BB_BPM=${BPM:-100}
    DRUM_SEQ="$("$(dirname "${BASH_SOURCE[0]}")"/bachbeat-seq.py \
               "$BB_BPM" "$BACH_EVENTS" 300 \
               "$([[ "$GRID" == "left" ]] && echo right || echo left)")"
    ;;
  melody)
    DRUM_SEQ="z,500|c,500|e,500|z,500|f,500|a,500|z,500|g,500|b,500|z,500|c,500|e,500"
    ;;
  *)
    # start_offset_ms = 300 gives a clean breath after the preview kick.
    DRUM_SEQ="$("$(dirname "${BASH_SOURCE[0]}")"/waltz-seq.py \
               "$WALTZ" "$BPM" "$BARS" 300 "$GRID" "$MELODY")"
    ;;
esac
SEQ_STR="$PROMPT_SEQ|$DRUM_SEQ"

# Generate the TTS greeting. -r 165 slightly slower than default so it
# reads less urgent; -v Samantha is the long-time macOS US voice.
say -v Samantha -r 165 \
    -o "$STAGE/tts.aiff" \
    "good afternoon $HANDLE. enjoy $CITY."

# Estimate runtime so AC_HEADLESS_MS doesn't cut the last hits. Prompt
# typing is ~1.6s; kit-switch + mixdown + preview is ~(1.6 + 0.1×mix + 0.4)s.
mix_ms=$(( 1400 + 200 + 100 * KIT_MIXDOWN + 400 ))
case "$MODE" in
  bachbeat)
    # Bachbeat note count × per-note duration. Per-note ~= 60000/bpm * (ticks/480).
    # MIDI_EVENTS are all 120 ticks ≈ 0.25 beat, so per-note ~= 15000/bpm.
    body_ms=$(( BACH_EVENTS * 15000 / BPM ))
    label="bachbeat events=$BACH_EVENTS bpm=$BPM"
    ;;
  *)
    body_ms=$(( BARS * 3 * 60000 / BPM ))
    label="waltz=$WALTZ bpm=$BPM bars=$BARS"
    ;;
esac
run_ms=$(( 2000 + 1620 + mix_ms + body_ms + 1800 ))
echo "→ running ac-native for ~$(( run_ms / 1000 )) s ($label grid=$GRID mixdown=$KIT_MIXDOWN) …"
AC_HEADLESS_MS=$run_ms \
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
