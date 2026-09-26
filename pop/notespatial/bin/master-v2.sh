#!/usr/bin/env bash
# pop/notespatial/bin/master-v2.sh — the second master: air, a room, a print that
# holds up on Spotify. Two stages. A shapes; B is the house law (one static dB
# into an oversampled true-peak limiter).
#   bash master-v2.sh <print.wav> <out.flac> [target_lufs=-13] [ceiling_dbtp=-2.2]
#
# A · 20 Hz high-pass → diffuse-field EQ for the MIT KEMAR head (undoes the
#     ear-canal resonance and the set's low roll-off; headphones add their own)
#     → the chapter arc narrowed 1.7:1 (bin/chapter-gain.mjs) → a synthetic
#     room (bin/room-ir.mjs) mixed under the dry print → slow glue (1.5:1,
#     40/500 ms, 6 dB knee) → bass +1.5 dB @ 90 → air +2.5 dB @ 9 k →
#     bass mono under 120 Hz.
# B · measure → static gain to the target → 4× oversampled limiter → 24-bit FLAC.
set -euo pipefail
IN="$1"; OUT="$2"; TARGET="${3:--13}"; CEIL="${4:--2.2}"
# taste knobs (env): ROOM wet weight, AIR dB @ 8 k, MID scale of the ear-canal cut (1 = measured), GLUE ratio
ROOM="${ROOM:-0.18}"; DIRT="${DIRT:-0.22}"; PAD="${PAD:-0}"; PAD_FADE="${PAD_FADE:-0.001}"; ARC="${ARC:-1}"; AIR="${AIR:-3}"; MID="${MID:-0.65}"; GLUE="${GLUE:-2}"; KEEP_STAGE="${KEEP_STAGE:-}"
HERE="$(cd "$(dirname "$0")" && pwd)"; O="$HERE/../out"
RT="${RT:-1}"; IR="$O/room-ir-rt$RT.wav"; [ -f "$IR" ] || node "$HERE/room-ir.mjs" "$IR" "$RT" >/dev/null
if [ "$ARC" = "1" ]; then EXPR="$(node "$HERE/chapter-gain.mjs" "$IN" 1.7 -20 2>/dev/null)"; else EXPR="1"; fi  # ARC=0 for an excerpt: no chapter map
DFEQ="entry(80,3);entry(125,2.5);entry(160,2);entry(200,1.6);entry(250,1.2);entry(315,1.2);entry(400,1.2);entry(500,.8);entry(630,0.3);entry(800,0.3);entry(1000,0);entry(1250,$(python3 -c "print(round(-0.6*$MID,2))"));entry(1600,$(python3 -c "print(round(-3*$MID,2))"));entry(2000,$(python3 -c "print(round(-7.9*$MID,2))"));entry(2500,$(python3 -c "print(round(-9*$MID,2))"));entry(3150,$(python3 -c "print(round(-5.8*$MID,2))"));entry(4000,$(python3 -c "print(round(-4.6*$MID,2))"));entry(5000,$(python3 -c "print(round(-1.8*$MID,2))"));entry(6300,0.5);entry(8000,2);entry(10000,1);entry(11000,-2);entry(12500,-7);entry(14000,-6);entry(16000,0);entry(20000,0)"
STAGE="$O/.v2-stage.wav"
cat > "$O/.v2-graph.txt" <<G
[0:a]apad=pad_dur=$PAD,highpass=f=32:p=2,
firequalizer=gain_entry='$DFEQ':min_phase=on,
volume='$EXPR':eval=frame,
asplit=3[dry][s][d];
[s][1:a]afir=dry=0:wet=1[wet];
[d]highpass=f=180:p=2,lowpass=f=4500:p=2,asoftclip=type=tanh:threshold=0.35:output=1:oversample=4[dirt];
[dry][wet][dirt]amix=inputs=3:weights='1 $ROOM $DIRT':normalize=0,
acompressor=threshold=0.05:ratio=$GLUE:attack=40:release=500:knee=8:makeup=1,
bass=g=0.5:f=90:w=0.6,treble=g=$AIR:f=8000:w=0.7,
asplit[a][b];
[a]pan=mono|c0=0.5*c0+0.5*c1[m];
[b]pan=mono|c0=0.5*c0-0.5*c1,highpass=f=120:p=2[sd];
[m][sd]join=inputs=2:channel_layout=stereo,pan=stereo|c0=c0+c1|c1=c0-c1,areverse,afade=t=in:d=$PAD_FADE,areverse[out]
G
ffmpeg -y -v error -i "$IN" -i "$IR" -filter_complex "$(tr -d "\n" < "$O/.v2-graph.txt")" -map "[out]" -c:a pcm_f32le "$STAGE"
I=$(ffmpeg -hide_banner -nostats -i "$STAGE" -af ebur128 -f null - 2>&1 | grep -E "^\s+I:" | awk '{print $2}')
GAIN=$(python3 -c "print(round($TARGET-($I),2))"); LIM=$(python3 -c "print(round(10**($CEIL/20),4))")
echo "stage A: $I LUFS → static ${GAIN} dB, ceiling $CEIL dBTP"
ffmpeg -y -v error -i "$STAGE" -af "volume=${GAIN}dB,aresample=176400,alimiter=limit=${LIM}:attack=6:release=90:asc=1:level=0,aresample=44100" -c:a flac -sample_fmt s32 -bits_per_raw_sample 24 "$OUT"
[ -n "$KEEP_STAGE" ] && cp "$STAGE" "${OUT%.flac}.stage.wav"; rm -f "$STAGE"; echo "$OUT"
