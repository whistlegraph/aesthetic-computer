#!/bin/sh
# Word-level transcripts for every downloaded take (whisper.cpp
# ggml-small.en, -ml 1 sub-word tokens, JSON) into analysis/whisper-raw/.
# Skips takes already transcribed. WHISPER_MODEL overrides the model path.
set -e
cd "$(dirname "$0")/.."
MODEL="${WHISPER_MODEL:-$HOME/Models/ggml-small.en.bin}"
[ -f "$MODEL" ] || { echo "model not found: $MODEL (set WHISPER_MODEL)"; exit 1; }
mkdir -p analysis/whisper-raw
for wav in source/flwe-*.wav; do
  id=$(basename "$wav" .wav)
  out="analysis/whisper-raw/$id.json"
  [ -f "$out" ] && continue
  tmp=$(mktemp -t flwe16k).wav
  ffmpeg -y -loglevel error -i "$wav" -ac 1 -ar 16000 "$tmp"
  whisper-cli -m "$MODEL" -f "$tmp" -ml 1 -oj -of "analysis/whisper-raw/$id" >/dev/null
  rm -f "$tmp"
  echo "transcribed $id"
done
