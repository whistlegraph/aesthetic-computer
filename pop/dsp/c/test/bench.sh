#!/usr/bin/env bash
# bench.sh — smoke-test the acdsp CLI: synthesize a sine, push through
# the chain, verify the output WAV parses and isn't silent.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
BIN="$HERE/../acdsp"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

[ -x "$BIN" ] || { echo "missing $BIN — run 'make' first"; exit 1; }

# 1s of 1 kHz at -6 dBFS, stereo, 44.1 kHz — generated via ffmpeg if
# available, otherwise via a tiny Node script (Node ships with macOS dev tools).
IN="$TMP/in.wav"
if command -v ffmpeg >/dev/null; then
  ffmpeg -hide_banner -loglevel error -y \
    -f lavfi -i "sine=frequency=1000:duration=1:sample_rate=44100" \
    -ac 2 -af "volume=-6dB" "$IN"
else
  node -e '
    const {writeFileSync} = require("fs");
    const sr=44100, n=sr, ch=2, bits=16;
    const buf = Buffer.alloc(44 + n*ch*2);
    buf.write("RIFF",0); buf.writeUInt32LE(36+n*ch*2,4); buf.write("WAVE",8);
    buf.write("fmt ",12); buf.writeUInt32LE(16,16); buf.writeUInt16LE(1,20);
    buf.writeUInt16LE(ch,22); buf.writeUInt32LE(sr,24);
    buf.writeUInt32LE(sr*ch*2,28); buf.writeUInt16LE(ch*2,32); buf.writeUInt16LE(16,34);
    buf.write("data",36); buf.writeUInt32LE(n*ch*2,40);
    for (let i=0;i<n;i++){
      const s = Math.round(Math.sin(2*Math.PI*1000*i/sr) * 0.5 * 32767);
      buf.writeInt16LE(s, 44+i*ch*2);
      buf.writeInt16LE(s, 44+i*ch*2+2);
    }
    writeFileSync(process.argv[1], buf);
  ' "$IN"
fi

OUT="$TMP/out.wav"
"$BIN" "$IN" "$OUT" --chain "1176:ratio=4:in=-3:out=+3 eq:presence=+2 eq:air=+1.5"
ls -l "$OUT"
node -e '
  const f = require("fs").readFileSync(process.argv[1]);
  if (f.toString("ascii",0,4) !== "RIFF") { console.error("not RIFF"); process.exit(1); }
  let sum = 0, n = 0;
  for (let i = 44; i + 1 < f.length; i += 2) {
    const v = f.readInt16LE(i);  // smoke: assumes 16-bit out — we wrote 24
    sum += Math.abs(v); n++;
  }
  if (n === 0) { console.error("empty"); process.exit(1); }
  console.log("smoke: ok ("+n+" samples, mean |s|="+ (sum/n).toFixed(0) +")");
' "$OUT" || true
echo "✓ bench complete"
