// wav.mjs — Node-side WAV file IO for /pop. Kept separate from
// lib/analysis.mjs so the DSP there stays pure (browser-importable).

import { readFileSync } from "node:fs";

const DEFAULT_SAMPLE_RATE = 48_000;

// Reads a RIFF/WAVE file to a mono Float32Array. Supports 16-bit, 24-bit
// and 32-bit-float PCM; stereo (or more) is downmixed by averaging.
export function readWavMono(path) {
  const buf = readFileSync(path);
  if (buf.toString("ascii", 0, 4) !== "RIFF")
    throw new Error(`not a RIFF file: ${path}`);

  let channels = 1, sampleRate = DEFAULT_SAMPLE_RATE, bits = 16, fmt = 1;
  let i = 12;
  while (i < buf.length - 8) {
    const id = buf.toString("ascii", i, i + 4);
    const size = buf.readUInt32LE(i + 4);
    const body = i + 8;
    if (id === "fmt ") {
      fmt        = buf.readUInt16LE(body);
      channels   = buf.readUInt16LE(body + 2);
      sampleRate = buf.readUInt32LE(body + 4);
      bits       = buf.readUInt16LE(body + 14);
    } else if (id === "data") {
      const bytesPerSample = bits / 8;
      const frames = Math.floor(size / (bytesPerSample * channels));
      const out = new Float32Array(frames);
      for (let f = 0; f < frames; f++) {
        let acc = 0;
        for (let c = 0; c < channels; c++) {
          const o = body + (f * channels + c) * bytesPerSample;
          if (fmt === 3 && bits === 32)      acc += buf.readFloatLE(o);
          else if (bits === 32)              acc += buf.readInt32LE(o) / 2147483648;
          else if (bits === 24)              acc += ((buf.readUInt8(o) | (buf.readUInt8(o + 1) << 8) | (buf.readInt8(o + 2) << 16))) / 8388608;
          else                               acc += buf.readInt16LE(o) / 32768;
        }
        out[f] = acc / channels;
      }
      return { samples: out, sampleRate };
    }
    i = body + size + (size & 1); // chunks are word-aligned
  }
  throw new Error(`no data chunk in ${path}`);
}
