// minimal RIFF/WAVE read + write. float samples in, float samples out.

import { readFileSync, writeFileSync } from "node:fs";

const { max, min, round } = Math;

export function write(path, samples, rate = 44100, bits = 16) {
  const bytes = bits >> 3;
  const data = Buffer.alloc(samples.length * bytes);
  const peak = (1 << (bits - 1)) - 1;
  for (let i = 0; i < samples.length; i += 1) {
    const v = round(max(-1, min(1, samples[i])) * peak);
    if (bits === 16) data.writeInt16LE(v, i * 2);
    else if (bits === 24) {
      const u = v < 0 ? v + 0x1000000 : v;
      data.writeUIntLE(u, i * 3, 3);
    } else data.writeInt32LE(v, i * 4);
  }
  const head = Buffer.alloc(44);
  head.write("RIFF", 0);
  head.writeUInt32LE(36 + data.length, 4);
  head.write("WAVE", 8);
  head.write("fmt ", 12);
  head.writeUInt32LE(16, 16);
  head.writeUInt16LE(1, 20); // pcm
  head.writeUInt16LE(1, 22); // mono
  head.writeUInt32LE(rate, 24);
  head.writeUInt32LE(rate * bytes, 28);
  head.writeUInt16LE(bytes, 32);
  head.writeUInt16LE(bits, 34);
  head.write("data", 36);
  head.writeUInt32LE(data.length, 40);
  writeFileSync(path, Buffer.concat([head, data]));
}

export function read(path) {
  const buf = readFileSync(path);
  if (buf.toString("latin1", 0, 4) !== "RIFF") throw new Error("not a wav");
  let pos = 12,
    fmt = null,
    data = null;
  while (pos + 8 <= buf.length) {
    const id = buf.toString("latin1", pos, pos + 4);
    const size = buf.readUInt32LE(pos + 4);
    const body = pos + 8;
    if (id === "fmt ")
      fmt = {
        format: buf.readUInt16LE(body),
        channels: buf.readUInt16LE(body + 2),
        rate: buf.readUInt32LE(body + 4),
        bits: buf.readUInt16LE(body + 14),
      };
    else if (id === "data") data = buf.subarray(body, body + size);
    pos = body + size + (size & 1);
  }
  if (!fmt || !data) throw new Error("wav missing fmt or data");

  const bytes = fmt.bits >> 3;
  const frames = Math.floor(data.length / bytes / fmt.channels);
  const out = new Float64Array(frames);
  const peak = (1 << (fmt.bits - 1)) - 1;
  for (let i = 0; i < frames; i += 1) {
    const o = i * bytes * fmt.channels; // mix down to mono by taking ch 0
    if (fmt.format === 3) out[i] = data.readFloatLE(o);
    else if (fmt.bits === 16) out[i] = data.readInt16LE(o) / peak;
    else if (fmt.bits === 24) out[i] = data.readIntLE(o, 3) / peak;
    else out[i] = data.readInt32LE(o) / peak;
  }
  return { samples: out, rate: fmt.rate };
}
