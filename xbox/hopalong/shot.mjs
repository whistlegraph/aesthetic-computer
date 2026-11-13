#!/usr/bin/env node
// Frames without a browser or a console: run the game against a quiet host,
// rasterise the triangles it hands `triangles3d` (flat colour, z-buffer,
// later wins a tie, as on the host) and write PNGs.
//
//   node xbox/hopalong/shot.mjs [--level 0] [--at 2,8,20] [--title] [--out dir]
//
// --at lists the seconds to capture; the autopilot plays in between.

import { mkdirSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { deflateSync } from "node:zlib";
import { load, quietHost } from "./tests/load.mjs";

const arg = (name, fallback) => { const i = process.argv.indexOf("--" + name); return i < 0 ? fallback : process.argv[i + 1]; };
const out = resolve(arg("out", "xbox/hopalong/shots"));
const level = Number(arg("level", 0)), title = process.argv.includes("--title");
const times = String(arg("at", "2,8,16")).split(",").map(Number);
const SW = 960, SH = 540, scale = SW / 1920;

function crc(bytes) {
  let c = ~0;
  for (const byte of bytes) { c ^= byte; for (let k = 0; k < 8; k++) c = (c >>> 1) ^ (c & 1 ? 0xedb88320 : 0); }
  return ~c >>> 0;
}
function png(rgb, w, h) {
  const raw = Buffer.alloc((w * 3 + 1) * h);
  for (let y = 0; y < h; y++) rgb.copy(raw, y * (w * 3 + 1) + 1, y * w * 3, (y + 1) * w * 3);
  const chunk = (type, data) => {
    const body = Buffer.concat([Buffer.from(type), data]), head = Buffer.alloc(4), tail = Buffer.alloc(4);
    head.writeUInt32BE(data.length); tail.writeUInt32BE(crc(body));
    return Buffer.concat([head, body, tail]);
  };
  const header = Buffer.alloc(13);
  header.writeUInt32BE(w, 0); header.writeUInt32BE(h, 4); header.set([8, 2, 0, 0, 0], 8);
  return Buffer.concat([Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]), chunk("IHDR", header), chunk("IDAT", deflateSync(raw)), chunk("IEND", Buffer.alloc(0))]);
}

function raster(frame) {
  const rgb = Buffer.alloc(SW * SH * 3), depth = new Float32Array(SW * SH).fill(Infinity);
  for (let i = 0; i < SW * SH; i++) rgb.set(frame.wipe, i * 3);
  const f = frame.faces;
  for (let t = 0; t < frame.count; t++) {
    const o = t * 12;
    const x1 = f[o] * scale, y1 = f[o + 1] * scale, z1 = f[o + 2], x2 = f[o + 3] * scale, y2 = f[o + 4] * scale, z2 = f[o + 5];
    const x3 = f[o + 6] * scale, y3 = f[o + 7] * scale, z3 = f[o + 8];
    const area = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1);
    if (!area) continue;
    const r = Math.max(0, Math.min(255, f[o + 9])), g = Math.max(0, Math.min(255, f[o + 10])), b = Math.max(0, Math.min(255, f[o + 11]));
    const minX = Math.max(0, Math.floor(Math.min(x1, x2, x3))), maxX = Math.min(SW - 1, Math.ceil(Math.max(x1, x2, x3)));
    const minY = Math.max(0, Math.floor(Math.min(y1, y2, y3))), maxY = Math.min(SH - 1, Math.ceil(Math.max(y1, y2, y3)));
    for (let y = minY; y <= maxY; y++) for (let x = minX; x <= maxX; x++) {
      const px = x + .5, py = y + .5;
      const a = ((x2 - px) * (y3 - py) - (x3 - px) * (y2 - py)) / area;
      const c = ((x3 - px) * (y1 - py) - (x1 - px) * (y3 - py)) / area;
      const e = 1 - a - c;
      if (a < 0 || c < 0 || e < 0) continue;
      const z = a * z1 + c * z2 + e * z3, at = y * SW + x;
      if (z > depth[at]) continue;
      depth[at] = z; rgb[at * 3] = r; rgb[at * 3 + 1] = g; rgb[at * 3 + 2] = b;
    }
  }
  return png(rgb, SW, SH);
}

mkdirSync(out, { recursive: true });
const { host, frame } = quietHost();
const game = load(host);
game.boot();
if (!title) game.hopalong.start(level, true);
let tick = 0;
for (const seconds of times) {
  while (tick < seconds * 60) { game.sim(); tick++; }
  game.paint();
  const state = game.hopalong.state;
  const file = resolve(out, `${title ? "title" : "stage" + (level + 1)}-${String(seconds).padStart(3, "0")}s.png`);
  writeFileSync(file, raster(frame));
  console.log(`${file}  ${state.mode} x=${state.x.toFixed(1)} y=${state.y.toFixed(1)} ${frame.count} tris`);
}
