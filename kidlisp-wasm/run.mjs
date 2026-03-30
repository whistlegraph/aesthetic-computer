#!/usr/bin/env node
// KidLisp WASM Runner
// Compiles a .lisp file, runs the WASM, outputs a PPM image.

import { readFileSync, writeFileSync } from "fs";
import { Compiler } from "./compiler.mjs";

const WIDTH = 128;
const HEIGHT = 128;

// ─── Pixel Buffer ───────────────────────────────────────────────────

const fb = new Uint8Array(WIDTH * HEIGHT * 4); // RGBA

function setPixel(x, y, r, g, b) {
  x = Math.round(x);
  y = Math.round(y);
  if (x < 0 || x >= WIDTH || y < 0 || y >= HEIGHT) return;
  const i = (y * WIDTH + x) * 4;
  fb[i] = r;
  fb[i + 1] = g;
  fb[i + 2] = b;
  fb[i + 3] = 255;
}

// ─── Drawing State ──────────────────────────────────────────────────

let inkR = 255,
  inkG = 255,
  inkB = 255;

// ─── Host Functions ─────────────────────────────────────────────────

function wipe(r, g, b) {
  r = Math.round(r);
  g = Math.round(g);
  b = Math.round(b);
  for (let i = 0; i < WIDTH * HEIGHT * 4; i += 4) {
    fb[i] = r;
    fb[i + 1] = g;
    fb[i + 2] = b;
    fb[i + 3] = 255;
  }
}

function ink(r, g, b) {
  inkR = Math.round(r);
  inkG = Math.round(g);
  inkB = Math.round(b);
}

function plot(x, y) {
  setPixel(x, y, inkR, inkG, inkB);
}

function line(x0, y0, x1, y1) {
  x0 = Math.round(x0);
  y0 = Math.round(y0);
  x1 = Math.round(x1);
  y1 = Math.round(y1);
  const dx = Math.abs(x1 - x0);
  const dy = Math.abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1;
  const sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;
  while (true) {
    setPixel(x0, y0, inkR, inkG, inkB);
    if (x0 === x1 && y0 === y1) break;
    const e2 = 2 * err;
    if (e2 > -dy) {
      err -= dy;
      x0 += sx;
    }
    if (e2 < dx) {
      err += dx;
      y0 += sy;
    }
  }
}

function box(x, y, w, h) {
  x = Math.round(x);
  y = Math.round(y);
  w = Math.round(w);
  h = Math.round(h);
  for (let py = y; py < y + h; py++) {
    for (let px = x; px < x + w; px++) {
      setPixel(px, py, inkR, inkG, inkB);
    }
  }
}

function circle(cx, cy, r) {
  cx = Math.round(cx);
  cy = Math.round(cy);
  r = Math.round(r);
  for (let y = -r; y <= r; y++) {
    for (let x = -r; x <= r; x++) {
      if (x * x + y * y <= r * r) {
        setPixel(cx + x, cy + y, inkR, inkG, inkB);
      }
    }
  }
}

function tri(x0, y0, x1, y1, x2, y2) {
  // Scanline triangle fill
  x0 = Math.round(x0); y0 = Math.round(y0);
  x1 = Math.round(x1); y1 = Math.round(y1);
  x2 = Math.round(x2); y2 = Math.round(y2);
  const minY = Math.max(0, Math.min(y0, y1, y2));
  const maxY = Math.min(HEIGHT - 1, Math.max(y0, y1, y2));
  for (let y = minY; y <= maxY; y++) {
    let minX = WIDTH, maxX = 0;
    const edges = [[x0,y0,x1,y1],[x1,y1,x2,y2],[x2,y2,x0,y0]];
    for (const [ax,ay,bx,by] of edges) {
      if ((ay <= y && by > y) || (by <= y && ay > y)) {
        const t = (y - ay) / (by - ay);
        const x = Math.round(ax + t * (bx - ax));
        if (x < minX) minX = x;
        if (x > maxX) maxX = x;
      }
    }
    for (let x = Math.max(0, minX); x <= Math.min(WIDTH - 1, maxX); x++) {
      setPixel(x, y, inkR, inkG, inkB);
    }
  }
}

// ─── Compile & Run ──────────────────────────────────────────────────

const input = process.argv[2] || "hello.lisp";
const source = readFileSync(
  new URL(input, import.meta.url).pathname,
  "utf-8",
);

console.log(`Compiling ${input}...`);
const compiler = new Compiler();
const wasmBytes = compiler.compile(source);
console.log(`WASM binary: ${wasmBytes.length} bytes`);

const { instance } = await WebAssembly.instantiate(wasmBytes, {
  env: { wipe, ink, line, box, circle, plot, tri },
});

console.log("Running paint...");
instance.exports.paint(WIDTH, HEIGHT, 0);

// ─── Output PPM ─────────────────────────────────────────────────────

const ppm = Buffer.alloc(15 + WIDTH * HEIGHT * 3); // header + pixels
const header = `P6\n${WIDTH} ${HEIGHT}\n255\n`;
ppm.write(header);
let offset = header.length;
for (let i = 0; i < WIDTH * HEIGHT * 4; i += 4) {
  ppm[offset++] = fb[i];
  ppm[offset++] = fb[i + 1];
  ppm[offset++] = fb[i + 2];
}

const outFile = input.replace(/\.lisp$/, ".ppm");
writeFileSync(new URL(outFile, import.meta.url).pathname, ppm.slice(0, offset));
console.log(`Wrote ${outFile} (${WIDTH}x${HEIGHT})`);

// ─── Terminal Preview (ANSI) ────────────────────────────────────────

const PREVIEW_W = Math.min(WIDTH, 64);
const scaleX = WIDTH / PREVIEW_W;
const scaleY = (HEIGHT / PREVIEW_W) * 2; // 2 rows per char with ▀

console.log(`\nPreview (${PREVIEW_W} cols):`);
for (let row = 0; row < PREVIEW_W; row++) {
  let line = "";
  for (let col = 0; col < PREVIEW_W; col++) {
    const tx = Math.floor(col * scaleX);
    const ty = Math.floor(row * scaleY);
    const by = Math.floor(row * scaleY + scaleY / 2);
    const ti = (ty * WIDTH + tx) * 4;
    const bi = (Math.min(by, HEIGHT - 1) * WIDTH + tx) * 4;
    line += `\x1b[38;2;${fb[ti]};${fb[ti + 1]};${fb[ti + 2]};48;2;${fb[bi]};${fb[bi + 1]};${fb[bi + 2]}m\u2580`;
  }
  line += "\x1b[0m";
  process.stdout.write(line + "\n");
}
