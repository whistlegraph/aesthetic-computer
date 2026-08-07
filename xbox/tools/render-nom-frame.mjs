#!/usr/bin/env node

import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";

import { buildNomBundle } from "./bundle-nom.mjs";

const gameOver = process.argv.includes("--over");
const outputArg = process.argv.slice(2).find((arg) => !arg.startsWith("--"));
const output = resolve(outputArg || "tmp/dannom-native-frame.svg");
const commands = [];
const pad = { down: [] };
let now = 1_786_089_600_000;
const host = {
  wipe: (...args) => commands.push(["wipe", ...args]),
  box: (...args) => commands.push(["box", ...args]),
  line: (...args) => commands.push(["line", ...args]),
  write: (...args) => commands.push(["write", ...args]),
  comicWrite: (...args) => commands.push(["text", ...args]),
  synth() {}, oscillator() {}, oscillatorStop() {},
  gameView: () => ({ width: 1280, height: 720 }),
  gamepad: () => ({ connected: true, down: pad.down.slice() }),
  runtime: () => ({ unixMs: now, monotonicUs: now * 1000 }),
  capabilities: () => ({ platform: "xbox-uwp", inputFamily: "xbox" }),
  nomHandle: () => "jeffrey",
  telemetry() {},
};
const names = Object.keys(host);
const lifecycle = new Function(...names,
  `${await buildNomBundle()}\nreturn { boot: globalThis.boot, sim: globalThis.sim, paint: globalThis.paint };`,
)(...Object.values(host));
lifecycle.boot();
for (let frame = 0; frame < 150; frame += 1) {
  now += 1000 / 60;
  lifecycle.sim();
}
if (gameOver) {
  pad.down = ["A"];
  now += 1000 / 60;
  lifecycle.sim();
  pad.down = [];
  for (let frame = 0; frame < 60; frame += 1) {
    now += 1000 / 60;
    lifecycle.sim();
  }
}
lifecycle.paint();

const escape = (value) => String(value).replaceAll("&", "&amp;")
  .replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
const rgb = (r, g, b) => `rgb(${r},${g},${b})`;
const body = commands.map(([kind, ...args]) => {
  if (kind === "wipe")
    return `<rect width="1280" height="720" fill="${rgb(...args)}"/>`;
  if (kind === "box") {
    const [x, y, width, height, r, g, b] = args;
    return `<rect x="${x}" y="${y}" width="${width}" height="${height}" fill="${rgb(r, g, b)}"/>`;
  }
  if (kind === "line") {
    const [x1, y1, x2, y2, width, r, g, b] = args;
    return `<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${rgb(r, g, b)}" stroke-width="${width}"/>`;
  }
  if (kind === "text" || kind === "write") {
    const [value, x, y, size, r = 255, g = 255, b = 255] = args;
    return `<text x="${x}" y="${y + size * .78}" fill="${rgb(r, g, b)}" font-family="Comic Relief" font-size="${size}">${escape(value)}</text>`;
  }
  return "";
}).join("\n");
const font = (await readFile(new URL(
  "../../system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf",
  import.meta.url,
))).toString("base64");
const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="1280" height="720" viewBox="0 0 1280 720">
<style>@font-face{font-family:'Comic Relief';src:url(data:font/ttf;base64,${font})}</style>
${body}
</svg>`;
await writeFile(output, svg);
console.log(output);
