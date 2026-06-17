#!/usr/bin/env node
// fx.mjs — fetch one-shot FX (gong, record scratches) from Freesound into
// c/fx/, decoded to mono 48k wav. cached by file existence.

import { mkdirSync, existsSync, copyFileSync } from "node:fs";
import { dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { searchSounds, downloadPreview } from "../../lib/freesound.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const fxDir = join(HERE, "fx");
mkdirSync(fxDir, { recursive: true });

const WANT = [
  { name: "gong", query: "gong hit", filter: "duration:[1.5 TO 8]" },
  { name: "scratch1", query: "record scratch vinyl", filter: "duration:[0.3 TO 2]" },
  { name: "scratch2", query: "turntable scratch dj", filter: "duration:[0.3 TO 2.5]" },
];

for (const w of WANT) {
  const wav = join(fxDir, `${w.name}.wav`);
  if (existsSync(wav)) { console.log(`✓ ${w.name} cached`); continue; }
  const data = await searchSounds({ query: w.query, filter: w.filter, pageSize: 5, sort: "rating_desc" });
  const hit = (data.results || [])[0];
  if (!hit) { console.warn(`! no result for "${w.query}"`); continue; }
  const mp3 = await downloadPreview(hit);
  const ok = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", mp3,
    "-af", "loudnorm=I=-15:TP=-1.2", "-ac", "1", "-ar", String(SR), wav],
    { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
  console.log(`${ok ? "✓" : "✗"} ${w.name} ← ${hit.name} (#${hit.id}, ${hit.username})`);
}
console.log("# fx ready in c/fx/");
