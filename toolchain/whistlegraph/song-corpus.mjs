#!/usr/bin/env node
// song-corpus.mjs — find every TikTok performance of one song by lyric.
//
// Greps downloads/TRANSCRIPTS.json (whisper transcripts of the whole
// @whistlegraph account) for a pattern and writes a corpus file that
// analyze-corpus.mjs can sweep.
//
//   node song-corpus.mjs "butterfl|flapping" --slug imab
//   → downloads/imab.corpus.json

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const DL = resolve(HERE, "downloads");
const argv = process.argv.slice(2);
const pattern = argv.find((a) => !a.startsWith("--"));
const flag = (n, d = null) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
if (!pattern) { console.error(`usage: node song-corpus.mjs <regex> [--slug name]`); process.exit(1); }
const slug = flag("slug", pattern.replace(/[^a-z0-9]+/gi, "-").toLowerCase());

const t = JSON.parse(readFileSync(resolve(DL, "TRANSCRIPTS.json"), "utf8"));
const re = new RegExp(pattern, "i");
const clips = t.videos
  .filter((v) => re.test(v.text || ""))
  .map((v) => ({ id: v.id, url: v.url || `https://www.tiktok.com/@whistlegraph/video/${v.id}`,
                 date: v.date, text: (v.text || "").slice(0, 400) }));
const out = resolve(DL, `${slug}.corpus.json`);
writeFileSync(out, JSON.stringify({ slug, pattern, generated: new Date().toISOString(),
  count: clips.length, clips }, null, 2) + "\n");
console.log(`✓ ${out} · ${clips.length} clips (${clips[0]?.date} → ${clips[clips.length - 1]?.date})`);
