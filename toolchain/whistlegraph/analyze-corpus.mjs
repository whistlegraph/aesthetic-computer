#!/usr/bin/env node
// analyze-corpus.mjs — sweep a song corpus: download + musically analyze
// every performance, then map syllables → notes on each.
//
// Per clip: grab.mjs (yt-dlp → wav → analyze.py tempo/key/melody), then
// syllnote.py (whisper words → voiced-nucleus notes, plosive-safe).
// Failures (private/deleted clips) are recorded and skipped, the sweep
// continues. Re-runs only do missing work.
//
//   node analyze-corpus.mjs downloads/imab.corpus.json [--max N]
//   → per-clip .analysis.json + .syllnote.json, status back into the corpus file

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const DL = resolve(HERE, "downloads");
const PY = `${REPO}/pop/.venv/bin/python`;
const argv = process.argv.slice(2);
const corpusPath = resolve(argv.find((a) => !a.startsWith("--")) || "");
const flag = (n, d = null) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const MAX = Number(flag("max", Infinity));

const corpus = JSON.parse(readFileSync(corpusPath, "utf8"));
let done = 0, grabbed = 0, mapped = 0, failed = 0;
for (const clip of corpus.clips) {
  if (done >= MAX) break;
  done++;
  const base = resolve(DL, `whistlegraph-${clip.id}`);
  const wav = `${base}.wav`, analysis = `${base}.analysis.json`, syl = `${base}.syllnote.json`;
  if (!existsSync(analysis) || !existsSync(wav)) {
    if (clip.status === "gone") continue;
    console.log(`→ grab ${clip.id} (${clip.date})`);
    const r = spawnSync("node", [resolve(HERE, "grab.mjs"), clip.url],
      { stdio: ["ignore", "ignore", "inherit"], timeout: 180_000 });
    if (r.status !== 0 || !existsSync(wav)) {
      clip.status = "gone"; failed++;
      writeFileSync(corpusPath, JSON.stringify(corpus, null, 2) + "\n");
      continue;
    }
    grabbed++;
  }
  if (!existsSync(syl)) {
    const r = spawnSync(PY, [resolve(HERE, "syllnote.py"), wav],
      { stdio: ["ignore", "inherit", "inherit"], timeout: 300_000 });
    if (r.status !== 0) { clip.status = "syllnote-failed"; failed++; continue; }
    mapped++;
  }
  clip.status = "analyzed";
  writeFileSync(corpusPath, JSON.stringify(corpus, null, 2) + "\n");
}
const ok = corpus.clips.filter((c) => c.status === "analyzed").length;
console.log(`✓ corpus sweep: ${ok}/${corpus.clips.length} analyzed (${grabbed} fetched, ${mapped} mapped, ${failed} failed this run)`);
