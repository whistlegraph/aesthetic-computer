#!/usr/bin/env node
// auto-align.mjs — run whisper-cli on every source mp3, find every word
// matching the group target ("america" / "dora"), and fill clips.json
// automatically with start/end timestamps.
//
// caches whisper output next to each source as <source>-words.json. re-runs
// only if the source is newer.
//
// usage:
//   node bin/auto-align.mjs                # transcribe all + write clips.json
//   node bin/auto-align.mjs --force        # ignore cache, re-whisper

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, readdirSync, statSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);
const SOURCES_DIR = join(ROOT, "sources");
const MODEL = "/Users/jas/aesthetic-computer/recap/models/ggml-base.en.bin";

const argv = process.argv.slice(2);
const FORCE = argv.includes("--force");

if (!existsSync(MODEL)) {
  console.error(`✗ missing whisper model: ${MODEL}`);
  process.exit(1);
}

// what word we look for in each group
const TARGET_WORD = {
  america: /^america[!?,.]?$/i,
  dora:    /^dora[!?,.]?$/i,
  // soy-dora (spanish source) — also accept "dora" alone
};

function whisperWords(srcMp3) {
  const sib = srcMp3.replace(/\.mp3$/, "-words.json");
  const sibStat = existsSync(sib) ? statSync(sib).mtimeMs : 0;
  const srcStat = statSync(srcMp3).mtimeMs;
  if (!FORCE && existsSync(sib) && sibStat >= srcStat) {
    return JSON.parse(readFileSync(sib, "utf8"));
  }
  console.log(`  → whisper-cli ${basename(srcMp3)}`);
  // whisper-cli writes <base>.json alongside when --output-json is set.
  // we'll use -ojf (full json, per-token timing) and --max-len 1 to force word-level.
  const baseNoExt = srcMp3.replace(/\.mp3$/, "");
  const tmpJson = `${baseNoExt}.json`;
  const res = spawnSync("whisper-cli", [
    "-m", MODEL,
    "-f", srcMp3,
    "-ojf",
    "-of", baseNoExt,
    "--max-len", "1",
    "-ml", "1",
    "-sow",
  ], { stdio: ["ignore", "ignore", "inherit"] });
  if (res.status !== 0) {
    console.error(`  ! whisper-cli failed for ${basename(srcMp3)}`);
    return null;
  }
  if (!existsSync(tmpJson)) {
    console.error(`  ! whisper-cli produced no json for ${basename(srcMp3)}`);
    return null;
  }
  const raw = JSON.parse(readFileSync(tmpJson, "utf8"));
  // normalize to [{text, fromMs, toMs}]
  const words = [];
  for (const seg of raw.transcription || []) {
    const text = (seg.text || "").trim();
    if (!text) continue;
    const fromMs = seg.offsets?.from ?? Math.round((seg.timestamps?.from || 0));
    const toMs   = seg.offsets?.to   ?? Math.round((seg.timestamps?.to   || 0));
    words.push({ text, fromMs, toMs });
  }
  writeFileSync(sib, JSON.stringify(words, null, 2));
  try { unlinkSync(tmpJson); } catch {}
  return words;
}

function findOccurrences(words, regex) {
  const hits = [];
  for (const w of words) {
    if (regex.test(w.text)) {
      hits.push({
        start: +(w.fromMs / 1000).toFixed(3),
        end:   +(w.toMs   / 1000).toFixed(3),
        text:  w.text,
      });
    }
  }
  return hits;
}

// run alignment per group/source and collect candidate clips
const clips = [];
for (const group of ["america", "dora"]) {
  const dir = join(SOURCES_DIR, group);
  if (!existsSync(dir)) continue;
  const sources = readdirSync(dir).filter((f) => f.endsWith(".mp3"));
  for (const src of sources) {
    const abs = join(dir, src);
    const slug = src.replace(/\.mp3$/, "");
    const words = whisperWords(abs);
    if (!words) continue;
    const hits = findOccurrences(words, TARGET_WORD[group]);
    console.log(`  · ${group}/${slug}: ${hits.length} matches`);
    hits.forEach((h, i) => {
      // pad ±20 ms — whisper word boundaries are typically tight to onset.
      const start = Math.max(0, h.start - 0.02);
      const end   = h.end + 0.03;
      clips.push({
        source: `sources/${group}/${src}`,
        group,
        tag: `w${i + 1}`,
        start: +start.toFixed(3),
        end:   +end.toFixed(3),
        fade:  0.015,
        gain_db: 0,
        _whisper_text: h.text,
      });
    });
  }
}

// merge with any existing clips.json (preserve user-tuned manual entries)
const clipsPath = join(ROOT, "clips.json");
let existing = [];
if (existsSync(clipsPath)) {
  try { existing = JSON.parse(readFileSync(clipsPath, "utf8")); } catch {}
}
// keep manually-set (non-null start) entries that aren't in this auto run's groups,
// or that we deliberately tagged with "manual-..." prefix
const keep = existing.filter((c) => c.start != null && c.tag && c.tag.startsWith("manual-"));
const final = [...keep, ...clips];

writeFileSync(clipsPath, JSON.stringify(final, null, 2));
console.log(`\n✓ wrote ${final.length} clips → ${clipsPath}  (${clips.length} auto, ${keep.length} manual kept)`);
console.log(`  next: node bin/extract.mjs && node bin/variations.mjs && node bin/stitch.mjs --random`);
