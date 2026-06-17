#!/usr/bin/env node
// clean.mjs — isolate the words from the recordings.
//
// the america/dora utterances were cut straight from full mixes, so crowd,
// band and theme-music ride under every word — and the whisper transform in
// bin/hooks.mjs smears that bed into the breath. fix at the source:
//
//   1. demucs (two-stems) separates each FULL source recording into
//      vocals/accompaniment — full songs give the model context that
//      sub-second clips can't.  cached in .cache/demucs/.
//   2. every clip in clips.json is re-cut from the vocal stem, then
//      denoised (afftdn), silence-trimmed on both ends (the music bleed is
//      silence now, so the word boundary tightens itself), faded, loudnormed.
//   3. results overwrite utterances/<group>/ — same names, cleaner words.
//
// after this, re-run `node bin/hooks.mjs build` (delete stale snapped/ first
// or pass --resnap here) so the combination library inherits the clean cuts.
//
// usage:
//   node bin/clean.mjs               # separate + re-cut america/dora
//   node bin/clean.mjs --resnap      # …then rebuild snapped/ + hooks.html
//   node bin/clean.mjs --group dora  # one group only

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, rmSync, renameSync } from "node:fs";
import { basename, dirname, join, extname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);
const SR = 48_000;

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}

const slug = (s) => s.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
const srcSlug = (srcRel) => basename(srcRel, extname(srcRel));

const clips = JSON.parse(readFileSync(join(ROOT, "clips.json"), "utf8"))
  .filter((c) => c.start != null && c.end != null)
  .filter((c) => !flags.group || c.group === flags.group);

// ── 1. demucs each needed span (cached) ─────────────────────────────────
// whole recordings give the model context that sub-second clips can't —
// but marathon sources (3-hour episode compilations) would OOM an 8 GB
// machine, so anything over LONG_SRC gets a ±15 s proxy window per clip
// and only that window is separated.
const cacheDir = join(ROOT, ".cache", "demucs");
const proxyDir = join(ROOT, ".cache", "proxies");
mkdirSync(cacheDir, { recursive: true });
mkdirSync(proxyDir, { recursive: true });
const LONG_SRC = 600; // seconds
const CTX = 15;       // proxy context either side of the clip

function probeDur(p) {
  const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", p], { encoding: "utf8" });
  return +r.stdout.trim() || 0;
}

const srcDur = {};
for (const s of [...new Set(clips.map((c) => c.source))]) srcDur[s] = probeDur(join(ROOT, s));

// per clip: which file goes through demucs, where its stem lands, and the
// time offset between source timestamps and that stem.
const jobs = clips.map((c) => {
  if (srcDur[c.source] <= LONG_SRC) {
    return { clip: c, input: join(ROOT, c.source), stemName: srcSlug(c.source), offset: 0 };
  }
  const winStart = Math.max(0, c.start - CTX);
  const stemName = `${srcSlug(c.source)}-${slug(c.tag)}`;
  const proxy = join(proxyDir, `${stemName}.wav`);
  if (!existsSync(proxy)) {
    spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-ss", String(winStart), "-i", join(ROOT, c.source),
      "-t", String((c.end - c.start + CTX * 2).toFixed(3)),
      "-ac", "2", "-ar", "44100", proxy,
    ], { stdio: ["ignore", "ignore", "inherit"] });
  }
  return { clip: c, input: proxy, stemName, offset: winStart };
});

const stemFor = (j) => join(cacheDir, "htdemucs", j.stemName, "vocals.wav");
const todo = [...new Map(jobs.filter((j) => !existsSync(stemFor(j))).map((j) => [j.input, j])).values()];
if (todo.length) {
  console.log(`# demucs — separating ${todo.length} input(s), one at a time (8 GB machine):`);
  for (const j of todo) {
    console.log(`  · ${basename(j.input)} (${probeDur(j.input).toFixed(0)}s)`);
    const res = spawnSync("demucs", ["--two-stems=vocals", "-o", cacheDir, j.input],
      { stdio: ["ignore", "inherit", "inherit"] });
    if (res.status !== 0) { console.error(`✗ demucs failed on ${basename(j.input)}`); process.exit(1); }
  }
} else {
  console.log("# demucs — all vocal stems cached");
}

// ── 2. re-cut every word from its vocal stem ────────────────────────────
// pad the cut a touch, then let silence-trim find the true word boundary:
// denoise → trim head → fade in → (reversed) trim tail → fade out → loudnorm.
const PAD = 0.06;
const CLEAN_CHAIN =
  "afftdn=nf=-30," +
  "silenceremove=start_periods=1:start_threshold=-38dB:start_silence=0.02," +
  "afade=t=in:d=0.006," +
  "areverse," +
  "silenceremove=start_periods=1:start_threshold=-38dB:start_silence=0.02," +
  "afade=t=in:d=0.02," +
  "areverse," +
  "loudnorm=I=-14:TP=-1:LRA=11";

console.log(`\n# re-cutting ${clips.length} words from vocal stems:`);
let made = 0, fellBack = 0;
for (const j of jobs) {
  const c = j.clip;
  const stem = stemFor(j);
  const outDir = join(ROOT, "utterances", c.group);
  mkdirSync(outDir, { recursive: true });
  const name = `${srcSlug(c.source)}-${slug(c.tag)}`;
  const outAbs = join(outDir, `${name}.wav`);
  const tmp = outAbs + ".clean.tmp.wav";
  const dur = c.end - c.start + PAD * 2;
  const cut = (input, offset) => spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-ss", String(Math.max(0, c.start - offset - PAD).toFixed(3)), "-i", input,
    "-t", String(dur.toFixed(3)),
    "-af", CLEAN_CHAIN + (c.gain_db ? `,volume=${c.gain_db}dB` : ""),
    "-ac", "1", "-ar", String(SR), tmp,
  ], { stdio: ["ignore", "ignore", "inherit"] }).status === 0;

  let ok = cut(stem, j.offset) && probeDur(tmp) > 0.08;
  if (!ok) {
    // stem came up empty for this span (separation miss) — fall back to the
    // original mix with denoise+trim only, and say so.
    ok = cut(join(ROOT, c.source), 0) && probeDur(tmp) > 0.08;
    if (ok) { console.log(`  ! ${c.group}/${name} — stem empty, denoised the mix cut instead`); fellBack++; }
  }
  if (ok) {
    renameSync(tmp, outAbs);
    console.log(`  ✓ ${c.group}/${name}.wav  (${probeDur(outAbs).toFixed(2)}s)`);
    made++;
  } else {
    rmSync(tmp, { force: true });
    console.log(`  ✗ ${c.group}/${name} — both stem and mix cut failed, kept old file`);
  }
}
console.log(`\n# ${made}/${clips.length} re-cut clean${fellBack ? ` (${fellBack} fell back to denoised mix)` : ""}`);

// ── 3. optional resnap ──────────────────────────────────────────────────
if (flags.resnap) {
  const groups = flags.group ? [flags.group] : [...new Set(clips.map((c) => c.group))];
  for (const g of groups) {
    rmSync(join(ROOT, "snapped", g), { recursive: true, force: true });
    console.log(`# cleared snapped/${g}/`);
  }
  const res = spawnSync("node", [join(HERE, "hooks.mjs")], { stdio: "inherit" });
  process.exit(res.status ?? 0);
}
