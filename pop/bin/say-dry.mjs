#!/usr/bin/env node
// say-dry.mjs — say.mjs + dryness validation + auto-retry.
//
// ElevenLabs jeffrey-pvc returns roughly 50/50 a clean dry roll or a
// reverb-tail wet roll. We need the dry one. This wrapper:
//
//   1. Calls say.mjs with --force (re-rolls ElevenLabs)
//   2. Runs profile-vocal.mjs on the result
//   3. If PASS → done. If FAIL → save attempt aside, re-roll.
//   4. After --max-retries failed rolls, optionally perturb the lyric
//      (insert a soft comma at a section break) to give the model a
//      different prosody seed, and keep trying.
//   5. If we run out of retries, keep the best attempt (lowest tail50)
//      under OUT_PATH and warn loudly.
//
// Usage:
//   node bin/say-dry.mjs ../big-pictures/ac24-may-26.txt
//   node bin/say-dry.mjs ../big-pictures/ac24-may-26.txt --max-retries 8
//   node bin/say-dry.mjs ../big-pictures/ac24-may-26.txt --ratio-max 0.05 --tail-max 0.10
//
// Passes any unknown flags through to say.mjs (e.g. --voice, --speed).

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, copyFileSync,
         mkdirSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SAY = resolve(HERE, "say.mjs");
const PROFILE = resolve(HERE, "profile-vocal.mjs");

const argv = process.argv.slice(2);
const ownKeys = new Set(["max-retries", "ratio-max", "tail-max", "out", "perturb-after"]);
const flags = {};
const passthrough = [];
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    const hasVal = n !== undefined && !n.startsWith("--");
    if (ownKeys.has(k)) {
      if (hasVal) { flags[k] = n; i++; } else flags[k] = true;
    } else {
      passthrough.push(a);
      if (hasVal) { passthrough.push(n); i++; }
    }
  } else positional.push(a);
}

const lyricPath = positional[0] && resolve(process.cwd(), positional[0]);
if (!lyricPath || !existsSync(lyricPath)) {
  console.error("usage: say-dry.mjs <lyric.txt> [--max-retries N] [--ratio-max X] [--tail-max X] [--out path]");
  process.exit(2);
}

const MAX_RETRIES = Number(flags["max-retries"] ?? 6);
const PERTURB_AFTER = Number(flags["perturb-after"] ?? 3);  // attempts before we start nudging text
const RATIO_MAX = Number(flags["ratio-max"] ?? 0.04);
const TAIL_MAX  = Number(flags["tail-max"]  ?? 1.0);  // informational, not gated

// Lock-in ElevenLabs voice settings for the DRY roll (calibrated 2026-05-26).
// stability 0.85 + style 0 + similarity 0.95 produced the clear take; default
// settings (stability 0.5, style 0.4) reliably produced the wet/blurry take.
// Callers can override with --stability / --style / --similarity passthrough.
if (!passthrough.includes("--stability")) passthrough.push("--stability", "0.85");
if (!passthrough.includes("--style")) passthrough.push("--style", "0");
if (!passthrough.includes("--similarity")) passthrough.push("--similarity", "0.95");

// Where say.mjs will write by default:
const slug = basename(lyricPath).replace(/\.[^.]+$/, "");
const REPO_POP = resolve(HERE, "..");
const DEFAULT_OUT = `${REPO_POP}/big-pictures/out/${slug}-vocal.mp3`;
const OUT_PATH = flags.out ? resolve(process.cwd(), flags.out) : DEFAULT_OUT;
const ATTEMPTS_DIR = `${dirname(OUT_PATH)}/.attempts`;
mkdirSync(ATTEMPTS_DIR, { recursive: true });

// ── perturbation strategies (mild, lyric-preserving) ──────────────────
// We start with NO text changes (pure re-roll). After PERTURB_AFTER
// failed rolls we begin inserting invisible-prosody nudges that don't
// change meaning but reshape ElevenLabs' choice. Each retry tries a
// different nudge.
const PERTURBATIONS = [
  (t) => t,                                         // 0: no change
  (t) => t.replace(/—/g, ",,"),                    // 1: heavier comma pauses
  (t) => t.replace(/ — /g, "... "),                // 2: ellipsis pauses
  (t) => t.replace(/^hook$/m, "hook,"),            // 3: section nudge
  (t) => t.replace(/^verse 1$/m, "verse one,"),    // 4: spelled-out
  (t) => t.replace(/^verse 2$/m, "verse two,"),    // 5: spelled-out
  (t) => t.replace(/,/g, ".."),                    // 6: dot pauses everywhere
];

function profile(mp3) {
  const r = spawnSync("node", [PROFILE, mp3, "--json",
    "--ratio-max", String(RATIO_MAX), "--tail-max", String(TAIL_MAX)],
    { encoding: "utf8" });
  // exit code 0 = pass, 1 = fail (still has json on stdout)
  try { return JSON.parse(r.stdout); } catch { return null; }
}

function rollOnce(lyricFile, attemptNo) {
  console.log(`\n── attempt ${attemptNo}/${MAX_RETRIES} ─────────────────────────`);
  const args = [SAY, lyricFile, "--force", ...passthrough, "--out", OUT_PATH];
  const r = spawnSync("node", args, { stdio: "inherit" });
  if (r.status !== 0) { console.error("✗ say.mjs failed"); process.exit(1); }
  const p = profile(OUT_PATH);
  if (!p) { console.error("✗ profiler failed"); process.exit(1); }
  console.log(`  ratio ${p.ratio.toFixed(4)}  tail50 ${p.tail50.toFixed(4)}  →  ${p.verdict}${p.pass ? " ✓" : " ✗"}`);
  return p;
}

const originalLyric = readFileSync(lyricPath, "utf8");
const baseTmpLyric = `${ATTEMPTS_DIR}/${slug}.attempt.txt`;

let best = null; // {profile, savedAt}
for (let attempt = 1; attempt <= MAX_RETRIES; attempt++) {
  // Pick which lyric (perturbed or not) for this attempt
  let lyricFile = lyricPath;
  if (attempt > PERTURB_AFTER) {
    const idx = Math.min(attempt - PERTURB_AFTER, PERTURBATIONS.length - 1);
    const text = PERTURBATIONS[idx](originalLyric);
    writeFileSync(baseTmpLyric, text);
    lyricFile = baseTmpLyric;
    console.log(`  (perturbation #${idx} active)`);
  }
  const p = rollOnce(lyricFile, attempt);

  // Always keep a copy of this attempt for inspection
  const savedAt = `${ATTEMPTS_DIR}/${slug}-attempt${attempt}.mp3`;
  copyFileSync(OUT_PATH, savedAt);

  if (p.pass) {
    console.log(`\n✓ DRY take landed on attempt ${attempt}`);
    console.log(`  ${OUT_PATH}`);
    process.exit(0);
  }
  if (!best || p.tail50 < best.profile.tail50) best = { profile: p, savedAt };
}

console.warn(`\n✗ ${MAX_RETRIES} rolls all FAILED dryness check`);
console.warn(`  best: tail50=${best.profile.tail50.toFixed(4)} ratio=${best.profile.ratio.toFixed(4)}`);
console.warn(`  using best attempt → ${OUT_PATH}`);
copyFileSync(best.savedAt, OUT_PATH);
process.exit(1);
