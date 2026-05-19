#!/usr/bin/env node
// sing.mjs — jeffrey-pvc sung vocal for the hippyhayzard ballad.
//
// Mirrors the proven pop/dance/bin/sing.mjs steps 1-4 (say → align →
// score-pitch → score-stretch) but tuned for an earthbound/mother
// SORROW BALLAD, not a trance hook:
//   • ballad TTS settings (higher stability locks jeffrey identity,
//     low style = plain/tender, slightly slow)
//   • gentle vibrato, slow stretch bpm so held notes really ring
//   • step 5 is NOT the trance many-jeffreys racing stack — it's an
//     intimate single-voice treatment (soft room + a quiet octave-down
//     double) laid over the 1:28 instrumental bed after the intro.
//
// The vocal score is hippyhayzard.np; the lyric is hippyhayzard.txt
// (word order/count parity is required — both derive the same words).
//
// Usage:
//   node pop/hippyhayzard/bin/sing.mjs            # full sung song
//   node pop/hippyhayzard/bin/sing.mjs --force    # bypass say cache

import { existsSync, readFileSync, writeFileSync, mkdirSync, copyFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}

const HERE = dirname(fileURLToPath(import.meta.url));   // pop/hippyhayzard/bin
const HH = resolve(HERE, "..");                          // pop/hippyhayzard
const POP = resolve(HH, "..");                           // pop
const REL = (p) => p.replace(POP + "/", "");

const SCORE = `${HH}/hippyhayzard.np`;
const LYRIC = `${HH}/hippyhayzard.txt`;
const OUT_DIR = `${HH}/out`;
const TMP = `${OUT_DIR}/.tmp`;
mkdirSync(TMP, { recursive: true });

const VOCAL = `${OUT_DIR}/hippyhayzard-vocal.mp3`;
const VOCAL_ALIGN = `${VOCAL}.alignment.json`;
const WORDS = `${OUT_DIR}/hippyhayzard-vocal-words.json`;
const PITCHED = `${OUT_DIR}/hippyhayzard-pitched.mp3`;
const PITCHED_ALIGN = `${OUT_DIR}/hippyhayzard-pitched-alignment.json`;
const STRETCHED = `${OUT_DIR}/hippyhayzard-stretched.mp3`;
const BED = `${OUT_DIR}/hippyhayzard.mp3`;
const FINAL = resolve(process.cwd(), flags.out || `${OUT_DIR}/hippyhayzard-song.mp3`);
const FORCE = !!flags.force;

for (const [p, what] of [[SCORE, "score"], [LYRIC, "lyric"]]) {
  if (!existsSync(p)) { console.error(`✗ ${what} missing: ${p}`); process.exit(1); }
}
if (!existsSync(BED)) {
  console.error(`✗ bed missing: ${BED}\n  run: node pop/hippyhayzard/bin/render.mjs --mode song`);
  process.exit(1);
}

function run(cmd, args, cwd = POP, soft = false) {
  const r = spawnSync(cmd, args, { cwd, stdio: ["ignore", "inherit", "inherit"] });
  if (r.status !== 0 && !soft) {
    console.error(`✗ ${cmd} ${args.slice(0, 3).join(" ")}… failed (exit ${r.status})`);
    process.exit(1);
  }
  return r.status === 0;
}
function step(name, fn) {
  const t0 = Date.now();
  process.stdout.write(`▸ ${name}\n`);
  fn();
  console.log(`  ${((Date.now() - t0) / 1000).toFixed(1)}s`);
}

console.log(`━━━ hippyhayzard · sing (jeffrey-pvc ballad) ━━━\n`);

// ── 1. say (jeffrey-pvc TTS, ballad settings, with timestamps) ────────
step("1 · say (jeffrey-pvc — tender ballad settings)", () => {
  const base = ["bin/say.mjs", REL(LYRIC),
    "--stability", "0.70", "--similarity", "0.92", "--style", "0.28",
    "--speed", "0.92", "--out", REL(VOCAL)];
  const withTs = [...base, "--timestamps"];
  if (FORCE) { base.push("--force"); withTs.push("--force"); }
  const r = spawnSync("node", withTs, { cwd: POP, stdio: ["ignore", "inherit", "inherit"] });
  if (r.status === 0) return;
  console.log("  ↪ timestamps unavailable — plain TTS + whisper align");
  run("node", base);
});

// ── 2. align → words.json ─────────────────────────────────────────────
const lyricWords = readFileSync(LYRIC, "utf8")
  .split("\n")
  .filter((l) => l.trim() && !/^(verse \d+|hook|intro|outro|bridge|chorus)$/i.test(l.trim()))
  .join(" ")
  .split(/\s+/).map((w) => w.replace(/[^a-zA-Z0-9]/g, "")).filter(Boolean);
const expected = lyricWords.length;

step(`2 · align (target ${expected} words)`, () => {
  if (existsSync(VOCAL_ALIGN)) {
    const a = JSON.parse(readFileSync(VOCAL_ALIGN, "utf8"));
    if (Array.isArray(a.words) && a.words.length >= expected * 0.7) {
      writeFileSync(WORDS, JSON.stringify(a.words, null, 0));
      console.log(`  ✓ ${a.words.length} words from ElevenLabs timestamps`);
      return;
    }
  }
  const wargs = ["bin/align.mjs", REL(VOCAL)];
  if (FORCE) wargs.push("--force");
  const ok = run("node", wargs, POP, true);
  if (ok && existsSync(WORDS)) {
    const ww = JSON.parse(readFileSync(WORDS, "utf8"));
    if (ww.length >= expected * 0.7) { console.log(`  ✓ ${ww.length} words from whisper`); return; }
  }
  const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", VOCAL], { encoding: "utf8" });
  const dur = parseFloat(probe.stdout.trim()) || 0;
  if (dur <= 0) { console.error("✗ cannot measure vocal duration for fallback alignment"); process.exit(1); }
  const span = (dur * 1000) / expected;
  writeFileSync(WORDS, JSON.stringify(
    lyricWords.map((w, i) => ({ text: w, fromMs: Math.floor(i * span), toMs: Math.floor((i + 1) * span) })), null, 0));
  console.log(`  ✓ synthesized ${expected} uniform windows over ${dur.toFixed(1)}s`);
});

// ── 3. score-pitch (WORLD f0 → the .np melody, gentle vibrato) ────────
step("3 · score-pitch (WORLD · gentle ballad vibrato)", () => {
  run("node", ["bin/score-pitch.mjs",
    "--slug", "hippyhayzard", "--section", "all",
    "--score", REL(SCORE), "--vocal", REL(VOCAL), "--words", REL(WORDS),
    "--transpose", "0", "--vibrato-hz", "5.0", "--vibrato-cents", "12",
    "--out", REL(PITCHED)]);
});

// ── 4. score-stretch (hold the notes) ────────────────────────────────
// 76 BPM made a 117s vocal — 1.5× the 88s bed. 112 BPM packs the same
// note weights into ~80s so the sung line fits inside the 1:28 bed
// while the *4–*8 sustains still ring balladically (a *8 ≈ 4.3s).
step("4 · score-stretch (rubberband · 112 BPM → fits the 1:28 bed)", () => {
  run("node", ["bin/score-stretch.mjs",
    "--slug", "hippyhayzard", "--section", "all",
    "--score", REL(SCORE), "--in", REL(PITCHED), "--alignment", REL(PITCHED_ALIGN),
    "--bpm", "112", "--max-stretch", "12.0", "--out", REL(STRETCHED)]);
});

// ── 5. intimate treatment + lay over the 1:28 bed ────────────────────
step("5 · treat + mix onto the bed (after the 4-bar intro)", () => {
  const lead = `${TMP}/lead.wav`;
  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", STRETCHED,
    "-ar", "48000", "-ac", "1", lead]);

  // intro = 4 bars @ 152 BPM half-time = 4 * (60/152*4) ≈ 6.32 s
  const introMs = Math.round(4 * (60 / 152 * 4) * 1000);

  // The bed is the length authority — the song ends when the bed's
  // resolved tonic + drone tail end, NOT whenever the vocal runs out.
  const bp = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", BED], { encoding: "utf8" });
  const bedDur = parseFloat(bp.stdout.trim()) || 93;
  const fadeStart = Math.max(0, bedDur - 2.5);

  // VOCAL-FORWARD mix. The earlier take buried + garbled the words:
  // the octave-down double smeared consonants and the long reverb
  // (fb 0.78) washed them out, under too-loud instruments. Now:
  //   • no double — single clean voice
  //   • near-dry (tiny 18ms slap only), so consonants survive
  //   • compress + presence EQ (+4dB ~3kHz) so jeffrey sits on top
  //   • bed cut to 0.42 AND hard-ducked (ratio 8) under the voice
  const fc =
    `[1:a]highpass=f=95,` +
      `acompressor=threshold=-22dB:ratio=4:attack=8:release=180:makeup=4:knee=3,` +
      `equalizer=f=3000:t=q:w=1.4:g=4,equalizer=f=200:t=q:w=1.2:g=-2,` +
      `aecho=0.6:0.3:18:0.07,volume=1.5,` +
      `adelay=${introMs}|${introMs}[vox];` +
    `[vox]asplit=2[voxM][voxK];` +
    `[0:a]volume=0.42[bedlo];` +
    `[bedlo][voxK]sidechaincompress=threshold=0.02:ratio=8:attack=8:release=300:makeup=1[bedd];` +
    `[bedd][voxM]amix=inputs=2:duration=longest:normalize=0,` +
      `atrim=duration=${bedDur.toFixed(3)},` +
      `afade=t=out:st=${fadeStart.toFixed(3)}:d=2.5,` +
      `loudnorm=I=-14:TP=-1.0:LRA=11,aresample=48000[out]`;

  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", BED, "-i", lead,
    "-filter_complex", fc, "-map", "[out]",
    "-c:a", "libmp3lame", "-q:a", "2", FINAL]);
});

mkdirSync(dirname(FINAL), { recursive: true });
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", FINAL], { encoding: "utf8" });
const sz = (readFileSync(FINAL).length / 1024).toFixed(0);
console.log(`\n✓ ${FINAL} (${sz} KB · ${parseFloat(probe.stdout || 0).toFixed(1)}s · jeffrey-pvc sung ballad on the bed)`);
copyFileSync(FINAL, `${OUT_DIR}/hippyhayzard-song.mp3`);
