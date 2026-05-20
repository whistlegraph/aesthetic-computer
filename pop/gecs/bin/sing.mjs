#!/usr/bin/env node
// gecs/bin/sing.mjs — jeffrey-pvc sung vocal for kernel panic. Same
// proven pipeline as pop/hippyhayzard/bin/sing.mjs (say → align →
// score-pitch → score-stretch) but tuned HOT for 100-gecs hyperpop:
//
//   • TTS: stability 0.55 (still ≥0.5 — clone stays jeffrey; per the
//     jeffrey_pvc_settings memory anything lower drifts), style 0.55
//     (more bratty / less measured), speed 1.10 (rapid delivery).
//   • score-pitch: heavier vibrato (6.5Hz × 22c) for the autotune-wobble
//     gecs vocal tic; the .np already does the chipmunk lift in the
//     chorus by writing the melody up at A4-F#5.
//   • score-stretch: 168 BPM so the sung line packs tight inside the
//     84.4s bed (162 left some chorus held notes ringing past the kit).
//   • mix: aggressive comp + presence EQ (+5dB ~3.5kHz) + light tape-
//     style saturation; no reverb (gecs vox sit dry on top of the
//     bit-crushed bed). Bed hard-ducked under the vocal (ratio 10).
//
// Word order MUST match pop/gecs/gecs.txt strictly (175 words).
//
// Usage:
//   node pop/gecs/bin/sing.mjs            # full sung song
//   node pop/gecs/bin/sing.mjs --force    # bypass say cache

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

const HERE = dirname(fileURLToPath(import.meta.url));
const G = resolve(HERE, "..");
const POP = resolve(G, "..");
const REL = (p) => p.replace(POP + "/", "");

const SCORE = `${G}/gecs.np`;
const LYRIC = `${G}/gecs.txt`;
const OUT_DIR = `${G}/out`;
const TMP = `${OUT_DIR}/.tmp`;
mkdirSync(TMP, { recursive: true });

const VOCAL = `${OUT_DIR}/gecs-vocal.mp3`;
const VOCAL_ALIGN = `${VOCAL}.alignment.json`;
const WORDS = `${OUT_DIR}/gecs-vocal-words.json`;
const PITCHED = `${OUT_DIR}/gecs-pitched.mp3`;
const PITCHED_ALIGN = `${OUT_DIR}/gecs-pitched-alignment.json`;
const STRETCHED = `${OUT_DIR}/gecs-stretched.mp3`;
const BED = `${OUT_DIR}/gecs.mp3`;
const FINAL = resolve(process.cwd(), flags.out || `${OUT_DIR}/gecs-song.mp3`);
const FORCE = !!flags.force;

for (const [p, what] of [[SCORE, "score"], [LYRIC, "lyric"]]) {
  if (!existsSync(p)) { console.error(`✗ ${what} missing: ${p}`); process.exit(1); }
}
if (!existsSync(BED)) {
  console.error(`✗ bed missing: ${BED}\n  run: node pop/gecs/bin/render.mjs`);
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

console.log(`━━━ gecs · sing (jeffrey-pvc hyperpop) ━━━\n`);

// ── 1. say × N — one ElevenLabs call PER SECTION with section-specific
// voice settings. Then concatenate the per-section mp3s into VOCAL and
// merge per-section alignment JSONs into VOCAL_ALIGN (with cumulative
// time offsets) so steps 2-5 see a single contiguous vocal as before.
//
//   verse     — stability 0.72, style 0.30, speed 1.10  (solid + rapid)
//   chorus    — stability 0.58, style 0.65, speed 1.05  (excited, big)
//   breakdown — stability 0.50, style 0.85, speed 0.95  (shouted, loose)
//
// say.mjs already content-hash-caches per call body, so unchanged
// sections skip the API on re-run. `--force` invalidates all.

// Parse gecs.txt into ORDERED sections (chorus appears 3× — sing each).
function parseSections(txt) {
  const out = [];
  let cur = null;
  for (const raw of txt.split("\n")) {
    const t = raw.trim();
    if (!t) continue;
    const m = t.match(/^(verse \d+|chorus|breakdown|hook|intro|outro|bridge)$/i);
    if (m) {
      if (cur) out.push(cur);
      const name = m[1].toLowerCase();
      const kind = name.startsWith("verse") ? "verse"
        : name === "chorus" ? "chorus"
        : name === "breakdown" ? "breakdown" : name;
      cur = { name, kind, lines: [] };
    } else if (cur) {
      cur.lines.push(t);
    }
  }
  if (cur) out.push(cur);
  return out;
}

const SECTION_VOICE = {
  verse:     { stability: "0.72", style: "0.30", speed: "1.10" },
  chorus:    { stability: "0.58", style: "0.65", speed: "1.05" },
  breakdown: { stability: "0.50", style: "0.85", speed: "0.95" },
};

const SECTIONS_TXT = parseSections(readFileSync(LYRIC, "utf8"));
const SECTION_DIR = `${TMP}/sections`;
mkdirSync(SECTION_DIR, { recursive: true });

step(`1 · say × ${SECTIONS_TXT.length} (per-section voice settings)`, () => {
  // Number each section occurrence so the temp file paths are unique
  // (3 chorus calls → chorus-1.txt, chorus-2.txt, chorus-3.txt etc).
  const counts = {};
  for (const sec of SECTIONS_TXT) {
    counts[sec.name] = (counts[sec.name] || 0) + 1;
    sec.idx = counts[sec.name];
    sec.slug = sec.name.replace(/\s+/g, "");          // "verse 1" → "verse1"
    sec.txtPath = `${SECTION_DIR}/${sec.slug}-${sec.idx}.txt`;
    sec.mp3Path = `${SECTION_DIR}/${sec.slug}-${sec.idx}.mp3`;
    sec.alignPath = `${sec.mp3Path}.alignment.json`;
    writeFileSync(sec.txtPath, sec.lines.join("\n") + "\n");
  }
  // Fire each section sequentially (parallel hammers the API + machine).
  for (const sec of SECTIONS_TXT) {
    const v = SECTION_VOICE[sec.kind] || SECTION_VOICE.verse;
    const args = ["bin/say.mjs", REL(sec.txtPath),
      "--stability", v.stability, "--similarity", "0.92", "--style", v.style,
      "--speed", v.speed, "--out", REL(sec.mp3Path), "--timestamps"];
    if (FORCE) args.push("--force");
    process.stdout.write(`  · ${sec.name}(${sec.idx}) [${sec.kind}: stab ${v.stability} · sty ${v.style} · spd ${v.speed}]\n`);
    const r = spawnSync("node", args, { cwd: POP, stdio: ["ignore", "inherit", "inherit"] });
    if (r.status !== 0) { console.error(`✗ say failed for ${sec.name}(${sec.idx})`); process.exit(1); }
  }
});

// ── 2. concat per-section mp3s + merge alignments → VOCAL + WORDS ────
const lyricWords = readFileSync(LYRIC, "utf8")
  .split("\n")
  .filter((l) => l.trim() && !/^(verse \d+|hook|intro|outro|bridge|chorus|breakdown)$/i.test(l.trim()))
  .join(" ")
  .split(/\s+/).map((w) => w.replace(/[^a-zA-Z0-9]/g, "")).filter(Boolean);
const expected = lyricWords.length;

step(`2 · concat sections + merge alignments (target ${expected} words)`, () => {
  // Convert each section mp3 → 48k wav, measure exact duration, and
  // stitch with ffmpeg concat. Wav avoids the mp3-concat priming gap.
  const wavList = [];
  let offsetSec = 0;
  const mergedWords = [];
  for (const sec of SECTIONS_TXT) {
    const wav = `${SECTION_DIR}/${sec.slug}-${sec.idx}.wav`;
    run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", sec.mp3Path, "-ar", "48000", "-ac", "1", wav]);
    wavList.push(wav);
    // word offsets: shift this section's words by cumulative offset
    if (!existsSync(sec.alignPath)) {
      console.error(`✗ alignment missing for ${sec.name}(${sec.idx})`); process.exit(1);
    }
    const a = JSON.parse(readFileSync(sec.alignPath, "utf8"));
    const offMs = Math.round(offsetSec * 1000);
    for (const w of (a.words || [])) {
      mergedWords.push({
        text: w.text.replace(/[^a-zA-Z0-9]/g, ""),
        fromMs: w.fromMs + offMs,
        toMs: w.toMs + offMs,
      });
    }
    const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
      "-of", "default=noprint_wrappers=1:nokey=1", wav], { encoding: "utf8" });
    offsetSec += parseFloat(probe.stdout.trim()) || 0;
  }
  // ffmpeg concat demuxer requires a manifest
  const manifest = `${SECTION_DIR}/manifest.txt`;
  writeFileSync(manifest, wavList.map((p) => `file '${p}'`).join("\n") + "\n");
  const combinedWav = `${SECTION_DIR}/combined.wav`;
  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "concat", "-safe", "0", "-i", manifest,
    "-ar", "48000", "-ac", "1", combinedWav]);
  // combined wav → mp3 for VOCAL (so the downstream pipeline gets mp3)
  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", combinedWav, "-c:a", "libmp3lame", "-q:a", "2", VOCAL]);
  // Synthesize a combined alignment json (matches format of single say.mjs)
  writeFileSync(VOCAL_ALIGN, JSON.stringify({
    source: "elevenlabs/with-timestamps/multi-section",
    sections: SECTIONS_TXT.map((s) => ({ name: s.name, idx: s.idx, kind: s.kind })),
    words: mergedWords,
  }, null, 2));
  writeFileSync(WORDS, JSON.stringify(mergedWords, null, 0));
  console.log(`  ✓ ${mergedWords.length} words across ${SECTIONS_TXT.length} sections · combined ${offsetSec.toFixed(1)}s vocal`);
});

// ── 3. score-pitch (WORLD f0 → the .np melody, heavy hyperpop vibrato) ──
step("3 · score-pitch (WORLD · hyperpop wobble 6.5Hz/22c)", () => {
  run("node", ["bin/score-pitch.mjs",
    "--slug", "gecs", "--section", "all",
    "--score", REL(SCORE), "--vocal", REL(VOCAL), "--words", REL(WORDS),
    "--transpose", "0", "--vibrato-hz", "6.5", "--vibrato-cents", "22",
    "--out", REL(PITCHED)]);
});

// ── 4. score-stretch (pack into the 84.4s bed) ────────────────────────
step("4 · score-stretch (rubberband · 168 BPM → fits 84.4s bed)", () => {
  run("node", ["bin/score-stretch.mjs",
    "--slug", "gecs", "--section", "all",
    "--score", REL(SCORE), "--in", REL(PITCHED), "--alignment", REL(PITCHED_ALIGN),
    "--bpm", "168", "--max-stretch", "8.0", "--out", REL(STRETCHED)]);
});

// ── 5. hyperpop mix: dry, hot, in-your-face. Lay over the bed. ────────
step("5 · treat + mix onto the bed (intro = 4 bars @ 165 BPM)", () => {
  const lead = `${TMP}/lead.wav`;
  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", STRETCHED,
    "-ar", "48000", "-ac", "1", lead]);

  // intro = 4 bars @ 165 BPM = 4 * (60/165*4) = 5.818 s
  const introMs = Math.round(4 * (60 / 165 * 4) * 1000);

  const bp = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", BED], { encoding: "utf8" });
  const bedDur = parseFloat(bp.stdout.trim()) || 84.4;
  const fadeStart = Math.max(0, bedDur - 1.0);

  // VOCAL chain (less airy):
  //   highpass 80Hz only (was 110Hz — that was eating chest warmth)
  //   acompressor (thresh -18dB, ratio 4, gentle attack — keep body)
  //   equalizer +2.5dB 250Hz (RESTORE chest, not de-mud — was -3dB)
  //   equalizer +2dB ~2.5kHz (presence cut-through, not the harsh 3.5k +5dB)
  //   equalizer -2dB 6kHz (KILL the airy/breathy top end)
  //
  // BED stays at parity (0.85 vol) and only gets light ducking (ratio 3,
  // higher threshold) — earlier ratio-10/thresh-0.015 was making the bed
  // disappear the moment any vocal hit. Now bed is heard THROUGH vocal.
  const fc =
    `[1:a]highpass=f=80,` +
      `acompressor=threshold=-18dB:ratio=4:attack=8:release=160:makeup=4:knee=4,` +
      `equalizer=f=250:t=q:w=1.0:g=2.5,` +
      `equalizer=f=2500:t=q:w=1.4:g=2,` +
      `equalizer=f=6000:t=q:w=1.4:g=-2,` +
      `acompressor=threshold=-10dB:ratio=2.5:attack=4:release=80:makeup=1.5,` +
      `volume=1.35,` +
      `adelay=${introMs}|${introMs},` +
      `apad=whole_dur=${bedDur.toFixed(3)}[vox];` +
    `[vox]asplit=2[voxM][voxK];` +
    `[0:a]volume=0.95[bedlo];` +
    `[bedlo][voxK]sidechaincompress=threshold=0.08:ratio=3:attack=10:release=200:makeup=1[bedd];` +
    `[bedd][voxM]amix=inputs=2:duration=first:normalize=0,` +
      `atrim=duration=${bedDur.toFixed(3)},` +
      `afade=t=out:st=${fadeStart.toFixed(3)}:d=1.0,` +
      `loudnorm=I=-11:TP=-1.0:LRA=8,aresample=48000[out]`;

  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", BED, "-i", lead,
    "-filter_complex", fc, "-map", "[out]",
    "-c:a", "libmp3lame", "-q:a", "2", FINAL]);
});

mkdirSync(dirname(FINAL), { recursive: true });
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", FINAL], { encoding: "utf8" });
const sz = (readFileSync(FINAL).length / 1024).toFixed(0);
console.log(`\n✓ ${FINAL} (${sz} KB · ${parseFloat(probe.stdout || 0).toFixed(1)}s · jeffrey-pvc hyperpop sung on the bed)`);
copyFileSync(FINAL, `${OUT_DIR}/gecs-song.mp3`);
