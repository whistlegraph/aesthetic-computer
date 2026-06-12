#!/usr/bin/env node
// render-c.mjs — the C-backend render flow for momabobasheep (the house
// pattern): bake (JS composes + preps sustains + score.h) → cc → ./momabobasheep
// (C renders the 10:10 pre-master) → the SAME ffmpeg master chain as
// bin/render-momabobasheep.mjs (gentle sleep master, measured LINEAR gain
// to -16 LUFS / -1 dBTP, truncate to 10:00) → out/momabobasheep-c.mp3 +
// out/momabobasheep-MASTER-c.wav, then the same clip test.
//
// usage: node pop/momboba/c/render-c.mjs [--no-bake] [--no-build]

import { spawnSync } from "node:child_process";
import { mkdirSync, unlinkSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));   // pop/momboba/c
const ROOT = dirname(HERE);                             // pop/momboba
const OUT = join(ROOT, "out");
mkdirSync(OUT, { recursive: true });

const argv = process.argv.slice(2);
const has = (f) => argv.includes(f);

function run(label, cmd, args, opts = {}) {
  console.log(`# ${label}`);
  const res = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (res.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
}

if (!has("--no-bake")) run("bake (events + sustains + score.h)", "node", [join(HERE, "bake.mjs")]);
if (!has("--no-build")) run("cc", "sh", [join(HERE, "build.sh")]);

const rawWav = join(OUT, "momabobasheep-raw.wav");
run("render (C engine)", join(HERE, "momabobasheep"),
    ["--out", rawWav, "--vocals", HERE + "/vocals/"]);

// ═══════════════════════════════════════════════════════════════════════
//  SLEEP MASTER — identical to the JS renderer's: clean low end, warmth,
//  ONE soft slow glue compressor (not a leveller — the baked night arc
//  must survive), measured LINEAR gain to -16 LUFS clamped at -1 dBTP,
//  gentle seam fades, truncate 610 s → 600 s.
// ═══════════════════════════════════════════════════════════════════════
const TOTAL_DUR = 600, FADE_OUT = 5, FADE_IN = 2;
// 2004-CD toasty chain — MUST mirror render-momabobasheep.mjs exactly:
// mono lows below 120 (phase-coherent crossover), warm 240 Hz body,
// linear gain to −13.5 LUFS clamped at −1 dBTP.
const TARGET_LUFS = -13.5;
const CHAIN = (vol) => [
  "[0:a]highpass=f=32,acrossover=split=120:order=4th[lo][hi]",
  "[lo]pan=stereo|c0=.5*c0+.5*c1|c1=.5*c0+.5*c1[lom]",            // bass mono
  "[lom][hi]amix=inputs=2:normalize=0," + [
    "bass=g=3:f=85",
    "equalizer=f=240:t=q:w=1.0:g=1.5",   // the TOAST — warm low-mid body
    "highshelf=f=3200:g=-3.5",
    "acompressor=threshold=-24dB:ratio=2:attack=80:release=700:makeup=1.5:knee=6",
    "treble=g=-1.0:f=8000",
    `afade=t=in:st=0:d=${FADE_IN}`,
    `afade=t=out:st=${TOTAL_DUR - FADE_OUT}:d=${FADE_OUT}`,
    ...(vol ? [vol] : []),
  ].join(",") + "[m]",
].join(";");

// pass 1 — measure the EQ'd/compressed signal (loudnorm as a METER only)
console.log("[master] pass 1/2 — measuring loudness…");
const meas = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
  "-i", rawWav, "-filter_complex", CHAIN("loudnorm=print_format=json"), "-map", "[m]", "-to", String(TOTAL_DUR),
  "-f", "null", "-"], { encoding: "utf8" });
const jf = (re) => { const m = (meas.stderr || "").match(re); return m ? m[1] : null; };
const mI = jf(/"input_i"\s*:\s*"([^"]+)"/), mTP = jf(/"input_tp"\s*:\s*"([^"]+)"/);
let gainDb = 0;
if (mI && mTP) gainDb = Math.min(TARGET_LUFS - parseFloat(mI), -1 - parseFloat(mTP));
else console.warn("   ! couldn't parse pass-1 measurement — applying unity gain");
const VOL = `volume=${gainDb.toFixed(2)}dB`;
console.log(`   · linear gain ${gainDb >= 0 ? "+" : ""}${gainDb.toFixed(2)} dB (measured ${mI ?? "?"} LUFS / ${mTP ?? "?"} dBTP)`);

// pass 2 — apply → 24-bit WAV + 320k mp3 (both stay in pop/momboba/out/)
const wavPath = join(OUT, "momabobasheep-MASTER-c.wav");
const mp3Path = join(OUT, "momabobasheep-c.mp3");
console.log("[master] pass 2/2 — applying → WAV + mp3…");
const apply = (extra, dst) => spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-i", rawWav, "-filter_complex", CHAIN(VOL), "-map", "[m]", "-to", String(TOTAL_DUR), ...extra, dst], { stdio: "inherit" });
const w = apply(["-ar", "44100", "-c:a", "pcm_s24le"], wavPath);
const ff = apply(["-ar", "44100", "-c:a", "libmp3lame", "-b:a", "320k"], mp3Path);
if (w.status !== 0 || ff.status !== 0) { console.error("✗ ffmpeg master failed"); process.exit(1); }
if (has("--clean-raw")) { try { unlinkSync(rawWav); } catch {} }
console.log(`✓ ${wavPath}`);
console.log(`✓ ${mp3Path} (320k · measured linear gain · target -16 LUFS / -1 dBTP)`);

// ── CLIP TEST — sample peak + true peak + integrated LUFS of the build ──
{
  const vd = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
    "-i", wavPath, "-af", "volumedetect", "-f", "null", "-"], { encoding: "utf8" });
  const mm = (vd.stderr || "").match(/max_volume:\s*(-?[\d.]+) dB/);
  const maxDb = mm ? parseFloat(mm[1]) : NaN;
  const ld = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
    "-i", wavPath, "-af", "loudnorm=print_format=summary", "-f", "null", "-"], { encoding: "utf8" });
  const tpm = (ld.stderr || "").match(/Input True Peak:\s*(-?[\d.]+|inf)/);
  const im = (ld.stderr || "").match(/Input Integrated:\s*(-?[\d.]+|inf)/);
  const clip = Number.isFinite(maxDb) && maxDb >= -0.05;
  console.log(`[clip-test] sample peak ${Number.isFinite(maxDb) ? maxDb.toFixed(2) : "?"} dBFS · ` +
    `true peak ${tpm ? tpm[1] : "?"} dBTP · integrated ${im ? im[1] : "?"} LUFS`);
  console.log(clip ? "   ✗ CLIPPING — sample peak ≥ -0.05 dBFS" : "   ✓ no clipping (headroom present)");
}
