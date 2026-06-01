#!/usr/bin/env node
// bake-vowels.mjs — full "vowel extraction" dance track. Renders the
// loukeman-style 4/4 dance bed (kick / sub / perc / pads) via the C
// engine, then mixes the pure-vowel drone (pop/big-pictures/out/
// amazing-vowels.wav, built by extract-vowels.mjs) over it — with the
// whole vowel pad SIDECHAINED to the bed's kick so it dips with every
// kick "intelligently and magically" (and stops pumping for free during
// the break/outro where the kick drops out).
//
//   bed  ──asplit──┬─────────────────────────────[bedMain]──┐
//                  └─[bedKey]──► sidechaincompress key       ├─amix─► master
//   vowels ──reverb wash──► sidechaincompress (ducked) ──────┘
//
// Output: ~/Documents/Shelf/amazing-grace-dance/
//   amazing-grace-vowels.mp3 + -MASTER.wav
//
// Usage:
//   node pop/big-pictures/bin/bake-vowels.mjs [outDir] \
//     [--no-rebuild-bed] [--duck 6] [--vox 0.95] [--bed 0.9] [--reverb 0.22]

import { execSync, spawnSync } from "node:child_process";
import { existsSync, statSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");
const C    = resolve(POP, "big-pictures/c");

const argv = process.argv.slice(2);
function flag(name, def) {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : def;
}
const outDir = argv.find((a) => !a.startsWith("--") &&
  argv[argv.indexOf(a) - 1]?.startsWith("--") !== true) ||
  `${homedir()}/Documents/Shelf/amazing-grace-dance`;
mkdirSync(outDir, { recursive: true });

const REBUILD_BED = !argv.includes("--no-rebuild-bed");
const DUCK_RATIO  = flag("duck", "6");      // sidechain ratio (higher = deeper pump)
const VOX_GAIN    = flag("vox", "0.7");
const BED_GAIN    = flag("bed", "0.9");
const REVERB      = parseFloat(flag("reverb", "0.22"));
const VOX_FADEIN  = parseFloat(flag("vox-fadein", "15")); // slow swell into the track
const VOX_DELAY   = parseFloat(flag("vox-delay", "1.5")); // true silence before the swell
const DRIVE       = flag("drive", "0.30");                // engine pre-tanh soft-clip (kills maxing)
const MONO_KICK   = !argv.includes("--haas-kick");        // mono-solid low end (no HAAS phase smear)
const PEAK_DB     = parseFloat(flag("peak", "-1.0"));     // master true-peak ceiling
const MONO_BASS_HZ = parseFloat(flag("mono-bass", "120")); // fold bed lows below this to mono (0 = off)
const BED_ONLY    = argv.includes("--bed-only");          // master just the bed, no vocals

const ENGINE   = `${C}/amazing-dance`;
const ENGINE_C = `${C}/amazing-dance.c`;
const BUILD    = `${C}/build-dance.sh`;
const BED_WAV  = `${outDir}/.amazing-vowels-bed.wav`;
const VOX_WAV  = resolve(POP, "big-pictures/out/amazing-vowels.wav");
const FINAL_WAV = `${outDir}/amazing-grace-vowels-MASTER.wav`;
const FINAL_MP3 = `${outDir}/amazing-grace-vowels.mp3`;

function run(cmd, args, label) {
  process.stdout.write(`→ ${label}...\n`);
  const r = spawnSync(cmd, args, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
}
function dur(p) {
  return parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${p}"`
  ).toString().trim());
}

if (!existsSync(VOX_WAV)) {
  console.error(`✗ vowel stem missing: ${VOX_WAV}\n  build it first:  node pop/big-pictures/bin/extract-vowels.mjs`);
  process.exit(1);
}

// 1 — build engine if stale
const needsBuild = !existsSync(ENGINE) ||
  statSync(ENGINE_C).mtimeMs > statSync(ENGINE).mtimeMs;
if (needsBuild) run("bash", [BUILD], "build C engine");

// 2 — render bed (rebuild unless told not to). Clean-bed flags: gentle
// soft-clip drive + mono kick (no HAAS phase smear).
if (REBUILD_BED || !existsSync(BED_WAV)) {
  const bedArgs = ["--out", BED_WAV, "--drive", DRIVE];
  if (MONO_KICK) bedArgs.push("--mono-kick");
  run(ENGINE, bedArgs, `render dance bed (drive ${DRIVE}${MONO_KICK ? ", mono-kick" : ""})`);
} else {
  console.log(`→ reusing bed ${BED_WAV}`);
}

const bedDur = dur(BED_WAV);
const voxDur = dur(VOX_WAV);
const total = Math.max(bedDur, voxDur);
console.log(`\n  bed ${bedDur.toFixed(1)}s · vowels ${voxDur.toFixed(1)}s · master ${total.toFixed(1)}s`);
console.log(`  duck ratio ${DUCK_RATIO} · vox ${VOX_GAIN} · bed ${BED_GAIN} · reverb ${REVERB}\n`);

// 3 — mix: sidechain the vowel pad to the bed's kick, then sum
//   • bedKey = the bed itself; its kick is the dominant transient, so the
//     pad pumps on kicks and relaxes through the break/outro automatically.
//   • light stereo reverb wash on the vowels before ducking.
const reverbStage = REVERB > 0
  ? `aecho=0.85:0.9:90|150:${(REVERB).toFixed(2)}|${(REVERB * 0.7).toFixed(2)},`
  : "";
// Vowel chain: reverb wash → level → EXP swell (slow start, stays quiet
// early) → adelay so the very top of the track is TRUE silence. The swell
// is applied BEFORE the delay, so it begins after the silent pad.
const voxDelayMs = Math.round(VOX_DELAY * 1000);
const PRE_WAV = `${outDir}/.amazing-vowels-premix.wav`;
// Bed conditioning: fold everything below MONO_BASS_HZ to mono so the
// low end is phase-coherent (club-safe), keeping the highs stereo-wide.
// bed-only doesn't need a sidechain key, so don't split off [bedkey]
// (an unconnected filter pad would make ffmpeg error).
// A bare label ([bedmain]) attaches with no comma; a filter (asplit)
// attaches after a comma.
const bedTail = BED_ONLY ? `[bedmain]` : `,asplit=2[bedmain][bedkey]`;
const bedCond = MONO_BASS_HZ > 0
  ? `[0:a]aresample=48000,aformat=channel_layouts=stereo,asplit=2[blo][bhi];` +
    `[blo]lowpass=f=${MONO_BASS_HZ},` +
      `pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[blom];` +
    `[bhi]highpass=f=${MONO_BASS_HZ}[bhim];` +
    `[blom][bhim]amix=inputs=2:normalize=0${bedTail}`
  : `[0:a]aresample=48000,aformat=channel_layouts=stereo${bedTail}`;
const filter = BED_ONLY
  ? [bedCond, `[bedmain]volume=${BED_GAIN},atrim=duration=${total.toFixed(3)}[out]`].join(";")
  : [
      bedCond,
      `[1:a]aresample=48000,aformat=channel_layouts=stereo,${reverbStage}` +
        `volume=${VOX_GAIN},afade=t=in:st=0:d=${VOX_FADEIN.toFixed(2)}:curve=exp,` +
        `adelay=${voxDelayMs}|${voxDelayMs}[voxwet]`,
      `[voxwet][bedkey]sidechaincompress=threshold=0.04:ratio=${DUCK_RATIO}:` +
        `attack=5:release=240:makeup=2:level_sc=0.8[voxducked]`,
      `[bedmain]volume=${BED_GAIN}[bedlvl]`,
      `[bedlvl][voxducked]amix=inputs=2:duration=longest:dropout_transition=0:` +
        `normalize=0,atrim=duration=${total.toFixed(3)}[out]`,
    ].join(";");

console.log(`→ mixing + sidechaining vowels to the kick (premix, no normalization)...`);
execSync(
  `ffmpeg -y -loglevel error -i "${BED_WAV}" -i "${VOX_WAV}" ` +
  `-filter_complex "${filter}" -map "[out]" ` +
  `-ar 48000 -ac 2 -c:a pcm_s16le "${PRE_WAV}"`,
  { stdio: ["ignore", "ignore", "inherit"] }
);

// 3b — FX SAMPLE LAYERS (CC0, cached in the vault): a pitched-down
//      chainsaw grind landing on the ~1:25 build, and a tornado-wind wash
//      through the break. Folded into the premix before mastering.
let masterSrc = PRE_WAV;
const FX_ENABLED = !argv.includes("--no-fx");
const CACHE = `${homedir()}/aesthetic-computer-vault/personal/pop/freesound-cache`;
const sawSample  = `${CACHE}/463731-chainsaw_cut_loop.wav`;
const windSample = `${CACHE}/251637-insideatornado_mp3.wav`;
if (FX_ENABLED && existsSync(sawSample) && existsSync(windSample)) {
  const FX_WAV = `${outDir}/.amazing-vowels-fx.wav`;
  const fxF = [
    // pitched-DOWN chainsaw grind (varispeed ×0.6) at ~1:24 (84 s), ~12 s
    `[0:a]atrim=0:12,asetrate=48000*0.6,aresample=48000,aformat=channel_layouts=stereo,` +
      `volume=0.40,afade=t=in:st=0:d=1.5,afade=t=out:st=9:d=2.5,adelay=84000|84000[saw]`,
    // tornado wind wash through the break (~96 s), ~34 s, heavy reverb
    `[1:a]atrim=0:34,aformat=channel_layouts=stereo,volume=0.20,` +
      `aecho=0.8:0.85:140|240:0.35|0.25,afade=t=in:st=0:d=4,afade=t=out:st=30:d=4,` +
      `adelay=96000|96000[wind]`,
    `[saw][wind]amix=inputs=2:duration=longest:normalize=0,apad,` +
      `atrim=duration=${total.toFixed(3)}[fx]`,
  ].join(";");
  execSync(`ffmpeg -y -loglevel error -i "${sawSample}" -i "${windSample}" ` +
           `-filter_complex "${fxF}" -map "[fx]" -ar 48000 -ac 2 -c:a pcm_s16le "${FX_WAV}"`);
  const COMBINED = `${outDir}/.amazing-vowels-premix-fx.wav`;
  execSync(`ffmpeg -y -loglevel error -i "${PRE_WAV}" -i "${FX_WAV}" ` +
           `-filter_complex "[0:a][1:a]amix=inputs=2:duration=longest:normalize=0[m]" ` +
           `-map "[m]" -ar 48000 -ac 2 -c:a pcm_s16le "${COMBINED}"`);
  masterSrc = COMBINED;
  console.log(`→ fx layered: pitched chainsaw @ 1:24 + tornado wind in the break`);
}

// Master with a FADE-PRESERVING static gain (dynamic loudnorm was lifting
// the quiet intro and undoing the swell). Measure the premix peak, apply a
// single makeup gain to hit the true-peak ceiling, and an alimiter to catch
// stray transients — no time-varying gain, so the silent start stays silent.
const detect = execSync(
  `ffmpeg -hide_banner -i "${masterSrc}" -af volumedetect -f null - 2>&1 | grep max_volume`
).toString();
const peakDb = parseFloat(detect.match(/max_volume:\s*(-?[\d.]+)/)?.[1] ?? "0");
const makeupDb = (PEAK_DB - peakDb).toFixed(2);
console.log(`→ master: premix peak ${peakDb}dB → +${makeupDb}dB makeup → ${PEAK_DB}dB ceiling (static, fade-safe)`);
execSync(
  `ffmpeg -y -loglevel error -i "${masterSrc}" ` +
  `-af "volume=${makeupDb}dB,alimiter=limit=${PEAK_DB}dB:level=false" ` +
  `-ar 48000 -ac 2 -c:a pcm_s16le "${FINAL_WAV}"`
);
execSync(`ffmpeg -y -loglevel error -i "${FINAL_WAV}" -b:a 320k "${FINAL_MP3}"`);

console.log(`\n✓ ${FINAL_WAV.replace(homedir(), "~")} (${dur(FINAL_WAV).toFixed(1)}s)`);
console.log(`✓ ${FINAL_MP3.replace(homedir(), "~")}`);
console.log(`\n  open -a "QuickTime Player" "${FINAL_MP3}"`);
