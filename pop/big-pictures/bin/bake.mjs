#!/usr/bin/env node
// bake-vowels.mjs — full "vowel extraction" dance track. Renders the
// loukeman-style 4/4 dance bed (kick / sub / perc / pads) via the C
// engine, then mixes the pure-vowel drone (pop/big-pictures/out/
// amaythingra-vowels.wav, built by extract-vowels.mjs) over it — with the
// whole vowel pad SIDECHAINED to the bed's kick so it dips with every
// kick "intelligently and magically" (and stops pumping for free during
// the break/outro where the kick drops out).
//
//   bed  ──asplit──┬─────────────────────────────[bedMain]──┐
//                  └─[bedKey]──► sidechaincompress key       ├─amix─► master
//   vowels ──reverb wash──► sidechaincompress (ducked) ──────┘
//
// Output: ~/Documents/Shelf/amaythingra/
//   amaythingra.mp3 + -MASTER.wav
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
  `${homedir()}/Documents/Shelf/amaythingra`;
mkdirSync(outDir, { recursive: true });

const REBUILD_BED = !argv.includes("--no-rebuild-bed");
const DUCK_RATIO  = flag("duck", "6");      // sidechain ratio (higher = deeper pump)
const VOX_GAIN    = flag("vox", "0.7");
const BED_GAIN    = flag("bed", "0.9");
const REVERB      = parseFloat(flag("reverb", "0.22"));
const VOX_FADEIN  = parseFloat(flag("vox-fadein", "15")); // slow swell into the track
const VOX_DELAY   = parseFloat(flag("vox-delay", "1.5")); // true silence before the swell
// @jeffrey: the jeffrey ahh/ooh vocals should ONLY appear in the last N
// seconds — an outro vocal swell, not a full-length wash. >0 windows the
// TAIL of the vowel stem and places it so it rides out the final N s.
const VOX_LAST    = parseFloat(flag("vox-last", "60"));   // 0 = full-length (old behavior)
const DRIVE       = flag("drive", "0.30");                // engine pre-tanh soft-clip (kills maxing)
const MONO_KICK   = !argv.includes("--haas-kick");        // mono-solid low end (no HAAS phase smear)
const MONO_BASS_HZ = parseFloat(flag("mono-bass", "120")); // fold bed lows below this to mono (0 = off)
const BED_ONLY    = argv.includes("--bed-only");          // master just the bed, no vocals
// Opening rebalance (@jeffrey 2026-06-02): the bed's crunchy kicks slam
// at full level from t=0. Fade the WHOLE bed in over the opening so the
// boot chime + granular IMG_8802 hum carry the first ~20 s and the kit
// swells up instead of punching in. The chainsaw/tornado FX were "wayyy
// too loud" — knocked right down (now flag-tunable).
const BED_FADEIN  = parseFloat(flag("bed-fadein", "6"));  // bed swell-in seconds (0 = off)
// @jeffrey: more drama at the FIRST DROP (the 0:32 WOOP). Pull the build
// DOWN over the seconds leading in (anticipation), then snap back to full
// ON the drop so it hits harder by contrast. The dip ramps from 1.0 →
// DROP_DIP across DROP_DIP_LEN s ending at DROP_AT, then jumps to full.
const DROP_AT      = parseFloat(flag("drop-at", "32"));    // 0:32 WOOP downbeat
const DROP_DIP     = parseFloat(flag("drop-dip", "0.58")); // build floor (1 = no dip)
const DROP_DIP_LEN = parseFloat(flag("drop-dip-len", "6"));// seconds of dip before the drop
const SAW_GAIN    = parseFloat(flag("saw", "0"));         // chainsaw sample REMOVED (@jeffrey)
const WIND_GAIN   = parseFloat(flag("wind", "0"));        // tornado REMOVED — too loud/annoying (@jeffrey)
// @jeffrey 2026-06-02: an "aesthetic dot computer" voice stamp before the
// drop, but the "com" is replaced by a single crow CAW (crow ≈ caw ≈ com)
// landing at 2:09 ON THE BEAT (129.0 s = beat 258 on the 0.5 s grid).
const STAMP_GAIN  = parseFloat(flag("stamp", "0"));       // "aesthetic dot computer" stamp REMOVED (@jeffrey)
const STAMP_AT    = parseFloat(flag("stamp-at", "150.0"));// 2:30, on the beat (150.0 = bar 75 downbeat)
const CROW_GAIN   = parseFloat(flag("crow", "0.85"));     // single crow caw (0 = off)
const CROW_AT     = parseFloat(flag("crow-at", "129.0")); // 2:09, on the beat (stays put — separate from stamp)
const PUNCH_GAIN  = parseFloat(flag("punch", "0"));       // boxing-glove combo REMOVED (@jeffrey)
const HUM_GAIN    = parseFloat(flag("hum", "0.13"));      // car-horn — quieter, the drop was too busy (@jeffrey)
const HUM_DELAY   = parseFloat(flag("hum-delay", "0"));   // from the START of the track now (@jeffrey)
const HUM_DUCK    = !argv.includes("--no-hum-duck");      // sidechain the hum to the bed's kick
const HUM_VERB    = parseFloat(flag("hum-verb", "0.30")); // reverb wash so the horn blends with the saws
// @jeffrey 2026-06-02: pull the jeffrey-pvc vowel drone OUT "for now" —
// keep the bed + boot chime + car-horn intro + FX. The vowel stem stays
// on disk; --no-vox just leaves it out of this mix.
const NO_VOX      = argv.includes("--no-vox");
const CHIME_GAIN  = parseFloat(flag("chime", "0.5"));     // boot chime — flourish at the end of the drop (@jeffrey)
// @jeffrey 2026-06-02: the boot chime sits WAY quiet + deep in reverb,
// landing at ~32 s — right after the cowbell (24 s), as part of the build
// ramp into the WOOP. ON THE BEAT (120 BPM → 32.0 s = bar 16 downbeat).
const CHIME_DELAY = parseFloat(flag("chime-delay", "162.0")); // ~2:42, end of drop, on bar 81 downbeat

const ENGINE   = `${C}/amaythingra`;
const ENGINE_C = `${C}/amaythingra.c`;
const BUILD    = `${C}/build-amaythingra.sh`;
const BED_WAV  = `${outDir}/.amaythingra-vowels-bed.wav`;
const VOX_WAV  = resolve(POP, "big-pictures/out/amaythingra-vowels.wav");
const CHIME_WAV = resolve(POP, "big-pictures/out/boot-chime.wav");
const HUM_WAV   = resolve(POP, "big-pictures/out/intro-hum.wav");
const FINAL_WAV = `${outDir}/amaythingra-MASTER.wav`;
const FINAL_MP3 = `${outDir}/amaythingra.mp3`;

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

// Opening elements — auto-generate if missing so a bake is self-contained.
const BIN = resolve(POP, "big-pictures/bin");
if (CHIME_GAIN > 0 && !existsSync(CHIME_WAV)) {
  run("node", [`${BIN}/make-boot-chime.mjs`], "synth boot chime");
}
if (HUM_GAIN > 0 && !existsSync(HUM_WAV)) {
  run("node", [`${BIN}/make-hum-loop.mjs`], "build granular intro hum");
}
const USE_CHIME = CHIME_GAIN > 0 && existsSync(CHIME_WAV);
const USE_HUM   = HUM_GAIN > 0 && existsSync(HUM_WAV);

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
const PRE_WAV = `${outDir}/.amaythingra-vowels-premix.wav`;
// Bed conditioning: fold everything below MONO_BASS_HZ to mono so the
// low end is phase-coherent (club-safe), keeping the highs stereo-wide.
// Only the full mix (vowels in) needs a sidechain key split off the bed.
// bed-only and --no-vox don't, so they keep a bare [bedmain] label (an
// unconnected [bedkey] pad would make ffmpeg error). A bare label attaches
// with no comma; a filter (asplit) attaches after a comma.
const includeVox = !BED_ONLY && !NO_VOX;
// Sidechain keys: the vowels duck to the kick, and so does the car horn
// (@jeffrey "car hum side chained to the kick"). Split off as many bed keys
// as there are consumers.
const needKeyHum = !BED_ONLY && USE_HUM && HUM_DUCK && HUM_GAIN > 0;
const keyConsumers = (includeVox ? 1 : 0) + (needKeyHum ? 1 : 0);
const bedTail = keyConsumers > 0 ? `,asplit=2[bedmain][bedkey0]` : `[bedmain]`;
const bedCond = MONO_BASS_HZ > 0
  ? `[0:a]aresample=48000,aformat=channel_layouts=stereo,asplit=2[blo][bhi];` +
    `[blo]lowpass=f=${MONO_BASS_HZ},` +
      `pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[blom];` +
    `[bhi]highpass=f=${MONO_BASS_HZ}[bhim];` +
    `[blom][bhim]amix=inputs=2:normalize=0${bedTail}`
  : `[0:a]aresample=48000,aformat=channel_layouts=stereo${bedTail}`;
// Bed swell-in — LINEAR (tri) so the kit comes up steadily and is clearly
// audible by ~5 s (the C engine already opens with church bells 0-6 s and
// a quiet kick entry), WITHOUT slamming at t=0. The car horn then enters
// at HUM_DELAY, well after the kicks are established (@jeffrey).
const bedFade = BED_FADEIN > 0
  ? `,afade=t=in:st=0:d=${BED_FADEIN.toFixed(2)}:curve=tri`
  : "";

// Hand out the bed sidechain key(s) to their consumers (vowels and/or hum).
let keyVox = null, keyHum = null, keySplitStage = null;
if (keyConsumers === 1) {
  if (includeVox) keyVox = "[bedkey0]"; else keyHum = "[bedkey0]";
} else if (keyConsumers === 2) {
  keySplitStage = `[bedkey0]asplit=2[bedkeyv][bedkeyh]`;
  keyVox = "[bedkeyv]"; keyHum = "[bedkeyh]";
}

// Opening layers ride as extra inputs after BED(0) and VOX(1) — VOX is
// always present on the command line (input 1) but only referenced when
// includeVox. The boot chime fires at the very top; the car-horn loop
// swells in AFTER the kicks are established (HUM_DELAY), then dissolves.
const extraInputs = [];
const introStages = [];
const introLabels = [];
if (!BED_ONLY && USE_CHIME) {
  const idx = 2 + extraInputs.length;
  extraInputs.push(CHIME_WAV);
  const ms = Math.round(CHIME_DELAY * 1000);
  // Deep reverb: pad a long tail first (aecho can't extend duration), then
  // dense early reflections + a long bloom, darken the wash, drop the dry
  // (in_gain 0.45) so it sits FAR back, then delay onto the beat at 48 s.
  introStages.push(
    `[${idx}:a]aresample=48000,aformat=channel_layouts=stereo,` +
    `volume=${CHIME_GAIN},apad=pad_dur=6,` +
    `aecho=0.45:0.9:80|160|290|450:0.6|0.45|0.32|0.22,` +
    `aecho=0.7:0.85:650|1000|1500:0.45|0.3|0.18,lowpass=f=5200,` +
    `adelay=${ms}|${ms}[chime]`);
  introLabels.push(`[chime]`);
}
if (!BED_ONLY && USE_HUM) {
  const idx = 2 + extraInputs.length;
  extraInputs.push(HUM_WAV);
  const ms = Math.round(HUM_DELAY * 1000);
  // reverb wash → level → delay onto the kick grid. The wash lets the horn
  // sit in the saws' space so the two blend (@jeffrey "saws and hum should
  // blend more"). Then sidechain-duck it to the kick.
  const humVerb = HUM_VERB > 0
    ? `aecho=0.85:0.88:110|200:${HUM_VERB.toFixed(2)}|${(HUM_VERB * 0.7).toFixed(2)},`
    : "";
  introStages.push(
    `[${idx}:a]aresample=48000,aformat=channel_layouts=stereo,${humVerb}` +
    `volume=${HUM_GAIN},adelay=${ms}|${ms}[humpre]`);
  if (needKeyHum) {
    introStages.push(
      `[humpre]${keyHum}sidechaincompress=threshold=0.05:ratio=4:` +
      `attack=4:release=180:makeup=1.5:level_sc=0.8[hum]`);
  } else {
    introStages.push(`[humpre]acopy[hum]`);
  }
  introLabels.push(`[hum]`);
}

// Assemble the mix label list: bed always, vowels only when included,
// then the intro layers.
const mixLabels = [`[bedlvl]`];
// Build-dip automation: 1.0 until (DROP_AT - DROP_DIP_LEN), ramp down to
// DROP_DIP right at DROP_AT, then snap back to 1.0 — the anticipation dip
// + drop impact. Disabled when DROP_DIP >= 1.
const dipStart = (DROP_AT - DROP_DIP_LEN).toFixed(3);
const dropAtS  = DROP_AT.toFixed(3);
const dropAuto = DROP_DIP < 1
  ? `,volume='if(lt(t,${dipStart}),1,if(lt(t,${dropAtS}),` +
    `1-${(1 - DROP_DIP).toFixed(3)}*(t-${dipStart})/${DROP_DIP_LEN.toFixed(3)},1))':eval=frame`
  : "";
const stages = [bedCond, `[bedmain]volume=${BED_GAIN}${bedFade}${dropAuto}[bedlvl]`];
if (keySplitStage) stages.push(keySplitStage);
if (includeVox) {
  let voxChain;
  if (VOX_LAST > 0) {
    // Outro-only vocals: take the TAIL of the vowel stem and place it so it
    // occupies just the final VOX_LAST seconds of the track, swelling in.
    // The master's 7 s tail fade rides it out at the very end.
    const winLen = Math.min(VOX_LAST, voxDur);
    const voxStart = Math.max(0, voxDur - winLen);
    const placeMs = Math.round(Math.max(0, total - winLen) * 1000);
    const fadeIn = Math.min(VOX_FADEIN, winLen * 0.4);
    voxChain =
      `[1:a]aresample=48000,aformat=channel_layouts=stereo,` +
      `atrim=start=${voxStart.toFixed(3)}:end=${voxDur.toFixed(3)},asetpts=PTS-STARTPTS,` +
      `${reverbStage}volume=${VOX_GAIN},` +
      `afade=t=in:st=0:d=${fadeIn.toFixed(2)}:curve=exp,` +
      `adelay=${placeMs}|${placeMs}[voxwet]`;
  } else {
    voxChain =
      `[1:a]aresample=48000,aformat=channel_layouts=stereo,${reverbStage}` +
      `volume=${VOX_GAIN},afade=t=in:st=0:d=${VOX_FADEIN.toFixed(2)}:curve=exp,` +
      `adelay=${voxDelayMs}|${voxDelayMs}[voxwet]`;
  }
  stages.push(
    voxChain,
    `[voxwet]${keyVox}sidechaincompress=threshold=0.04:ratio=${DUCK_RATIO}:` +
      `attack=5:release=240:makeup=2:level_sc=0.8[voxducked]`);
  mixLabels.push(`[voxducked]`);
}
stages.push(...introStages);
mixLabels.push(...introLabels);

const filter = BED_ONLY
  ? [bedCond, `[bedmain]volume=${BED_GAIN}${bedFade},atrim=duration=${total.toFixed(3)}[out]`].join(";")
  : mixLabels.length === 1
    ? [...stages, `[bedlvl]atrim=duration=${total.toFixed(3)}[out]`].join(";")
    : [...stages, `${mixLabels.join("")}amix=inputs=${mixLabels.length}:duration=longest:` +
        `dropout_transition=0:normalize=0,atrim=duration=${total.toFixed(3)}[out]`].join(";");

console.log(`→ mixing: ${includeVox ? "vowels sidechained to kick · " : "(no vox) · "}` +
  `bed fade-in ${BED_FADEIN}s${USE_CHIME ? ` · boot chime @${CHIME_DELAY}s (reverb)` : ""}` +
  `${USE_HUM ? ` · car horn @${HUM_DELAY}s` : ""} (premix)...`);
execSync(
  `ffmpeg -y -loglevel error -i "${BED_WAV}" -i "${VOX_WAV}" ` +
  extraInputs.map((p) => `-i "${p}" `).join("") +
  `-filter_complex "${filter}" -map "[out]" ` +
  `-ar 48000 -ac 2 -c:a pcm_s16le "${PRE_WAV}"`,
  { stdio: ["ignore", "ignore", "inherit"] }
);

// 3b — FX SAMPLE LAYERS. Each present sample becomes one input + one
//      filter stage producing [fxN]; all present layers amix together.
//      Built as a list so a missing sample just drops its layer instead
//      of skipping the whole stage. Chainsaw + tornado live in the vault
//      cache; the voice stamp + crow ship in the repo.
let masterSrc = PRE_WAV;
const FX_ENABLED = !argv.includes("--no-fx");
const CACHE = `${homedir()}/aesthetic-computer-vault/personal/pop/freesound-cache`;
const sawSample  = `${CACHE}/463731-chainsaw_cut_loop.wav`;
const windSample = `${CACHE}/251637-insideatornado_mp3.wav`;
const stampSample = resolve(POP, "big-pictures/out/cawmpoter-stamp.wav"); // jeffrey-pvc, pitched
const crowSample  = resolve(REPO, "pop/hellsine/samples/crow.wav");
// Auto-generate the pitched jeffrey stamp if missing (pitch-shifts the
// locally-cached say.mjs output — does NOT re-hit the paid /api/say).
if (!argv.includes("--no-fx") && STAMP_GAIN > 0 && !existsSync(stampSample) &&
    existsSync(resolve(POP, "big-pictures/voice-takes/stamp/cawmpoter-jeffrey.mp3"))) {
  run("node", [`${BIN}/make-stamp.mjs`], "pitch jeffrey-pvc stamp");
}

const fxInputs = [];
const fxStages = [];
const fxLabels = [];
function addFx(path, gain, makeStage) {
  if (!path || gain <= 0 || !existsSync(path)) return;
  const idx = fxInputs.length;
  fxInputs.push(path);
  const label = `fx${idx}`;
  fxStages.push(makeStage(idx, label));
  fxLabels.push(`[${label}]`);
}

// pitched-DOWN chainsaw grind (varispeed ×0.6) at ~1:24 (84 s), tucked low
addFx(sawSample, SAW_GAIN, (i, l) =>
  `[${i}:a]atrim=0:12,asetrate=48000*0.6,aresample=48000,aformat=channel_layouts=stereo,` +
  `lowpass=f=3500,volume=${SAW_GAIN},afade=t=in:st=0:d=2.5,afade=t=out:st=8:d=3.5,adelay=84000|84000[${l}]`);
// tornado-wind wash through the break (~96 s), atmosphere not a gust
addFx(windSample, WIND_GAIN, (i, l) =>
  `[${i}:a]atrim=0:34,aformat=channel_layouts=stereo,volume=${WIND_GAIN},` +
  `aecho=0.8:0.85:140|240:0.35|0.25,afade=t=in:st=0:d=6,afade=t=out:st=28:d=6,adelay=96000|96000[${l}]`);
// jeffrey-pvc "aesthetic dot computer" (pitched up) — full phrase, starting
// ON THE BEAT at STAMP_AT (2:30). Separate from the crow now (@jeffrey).
const stampMs = Math.round(STAMP_AT * 1000);
addFx(stampSample, STAMP_GAIN, (i, l) =>
  `[${i}:a]aformat=channel_layouts=stereo,` +
  `volume=${STAMP_GAIN},apad=pad_dur=2,` +
  `aecho=0.6:0.7:150|300:0.30|0.18,adelay=${stampMs}|${stampMs}[${l}]`);
// ONE crow CAW (first caw only, 0–0.58 s), ON THE BEAT at CROW_AT (2:09)
const crowMs = Math.round(CROW_AT * 1000);
addFx(crowSample, CROW_GAIN, (i, l) =>
  `[${i}:a]atrim=0:0.58,asetpts=PTS-STARTPTS,aformat=channel_layouts=stereo,` +
  `volume=${CROW_GAIN},afade=t=out:st=0.50:d=0.08,apad=pad_dur=2,` +
  `aecho=0.7:0.7:120|260:0.30|0.20,adelay=${crowMs}|${crowMs}[${l}]`);
// BOXING-GLOVE COMBO coming in after the cowbell (24 s), before the WOOP
// ramp — loosely around the kicks but a bit on their own (slightly off the
// grid), rising in as it enters, alternating L/R (@jeffrey).
const punchSamples = [
  `${CACHE}/348242-punch_boxing_03_wav.wav`,
  `${CACHE}/348243-punch_boxing_02_wav.wav`,
];
const punchHits = [
  { t: 24.12, g: 0.60, pan: -0.5 },
  { t: 24.58, g: 0.78, pan:  0.5 },
  { t: 25.06, g: 0.90, pan: -0.4 },
  { t: 25.55, g: 1.00, pan:  0.4 },
  { t: 25.96, g: 1.00, pan:  0.0 },
];
punchHits.forEach((p, k) => {
  const samp = punchSamples[k % punchSamples.length];
  const ms = Math.round(p.t * 1000);
  const g = (PUNCH_GAIN * p.g).toFixed(3);
  const Lc = (1 - Math.max(0, p.pan)).toFixed(2);
  const Rc = (1 + Math.min(0, p.pan)).toFixed(2);
  addFx(samp, PUNCH_GAIN, (i, l) =>
    `[${i}:a]atrim=0:0.4,asetpts=PTS-STARTPTS,aformat=channel_layouts=stereo,` +
    `pan=stereo|c0=${Lc}*c0|c1=${Rc}*c1,volume=${g},` +
    `afade=t=out:st=0.32:d=0.08,adelay=${ms}|${ms}[${l}]`);
});

if (FX_ENABLED && fxLabels.length) {
  const FX_WAV = `${outDir}/.amaythingra-vowels-fx.wav`;
  const mixTail = fxLabels.length === 1
    ? `${fxLabels[0]}apad,atrim=duration=${total.toFixed(3)}[fx]`
    : `${fxLabels.join("")}amix=inputs=${fxLabels.length}:duration=longest:normalize=0,apad,` +
      `atrim=duration=${total.toFixed(3)}[fx]`;
  const fxF = [...fxStages, mixTail].join(";");
  execSync(`ffmpeg -y -loglevel error ${fxInputs.map((p) => `-i "${p}"`).join(" ")} ` +
           `-filter_complex "${fxF}" -map "[fx]" -ar 48000 -ac 2 -c:a pcm_s16le "${FX_WAV}"`);
  const COMBINED = `${outDir}/.amaythingra-vowels-premix-fx.wav`;
  execSync(`ffmpeg -y -loglevel error -i "${PRE_WAV}" -i "${FX_WAV}" ` +
           `-filter_complex "[0:a][1:a]amix=inputs=2:duration=longest:normalize=0[m]" ` +
           `-map "[m]" -ar 48000 -ac 2 -c:a pcm_s16le "${COMBINED}"`);
  masterSrc = COMBINED;
  const names = [];
  if (existsSync(sawSample) && SAW_GAIN > 0) names.push("chainsaw@1:24");
  if (existsSync(windSample) && WIND_GAIN > 0) names.push("tornado/break");
  if (existsSync(stampSample) && STAMP_GAIN > 0) names.push(`stamp @${STAMP_AT}s`);
  if (existsSync(crowSample) && CROW_GAIN > 0) names.push(`crow caw @${CROW_AT}s`);
  console.log(`→ fx layered: ${names.join(" + ")}`);
}

// ── MASTER (DistroKid-ready) ─────────────────────────────────────────
// 1) gentle glue compressor, 2) LINEAR two-pass loudnorm to the streaming
// target (-14 LUFS, -1 dBTP) — linear mode applies a CONSTANT gain so the
// quiet intro/outro fades are preserved (dynamic loudnorm pumped them),
// 3) a true-peak alimiter safety so nothing clips. Then a 7 s tail fade.
// Exports: 48k/24-bit master WAV, 44.1k/16-bit DistroKid WAV, 320k mp3.
const LUFS = parseFloat(flag("lufs", "-14"));   // streaming target
const FADE = `afade=t=out:st=${(total - 7).toFixed(2)}:d=7`;
const COMP = "acompressor=threshold=-18dB:ratio=2:attack=20:release=200:makeup=1:detection=rms";
const DK_WAV = `${outDir}/amaythingra-DistroKid.wav`;

// pass A — glue compressor + tail fade → 24-bit premaster
const PREMASTER = `${outDir}/.amaythingra-premaster.wav`;
console.log(`→ master: glue compressor + ${LUFS} LUFS linear loudnorm + -1 dBTP limiter...`);
execSync(`ffmpeg -y -loglevel error -i "${masterSrc}" -af "${COMP},${FADE}" ` +
         `-ar 48000 -ac 2 -c:a pcm_s24le "${PREMASTER}"`);

// pass B — measure loudness, then apply LINEAR loudnorm to the target
const meas = execSync(
  `ffmpeg -hide_banner -i "${PREMASTER}" ` +
  `-af "loudnorm=I=${LUFS}:TP=-1:LRA=11:print_format=json" -f null - 2>&1`
).toString();
const mj = JSON.parse(meas.slice(meas.indexOf("{"), meas.lastIndexOf("}") + 1));
console.log(`  measured premaster: I=${mj.input_i} LUFS · TP=${mj.input_tp} dBTP · LRA=${mj.input_lra}`);
const ln = `loudnorm=I=${LUFS}:TP=-1:LRA=11:measured_I=${mj.input_i}:measured_TP=${mj.input_tp}:` +
  `measured_LRA=${mj.input_lra}:measured_thresh=${mj.input_thresh}:offset=${mj.target_offset}:linear=true`;
execSync(`ffmpeg -y -loglevel error -i "${PREMASTER}" ` +
         `-af "${ln},alimiter=limit=-1dB:level=false" ` +
         `-ar 48000 -ac 2 -c:a pcm_s24le "${FINAL_WAV}"`);

// DistroKid upload file: 44.1k / 16-bit with triangular dither
execSync(`ffmpeg -y -loglevel error -i "${FINAL_WAV}" ` +
         `-af "aresample=44100:dither_method=triangular" -ar 44100 -ac 2 -c:a pcm_s16le "${DK_WAV}"`);
execSync(`ffmpeg -y -loglevel error -i "${FINAL_WAV}" -b:a 320k "${FINAL_MP3}"`);

// ── ANALYSIS: clipping + final loudness on the DistroKid file ────────
const stats = execSync(
  `ffmpeg -hide_banner -i "${DK_WAV}" -af astats=metadata=1 -f null - 2>&1`
).toString();
const peakDb  = parseFloat(stats.match(/Peak level dB:\s*(-?[\d.]+)/)?.[1] ?? "0");
const clipped = parseInt(stats.match(/Number of clipped samples:\s*(\d+)/)?.[1] ?? "0", 10);
const fmeas = execSync(
  `ffmpeg -hide_banner -i "${DK_WAV}" -af "loudnorm=I=${LUFS}:TP=-1:print_format=json" -f null - 2>&1`
).toString();
const fj = JSON.parse(fmeas.slice(fmeas.indexOf("{"), fmeas.lastIndexOf("}") + 1));

console.log(`\n── master analysis (DistroKid file) ──`);
console.log(`  integrated : ${fj.input_i} LUFS   (target ${LUFS})`);
console.log(`  true peak  : ${fj.input_tp} dBTP   (ceiling -1.0)`);
console.log(`  sample peak: ${peakDb} dBFS · clipped samples: ${clipped}` +
  `${clipped === 0 ? "  ✓ no clipping" : "  ⚠ CLIPPING"}`);

console.log(`\n✓ ${FINAL_WAV.replace(homedir(), "~")} (${dur(FINAL_WAV).toFixed(1)}s · 48k/24-bit master)`);
console.log(`✓ ${DK_WAV.replace(homedir(), "~")}  ← DistroKid upload (44.1k/16-bit, ${LUFS} LUFS)`);
console.log(`✓ ${FINAL_MP3.replace(homedir(), "~")}`);

// ── DISTROKID PACKAGE (--package): refresh the tagged WAV + cover-embedded
// mp3 on the Desktop so every bake keeps the release deliverable current.
if (argv.includes("--package")) {
  const PKG = `${homedir()}/Desktop/amaythingra-DistroKid`;
  mkdirSync(PKG, { recursive: true });
  const META = `-metadata title="Amaythingra" -metadata artist="Aesthetic Dot Computer" ` +
    `-metadata album_artist="Aesthetic Dot Computer" -metadata album="Pixsies" ` +
    `-metadata genre="Dance" -metadata date="2026"`;
  const deskWav = `${homedir()}/Desktop/amaythingra.wav`;
  const deskMp3 = `${homedir()}/Desktop/amaythingra.mp3`;
  const cover3000 = `${PKG}/cover-3000.png`;
  execSync(`ffmpeg -y -loglevel error -i "${DK_WAV}" -c:a copy ${META} "${deskWav}"`);
  execSync(`cp "${deskWav}" "${PKG}/amaythingra.wav"`);
  if (existsSync(cover3000)) {
    const embed = "/tmp/amaythingra-cover-embed.jpg";
    execSync(`magick "${cover3000}" -resize 1500x1500 -quality 90 "${embed}"`);
    execSync(`ffmpeg -y -loglevel error -i "${FINAL_MP3}" -i "${embed}" -map 0:a -map 1:v ` +
      `-c:a copy -c:v copy -id3v2_version 3 ${META} -metadata:s:v title="Album cover" ` +
      `-disposition:v:0 attached_pic "${deskMp3}"`);
  } else {
    execSync(`ffmpeg -y -loglevel error -i "${FINAL_MP3}" -c:a copy ${META} "${deskMp3}"`);
  }
  execSync(`cp "${deskMp3}" "${PKG}/amaythingra-reference.mp3"`);
  console.log(`✓ packaged → ${PKG.replace(homedir(), "~")} (tagged WAV + cover-embedded mp3, Title-Case)`);
}
console.log(`\n  open -a "QuickTime Player" "${FINAL_MP3}"`);
