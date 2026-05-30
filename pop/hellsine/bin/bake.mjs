#!/usr/bin/env node
// bake.mjs — hellsine: engine → (opt scratch post-FX) → Spotify finalize.
//
// The engine (hellsine.mjs) is the source mix — all-sine, plus the one
// sampled exception (@jeffrey's rattle, if recorded). This driver:
//   1. renders the engine ONCE → pre-master float WAV + struct.json,
//   2. (optional, --scratch) runs pop/dance/bin/scratch-mix.mjs on that
//      one buffer, beat-locked to hellsine's own struct.json grid.
//      OFF by default: the chop&screw turntable gesture fights the
//      John-Williams reading on a first listen — opt in once the
//      composition is locked.
//   3. finalizes for Spotify: gentle glue → loudnorm I=-14 / TP=-1.5 →
//      limiter → end fade → hellsine-MASTER.wav + hellsine.mp3.
//
// Pure-sine mixes render dark; hardcore saturation adds upper harmonics
// so hellsine starts brighter than the trance bed, but A/B every master
// against a brightened take before release (see pop/RELEASES.md).
//
// Usage: node pop/hellsine/bin/bake.mjs [outDir] [--scratch]
//        [-- <extra engine flags e.g. --hell 14 --bpm 186>]
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { acdspAvailable, processWav, chain, eq, compressor } from "../../lib/master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
let args = process.argv.slice(2);
const SCRATCH_ON = args.includes("--scratch");
const USE_ACDSP  = args.includes("--acdsp");
args = args.filter((a) => a !== "--scratch" && a !== "--acdsp");
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
const outDir = (sep >= 0 ? args.slice(0, sep) : args)[0] ||
  `${homedir()}/Documents/Shelf/hellsine`;

const ENGINE = `${REPO}/pop/hellsine/bin/hellsine.mjs`;
const SCRATCH = `${REPO}/pop/dance/bin/scratch-mix.mjs`;
const pre = `${outDir}/.hellsine-pre.wav`;
const struct = `${pre.replace(/\.wav$/, "")}.assets/struct.json`;
const scr = `${outDir}/.hellsine-scr.wav`;
const finalWav = `${outDir}/hellsine-MASTER.wav`;
const finalMp3 = `${outDir}/hellsine.mp3`;

const run = (cmd, a, label) => {
  console.log(`\n[bake] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`[bake] FAILED: ${label}`); process.exit(1); }
};

// 1 — the single all-sine engine render. `--strategy ultimate` is the
// composed-through showcase that gates typewriter/splash/TTS-mantra/
// jeffrey-pvc/AC-stamp/kick-rattle-warps/perc-break — i.e. virtually
// every sample-driven layer of the track. Without it the mix collapses
// to bare sines + grenade-kick + rattle, no vocals. `engineExtra` after
// `--` can still override it (e.g. `-- --strategy stretto`).
const hasStrategyOverride = engineExtra.some((a) => a === "--strategy");
const strategyArgs = hasStrategyOverride ? [] : ["--strategy", "ultimate"];
run("node", [ENGINE, "--out", pre, "--struct", struct, ...strategyArgs, ...engineExtra],
  "render engine (all-sine source mix)");

// 2 — optional in-mix post-FX, beat-locked to struct.json (no vocal
//     stamp: the concept track stays strictly instrumental + all-sine)
let mix = pre;
if (SCRATCH_ON) {
  run("node", [SCRATCH, pre, scr, "", struct],
    "post-FX (scratch + beat-locked breakbeat + FM growls)");
  mix = scr;
}

// 2.5 — FINAL-DROP PERMA SPEED-UP — @jeffrey: after the "aesthetic dot
// computer" stamp the track hits the climax drop at 110.77 s (statement
// + bridge + AC stamp first, THEN the gear-shift). From there until the
// end, atempo=1.06 stays on for the rest of the track — a permanent
// BPM lift through climax + coda + horse/train outro. Pre-AC-stamp
// content is untouched. The crow-section speed-bump was rejected, so
// it's no longer applied — the only tempo change is this one-way ramp.
const tempoOut = `${outDir}/.hellsine-tempo.wav`;
const TEMPO_CUT = 110.77;             // climax.startSec — the final drop
const TEMPO_MUL = 1.06;
const tempoChain = [
  "asplit=2[a0][a1]",
  `[a0]atrim=start=0:end=${TEMPO_CUT},asetpts=PTS-STARTPTS[t0]`,
  `[a1]atrim=start=${TEMPO_CUT},asetpts=PTS-STARTPTS,atempo=${TEMPO_MUL}[t1]`,
  "[t0][t1]concat=n=2:v=0:a=1[out]",
].join(";");
run("ffmpeg", ["-y", "-i", mix, "-filter_complex", tempoChain, "-map", "[out]",
  "-ar", "48000", "-c:a", "pcm_f32le", tempoOut],     // float — acdsp can't read s24le's WAVE_FORMAT_EXTENSIBLE
  `final-drop perma speed-up (atempo=${TEMPO_MUL} from ${TEMPO_CUT}s onward)`);
mix = tempoOut;

// 3 — finalize for Spotify (Spotify normalises to -14 on playback)
// The new mix (halftime hole-kick + 8-partial pads + sparkle + steam)
// is inherently brighter than the original dark gabber-punch cut, so
// the brightening pass is now a LIGHT polish. The old +8 dB shelf +
// dual presence/sparkle boosts piled into the 2-12 kHz range and read
// as buzz hash. Keep just a gentle air lift + warm the low end.
let finalizeIn = mix;
if (USE_ACDSP) {
  if (!acdspAvailable()) {
    console.error("[bake] --acdsp requested but pop/dsp/c/acdsp not built.");
    console.error("       build it: (cd pop/dsp/c && make)"); process.exit(1);
  }
  const acOut = `${outDir}/.hellsine-acdsp.wav`;
  // hellsine character chain — keeps the "light polish" intent of the
  // legacy chain (low body trim, low-mid scoop, gentle 1176 4:1 glue with
  // mild FET iron for upper-harmonic brightness, air lift). The 1176's
  // GR-modulated saturation does the brightening work the old +2 dB
  // 9 kHz high-shelf used to do, but musically (harmonics tied to the
  // dynamics) instead of statically.
  const spec = chain(
    eq({ type: "peak", f: 150, q: 1.0, g: -1 }),
    eq("mud", -2),
    compressor("1176", { ratio: 4, in: -3, out: +3, attack: 4, release: 4, iron: 0.5 }),
    eq("air", +2),
  );
  console.log(`\n[bake] acdsp character pass (1176 + EQ via C lib)\n        chain: ${spec}`);
  const r = processWav(mix, acOut, spec, { float: true });
  if (!r.ok) { console.error("[bake] acdsp failed:\n" + r.stderr); process.exit(1); }
  process.stderr.write(r.stderr);
  finalizeIn = acOut;
}

// CLASSICAL DYNAMIC ENVELOPE — pre-loudnorm gain ride that shapes the
// dynamic narrative across sections (master time). Now FULLY CONTINUOUS —
// no hard step at the bridge entrance (the old 0.78 → 0.55 cliff at
// 47.5 s read as an "unnaturally quieter" jump on first listen). Every
// segment hands off at the same value as the next.
//   0.0-15.8 (overture)  : 0.40 → 0.55, hushed exposition rising
//   15.8-23 (drop)       : 0.55 → 1.00 quick lift INTO the drop
//   23-47.5 (statement)  : 0.78 (recover from drop peak)
//   47.5-79.1 (bridge)   : 0.78 → 0.62 gradual descent (no cliff)
//   79.1-110.8 (develop) : 0.62 → 1.18 long build, lands at climax level
//   110.8-140.6 (climax) : 1.18, apex push (above unity)
//   140.6-162 (coda)     : 1.18 → 0.45 graceful taper
// Pre-loudnorm so the dynamic shape is preserved through normalization.
const dynEnv =
  "volume=eval=frame:volume='" +
    "if(lt(t,15.82), 0.40+0.15*(t/15.82)," +
    "if(lt(t,23.0), 0.55+0.45*((t-15.82)/7.18)," +
    "if(lt(t,47.5), 1.00+(0.78-1.00)*((t-23.0)/24.5)," +
    "if(lt(t,79.1), 0.78+(0.62-0.78)*((t-47.5)/31.6)," +
    "if(lt(t,110.8), 0.62+(1.18-0.62)*((t-79.1)/31.7)," +
    "if(lt(t,140.6), 1.18," +
    "1.18+(0.45-1.18)*((t-140.6)/21.4)))))))'";

const ffChain = USE_ACDSP
  // acdsp already handled compressor + EQ — just dynamic envelope + loudness + limit + fade.
  ? dynEnv + "," +
    "loudnorm=I=-14:TP=-1.5:LRA=11," +
    "alimiter=limit=0.88:attack=4:release=60:level=disabled," +
    "volume=-1.2dB," +
    // Long, gentle fade so the last second is genuinely silent before
    // truncation — @jeffrey heard a click at ~2:41 where the cut hit a
    // non-zero sample. fade in earlier (start at 158.0) + fade for 2.8 s
    // so the audio is ~−100 dB by the truncation point.
    "afade=t=out:st=158.0:d=2.8"
  // legacy chain (kept for A/B): clean acompressor + EQ + loudnorm + limit.
  : dynEnv + "," +
    "highshelf=f=9000:g=2," +
    "equalizer=f=150:t=q:w=1.0:g=-1," +
    "equalizer=f=320:t=q:w=1.4:g=-2," +
    "acompressor=threshold=-12dB:ratio=1.5:attack=10:release=80:makeup=1:knee=6," +
    "loudnorm=I=-14:TP=-1.5:LRA=11," +
    "alimiter=limit=0.88:attack=4:release=60:level=disabled," +
    "volume=-1.2dB," +
    // Earlier + longer fade to kill the end-of-file click @ ~2:41.
    "afade=t=out:st=158.0:d=2.8";

run("ffmpeg", ["-y", "-i", finalizeIn, "-af", ffChain,
  "-ar", "44100", "-sample_fmt", "s16",
  "-to", "162.0",      // truncate; climax-onward atempo 1.06 compresses ~55s → ~52s
  finalWav],
  `finalize → -14 LUFS (light polish)${USE_ACDSP ? " [acdsp]" : ""}`);
run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");
spawnSync("rm", ["-f", scr]);
if (USE_ACDSP) spawnSync("rm", ["-f", `${outDir}/.hellsine-acdsp.wav`]);
spawnSync("open", ["-a", "QuickTime Player", finalWav]);
console.log(`\n[bake] done → ${finalWav}${USE_ACDSP ? " (acdsp character)" : ""}`);
