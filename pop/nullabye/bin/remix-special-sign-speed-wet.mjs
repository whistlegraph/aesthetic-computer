#!/usr/bin/env node
// Crossfade matched dry and 97%-wet Special Sign renders from the authored
// spiral's instantaneous angular speed. Broad acceleration opens the spatial
// field; narrow alternating flybys briefly expose the direct body. The longer
// central eight-turn gesture reaches 97% wet at its velocity crest.

import { existsSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const SR = 48_000;
const CTRL = 240;
const BPM = 76;
const BAR = 4 * 60 / BPM;
const TAU = Math.PI * 2;
const RELEASE_START = 18.947368;
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};
const dryPath = resolve(arg("--dry", resolve(HERE, "../out/review/Special-Sign-SPEED-DRY-AUDITION.wav")));
const wetPath = resolve(arg("--wet", resolve(HERE, "../out/review/Special-Sign-SPEED-WET97-AUDITION.wav")));
const outPath = resolve(arg("--out", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-AUDITION.wav")));
const mp3Path = resolve(arg("--mp3", outPath.slice(0, -extname(outPath).length) + ".mp3"));
const startSec = Number(arg("--source-start", String(RELEASE_START)));
const targetLufs = Number(arg("--target-lufs", "-15"));
const maxTruePeak = Number(arg("--max-true-peak", "-1.2"));
const wetFloor = Number(arg("--wet-floor", "0.08"));
const wetCeiling = Number(arg("--wet-ceiling", "0.97"));
const speedAtCeiling = Number(arg("--speed-at-ceiling", "0.92"));
const flybyDryDepth = Number(arg("--flyby-dry-depth", "0.34"));
const rawPath = `${outPath}.speed-mix.f32`;
const premixPath = `${outPath}.premix.wav`;

for (const path of [dryPath, wetPath]) if (!existsSync(path)) throw new Error(`missing input: ${path}`);
const run = (args, options = {}) => {
  const result = spawnSync("ffmpeg", args, options.capture
    ? { encoding: options.encoding, maxBuffer: 256 * 1024 * 1024 }
    : { stdio: "inherit" });
  if (result.status !== 0) throw new Error(`ffmpeg failed (${result.status})`);
  return result;
};
const decodeStereo = (path) => run(["-hide_banner", "-loglevel", "error", "-i", path,
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"], { capture: true }).stdout;

const clamp01 = (x) => Math.max(0, Math.min(1, x));
const smooth01 = (x) => { x = clamp01(x); return x * x * (3 - 2 * x); };
const ease01 = (x) => { x = clamp01(x); return x * x * x * (10 + x * (-15 + 6 * x)); };
const endingMotionTime = (t) => {
  const end = 38 * BAR + 0.325, span = 6, start = end - span;
  if (t <= start) return t;
  const u = clamp01((t - start) / span);
  return start + span * (u - u * u + u * u * u / 3);
};
const burstTurns = (t, at, dur, turns, wobble, cycles) => {
  if (t <= at) return 0;
  if (t >= at + dur) return turns;
  const raw = (t - at) / dur, u = ease01(raw), window = Math.sin(Math.PI * raw) ** 2;
  const elastic = wobble * window * (0.76 * Math.sin(TAU * (cycles * raw + 0.075 * Math.sin(TAU * raw))) +
    0.24 * Math.sin(TAU * 0.67 * raw + 0.41));
  return turns * u + elastic / TAU;
};
const worldWobbleTurns = (t) => {
  const at = [10 * BAR + 0.37, 14 * BAR + 0.83, 26 * BAR - 0.64, 31 * BAR + 0.41];
  const dur = [10.8, 4.1, 12.4, 11.6], cycles = [1.37, 0.72, 2.18, 1.46], amp = [0.34, 0.36, 0.54, 0.38];
  for (let i = 0; i < at.length; i++) if (t >= at[i] && t <= at[i] + dur[i]) {
    const u = (t - at[i]) / dur[i];
    let e = Math.sin(Math.PI * u); e = smooth01(e * e);
    const drift = u + 0.075 * Math.sin(Math.PI * u) * Math.sin(TAU * (0.63 * u + i * 0.17));
    const phase = TAU * cycles[i] * drift;
    return 0.5 * amp[i] * e * (0.82 * Math.sin(phase) + 0.18 * Math.sin(phase * 0.47 + i * 1.31));
  }
  return 0;
};
const constellationTurns = (t) => {
  if (t <= 50) return 0;
  if (t >= 78) return 10;
  const raw = (t - 50) / 28;
  let turns = 2 * ease01(ease01(raw));
  if (t >= 58.5) {
    const r = clamp01((t - 58.5) / 16), window = Math.sin(Math.PI * r) ** 2;
    const wobble = 0.31 * window * (0.78 * Math.sin(TAU * (1.63 * r + 0.08 * Math.sin(TAU * r))) +
      0.22 * Math.sin(TAU * 0.71 * r + 0.4));
    turns += 8 * ease01(r) + wobble / TAU;
  }
  return turns;
};
const spiralTurns = (sourceTime) => {
  const t = endingMotionTime(sourceTime);
  return burstTurns(t, 32.6, 6.2, 32, 0.46, 1.42) +
    burstTurns(t, 43.6, 5.9, 2, 0.42, 1.18) +
    constellationTurns(t) + burstTurns(t, 85.7, 8.6, 4, 0.49, 1.73) +
    worldWobbleTurns(t);
};

console.log("[mix] decoding matched dry and 97%-wet renders…");
const dry = decodeStereo(dryPath), wet = decodeStereo(wetPath);
const frames = Math.min(dry.length, wet.length) / 8 | 0;
const duration = frames / SR;
const controls = Math.ceil(duration * CTRL) + 1;
const rawSpeed = new Float64Array(controls), turns = new Float64Array(controls);
const dt = 1 / CTRL;
for (let i = 0; i < controls; i++) {
  const t = Math.min(duration, i / CTRL), source = startSec + t;
  turns[i] = spiralTurns(source);
  rawSpeed[i] = Math.abs(spiralTurns(source + dt * 0.5) - spiralTurns(source - dt * 0.5)) / dt;
}
// A symmetric 100 ms control window reads movement rather than numerical
// micro-wobble, while the later narrow phase notch restores dry flyby detail.
const radius = Math.round(CTRL * 0.05), prefix = new Float64Array(controls + 1);
for (let i = 0; i < controls; i++) prefix[i + 1] = prefix[i] + rawSpeed[i];
const speed = new Float64Array(controls), wetMix = new Float64Array(controls);
let peakSpeed = 0, minWet = 1, maxWet = 0, flybyWindows = 0, wasDry = false;
for (let i = 0; i < controls; i++) {
  const a = Math.max(0, i - radius), b = Math.min(controls, i + radius + 1);
  speed[i] = (prefix[b] - prefix[a]) / (b - a);
  peakSpeed = Math.max(peakSpeed, speed[i]);
  const norm = clamp01(speed[i] / speedAtCeiling);
  let mix = wetFloor + (wetCeiling - wetFloor) * smooth01(norm);
  const nearest = Math.round(turns[i]);
  const alternatingPass = ((nearest % 4) + 4) % 4 === 2;
  const phaseDistance = Math.abs(turns[i] - nearest);
  const flyby = alternatingPass ? Math.exp(-0.5 * (phaseDistance / 0.055) ** 2) * smooth01(norm) : 0;
  mix = Math.max(wetFloor, mix - flybyDryDepth * flyby);
  wetMix[i] = mix;
  const dryNow = flyby > 0.35;
  if (dryNow && !wasDry) flybyWindows++;
  wasDry = dryNow;
  minWet = Math.min(minWet, mix); maxWet = Math.max(maxWet, mix);
}

console.log(`[mix] ${peakSpeed.toFixed(2)} turns/s peak · ${(maxWet * 100).toFixed(0)}% wet crest · ${flybyWindows} dry flyby windows`);
const mixed = Buffer.allocUnsafe(frames * 8);
for (let frame = 0; frame < frames; frame++) {
  const u = frame * CTRL / SR, ci = Math.min(controls - 2, Math.floor(u)), f = u - ci;
  const mix = wetMix[ci] * (1 - f) + wetMix[ci + 1] * f;
  const dryGain = Math.cos(mix * Math.PI / 2), wetGain = Math.sin(mix * Math.PI / 2);
  const offset = frame * 8;
  mixed.writeFloatLE(dry.readFloatLE(offset) * dryGain + wet.readFloatLE(offset) * wetGain, offset);
  mixed.writeFloatLE(dry.readFloatLE(offset + 4) * dryGain + wet.readFloatLE(offset + 4) * wetGain, offset + 4);
}
writeFileSync(rawPath, mixed);
run(["-hide_banner", "-y", "-loglevel", "error", "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", "alimiter=limit=.87:attack=8:release=150:level=disabled", "-c:a", "pcm_f32le", premixPath]);
const meter = run(["-hide_banner", "-nostats", "-loglevel", "info", "-i", premixPath,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"], { capture: true, encoding: "utf8" });
const field = (name) => {
  const match = (meter.stderr || "").match(new RegExp(`"${name}"\\s*:\\s*"([^"]+)"`));
  return match ? Number(match[1]) : NaN;
};
const measuredLufs = field("input_i"), measuredTruePeak = field("input_tp");
if (!Number.isFinite(measuredLufs) || !Number.isFinite(measuredTruePeak)) throw new Error("loudness measurement failed");
const gainDb = Math.min(targetLufs - measuredLufs, maxTruePeak - measuredTruePeak);
const finalFilter = `volume=${gainDb.toFixed(2)}dB`;
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premixPath, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "pcm_s24le", outPath]);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premixPath, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "libmp3lame", "-b:a", "320k", mp3Path]);

const automationPath = outPath.replace(/\.[^.]+$/, ".automation.json");
const stride = CTRL / 60;
const points = [];
for (let i = 0; i < controls; i += stride) points.push({
  t: +(i / CTRL).toFixed(4), speedTurnsPerSecond: +speed[i].toFixed(4), wet: +wetMix[i].toFixed(4),
});
writeFileSync(automationPath, JSON.stringify({
  title: "Special Sign speed-following wet automation", duration, sourceStartSeconds: startSec,
  inputs: { dry: dryPath.replace(`${REPO}/`, ""), wet97: wetPath.replace(`${REPO}/`, "") },
  centralSuperSpin: { sourceSeconds: [58.5, 74.5], releaseSeconds: [58.5 - startSec, 74.5 - startSec], turns: 8,
    durationSeconds: 16, previousDurationSeconds: 11, peakTurnsPerSecond: 0.9375 },
  mapping: { wetFloor, wetCeiling, speedAtCeilingTurnsPerSecond: speedAtCeiling,
    equalPowerCrossfade: true, flybyRule: "narrow dry notch on alternating integer-turn crossings",
    flybyDryDepth, flybyWindows },
  measured: { peakSpeedTurnsPerSecond: peakSpeed, minWet, maxWet, premixLufs: measuredLufs,
    premixTruePeakDb: measuredTruePeak, linearGainDb: gainDb, targetLufs, maxTruePeakDb: maxTruePeak },
  points,
}, null, 2) + "\n");
unlinkSync(rawPath); unlinkSync(premixPath);
console.log(`✓ ${outPath}`);
console.log(`✓ ${mp3Path}`);
console.log(`✓ ${automationPath}`);
console.log(`  ${measuredLufs.toFixed(2)} LUFS / ${measuredTruePeak.toFixed(2)} dBTP premix · ${gainDb.toFixed(2)} dB linear trim`);
