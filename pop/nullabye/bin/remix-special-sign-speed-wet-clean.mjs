#!/usr/bin/env node
// Clean speed-following spatial return for Special Sign. Keep one intact dry
// master, extract only the wet render's antisymmetric stereo field, band-limit
// that return, and scale it with the authored speed automation. This avoids the
// moving full-band combs produced by crossfading two propagation timelines.

import { existsSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const SR = 48_000;
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};
const dryPath = resolve(arg("--dry", resolve(HERE, "../out/review/Special-Sign-SPEED-DRY-AUDITION.wav")));
const dryProvenancePath = resolve(arg("--dry-provenance", dryPath.replace(/\.[^.]+$/, ".provenance.json")));
const wetPath = resolve(arg("--wet", resolve(HERE, "../out/review/Special-Sign-SPEED-WET97-CLEANFIELD-AUDITION.wav")));
const automationPath = resolve(arg("--automation", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-AUDITION.automation.json")));
const scorePath = resolve(arg("--score", resolve(HERE, "../out/review/Special-Sign-SPEED-WET.scorodeon.json")));
const outPath = resolve(arg("--out", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-CLEAN-AUDITION.wav")));
const mp3Path = resolve(arg("--mp3", outPath.slice(0, -extname(outPath).length) + ".mp3"));
const targetLufs = Number(arg("--target-lufs", "-15"));
const maxTruePeak = Number(arg("--max-true-peak", "-1.2"));
const highpassHz = Number(arg("--return-highpass", "80"));
const lowpassHz = Number(arg("--return-lowpass", "11500"));
const sideGain = Number(arg("--side-gain", "1"));
const wetCurveExponent = Number(arg("--wet-curve-exponent", "1.10"));
const introWetCeiling = Number(arg("--intro-wet-ceiling", "0.24"));
const introDryEnd = Number(arg("--intro-dry-end", "20.5"));
const introReleaseEnd = Number(arg("--intro-release-end", "23"));
const motionDryPocket = { enter: 21.25, hold: 22.25, release: 25.25, exit: 26.75, wetCeiling: 0.035 };
const materialKickLevel = Number(arg("--material-kick-level", "0.085"));
const materialKickDuckDb = Number(arg("--material-kick-duck-db", "1.35"));
const materialKickOversample = 4;
const reverseKickLevel = Number(arg("--reverse-kick-level", "0.043"));
const reverseKickLead = Number(arg("--reverse-kick-lead", "0.42"));
const reverseBellLevel = Number(arg("--reverse-bell-level", "0.024"));
const reverseBellLead = Number(arg("--reverse-bell-lead", "1.28"));
const rawPath = `${outPath}.clean-speed-mix.f32`;
const premixPath = `${outPath}.premix.wav`;
for (const path of [dryPath, dryProvenancePath, wetPath, automationPath, scorePath]) if (!existsSync(path)) throw new Error(`missing input: ${path}`);

const run = (args, options = {}) => {
  const result = spawnSync("ffmpeg", args, options.capture
    ? { encoding: options.encoding, maxBuffer: 256 * 1024 * 1024 }
    : { stdio: "inherit" });
  if (result.status !== 0) throw new Error(`ffmpeg failed (${result.status})`);
  return result;
};
const decodeStereo = (path) => run(["-hide_banner", "-loglevel", "error", "-i", path,
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"], { capture: true }).stdout;
const automation = JSON.parse(readFileSync(automationPath, "utf8"));
const score = JSON.parse(readFileSync(scorePath, "utf8"));
const dryProvenance = JSON.parse(readFileSync(dryProvenancePath, "utf8"));
const points = automation.points;
const wetAt = (t) => {
  const u = Math.max(0, Math.min(points.length - 1, t * 60));
  const i = Math.min(points.length - 2, Math.floor(u)), f = u - i;
  const authored = points[i].wet * (1 - f) + points[i + 1].wet * f;
  let effective = authored;
  if (t <= introDryEnd) effective = Math.min(effective, introWetCeiling);
  else if (t < introReleaseEnd) {
    const release = (t - introDryEnd) / (introReleaseEnd - introDryEnd);
    effective = Math.min(effective, introWetCeiling + (1 - introWetCeiling) * release * release * (3 - 2 * release));
  }
  // The small system turn begins near 24 s but has not built enough velocity to
  // justify a large field yet. Let the direct signal carry its approach, then
  // hand spatial energy back only as the bodies gather speed.
  if (t >= motionDryPocket.enter && t <= motionDryPocket.exit) {
    let ceiling = motionDryPocket.wetCeiling;
    if (t < motionDryPocket.hold) {
      const u = (t - motionDryPocket.enter) / (motionDryPocket.hold - motionDryPocket.enter);
      const s = u * u * (3 - 2 * u);
      ceiling = 1 + (motionDryPocket.wetCeiling - 1) * s;
    } else if (t > motionDryPocket.release) {
      const u = (t - motionDryPocket.release) / (motionDryPocket.exit - motionDryPocket.release);
      const s = u * u * (3 - 2 * u);
      ceiling = motionDryPocket.wetCeiling + (1 - motionDryPocket.wetCeiling) * s;
    }
    effective = Math.min(effective, ceiling);
  }
  // A slight convex bend keeps the 97% speed crest essentially intact while
  // making intermediate motion more direct and tactile.
  return Math.pow(Math.max(0, Math.min(1, effective)), wetCurveExponent);
};

// The boom lane carries four near-simultaneous authored events per electro
// kick. Group them into one mastering trigger without inventing a new rhythm.
const boom = score.lanes.find((lane) => lane.name === "boom");
if (!boom) throw new Error("score has no boom lane for material-kick triggers");
const kickGroups = [];
for (const event of [...boom.events].sort((a, b) => a.t - b.t)) {
  const group = kickGroups.at(-1);
  if (!group || event.t - group[0].t > 0.012) kickGroups.push([event]); else group.push(event);
}
const kickTimes = kickGroups.filter((group) => group.length >= 4 && group.some((event) => event.kind === "noise"))
  .map((group) => group[0].t);
// Every eighth kick gets an intake. Keeping seven direct attacks between each
// reversed one makes the gesture structural instead of a constant effect.
const reverseKickTimes = kickTimes.filter((_, i) => i % 8 === 7).slice(1);
const bass = score.lanes.find((lane) => lane.name === "bass");
if (!bass) throw new Error("score has no bass lane for reverse-bell cadences");
// The fourth bass event closes each four-bar harmonic sentence. A reversed
// bell rises into those authored notes, alternating sides around the listener.
const reverseBellEvents = [...bass.events].sort((a, b) => a.t - b.t)
  .filter((_, i) => i % 4 === 3)
  .slice(1)
  .map((event, i) => ({ t: event.t, midi: event.pitch + 36, pan: i % 2 ? 0.58 : -0.58 }));
const materialKick = (age) => {
  if (age < 0 || age >= 0.42) return 0;
  let sample = 0;
  for (let os = 0; os < materialKickOversample; os++) {
    const t = age + (os + 0.5) / (SR * materialKickOversample);
    const f0 = 168, f1 = 43, tau = 0.030;
    const phase = Math.PI * 2 * (f1 * t + (f0 - f1) * tau * (1 - Math.exp(-t / tau)));
    const body = Math.sin(phase) * Math.exp(-t / 0.155);
    const knock = Math.sin(phase * 2.13 + 0.24) * Math.exp(-t / 0.052);
    const beater = (Math.sin(Math.PI * 2 * 3400 * t) + 0.58 * Math.sin(Math.PI * 2 * 6100 * t + 0.3) +
      0.24 * Math.sin(Math.PI * 2 * 9700 * t + 0.8)) * Math.exp(-t / 0.0105);
    sample += 0.78 * body + 0.19 * knock + 0.12 * beater;
  }
  return sample / materialKickOversample;
};
const reverseKick = (remaining) => {
  if (remaining < 0 || remaining >= reverseKickLead) return 0;
  // Read the synthesized kick backward, stopping a few milliseconds into its
  // attack so the forward material kick owns the actual edge.
  const sourceAge = Math.max(0, reverseKickLead - remaining + 0.004);
  const edgeFade = Math.min(1, remaining / 0.006);
  return materialKick(sourceAge) * edgeFade;
};
const reverseBell = (remaining, midi) => {
  if (remaining < 0 || remaining >= reverseBellLead) return 0;
  const u = 1 - remaining / reverseBellLead;
  const envelope = Math.pow(u, 2.35) * Math.min(1, remaining / 0.008);
  const hz = 440 * Math.pow(2, (midi - 69) / 12);
  const ratios = [1, 2.01, 2.74, 4.08, 5.43];
  const gains = [1, 0.48, 0.31, 0.18, 0.11];
  let bell = 0;
  for (let i = 0; i < ratios.length; i++) {
    // Phase runs backward with the source tail while the amplitude grows.
    bell += Math.sin(Math.PI * 2 * hz * ratios[i] * remaining + i * 0.37) * gains[i];
  }
  return bell * envelope / 1.72;
};

console.log("[mix] decoding dry anchor and clean spatial field…");
const dry = decodeStereo(dryPath), wet = decodeStereo(wetPath);
const frames = Math.min(dry.length, wet.length) / 8 | 0;
const mixed = Buffer.allocUnsafe(frames * 8);
const hpAlpha = (1 / (Math.PI * 2 * highpassHz)) / (1 / (Math.PI * 2 * highpassHz) + 1 / SR);
const lpK = 1 - Math.exp(-Math.PI * 2 * lowpassHz / SR);
let hp = 0, lp = 0, previous = 0;
let kickIndex = 0;
let reverseKickIndex = 0, reverseBellIndex = 0;
for (let frame = 0; frame < frames; frame++) {
  const offset = frame * 8;
  const dryL = dry.readFloatLE(offset), dryR = dry.readFloatLE(offset + 4);
  const side = (wet.readFloatLE(offset) - wet.readFloatLE(offset + 4)) * 0.5;
  hp = hpAlpha * (hp + side - previous); previous = side;
  lp += lpK * (hp - lp);
  const t = frame / SR, send = wetAt(t) * sideGain;
  while (kickIndex + 1 < kickTimes.length && kickTimes[kickIndex + 1] <= t) kickIndex++;
  const kickAge = t - kickTimes[kickIndex];
  const duckEnv = kickAge >= 0 && kickAge < 0.8 ? Math.exp(-kickAge / 0.185) : 0;
  // Sidechain depth follows the same spatial-motion send. Near-dry passages
  // retain only a shallow balancing dip; fully wet spins receive the full duck.
  const movingDuckDb = materialKickDuckDb * (0.30 + 0.70 * Math.sqrt(Math.max(0, Math.min(1, send))));
  const duckGain = Math.pow(10, -(movingDuckDb * duckEnv) / 20);
  const kick = materialKick(kickAge) * materialKickLevel;
  while (reverseKickIndex + 1 < reverseKickTimes.length && reverseKickTimes[reverseKickIndex] < t) reverseKickIndex++;
  const kickReverse = reverseKick(reverseKickTimes[reverseKickIndex] - t) * reverseKickLevel;
  while (reverseBellIndex + 1 < reverseBellEvents.length && reverseBellEvents[reverseBellIndex].t < t) reverseBellIndex++;
  const bellEvent = reverseBellEvents[reverseBellIndex];
  const bell = reverseBell(bellEvent.t - t, bellEvent.midi) * reverseBellLevel;
  const bellAngle = (bellEvent.pan + 1) * Math.PI / 4;
  mixed.writeFloatLE((dryL + lp * send) * duckGain + kick + kickReverse + bell * Math.cos(bellAngle), offset);
  mixed.writeFloatLE((dryR - lp * send) * duckGain + kick + kickReverse + bell * Math.sin(bellAngle), offset + 4);
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
unlinkSync(rawPath); unlinkSync(premixPath);

const provenancePath = outPath.replace(/\.[^.]+$/, ".provenance.json");
const finalMix = {
  acceptedAudition: outPath.replace(`${REPO}/`, ""), base: dryPath.replace(`${REPO}/`, ""), spatialField: wetPath.replace(`${REPO}/`, ""),
  automation: automationPath.replace(`${REPO}/`, ""), topology: "intact dry master plus antisymmetric speed-following spatial return",
  returnFilterHz: { highpass: highpassHz, lowpass: lowpassHz }, sideGain, wetCurveExponent,
  introSpatialCap: { wetCeiling: introWetCeiling, releaseSeconds: [0, introDryEnd], transitionEndSeconds: introReleaseEnd },
  motionDryPocket: { ...motionDryPocket, purpose: "keep the low-velocity turn near 24 seconds direct and remove the apparent amplitude dip" },
  materialKick: { name: "material kick", triggers: kickTimes.length, oversample: materialKickOversample,
    level: materialKickLevel, sidechainDuckDbMaximum: materialKickDuckDb, sidechainMotionFollow: "30% floor plus 70% square-root of spatial send", sidechainReleaseSeconds: 0.185,
    placement: "whole-mix sidechain key; dry mono return added after the spatial field" },
  reverseGestures: {
    kick: { triggers: reverseKickTimes, every: "eighth authored material kick", leadSeconds: reverseKickLead,
      level: reverseKickLevel, placement: "dry mono mastering return" },
    bell: { events: reverseBellEvents, every: "fourth authored bass event", leadSeconds: reverseBellLead,
      level: reverseBellLevel, placement: "alternating wide stereo mastering return" },
  },
  monoInvariant: "the added return is L=-R and cancels exactly before the final linear trim",
  fuzzRemoval: "no full-band dry/wet crossfade; one direct timeline plus filtered side field",
  measuredPremixLufs: measuredLufs, measuredPremixTruePeakDb: measuredTruePeak,
  targetLufs, maxTruePeakDb: maxTruePeak, linearGainDb: gainDb,
};
writeFileSync(provenancePath, JSON.stringify({ ...dryProvenance, finalMix }, null, 2) + "\n");
const effectiveAutomationPath = outPath.replace(/\.[^.]+$/, ".automation.json");
writeFileSync(effectiveAutomationPath, JSON.stringify({
  ...automation,
  title: "Special Sign — final motion-dependent spatial automation",
  sourceAutomation: automationPath.replace(`${REPO}/`, ""),
  mapping: { ...automation.mapping, finalWetCurveExponent: wetCurveExponent, motionDryPocket },
  points: points.map((point) => ({ ...point, wet: +wetAt(point.t).toFixed(6) })),
}, null, 2) + "\n");
console.log(`✓ ${outPath}`);
console.log(`✓ ${mp3Path}`);
console.log(`✓ ${provenancePath}`);
console.log(`✓ ${effectiveAutomationPath}`);
console.log(`  ${measuredLufs.toFixed(2)} LUFS / ${measuredTruePeak.toFixed(2)} dBTP premix · ${gainDb.toFixed(2)} dB linear trim`);
