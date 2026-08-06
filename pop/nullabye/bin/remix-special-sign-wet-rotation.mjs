#!/usr/bin/env node
// Preserve the accepted Special Sign master as the dry anchor, then return the
// listener-relative engine mix through the stereo difference channel.  The
// wet return is present throughout and rises smoothly around the authored
// eight-turn super-spin.  Because it is L=-R, the added field cancels exactly
// in a mono fold before the final linear loudness trim.

import { existsSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};
const base = resolve(arg("--base", resolve(HERE, "../out/review/Special-Sign-ALL-BODIES-HUM-AUDITION.wav")));
const wet = resolve(arg("--wet", resolve(HERE, "../out/review/Special-Sign-WET68-AUDITION.wav")));
const out = resolve(arg("--out", resolve(HERE, "../out/review/Special-Sign-WET-ROTATION-AUDITION.wav")));
const mp3 = resolve(arg("--mp3", out.slice(0, -extname(out).length) + ".mp3"));
const baseline = Number(arg("--baseline", "0.20"));
const spin = Number(arg("--spin", "0.85"));
const spinStart = Number(arg("--spin-start", "42.552632"));
const spinEnd = Number(arg("--spin-end", "53.552632"));
const ramp = Number(arg("--ramp", "2"));
const targetLufs = Number(arg("--target-lufs", "-15"));
const maxTruePeak = Number(arg("--max-true-peak", "-1.2"));
const premix = `${out}.premix.wav`;

for (const path of [base, wet]) if (!existsSync(path)) throw new Error(`missing input: ${path}`);
const run = (args, capture = false) => {
  const result = spawnSync("ffmpeg", args, capture ? { encoding: "utf8" } : { stdio: "inherit" });
  if (result.status !== 0) throw new Error(`ffmpeg failed (${result.status})`);
  return result;
};

const riseStart = spinStart - ramp;
const fallEnd = spinEnd + ramp;
const lift = spin - baseline;
const envelope = `${baseline}+${lift}*(if(lt(t,${riseStart}),0,` +
  `if(lt(t,${spinStart}),(t-${riseStart})/${ramp},` +
  `if(lt(t,${spinEnd}),1,if(lt(t,${fallEnd}),(${fallEnd}-t)/${ramp},0)))))`;
const graph = `[1:a]pan=stereo|c0=.5*c0-.5*c1|c1=-.5*c0+.5*c1,` +
  `highpass=f=75,lowpass=f=15000,volume='${envelope}':eval=frame[wet];` +
  `[0:a][wet]amix=inputs=2:normalize=0:duration=first,` +
  `alimiter=limit=.87:attack=8:release=150:level=disabled[mix]`;

run(["-hide_banner", "-y", "-loglevel", "error", "-i", base, "-i", wet,
  "-filter_complex", graph, "-map", "[mix]", "-ar", "48000", "-c:a", "pcm_f32le", premix]);
const meter = run(["-hide_banner", "-nostats", "-loglevel", "info", "-i", premix,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"], true);
const field = (name) => {
  const match = (meter.stderr || "").match(new RegExp(`"${name}"\\s*:\\s*"([^"]+)"`));
  return match ? Number(match[1]) : NaN;
};
const measuredLufs = field("input_i");
const measuredTruePeak = field("input_tp");
if (!Number.isFinite(measuredLufs) || !Number.isFinite(measuredTruePeak)) throw new Error("loudness measurement failed");
const gainDb = Math.min(targetLufs - measuredLufs, maxTruePeak - measuredTruePeak);
const finalFilter = `volume=${gainDb.toFixed(2)}dB`;
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premix, "-af", finalFilter,
  "-ar", "48000", "-c:a", "pcm_s24le", out]);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premix, "-af", finalFilter,
  "-ar", "48000", "-c:a", "libmp3lame", "-b:a", "320k", mp3]);
unlinkSync(premix);

const baseProvenancePath = base.replace(/\.[^.]+$/, ".provenance.json");
const wetProvenancePath = wet.replace(/\.[^.]+$/, ".provenance.json");
const readJson = (path) => existsSync(path) ? JSON.parse(readFileSync(path, "utf8")) : null;
const baseProvenance = readJson(baseProvenancePath) ?? {};
const wetProvenance = readJson(wetProvenancePath);
const provenance = {
  ...baseProvenance,
  title: "Special Sign",
  base: base.replace(`${REPO}/`, ""),
  wetReturn: wet.replace(`${REPO}/`, ""),
  baseProvenance: baseProvenancePath.replace(`${REPO}/`, ""),
  wetProvenance: wetProvenancePath.replace(`${REPO}/`, ""),
  wetSpatialEngine: wetProvenance?.spatialEffectors ?? null,
  mix: {
    topology: "accepted master plus listener-relative side-only parallel wet return",
    spatialWet: 0.68,
    baselineReturn: baseline,
    superSpinReturn: spin,
    superSpinReleaseSeconds: [spinStart, spinEnd],
    rampSeconds: ramp,
    filtersHz: { highpass: 75, lowpass: 15000 },
    monoInvariant: "wet return is L=-R and cancels in the mono fold before final linear gain",
    limiter: { limit: 0.87, attackMs: 8, releaseMs: 150, autoLevel: false },
    measuredPremixLufs: measuredLufs,
    measuredPremixTruePeakDb: measuredTruePeak,
    targetLufs,
    maxTruePeakDb: maxTruePeak,
    linearGainDb: gainDb,
  },
};
const provenancePath = out.replace(/\.[^.]+$/, ".provenance.json");
writeFileSync(provenancePath, JSON.stringify(provenance, null, 2) + "\n");
console.log(`✓ ${out}`);
console.log(`✓ ${mp3}`);
console.log(`✓ ${provenancePath}`);
console.log(`  ${measuredLufs.toFixed(2)} LUFS / ${measuredTruePeak.toFixed(2)} dBTP premix · ${gainDb.toFixed(2)} dB linear trim`);
