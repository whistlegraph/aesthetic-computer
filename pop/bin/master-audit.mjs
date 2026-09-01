#!/usr/bin/env node
// Compare delivered masters and legal reference previews with one decoder.
// Prints TSV: whole-file loudness plus an 8-second translation stress test.
//
//   node pop/bin/master-audit.mjs <audio...>

import { spawnSync } from "node:child_process";
import { basename } from "node:path";

function run(command, args) {
  const result = spawnSync(command, args, {
    encoding: "utf8",
    maxBuffer: 32 << 20,
  });
  if (result.status !== 0) {
    throw new Error(`${command} failed for ${args.at(-1)}\n${result.stderr}`);
  }
  return `${result.stdout || ""}${result.stderr || ""}`;
}

function number(text, pattern) {
  const match = text.match(pattern);
  return match ? Number(match[1]) : null;
}

function fixed(value, places = 1) {
  return value == null || !Number.isFinite(value) ? "" : value.toFixed(places);
}

function loudestWindow(path, windowSeconds) {
  const log = run("ffmpeg", [
    "-hide_banner", "-nostats", "-i", path,
    "-af", "aresample=48000,asetnsamples=n=48000:p=1,astats=metadata=1:reset=1,ametadata=print:key=lavfi.astats.Overall.RMS_level:file=-",
    "-f", "null", "-",
  ]);
  const levels = [...log.matchAll(/RMS_level=(-?[\d.]+)/g)].map((match) => Number(match[1]));
  const energies = levels.map((db) => 10 ** (db / 10));
  const count = Math.min(windowSeconds, energies.length);
  let sum = energies.slice(0, count).reduce((a, b) => a + b, 0);
  let best = sum;
  let start = 0;
  for (let i = count; i < energies.length; i++) {
    sum += energies[i] - energies[i - count];
    if (sum > best) {
      best = sum;
      start = i - count + 1;
    }
  }
  return { start, rms: best > 0 ? 10 * Math.log10(best / count) : null };
}

function segmentMean(path, start, filter) {
  const log = run("ffmpeg", [
    "-hide_banner", "-nostats", "-ss", String(start), "-t", "8", "-i", path,
    "-af", `${filter ? `${filter},` : ""}volumedetect`,
    "-f", "null", "-",
  ]);
  return number(log, /mean_volume:\s*(-?[\d.]+) dB/);
}

function audit(path) {
  const probe = JSON.parse(run("ffprobe", [
    "-v", "error",
    "-show_entries", "format=duration:stream=codec_name,sample_rate,channels",
    "-of", "json",
    path,
  ]));
  const loudnessLog = run("ffmpeg", [
    "-hide_banner", "-nostats", "-i", path,
    "-af", "ebur128=peak=true:framelog=quiet",
    "-f", "null", "-",
  ]);
  const summary = loudnessLog.slice(loudnessLog.lastIndexOf("Integrated loudness"));
  const integrated = number(summary, /I:\s*(-?[\d.]+) LUFS/);
  const range = number(summary, /LRA:\s*(-?[\d.]+) LU/);
  const truePeak = number(summary, /Peak:\s*(-?[\d.]+) dBFS/);
  const volumeLog = run("ffmpeg", [
    "-hide_banner", "-nostats", "-i", path,
    "-af", "volumedetect", "-f", "null", "-",
  ]);
  const mean = number(volumeLog, /mean_volume:\s*(-?[\d.]+) dB/);
  const samplePeak = number(volumeLog, /max_volume:\s*(-?[\d.]+) dB/);
  const loudest8 = loudestWindow(path, 8);
  const loudest30 = loudestWindow(path, 30);
  const clipMean = segmentMean(path, loudest8.start, "");
  const monoMean = segmentMean(path, loudest8.start, "pan=mono|c0=0.5*c0+0.5*c1");
  const phoneMean = segmentMean(path, loudest8.start, "pan=mono|c0=0.5*c0+0.5*c1,highpass=f=180,lowpass=f=8000");
  const subMean = segmentMean(path, loudest8.start, "lowpass=f=100");
  const presenceMean = segmentMean(path, loudest8.start, "highpass=f=1200,lowpass=f=6000");
  const airMean = segmentMean(path, loudest8.start, "highpass=f=6000,lowpass=f=16000");

  return {
    file: basename(path),
    seconds: Number(probe.format.duration),
    codec: probe.streams[0]?.codec_name || "",
    sampleRate: Number(probe.streams[0]?.sample_rate || 0),
    integrated,
    range,
    truePeak,
    samplePeak,
    mean,
    plr: truePeak - integrated,
    spotifyNormalGain: -14 - integrated,
    loudest8Start: loudest8.start,
    loudest8Rms: loudest8.rms,
    monoLoss: monoMean - clipMean,
    phoneLoss: phoneMean - clipMean,
    subRelative: subMean - clipMean,
    presenceRelative: presenceMean - clipMean,
    airRelative: airMean - clipMean,
    loudest30Start: loudest30.start,
    loudest30Rms: loudest30.rms,
  };
}

console.log([
  "file", "seconds", "codec", "sample_rate", "LUFS-I", "LRA", "dBTP",
  "sample_peak", "mean_dBFS", "PLR", "spotify_normal_gain",
  "loudest_8s_start", "loudest_8s_RMS", "mono_loss", "phone_loss",
  "sub_relative", "presence_relative", "air_relative",
  "loudest_30s_start", "loudest_30s_RMS",
].join("\t"));

for (const path of process.argv.slice(2)) {
  const m = audit(path);
  console.log([
    m.file, fixed(m.seconds, 3), m.codec, m.sampleRate,
    fixed(m.integrated), fixed(m.range), fixed(m.truePeak),
    fixed(m.samplePeak), fixed(m.mean), fixed(m.plr), fixed(m.spotifyNormalGain),
    fixed(m.loudest8Start, 0), fixed(m.loudest8Rms), fixed(m.monoLoss),
    fixed(m.phoneLoss), fixed(m.subRelative), fixed(m.presenceRelative),
    fixed(m.airRelative), fixed(m.loudest30Start, 0), fixed(m.loudest30Rms),
  ].join("\t"));
}
