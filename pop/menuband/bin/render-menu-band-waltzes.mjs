#!/usr/bin/env node

import { spawn, spawnSync } from "node:child_process";
import { existsSync, readFileSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const DEFAULT_MANIFEST = resolve(ROOT, "pop/menuband/waltzes/menu-band-waltzes.json");
const AUDIO_RENDERER = resolve(ROOT, "pop/menuband/bin/render-menu-band-waltz.swift");
const VIDEO_RENDERER = resolve(ROOT, "pop/menuband/bin/sim-piano-variation.mjs");
const SWIFT = existsSync(resolve(homedir(), ".local/bin/swift"))
  ? resolve(homedir(), ".local/bin/swift") : "/usr/bin/swift";
const FFMPEG = existsSync(resolve(homedir(), ".local/bin/ffmpeg"))
  ? resolve(homedir(), ".local/bin/ffmpeg") : "ffmpeg";
const FFPROBE = existsSync(resolve(homedir(), ".local/bin/ffprobe"))
  ? resolve(homedir(), ".local/bin/ffprobe") : "ffprobe";

function usage(exitCode = 0) {
  const out = exitCode ? process.stderr : process.stdout;
  out.write(`usage: node pop/menuband/bin/render-menu-band-waltzes.mjs [options]\n\n`);
  out.write(`  --manifest FILE       collection manifest\n`);
  out.write(`  --ids A,B             render only named IDs\n`);
  out.write(`  --jobs N              concurrent waltzes (default: 1)\n`);
  out.write(`  --audio-only          stop after WAV + score\n`);
  out.write(`  --video-only          reuse existing WAV + score\n`);
  out.write(`  --no-skip-existing    replace complete MP4 previews\n`);
  out.write(`  --dry-run             print work without rendering\n`);
  process.exit(exitCode);
}

function parse(argv) {
  const options = {
    manifest: DEFAULT_MANIFEST,
    ids: [],
    jobs: 1,
    audioOnly: false,
    videoOnly: false,
    skipExisting: true,
    dryRun: false,
  };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    const value = () => {
      const found = argv[++index];
      if (!found || found.startsWith("--")) throw new Error(`${arg} needs a value`);
      return found;
    };
    if (arg === "--manifest") options.manifest = resolve(value());
    else if (arg === "--ids") options.ids.push(...value().split(",").map((id) => id.trim()).filter(Boolean));
    else if (arg === "--jobs") options.jobs = Number(value());
    else if (arg === "--audio-only") options.audioOnly = true;
    else if (arg === "--video-only") options.videoOnly = true;
    else if (arg === "--no-skip-existing") options.skipExisting = false;
    else if (arg === "--dry-run") options.dryRun = true;
    else if (arg === "--help" || arg === "-h") usage();
    else throw new Error(`unknown option: ${arg}`);
  }
  if (!Number.isInteger(options.jobs) || options.jobs < 1) throw new Error("--jobs must be a positive integer");
  if (options.audioOnly && options.videoOnly) throw new Error("choose audio-only or video-only, not both");
  return options;
}

function loadCollection(path) {
  if (!existsSync(path)) throw new Error(`manifest is missing: ${path}`);
  const root = JSON.parse(readFileSync(path, "utf8"));
  if (!Array.isArray(root.variations) || root.variations.length !== 3) {
    throw new Error("Menu Band Waltzes must contain exactly three pieces");
  }
  const defaults = root.defaults || {};
  return root.variations.map((entry) => ({ ...defaults, ...entry, visual: { ...(defaults.visual || {}), ...(entry.visual || {}) } }));
}

function complete(path, durationSec) {
  if (!existsSync(path) || statSync(path).size < 100_000) return false;
  const probe = spawnSync(FFPROBE, [
    "-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", path,
  ], { encoding: "utf8" });
  const actual = Number(probe.stdout.trim());
  return probe.status === 0 && Number.isFinite(actual) && Math.abs(actual - durationSec) < 0.1;
}

function run(command, args, label) {
  return new Promise((resolvePromise, rejectPromise) => {
    const child = spawn(command, args, { cwd: ROOT, stdio: ["ignore", "pipe", "pipe"] });
    let stdout = "";
    let stderr = "";
    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => { stdout += chunk; process.stdout.write(`[${label}] ${chunk}`); });
    child.stderr.on("data", (chunk) => { stderr += chunk; process.stderr.write(`[${label}] ${chunk}`); });
    child.once("error", rejectPromise);
    child.once("exit", (code, signal) => {
      if (code === 0) resolvePromise({ stdout, stderr });
      else rejectPromise(new Error(`${label} ${signal ? `terminated by ${signal}` : `exited ${code}`}`));
    });
  });
}

async function renderOne(entry, options) {
  const baseDir = resolve(dirname(options.manifest), JSON.parse(readFileSync(options.manifest, "utf8")).baseDir || ".");
  const outPath = resolve(baseDir, entry.outPath);
  const outDir = dirname(outPath);
  const artifacts = [];
  if (!options.videoOnly) {
    await run(SWIFT, [AUDIO_RENDERER, "--manifest", options.manifest, "--id", entry.id, "--out-dir", outDir], `${entry.id}:audio`);
    const rawAudio = resolve(outDir, `${entry.id}.raw.wav`);
    const masteredAudio = resolve(baseDir, entry.audioPath);
    await run(FFMPEG, [
      "-hide_banner", "-y", "-loglevel", "error", "-i", rawAudio,
      "-af", "highpass=f=28,loudnorm=I=-18:TP=-1.5:LRA=8",
      "-ar", "48000", "-c:a", "pcm_s24le", masteredAudio,
    ], `${entry.id}:master`);
    artifacts.push(masteredAudio);
  }
  if (!options.audioOnly) {
    await run(process.execPath, [VIDEO_RENDERER, "--manifest", options.manifest, "--id", entry.id], `${entry.id}:video`);
    artifacts.push(outPath);
  }
  return artifacts;
}

async function pool(items, jobs, worker) {
  const results = [];
  let cursor = 0;
  async function lane() {
    while (cursor < items.length) {
      const item = items[cursor++];
      results.push(await worker(item));
    }
  }
  await Promise.all(Array.from({ length: Math.min(jobs, items.length) }, lane));
  return results;
}

async function main() {
  const options = parse(process.argv.slice(2));
  let pieces = loadCollection(options.manifest);
  if (options.ids.length) {
    const wanted = new Set(options.ids);
    const unknown = [...wanted].filter((id) => !pieces.some((piece) => piece.id === id));
    if (unknown.length) throw new Error(`unknown waltz: ${unknown.join(", ")}`);
    pieces = pieces.filter((piece) => wanted.has(piece.id));
  }
  const baseDir = resolve(dirname(options.manifest), JSON.parse(readFileSync(options.manifest, "utf8")).baseDir || ".");
  const pending = pieces.filter((piece) => {
    const out = resolve(baseDir, piece.outPath);
    if (!options.skipExisting || options.audioOnly || options.videoOnly) return true;
    if (complete(out, piece.durationSec)) {
      console.log(`[${piece.id}] skip ${out}`);
      return false;
    }
    return true;
  });
  console.log(`Menu Band Waltzes · ${pending.length}/${pieces.length} pending · ${options.jobs} job${options.jobs === 1 ? "" : "s"}`);
  if (options.dryRun) {
    for (const piece of pending) console.log(`${piece.id} · ${piece.name} · ${piece.bpm} BPM · ${piece.durationSec}s`);
    return;
  }
  const rendered = (await pool(pending, options.jobs, (piece) => renderOne(piece, options))).flat();
  for (const path of rendered) console.log(`✓ ${path}`);
}

main().catch((error) => {
  console.error(`render-menu-band-waltzes: ${error.message}`);
  process.exit(1);
});
