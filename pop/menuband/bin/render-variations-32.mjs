#!/usr/bin/env node

import { spawn, spawnSync } from "node:child_process";
import { existsSync, readFileSync, readdirSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const DEFAULT_MANIFEST = resolve(ROOT, "pop/menuband/variations/variations-32.json");
const SCORE_RENDERER = resolve(ROOT, "pop/menuband/bin/render-variation-score.swift");
const VIDEO_RENDERER = resolve(ROOT, "pop/menuband/bin/sim-piano-variation.mjs");
const SWIFT = resolve(homedir(), ".local/bin/swift");
const FFPROBE = resolve(homedir(), ".local/bin/ffprobe");
const OUTPUT_ROOT = resolve(ROOT, "pop/menuband/out/variations");

function usage(exitCode = 0) {
  const output = exitCode === 0 ? process.stdout : process.stderr;
  output.write(`Usage: node pop/menuband/bin/render-variations-32.mjs [options]\n\n`);
  output.write(`Options:\n`);
  output.write(`  --manifest FILE    Variation manifest (default: variations-32.json)\n`);
  output.write(`  --start N          First zero-based manifest index, inclusive (default: 0)\n`);
  output.write(`  --end N            Last zero-based manifest index, inclusive (default: last)\n`);
  output.write(`  --ids A,B          Render only these IDs; repeatable and intersected with range\n`);
  output.write(`  --jobs N           Concurrent variations (default: 1)\n`);
  output.write(`  --dry-run          Print the selected work without invoking renderers\n`);
  output.write(`  --skip-existing    Skip MP4s whose probed duration matches the manifest (default)\n`);
  output.write(`  --no-skip-existing Re-render variations even when their MP4 duration matches\n`);
  output.write(`  -h, --help         Show this help\n`);
  process.exit(exitCode);
}

function valueAfter(args, index, flag) {
  const value = args[index + 1];
  if (value === undefined || value.startsWith("--")) {
    throw new Error(`${flag} requires a value`);
  }
  return value;
}

function integer(value, flag, minimum = 0) {
  if (!/^\d+$/.test(value)) throw new Error(`${flag} must be an integer`);
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < minimum) {
    throw new Error(`${flag} must be at least ${minimum}`);
  }
  return parsed;
}

function parseArguments(argv) {
  const options = {
    manifest: DEFAULT_MANIFEST,
    start: 0,
    end: undefined,
    ids: [],
    jobs: 1,
    dryRun: false,
    skipExisting: true,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    switch (arg) {
      case "--manifest":
        options.manifest = resolve(valueAfter(argv, i, arg));
        i += 1;
        break;
      case "--start":
        options.start = integer(valueAfter(argv, i, arg), arg);
        i += 1;
        break;
      case "--end":
        options.end = integer(valueAfter(argv, i, arg), arg);
        i += 1;
        break;
      case "--ids":
        options.ids.push(...valueAfter(argv, i, arg).split(",").map((id) => id.trim()).filter(Boolean));
        i += 1;
        break;
      case "--jobs":
        options.jobs = integer(valueAfter(argv, i, arg), arg, 1);
        i += 1;
        break;
      case "--dry-run":
        options.dryRun = true;
        break;
      case "--skip-existing":
        options.skipExisting = true;
        break;
      case "--no-skip-existing":
        options.skipExisting = false;
        break;
      case "-h":
      case "--help":
        usage(0);
        break;
      default:
        throw new Error(`unknown option: ${arg}`);
    }
  }
  return options;
}

function loadVariations(manifestPath) {
  if (!existsSync(manifestPath)) throw new Error(`missing manifest: ${manifestPath}`);
  const root = JSON.parse(readFileSync(manifestPath, "utf8"));
  const variations = Array.isArray(root) ? root : root?.variations;
  if (!Array.isArray(variations) || variations.length === 0) {
    throw new Error("manifest must be an array or an object with a nonempty variations array");
  }
  return variations.map((variation, index) => {
    const id = String(variation?.id ?? variation?.slug ?? "").trim();
    if (!id) throw new Error(`manifest variation ${index} has no id`);
    const durationSec = Number(variation?.durationSec ?? 32);
    return { index, id, name: String(variation?.name ?? id), durationSec };
  });
}

function cleanStem(value) {
  return value.toLowerCase().replace(/[^\p{L}\p{N}]+/gu, "-").replace(/^-|-$/g, "");
}

function completeMP4(path, expectedDuration) {
  try {
    if (!path.endsWith(".mp4") || !statSync(path).isFile() || statSync(path).size <= 0) return false;
    const probe = spawnSync(FFPROBE, [
      "-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", path,
    ], { encoding: "utf8" });
    const actualDuration = Number(probe.stdout.trim());
    const tolerance = Math.max(0.08, expectedDuration / 600);
    return probe.status === 0 && Number.isFinite(actualDuration)
      && Math.abs(actualDuration - expectedDuration) <= tolerance;
  } catch {
    return false;
  }
}

function existingVideo(item) {
  const { id, durationSec } = item;
  const stem = cleanStem(id);
  const variationDir = resolve(OUTPUT_ROOT, stem);
  if (existsSync(variationDir)) {
    for (const entry of readdirSync(variationDir, { withFileTypes: true })) {
      if (entry.isFile()) {
        const path = resolve(variationDir, entry.name);
        if (completeMP4(path, durationSec)) return path;
      }
    }
  }

  const legacyCandidates = [
    resolve(ROOT, `pop/menuband/out/menuband-${stem}-reel.mp4`),
    resolve(ROOT, `pop/menuband/out/${stem}-reel.mp4`),
  ];
  return legacyCandidates.find((path) => completeMP4(path, durationSec));
}

function selectVariations(variations, options) {
  const last = options.end ?? variations.length - 1;
  if (options.start >= variations.length) {
    throw new Error(`--start ${options.start} exceeds last manifest index ${variations.length - 1}`);
  }
  if (last < options.start) throw new Error("--end must be greater than or equal to --start");
  if (last >= variations.length) {
    throw new Error(`--end ${last} exceeds last manifest index ${variations.length - 1}`);
  }

  const wanted = new Set(options.ids);
  if (wanted.size > 0) {
    const known = new Set(variations.map(({ id }) => id));
    const missing = [...wanted].filter((id) => !known.has(id));
    if (missing.length) throw new Error(`unknown variation ID${missing.length === 1 ? "" : "s"}: ${missing.join(", ")}`);
  }

  return variations.filter(({ index, id }) => (
    index >= options.start && index <= last && (wanted.size === 0 || wanted.has(id))
  ));
}

const activeChildren = new Set();
let interrupted = false;

function prefixLines(stream, prefix, target) {
  let pending = "";
  stream.setEncoding("utf8");
  stream.on("data", (chunk) => {
    const lines = (pending + chunk).split(/\r?\n/);
    pending = lines.pop() ?? "";
    for (const line of lines) target.write(`${prefix}${line}\n`);
  });
  stream.on("end", () => {
    if (pending) target.write(`${prefix}${pending}\n`);
  });
}

function runCommand(command, args, label) {
  return new Promise((resolvePromise, rejectPromise) => {
    const child = spawn(command, args, { cwd: ROOT, stdio: ["ignore", "pipe", "pipe"] });
    activeChildren.add(child);
    prefixLines(child.stdout, `[${label}] `, process.stdout);
    prefixLines(child.stderr, `[${label}] `, process.stderr);
    child.once("error", (error) => {
      activeChildren.delete(child);
      rejectPromise(error);
    });
    child.once("exit", (code, signal) => {
      activeChildren.delete(child);
      if (code === 0) resolvePromise();
      else rejectPromise(new Error(signal ? `terminated by ${signal}` : `exited ${code}`));
    });
  });
}

function commandPlan(item, manifestPath) {
  return [
    {
      phase: "score",
      command: SWIFT,
      args: [SCORE_RENDERER, "--manifest", manifestPath, "--index", String(item.index)],
    },
    {
      phase: "video",
      command: process.execPath,
      args: [VIDEO_RENDERER, "--manifest", manifestPath, "--index", String(item.index)],
    },
  ];
}

function displayCommand({ command, args }) {
  return [command, ...args].map((part) => JSON.stringify(part)).join(" ");
}

async function renderOne(item, manifestPath) {
  const label = `${String(item.index).padStart(2, "0")}:${item.id}`;
  const started = Date.now();
  console.log(`[${label}] start ${item.name}`);
  for (const step of commandPlan(item, manifestPath)) {
    console.log(`[${label}] ${step.phase}`);
    await runCommand(step.command, step.args, `${label}:${step.phase}`);
  }
  console.log(`[${label}] done ${((Date.now() - started) / 1000).toFixed(1)}s`);
}

async function runPool(items, jobs, worker) {
  let cursor = 0;
  const results = [];
  async function lane() {
    while (!interrupted && cursor < items.length) {
      const item = items[cursor];
      cursor += 1;
      try {
        await worker(item);
        results.push({ item, status: "succeeded" });
      } catch (error) {
        console.error(`[${String(item.index).padStart(2, "0")}:${item.id}] failed: ${error.message}`);
        results.push({ item, status: "failed", error });
      }
    }
  }
  await Promise.all(Array.from({ length: Math.min(jobs, items.length) }, lane));
  return results;
}

function stopChildren(signal) {
  interrupted = true;
  for (const child of activeChildren) child.kill(signal);
}
process.once("SIGINT", () => stopChildren("SIGINT"));
process.once("SIGTERM", () => stopChildren("SIGTERM"));

async function main() {
  const options = parseArguments(process.argv.slice(2));
  const variations = loadVariations(options.manifest);
  const selected = selectVariations(variations, options);
  const skipped = [];
  const pending = [];

  for (const item of selected) {
    const output = options.skipExisting ? existingVideo(item) : undefined;
    if (output) skipped.push({ item, output });
    else pending.push(item);
  }

  console.log(`manifest ${options.manifest}`);
  console.log(`selected ${selected.length}; pending ${pending.length}; existing ${skipped.length}; jobs ${options.jobs}`);
  for (const { item, output } of skipped) {
    console.log(`[${String(item.index).padStart(2, "0")}:${item.id}] skip existing ${output}`);
  }

  if (options.dryRun) {
    for (const item of pending) {
      for (const step of commandPlan(item, options.manifest)) {
        console.log(`[dry-run ${String(item.index).padStart(2, "0")}:${item.id}:${step.phase}] ${displayCommand(step)}`);
      }
    }
    console.log(`summary selected=${selected.length} planned=${pending.length} skipped=${skipped.length} failed=0`);
    return;
  }

  if (!existsSync(SWIFT)) throw new Error(`missing Swift guard: ${SWIFT}`);
  if (!existsSync(SCORE_RENDERER)) throw new Error(`missing score renderer: ${SCORE_RENDERER}`);
  if (!existsSync(VIDEO_RENDERER)) throw new Error(`missing video renderer: ${VIDEO_RENDERER}`);

  const results = await runPool(pending, options.jobs, (item) => renderOne(item, options.manifest));
  const succeeded = results.filter(({ status }) => status === "succeeded").length;
  const failed = results.filter(({ status }) => status === "failed").length;
  console.log(`summary selected=${selected.length} succeeded=${succeeded} skipped=${skipped.length} failed=${failed}`);
  if (interrupted) process.exitCode = 130;
  else if (failed > 0) process.exitCode = 1;
}

main().catch((error) => {
  console.error(`render-variations-32: ${error.message}`);
  process.exitCode = 1;
});
