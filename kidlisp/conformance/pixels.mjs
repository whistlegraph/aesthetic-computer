#!/usr/bin/env node
import { Worker } from "node:worker_threads";
import { createHash } from "node:crypto";
import { readFile, writeFile, mkdir, readdir } from "node:fs/promises";
import { resolve, join, dirname } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { KidLispExecution } from "../../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";

const here = dirname(fileURLToPath(import.meta.url));
export const hashPixels = bytes => createHash("sha256").update(bytes).digest("hex");
const hashJSON = value => hashPixels(Buffer.from(JSON.stringify(value)));

export function pixelFixture(input) {
  const { source, seed = 0, epochMs = 0, stepMs = 1000 / 60, width = 96, height = 64, frames = 12, events = [], maxSteps = 100000, maxDepth = 128, monitor = false } = input;
  if (typeof source !== "string" || !source.trim() || source.length > 1000000) throw new TypeError("Pixel source must contain 1..1,000,000 characters");
  new KidLispExecution({ seed, epochMs, stepMs, maxSteps, maxDepth }).beginFrame(frames - 1);
  if (![width, height].every(n => Number.isInteger(n) && n >= 1 && n <= 512)) throw new RangeError("Pixel viewport must be 1..512 per axis");
  if (!Number.isInteger(frames) || frames < 1 || frames > 240 || width * height * frames > 8 * 1024 * 1024) throw new RangeError("Pixel replay exceeds frame/output budget");
  if (!Array.isArray(events) || events.length > 10000) throw new RangeError("Invalid pixel input count");
  const inputs = events.map(({ frame, type, x, y, dx = 0, dy = 0 }) => {
    if (!Number.isInteger(frame) || frame < 0 || frame >= frames || !["touch", "draw", "lift"].includes(type) || ![x, y, dx, dy].every(n => Number.isFinite(n) && Math.abs(n) <= 8192)) throw new TypeError("Invalid pixel input event");
    return { frame, type, x, y, dx, dy };
  });
  if (typeof monitor !== "boolean") throw new TypeError("monitor must be boolean");
  return { source, seed, epochMs, stepMs, width, height, frames, events: inputs, maxSteps, maxDepth, monitor };
}

export function replayPixels(input, { timeoutMs = 15000 } = {}) {
  const fixture = pixelFixture(input);
  if (!Number.isInteger(timeoutMs) || timeoutMs < 1 || timeoutMs > 60000) throw new RangeError("Pixel timeout must be 1..60000ms");
  return new Promise((resolveRun, reject) => {
    const worker = new Worker(new URL("./pixel-worker.mjs", import.meta.url), {
      workerData: fixture, execArgv: ["--disable-warning=MODULE_TYPELESS_PACKAGE_JSON"],
      resourceLimits: { maxOldGenerationSizeMb: 128, stackSizeMb: 4 },
    });
    let settled = false;
    const finish = (error, result) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      // Terminate before resolving, including any unexpected background work.
      worker.terminate().then(() => error ? reject(error) : resolveRun(result), reject);
    };
    const timer = setTimeout(() => finish(Object.assign(new Error("Pixel replay timed out"), { code: "PIXEL_TIMEOUT" })), timeoutMs);
    worker.once("message", ({ result, error }) => finish(error ? Object.assign(new Error(error.message), error) : null, result));
    worker.once("error", error => finish(error));
    worker.once("exit", code => { if (!settled) finish(new Error(`Pixel worker exited without a result (${code})`)); });
  });
}

export function comparePixelFrames(before, after, width, height) {
  const size = width * height * 4;
  if (!Number.isSafeInteger(size) || size < 4 || !Number.isInteger(width) || !Number.isInteger(height) || width < 1 || height < 1 || before?.length !== size || after?.length !== size) throw new TypeError("Pixel buffers must match the viewport");
  let changedPixels = 0, changedChannels = 0, first = null;
  for (let i = 0; i < size; i += 4) {
    let changed = false;
    for (let c = 0; c < 4; c++) if (before[i + c] !== after[i + c]) { changed = true; changedChannels++; }
    if (changed) {
      changedPixels++;
      first ||= { x: (i / 4) % width, y: Math.floor(i / 4 / width), before: Array.from(before.slice(i, i + 4)), after: Array.from(after.slice(i, i + 4)) };
    }
  }
  return { equal: changedPixels === 0, changedPixels, changedChannels, first };
}

async function png(path, rgba, width, height) {
  const { default: sharp } = await import("sharp");
  await sharp(Buffer.from(rgba), { raw: { width, height, channels: 4 } }).png().toFile(path);
}

async function loadPNG(path, width, height) {
  const { default: sharp } = await import("sharp");
  const { data, info } = await sharp(path).ensureAlpha().raw().toBuffer({ resolveWithObject: true });
  if (info.width !== width || info.height !== height || info.channels !== 4) throw new Error(`Invalid golden dimensions: ${path}`);
  return data;
}

export async function pixelSuite({ command = "check", fixtures = join(here, "pixels"), golden = join(here, "pixel-golden"), out = "/tmp/kidlisp-pixels" } = {}) {
  if (!["check", "record"].includes(command)) throw new Error("Expected check or record");
  const names = (await readdir(fixtures)).filter(n => /^[a-z0-9-]+\.json$/.test(n)).sort();
  if (!names.length) throw new Error("No pixel fixtures");
  // Missing goldens fail; checking never blesses current output automatically.
  const expected = command === "check" ? JSON.parse(await readFile(join(golden, "manifest.json"), "utf8")) : null;
  if (expected && (expected.version !== 1 || expected.profile !== "pixels-v1" || JSON.stringify(Object.keys(expected.fixtures).sort()) !== JSON.stringify(names))) throw new Error("Golden profile or fixture set differs");
  const manifest = { version: 1, profile: "pixels-v1", engine: process.version, platform: `${process.platform}/${process.arch}`, fixtures: {} };
  const report = { profile: "pixels-v1", passed: true, fixtures: {} };
  await mkdir(out, { recursive: true });
  if (command === "record") await mkdir(golden, { recursive: true });
  for (const name of names) {
    const input = pixelFixture(JSON.parse(await readFile(join(fixtures, name), "utf8")));
    const fixtureHash = hashJSON(input);
    const before = expected?.fixtures[name];
    if (before && (before.fixtureHash !== fixtureHash || before.frames.length !== input.frames)) throw new Error(`Golden fixture changed: ${name}; review before recording`);
    const first = await replayPixels(input);
    const second = await replayPixels(input);
    const rows = [];
    const hashes = [];
    for (const frame of first.frames) {
      const file = `${name.slice(0, -5)}-${String(frame.frame).padStart(3, "0")}.png`;
      const repeat = comparePixelFrames(frame.rgba, second.frames[frame.frame].rgba, input.width, input.height);
      let baseline = null;
      const hash = hashPixels(frame.rgba);
      hashes.push({ frame: frame.frame, timeMs: frame.timeMs, sha256: hash, file });
      if (before) {
        const prior = before.frames[frame.frame];
        if (prior.frame !== frame.frame || prior.timeMs !== frame.timeMs || prior.file !== file) throw new Error(`Invalid golden frame: ${name}/${frame.frame}`);
        const bytes = await loadPNG(join(golden, file), input.width, input.height);
        if (hashPixels(bytes) !== prior.sha256) throw new Error(`Golden image hash mismatch: ${file}`);
        baseline = comparePixelFrames(bytes, frame.rgba, input.width, input.height);
        if (!baseline.equal) {
          await png(join(out, `expected-${file}`), bytes, input.width, input.height);
          const diff = new Uint8ClampedArray(bytes.length);
          for (let i = 0; i < diff.length; i += 4) {
            const changed = bytes.subarray(i, i + 4).some((v, c) => v !== frame.rgba[i + c]);
            diff.set(changed ? [255, 0, 128, 255] : [0, 0, 0, 255], i);
          }
          await png(join(out, `diff-${file}`), diff, input.width, input.height);
        }
      }
      if (!repeat.equal || baseline?.equal === false) report.passed = false;
      await png(join(out, file), frame.rgba, input.width, input.height);
      if (command === "record") {
        if (!repeat.equal) throw new Error(`Nondeterministic pixels: ${file}`);
        await png(join(golden, file), frame.rgba, input.width, input.height);
      }
      rows.push({ frame: frame.frame, sha256: hash, repeat, baseline });
    }
    manifest.fixtures[name] = { fixtureHash, width: input.width, height: input.height, frames: hashes };
    report.fixtures[name] = rows;
  }
  if (command === "record") await writeFile(join(golden, "manifest.json"), JSON.stringify(manifest, null, 2) + "\n");
  await writeFile(join(out, "report.json"), JSON.stringify(report, null, 2) + "\n");
  return report;
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  const [command = "check", ...args] = process.argv.slice(2);
  const options = { command };
  for (let i = 0; i < args.length; i += 2) {
    if (!["--fixtures", "--golden", "--out"].includes(args[i]) || !args[i + 1]) throw new Error("usage: pixels.mjs check|record [--fixtures DIR] [--golden DIR] [--out DIR]");
    options[args[i].slice(2)] = resolve(args[i + 1]);
  }
  const report = await pixelSuite(options);
  console.log(`${report.passed ? "PASS" : "FAIL"}: ${Object.keys(report.fixtures).length} exact pixel fixtures; ${options.out || "/tmp/kidlisp-pixels"}/report.json`);
  process.exitCode = report.passed ? 0 : 1;
}
