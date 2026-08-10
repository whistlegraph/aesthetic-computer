#!/usr/bin/env node
// Device Portal control surface for the oskiewar native live JavaScript loop.
// Designed for blueberry, where the Xbox vault credentials already live.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, statSync,
  writeFileSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { compilePublishedKidLisp } from "./kidlisp-native.mjs";

const defaultEnv = resolve(homedir(),
  "aesthetic-computer/aesthetic-computer-vault/xbox/device-portal.env");

function parseEnv(path) {
  if (!existsSync(path)) return {};
  const result = {};
  for (const raw of readFileSync(path, "utf8").split(/\r?\n/)) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    const match = line.match(/^(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (!match) continue;
    let value = match[2].trim();
    if ((value.startsWith('"') && value.endsWith('"')) ||
        (value.startsWith("'") && value.endsWith("'"))) value = value.slice(1, -1);
    result[match[1]] = value;
  }
  return result;
}

const fileEnv = parseEnv(process.env.XBOX_DEVICE_PORTAL_ENV || defaultEnv);
const config = { ...fileEnv, ...process.env };
const host = config.XBOX_DEVICE_PORTAL_HOST;
const port = config.XBOX_DEVICE_PORTAL_PORT || "11443";
const username = config.XBOX_DEVICE_PORTAL_USERNAME;
const password = config.XBOX_DEVICE_PORTAL_PASSWORD;
if (!host || !username || !password) {
  console.error(`Missing Device Portal configuration. Expected ${defaultEnv}`);
  process.exit(2);
}

const base = `https://${host}:${port}`;
const auth = `${username}:${password}`;
const autoAuth = `auto-${username}:${password}`;

function curl(args, { json = false, input } = {}) {
  const result = spawnSync("curl", ["-k", "-sS", ...args], {
    encoding: "utf8", maxBuffer: 16 * 1024 * 1024,
    ...(input === undefined ? {} : { input }),
  });
  if (result.status !== 0) throw new Error(result.stderr.trim() || `curl exited ${result.status}`);
  return json ? JSON.parse(result.stdout || "null") : result.stdout;
}

function packages() {
  return curl(["-u", auth, `${base}/api/app/packagemanager/packages`], { json: true })
    .InstalledPackages.filter((item) => item.PackageFamilyName === "AestheticComputer.NativeBios")
    .sort((a, b) => b.Version.Revision - a.Version.Revision);
}

function installed() {
  const item = packages()[0];
  if (!item) throw new Error("oskiewar is not installed");
  return item;
}

const sleep = (milliseconds) =>
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, milliseconds);

function prune() {
  const stale = curl(["-u", auth, `${base}/api/app/packagemanager/packages`], { json: true })
    .InstalledPackages.filter((item) => item.PackageFamilyName !== "AestheticComputer.NativeBios")
    .filter((item) => item.CanUninstall !== false)
    .filter((item) => item.Publisher === "CN=AestheticComputerDev" ||
      item.PackageFamilyName.startsWith("AestheticComputer."));
  for (const item of stale) {
    const query = new URLSearchParams({ package: item.PackageFullName });
    curl(["-u", autoAuth, "-X", "DELETE",
      `${base}/api/app/packagemanager/package?${query}`]);
    console.log(`removed stale AC dev package ${item.PackageFullName}`);
  }
  if (stale.length === 0) console.log("no stale AC dev packages installed");
  return stale.length;
}

function install(packagePath, dependencyPaths = []) {
  if (!packagePath) throw new Error("usage: xbox-live install <NativeBios.msix> [dependency.appx ...]");
  const main = resolve(packagePath);
  const dependencies = dependencyPaths.map((item) => resolve(item));
  for (const file of [main, ...dependencies])
    if (!existsSync(file)) throw new Error(`package file not found: ${file}`);
  const revision = Number.parseInt(basename(main).match(/_1\.0\.0\.(\d+)_/)?.[1] || "0", 10);
  const form = [main, ...dependencies].flatMap((file) => ["-F", `file=@${file}`]);
  const query = new URLSearchParams({ package: basename(main) });
  curl(["-u", autoAuth, "-X", "POST", ...form,
    `${base}/api/app/packagemanager/package?${query}`]);
  let ready = false;
  for (let attempt = 0; attempt < 60; attempt++) {
    sleep(1000);
    const current = packages()[0];
    if (current && current.Version.Revision >= revision) { ready = true; break; }
  }
  if (!ready) throw new Error(`timed out waiting for Native BIOS revision ${revision}`);
  console.log(`installed ${installed().PackageFullName}`);
  prune();
}

function appFileUrl(item, filename) {
  const query = new URLSearchParams({ knownfolderid: "LocalAppData",
    packagefullname: item.PackageFullName, path: "\\LocalState",
    ...(filename ? { filename } : {}) });
  return `${base}/api/filesystem/apps/file?${query}`;
}

function status() {
  const item = installed();
  const processes = JSON.stringify(curl(["-u", auth,
    `${base}/api/resourcemanager/processes`], { json: true }));
  console.log(JSON.stringify({ host, package: item.PackageFullName,
    relativeId: item.PackageRelativeId, version: item.Version,
    running: processes.includes(item.PackageFullName) && processes.includes("NativeBios.exe") }, null, 2));
}

function launch() {
  const item = installed();
  const query = new URLSearchParams({
    appid: Buffer.from(item.PackageRelativeId).toString("base64"),
    package: Buffer.from(item.PackageFullName).toString("base64"),
  });
  curl(["-u", autoAuth, "-X", "POST", "-H", "Content-Length: 0",
    `${base}/api/taskmanager/app?${query}`]);
  console.log(`launched ${item.PackageFullName}`);
}

function publish(sourcePath) {
  if (!sourcePath) throw new Error("usage: xbox-live publish <piece.js>");
  const absolute = resolve(sourcePath);
  if (basename(absolute) === "oskiewar.js" &&
      process.env.OSKIEWAR_UNIFIED_DEPLOY !== "1")
    throw new Error("canonical Oskiewar deploys must use npm run oskiewar:deploy");
  if (!existsSync(absolute)) throw new Error(`piece not found: ${absolute}`);
  let source = readFileSync(absolute, "utf8");
  if (source.startsWith("// @bundle-qr")) {
    const qrPath = resolve(dirname(fileURLToPath(import.meta.url)),
      "../../system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs");
    const qrSource = readFileSync(qrPath, "utf8").replace(
      /\nexport\s*\{[\s\S]*?\};\s*$/, "\n");
    source = qrSource + "\n" + source;
    return publishSource(source, absolute + " + qr");
  }
  const item = installed();
  curl(["-u", autoAuth, "-X", "POST", "-F",
    `file=@${absolute};filename=live-piece.js`, appFileUrl(item)]);
  console.log(JSON.stringify({ published: absolute, package: item.PackageFullName }));
}

function publishSource(source, label) {
  if (typeof source !== "string" || source.length === 0 ||
      Buffer.byteLength(source, "utf8") > 2 * 1024 * 1024)
    throw new Error("generated piece source is empty or exceeds 2 MiB");
  const item = installed();
  curl(["-u", autoAuth, "-X", "POST", "-F",
    "file=@-;filename=live-piece.js;type=application/javascript", appFileUrl(item)], { input: source });
  console.log(JSON.stringify({ published: label, bytes: Buffer.byteLength(source, "utf8"),
    package: item.PackageFullName }));
}

async function deployKidLisp(code) {
  if (!code) throw new Error("usage: xbox-live deploy-kidlisp <$code>");
  const compiled = await compilePublishedKidLisp(code);
  publishSource(compiled.generated, `$${compiled.code} by ${compiled.handle}`);
  console.log(JSON.stringify({ kidlisp: `$${compiled.code}`, handle: compiled.handle,
    forms: compiled.formCount, paintings: compiled.paintings }));
  launch();
  sleep(1200);
  logs("30");
}

function logs(tail = "80") {
  const count = Math.max(1, Math.min(5000, Number.parseInt(tail, 10) || 80));
  const content = curl(["-u", autoAuth, "-H", "Range: bytes=-1048576",
    appFileUrl(installed(), "ac-native-bios.log")]);
  console.log(content.trimEnd().split(/\r?\n/).slice(-count).join("\n"));
}

function frameDump(outputPath = "") {
  const content = curl(["-u", autoAuth, "-H", "Range: bytes=-8388608",
    appFileUrl(installed(), "ac-native-bios.log")]);
  const marker = "AC_NATIVE_JS FIGHT_TRACE ";
  const chunks = [];
  for (const line of content.split(/\r?\n/)) {
    const offset = line.indexOf(marker);
    if (offset < 0) continue;
    try { chunks.push(JSON.parse(line.slice(offset + marker.length))); } catch {}
  }
  if (!chunks.length) throw new Error("no oskiewar frame telemetry in the native log");
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  const target = resolve(outputPath ||
    `tmp/xbox-captures/oskiewar-frames-${stamp}.json`);
  mkdirSync(dirname(target), { recursive: true });
  const latestRound = chunks.at(-1).round;
  const frames = chunks.filter((item) => item.round === latestRound)
    .flatMap((item) => item.frames)
    .filter((frame, index, all) => index === 0 || frame[0] !== all[index - 1][0]);
  const schema = chunks.at(-1).schema;
  const index = Object.fromEntries(schema.map((name, offset) => [name, offset]));
  const fields = ["cameraX", "cameraY", "cameraWidth", "dollX", "dollY",
    "dollZ", "dollWidth", "roll"];
  const samples = Object.fromEntries(fields.map((field) => [field, []]));
  const spikes = [];
  for (let frame = 1; frame < frames.length; frame++) {
    const elapsed = (frames[frame][index.us] - frames[frame - 1][index.us]) / 1000000;
    if (!(elapsed > 0) || elapsed > .1) continue;
    for (const field of fields) {
      const delta = frames[frame][index[field]] - frames[frame - 1][index[field]];
      const speed = delta / elapsed;
      samples[field].push({ frame, delta, speed });
    }
  }
  const metrics = {};
  for (const field of fields) {
    const values = frames.map((frame) => frame[index[field]]);
    const steps = samples[field].map((sample) => Math.abs(sample.delta))
      .sort((a, b) => a - b);
    const p95 = steps[Math.floor(Math.max(0, steps.length - 1) * .95)] || 0;
    const threshold = Math.max(field === "roll" ? .0008 : 1, p95 * 3);
    let reversals = 0;
    for (let sample = 1; sample < samples[field].length; sample++) {
      const previous = samples[field][sample - 1];
      const current = samples[field][sample];
      if (Math.sign(previous.delta) && Math.sign(current.delta) &&
          Math.sign(previous.delta) !== Math.sign(current.delta)) reversals++;
      if (Math.abs(current.delta) > threshold) spikes.push({
        us: frames[current.frame][index.us], field,
        delta: Math.round(current.delta * 1000) / 1000,
        speed: Math.round(current.speed * 100) / 100,
      });
    }
    metrics[field] = { min: Math.min(...values), max: Math.max(...values),
      maxStep: steps.at(-1) || 0, p95Step: p95, reversals };
  }
  spikes.sort((a, b) => Math.abs(b.speed) - Math.abs(a.speed));
  const trace = { format: "ac.oskiewar.frame-dump", version: 2,
    schema, rounds: [latestRound], frames,
    analysis: { durationSeconds: frames.length > 1
      ? (frames.at(-1)[index.us] - frames[0][index.us]) / 1000000 : 0,
      metrics, spikes: spikes.slice(0, 40) } };
  writeFileSync(target, JSON.stringify(trace));
  console.log(JSON.stringify({ frames: target, count: trace.frames.length,
    rounds: trace.rounds, analysis: trace.analysis }));
}

function captureScreenshot(target) {
  mkdirSync(dirname(target), { recursive: true });
  const startedAt = Date.now();
  const result = spawnSync("curl", ["-k", "-sS", "-u", auth,
    "-o", target, "-w", "%{http_code}", `${base}/ext/screenshot`],
  { encoding: "utf8", maxBuffer: 1024 * 1024 });
  const status = String(result.stdout || "").trim();
  if (result.status !== 0 || status !== "200")
    throw new Error(result.stderr.trim() ||
      `screenshot failed with HTTP ${status || "unknown"}`);
  return { at: new Date().toISOString(), elapsedMs: Date.now() - startedAt,
    bytes: statSync(target).size };
}

function screenshot(outputPath = "") {
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  const target = resolve(outputPath ||
    `tmp/xbox-captures/oskiewar-${stamp}.png`);
  const result = captureScreenshot(target);
  console.log(JSON.stringify({ screenshot: target, bytes: result.bytes,
    elapsedMs: result.elapsedMs }));
}

function video(durationArg = "10", outputPath = "") {
  const duration = Math.max(2, Math.min(120, Number(durationArg) || 10));
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  const target = resolve(outputPath ||
    `tmp/xbox-captures/oskiewar-${stamp}.mp4`);
  const framesDirectory = mkdtempSync(join(tmpdir(), "oskiewar-video-"));
  const frames = [];
  try {
    const deadline = Date.now() + duration * 1000;
    let number = 0;
    while (Date.now() < deadline) {
      const path = join(framesDirectory, `${String(number).padStart(5, "0")}.png`);
      frames.push(captureScreenshot(path));
      number++;
    }
    if (frames.length < 2) throw new Error("not enough Xbox screenshots for video");
    mkdirSync(dirname(target), { recursive: true });
    const elapsedSeconds = Math.max(.001,
      (new Date(frames.at(-1).at) - new Date(frames[0].at)) / 1000);
    const frameRate = Math.max(.1, (frames.length - 1) / elapsedSeconds);
    const encoded = spawnSync("ffmpeg", ["-y", "-loglevel", "error",
      "-framerate", String(frameRate), "-i", join(framesDirectory, "%05d.png"),
      "-c:v", "libx264", "-preset", "veryfast", "-pix_fmt", "yuv420p", target],
    { encoding: "utf8", maxBuffer: 16 * 1024 * 1024 });
    if (encoded.status !== 0)
      throw new Error(encoded.stderr.trim() || "ffmpeg video encoding failed");
    const metadata = `${target}.json`;
    writeFileSync(metadata, JSON.stringify({ format: "ac.oskiewar.portal-video",
      version: 1, source: "xbox-device-portal-screenshots", frameRate,
      durationSeconds: elapsedSeconds, frames }, null, 2));
    console.log(JSON.stringify({ video: target, metadata, frameRate,
      count: frames.length, bytes: statSync(target).size }));
  } finally {
    rmSync(framesDirectory, { recursive: true, force: true });
  }
}

async function main() {
  const [command = "status", argument, ...rest] = process.argv.slice(2);
  if (command === "status") status();
  else if (command === "install") install(argument, rest);
  else if (command === "prune") prune();
  else if (command === "launch") launch();
  else if (command === "publish") publish(argument);
  else if (command === "logs") logs(argument);
  else if (command === "frames") frameDump(argument);
  else if (command === "screenshot") screenshot(argument);
  else if (command === "video") video(argument, rest[0]);
  else if (command === "deploy") { publish(argument); launch(); logs("20"); }
  else if (command === "deploy-kidlisp") await deployKidLisp(argument);
  else throw new Error("commands: status | install <msix> [deps...] | prune | launch | publish <piece.js> | logs [lines] | frames [output.json] | screenshot [output.png] | video [seconds] [output.mp4] | deploy <piece.js> | deploy-kidlisp <$code>");
}

try { await main(); } catch (error) { console.error(error.message); process.exit(1); }
