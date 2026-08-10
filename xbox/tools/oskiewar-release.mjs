#!/usr/bin/env node
// One release receipt for Oskiewar's web, iOS-web, and Xbox live surfaces.

import { createHash } from "node:crypto";
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const sourcePath = resolve(root, "xbox/live/oskiewar.js");
const receiptPath = resolve(root, ".git/oskiewar-parity.json");
const channels = ["web", "ios", "xbox"];

export const sha256 = (value) => createHash("sha256").update(value).digest("hex");

export function classifySeverity(paths) {
  const iosNative = paths.some((path) => path.startsWith("apple/oskiewar/") &&
    !path.endsWith("oskiewar.js"));
  const xboxNative = paths.some((path) => path.startsWith("xbox/native-bios/") ||
    path.startsWith("xbox/package/"));
  if (iosNative && xboxNative) return "multi-native";
  if (iosNative) return "ios-native";
  if (xboxNative) return "xbox-native";
  return "live";
}

export function newRelease(hash, commit, severity, previous = null) {
  const at = new Date().toISOString();
  return {
    format: "computer.aesthetic.oskiewar-parity", version: 1,
    desired: { hash, commit, severity, createdAt: at },
    channels: Object.fromEntries(channels.map((name) => [name, {
      status: previous?.channels?.[name]?.hash === hash ? "current" : "pending",
      hash: previous?.channels?.[name]?.hash || null, updatedAt: at,
    }])),
  };
}

function run(command, args, options = {}) {
  const result = spawnSync(command, args, { cwd: root, encoding: "utf8",
    stdio: options.capture ? "pipe" : "inherit",
    env: { ...process.env, OSKIEWAR_UNIFIED_DEPLOY: "1", ...options.env } });
  if (result.status !== 0) throw new Error((result.stderr || "").trim() ||
    `${command} exited ${result.status}`);
  return result.stdout || "";
}

function git(...args) { return run("git", args, { capture: true }).trim(); }
function readReceipt() {
  if (!existsSync(receiptPath)) return null;
  try { return JSON.parse(readFileSync(receiptPath, "utf8")); } catch { return null; }
}
function save(receipt) {
  mkdirSync(dirname(receiptPath), { recursive: true });
  const temporary = `${receiptPath}.${process.pid}`;
  writeFileSync(temporary, JSON.stringify(receipt, null, 2) + "\n");
  renameSync(temporary, receiptPath);
}
function mark(receipt, name, status, detail = "") {
  receipt.channels[name] = { status,
    hash: status === "current" ? receipt.desired.hash : receipt.channels[name]?.hash || null,
    updatedAt: new Date().toISOString(), ...(detail ? { detail } : {}) };
  save(receipt);
}

function sourceState(previous = null) {
  if (!existsSync(sourcePath)) throw new Error(`missing ${sourcePath}`);
  const bytes = readFileSync(sourcePath);
  const tracked = git("ls-files", "--", "xbox/live/oskiewar.js") !== "";
  const sourceStatus = git("status", "--porcelain", "--", "xbox/live/oskiewar.js");
  const dirty = sourceStatus !== "";
  const workingChanges = git("status", "--porcelain", "--", "apple/oskiewar",
    "xbox/live", "xbox/native-bios", "xbox/package")
    .split("\n").filter(Boolean).map((line) => line.replace(/^.{1,2}\s+/, ""));
  const baseline = previous?.desired?.commit || `${git("rev-parse", "HEAD")}^`;
  let committedChanges = [];
  try {
    committedChanges = git("diff", "--name-only", `${baseline}..HEAD`, "--",
      "apple/oskiewar", "xbox/live", "xbox/native-bios", "xbox/package")
      .split("\n").filter(Boolean);
  } catch {}
  const changed = [...new Set([...committedChanges, ...workingChanges])];
  const buildMatch = bytes.toString("utf8").match(/const buildVersion = (\d+);/);
  const build = buildMatch ? Number(buildMatch[1]) : null;
  const expectedBuild = Number(git("rev-list", "--count", "HEAD", "--",
    "xbox/live/oskiewar.js")) + (dirty ? 1 : 0);
  return { hash: sha256(bytes), tracked, dirty, changed,
    commit: git("rev-parse", "HEAD"), severity: classifySeverity(changed),
    build, expectedBuild };
}

async function verifyWeb(hash) {
  const response = await fetch(`https://oskiewar.com/oskiewar.js?parity=${Date.now()}`,
    { cache: "no-store" });
  if (!response.ok) throw new Error(`web returned HTTP ${response.status}`);
  const actual = sha256(Buffer.from(await response.arrayBuffer()));
  if (actual !== hash) throw new Error(`web hash ${actual.slice(0, 12)} != ${hash.slice(0, 12)}`);
}

async function reconcile(receipt, { dryRun = false } = {}) {
  const hash = receipt.desired.hash;
  if (receipt.channels.web.status !== "current") {
    if (dryRun) console.log("would deploy web");
    else try {
      run("fish", ["lith/deploy.fish"]);
      await verifyWeb(hash);
      mark(receipt, "web", "current", "verified production bytes");
    } catch (error) { mark(receipt, "web", "failed", error.message); }
  }
  // iOS game code is the production web channel; its bundled copy remains the
  // offline fallback. A native-shell change is deliberately not called live.
  if (receipt.channels.web.status === "current" &&
      receipt.desired.severity !== "ios-native" &&
      receipt.desired.severity !== "multi-native")
    mark(receipt, "ios", "current", "production web source; bundled fallback retained");
  else if (!dryRun) mark(receipt, "ios", "pending",
    receipt.desired.severity.includes("native") ? "native iOS refresh required" : "waiting for web");

  if (receipt.channels.xbox.status !== "current") {
    if (dryRun) console.log("would deploy Xbox live source");
    else try {
      run("node", ["xbox/tools/live.mjs", "deploy", "xbox/live/oskiewar.js"]);
      mark(receipt, "xbox", "current", "Device Portal accepted source and launch");
    } catch (error) { mark(receipt, "xbox", "failed", error.message); }
  }
  return receipt;
}

function print(receipt, current = null) {
  console.log(JSON.stringify({ current, receipt, parity: receipt
    ? channels.every((name) => receipt.channels[name]?.status === "current")
    : false }, null, 2));
}

async function main() {
  const [command = "status", ...args] = process.argv.slice(2);
  const dryRun = args.includes("--dry-run");
  const previous = readReceipt();
  const current = sourceState(previous);
  if (command === "status") return print(readReceipt(), current);
  if (command === "deploy") {
    if (!current.tracked || current.dirty)
      throw new Error("Oskiewar source must be tracked and committed before a unified release");
    if (current.build !== current.expectedBuild)
      throw new Error(`Oskiewar build v${current.build ?? "?"} does not match ` +
        `its committed source revision count v${current.expectedBuild}`);
    const receipt = newRelease(current.hash, current.commit, current.severity, previous);
    save(receipt);
    await reconcile(receipt, { dryRun });
    return print(receipt, current);
  }
  if (command === "deploy-xbox-dev") {
    const receipt = newRelease(current.hash, current.commit,
      current.severity, previous);
    receipt.desired.development = true;
    save(receipt);
    try {
      run("node", ["xbox/tools/live.mjs", "deploy", "xbox/live/oskiewar.js"]);
      mark(receipt, "xbox", "current", "explicit uncommitted Xbox development release");
    } catch (error) {
      mark(receipt, "xbox", "failed", error.message);
      throw error;
    }
    mark(receipt, "web", "pending", "working source is not committed");
    mark(receipt, "ios", "pending", "waiting for unified production release");
    return print(receipt, current);
  }
  if (command === "reconcile") {
    const receipt = readReceipt();
    if (!receipt) throw new Error("no pending Oskiewar release receipt");
    if (receipt.desired.hash !== current.hash)
      throw new Error("working source differs from the pending release; deploy the new release first");
    await reconcile(receipt, { dryRun });
    return print(receipt, current);
  }
  throw new Error("commands: status | deploy [--dry-run] | deploy-xbox-dev | reconcile [--dry-run]");
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url))
  main().catch((error) => { console.error(error.message); process.exitCode = 1; });
