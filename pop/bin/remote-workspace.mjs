#!/usr/bin/env node
// Private, on-demand backing store for ignored pop/*/out generations.
//
//   node pop/bin/remote-workspace.mjs push [lane]
//   node pop/bin/remote-workspace.mjs verify [lane]
//   node pop/bin/remote-workspace.mjs fetch <lane>/<path>
//   node pop/bin/remote-workspace.mjs restore <lane>

import { existsSync, mkdirSync, readFileSync, readdirSync } from "node:fs";
import { dirname, resolve, sep } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");
const ROOT = resolve(POP, "..");
const BUCKET = process.env.POP_WORKSPACE_BUCKET || "assets-aesthetic-computer";
const PREFIX = process.env.POP_WORKSPACE_PREFIX || "private/pop-workspace";

function loadSpacesEnv() {
  const vaultEnv = resolve(ROOT, "aesthetic-computer-vault/silo/.env");
  if (existsSync(vaultEnv)) {
    for (const line of readFileSync(vaultEnv, "utf8").split(/\r?\n/)) {
      const match = line.match(/^([A-Z0-9_]+)=(.*)$/);
      if (!match) continue;
      const [, key, raw] = match;
      if (!process.env[key]) {
        process.env[key] = raw.trim().replace(/^(['"])(.*)\1$/, "$2");
      }
    }
  }
  process.env.AWS_ACCESS_KEY_ID ||= process.env.SPACES_KEY || process.env.DO_SPACES_KEY;
  process.env.AWS_SECRET_ACCESS_KEY ||= process.env.SPACES_SECRET || process.env.DO_SPACES_SECRET;
  let endpoint = process.env.SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com";
  if (!/^https?:\/\//.test(endpoint)) endpoint = `https://${endpoint}`;
  return endpoint;
}

const ENDPOINT = loadSpacesEnv();

function aws(args, capture = false) {
  const result = spawnSync("aws", [...args, "--endpoint-url", ENDPOINT], {
    cwd: ROOT,
    env: process.env,
    encoding: "utf8",
    stdio: capture ? "pipe" : "inherit",
  });
  if (result.error) throw result.error;
  if (result.status !== 0) {
    if (capture && result.stderr) process.stderr.write(result.stderr);
    process.exit(result.status ?? 1);
  }
  return result.stdout || "";
}

function lanes() {
  return readdirSync(POP, { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && existsSync(resolve(POP, entry.name, "out")))
    .map((entry) => entry.name)
    .sort();
}

function checkedLane(raw) {
  if (!raw || !/^[a-z0-9][a-z0-9-]*$/.test(raw)) throw new Error(`invalid lane: ${raw}`);
  const out = resolve(POP, raw, "out");
  if (!out.startsWith(`${POP}${sep}`)) throw new Error("lane escapes pop/");
  return { lane: raw, out };
}

function remote(lane, suffix = "") {
  return `s3://${BUCKET}/${PREFIX}/${lane}/out${suffix ? `/${suffix}` : ""}`;
}

function syncLane(rawLane, dryRun) {
  const { lane, out } = checkedLane(rawLane);
  if (!existsSync(out)) throw new Error(`missing local output directory: ${out}`);
  console.log(`${dryRun ? "verify" : "push"} ${lane}: ${remote(lane)}`);
  const args = ["s3", "sync", out, remote(lane), "--no-progress"];
  if (dryRun) args.push("--dryrun");
  else args.push("--only-show-errors", "--acl", "private");
  const output = aws(args, dryRun);
  if (dryRun && output.trim()) process.stdout.write(output);
  return dryRun ? output.split(/\r?\n/).filter(Boolean).length : 0;
}

const [command, arg] = process.argv.slice(2);

if (command === "push" || command === "verify") {
  const selected = arg ? [checkedLane(arg).lane] : lanes();
  let pending = 0;
  for (const lane of selected) pending += syncLane(lane, command === "verify");
  if (command === "verify") {
    console.log(`pending remote actions: ${pending}`);
    process.exitCode = pending === 0 ? 0 : 2;
  }
} else if (command === "fetch") {
  if (!arg || arg.startsWith("/") || arg.split("/").includes("..")) {
    throw new Error("fetch expects <lane>/<path> without traversal");
  }
  const [lane, ...parts] = arg.split("/");
  checkedLane(lane);
  if (!parts.length) throw new Error("fetch expects a file beneath the lane");
  const relative = parts.join("/");
  const destination = resolve(POP, lane, "out", relative);
  if (!destination.startsWith(`${resolve(POP, lane, "out")}${sep}`)) {
    throw new Error("destination escapes the lane output directory");
  }
  mkdirSync(dirname(destination), { recursive: true });
  aws(["s3", "cp", remote(lane, relative), destination, "--no-progress"]);
  console.log(`restored ${arg}`);
} else if (command === "restore") {
  const { lane, out } = checkedLane(arg);
  mkdirSync(out, { recursive: true });
  aws(["s3", "sync", remote(lane), out, "--no-progress", "--only-show-errors"]);
  console.log(`restored lane ${lane}`);
} else {
  console.error("usage: remote-workspace.mjs push|verify [lane]");
  console.error("       remote-workspace.mjs fetch <lane>/<path>");
  console.error("       remote-workspace.mjs restore <lane>");
  process.exitCode = 1;
}
