#!/usr/bin/env node
// history — the sessions this machine has had, and a way back into one.
//
// `cr` reopens a Claude conversation and `cor` a Codex one; this is the same
// door for Easel. Every session wrote a transcript (transcript.mjs), and its
// engine event names the thread the engine was running, so resuming is a
// matter of reading that back and launching the interface with it: the same
// directory, the same engine and model, the same mode, the old thread.
//
//   ac history            list, then pick a number
//   ac history --last     straight into the most recent
//   ac history 3          straight into the third on the list
import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createInterface } from "node:readline";
import { Transcript, defaultRoot } from "./transcript.mjs";

const EASEL = join(dirname(fileURLToPath(import.meta.url)), "..", "bin", "easel");
const LIMIT = 20;

// The last thread the engine reported is the one to resume; a session that
// switched engines mid-way resumes on the engine it ended on.
function lastEngine(sessionId, root) {
  let found = null;
  for (const event of Transcript.read(sessionId, root)) if (event.kind === "engine" && event.thread) found = event;
  return found;
}

function when(iso) {
  const ms = Date.now() - Date.parse(iso);
  const minutes = Math.round(ms / 60000);
  if (minutes < 60) return `${minutes}m ago`;
  const hours = Math.round(minutes / 60);
  if (hours < 48) return `${hours}h ago`;
  return `${Math.round(hours / 24)}d ago`;
}

function tilde(path) {
  const home = homedir();
  return path === home ? "~" : path.startsWith(`${home}/`) ? `~${path.slice(home.length)}` : path;
}

export function sessions(root = defaultRoot()) {
  return Transcript.list(root)
    .map((meta) => ({ ...meta, engineEvent: lastEngine(meta.session_id, root) }))
    .filter((meta) => meta.engineEvent && meta.cwd)
    .slice(0, LIMIT);
}

export function launchArguments(meta) {
  const args = [];
  if (meta.pro) args.push("pro");
  if (meta.private) args.push("--private");
  args.push("--cwd", meta.cwd, "--backend", meta.engineEvent.engine, "--resume", meta.engineEvent.thread);
  if (meta.engineEvent.model) args.push("--model", meta.engineEvent.model);
  return args;
}

async function pick(list) {
  const rl = createInterface({ input: process.stdin, output: process.stdout });
  const answer = await new Promise((resolve) => rl.question("resume › ", resolve));
  rl.close();
  const index = Number.parseInt(answer, 10);
  return Number.isInteger(index) && index >= 1 && index <= list.length ? list[index - 1] : null;
}

async function main() {
  const argv = process.argv.slice(2);
  const list = sessions();
  if (!list.length) {
    process.stderr.write("No sessions yet. `a` starts one.\n");
    process.exit(1);
  }
  let chosen = null;
  const direct = argv.find((a) => /^\d+$/.test(a));
  if (argv.includes("--last")) chosen = list[0];
  else if (direct) chosen = list[Number(direct) - 1] || null;
  else {
    list.forEach((meta, index) => {
      const subject = meta.private ? "private" : meta.subject || "(no subject yet)";
      const mode = meta.pro ? "pro" : "piece";
      process.stdout.write(`${String(index + 1).padStart(3)}  ${when(meta.updated).padEnd(8)} ${mode.padEnd(5)} ${(meta.engineEvent.model || meta.engineEvent.engine).padEnd(18)} ${tilde(meta.cwd).padEnd(28)} ${subject.slice(0, 60)}\n`);
    });
    chosen = await pick(list);
  }
  if (!chosen) {
    process.stderr.write("Nothing chosen.\n");
    process.exit(1);
  }
  const result = spawnSync(EASEL, launchArguments(chosen), { stdio: "inherit" });
  process.exit(result.status ?? 1);
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) main();
