// Direct commands: lines Aesel runs itself, in the workspace, without asking
// the model. `!anything` is an explicit shell line. A bare line that is plainly
// a command — `npm start`, `rpm start`, `run dev`, "start the server" — is
// inferred from a small map and the workspace's package.json scripts. Anything
// that reads like a request rather than a command goes to the model as before.
//
// Nothing here spends braincells. A job that fails hands the model a short
// tail of its output on the next turn, so "why did that break?" still works.

import { spawn } from "node:child_process";
import { readFileSync, existsSync } from "node:fs";
import path from "node:path";

// Package managers, and the slips a thumb makes typing them.
const MANAGERS = { npm: "npm", pnpm: "pnpm", yarn: "yarn", bun: "bun" };
const SLIPS = { rpm: "npm", nmp: "npm", npn: "npm", mpm: "npm", bpm: "npm", nom: "npm", npmn: "npm", pnmp: "pnpm", yran: "yarn" };

// Programs whose bare invocation is a command, never a sentence.
const PROGRAMS = new Set(["node", "npx", "git", "ls", "pwd", "make", "python", "python3", "pip", "pip3", "cargo", "go", "deno", "tsc", "vite", "open", "cat", "tail", "head", "curl", "brew", "swift", "docker", "kill", "lsof"]);

// Some program names are also English words ("make a game", "go faster",
// "open the door"). Those count as commands only when what follows looks like
// their arguments.
const GIT_VERBS = new Set(["status", "log", "diff", "add", "commit", "push", "pull", "fetch", "branch", "checkout", "switch", "stash", "init", "clone", "show", "restore", "reset", "merge", "rebase", "remote", "tag"]);
const GO_VERBS = new Set(["run", "build", "test", "mod", "get", "install", "fmt", "vet"]);
const CARGO_VERBS = new Set(["run", "build", "test", "check", "new", "add", "clippy", "fmt"]);
const looksLikePath = (word) => /[./~]/.test(word || "") || /^https?:/.test(word || "");
function programLine(first, words, cwd) {
  const arg = words[1];
  switch (first) {
    case "git": return GIT_VERBS.has(arg);
    case "go": return GO_VERBS.has(arg);
    case "cargo": return CARGO_VERBS.has(arg);
    case "make": return existsSync(path.join(cwd, "Makefile")) && (words.length === 1 || (words.length === 2 && /^[\w.-]+$/.test(arg) && !/^(?:it|a|an|the|me|this|that|some|my)$/i.test(arg)));
    case "open": case "cat": case "tail": case "head": return words.slice(1).some(looksLikePath) || words.slice(1).some(w => /^-/.test(w));
    case "kill": return words.slice(1).every(w => /^-?\w+$/.test(w)) && words.slice(1).some(w => /^\d+$/.test(w));
    case "brew": case "pip": case "pip3": case "docker": case "swift": return words.length > 1 && /^[a-z-]+$/.test(arg) && !/^(?:me|the|a|an|it|some)$/i.test(arg);
    case "ls": case "pwd": return true;
    default: return words.length === 1 ? ["node", "python", "python3", "deno"].includes(first) === false : true;
  }
}

// npm verbs that need no `run`.
const BUILTIN_VERBS = new Set(["start", "test", "install", "i", "ci", "stop", "restart", "init", "update", "outdated", "ls", "audit", "run", "run-script", "exec", "add", "remove", "uninstall", "dev", "build"]);

// Plain words people use for a script.
const PHRASES = [
  [/^(?:please\s+)?(?:start|launch|boot|serve|run)(?:\s+(?:it|this|the\s+(?:app|server|project|site|game|dev\s+server)|up|the\s+thing))?(?:\s+(?:up|please))?[.!]?$/i, ["start", "dev", "serve"]],
  [/^(?:please\s+)?(?:run\s+)?(?:the\s+)?dev(?:\s+server)?[.!]?$/i, ["dev", "start"]],
  [/^(?:please\s+)?(?:run\s+(?:the\s+)?)?tests?[.!]?$/i, ["test"]],
  [/^(?:please\s+)?(?:run\s+(?:the\s+)?)?build(?:\s+it)?[.!]?$/i, ["build"]],
  [/^(?:please\s+)?install(?:\s+(?:the\s+)?(?:deps|dependencies|packages))?[.!]?$/i, ["install"]],
];

export function packageScripts(cwd) {
  try {
    const file = path.join(cwd, "package.json");
    if (!existsSync(file)) return null;
    return JSON.parse(readFileSync(file, "utf8")).scripts || {};
  } catch { return null; }
}

function managerFor(cwd) {
  if (existsSync(path.join(cwd, "pnpm-lock.yaml"))) return "pnpm";
  if (existsSync(path.join(cwd, "yarn.lock"))) return "yarn";
  if (existsSync(path.join(cwd, "bun.lockb")) || existsSync(path.join(cwd, "bun.lock"))) return "bun";
  return "npm";
}

function scriptCommand(manager, name) {
  if (name === "install") return `${manager} install`;
  if (manager === "npm" && (name === "start" || name === "test")) return `npm ${name}`;
  return manager === "npm" ? `npm run ${name}` : `${manager} ${name}`;
}

// What to run for `text`, or null when it is a message for the model.
// Returns { command, note } — note says what was inferred, when anything was.
export function inferDirect(text, cwd) {
  const line = String(text || "").trim();
  if (!line || line.includes("\n")) return null;
  if (line.startsWith("!")) {
    const command = line.slice(1).trim();
    return command ? { command, note: "" } : null;
  }
  if (line.length > 80) return null;
  const words = line.split(/\s+/);
  const first = words[0].toLowerCase();
  const scripts = packageScripts(cwd);

  // npm start, rpm start, yarn dev, pnpm run build …
  const manager = MANAGERS[first] || SLIPS[first];
  if (manager && words.length >= 2 && words.length <= 6) {
    const verb = words[1].toLowerCase();
    const script = verb === "run" || verb === "run-script" ? words[2] : verb;
    if (!/^[\w:.@/-]+$/.test(script || "")) return null;
    const known = BUILTIN_VERBS.has(verb) || (scripts && script in scripts);
    if (!known) return null;
    const rest = words.slice(1).join(" ");
    return { command: `${manager} ${rest}`, note: manager !== first ? `${first} → ${manager}` : "" };
  }
  if (manager && words.length === 1) return null;

  // node server.js, git status, make …
  if (PROGRAMS.has(first) && words.length <= 8 && /^[\w./@:=+,'"~-]+(?:\s+[\w./@:=+,'"~*-]+)*$/.test(line) && programLine(first, words, cwd)) {
    return { command: line, note: "" };
  }

  // "start the server", "run dev", "tests"
  if (!scripts) return null;
  for (const [pattern, candidates] of PHRASES) {
    if (!pattern.test(line)) continue;
    const name = candidates.find(candidate => candidate === "install" || candidate in scripts);
    if (!name) return null;
    const m = managerFor(cwd);
    const command = scriptCommand(m, name);
    return { command, note: `“${line}” → ${command}` };
  }
  // A bare script name: `dev`, `build:web`.
  if (words.length === 1 && /^[\w:.-]+$/.test(line) && line in scripts && line.length > 2) {
    const command = scriptCommand(managerFor(cwd), line);
    return { command, note: `${line} → ${command}` };
  }
  return null;
}

const TAIL_LINES = 14;
const CONTEXT_CHARS = 1600;

// Running jobs. Each streams its tail into one transcript entry.
export class DirectJobs {
  constructor({ cwd, onUpdate, env = process.env, shell = process.env.SHELL || "/bin/sh" }) {
    this.cwd = cwd;
    this.onUpdate = onUpdate;
    this.env = env;
    this.shell = shell;
    this.jobs = [];
    this.nextId = 1;
    this.failures = [];
  }

  run(command) {
    const id = this.nextId++;
    const job = { id, command, lines: [], partial: "", started: Date.now(), code: null, signal: null, child: null };
    // A login-free interactive-free shell so PATH (nvm, homebrew) matches the
    // user's terminal as nearly as a child can.
    const child = spawn(this.shell, ["-lc", command], {
      cwd: this.cwd,
      env: { ...this.env, FORCE_COLOR: "0", NO_COLOR: "1", CI: this.env.CI || "" },
      stdio: ["ignore", "pipe", "pipe"],
      detached: process.platform !== "win32",
    });
    job.child = child;
    const take = (chunk) => {
      const text = job.partial + String(chunk).replace(/\x1b\[[0-9;?]*[A-Za-z]/g, "").replace(/\r(?!\n)/g, "\n");
      const parts = text.split(/\r?\n/);
      job.partial = parts.pop();
      for (const part of parts) job.lines.push(part);
      if (job.lines.length > 400) job.lines.splice(0, job.lines.length - 400);
      this.onUpdate(job);
    };
    child.stdout.on("data", take);
    child.stderr.on("data", take);
    child.on("error", (error) => { job.lines.push(String(error.message || error)); job.code = 127; this.settle(job); });
    child.on("close", (code, signal) => { if (job.partial) { job.lines.push(job.partial); job.partial = ""; } job.code = code ?? (signal ? 128 : 0); job.signal = signal; this.settle(job); });
    this.jobs.push(job);
    this.onUpdate(job);
    return job;
  }

  settle(job) {
    if (job.settled) return;
    job.settled = true;
    if (job.code !== 0 && !job.killed) this.failures.push(job);
    this.onUpdate(job);
  }

  running() { return this.jobs.filter(job => job.code === null); }

  kill(which) {
    const targets = which == null || which === "" || which === "all"
      ? this.running()
      : this.running().filter(job => String(job.id) === String(which).replace(/^#/, ""));
    for (const job of targets) {
      job.killed = true;
      try { process.kill(process.platform !== "win32" ? -job.child.pid : job.child.pid, "SIGTERM"); } catch { try { job.child.kill("SIGTERM"); } catch {} }
      setTimeout(() => { if (job.code === null) { try { process.kill(-job.child.pid, "SIGKILL"); } catch { try { job.child.kill("SIGKILL"); } catch {} } } }, 2000).unref?.();
    }
    return targets;
  }

  // The words the model sees about jobs that failed since the last turn,
  // once each. Empty when nothing failed.
  takeContext() {
    if (!this.failures.length) return "";
    const failed = this.failures.splice(0);
    let out = "\n\n[Direct commands the user ran in the workspace that failed]";
    for (const job of failed) out += `\n$ ${job.command}  (exit ${job.code})\n${job.lines.slice(-20).join("\n")}`;
    return out.length > CONTEXT_CHARS ? out.slice(0, 200) + "\n…\n" + out.slice(-(CONTEXT_CHARS - 200)) : out;
  }
}

// The entry text for a job: its command, state and output tail.
export function jobText(job, now = Date.now()) {
  const seconds = Math.max(0, Math.round((now - job.started) / 1000));
  const state = job.code === null
    ? `running ${seconds}s · /kill ${job.id} stops it`
    : job.killed ? "stopped"
    : job.code === 0 ? `done in ${seconds}s` : `exit ${job.code}`;
  const tail = job.lines.filter((line, index, all) => line.trim() || index < all.length - 1).slice(-TAIL_LINES);
  return `$ ${job.command}   #${job.id} ${state} · no braincells${tail.length ? "\n" + tail.join("\n") : ""}`;
}
