#!/usr/bin/env node
// iris.mjs — a mission watcher for the slab menubar.
//
// Reports what the Iris mission controller and its lanes are doing on the
// fleet, as JSON the Swift menubar renders into an `Iris` submenu (mirrors
// `asana`). Each machine is read over ssh in one round-trip: the desktop
// badge's mission.json, and on the controller the mission daemon's state,
// config and recent events, plus the newest run status files.
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// machine names, hosts or accounts: those live in an UNTRACKED config at
// ~/.config/slab/iris.json (see `iris config`).
//
// Subcommands:
//   iris status            JSON summary to stdout (default; always exits 0)
//   iris events [n]        the controller's last n events, one per line
//   iris config            print/create the config stub path
//   iris open              open the configured board URL in the browser
//   iris avatar            (re)fetch the controller's avatar image into the cache
//
// Config shape:
//   machines[]  { name, role, ssh }   role = "controller" | "lane"; `ssh` is
//               an alias from ~/.ssh/config — or give { host, user, identityFile,
//               port } explicitly to bypass the alias (and any Match rules on it)
//   badgeFile   path under $HOME of the per-machine badge mission.json
//   controllerDir  path under $HOME of the controller's state directory
//   boardUrl    optional; opened by `iris open`
//   avatarFile  path under $HOME on the controller of a PNG avatar; cached at
//               ~/.local/share/slab/iris/avatar.png (refreshed weekly, or `iris avatar`)
//   sshTimeout  seconds per machine (default 6)

import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, statSync, unlinkSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname, basename } from "node:path";

const HOME = homedir();
const CONFIG_PATH =
  process.env.SLAB_IRIS_CONFIG || join(HOME, ".config", "slab", "iris.json");

const CONFIG_STUB = {
  _README:
    "slab iris config — UNTRACKED, never committed. List the machines that " +
    "run Iris missions; `ssh` is the alias from ~/.ssh/config.",
  label: "Iris",
  machines: [],
  machinesComment:
    'e.g. [{"name":"alpha","ssh":"alpha","role":"controller"},' +
    '{"name":"beta","role":"lane","host":"100.64.0.2","user":"beta","identityFile":"~/.ssh/id_ed25519"}]',
  badgeFile: ".local/share/desktop-badge/mission.json",
  controllerDir: ".local/share/iris/clickup-missions",
  boardUrl: "",
  avatarFile: ".local/share/desktop-badge/iris-avatar.png",
  sshTimeout: 6,
};
const AVATAR_CACHE = join(HOME, ".local", "share", "slab", "iris", "avatar.png");
const AVATAR_MAX_AGE_MS = 7 * 86400e3;

function loadConfig() {
  if (!existsSync(CONFIG_PATH)) return null;
  try {
    const c = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
    if (!Array.isArray(c.machines) || c.machines.length === 0) return null;
    return { ...CONFIG_STUB, ...c };
  } catch {
    return null;
  }
}

function ensureConfig() {
  if (existsSync(CONFIG_PATH)) return false;
  mkdirSync(dirname(CONFIG_PATH), { recursive: true });
  writeFileSync(CONFIG_PATH, JSON.stringify(CONFIG_STUB, null, 2) + "\n", {
    mode: 0o600,
  });
  return true;
}

// ─── remote read ─────────────────────────────────────────────────────────

// One ssh per machine. The remote side is plain sh (no node dependency):
// it prints tagged sections we split locally. Anything missing just leaves
// its section empty.
function remoteScript(cfg, machine) {
  const badge = `"$HOME/${cfg.badgeFile}"`;
  const dir = `"$HOME/${cfg.controllerDir}"`;
  const lines = [
    `echo '@@badge'; cat ${badge} 2>/dev/null; echo`,
    `echo '@@runs'; for d in $(ls -td ${dir}/runs/* 2>/dev/null | head -3); do echo "@@run $(basename "$d")"; cat "$d/status.json" 2>/dev/null; echo; done`,
  ];
  if (machine.role === "controller") {
    lines.push(
      `echo '@@state'; cat ${dir}/state.json 2>/dev/null; echo`,
      `echo '@@config'; cat ${dir}/config.json 2>/dev/null; echo`,
      `echo '@@events'; tail -n 40 ${dir}/events.jsonl 2>/dev/null`,
    );
  }
  lines.push(`echo '@@end'`);
  return lines.join("; ");
}

function ssh(cfg, machine) {
  return new Promise((resolve) => {
    const timeout = Number(cfg.sshTimeout) || 6;
    const args = [
      "-o", "BatchMode=yes",
      "-o", `ConnectTimeout=${timeout}`,
      "-o", "StrictHostKeyChecking=accept-new",
    ];
    // Explicit host/user/key bypass ~/.ssh/config aliases entirely, which also
    // skips any `Match host <alias> exec …` probes that alias may carry.
    if (machine.user) args.push("-o", `User=${machine.user}`);
    if (machine.identityFile) {
      args.push("-o", `IdentityFile=${machine.identityFile.replace(/^~/, HOME)}`,
                "-o", "IdentitiesOnly=yes");
    }
    if (machine.port) args.push("-p", String(machine.port));
    args.push(machine.host || machine.ssh || machine.name, remoteScript(cfg, machine));
    const child = spawn("ssh", args, { stdio: ["ignore", "pipe", "pipe"] });
    let out = "", err = "";
    child.stdout.on("data", (d) => (out += d));
    child.stderr.on("data", (d) => (err += d));
    const killer = setTimeout(() => child.kill("SIGKILL"), (timeout + 4) * 1000);
    child.on("close", (code) => {
      clearTimeout(killer);
      resolve({ ok: code === 0 && out.includes("@@end"), out, err: err.trim() });
    });
    child.on("error", (e) => {
      clearTimeout(killer);
      resolve({ ok: false, out: "", err: e.message });
    });
  });
}

function sections(text) {
  const map = { runs: [] };
  let key = null;
  for (const line of text.split("\n")) {
    if (line.startsWith("@@run ")) {
      map.runs.push({ id: line.slice(6).trim(), body: "" });
      key = "run";
      continue;
    }
    if (line.startsWith("@@")) {
      key = line.slice(2).trim();
      if (key !== "runs" && key !== "end") map[key] = "";
      continue;
    }
    if (key === "run") map.runs[map.runs.length - 1].body += line + "\n";
    else if (key && key in map) map[key] += line + "\n";
  }
  return map;
}

const parse = (s, fallback = null) => {
  try { return JSON.parse(s); } catch { return fallback; }
};

// Copy the controller's avatar PNG into the local cache (one scp, off any UI
// thread — the menubar calls this helper from a background queue). Returns the
// cache path, or "" when no controller is configured or the copy failed.
function fetchAvatar(cfg) {
  const machine = cfg.machines.find((m) => m.role === "controller");
  if (!machine || !cfg.avatarFile) return Promise.resolve("");
  return new Promise((resolve) => {
    mkdirSync(dirname(AVATAR_CACHE), { recursive: true });
    const timeout = Number(cfg.sshTimeout) || 6;
    const args = ["-q", "-o", "BatchMode=yes", "-o", `ConnectTimeout=${timeout}`,
                  "-o", "StrictHostKeyChecking=accept-new"];
    if (machine.identityFile) {
      args.push("-o", `IdentityFile=${machine.identityFile.replace(/^~/, HOME)}`,
                "-o", "IdentitiesOnly=yes");
    }
    if (machine.port) args.push("-P", String(machine.port));
    const host = machine.host || machine.ssh || machine.name;
    const who = machine.user ? `${machine.user}@${host}` : host;
    const tmp = AVATAR_CACHE + ".part";
    const child = spawn("scp", [...args, `${who}:${cfg.avatarFile}`, tmp], { stdio: "ignore" });
    const killer = setTimeout(() => child.kill("SIGKILL"), (timeout + 20) * 1000);
    child.on("close", (code) => {
      clearTimeout(killer);
      try {
        if (code === 0) { renameSync(tmp, AVATAR_CACHE); resolve(AVATAR_CACHE); return; }
      } catch {}
      try { unlinkSync(tmp); } catch {}
      resolve("");
    });
    child.on("error", () => { clearTimeout(killer); resolve(""); });
  });
}

function avatarFresh() {
  try {
    const st = statSync(AVATAR_CACHE);
    return st.size > 0 && Date.now() - st.mtimeMs < AVATAR_MAX_AGE_MS;
  } catch { return false; }
}

// ─── shaping ─────────────────────────────────────────────────────────────

const ageSec = (iso) => {
  const t = Date.parse(iso || "");
  return Number.isFinite(t) ? Math.max(0, Math.round((Date.now() - t) / 1000)) : null;
};

const PHASE_ORDER = ["running", "queued", "blocked", "awaiting_review"];

function shapeMachine(machine, res) {
  const m = {
    name: machine.name,
    role: machine.role || "lane",
    online: res.ok,
    error: res.ok ? "" : (res.err || "unreachable").split("\n")[0].slice(0, 120),
    heartbeatAge: null,
    headline: "",
    items: [],
    runs: [],
  };
  if (!res.ok) { m.headline = "unreachable"; return { m, sec: null }; }
  const sec = sections(res.out);
  const badge = parse(sec.badge, {}) || {};
  m.heartbeatAge = ageSec(badge.lastHeartbeat || badge.updatedAt);
  m.items = Array.isArray(badge.items)
    ? badge.items.map((i) => ({ text: String(i.text || ""), status: String(i.status || "") }))
    : [];
  m.headline = badge.phase || m.items[0]?.text || (badge.mission ? String(badge.mission) : "");
  m.nextStep = badge.nextStep ? String(badge.nextStep) : "";
  m.runs = sec.runs.map((r) => {
    const s = parse(r.body, {}) || {};
    return {
      run: r.id,
      taskId: s.task_id || r.id.split("-")[0],
      phase: s.phase || "",
      action: s.action || "",
      nextStep: s.next_step || "",
      updatedAt: s.updated_at || "",
      blockers: Array.isArray(s.blockers) ? s.blockers.map(String) : [],
    };
  });
  return { m, sec };
}

function shapeController(sec) {
  const state = parse(sec.state, {}) || {};
  const config = parse(sec.config, {}) || {};
  const events = (sec.events || "")
    .split("\n").filter(Boolean).map((l) => parse(l)).filter(Boolean);
  const lanes = state.lanes && typeof state.lanes === "object" ? state.lanes : {};
  const laneByTask = {};
  for (const [laneName, run] of Object.entries(lanes)) {
    if (run && run.id) laneByTask[run.id] = { lane: laneName, ...run };
  }
  const tasks = Object.values(state.tasks || {}).map((t) => {
    const live = laneByTask[t.id];
    return {
      id: String(t.id),
      name: String(t.name || t.id),
      url: String(t.url || ""),
      phase: live ? "running" : String(t.phase || ""),
      attempts: Number(t.attempts) || 0,
      blocker: t.blocker && t.phase === "blocked" ? String(t.blocker).split("\n")[0] : "",
      lane: live ? String(live.lane) : "",
      runDir: live && live.dir ? basename(String(live.dir)) : "",
      startedAt: live ? String(live.startedAt || "") : "",
      priority: String(t.priority || "normal"),
    };
  });
  const rank = (p) => { const i = PHASE_ORDER.indexOf(p); return i < 0 ? 99 : i; };
  tasks.sort((a, b) => rank(a.phase) - rank(b.phase) || a.name.localeCompare(b.name));
  const expiresAt = config.expiresAt || "";
  const exp = Date.parse(expiresAt);
  const controller = {
    lastPollAt: state.lastPollAt || "",
    pollAge: ageSec(state.lastPollAt),
    lastError: state.lastError ? String(state.lastError) : "",
    expiresAt,
    windowOpen: Number.isFinite(exp) ? Date.now() < exp : false,
    maxRunsPerTask: Number(config.maxRunsPerTask) || 0,
    waitingFor: state.waitingFor ? String(state.waitingFor) : "",
    heldBack: Object.fromEntries(
      Object.entries(state.heldBack || {}).filter(([, v]) => v).map(([k, v]) => [k, String(v)]),
    ),
    counts: {},
  };
  for (const t of tasks) controller.counts[t.phase] = (controller.counts[t.phase] || 0) + 1;
  const pollErrors = events.filter((e) => e.kind === "poll_error").length;
  controller.pollErrorsRecent = pollErrors;
  const recent = events
    .filter((e) => e.kind !== "poll_error")
    .filter((e) => e.kind !== "progress" || e.action)
    .slice(-12)
    .reverse()
    .map((e) => ({
      at: String(e.at || ""),
      kind: String(e.kind || ""),
      id: e.id ? String(e.id) : "",
      lane: e.lane ? String(e.lane) : "",
      text: String(e.action || e.reason || e.error || e.phase || "").slice(0, 120),
    }));
  return { controller, tasks, recent };
}

async function status() {
  const cfg = loadConfig();
  if (!cfg) {
    return { configured: false, label: "Iris: setup", machines: [], tasks: [], recent: [] };
  }
  const [results, avatarPath] = await Promise.all([
    Promise.all(cfg.machines.map((m) => ssh(cfg, m))),
    avatarFresh() ? Promise.resolve(AVATAR_CACHE) : fetchAvatar(cfg),
  ]);
  const machines = [];
  let controller = null, tasks = [], recent = [];
  cfg.machines.forEach((machine, i) => {
    const { m, sec } = shapeMachine(machine, results[i]);
    machines.push(m);
    if (machine.role === "controller" && sec) {
      const c = shapeController(sec);
      controller = c.controller; tasks = c.tasks; recent = c.recent;
    }
  });
  // Decorate running tasks with the lane machine's live run status.
  for (const t of tasks) {
    if (t.phase !== "running") continue;
    const lane = machines.find((m) => m.name === t.lane);
    const run = lane?.runs.find((r) => r.run === t.runDir) || lane?.runs.find((r) => r.taskId === t.id);
    if (run) { t.runPhase = run.phase; t.action = run.action; t.nextStep = run.nextStep; }
  }
  const counts = controller?.counts || {};
  const running = counts.running || 0;
  const bits = [running ? `${running} running` : "idle"];
  if (counts.queued) bits.push(`${counts.queued} queued`);
  if (counts.blocked) bits.push(`${counts.blocked} blocked`);
  if (counts.awaiting_review) bits.push(`${counts.awaiting_review} review`);
  const offline = machines.filter((m) => !m.online).length;
  if (offline) bits.push(`${offline} offline`);
  if (controller && !controller.windowOpen) bits.push("window closed");
  const name = cfg.label || "Iris";
  const label = controller ? `${name}: ${bits.join(" · ")}` : `${name}: ${offline ? "offline" : "no controller"}`;
  return { configured: true, label, boardUrl: cfg.boardUrl || "", avatarPath: avatarPath || "", machines, controller, tasks, recent };
}

// ─── main ────────────────────────────────────────────────────────────────

async function main() {
  const [cmd = "status", ...rest] = process.argv.slice(2);
  if (cmd === "config") {
    const made = ensureConfig();
    console.log((made ? "created " : "") + CONFIG_PATH);
    return;
  }
  if (cmd === "open") {
    const cfg = loadConfig();
    if (!cfg?.boardUrl) { console.error("no boardUrl in " + CONFIG_PATH); process.exit(1); }
    spawn("open", [cfg.boardUrl], { stdio: "ignore", detached: true }).unref();
    return;
  }
  if (cmd === "avatar") {
    const cfg = loadConfig();
    if (!cfg) { console.error("not configured: " + CONFIG_PATH); process.exit(1); }
    const path = await fetchAvatar(cfg);
    if (!path) { console.error("avatar fetch failed"); process.exit(1); }
    console.log(path);
    return;
  }
  if (cmd === "events") {
    const s = await status();
    const n = Number(rest[0]) || 12;
    for (const e of (s.recent || []).slice(0, n)) {
      console.log(`${e.at.slice(11, 19)}  ${e.kind.padEnd(18)} ${e.id.padEnd(10)} ${e.lane.padEnd(8)} ${e.text}`);
    }
    return;
  }
  if (cmd !== "status") { console.error("unknown subcommand: " + cmd); process.exit(2); }
  try {
    console.log(JSON.stringify(await status()));
  } catch (e) {
    console.log(JSON.stringify({ configured: true, label: "Iris: error", error: String(e.message || e), machines: [], tasks: [], recent: [] }));
  }
}

main();
