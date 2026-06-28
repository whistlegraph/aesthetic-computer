#!/usr/bin/env node
// claude-remote-bridge — mirror jasellite's claude session markers onto the
// local slab state dir so the menubar tracks/colorizes remote sessions as if
// they were native.
//
// The trick (no Swift changes): a remote claude runs inside a LOCAL Terminal
// window (opened by `jasellite`), so that window has a local tty and the
// transport (mosh/ssh/et) has a local pid. The remote hooks tag each marker
// with the launch id; `jasellite` recorded launch_id → (local_tty, local_pid).
// We fetch the remote markers, rewrite tty+pid to the local ones, and drop
// them in ~/.local/share/slab/state/active-prompts/ — which slab already reads.
//
// Owns only the markers it creates (tracked in remote-mirrors.json); never
// touches genuine local sessions. Run via `jasellite` (auto-starts it) or
// standalone: `claude-remote-bridge` (foreground loop).

import { execFileSync } from "node:child_process";
import {
  mkdirSync, readFileSync, writeFileSync, readdirSync, existsSync,
  rmSync, utimesSync, closeSync, openSync,
} from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const HOME = homedir();
const STATE = join(HOME, ".local/share/slab/state");
const ACTIVE = join(STATE, "active-prompts");
const AWAIT = join(STATE, "awaiting-prompts");
const RUN = join(STATE, "running-tools");
const LAUNCH_DIR = join(STATE, "remote-launches");
const OWNED_FILE = join(STATE, "remote-mirrors.json");
const PID_FILE = join(STATE, "remote-bridge.pid");
const CONFIG = join(HOME, ".config/slab/remote-claude.json");
const POLL_MS = 2000;

const DEFAULTS = {
  name: "jasellite",
  sshAlias: "jasellite",
  remoteDumpPath: "~/.local/share/slab/hooks/remote-dump.mjs",
};
function loadConfig() {
  try { return { ...DEFAULTS, ...JSON.parse(readFileSync(CONFIG, "utf8")) }; }
  catch { return { ...DEFAULTS }; }
}

const alive = (pid) => { try { process.kill(pid, 0); return true; } catch { return false; } };
const readJson = (p, fb) => { try { return JSON.parse(readFileSync(p, "utf8")); } catch { return fb; } };

// A live local pid on `tty` — the window's liveness proxy and the marker's
// claude_pid. Prefer the transport client, else the innermost process; 0 means
// the window is gone. Resolved fresh each tick so mosh's slow-to-appear client
// (it bootstraps over ssh first) isn't a race the launcher has to win.
function pidOnTty(tty) {
  if (!tty) return 0;
  try {
    const out = execFileSync("bash", ["-lc", `ps -t ${tty} -o pid=,comm= 2>/dev/null`], { encoding: "utf8" });
    const rows = out.split("\n").map((l) => l.trim().match(/^(\d+)\s+(.+)$/)).filter(Boolean);
    if (!rows.length) return 0;
    const client = rows.find((m) => /mosh-client|ssh|\bet\b/i.test(m[2]));
    return Number((client || rows[rows.length - 1])[1]);
  } catch { return 0; }
}

// Reuse one ssh connection across polls (kind to a flaky link, low latency).
function sshArgs(cfg) {
  return [
    "-o", "BatchMode=yes",
    "-o", "ConnectTimeout=8",
    "-o", "ControlMaster=auto",
    "-o", `ControlPath=${join(HOME, ".ssh/cm-jasellite-%r")}`,
    "-o", "ControlPersist=60",
    cfg.sshAlias,
  ];
}

// Pull all remote markers in one shot: { active:{sid:obj}, awaiting:{sid:str}, running:{sid:mtime} }.
function fetchRemote(cfg) {
  try {
    const out = execFileSync(
      "ssh",
      [...sshArgs(cfg), "node", cfg.remoteDumpPath],
      { encoding: "utf8", timeout: 15000 },
    );
    return JSON.parse(out);
  } catch {
    return null; // link down / box asleep — keep last mirrors, try next tick
  }
}

// launch_id → { local_tty, local_pid } for launches whose window is still alive.
function liveLaunches() {
  const map = {};
  let entries = [];
  try { entries = readdirSync(LAUNCH_DIR); } catch { return map; }
  for (const id of entries) {
    const rec = readJson(join(LAUNCH_DIR, id), null);
    if (!rec) continue;
    const pid = pidOnTty(rec.local_tty); // liveness by tty, resolved live
    if (!pid) {
      rmSync(join(LAUNCH_DIR, id), { force: true }); // window closed — retire it
      continue;
    }
    rec.livePid = pid;
    map[rec.launch_id] = rec;
  }
  return map;
}

function tick(cfg) {
  const launches = liveLaunches();
  const remote = fetchRemote(cfg);
  const owned = readJson(OWNED_FILE, {}); // sid -> launch_id we created
  if (!remote) return;

  const nextOwned = {};
  for (const [sid, marker] of Object.entries(remote.active)) {
    const lr = launches[marker.launch_id];
    if (!lr) continue; // no live local window for this session — skip
    // Rewrite identity to the local window; keep everything else from remote.
    const local = {
      session_id: sid,
      cwd: marker.cwd,
      subject: marker.subject,
      summary: marker.summary,
      tty: lr.local_tty,
      claude_pid: lr.livePid,
      updated: marker.updated,
      state: marker.state,
      remote_host: cfg.name,
    };
    writeFileSync(join(ACTIVE, sid), JSON.stringify(local));
    nextOwned[sid] = marker.launch_id;

    // Mirror the awaiting + running-tool side markers (keyed by sid, no identity).
    const awaitPath = join(AWAIT, sid);
    if (sid in remote.awaiting) writeFileSync(awaitPath, remote.awaiting[sid]);
    else rmSync(awaitPath, { force: true });

    const runPath = join(RUN, sid);
    if (sid in remote.running) {
      if (!existsSync(runPath)) closeSync(openSync(runPath, "w"));
      const t = remote.running[sid] / 1000;
      try { utimesSync(runPath, t, t); } catch {}
    } else rmSync(runPath, { force: true });
  }

  // Retire mirrors we used to own that are gone from the remote (or lost their window).
  for (const sid of Object.keys(owned)) {
    if (nextOwned[sid]) continue;
    rmSync(join(ACTIVE, sid), { force: true });
    rmSync(join(AWAIT, sid), { force: true });
    rmSync(join(RUN, sid), { force: true });
  }
  writeFileSync(OWNED_FILE, JSON.stringify(nextOwned));
}

function main() {
  mkdirSync(ACTIVE, { recursive: true });
  mkdirSync(AWAIT, { recursive: true });
  mkdirSync(RUN, { recursive: true });
  // Single-instance guard.
  const prev = readJson(PID_FILE, null);
  if (prev?.pid && alive(prev.pid) && prev.pid !== process.pid) {
    console.error(`bridge already running (pid ${prev.pid})`);
    process.exit(0);
  }
  writeFileSync(PID_FILE, JSON.stringify({ pid: process.pid }));
  const cfg = loadConfig();
  const stop = () => { rmSync(PID_FILE, { force: true }); process.exit(0); };
  process.on("SIGINT", stop);
  process.on("SIGTERM", stop);
  const loop = () => { try { tick(cfg); } catch (e) { console.error("tick:", e.message); } };
  loop();
  setInterval(loop, POLL_MS);
}

main();
