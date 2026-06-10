#!/usr/bin/env node
// asana.mjs — a tiny Asana bridge for the slab menubar.
//
// Reports the signed-in user's INCOMPLETE assigned tasks, grouped by project,
// as JSON the Swift menubar renders into an `Asana` submenu (mirrors `imsg`).
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// personal data: the Personal Access Token (and optional workspace pin) live
// in an UNTRACKED config at ~/.config/slab/asana.json (see `asana config`).
//
// Create a PAT at https://app.asana.com/0/my-apps as the account you want
// the menubar to mirror (here: jeffrey@fuser.studio).
//
// Subcommands:
//   asana status [--machine <name>]   JSON summary to stdout (default)
//   asana task <gid>                  full detail for one task (incl. notes)
//   asana comment <gid> <text…>       post a comment (story) on a task
//   asana config                      print/create the config stub path
//   asana open                        open Asana "My Tasks" in the browser
//   asana workspaces                  list the account's workspaces (gid + name)
//
// Machines: an Asana tag whose name matches an entry in the config's
// `machines` list routes that task to that machine. `--machine <name>`
// filters status to that machine's tasks; untagged tasks route to the
// config's `defaultMachine`. Machine names live ONLY in the untracked
// config, never in this file.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";

const HOME = homedir();
const CONFIG_PATH =
  process.env.SLAB_ASANA_CONFIG ||
  join(HOME, ".config", "slab", "asana.json");
const API = "https://app.asana.com/api/1.0";

// ─── config ──────────────────────────────────────────────────────────────

const CONFIG_STUB = {
  _README:
    "slab asana config — UNTRACKED, never committed. Paste a Personal " +
    "Access Token below (create one at https://app.asana.com/0/my-apps).",
  token: "REPLACE_ME",
  workspace: "",
  workspaceComment:
    "optional: pin a workspace gid (run `asana workspaces` to list). " +
    "Empty = use the account's first/default workspace.",
  machines: [],
  machinesComment:
    "optional: machine names — an Asana tag matching one routes the task " +
    "to that machine (`asana status --machine <name>`). Untagged tasks " +
    "route to defaultMachine.",
  defaultMachine: "",
};

function loadConfig() {
  if (!existsSync(CONFIG_PATH)) return null;
  try {
    const cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
    if (!cfg || !cfg.token || /REPLACE_ME/.test(cfg.token)) return null;
    return cfg;
  } catch {
    return null;
  }
}

function writeStub() {
  mkdirSync(dirname(CONFIG_PATH), { recursive: true });
  if (!existsSync(CONFIG_PATH)) {
    writeFileSync(CONFIG_PATH, JSON.stringify(CONFIG_STUB, null, 2) + "\n");
  }
}

// ─── Asana REST ────────────────────────────────────────────────────────────

async function api(cfg, path, init = {}) {
  const res = await fetch(`${API}${path}`, {
    method: init.method || "GET",
    headers: {
      Authorization: `Bearer ${cfg.token}`,
      ...(init.body ? { "Content-Type": "application/json" } : {}),
    },
    body: init.body ? JSON.stringify({ data: init.body }) : undefined,
  });
  if (!res.ok) {
    const body = await res.text().catch(() => "");
    const err = new Error(`asana ${res.status}`);
    err.status = res.status;
    err.body = body;
    throw err;
  }
  return (await res.json()).data;
}

// ─── machines ──────────────────────────────────────────────────────────────
// A task's machines = its Asana tags that match a configured machine name
// (case-insensitive). Tasks with no machine tag belong to `defaultMachine`.

function machinesOf(cfg, task) {
  const known = new Set((cfg.machines || []).map((m) => m.toLowerCase()));
  return (task.tags || [])
    .map((t) => (t.name || "").toLowerCase())
    .filter((n) => known.has(n));
}

// ─── status ────────────────────────────────────────────────────────────────

async function status(machine) {
  const cfg = loadConfig();
  if (!cfg) {
    return { configured: false, label: "Asana: setup" };
  }

  let me;
  try {
    me = await api(cfg, "/users/me?opt_fields=name,workspaces.name");
  } catch (e) {
    const tag = e.status === 401 ? "auth" : "error";
    return { configured: true, label: `Asana: ${tag}`, error: e.body || tag };
  }

  const workspace =
    cfg.workspace || (me.workspaces && me.workspaces[0] && me.workspaces[0].gid);
  if (!workspace) {
    return { configured: true, label: "Asana: no workspace" };
  }

  // Incomplete tasks assigned to me. `completed_since=now` filters to
  // not-yet-completed tasks; we pull the project memberships to group on.
  const fields =
    "name,due_on,permalink_url,projects.name,assignee_status,tags.name";
  let tasks;
  try {
    tasks = await api(
      cfg,
      `/tasks?assignee=me&workspace=${workspace}` +
        `&completed_since=now&opt_fields=${encodeURIComponent(fields)}&limit=100`,
    );
  } catch (e) {
    return { configured: true, label: "Asana: error", error: e.body };
  }

  // Optional machine filter: tasks tagged for the machine, plus (when the
  // machine is the configured default) every untagged task.
  if (machine) {
    const m = machine.toLowerCase();
    const def = (cfg.defaultMachine || "").toLowerCase();
    tasks = tasks.filter((t) => {
      const ms = machinesOf(cfg, t);
      return ms.length ? ms.includes(m) : m === def;
    });
  }

  // Today, as YYYY-MM-DD in local time, for overdue/today flags.
  const now = new Date();
  const today = `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(
    2,
    "0",
  )}-${String(now.getDate()).padStart(2, "0")}`;

  // Group by project. A task may belong to several projects; we file it under
  // each so it shows wherever you'd look. Projectless tasks go under "Inbox".
  const groups = new Map(); // name -> { name, url, tasks[] }
  const bucket = (name) => {
    if (!groups.has(name)) groups.set(name, { name, tasks: [] });
    return groups.get(name);
  };

  for (const t of tasks) {
    const entry = {
      name: t.name || "(untitled)",
      gid: t.gid,
      url: t.permalink_url || "",
      due: t.due_on || null,
      overdue: t.due_on ? t.due_on < today : false,
      today: t.due_on === today,
      machines: machinesOf(cfg, t),
    };
    const projects = (t.projects && t.projects.length && t.projects) || [
      { name: "Inbox" },
    ];
    for (const p of projects) bucket(p.name || "Inbox").tasks.push(entry);
  }

  // Sort: projects alphabetically; within a project, overdue first then by
  // due date (undated last), then name.
  const projects = [...groups.values()].sort((a, b) =>
    a.name.localeCompare(b.name),
  );
  for (const p of projects) {
    p.tasks.sort((a, b) => {
      const ad = a.due || "9999-99-99";
      const bd = b.due || "9999-99-99";
      if (ad !== bd) return ad < bd ? -1 : 1;
      return a.name.localeCompare(b.name);
    });
  }

  return {
    configured: true,
    user: me.name || "",
    workspace,
    machine: machine || null,
    count: tasks.length,
    label: `Asana: ${tasks.length}`,
    projects,
    updated: now.toISOString(),
  };
}

// ─── task / comment ──────────────────────────────────────────────────────────

async function taskDetail(gid) {
  const cfg = loadConfig();
  if (!cfg) return { configured: false };
  const fields =
    "name,notes,due_on,permalink_url,completed,assignee.name," +
    "projects.name,tags.name";
  const t = await api(cfg, `/tasks/${gid}?opt_fields=${encodeURIComponent(fields)}`);
  return {
    gid: t.gid,
    name: t.name || "",
    notes: t.notes || "",
    due: t.due_on || null,
    url: t.permalink_url || "",
    completed: !!t.completed,
    assignee: (t.assignee && t.assignee.name) || "",
    projects: (t.projects || []).map((p) => p.name),
    tags: (t.tags || []).map((x) => x.name),
    machines: machinesOf(cfg, t),
  };
}

async function comment(gid, text) {
  const cfg = loadConfig();
  if (!cfg) return { configured: false };
  const story = await api(cfg, `/tasks/${gid}/stories`, {
    method: "POST",
    body: { text },
  });
  return { ok: true, gid, story: story.gid };
}

// ─── workspaces ──────────────────────────────────────────────────────────────

async function workspaces() {
  const cfg = loadConfig();
  if (!cfg) return { configured: false };
  const me = await api(cfg, "/users/me?opt_fields=name,workspaces.name");
  return {
    user: me.name,
    workspaces: (me.workspaces || []).map((w) => ({
      gid: w.gid,
      name: w.name,
    })),
  };
}

// ─── main ──────────────────────────────────────────────────────────────────

const argv = process.argv.slice(2);
const flags = {};
const pos = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const eq = a.indexOf("=");
    if (eq > -1) flags[a.slice(2, eq)] = a.slice(eq + 1);
    else flags[a.slice(2)] = argv[++i];
  } else pos.push(a);
}
const cmd = pos[0] || "status";

if (cmd === "config") {
  writeStub();
  console.log(CONFIG_PATH);
  process.exit(0);
}

if (cmd === "open") {
  spawnSync("/usr/bin/open", ["https://app.asana.com/0/my-tasks"]);
  process.exit(0);
}

try {
  if (cmd === "workspaces") {
    console.log(JSON.stringify(await workspaces(), null, 2));
  } else if (cmd === "task") {
    if (!pos[1]) throw new Error("usage: asana task <gid>");
    console.log(JSON.stringify(await taskDetail(pos[1]), null, 2));
  } else if (cmd === "comment") {
    if (!pos[1] || !pos[2]) throw new Error("usage: asana comment <gid> <text…>");
    console.log(JSON.stringify(await comment(pos[1], pos.slice(2).join(" "))));
  } else {
    console.log(JSON.stringify(await status(flags.machine)));
  }
} catch (e) {
  console.log(
    JSON.stringify({ configured: true, label: "Asana: error", error: String(e) }),
  );
}
