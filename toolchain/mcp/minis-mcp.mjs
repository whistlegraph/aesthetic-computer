#!/usr/bin/env node
// minis-mcp.mjs — auto-toggle heavyweight chrome-devtools MCP servers.
//
// Every stdio MCP server spawns per Claude session, so an always-on
// chrome-devtools entry costs a server process in every parallel session
// whether or not the session touches a browser. This hook keeps them
// disabled by default and enables them by mention, per group:
//   minis    (chicken/panda)  — keyed on fuser/chicken/panda/minis
//   browser  (local devtools) — keyed on browser/chrome/devtools/…
//
// Claude Code hook, two modes:
//   prompt         (UserPromptSubmit) — if the prompt matches a group's
//                  keywords, remove that group's servers from this project's
//                  disabledMcpServers in ~/.claude.json and stamp the
//                  group's activity file. New sessions then connect them.
//   session-start  (SessionStart) — re-disable any group whose stamp is
//                  missing or older than EXPIRY_HOURS (work has wrapped).
//
// MCP servers only connect at session start, so an enable mid-session takes
// effect on the NEXT session — the hook tells Claude this via
// additionalContext so it can say so instead of hunting for missing tools.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const GROUPS = [
  {
    servers: ["chrome-devtools-chicken", "chrome-devtools-panda"],
    keywords: /\b(fuser|chicken|panda|minis)\b/i,
    stamp: "minis-mcp-active",
    emoji: "🐔🐼",
    label: "minis DevTools MCP",
  },
  {
    servers: ["chrome-devtools"],
    keywords: /\b(browser|chrome|devtools|screenshot|lighthouse|webpage|web page|headless)\b/i,
    stamp: "chrome-mcp-active",
    emoji: "🌐",
    label: "local DevTools MCP",
  },
];
const EXPIRY_HOURS = 12;
const CLAUDE_JSON = path.join(os.homedir(), ".claude.json");
const stampPath = (group) => path.join(os.homedir(), ".claude", group.stamp);

const mode = process.argv[2];
const stdin = fs.readFileSync(0, "utf8");
let input = {};
try { input = JSON.parse(stdin || "{}"); } catch { /* tolerate empty stdin */ }

// Portable across dev machines: the project key in ~/.claude.json is the
// repo's absolute path on THIS machine.
const PROJECT = process.env.CLAUDE_PROJECT_DIR || input.cwd || process.cwd();

function readConfig() {
  return JSON.parse(fs.readFileSync(CLAUDE_JSON, "utf8"));
}
function writeConfig(cfg) {
  fs.writeFileSync(CLAUDE_JSON, JSON.stringify(cfg, null, 2));
}
function disabledList(cfg) {
  const proj = cfg.projects?.[PROJECT];
  if (!proj) return null;
  proj.disabledMcpServers ??= [];
  return proj.disabledMcpServers;
}
function out(obj) { console.log(JSON.stringify(obj)); }

// No-op on machines where none of the servers are configured (and on
// fresh machines with no ~/.claude.json at all).
let config;
try { config = readConfig(); } catch { process.exit(0); }
const groups = GROUPS.filter((g) => g.servers.some((s) => config.mcpServers?.[s]));
if (groups.length === 0) process.exit(0);

if (mode === "prompt") {
  const enabled = [];
  for (const g of groups) {
    if (!g.keywords.test(input.prompt || "")) continue;
    fs.writeFileSync(stampPath(g), String(Date.now()));
    const dis = disabledList(config);
    if (!dis) continue;
    const before = dis.length;
    config.projects[PROJECT].disabledMcpServers = dis.filter((s) => !g.servers.includes(s));
    if (config.projects[PROJECT].disabledMcpServers.length === before) continue; // already enabled
    enabled.push(g);
  }
  if (enabled.length === 0) process.exit(0);
  writeConfig(config);
  const names = enabled.flatMap((g) => g.servers).join(", ");
  out({
    systemMessage: `${enabled.map((g) => g.emoji).join("")} ${enabled
      .map((g) => g.label)
      .join(" + ")} enabled (auto-expires after ${EXPIRY_HOURS}h without a mention) — connects on next session`,
    hookSpecificOutput: {
      hookEventName: "UserPromptSubmit",
      additionalContext:
        `minis-mcp hook: ${names} were just re-enabled for this project because the prompt matched their keywords. MCP servers connect at session START, so their tools are NOT available in this session — do not search for them. If the task needs those browsers now, tell the user to restart the session (or run /mcp) to connect them.`,
    },
  });
} else if (mode === "session-start") {
  const expired = [];
  for (const g of groups) {
    let fresh = false;
    try {
      fresh = Date.now() - Number(fs.readFileSync(stampPath(g), "utf8")) < EXPIRY_HOURS * 3600 * 1000;
    } catch { /* no stamp → not fresh */ }
    if (fresh) continue;
    const dis = disabledList(config);
    if (!dis) continue;
    const missing = g.servers.filter((s) => !dis.includes(s));
    if (missing.length === 0) continue; // already disabled
    dis.push(...missing);
    fs.rmSync(stampPath(g), { force: true });
    expired.push(g);
  }
  if (expired.length === 0) process.exit(0);
  writeConfig(config);
  out({
    systemMessage: `${expired.map((g) => g.emoji).join("")} ${expired
      .map((g) => g.label)
      .join(" + ")} auto-disabled (idle > ${EXPIRY_HOURS}h)`,
  });
} else {
  console.error("usage: minis-mcp.mjs prompt|session-start");
  process.exit(1);
}
