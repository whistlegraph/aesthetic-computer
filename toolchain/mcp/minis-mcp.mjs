#!/usr/bin/env node
// minis-mcp.mjs — auto-toggle the minis' chrome-devtools MCP servers.
//
// Claude Code hook, two modes:
//   prompt         (UserPromptSubmit) — if the prompt mentions fuser/chicken/
//                  panda/minis, remove both servers from this project's
//                  disabledMcpServers in ~/.claude.json and stamp the activity
//                  file. New sessions then connect them.
//   session-start  (SessionStart) — if the activity stamp is missing or older
//                  than EXPIRY_HOURS, re-add both servers to
//                  disabledMcpServers (work has wrapped).
//
// MCP servers only connect at session start, so an enable mid-session takes
// effect on the NEXT session — the hook tells Claude this via
// additionalContext so it can say so instead of hunting for missing tools.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const SERVERS = ["chrome-devtools-chicken", "chrome-devtools-panda"];
const KEYWORDS = /\b(fuser|chicken|panda|minis)\b/i;
const EXPIRY_HOURS = 12;
const CLAUDE_JSON = path.join(os.homedir(), ".claude.json");
const STAMP = path.join(os.homedir(), ".claude", "minis-mcp-active");

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

// No-op on machines where the minis' servers aren't configured (and on
// fresh machines with no ~/.claude.json at all).
let config;
try { config = readConfig(); } catch { process.exit(0); }
if (!SERVERS.some((s) => config.mcpServers?.[s])) process.exit(0);

if (mode === "prompt") {
  if (!KEYWORDS.test(input.prompt || "")) process.exit(0);
  fs.writeFileSync(STAMP, String(Date.now()));
  const cfg = config;
  const dis = disabledList(cfg);
  if (!dis) process.exit(0);
  const before = dis.length;
  cfg.projects[PROJECT].disabledMcpServers = dis.filter((s) => !SERVERS.includes(s));
  if (cfg.projects[PROJECT].disabledMcpServers.length === before) process.exit(0); // already enabled
  writeConfig(cfg);
  out({
    systemMessage: `🐔🐼 minis DevTools MCP enabled (auto-expires after ${EXPIRY_HOURS}h without a fuser/minis mention) — connects on next session`,
    hookSpecificOutput: {
      hookEventName: "UserPromptSubmit",
      additionalContext:
        "minis-mcp hook: chrome-devtools-chicken and chrome-devtools-panda were just re-enabled for this project because the prompt mentions fuser/the minis. MCP servers connect at session START, so their tools are NOT available in this session — do not search for them. If the task needs the minis' browsers now, tell the user to restart the session (or run /mcp) to connect them.",
    },
  });
} else if (mode === "session-start") {
  let fresh = false;
  try {
    fresh = Date.now() - Number(fs.readFileSync(STAMP, "utf8")) < EXPIRY_HOURS * 3600 * 1000;
  } catch { /* no stamp → not fresh */ }
  if (fresh) process.exit(0);
  const cfg = config;
  const dis = disabledList(cfg);
  if (!dis) process.exit(0);
  const missing = SERVERS.filter((s) => !dis.includes(s));
  if (missing.length === 0) process.exit(0); // already disabled
  dis.push(...missing);
  writeConfig(cfg);
  fs.rmSync(STAMP, { force: true });
  out({ systemMessage: "🐔🐼 minis DevTools MCP auto-disabled (idle > " + EXPIRY_HOURS + "h)" });
} else {
  console.error("usage: minis-mcp.mjs prompt|session-start");
  process.exit(1);
}
