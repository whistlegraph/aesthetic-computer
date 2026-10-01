#!/usr/bin/env node
// Reattach status watchers to live wrappers without restarting any Codex TUI.
import { readdirSync, readFileSync } from "node:fs";
import { execFileSync, spawn } from "node:child_process";
import { homedir } from "node:os";
import { join, basename, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const root = process.env.SLAB_HOME || join(homedir(), ".local/share/slab");
const active = join(root, "state/active-prompts");
const watcher = join(dirname(fileURLToPath(import.meta.url)), "codex-session-watch.mjs");
const processes = execFileSync("/bin/ps", ["-axo", "pid=,args="], { encoding: "utf8" })
  .split("\n").map(line => line.trim().split(/\s+/));
let repaired = 0;
for (const sid of readdirSync(active)) {
  try {
    const marker = JSON.parse(readFileSync(join(active, sid), "utf8"));
    if (marker.agent_type !== "codex" || !Number.isInteger(marker.wrapper_pid) || marker.wrapper_pid <= 0) continue;
    process.kill(marker.wrapper_pid, 0);
    for (const [pid, node, script, session] of processes) {
      if (basename(node || "") === "node" && script?.endsWith("/codex-session-watch.mjs") && session === sid) {
        try { process.kill(Number(pid), "SIGTERM"); } catch {}
      }
    }
    spawn(process.execPath, [watcher, sid, String(Math.floor(Date.now()/1000)),
      String(marker.wrapper_pid), marker.tty || "", marker.cwd || ""], {
      detached: true, stdio: "ignore", env: process.env,
    }).unref();
    repaired++;
  } catch { /* Skip stale or partially written markers. */ }
}
console.log(`Reattached ${repaired} Codex status watchers`);
