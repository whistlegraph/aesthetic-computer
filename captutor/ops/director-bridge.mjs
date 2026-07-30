#!/usr/bin/env node

import { execFileSync } from "node:child_process";
import { copyFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const home = homedir();
const args = process.argv.slice(2);
const progressPath = join(home, ".local", "share", "desktop-badge", "agent-progress.json");
const envPath = join(home, ".hermes", ".env");

function loadEnv() {
  if (!existsSync(envPath)) return;
  for (const line of readFileSync(envPath, "utf8").split(/\r?\n/)) {
    const index = line.indexOf("=");
    if (index < 1) continue;
    const key = line.slice(0, index);
    if (process.env[key] == null) process.env[key] = line.slice(index + 1);
  }
}

function readProgress() {
  try { return JSON.parse(readFileSync(progressPath, "utf8")); }
  catch { return null; }
}

if (args.includes("--install")) {
  const installRoot = join(home, ".local", "lib", "captutor-director");
  const installed = join(installRoot, "director-bridge.mjs");
  mkdirSync(installRoot, { recursive:true });
  copyFileSync(fileURLToPath(import.meta.url), installed);
  const label = "computer.captutor.director-bridge";
  const plist = join(home, "Library", "LaunchAgents", `${label}.plist`);
  mkdirSync(dirname(plist), { recursive:true });
  writeFileSync(plist, `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array><string>${process.execPath}</string><string>${installed}</string></array>
<key>RunAtLoad</key><true/><key>KeepAlive</key><true/>
<key>StandardOutPath</key><string>${installRoot}/bridge.log</string>
<key>StandardErrorPath</key><string>${installRoot}/bridge.err</string>
</dict></plist>\n`);
  const domain = `gui/${process.getuid()}`;
  try { execFileSync("/bin/launchctl", ["bootout", domain, plist], { stdio:"ignore" }); } catch {}
  execFileSync("/bin/launchctl", ["bootstrap", domain, plist]);
  execFileSync("/bin/launchctl", ["kickstart", "-k", `${domain}/${label}`]);
  console.log(JSON.stringify({ installed:true, plist }, null, 2));
  process.exit(0);
}

loadEnv();
const url = process.env.CAPTUTOR_DIRECTOR_URL || "";
const token = process.env.CAPTUTOR_DIRECTOR_TOKEN || "";
if (!url || !token) throw new Error("CAPTUTOR_DIRECTOR_URL and CAPTUTOR_DIRECTOR_TOKEN are required");

let sequence = 0;
let busy = false;
async function publish() {
  if (busy) return;
  busy = true;
  try {
    const progress = readProgress();
    if (!progress) return;
    // Captutor owns the overlay while its measured beat heartbeat is fresh.
    // Iris resumes automatically if the take exits or Panda reconnects.
    const current = await fetch(url).then((response) => response.ok ? response.json() : null).catch(() => null);
    const age = Date.now() - Date.parse(current?.updatedAt || 0);
    if (current?.source === "captutor" && current.status === "recording" && age < 7_000) return;
    const active = Array.isArray(progress.active) ? progress.active.filter(Boolean) : [];
    const currentLine = String(progress.activity || active[0] || "Iris is ready.");
    const nextLine = active.find((line) => line !== currentLine) || "";
    const state = {
      schema:"captutor-director-state/v1",
      source:"iris",
      sourceUpdatedAt:progress.updatedAt || null,
      goal:String(progress.mission || "Fuser tutorials"),
      taskGid:String(progress.taskGid || ""),
      screenplay:"",
      locale:"en",
      format:"",
      phase:String(progress.phase || progress.state || "working").toLowerCase(),
      status:String(progress.state || "working").toLowerCase(),
      beatIndex:null,
      beatCount:0,
      currentLine,
      nextLine,
      words:[],
      beatStartedAt:null,
      updatedAt:new Date().toISOString(),
      sequence:++sequence,
    };
    await fetch(url, {
      method:"POST",
      headers:{ "Content-Type":"application/json", Authorization:`Bearer ${token}` },
      body:JSON.stringify(state),
      signal:AbortSignal.timeout(1_500),
    }).catch(() => null);
  } finally {
    busy = false;
  }
}

await publish();
setInterval(publish, 2_500);
