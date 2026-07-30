#!/usr/bin/env node

import { execFileSync, spawn } from "node:child_process";
import { randomBytes } from "node:crypto";
import { copyFileSync, mkdirSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createDirectorMonitor } from "../lib/director-monitor.mjs";

const args = process.argv.slice(2);
const value = (flag, fallback) => {
  const index = args.indexOf(flag);
  return index < 0 ? fallback : args[index + 1];
};
const port = Number(value("--port", process.env.CAPTUTOR_DIRECTOR_PORT || "47831"));
const host = value("--host", process.env.CAPTUTOR_DIRECTOR_BIND || "0.0.0.0");
const token = value("--token", process.env.CAPTUTOR_DIRECTOR_TOKEN || "");
const overlayBin = join(dirname(fileURLToPath(import.meta.url)), "director-overlay");
let overlay = null;

function openOverlay() {
  overlay = spawn(overlayBin, [`http://127.0.0.1:${port}/state`], { stdio:"ignore" });
  const stop = () => {
    try { overlay?.kill("SIGTERM"); } catch {}
    process.exit(0);
  };
  process.once("SIGTERM", stop);
  process.once("SIGINT", stop);
}

if (args.includes("--install")) {
  const home = homedir();
  const installRoot = join(home, ".local", "lib", "captutor-director");
  const bin = join(installRoot, "director-monitor.mjs");
  const lib = join(installRoot, "director-monitor-lib.mjs");
  const overlaySource = join(installRoot, "director-overlay.swift");
  const installedOverlay = join(installRoot, "director-overlay");
  mkdirSync(installRoot, { recursive:true });
  copyFileSync(fileURLToPath(import.meta.url), bin);
  copyFileSync(fileURLToPath(new URL("../lib/director-monitor.mjs", import.meta.url)), lib);
  copyFileSync(fileURLToPath(new URL("./director-overlay.swift", import.meta.url)), overlaySource);
  execFileSync("/usr/bin/xcrun", ["swiftc", "-O", overlaySource, "-o", installedOverlay]);
  // The installed wrapper imports its sibling rather than a repository path.
  let installed = await import("node:fs/promises").then((fs) => fs.readFile(bin, "utf8"));
  installed = installed.replace('"../lib/director-monitor.mjs"', '"./director-monitor-lib.mjs"');
  writeFileSync(bin, installed, { mode:0o755 });
  const label = "computer.captutor.director-monitor";
  const plist = join(home, "Library", "LaunchAgents", `${label}.plist`);
  mkdirSync(dirname(plist), { recursive:true });
  const installToken = token || randomBytes(24).toString("hex");
  writeFileSync(plist, `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array><string>${process.execPath}</string><string>${bin}</string><string>--serve</string><string>--port</string><string>${port}</string>${args.includes("--no-overlay") ? "" : "<string>--overlay</string>"}</array>
<key>EnvironmentVariables</key><dict><key>CAPTUTOR_DIRECTOR_TOKEN</key><string>${installToken}</string></dict>
<key>RunAtLoad</key><true/><key>KeepAlive</key><true/>
<key>StandardOutPath</key><string>${installRoot}/director.log</string>
<key>StandardErrorPath</key><string>${installRoot}/director.err</string>
</dict></plist>\n`);
  const domain = `gui/${process.getuid()}`;
  try { execFileSync("/bin/launchctl", ["bootout", domain, plist], { stdio:"ignore" }); } catch {}
  execFileSync("/bin/launchctl", ["bootstrap", domain, plist]);
  execFileSync("/bin/launchctl", ["kickstart", "-k", `${domain}/${label}`]);
  console.log(JSON.stringify({ installed:true, plist, url:`http://127.0.0.1:${port}`, tokenConfigured:true }, null, 2));
  process.exit(0);
}

if (args.includes("--help") || args.includes("-h")) {
  console.log("usage: node bin/director-monitor.mjs [--serve] [--overlay] [--host 0.0.0.0] [--port 47831] [--token TOKEN]\n       node bin/director-monitor.mjs --install [--no-overlay] [--token TOKEN]");
  process.exit(0);
}

const { server } = createDirectorMonitor({ token });
server.listen(port, host, () => {
  console.log(`Captutor Director listening on http://${host}:${port}`);
  if (args.includes("--overlay")) openOverlay();
});
