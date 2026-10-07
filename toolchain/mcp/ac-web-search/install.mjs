#!/usr/bin/env node
// Run locally on each fleet host; never copies credentials to another host.
import { mkdirSync, writeFileSync, existsSync } from "node:fs";
import { homedir } from "node:os";
import { resolve, dirname } from "node:path";
import { execFileSync } from "node:child_process";

const name = "ac-web-search";
const root = resolve(import.meta.dirname, "../../..");
const script = resolve(import.meta.dirname, "server.mjs");
const home = homedir();
const stableNode = resolve(home, ".local/share/fnm/aliases/default/bin/node");
const node = existsSync(stableNode) ? stableNode : process.execPath;
const url = "http://127.0.0.1:7796/mcp";
const stdio = process.argv.includes("--stdio");
const noRegister = process.argv.includes("--no-register");
if (process.argv.slice(2).some(arg => !["--stdio", "--no-register"].includes(arg))) {
  throw new Error("Usage: node install.mjs [--stdio] [--no-register]");
}
const run = (bin, args, optional = false) => {
  try { return execFileSync(bin, args, { cwd: root, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }); }
  catch (error) { if (!optional) throw new Error(`${bin} failed: ${error.stderr || error.message}`); }
};
const xml = value => value.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
const serviceQuote = value => JSON.stringify(value.replaceAll("%", "%%"));

if (!stdio) {
  if (process.platform === "darwin") {
    const label = "computer.aesthetic.ac-web-search-mcp";
    const file = resolve(home, "Library/LaunchAgents", label + ".plist");
    const logDir = resolve(home, "Library/Logs/ac-web-search");
    mkdirSync(dirname(file), { recursive: true });
    mkdirSync(logDir, { recursive: true });
    writeFileSync(file, `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array><string>${xml(node)}</string><string>${xml(script)}</string><string>--http</string><string>7796</string></array>
<key>EnvironmentVariables</key><dict><key>HOME</key><string>${xml(home)}</string></dict>
<key>RunAtLoad</key><true/><key>KeepAlive</key><true/><key>ThrottleInterval</key><integer>5</integer>
<key>StandardOutPath</key><string>${xml(logDir)}/out.log</string>
<key>StandardErrorPath</key><string>${xml(logDir)}/err.log</string>
</dict></plist>\n`);
    const domain = `gui/${process.getuid()}`;
    run("launchctl", ["bootout", `${domain}/${label}`], true);
    let started = false;
    for (let i = 0; i < 10 && !started; i++) {
      try { run("launchctl", ["bootstrap", domain, file]); started = true; }
      catch (error) { if (i === 9) throw error; await new Promise(resolve => setTimeout(resolve, 200)); }
    }
  } else if (process.platform === "linux") {
    const file = resolve(home, ".config/systemd/user/ac-web-search.service");
    mkdirSync(dirname(file), { recursive: true });
    writeFileSync(file, `[Unit]\nDescription=AC web search MCP\nAfter=network-online.target\n[Service]\nExecStart=${serviceQuote(node)} ${serviceQuote(script)} --http 7796\nRestart=on-failure\nRestartSec=5\n[Install]\nWantedBy=default.target\n`);
    run("systemctl", ["--user", "daemon-reload"]);
    run("systemctl", ["--user", "enable", "ac-web-search.service"]);
    run("systemctl", ["--user", "restart", "ac-web-search.service"]);
  } else throw new Error("Use --stdio on this platform.");

  let healthy = false;
  for (let i = 0; i < 20; i++) {
    try {
      const response = await fetch(url, { method: "POST", headers: { "content-type": "application/json" },
        body: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "initialize", params: {} }), signal: AbortSignal.timeout(1000) });
      healthy = (await response.json()).result?.serverInfo?.name === name;
      if (healthy) break;
    } catch {}
    await new Promise(resolve => setTimeout(resolve, 200));
  }
  if (!healthy) throw new Error("AC web search daemon did not answer; agent configuration was not changed.");
  console.log(`AC web search running at ${url}`);
}

if (!noRegister) {
  for (const client of ["codex", "claude"]) {
    if (run(client, ["--version"], true) === undefined) {
      console.log(`${client} not installed; registration skipped.`);
      continue;
    }
    if (client === "codex") {
      run(client, ["mcp", "add", name, ...(stdio ? ["--", node, script] : ["--url", url])]);
    } else {
      // Replace only our own user-scope entry; leave all other servers alone.
      run(client, ["mcp", "remove", name, "--scope", "user"], true);
      run(client, ["mcp", "add", "--scope", "user", "--transport", stdio ? "stdio" : "http", name,
        ...(stdio ? ["--", node, script] : [url])]);
    }
    console.log(`Registered ac-web-search for ${client}.`);
  }
}
