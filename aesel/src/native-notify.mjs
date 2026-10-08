// Native notifications for the Aesel TUI, and its row in the AC network device
// registry (POST /api/app-device).
//
// On macOS a finished turn, a failure or a waiting approval posts a native
// notification, but only while the terminal running Aesel is not the frontmost
// app. When the Aesel Mac app is running, the notification goes through it
// (`aesel://notify`), so it carries Aesel's name and icon; otherwise it falls
// back to `osascript`. Focus is read with `lsappinfo` and the process's parent
// chain, so no Automation or Accessibility permission is needed.
// AESEL_NOTIFY=0 turns notifications off.
import { execFile } from "node:child_process";
import { randomUUID } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { arch, homedir, release } from "node:os";
import { dirname, join } from "node:path";

const GUI_BUNDLE = "computer.aesthetic.easel";
// Resolves stdout, or null when the command failed.
const run = (file, args) => new Promise(resolve =>
  execFile(file, args, { timeout: 3000 }, (error, stdout) => resolve(error ? null : String(stdout))));

async function ancestors(pid = process.pid) {
  const chain = new Set();
  for (let i = 0; i < 24 && pid > 1; i++) {
    chain.add(pid);
    pid = Number((await run("/bin/ps", ["-o", "ppid=", "-p", String(pid)]) || "").trim()) || 0;
  }
  return chain;
}

// True when the frontmost app is this process's terminal (one of its ancestors).
export async function terminalFocused({ lsappinfo = run, chain = ancestors } = {}) {
  const front = (await lsappinfo("/usr/bin/lsappinfo", ["front"]) || "").trim();
  if (!front) return false;
  const pid = Number((await lsappinfo("/usr/bin/lsappinfo", ["info", "-only", "pid", front]) || "").match(/(\d+)/)?.[1]);
  return Number.isInteger(pid) && (await chain()).has(pid);
}

export function notifyURL({ title, body = "", kind = "done" }) {
  const query = new URLSearchParams({ title: title.slice(0, 120), body: body.slice(0, 240), kind, from: "tui" });
  return `aesel://notify?${query}`;
}

export async function notifyNative({ title, body = "", kind = "done" }, { platform = process.platform, env = process.env, exec = run } = {}) {
  if (platform !== "darwin" || env.AESEL_NOTIFY === "0") return "off";
  if (await terminalFocused({ lsappinfo: exec })) return "focused";
  const text = String(body).replace(/\s+/g, " ").trim();
  // An older Aesel.app without the aesel:// scheme makes `open` fail; fall through.
  if ((await exec("/usr/bin/lsappinfo", ["find", `bundleid=${GUI_BUNDLE}`]) || "").trim() &&
      await exec("/usr/bin/open", ["-g", notifyURL({ title, body: text, kind })]) !== null) return "aesel";
  // argv, not string interpolation: titles and replies may contain quotes.
  await exec("/usr/bin/osascript", ["-e", "on run argv", "-e", "display notification (item 2 of argv) with title (item 1 of argv)", "-e", "end run", title, text.slice(0, 240)]);
  return "osascript";
}

// One id per machine for the TUI, kept beside Aesel's other local state.
function deviceId(home = homedir()) {
  const file = join(home, ".config", "aesel", "device-id");
  try { return readFileSync(file, "utf8").trim(); } catch {}
  const id = randomUUID();
  try { mkdirSync(dirname(file), { recursive: true }); writeFileSync(file, id, { mode: 0o600 }); } catch {}
  return id;
}

const PLATFORMS = { darwin: "mac", linux: "linux", win32: "windows" };

export function deviceReport(event, { version, platform = process.platform } = {}) {
  return {
    app: "aesel", deviceId: deviceId(), event, platform: PLATFORMS[platform] || "linux",
    label: "Aesel TUI", os: `${platform} ${release()}`.slice(0, 40), model: arch(),
    ...(version && /^\d+(?:\.\d+){0,3}$/.test(version) ? { version } : {}),
  };
}

// Best effort; a registry outage never touches the terminal.
export async function reportDevice(event, { token, version, fetch = globalThis.fetch } = {}) {
  try {
    await fetch("https://aesthetic.computer/api/app-device", {
      method: "POST", signal: AbortSignal.timeout(8000),
      headers: { "Content-Type": "application/json", ...(token && event !== "logout" ? { Authorization: `Bearer ${token}` } : {}) },
      body: JSON.stringify(deviceReport(event, { version })),
    });
  } catch {}
}
