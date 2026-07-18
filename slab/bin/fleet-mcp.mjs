#!/usr/bin/env node
// fleet-mcp.mjs — set screen brightness and output volume across every fleet
// Mac at once, from any agent session. This is the programmatic sibling of the
// slab menubar's ⌃⌥⌘A dark-mode fan-out (AppDelegate.applyAppearance): same
// idea — one command, all four Macs — but absolute 0–100 levels instead of a
// boolean, and reachable as MCP tools rather than only a global hotkey.
//
// Brightness is NOT uniform across the fleet, so each host declares its `kind`:
//   • builtin (neo, blueberry — MacBooks): the Homebrew `brightness` CLI FAILS
//     on Apple Silicon built-in panels (error -536870201) and AppleScript has
//     no brightness verb, so we use `acbright` — a tiny DisplayServices helper
//     (acbright.swift, sibling file) compiled to ~/.local/bin/acbright.
//   • ddc (panda, chicken — Mac minis): no built-in panel; the external monitor
//     is driven over DDC/CI with `m1ddc set luminance N`.
// Volume IS uniform — `osascript -e 'set volume output volume N'` everywhere.
//
// Transport per host mirrors the dark-mode fan-out: the process runs the
// command locally on whichever Mac it's on (matched by LocalHostName) and
// SSHes to the other three by their ssh-config alias. Hand-rolled JSON-RPC over
// stdio, same house style as frame-mcp / puppet-mcp — node builtins only.
import { spawn } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ACBRIGHT_SRC = join(HERE, "acbright.swift");

// The four Macs. `host` is the ssh-config alias used from any OTHER fleet Mac
// (blueberry/neo both already have these aliases); `self` is the LocalHostName
// that means "this is me, run locally, skip ssh". `kind` picks the brightness
// mechanism. Keep this list in sync with the menubar's appearanceHosts.
const FLEET = [
  { name: "neo",       host: "neo",       self: "neo",       kind: "builtin" },
  { name: "blueberry", host: "blueberry", self: "blueberry", kind: "builtin" },
  { name: "panda",     host: "panda",     self: "panda",     kind: "ddc" },
  { name: "chicken",   host: "chicken",   self: "chicken",   kind: "ddc" },
];

const SSH_OPTS = ["-o", "ConnectTimeout=6", "-o", "BatchMode=yes"];
const HOMEBREW = "/opt/homebrew/bin"; // where m1ddc lives; not always on ssh PATH

let localHostName = null; // resolved once, lazily
function getLocalHostName() {
  if (localHostName !== null) return Promise.resolve(localHostName);
  return run("scutil", ["--get", "LocalHostName"])
    .then((out) => (localHostName = out.trim()))
    .catch(() => (localHostName = ""));
}

// Run a command, resolve trimmed stdout or reject with stderr. `input` is fed
// to stdin. No shell here — args are exec'd directly.
function run(cmd, args, { timeoutMs = 20000, input } = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(cmd, args, { timeout: timeoutMs });
    let out = "", err = "";
    child.stdout.on("data", (d) => (out += d));
    child.stderr.on("data", (d) => (err += d));
    child.on("error", (e) => reject(e));
    child.on("close", (code) => {
      if (code === 0) resolve(out);
      else reject(new Error((err || out || `exit ${code}`).trim()));
    });
    if (input != null) child.stdin.end(input);
    else child.stdin.end();
  });
}

// Run `script` on a host under a login bash reading from stdin (`bash -ls`):
// locally if it's us, else over ssh. Feeding the script via stdin makes it
// shell-AGNOSTIC — the remote login shell only ever sees the tokens
// `bash -ls`, so a fish-login host (neo) runs the same bash script as the
// others, with zero nested quoting. `-l` puts Homebrew on PATH for m1ddc.
async function onHost(m, script) {
  const self = await getLocalHostName();
  if (m.self === self) return run("bash", ["-ls"], { input: script });
  return run("ssh", [...SSH_OPTS, m.host, "bash", "-ls"], { input: script });
}

// The remote command that ensures acbright exists (compiling from the deployed
// source if missing) and then invokes it. Built-in hosts have swiftc (Command
// Line Tools) and the repo checked out, but we don't depend on the repo path:
// the source is copied to ~/.local/bin on first build and cached thereafter.
function acbrightCmd(argPart) {
  return [
    'B="$HOME/.local/bin/acbright"',
    '[ -x "$B" ] || { mkdir -p "$HOME/.local/bin"; ' +
      `swiftc "${remoteSrcPath()}" -O -o "$B" 2>/dev/null || swiftc /tmp/acbright.swift -O -o "$B"; }`,
    `"$B" ${argPart}`,
  ].join("; ");
}
// acbright.swift is expected at ~/.local/bin/acbright.swift on built-in hosts
// (staged next to the binary). Falls back to /tmp/acbright.swift if a deploy
// dropped it there. The MCP never has to know the repo layout on the far end.
function remoteSrcPath() {
  return "$HOME/.local/bin/acbright.swift";
}

async function setBrightnessOn(m, level) {
  if (m.kind === "builtin") {
    const out = await onHost(m, acbrightCmd(String(level)));
    return { host: m.name, ok: true, value: Number(out.trim()) || level };
  }
  // ddc — m1ddc set luminance (needs Homebrew on PATH; login shell provides it)
  await onHost(m, `PATH=${HOMEBREW}:$PATH m1ddc set luminance ${level}`);
  return { host: m.name, ok: true, value: level };
}

async function setVolumeOn(m, { level, mute }) {
  const parts = [];
  if (typeof level === "number") parts.push(`set volume output volume ${level}`);
  if (typeof mute === "boolean") parts.push(`set volume output muted ${mute ? "true" : "false"}`);
  await onHost(m, `osascript ${parts.map((p) => `-e '${p}'`).join(" ")}`);
  const value = level !== undefined ? `${level}%` : mute ? "muted" : "unmuted";
  return { host: m.name, ok: true, value };
}

// Which machines a call targets: the `machines` arg (subset) or all four.
function resolve(machines) {
  if (!machines || !machines.length) return FLEET;
  const want = new Set(machines.map((s) => s.toLowerCase()));
  const picked = FLEET.filter((m) => want.has(m.name));
  const unknown = [...want].filter((w) => !FLEET.some((m) => m.name === w));
  if (unknown.length) throw new Error(`unknown machine(s): ${unknown.join(", ")} — fleet is ${FLEET.map((m) => m.name).join(", ")}`);
  return picked;
}

// Fan out `fn` over the targets in parallel; never reject — collect per-host
// ok/err so one asleep or offline Mac can't sink the whole call.
async function fanOut(targets, fn) {
  const results = await Promise.all(
    targets.map((m) => fn(m).catch((e) => ({ host: m.name, ok: false, error: String(e.message || e).trim() }))),
  );
  const lines = results.map((r) => (r.ok ? `  ✓ ${r.host} → ${r.value}` : `  ✗ ${r.host}: ${r.error}`));
  const okN = results.filter((r) => r.ok).length;
  return { text: `${okN}/${results.length} ok\n${lines.join("\n")}`, results };
}

async function toolBrightness({ level, machines } = {}) {
  if (typeof level !== "number" || level < 0 || level > 100) throw new Error("`level` must be a number 0–100");
  const { text } = await fanOut(resolve(machines), (m) => setBrightnessOn(m, Math.round(level)));
  return [{ type: "text", text: `brightness → ${Math.round(level)}%\n${text}` }];
}

async function toolVolume({ level, mute, machines } = {}) {
  if (level === undefined && mute === undefined) throw new Error("provide `level` (0–100) and/or `mute`");
  if (level !== undefined && (typeof level !== "number" || level < 0 || level > 100)) throw new Error("`level` must be 0–100");
  const { text } = await fanOut(resolve(machines), (m) => setVolumeOn(m, { level: level === undefined ? undefined : Math.round(level), mute }));
  const label = [level !== undefined ? `volume → ${Math.round(level)}%` : null, mute !== undefined ? `muted=${mute}` : null].filter(Boolean).join(", ");
  return [{ type: "text", text: `${label}\n${text}` }];
}

async function toolList() {
  const self = await getLocalHostName();
  const lines = FLEET.map((m) => `  ${m.name}${m.self === self ? " (this host)" : ""} — brightness:${m.kind}, ssh:${m.host}`);
  return [{ type: "text", text: `fleet (${FLEET.length} Macs):\n${lines.join("\n")}` }];
}

const TOOLS = [
  {
    name: "fleet_brightness",
    description:
      "Set screen brightness on the fleet Macs (neo, blueberry, panda, chicken) at once, to an absolute level 0–100. Built-in laptop panels (neo, blueberry) use the acbright DisplayServices helper; the Mac minis (panda, chicken) drive their external monitors over DDC/CI. Fans out in parallel and reports per-host success; an offline/asleep Mac just fails its line. Optionally target a subset with `machines`.",
    inputSchema: {
      type: "object",
      properties: {
        level: { type: "number", description: "Brightness percent, 0–100 (applied to every targeted Mac)." },
        machines: { type: "array", items: { type: "string" }, description: "Optional subset, e.g. [\"panda\",\"chicken\"]. Omit for all four." },
      },
      required: ["level"],
    },
  },
  {
    name: "fleet_volume",
    description:
      "Set output volume and/or mute on the fleet Macs (neo, blueberry, panda, chicken) at once. `level` is 0–100; `mute` is a boolean. Uniform across the fleet via osascript. Fans out in parallel with per-host results. Optionally target a subset with `machines`.",
    inputSchema: {
      type: "object",
      properties: {
        level: { type: "number", description: "Output volume percent, 0–100." },
        mute: { type: "boolean", description: "Mute (true) or unmute (false) output." },
        machines: { type: "array", items: { type: "string" }, description: "Optional subset. Omit for all four." },
      },
    },
  },
  {
    name: "fleet_list",
    description: "List the fleet Macs this server controls, their brightness mechanism (builtin/ddc), ssh alias, and which one is the local host.",
    inputSchema: { type: "object", properties: {} },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "fleet_brightness": return toolBrightness(args || {});
    case "fleet_volume": return toolVolume(args || {});
    case "fleet_list": return toolList();
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "fleet-mcp", version: "1.0.0" } } };
      case "initialized":
      case "notifications/initialized":
        return null;
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7770); // 7767 frame, 7769 puppet, 7768 Spotify
if (port) serveHttp({ handleMessage, port, banner: "🔆 fleet-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🔆 fleet-mcp server started (fleet_brightness, fleet_volume, fleet_list)" });
