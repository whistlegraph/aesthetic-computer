#!/usr/bin/env node
// aesel-mcp.mjs — talk to Aesel on AC OS machines from here.
//
// Aesel on a device keeps two files in /tmp: aesel-state.json, what its screen
// says (status, the turn's feed, the last 30 transcript entries, numbered),
// and aesel-say, a line it picks up within a quarter second and submits as if
// typed — prompts and slash commands alike. This server finds the machines on
// the LAN by Bonjour (lanserv's _http._tcp announcement) and reaches those two
// files over ssh.
//
// The images trust one key, neo's (fedac/native/keys/authorized_keys), so the
// ssh runs on the hop (AESEL_SSH_HOP, default neo). A device mints a new host
// key every boot, so there is nothing to pin: the address comes from the
// device's own lanserv answer on the LAN, and the text you send travels on
// stdin, never in a command line.
//
// aesel_flash writes the latest OTA onto a USB stick plugged into this Mac
// with `ac-os pull`. sudo asks through the Slab menubar's passphrase modal
// (slab/bin/ac-askpass as SUDO_ASKPASS), so a flash still goes through you:
// the modal holds the password a day, and this server never sees it.

import { spawn } from "node:child_process";
import { fileURLToPath, pathToFileURL } from "node:url";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const HOP = process.env.AESEL_SSH_HOP || "neo";
const STATE = "/tmp/aesel-state.json";
const SAY = "/tmp/aesel-say";
const IPV4 = /^(25[0-5]|2[0-4]\d|1?\d?\d)(\.(25[0-5]|2[0-4]\d|1?\d?\d)){3}$/;
const NAME = /^[a-z0-9][a-z0-9-]{0,62}$/;
const REPO = fileURLToPath(new URL("../..", import.meta.url));
const AC_OS = `${REPO}fedac/native/ac-os`;
const ASKPASS = `${REPO}slab/bin/ac-askpass`;
const DISK = /^disk([1-9]\d?)$/;
const STICK_MAX = 300e9; // a stick, never a drive

// The inner ssh, as one argument for the hop's shell. Only a validated IPv4
// address and fixed commands are ever spliced in.
export function deviceCommand(ip, command) {
  if (!IPV4.test(ip)) throw Error("not an IPv4 address");
  if (!/^[a-z0-9 ./_>-]+$/.test(command)) throw Error("unexpected device command");
  return `ssh -o BatchMode=yes -o ConnectTimeout=6 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o LogLevel=ERROR root@${ip} '${command}'`;
}

function onDevice(ip, command, input = null) {
  return new Promise((resolve, reject) => {
    const child = spawn("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", HOP, deviceCommand(ip, command)]);
    let out = "", err = "";
    const timer = setTimeout(() => { child.kill(); reject(Error("device ssh timed out")); }, 20000);
    child.stdout.on("data", (d) => { out += d; if (out.length > 1 << 20) child.kill(); });
    child.stderr.on("data", (d) => { err += d; });
    child.on("close", (code) => {
      clearTimeout(timer);
      if (code === 0) resolve(out);
      else reject(Error(err.trim().slice(0, 500) || `device ssh exited ${code}`));
    });
    child.stdin.end(input ?? "");
  });
}

// Bonjour names on the LAN, then each one's own lanserv JSON. Only machines
// that answer with a build are AC OS.
async function discover() {
  const browse = await run("dns-sd", ["-B", "_http._tcp", "local"], 3000);
  const names = [...new Set(browse.split("\n").map((l) => l.trim().split(/\s+/)).filter((p) => p[1] === "Add").map((p) => p.at(-1)))]
    .filter((n) => NAME.test(n));
  // The device's mDNS answers reach dns-sd but not getaddrinfo (fetch and
  // curl time out on `ac11.local`), so resolve with dns-sd and fetch the IP.
  const found = await Promise.all(names.map(async (name) => {
    try {
      const ip = (await run("dns-sd", ["-G", "v4", `${name}.local`], 2500)).match(/\s(\d+\.\d+\.\d+\.\d+)\s/)?.[1];
      if (!ip || !IPV4.test(ip)) return null;
      const res = await fetch(`http://${ip}/`, { signal: AbortSignal.timeout(3000) });
      const s = await res.json();
      return s && s.build && IPV4.test(s.ip) ? { name: s.name || name, ip: s.ip, build: s.build, piece: s.piece, uptime: s.uptime } : null;
    } catch { return null; }
  }));
  return found.filter(Boolean);
}

// dns-sd -B never exits; take what it printed in `ms`.
function run(cmd, args, ms) {
  return new Promise((resolve) => {
    const child = spawn(cmd, args);
    let out = "";
    child.stdout.on("data", (d) => { out += d; });
    setTimeout(() => { child.kill(); resolve(out); }, ms);
    child.on("error", () => resolve(out));
  });
}

async function resolveDevice(device) {
  if (typeof device !== "string" || !device) throw Error("name a device (aesel_devices lists them)");
  if (IPV4.test(device)) return device;
  const all = await discover();
  const hit = all.find((d) => d.name === device);
  if (!hit) throw Error(`no AC OS device named ${device} on the LAN; found: ${all.map((d) => d.name).join(", ") || "none"}`);
  return hit.ip;
}

async function readState(ip) {
  const raw = await onDevice(ip, `cat ${STATE}`);
  try { return JSON.parse(raw); } catch { throw Error("Aesel has written no state yet; is it the running piece?"); }
}

export function since(state, after = 0) {
  const entries = state.entries.filter((e) => !after || e.id > after);
  const { entries: _, ...rest } = state;
  return { ...rest, lastId: state.entries.reduce((m, e) => Math.max(m, e.id || 0), 0), entries };
}


// The disks a flash may touch: whole, external, on USB, stick-sized. A Mac's
// own disk can carry an ACBOOT partition too (frisbee's does), so the label
// alone never qualifies a disk.
export function stickOf(info, partitions = []) {
  if (!info || !DISK.test(info.DeviceIdentifier || "")) return null;
  if (info.Internal !== false || !info.WholeDisk || info.BusProtocol !== "USB") return null;
  if (!(info.TotalSize > 0 && info.TotalSize <= STICK_MAX)) return null;
  return { disk: info.DeviceIdentifier, name: info.MediaName || "", bytes: info.TotalSize, ac: partitions.includes("ACBOOT") };
}

async function plist([verb, ...rest]) {
  const xml = await capture("diskutil", [verb, "-plist", ...rest]);
  return JSON.parse(await capture("plutil", ["-convert", "json", "-o", "-", "-"], xml));
}

async function sticks() {
  const { AllDisksAndPartitions: disks = [] } = await plist(["list", "external", "physical"]);
  const found = await Promise.all(disks.map(async (d) =>
    stickOf(await plist(["info", d.DeviceIdentifier]), (d.Partitions || []).map((p) => p.VolumeName))));
  return found.filter(Boolean);
}

function capture(cmd, args, input = "") {
  return new Promise((resolve, reject) => {
    const child = spawn(cmd, args);
    let out = "", err = "";
    child.stdout.on("data", (d) => { out += d; });
    child.stderr.on("data", (d) => { err += d; });
    child.on("error", reject);
    child.on("close", (code) => code === 0 ? resolve(out) : reject(Error(err.trim() || `${cmd} exited ${code}`)));
    child.stdin.end(input);
  });
}

// One flash at a time; the stick is named by the caller or is the only
// AC stick plugged in.
let flashing = false;
async function flash({ disk, pull = true }) {
  if (flashing) throw Error("a flash is already running");
  const all = await sticks();
  const id = disk ? String(disk).replace(/^\/dev\//, "") : null;
  const target = id ? all.find((s) => s.disk === id) : (all.filter((s) => s.ac).length === 1 ? all.find((s) => s.ac) : null);
  if (!target) {
    const seen = all.map((s) => `${s.disk} ${s.name} ${(s.bytes / 1e9).toFixed(0)} GB${s.ac ? " (AC stick)" : ""}`).join("; ") || "no USB sticks";
    throw Error(id ? `${id} is not a USB stick here; found: ${seen}` : `name the disk to flash; found: ${seen}`);
  }
  flashing = true;
  try {
    const dev = `/dev/${target.disk}`;
    const env = { ...process.env, AC_USB_DEV: dev, SUDO_ASKPASS: ASKPASS, AC_FLASH_YES: "1",
      PATH: `/opt/homebrew/bin:/usr/local/bin:/usr/sbin:/sbin:${process.env.PATH || "/usr/bin:/bin"}` };
    const [cmd, args] = pull ? [AC_OS, ["pull"]] : [`${REPO}fedac/native/scripts/flash-mac.sh`, [dev, "/tmp/ac-os-pull"]];
    const { code, log } = await new Promise((resolve) => {
      const child = spawn(cmd, args, { env, cwd: `${REPO}fedac/native` });
      let log = "";
      const timer = setTimeout(() => child.kill(), 25 * 60 * 1000);
      const take = (d) => { log = (log + d).slice(-200000); };
      child.stdout.on("data", take);
      child.stderr.on("data", take);
      child.on("close", (code) => { clearTimeout(timer); resolve({ code, log }); });
      child.stdin.end();
    });
    // A mistyped password would otherwise sit in the modal's cache all day.
    if (/incorrect password|Sorry, try again|no password was provided/i.test(log))
      await capture("nc", ["-U", `${process.env.HOME}/.ac-daemon.sock`], '{"op":"forget","label":"sudo"}').catch(() => {});
    const lines = log.replace(/\x1b\[[0-9;]*m/g, "").split("\n").filter((l) => l.trim());
    return {
      ok: code === 0 && /\[flash-mac\] Done\./.test(log),
      disk: target.disk, stick: target.name,
      build: log.match(/Latest OTA: (\S+)/)?.[1] || null,
      verified: lines.filter((l) => l.includes("\u2713")).length,
      log: lines.slice(-40).join("\n"),
    };
  } finally { flashing = false; }
}

const device = { type: "string", description: "Device name from aesel_devices (e.g. ac11) or its LAN IPv4 address." };
const tools = [
  { name: "aesel_devices", description: "Find AC OS machines on this LAN by Bonjour: name, IP, build, running piece.",
    inputSchema: { type: "object", properties: {}, additionalProperties: false },
    annotations: { readOnlyHint: true, openWorldHint: true } },
  { name: "aesel_state", description: "Aesel's screen on a device: status, whether a turn is running, the piece file, model, spend, and the last transcript entries (numbered). Pass after=<lastId> for only what is new.",
    inputSchema: { type: "object", properties: { device, after: { type: "integer", minimum: 0 } }, required: ["device"], additionalProperties: false },
    annotations: { readOnlyHint: true, openWorldHint: true } },
  { name: "aesel_say", description: "Type a line into Aesel on a device and submit it, as if typed there: a prompt for the agent, or a slash command (/retry, /new, /piece name, /model flash, /preview). Returns the lastId to wait from.",
    inputSchema: { type: "object", properties: { device, text: { type: "string", minLength: 1, maxLength: 4000 } }, required: ["device", "text"], additionalProperties: false },
    annotations: { readOnlyHint: false, destructiveHint: false, openWorldHint: true } },
  { name: "aesel_wait", description: "Wait (up to timeout seconds) until Aesel's current turn ends, then return the entries after `after`. Use after aesel_say.",
    inputSchema: { type: "object", properties: { device, after: { type: "integer", minimum: 0 }, timeout: { type: "integer", minimum: 5, maximum: 600, default: 180 } }, required: ["device"], additionalProperties: false },
    annotations: { readOnlyHint: true, openWorldHint: true } },
  { name: "aesel_piece", description: "The source of the piece Aesel is writing on a device.",
    inputSchema: { type: "object", properties: { device }, required: ["device"], additionalProperties: false },
    annotations: { readOnlyHint: true, openWorldHint: true } },
  { name: "aesel_sticks", description: "USB sticks plugged into this Mac that a flash could write: disk, name, size, and whether it already holds AC OS.",
    inputSchema: { type: "object", properties: {}, additionalProperties: false },
    annotations: { readOnlyHint: true } },
  { name: "aesel_flash", description: "Erase a USB stick plugged into this Mac and write the latest AC OS OTA onto it (ac-os pull), baked with your login, wifi presets and boot piece. sudo asks through the Slab passphrase modal, cached a day. Without `disk`, flashes the only stick that already holds AC OS. Takes a few minutes; the stick is ejected when done.",
    inputSchema: { type: "object", properties: { disk: { type: "string", pattern: "^(/dev/)?disk[1-9][0-9]?$", description: "Whole-disk identifier from aesel_sticks, e.g. disk6." }, pull: { type: "boolean", default: true, description: "Fetch the latest OTA first (default). false flashes what is already staged in /tmp/ac-os-pull." } }, additionalProperties: false },
    annotations: { readOnlyHint: false, destructiveHint: true, openWorldHint: true } },
];

async function call(name, args) {
  if (name === "aesel_devices") return discover();
  if (name === "aesel_sticks") return sticks();
  if (name === "aesel_flash") return flash(args);
  const ip = await resolveDevice(args.device);
  if (name === "aesel_state") return since(await readState(ip), args.after || 0);
  if (name === "aesel_piece") {
    const state = await readState(ip);
    if (!/^\/pieces\/[a-z0-9-]+\.mjs$/.test(state.piece || "")) throw Error("no piece file in Aesel's state");
    return `// ${state.piece}\n${await onDevice(ip, `cat ${state.piece}`)}`;
  }
  if (name === "aesel_say") {
    const before = since(await readState(ip));
    await onDevice(ip, `cat > ${SAY}`, String(args.text).replace(/\r?\n/g, " ") + "\n");
    return { sent: true, lastId: before.lastId, status: before.status };
  }
  if (name === "aesel_wait") {
    const deadline = Date.now() + (args.timeout || 180) * 1000;
    let state, started = false;
    // A turn can take a moment to begin after aesel_say; count it ended only
    // once it has been seen working, or once new entries have arrived.
    for (;;) {
      state = since(await readState(ip), args.after || 0);
      if (state.working) started = true;
      else if (started || (state.entries.length && state.entries.some((e) => e.kind !== "user"))) break;
      if (Date.now() > deadline) return { ...state, timedOut: true };
      await new Promise((r) => setTimeout(r, 1500));
    }
    return state;
  }
  throw Error("Unknown tool");
}

export async function handleMessage({ id, method, params }) {
  if (id === undefined || id === null) return null;
  const result = (value) => ({ jsonrpc: "2.0", id, result: value });
  if (method === "initialize") return result({ protocolVersion: params?.protocolVersion || "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "aesel", version: "1.0.0" } });
  if (method === "ping") return result({});
  if (method === "tools/list") return result({ tools });
  if (method !== "tools/call") return { jsonrpc: "2.0", id, error: { code: -32601, message: "Method not found" } };
  try {
    if (!tools.some((t) => t.name === params?.name)) throw Error("Unknown tool");
    const value = await call(params.name, params.arguments || {});
    return result({ content: [{ type: "text", text: typeof value === "string" ? value : JSON.stringify(value, null, 2) }] });
  } catch (e) {
    return result({ isError: true, content: [{ type: "text", text: String(e.message || e).slice(0, 2000) }] });
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const port = httpPort(process.argv, 7788);
  if (port) serveHttp({ handleMessage, port, banner: "Aesel on AC OS" });
  else serveStdio({ handleMessage, banner: "Aesel on AC OS" });
}
