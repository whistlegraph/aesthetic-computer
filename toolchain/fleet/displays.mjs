#!/usr/bin/env node
import { spawn } from "node:child_process";
import { readFile, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { basename, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { loadRegistry } from "./registry.mjs";
import { addressParts, planLayout, snapshot, validateSeat, seatEdges } from "./display-model.mjs";

export function run(command, args, { input = "", timeout = 15000 } = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { timeout, stdio: ["pipe", "pipe", "pipe"] });
    let stdout = "", stderr = "";
    child.stdout.on("data", d => { stdout += d; });
    child.stderr.on("data", d => { stderr += d; });
    child.stdin.on("error", () => {});
    child.on("error", reject);
    child.on("close", code => code === 0 ? resolve(stdout) : reject(new Error(stderr.trim() || `Command exited ${code}`)));
    child.stdin.end(input);
  });
}

export function displayTargets(names) {
  const { machines } = loadRegistry();
  if (names !== undefined && (!Array.isArray(names) || !names.length)) throw new Error("machines must be a nonempty array");
  const requested = names ?? Object.keys(machines).filter(name => /macos/i.test(machines[name].os || "") && machines[name].tailscale);
  return [...new Set(requested)].map(name => {
    const entry = Object.entries(machines).find(([key, m]) => key === name || m.tailscale?.name === name);
    if (!entry) throw new Error(`Unknown fleet machine: ${name}`);
    const [key, m] = entry;
    if (!/macos/i.test(m.os || "")) throw new Error(`${key} is not a supported Mac`);
    const host = m.ssh?.alias || m.tailscale?.ip || key;
    const target = m.ssh?.alias ? host : m.user ? `${m.user}@${host}` : host;
    if (!/^[a-zA-Z0-9][a-zA-Z0-9.@:-]*$/.test(target)) throw new Error("Invalid registry SSH target");
    return { name: key, localNames: [key, m.tailscale?.name], target };
  });
}
let localName;
export async function native(machine, args, input) {
  localName ??= (await run("/usr/sbin/scutil", ["--get", "LocalHostName"])).trim();
  const options = { input: input === undefined ? "" : JSON.stringify(input), timeout: args[0] === "identify" ? (Number(args[1]) + 10) * 1000 : 15000 };
  let raw;
  if (machine.localNames.includes(localName)) {
    raw = await run(join(homedir(), ".local/bin/slab-displays-native"), args, options);
  } else {
    // Only allowlisted verbs, integers, and generated backup names cross SSH.
    if (!args.every(a => /^[a-zA-Z0-9.-]+$/.test(String(a)))) throw new Error("Invalid native argument");
    raw = await run("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=5", machine.target,
      `.local/bin/slab-displays-native ${args.join(" ")}`], options);
  }
  return JSON.parse(raw);
}

export async function listDisplays({ machines } = {}) {
  const targets = displayTargets(machines);
  const results = await Promise.all(targets.map(async m => {
    try {
      const inv = await native(m, ["list"]);
      return { machine: m.name, ok: true, ...inv, displays: inv.displays.map(d => ({ ...d, address: `${m.name}:${d.number}` })) };
    } catch (e) { return { machine: m.name, ok: false, error: e.message }; }
  }));
  return { version: 1, kind: "fleet-displays", machines: results };
}

export async function identifyDisplays({ machines, address, seconds = 8 } = {}) {
  if (!Number.isFinite(seconds) || seconds < 1 || seconds > 30) throw new Error("seconds must be 1–30");
  if (address && machines) throw new Error("Provide address or machines, not both");
  const parsed = address ? addressParts(address) : null;
  const targets = displayTargets(parsed ? [parsed.machine] : machines);
  return Promise.all(targets.map(async m => {
    try { return { machine: m.name, ok: true, ...await native(m, ["identify", String(seconds), ...(parsed ? [String(parsed.number)] : [])]) }; }
    catch (e) { return { machine: m.name, ok: false, error: e.message }; }
  }));
}

export async function displayModes({ address }) {
  const { machine, number } = addressParts(address);
  return native(displayTargets([machine])[0], ["modes", String(number)]);
}

export async function layoutDisplays({ machine, changes, apply = false, expected } = {}) {
  if (typeof apply !== "boolean") throw new Error("apply must be a boolean");
  if (!Array.isArray(changes) || !changes.length) throw new Error("Provide display changes");
  const target = displayTargets([machine])[0];
  const inv = await native(target, ["list"]);
  const modes = new Map();
  for (const change of changes) if (change.modeID !== undefined) {
    if (!Number.isInteger(change.number) || change.number < 1) throw new Error("Invalid display number");
    modes.set(change.number, await native(target, ["modes", String(change.number)]));
  }
  const request = planLayout(inv, changes, modes);
  if (expected !== undefined) {
    if (!Array.isArray(expected)) throw new Error("expected must be the array returned by a preview");
    const normalized = expected.map(p => ({ uuid: p.uuid, x: p.x, y: p.y, modeID: p.modeID, rotation: p.rotation })).sort((a, b) => String(a.uuid).localeCompare(String(b.uuid)));
    if (JSON.stringify(normalized) !== JSON.stringify(request.expected)) throw new Error("Geometry changed since preview; preview again");
  }
  if (!apply) return { machine: target.name, applied: false, ...request };
  return { machine: target.name, applied: true, ...await native(target, ["apply"], request) };
}

export async function restoreDisplays({ machine, backup, apply = false }) {
  if (typeof apply !== "boolean") throw new Error("apply must be a boolean");
  const name = basename(backup || "");
  if (!/^layout-[A-Fa-f0-9-]{36}\.json$/.test(name)) throw new Error("Expected the backup path returned by apply");
  const target = displayTargets([machine])[0];
  const layout = await native(target, ["saved", name]);
  const inv = await native(target, ["list"]);
  const current = snapshot(inv);
  if (JSON.stringify(layout.map(p => p.uuid).sort()) !== JSON.stringify(current.map(p => p.uuid).sort())) throw new Error("Connected displays differ from the backup");
  const request = { expected: current, layout };
  if (!apply) return { machine: target.name, applied: false, ...request };
  return { machine: target.name, applied: true, ...await native(target, ["apply"], request) };
}

const usage = `displays list [MACHINE ...] [--json]
displays identify [MACHINE ... | MACHINE:NUMBER] [--seconds 8]
displays modes MACHINE:NUMBER
displays set MACHINE:NUMBER [--x N] [--y N] [--mode N] [--apply]
displays layout MACHINE FILE.json [--apply]
displays restore MACHINE BACKUP [--apply]
displays map FILE.json --output FILE.html
displays edges FILE.json

set/layout/restore preview by default. --apply changes the current login session.
Seat maps use their own physical canvas; moving a map rectangle does not move macOS displays.`;

async function main(args) {
  const command = args.shift() ?? "list";
  if (['seat-load', 'seat-apply', 'seat-restore', 'seat-identify'].includes(command)) {
    const seat = await import('./deskflow-seat.mjs');
    const result = command === 'seat-load' ? await seat.loadSeat()
      : command === 'seat-identify' ? await seat.identifySeat()
      : command === 'seat-restore' ? await seat.restoreSeat()
      : await seat.applySeat(JSON.parse(await readFile(args[0], 'utf8')));
    console.log(JSON.stringify(result));
    return;
  }
  const options = {};
  const positional = [];
  const allowed = { list: ["json"], identify: ["seconds"], modes: [], set: ["x", "y", "mode", "apply"], layout: ["apply"], restore: ["apply"], map: ["output"], edges: [] };
  if (["help", "--help", "-h"].includes(command)) { console.log(usage); return; }
  if (!allowed[command]) throw new Error(usage);
  while (args.length) {
    const arg = args.shift();
    if (!arg.startsWith("--")) { positional.push(arg); continue; }
    const key = arg.slice(2);
    if (!allowed[command].includes(key) || key in options) throw new Error(`Unknown or repeated option: ${arg}`);
    options[key] = ["json", "apply"].includes(key) ? true : args.shift();
    if (options[key] === undefined) throw new Error(`Missing value for ${arg}`);
  }
  let result;
  switch (command) {
    case "list":
      result = await listDisplays({ machines: positional.length ? positional : undefined });
      if (!options.json) {
        console.log(result.machines.flatMap(m => m.ok ? m.displays.map(d => `${d.address.padEnd(14)} ${d.name}  ${d.bounds.width}×${d.bounds.height} @ ${d.bounds.x},${d.bounds.y}${d.main ? "  main" : ""}${d.mirrored ? "  mirrored" : ""}`) : [`${m.machine}: ${m.error}`]).join("\n"));
        if (result.machines.some(m => !m.ok)) process.exitCode = 1;
        return;
      }
      break;
    case "identify":
      if (positional.some(p => p.includes(":")) && positional.length !== 1) throw new Error("Identify one address or a list of machines");
      result = await identifyDisplays({ ...(positional[0]?.includes(":") ? { address: positional[0] } : { machines: positional.length ? positional : undefined }), seconds: options.seconds === undefined ? 8 : Number(options.seconds) });
      break;
    case "modes":
      if (positional.length !== 1) throw new Error(usage);
      result = await displayModes({ address: positional[0] }); break;
    case "set": {
      if (positional.length !== 1) throw new Error(usage);
      const { machine, number } = addressParts(positional[0]);
      const change = { number };
      for (const [flag, field] of [["x", "x"], ["y", "y"], ["mode", "modeID"]]) if (options[flag] !== undefined) change[field] = Number(options[flag]);
      if (Object.keys(change).length === 1) throw new Error("set requires --x, --y, or --mode");
      result = await layoutDisplays({ machine, changes: [change], apply: options.apply ?? false }); break;
    }
    case "layout":
      if (positional.length !== 2) throw new Error(usage);
      result = await layoutDisplays({ machine: positional[0], changes: JSON.parse(await readFile(positional[1], "utf8")), apply: options.apply ?? false }); break;
    case "restore":
      if (positional.length !== 2) throw new Error(usage);
      result = await restoreDisplays({ machine: positional[0], backup: positional[1], apply: options.apply ?? false }); break;
    case "map": {
      if (positional.length !== 1 || !options.output) throw new Error(usage);
      const seat = validateSeat(JSON.parse(await readFile(positional[0], "utf8")));
      const template = await readFile(new URL("./display-map.html", import.meta.url), "utf8");
      await writeFile(options.output, template.replace("/*SEAT_DATA*/ null", JSON.stringify(seat).replaceAll("<", "\\u003c")));
      result = { output: resolve(options.output) }; break;
    }
    case "edges":
      if (positional.length !== 1) throw new Error(usage);
      result = seatEdges(validateSeat(JSON.parse(await readFile(positional[0], "utf8"))).screens); break;
  }
  console.log(JSON.stringify(result, null, 2));
  if (result?.machines?.some(m => !m.ok) || (Array.isArray(result) && result.some(m => m.ok === false))) process.exitCode = 1;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main(process.argv.slice(2)).catch(e => { console.error(`displays: ${e.message}`); process.exitCode = 1; });
}
