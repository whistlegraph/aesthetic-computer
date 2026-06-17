#!/usr/bin/env node
// frame.mjs — capture a rich "frame" of a remote Mac for fleet automation.
//
// A frame = pixels (a downscaled JPEG thumbnail) + OCR'd text with click
// coordinates + the Accessibility element tree (roles/titles/AXPress targets)
// + window/cursor/frontmost state, packed into one JSON envelope. It is the
// native-capture complement to `puppet`: `frame` OBSERVES the whole screen
// (any native app, no DOM needed), `puppet` ACTS (trusted stroke/gesture/key).
// Together they close an observe→act loop across the fleet.
//
// The capture is produced ON the target by the SlabMenubar app (see
// FrameCapture.swift) — it already holds Accessibility trust and lives in the
// GUI session, so it can reach the WindowServer that a plain ssh session
// cannot. This CLI just drops a request file over SSH and reads the result.
//
// PERMISSIONS ARE LAZY: the app never prompts at launch. The first real frame
// request is what triggers the Screen Recording grant on the target; until
// granted, the envelope reports `capture: "permission_needed"` and `frame
// setup <machine>` walks you through the one-time toggle.
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// machine names: the registry lives in the UNTRACKED config shared with
// puppet at ~/.config/slab/puppet.json. A machine's name doubles as its ssh
// host (the minis are ssh aliases); set "sshHost" per machine to override.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";

const HOME = process.env.HOME;
const CONFIG_PATH =
  process.env.SLAB_PUPPET_CONFIG || join(HOME, ".config", "slab", "puppet.json");
const FRAMES_DIR = join(HOME, ".local", "share", "slab", "frames");
const APP_BUNDLE = "computer.slab.menubar";
const REMOTE_STATE = "~/.local/share/slab/state"; // expanded by the remote shell

function loadMachines() {
  if (!existsSync(CONFIG_PATH)) {
    console.error(`no config at ${CONFIG_PATH} — register machines (puppet config)`);
    process.exit(1);
  }
  const cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
  return cfg.machines || {};
}

function sshHostFor(name, machines) {
  return machines[name]?.sshHost || name;
}

// One ssh invocation with a warm multiplexed master so repeated frames are fast.
function ssh(host, remoteCmd, { timeoutMs = 15000 } = {}) {
  const cp = join(HOME, ".ssh", `cm-${host}`);
  return execFileSync(
    "ssh",
    [
      "-o", "ControlMaster=auto",
      "-o", `ControlPath=${cp}`,
      "-o", "ControlPersist=300",
      "-o", "BatchMode=yes",
      host,
      remoteCmd,
    ],
    { encoding: "utf8", timeout: timeoutMs, maxBuffer: 64 * 1024 * 1024 },
  );
}

function captureFrame(name, { noOCR = false, out, json = false } = {}) {
  const machines = loadMachines();
  if (!machines[name]) {
    console.error(`unknown machine "${name}" — known: ${Object.keys(machines).join(", ") || "(none)"}`);
    process.exit(1);
  }
  const host = sshHostFor(name, machines);
  const mode = noOCR ? "noocr" : "full";
  // Drop the request, wait for the daemon's done marker (≤4s), emit the envelope.
  const remote =
    `d=${REMOTE_STATE}; mkdir -p "$d"; rm -f "$d/frame.done"; ` +
    `printf '%s' '${mode}' > "$d/frame.req"; ` +
    `for i in $(seq 1 200); do [ -f "$d/frame.done" ] && break; sleep 0.02; done; ` +
    `cat "$d/frame.out.json" 2>/dev/null`;
  let raw;
  try {
    raw = ssh(host, remote);
  } catch (e) {
    console.error(`ssh ${host} failed: ${e.message.split("\n")[0]}`);
    process.exit(1);
  }
  let env;
  try {
    env = JSON.parse(raw);
  } catch {
    console.error(`no frame from ${name} — is SlabMenubar running there? (frame doctor ${name})`);
    process.exit(1);
  }
  if (env.capture === "permission_needed") {
    console.error(
      `${name}: Screen Recording not granted to SlabMenubar yet.\n` +
        `  run:  frame setup ${name}\n` +
        `  (AX + window/meta still captured; pixels + OCR are blocked until granted)`,
    );
  }
  // Write the thumbnail out; keep the base64 out of stdout unless --json.
  const outPath = out || join(FRAMES_DIR, `${name}.jpg`);
  if (env.thumb_jpg_b64) {
    mkdirSync(dirname(outPath), { recursive: true });
    writeFileSync(outPath, Buffer.from(env.thumb_jpg_b64, "base64"));
  }
  if (json) {
    process.stdout.write(JSON.stringify(env));
    return;
  }
  const { thumb_jpg_b64, ...rest } = env;
  rest.thumb = env.thumb_jpg_b64 ? outPath : null;
  rest.ocr_count = (env.ocr || []).length;
  rest.ax_count = (env.ax?.elements || []).length;
  delete rest.ocr;
  delete rest.ax;
  console.log(JSON.stringify(rest, null, 2));
}

// Read a TCC grant state over ssh (system DB is world-readable here).
function srGrant(host) {
  try {
    const q =
      `sqlite3 "/Library/Application Support/com.apple.TCC/TCC.db" ` +
      `"select auth_value from access where service='kTCCServiceScreenCapture' ` +
      `and client='${APP_BUNDLE}';" 2>/dev/null`;
    return ssh(host, q).trim();
  } catch {
    return "";
  }
}

function doctor(name) {
  const machines = loadMachines();
  const names = name ? [name] : Object.keys(machines);
  if (!names.length) {
    console.error(`no machines registered (${CONFIG_PATH})`);
    process.exit(1);
  }
  for (const n of names) {
    const host = sshHostFor(n, machines);
    let running = "?";
    try {
      running = ssh(host, "pgrep -x slab-menubar >/dev/null && echo yes || echo no").trim();
    } catch (e) {
      console.log(`${n}: UNREACHABLE (${e.message.split("\n")[0]})`);
      continue;
    }
    const sr = srGrant(host);
    const srLabel = sr === "2" ? "granted" : sr === "" ? "not listed (capture once to register)" : `denied (auth=${sr})`;
    console.log(
      `${n}: SlabMenubar ${running === "yes" ? "running" : "NOT running"} | ` +
        `Screen Recording ${srLabel} | Accessibility inherited from app trust`,
    );
  }
}

function setup(name) {
  const machines = loadMachines();
  if (!machines[name]) {
    console.error(`unknown machine "${name}"`);
    process.exit(1);
  }
  const host = sshHostFor(name, machines);
  console.log(`Triggering a capture on ${name} to surface the Screen Recording prompt…`);
  captureFrame(name, { noOCR: true });
  const sr = srGrant(host);
  if (sr === "2") {
    console.log(`✓ ${name}: Screen Recording already granted — frames will include pixels + OCR.`);
    return;
  }
  console.log(
    `\nOn ${name}'s screen, grant Screen Recording to SlabMenubar:\n` +
      `  1. A system prompt may already be showing — click "Allow".\n` +
      `  2. Otherwise: System Settings → Privacy & Security → Screen & System\n` +
      `     Audio Recording → enable "SlabMenubar".\n` +
      `  3. Re-run:  frame ${name}   (the app picks up the grant; no restart needed)\n` +
      `(Accessibility is already granted to the app, so the AX tree needs no toggle.)`,
  );
}

function list() {
  const machines = loadMachines();
  const names = Object.keys(machines);
  if (!names.length) {
    console.log(`(no machines in ${CONFIG_PATH})`);
    return;
  }
  for (const n of names) console.log(`${n}\t-> ssh ${sshHostFor(n, machines)}`);
}

// ---- arg parse ----
const argv = process.argv.slice(2);
const cmd = argv[0];
const flag = (f) => argv.includes(f);
const opt = (f) => {
  const i = argv.indexOf(f);
  return i >= 0 ? argv[i + 1] : undefined;
};

if (!cmd || cmd === "-h" || cmd === "--help") {
  console.log(
    "frame — capture a rich frame (pixels + OCR + AX + state) of a remote Mac\n\n" +
      "  frame <machine> [--no-ocr] [--out file.jpg] [--json]\n" +
      "  frame doctor [machine]      per-machine daemon + permission status\n" +
      "  frame setup <machine>       trigger + guide the one-time Screen Recording grant\n" +
      "  frame list                  registered machines\n",
  );
  process.exit(0);
}
if (cmd === "doctor") doctor(argv[1]);
else if (cmd === "setup") setup(argv[1]);
else if (cmd === "list") list();
else captureFrame(cmd, { noOCR: flag("--no-ocr"), out: opt("--out"), json: flag("--json") });
