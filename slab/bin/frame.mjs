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

import { execFileSync, spawn } from "node:child_process";
import { appendFileSync, existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import net from "node:net";
import { dirname, join } from "node:path";

const HOME = process.env.HOME;
const CONFIG_PATH =
  process.env.SLAB_PUPPET_CONFIG || join(HOME, ".config", "slab", "puppet.json");
const FRAMES_DIR = join(HOME, ".local", "share", "slab", "frames");
const SOCK_PATH = process.env.SLAB_FRAME_SOCK || join(HOME, ".local", "share", "slab", "frame.sock");
// The menubar app on THIS (controller) Mac watches `state/open-frame` and
// raises a preview window per machine, badged with the machine name (see
// FramePreview.swift). Opt-in via `--preview` / `frame view <machine>` so a
// plain `frame` call never pops a window. SLAB_HOME mirrors the Swift side.
const SLAB_HOME = process.env.SLAB_HOME || join(HOME, ".local", "share", "slab");
const FRAME_REQUEST_FILE = join(SLAB_HOME, "state", "open-frame");
const APP_BUNDLE = "computer.slab.menubar";

// The remote read-loop agent: ONE per machine, held open by the server so each
// frame is a stdin-write + stdout-read on an already-open ssh channel — no
// channel setup (~100ms) and no node restart per call. `bash -s` reads its
// SCRIPT from stdin, so the agent can't also read modes from stdin — instead we
// drop this script on the target once and feed modes to `bash <file>` over the
// persistent channel. Emits exactly one JSON line per mode (terminator-framed).
// Emits a length-prefixed binary frame per request: `ACF1 <jsonLen> <jpgLen>\n`
// then the JSON bytes then the raw JPEG bytes — no base64. One frame per mode
// line read on stdin.
// NOTE: this is a JS template literal — bash `${...}` would be interpreted as JS
// interpolation, so the script uses only `$var` and `$(...)` (both literal here).
const AGENT_SCRIPT = String.raw`#!/bin/bash
d="$HOME/.local/share/slab/state"; mkdir -p "$d"
emit() {
  local j="$d/frame.out.json" p="$d/frame.out.jpg" jl pl e el
  jl=$(wc -c < "$j" 2>/dev/null | tr -d ' '); [ -z "$jl" ] && jl=0
  pl=$(wc -c < "$p" 2>/dev/null | tr -d ' '); [ -z "$pl" ] && pl=0
  if [ "$jl" = 0 ]; then
    e='{"capture":"error","reason":"no-out"}'
    el=$(printf '%s' "$e" | wc -c | tr -d ' ')
    printf 'ACF1 %s 0\n%s' "$el" "$e"; return
  fi
  printf 'ACF1 %s %s\n' "$jl" "$pl"; cat "$j"; [ "$pl" != 0 ] && cat "$p"
}
while IFS= read -r mode; do
  [ -z "$mode" ] && mode=full
  rm -f "$d/frame.done"; printf '%s' "$mode" > "$d/frame.req"
  for i in $(seq 1 400); do [ -f "$d/frame.done" ] && break; sleep 0.01; done
  emit
done`;
const AGENT_REMOTE_PATH = "~/.local/share/slab/frame-agent.sh";

function loadMachines() {
  let machines = {};
  if (existsSync(CONFIG_PATH)) {
    machines = JSON.parse(readFileSync(CONFIG_PATH, "utf8")).machines || {};
  }
  // The controller captures itself with no ssh (ssh-to-self is host-key
  // fragile). Expose it under its LocalHostName as a `local: true` machine —
  // synthetic, so it never has to live in puppet.json (which puppet's daemon
  // also reads and would try to CDP-connect).
  let self = "local";
  try {
    self = execFileSync("scutil", ["--get", "LocalHostName"], { encoding: "utf8" }).trim() || "local";
  } catch {}
  if (!machines[self]) machines[self] = { local: true };
  return machines;
}

function sshHostFor(name, machines) {
  return machines[name]?.sshHost || name;
}

// One ssh invocation with a warm multiplexed master so repeated frames are fast.
// The command is fed to `bash -s` over stdin, NOT passed as an argument: the
// remote runs the login shell, and some machines use fish (blueberry) which
// can't parse the bash `for/$()/done` syntax. `bash -s` + stdin is shell-
// agnostic and sidesteps all quoting.
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
      "bash -s",
    ],
    { input: remoteCmd, encoding: "utf8", timeout: timeoutMs, maxBuffer: 64 * 1024 * 1024 },
  );
}

// Run a shell command on a machine — locally (no ssh) when it's flagged
// "local" in the registry (e.g. neo, the controller, capturing its own screen;
// `ssh localhost` is awkward/host-key-fragile), otherwise over the warm master.
function runOn(name, machines, cmd, { timeoutMs = 15000 } = {}) {
  if (machines[name]?.local) {
    return execFileSync("bash", ["-c", cmd], {
      encoding: "utf8", timeout: timeoutMs, maxBuffer: 64 * 1024 * 1024,
    });
  }
  return ssh(sshHostFor(name, machines), cmd, { timeoutMs });
}

function sshOpts(host) {
  return [
    "-o", "ControlMaster=auto",
    "-o", `ControlPath=${join(HOME, ".ssh", `cm-${host}`)}`,
    "-o", "ControlPersist=300",
    "-o", "BatchMode=yes",
  ];
}

// ─── ACF1 binary frame codec ──────────────────────────────────────────────
// Wire format: `ACF1 <jsonLen> <jpgLen>\n` + <jsonLen bytes JSON> + <jpgLen
// bytes raw JPEG>. No base64. Parses one frame off the front of a Buffer.
// Returns {frame:{json,jpg}, rest} when complete, {skip, rest} for a non-ACF1
// line (ssh banner), or null when more bytes are needed.
function parseFrame(buf) {
  const nl = buf.indexOf(0x0a);
  if (nl < 0) return null;
  const header = buf.subarray(0, nl).toString("ascii");
  const m = header.match(/^ACF1 (\d+) (\d+)$/);
  if (!m) return { skip: true, rest: buf.subarray(nl + 1) };
  const jl = +m[1], pl = +m[2];
  const start = nl + 1, end = start + jl + pl;
  if (buf.length < end) return null;
  return {
    frame: { json: buf.subarray(start, start + jl).toString("utf8"), jpg: buf.subarray(start + jl, end) },
    rest: buf.subarray(end),
  };
}
function frameBytes(json, jpg) {
  const jb = Buffer.from(json, "utf8");
  return Buffer.concat([Buffer.from(`ACF1 ${jb.length} ${jpg.length}\n`, "ascii"), jb, jpg]);
}

// Ask the local menubar app to open (or live-reload) a preview window for a
// pulled frame. One `path\tmachine` line appended to `state/open-frame`; the
// app's 2 s tick consumes it (FramePreview.consumeRequests). Re-asking the
// same machine raises + reloads its window. Best-effort: a missing menubar
// just leaves an unconsumed line, which is harmless.
function requestPreview(machine, jpgPath) {
  mkdirSync(dirname(FRAME_REQUEST_FILE), { recursive: true });
  appendFileSync(FRAME_REQUEST_FILE, `${jpgPath}\t${machine}\n`);
}

// ─── persistent server: one held-open agent per machine ───────────────────
// Removes the two per-call costs the transport audit found: node cold-start
// (the server stays warm) and ssh channel setup (each agent is a single ssh
// process kept open, fed modes on stdin). A frame then costs ~daemon work + one
// RTT on the open channel instead of +~170ms of process/connection spin-up.
const agents = new Map(); // name -> { proc, pending: [resolve], buf }

function ensureAgentScript(name, machines) {
  if (machines[name]?.local) {
    const p = join(HOME, ".local", "share", "slab", "frame-agent.sh");
    mkdirSync(dirname(p), { recursive: true });
    writeFileSync(p, AGENT_SCRIPT);
    return p;
  }
  const host = sshHostFor(name, machines);
  execFileSync(
    "ssh",
    [...sshOpts(host), host, `mkdir -p ~/.local/share/slab && cat > ${AGENT_REMOTE_PATH}`],
    { input: AGENT_SCRIPT, encoding: "utf8", timeout: 15000 },
  );
  return AGENT_REMOTE_PATH;
}

function spawnAgent(name, machines) {
  const scriptPath = ensureAgentScript(name, machines);
  let proc;
  if (machines[name]?.local) {
    proc = spawn("bash", [scriptPath], { stdio: ["pipe", "pipe", "ignore"] });
  } else {
    const host = sshHostFor(name, machines);
    proc = spawn("ssh", [...sshOpts(host), host, `bash ${AGENT_REMOTE_PATH}`], {
      stdio: ["pipe", "pipe", "ignore"],
    });
  }
  const ag = { proc, pending: [], buf: Buffer.alloc(0) }; // buf is binary
  proc.stdout.on("data", (chunk) => {
    ag.buf = Buffer.concat([ag.buf, chunk]);
    for (;;) {
      const r = parseFrame(ag.buf);
      if (!r) break;
      ag.buf = r.rest;
      if (r.skip) continue; // ssh banner line
      const resolve = ag.pending.shift();
      if (resolve) resolve(r.frame);
    }
  });
  proc.on("exit", () => {
    agents.delete(name);
    const dead = { json: '{"capture":"error","reason":"agent-exit"}', jpg: Buffer.alloc(0) };
    ag.pending.forEach((r) => r(dead));
  });
  agents.set(name, ag);
  return ag;
}

function agentRequest(name, machines, mode, timeoutMs = 15000) {
  return new Promise((resolve, reject) => {
    let ag;
    try {
      ag = agents.get(name) || spawnAgent(name, machines);
    } catch (e) {
      reject(e);
      return;
    }
    const timer = setTimeout(() => reject(new Error("agent timeout")), timeoutMs);
    ag.pending.push((frame) => { clearTimeout(timer); resolve(frame); });
    ag.proc.stdin.write(mode + "\n");
  });
}

function runServer() {
  mkdirSync(dirname(SOCK_PATH), { recursive: true });
  try { unlinkSync(SOCK_PATH); } catch {}
  const server = net.createServer((sock) => {
    // Request is a JSON line; response is an ACF1 binary frame (forwarded from
    // the held agent), so we frame the request side on '\n' but write binary.
    let buf = Buffer.alloc(0);
    sock.on("data", async (chunk) => {
      buf = Buffer.concat([buf, chunk]);
      let nl;
      while ((nl = buf.indexOf(0x0a)) >= 0) {
        const line = buf.subarray(0, nl).toString("utf8");
        buf = buf.subarray(nl + 1);
        if (!line.trim()) continue;
        let req;
        try { req = JSON.parse(line); } catch { sock.write(frameBytes('{"error":"bad json"}', Buffer.alloc(0))); continue; }
        try {
          const frame = await agentRequest(req.machine, loadMachines(), req.mode || "full");
          sock.write(frameBytes(frame.json, frame.jpg));
        } catch (e) {
          sock.write(frameBytes(JSON.stringify({ error: String(e.message || e) }), Buffer.alloc(0)));
        }
      }
    });
    sock.on("error", () => {});
  });
  server.on("error", (e) => {
    if (e.code === "EADDRINUSE") process.exit(0); // another server won the race
    throw e;
  });
  server.listen(SOCK_PATH, () => console.log(`frame server listening on ${SOCK_PATH}`));
}

function requestViaServer(machine, mode, timeoutMs = 15000) {
  return new Promise((resolve, reject) => {
    const sock = net.createConnection(SOCK_PATH);
    let buf = Buffer.alloc(0);
    const timer = setTimeout(() => { sock.destroy(); reject(new Error("server rpc timeout")); }, timeoutMs);
    sock.on("error", (e) => { clearTimeout(timer); reject(e); });
    sock.on("connect", () => sock.write(JSON.stringify({ machine, mode }) + "\n"));
    sock.on("data", (chunk) => {
      buf = Buffer.concat([buf, chunk]);
      const r = parseFrame(buf);
      if (r && r.frame) { clearTimeout(timer); sock.end(); resolve(r.frame); }
    });
  });
}

// One-shot direct path (no server): spawn ssh/local, feed the request, read one
// ACF1 frame off stdout. Returns {json, jpg}.
function directFrame(name, machines, mode, timeoutMs = 15000) {
  return new Promise((resolve, reject) => {
    const remote =
      `d="$HOME/.local/share/slab/state"; mkdir -p "$d"; rm -f "$d/frame.done"; ` +
      `printf '%s' '${mode}' > "$d/frame.req"; ` +
      `for i in $(seq 1 400); do [ -f "$d/frame.done" ] && break; sleep 0.01; done; ` +
      `j="$d/frame.out.json"; p="$d/frame.out.jpg"; ` +
      `jl=$(wc -c < "$j" 2>/dev/null | tr -d ' '); pl=$(wc -c < "$p" 2>/dev/null | tr -d ' '); ` +
      `[ -z "$jl" ] && jl=0; [ -z "$pl" ] && pl=0; ` +
      `printf 'ACF1 %s %s\\n' "$jl" "$pl"; cat "$j" 2>/dev/null; [ "$pl" != 0 ] && cat "$p" 2>/dev/null`;
    let proc;
    if (machines[name]?.local) {
      proc = spawn("bash", ["-c", remote], { stdio: ["ignore", "pipe", "ignore"] });
    } else {
      const host = sshHostFor(name, machines);
      proc = spawn("ssh", [...sshOpts(host), host, "bash -s"], { stdio: ["pipe", "pipe", "ignore"] });
      proc.stdin.end(remote);
    }
    let buf = Buffer.alloc(0), settled = false;
    const finish = (fn, arg) => { if (!settled) { settled = true; clearTimeout(timer); fn(arg); } };
    const timer = setTimeout(() => { proc.kill(); finish(reject, new Error("direct timeout")); }, timeoutMs);
    proc.stdout.on("data", (chunk) => {
      buf = Buffer.concat([buf, chunk]);
      let r = parseFrame(buf);
      while (r && r.skip) { buf = r.rest; r = parseFrame(buf); }
      if (r && r.frame) { finish(resolve, r.frame); proc.kill(); }
    });
    proc.on("error", (e) => finish(reject, e));
    proc.on("exit", () => {
      const r = parseFrame(buf);
      if (r && r.frame) finish(resolve, r.frame);
      else finish(reject, new Error("no frame"));
    });
  });
}

async function captureFrame(name, { noOCR = false, fast = false, cursor = false, cursorAt, crop, baseline = false, diff = false, out, json = false, direct = false, preview = false } = {}) {
  const machines = loadMachines();
  if (!machines[name]) {
    console.error(`unknown machine "${name}" — known: ${Object.keys(machines).join(", ") || "(none)"}`);
    process.exit(1);
  }
  const mode = [noOCR ? "noocr" : "full", fast ? "fast" : "", cursorAt ? `cursor=${cursorAt[0]},${cursorAt[1]}` : cursor ? "cursor" : "", crop ? `crop=${crop.join(",")}` : "", baseline ? "baseline" : "", diff ? "diff" : ""]
    .filter(Boolean).join(" ");
  // Use the resident server only if already running (opt-in; see runServer);
  // otherwise a one-shot direct ssh. Both return an ACF1 {json, jpg} frame —
  // the JPEG is raw bytes, never base64.
  let frame = null;
  if (!direct && existsSync(SOCK_PATH)) {
    try { frame = await requestViaServer(name, mode); } catch { frame = null; }
  }
  if (!frame) {
    try { frame = await directFrame(name, machines, mode); } catch (e) {
      console.error(`${name} unreachable: ${String(e.message || e).split("\n")[0]}`);
      process.exit(1);
    }
  }
  let env;
  try {
    env = JSON.parse(frame.json);
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
  // Write the raw JPEG to disk (no base64 anywhere on the path).
  const outPath = out || join(FRAMES_DIR, `${name}.jpg`);
  const hasJpg = frame.jpg && frame.jpg.length > 0;
  if (hasJpg) {
    mkdirSync(dirname(outPath), { recursive: true });
    writeFileSync(outPath, frame.jpg);
  }
  // Opt-in: pop (or live-reload) the local preview window, badged `name`.
  // Only with pixels in hand — a permission-blocked frame has nothing to show.
  if (preview && hasJpg) requestPreview(name, outPath);
  else if (preview && !hasJpg) {
    console.error(`(no pixels to preview for ${name} — capture blocked; run: frame setup ${name})`);
  }
  if (json) {
    process.stdout.write(frame.json);
    return;
  }
  const rest = { ...env };
  rest.thumb = hasJpg ? outPath : null;
  rest.ocr_count = (env.ocr || []).length;
  rest.ax_count = (env.ax?.elements || []).length;
  delete rest.ocr;
  delete rest.ax;
  console.log(JSON.stringify(rest, null, 2));
}

// Read a TCC grant state (system DB is world-readable here).
function srGrant(name, machines) {
  try {
    const q =
      `sqlite3 "/Library/Application Support/com.apple.TCC/TCC.db" ` +
      `"select auth_value from access where service='kTCCServiceScreenCapture' ` +
      `and client='${APP_BUNDLE}';" 2>/dev/null`;
    return runOn(name, machines, q).trim();
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
    let running = "?";
    try {
      running = runOn(n, machines, "pgrep -x slab-menubar >/dev/null && echo yes || echo no").trim();
    } catch (e) {
      console.log(`${n}: UNREACHABLE (${e.message.split("\n")[0]})`);
      continue;
    }
    const sr = srGrant(n, machines);
    const srLabel = sr === "2" ? "granted" : sr === "" ? "not listed (capture once to register)" : `denied (auth=${sr})`;
    console.log(
      `${n}: SlabMenubar ${running === "yes" ? "running" : "NOT running"} | ` +
        `Screen Recording ${srLabel} | Accessibility inherited from app trust`,
    );
  }
}

async function setup(name) {
  const machines = loadMachines();
  if (!machines[name]) {
    console.error(`unknown machine "${name}"`);
    process.exit(1);
  }
  console.log(`Triggering a capture on ${name} to surface the Screen Recording prompt…`);
  await captureFrame(name, { noOCR: true });
  const sr = srGrant(name, machines);
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
const pointOpt = (f) => {
  const raw = opt(f);
  if (!raw) return undefined;
  const p = raw.split(",").map(Number);
  return p.length === 2 && p.every(Number.isFinite) ? p : undefined;
};

if (!cmd || cmd === "-h" || cmd === "--help") {
  console.log(
    "frame — capture a rich frame (pixels + OCR + AX + state) of a remote Mac\n\n" +
      "  frame <machine> [--no-ocr] [--fast] [--cursor] [--direct] [--preview] [--out file.jpg] [--json]\n" +
      "      --fast: Vision .fast OCR (lower latency, less accurate on small text)\n" +
      "      --cursor: draw a high-contrast virtual marker at the mouse position\n" +
      "      --direct: bypass the resident server, do a one-shot ssh\n" +
      "      --preview: pop a labeled preview window of the pulled frame on THIS Mac\n" +
      "  frame view <machine>        capture + open the badged preview window (= --preview)\n" +
      "  frame doctor [machine]      per-machine daemon + permission status\n" +
      "  frame setup <machine>       trigger + guide the one-time Screen Recording grant\n" +
      "  frame list                  registered machines\n" +
      "  frame server                run the resident server (holds a warm agent per\n" +
      "                              machine; auto-started on demand otherwise)\n" +
      "  frame stop                  stop the resident server\n",
  );
  process.exit(0);
}
if (cmd === "server") runServer();
else if (cmd === "stop") {
  try { unlinkSync(SOCK_PATH); } catch {}
  try { execFileSync("pkill", ["-f", "frame.mjs server"]); } catch {}
  console.log("frame server stopped");
} else if (cmd === "doctor") doctor(argv[1]);
else if (cmd === "setup") await setup(argv[1]);
else if (cmd === "list") list();
else if (cmd === "view") {
  // `frame view <machine>` — capture + open the badged preview window.
  // Sugar for `frame <machine> --preview` so it reads like the slab-video /
  // slab-pdf "show me this" verbs.
  if (!argv[1]) { console.error("usage: frame view <machine>"); process.exit(1); }
  await captureFrame(argv[1], { noOCR: flag("--no-ocr"), fast: flag("--fast"), cursor: flag("--cursor"), direct: flag("--direct"), out: opt("--out"), preview: true });
} else await captureFrame(cmd, { noOCR: flag("--no-ocr"), fast: flag("--fast"), cursor: flag("--cursor"), cursorAt: pointOpt("--cursor-at"), crop: opt("--crop")?.split(",").map(Number), baseline: flag("--baseline"), diff: flag("--diff"), direct: flag("--direct"), out: opt("--out"), json: flag("--json"), preview: flag("--preview") });
