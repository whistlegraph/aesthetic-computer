#!/usr/bin/env node
// reel — screen VIDEO capture for the fleet. The moving-picture sibling of `frame`.
//
//   frame <machine>   → one still  (jpeg + OCR + AX)
//   reel  <machine>   → a clip     (hardware-encoded mp4)
//
// Neither shells out to a screen grabber. Both poke a file into the target's
// slab state dir; SlabMenubar (which holds the Screen Recording grant and lives
// in the GUI session, where an ssh shell cannot go) does the actual capture.
// Here that's ScreenRecord.swift → SCStream → SCRecordingOutput → mp4, so no
// frame ever crosses a process boundary and nothing drops under load.
//
// Usage
//   reel start  [machine] [--window "Fuser"] [--fps 60] [--cursor]
//   reel stop   [machine] [--out ./clip.mp4]
//   reel status [machine]
//   reel clip   [machine] --secs 5 [--window …] [--out ./clip.mp4]
//
// --window takes a substring of the window title OR the app name, and captures
// just that window: it crops to the app, follows it if it moves, and keeps the
// desktop, menu bar, and every other app out of frame. Without it you get the
// whole display.
//
// The cursor is OFF by default. Scripted tutorials draw their own (puppet's
// virtual pointer) because the real macOS cursor teleports when driven
// synthetically, which reads as broken. Pass --cursor when a human is driving.

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync, mkdirSync, rmSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname, resolve } from "node:path";

const HOME = homedir();
const CONFIG_PATH = join(HOME, ".config", "slab", "puppet.json");
const SLAB_HOME = process.env.SLAB_HOME || join(HOME, ".local", "share", "slab");
const STATE = join(SLAB_HOME, "state");

const REQ = join(STATE, "reel.req");
const DONE = join(STATE, "reel.done");
const ST = join(STATE, "reel.state");
const OUT = join(STATE, "reel.out.mp4");

// Remote paths are the same shape but must expand on the far side, where $HOME
// differs. Keep them as shell-expandable strings, not resolved locals.
const R_STATE = "$HOME/.local/share/slab/state";

function loadMachines() {
  let machines = {};
  if (existsSync(CONFIG_PATH)) {
    machines = JSON.parse(readFileSync(CONFIG_PATH, "utf8")).machines || {};
  }
  let self = "local";
  try {
    self = execFileSync("scutil", ["--get", "LocalHostName"], { encoding: "utf8" }).trim() || "local";
  } catch {}
  if (!machines[self]) machines[self] = { local: true };
  return machines;
}

function isLocal(machine, machines) {
  return !machine || machines[machine]?.local === true;
}

function ssh(host, remoteCmd, { timeoutMs = 20000 } = {}) {
  const cp = join(HOME, ".ssh", `cm-${host}`);
  return execFileSync(
    "ssh",
    ["-o", "ControlMaster=auto", "-o", `ControlPath=${cp}`,
     "-o", "ControlPersist=300", "-o", "BatchMode=yes",
     host, "bash -s"],
    { input: remoteCmd, encoding: "utf8", timeout: timeoutMs, maxBuffer: 64 * 1024 * 1024 },
  );
}

// Drop the request and wait for the menubar to acknowledge. The done-marker is
// the handshake: the app removes it on receipt and re-touches it once the
// action has fully settled (for `stop`, that means the mp4's moov atom is
// flushed — so a caller that sees done can safely read the file).
function poke(machine, machines, req, { timeoutMs = 15000 } = {}) {
  const body = JSON.stringify(req);
  if (isLocal(machine, machines)) {
    mkdirSync(STATE, { recursive: true });
    rmSync(DONE, { force: true });
    writeFileSync(REQ, body);
    const deadline = Date.now() + timeoutMs;
    while (Date.now() < deadline) {
      if (existsSync(DONE)) {
        return JSON.parse(readFileSync(ST, "utf8"));
      }
      execFileSync("sleep", ["0.05"]);
    }
    throw new Error("timed out waiting for SlabMenubar — is it running? (pgrep -x slab-menubar)");
  }

  const host = machines[machine]?.sshHost || machine;
  const b64 = Buffer.from(body).toString("base64");
  const out = ssh(host, `
set -e
mkdir -p ${R_STATE}
rm -f ${R_STATE}/reel.done
echo '${b64}' | base64 -d > ${R_STATE}/reel.req
for i in $(seq 1 ${Math.ceil(timeoutMs / 50)}); do
  [ -f ${R_STATE}/reel.done ] && { cat ${R_STATE}/reel.state; exit 0; }
  sleep 0.05
done
echo '{"error":"timed out waiting for SlabMenubar"}'
`);
  return JSON.parse(out.trim());
}

function pull(machine, machines, remotePath, localPath) {
  mkdirSync(dirname(localPath), { recursive: true });
  if (isLocal(machine, machines)) {
    execFileSync("cp", [remotePath, localPath]);
    return;
  }
  const host = machines[machine]?.sshHost || machine;
  const cp = join(HOME, ".ssh", `cm-${host}`);
  execFileSync("scp", ["-o", `ControlPath=${cp}`, `${host}:${remotePath}`, localPath], {
    stdio: "inherit",
  });
}

function report(st) {
  if (st.error) {
    console.error(`✗ ${st.error}`);
    process.exit(1);
  }
  return st;
}

function human(bytes) {
  if (bytes > 1e6) return `${(bytes / 1e6).toFixed(1)} MB`;
  if (bytes > 1e3) return `${(bytes / 1e3).toFixed(0)} KB`;
  return `${bytes} B`;
}

// --- args -------------------------------------------------------------------

const argv = process.argv.slice(2);
const cmd = argv[0];
const flag = (name, fallback = undefined) => {
  const i = argv.indexOf(`--${name}`);
  return i === -1 ? fallback : argv[i + 1];
};
const has = (name) => argv.includes(`--${name}`);
// First non-flag after the command is the machine (optional → local).
const positional = argv.slice(1).filter((a, i, arr) => {
  if (a.startsWith("--")) return false;
  const prev = arr[i - 1];
  return !(prev && prev.startsWith("--") && !["--cursor"].includes(prev));
});
const machine = positional[0];

const machines = loadMachines();

switch (cmd) {
  case "start": {
    const st = report(poke(machine, machines, {
      action: "start",
      window: flag("window"),
      fps: Number(flag("fps", 60)),
      cursor: has("cursor"),
      out: flag("remote-out"),
    }));
    console.log(`● recording${st.path ? ` → ${st.path}` : ""}`);
    break;
  }

  case "stop": {
    const st = report(poke(machine, machines, { action: "stop" }));
    const dest = resolve(flag("out", "./reel.mp4"));
    pull(machine, machines, st.path || OUT, dest);
    console.log(`■ ${dest} (${human(st.bytes || 0)})`);
    break;
  }

  case "status": {
    const st = poke(machine, machines, { action: "status" });
    console.log(JSON.stringify(st, null, 2));
    break;
  }

  // start → wait → stop, for a fixed-length take. The common case when you just
  // want N seconds of something already on screen.
  case "clip": {
    const secs = Number(flag("secs", 5));
    report(poke(machine, machines, {
      action: "start",
      window: flag("window"),
      fps: Number(flag("fps", 60)),
      cursor: has("cursor"),
    }));
    console.log(`● recording ${secs}s…`);
    execFileSync("sleep", [String(secs)]);
    const st = report(poke(machine, machines, { action: "stop" }));
    const dest = resolve(flag("out", "./reel.mp4"));
    pull(machine, machines, st.path || OUT, dest);
    console.log(`■ ${dest} (${human(st.bytes || 0)})`);
    break;
  }

  default:
    console.log(readFileSync(new URL(import.meta.url)).toString()
      .split("\n").filter((l) => l.startsWith("//")).map((l) => l.slice(3)).join("\n"));
    process.exit(cmd ? 1 : 0);
}
