#!/usr/bin/env node
// reel-compatible CDP page recorder for capture hosts without ScreenCaptureKit
// permission. It records the real composited Chrome page over Page.screencast,
// preserving Captutor's measured-timeline contract and drawn tutorial cursor.
//
// Use:
//   CAPTUTOR_REEL=$PWD/captutor/bin/cdp-reel.mjs CDP_PORT=9333 \
//     node captutor/captutor.mjs render screenplay.mjs
//
// This is a fallback, not Stage Mode: browser chrome and the macOS desktop are
// intentionally absent. The output is still a real time-based interaction
// recording, not reconstructed DOM animation or a slide export.
import { spawn, spawnSync } from "node:child_process";
import {
  closeSync, copyFileSync, existsSync, mkdirSync, openSync, readFileSync,
  readdirSync, writeFileSync,
} from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const SCRIPT = fileURLToPath(import.meta.url);
const STATE = join(process.env.HOME, ".local/share/slab/state/reel.state");
const HOST = process.env.CDP_HOST || "127.0.0.1";
const PORT = process.env.CDP_PORT || "9222";
const sleep = (ms) => new Promise((resolveSleep) => setTimeout(resolveSleep, ms));

const arg = (name, fallback = null) => {
  const index = process.argv.indexOf(name);
  return index >= 0 ? process.argv[index + 1] : fallback;
};

class CDP {
  constructor(url) {
    this.id = 0;
    this.pending = new Map();
    this.listeners = new Map();
    this.ready = new Promise((resolveReady, rejectReady) => {
      this.ws = new WebSocket(url);
      this.ws.addEventListener("open", resolveReady);
      this.ws.addEventListener("error", rejectReady);
      this.ws.addEventListener("message", (event) => {
        const message = JSON.parse(event.data);
        if (message.id) {
          const pending = this.pending.get(message.id);
          if (!pending) return;
          this.pending.delete(message.id);
          message.error ? pending.reject(new Error(JSON.stringify(message.error))) : pending.resolve(message.result);
          return;
        }
        for (const listener of this.listeners.get(message.method) || []) listener(message.params);
      });
    });
  }

  async send(method, params = {}) {
    await this.ready;
    const id = ++this.id;
    return new Promise((resolveSend, rejectSend) => {
      this.pending.set(id, { resolve: resolveSend, reject: rejectSend });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  on(method, listener) {
    const rows = this.listeners.get(method) || [];
    rows.push(listener);
    this.listeners.set(method, rows);
  }
}

async function pageTarget() {
  const targets = await fetch(`http://${HOST}:${PORT}/json`).then((response) => response.json());
  const match = process.env.CAPTUTOR_CDP_MATCH;
  const pages = targets.filter((target) => target.type === "page");
  return (match ? pages.find((target) => target.url.includes(match)) : pages[0]) || null;
}

function ffconcatPath(value) {
  return value.replaceAll("'", "'\\''");
}

async function daemon(work, fps) {
  mkdirSync(work, { recursive: true });
  const target = await pageTarget();
  if (!target) throw new Error("cdp-reel: no page target");
  const cdp = new CDP(target.webSocketDebuggerUrl);
  const frames = [];
  let sequence = 0;

  cdp.on("Page.screencastFrame", ({ data, metadata, sessionId }) => {
    const file = join(work, `frame-${String(sequence++).padStart(6, "0")}.jpg`);
    writeFileSync(file, Buffer.from(data, "base64"));
    frames.push({ file, at: Date.now() / 1000, metadata });
    cdp.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
    if (frames.length === 1) {
      writeFileSync(join(work, "ready.json"), JSON.stringify({ since: frames[0].at }));
    }
  });

  await cdp.send("Page.enable");
  await cdp.send("Page.startScreencast", {
    format: "jpeg",
    quality: 92,
    everyNthFrame: 1,
  });

  while (!existsSync(join(work, "stop"))) await sleep(100);
  const stoppedAt = Date.now() / 1000;
  await cdp.send("Page.stopScreencast").catch(() => {});
  await sleep(250);
  if (frames.length === 0) throw new Error("cdp-reel: no frames captured");

  const concat = ["ffconcat version 1.0"];
  for (let index = 0; index < frames.length; index++) {
    const current = frames[index];
    const nextAt = frames[index + 1]?.at ?? stoppedAt;
    concat.push(`file '${ffconcatPath(current.file)}'`);
    concat.push(`duration ${Math.max(1 / fps, nextAt - current.at).toFixed(6)}`);
  }
  concat.push(`file '${ffconcatPath(frames.at(-1).file)}'`);
  const concatFile = join(work, "frames.ffconcat");
  const clip = join(work, "clip.mp4");
  writeFileSync(concatFile, concat.join("\n") + "\n");
  const encoded = spawnSync("ffmpeg", [
    "-y", "-loglevel", "error", "-f", "concat", "-safe", "0", "-i", concatFile,
    "-vf", `fps=${fps},pad=ceil(iw/2)*2:ceil(ih/2)*2,format=yuv420p`,
    "-c:v", "libx264", "-preset", "medium",
    "-crf", "18", "-movflags", "+faststart", clip,
  ], { encoding: "utf8" });
  if (encoded.status !== 0) throw new Error(`cdp-reel ffmpeg: ${encoded.stderr}`);
  writeFileSync(join(work, "done.json"), JSON.stringify({ clip, frames: frames.length, stoppedAt }));
  try { cdp.ws.close(); } catch {}
}

async function start() {
  const fps = Number(arg("--fps", "30"));
  const work = `/tmp/captutor-cdp-reel-${process.pid}-${Date.now()}`;
  mkdirSync(work, { recursive: true });
  mkdirSync(resolve(STATE, ".."), { recursive: true });
  const log = openSync(join(work, "daemon.log"), "a");
  const child = spawn(process.execPath, [SCRIPT, "capture-daemon", "--work", work, "--fps", String(fps)], {
    detached: true,
    stdio: ["ignore", log, log],
    env: process.env,
  });
  child.unref();
  closeSync(log);

  const readyFile = join(work, "ready.json");
  for (let tries = 0; tries < 150 && !existsSync(readyFile); tries++) await sleep(100);
  if (!existsSync(readyFile)) {
    const detail = existsSync(join(work, "daemon.log")) ? readFileSync(join(work, "daemon.log"), "utf8") : "";
    throw new Error(`cdp-reel did not start: ${detail}`);
  }
  const ready = JSON.parse(readFileSync(readyFile, "utf8"));
  writeFileSync(STATE, JSON.stringify({
    recording: true,
    since: ready.since,
    backend: "cdp-screencast",
    captureSessionDir: work,
    pid: child.pid,
  }, null, 2));
  console.log(`recording cdp-screencast since ${ready.since}`);
}

async function stop() {
  if (!existsSync(STATE)) throw new Error("cdp-reel: no active state");
  const state = JSON.parse(readFileSync(STATE, "utf8"));
  if (state.backend !== "cdp-screencast" || !state.captureSessionDir) {
    throw new Error("cdp-reel: active state belongs to another recorder");
  }
  const work = state.captureSessionDir;
  writeFileSync(join(work, "stop"), "stop\n");
  const doneFile = join(work, "done.json");
  for (let tries = 0; tries < 600 && !existsSync(doneFile); tries++) await sleep(100);
  if (!existsSync(doneFile)) {
    const detail = existsSync(join(work, "daemon.log")) ? readFileSync(join(work, "daemon.log"), "utf8") : "";
    throw new Error(`cdp-reel did not stop: ${detail}`);
  }
  const done = JSON.parse(readFileSync(doneFile, "utf8"));
  const out = resolve(arg("--out", "clip.mp4"));
  copyFileSync(done.clip, out);
  writeFileSync(STATE, JSON.stringify({ ...state, recording: false, out, frames: done.frames }, null, 2));
  console.log(`stopped cdp-screencast → ${out} (${done.frames} source frames)`);
}

async function status() {
  console.log(existsSync(STATE) ? readFileSync(STATE, "utf8") : JSON.stringify({ recording: false }));
}

const command = process.argv[2];
if (command === "capture-daemon") await daemon(resolve(arg("--work")), Number(arg("--fps", "30")));
else if (command === "start") await start();
else if (command === "stop") await stop();
else if (command === "status") await status();
else {
  console.log("usage: cdp-reel start|stop|status [--fps 30] [--out clip.mp4]");
  process.exit(command ? 1 : 0);
}
