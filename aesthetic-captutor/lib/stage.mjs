// stage — a disposable headless Chromium and a CDP page to drive.
//
// captutor films a real desktop (SlabMenubar → ScreenCaptureKit) because
// fuser's tutorials want browser chrome and a desk in frame. AC tutorials
// don't: the piece IS the whole screen on the devices AC is made for, so the
// composited page is the honest picture, and Page.screencast (the same
// transport as captutor/bin/cdp-reel.mjs) is the whole camera. That choice is
// what lets a capture host film with no Screen Recording grant at all.

import { spawn, execFileSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const FFMPEG = process.env.FFMPEG || "ffmpeg";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const CHROMIUM_CANDIDATES = [
  process.env.AESTHETIC_CAPTUTOR_CHROMIUM,
  "/Applications/Chromium.app/Contents/MacOS/Chromium",
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/usr/bin/chromium",
  "/usr/bin/chromium-browser",
].filter(Boolean);

export async function launchChromium({ view, port = Number(process.env.CDP_PORT || 9777) } = {}) {
  const bin = CHROMIUM_CANDIDATES.find((p) => existsSync(p));
  if (!bin) throw new Error(`no chromium found (tried: ${CHROMIUM_CANDIDATES.join(", ")})`);
  const profile = mkdtempSync(join(tmpdir(), "aesthetic-captutor-"));
  const child = spawn(bin, [
    "--headless=new",
    `--remote-debugging-port=${port}`,
    `--user-data-dir=${profile}`,
    `--window-size=${view.w},${view.h}`,
    "--hide-scrollbars",
    "--mute-audio",
    "--no-first-run",
    "--no-default-browser-check",
    "--autoplay-policy=no-user-gesture-required",
    "--use-gl=angle", // SwiftShader-backed WebGL for AC's canvas work
    "about:blank",
  ], { stdio: ["ignore", "ignore", "pipe"] });
  let stderr = "";
  child.stderr.on("data", (d) => { stderr += d; });

  // Wait for the debugger endpoint, not a fixed nap.
  for (let i = 0; i < 100; i++) {
    try {
      const list = await fetch(`http://127.0.0.1:${port}/json/list`).then((r) => r.json());
      if (list.some((t) => t.type === "page")) return { child, port, kill: () => child.kill("SIGTERM") };
    } catch {}
    if (child.exitCode !== null) break;
    await sleep(100);
  }
  child.kill("SIGTERM");
  throw new Error(`chromium did not expose CDP on :${port}\n${stderr.slice(-800)}`);
}

/// Minimal CDP client over the built-in WebSocket (node ≥ 22) — the same shape
/// as captutor/bin/cdp-reel.mjs's, plus the input verbs AC screenplays need.
class CDP {
  constructor(url) {
    this.id = 0;
    this.pending = new Map();
    this.listeners = new Map();
    this.ready = new Promise((res, rej) => {
      this.ws = new WebSocket(url);
      this.ws.addEventListener("open", res);
      this.ws.addEventListener("error", rej);
      this.ws.addEventListener("message", (event) => {
        const m = JSON.parse(event.data);
        if (m.id) {
          const p = this.pending.get(m.id);
          if (!p) return;
          this.pending.delete(m.id);
          m.error ? p.reject(new Error(JSON.stringify(m.error))) : p.resolve(m.result);
          return;
        }
        for (const fn of this.listeners.get(m.method) || []) fn(m.params);
      });
    });
  }
  async send(method, params = {}) {
    await this.ready;
    const id = ++this.id;
    return new Promise((res, rej) => {
      this.pending.set(id, { resolve: res, reject: rej });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }
  on(method, fn) {
    const rows = this.listeners.get(method) || [];
    rows.push(fn);
    this.listeners.set(method, rows);
  }
}

// Named keys, CDP-shaped. Printable characters synthesize their own entry.
const KEYS = {
  Enter: { key: "Enter", code: "Enter", keyCode: 13, text: "\r" },
  Escape: { key: "Escape", code: "Escape", keyCode: 27 },
  Backspace: { key: "Backspace", code: "Backspace", keyCode: 8 },
  Tab: { key: "Tab", code: "Tab", keyCode: 9 },
  " ": { key: " ", code: "Space", keyCode: 32, text: " " },
};

export async function attachPage({ port }) {
  const targets = await fetch(`http://127.0.0.1:${port}/json/list`).then((r) => r.json());
  const target = targets.find((t) => t.type === "page");
  if (!target) throw new Error("no page target");
  const cdp = new CDP(target.webSocketDebuggerUrl);
  await cdp.send("Page.enable");
  await cdp.send("Runtime.enable");

  return {
    raw: cdp,
    on: (m, fn) => cdp.on(m, fn),
    send: (m, p) => cdp.send(m, p),

    async setViewport({ w, h, dpr }) {
      await cdp.send("Emulation.setDeviceMetricsOverride", {
        width: w, height: h, deviceScaleFactor: dpr, mobile: false,
      });
    },

    async nav(url) {
      const loaded = new Promise((res) => cdp.on("Page.loadEventFired", res));
      await cdp.send("Page.navigate", { url });
      await loaded;
    },

    async eval(expression) {
      const { result } = await cdp.send("Runtime.evaluate", { expression, returnByValue: true });
      return result?.value;
    },

    async key(name) {
      const k = KEYS[name] || { key: name, code: `Key${name.toUpperCase()}`, text: name };
      const base = { key: k.key, code: k.code };
      if (k.keyCode) Object.assign(base, {
        windowsVirtualKeyCode: k.keyCode, nativeVirtualKeyCode: k.keyCode,
      });
      await cdp.send("Input.dispatchKeyEvent", { type: "keyDown", ...base, text: k.text });
      await cdp.send("Input.dispatchKeyEvent", { type: "keyUp", ...base });
    },

    async tap(x, y) {
      for (const type of ["mousePressed", "mouseReleased"]) {
        await cdp.send("Input.dispatchMouseEvent", {
          type, x, y, button: "left", clickCount: 1,
        });
        await sleep(60);
      }
    },

    async drag(from, to, steps = 24, ms = 600) {
      await cdp.send("Input.dispatchMouseEvent", {
        type: "mousePressed", x: from.x, y: from.y, button: "left", clickCount: 1,
      });
      for (let i = 1; i <= steps; i++) {
        const t = i / steps;
        // Slight ease so the stroke reads as a hand, not a plotter.
        const e = t * t * (3 - 2 * t);
        await cdp.send("Input.dispatchMouseEvent", {
          type: "mouseMoved",
          x: from.x + (to.x - from.x) * e,
          y: from.y + (to.y - from.y) * e,
          button: "left",
        });
        await sleep(ms / steps);
      }
      await cdp.send("Input.dispatchMouseEvent", {
        type: "mouseReleased", x: to.x, y: to.y, button: "left", clickCount: 1,
      });
    },
  };
}

/// Record the page as timestamped JPEG frames, then cut them into an mp4 whose
/// frame durations honor real wall-clock arrival times — captutor's measured
/// timeline, applied to pictures. The ffconcat approach is lifted directly from
/// captutor/bin/cdp-reel.mjs, which proved it.
export class ScreencastRecorder {
  constructor(page, { dir, fps = 30 }) {
    this.page = page;
    this.dir = dir;
    this.fps = fps;
    this.frames = [];
  }

  async start() {
    mkdirSync(this.dir, { recursive: true });
    let seq = 0;
    let firstFrame;
    const gotFirst = new Promise((res) => { firstFrame = res; });
    this.screencastParams = { format: "jpeg", quality: 88, everyNthFrame: 1 };
    this.page.on("Page.screencastFrame", ({ data, sessionId }) => {
      const file = join(this.dir, `frame-${String(seq++).padStart(6, "0")}.jpg`);
      writeFileSync(file, Buffer.from(data, "base64"));
      this.frames.push({ file, at: Date.now() / 1000 });
      this.page.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
      if (this.frames.length === 1) firstFrame();
    });
    await this.page.send("Page.startScreencast", this.screencastParams);
    await gotFirst;
    this.since = this.frames[0].at;

    // Stall watchdog. Chromium's screencast can silently stop delivering
    // frames mid-session (observed: a page that repaints every frame, and no
    // screencastFrame for 30s+). Re-issuing startScreencast revives it; a
    // static prompt costs a redundant restart at worst.
    this.watchdog = setInterval(async () => {
      const newest = this.frames.at(-1)?.at ?? 0;
      if (Date.now() / 1000 - newest > 1.5) {
        // A bare re-start is a no-op while Chrome believes the session is
        // still live — it must be stopped first to actually revive delivery.
        try {
          await this.page.send("Page.stopScreencast");
          await this.page.send("Page.startScreencast", this.screencastParams);
        } catch {}
      }
    }, 1000);
    return this.since;
  }

  async stop({ out }) {
    const stoppedAt = Date.now() / 1000;
    clearInterval(this.watchdog);
    await this.page.send("Page.stopScreencast").catch(() => {});
    await sleep(250);
    if (!this.frames.length) throw new Error("no frames captured");

    // Resample to constant frame rate against the wall clock: output tick k
    // shows whichever frame was truly on screen at since + k/fps. This maps
    // the video timeline to the same clock the beat cues were stamped with —
    // no per-gap clamping, so bursty frame arrival cannot stretch time.
    const esc = (v) => v.replaceAll("'", "'\\''");
    const ticks = Math.max(1, Math.round((stoppedAt - this.since) * this.fps));
    const concat = ["ffconcat version 1.0"];
    let p = 0;
    for (let k = 0; k < ticks; k++) {
      const t = this.since + k / this.fps;
      while (p + 1 < this.frames.length && this.frames[p + 1].at <= t) p++;
      concat.push(`file '${esc(this.frames[p].file)}'`);
      concat.push(`duration ${(1 / this.fps).toFixed(6)}`);
    }
    concat.push(`file '${esc(this.frames.at(-1).file)}'`);
    const concatFile = join(this.dir, "frames.ffconcat");
    writeFileSync(concatFile, concat.join("\n") + "\n");

    // Normalize to the exact delivery geometry: whatever size the screencast
    // captured at (it tracks viewport × dpr), scale to fit and pad to frame.
    const clip = join(this.dir, "..", "clip.mp4");
    execFileSync(FFMPEG, [
      "-y", "-loglevel", "error", "-f", "concat", "-safe", "0", "-i", concatFile,
      "-vf",
      `fps=${this.fps},scale=${out.w}:${out.h}:force_original_aspect_ratio=decrease:flags=lanczos,` +
      `pad=${out.w}:${out.h}:(ow-iw)/2:(oh-ih)/2:black,format=yuv420p`,
      "-c:v", "libx264", "-preset", "medium", "-crf", "18",
      "-movflags", "+faststart", clip,
    ]);
    return clip;
  }
}
