// frame-tape.mjs — record a short HQ video of a fleet Mac's screen (or a
// sub-region of it): the fixed-length-clip sibling of a single `frame`.
//
//   frame <machine>          → one still  (jpeg + OCR + AX)
//   reel  <machine>          → an open-ended clip you start and stop by hand
//   tape  <machine> <secs>   → a fixed-length clip, cropped, probed, returned
//
// WHY this drives `reel` instead of shelling `screencapture -v`/ffmpeg over ssh:
// the Screen Recording (TCC) grant belongs to the SlabMenubar app, which lives
// in the GUI session where an ssh shell cannot reach the WindowServer. A bare
// `screencapture -v` from an ssh session fails the same way a bare screenshot
// does. `reel` already pokes SlabMenubar (ScreenRecord.swift → SCStream →
// SCRecordingOutput → mp4), so we drive that and inherit the working grant,
// hardware h264, and the moov-flush handshake — fleet-wide, local or remote.
//
// reel films the whole display (or a named window); it has no arbitrary crop
// rect. So a sub-region tape records the full display through reel, then crops
// locally with ffmpeg. The crop arrives in global screen POINTS (the same
// convention frame's --crop uses); reel's mp4 is pixels clamped to a 2560 long
// edge, so we scale points→pixels by the mp4/point size ratio before cropping.

import { execFile } from "node:child_process";
import { existsSync, mkdirSync, rmSync, statSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = join(HERE, "..", "..");
const REEL = join(HERE, "..", "bin", "reel.mjs");
const FRAME = join(HERE, "..", "bin", "frame.mjs");
const TAPES_DIR = join(homedir(), ".local", "share", "slab", "tapes");

// h264's hardware encoder and plain practicality both argue against long takes.
const MAX_DURATION = 60;

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Prefer the repo's QoS shim (utility priority so a render never starves the UI)
// but resolve an absolute path so this works from a daemon with a bare PATH.
function bin(name) {
  for (const p of [join(REPO, "toolchain", "shims", name),
                   `/opt/homebrew/bin/${name}`, `/usr/local/bin/${name}`]) {
    if (existsSync(p)) return p;
  }
  return name;
}
const FFMPEG = bin("ffmpeg");
const FFPROBE = bin("ffprobe");

function run(cmd, args, { timeoutMs } = {}) {
  return new Promise((res, rej) => {
    execFile(cmd, args, { timeout: timeoutMs, maxBuffer: 16 * 1024 * 1024, encoding: "utf8" },
      (err, stdout, stderr) => {
        if (err) return rej(new Error((stderr || err.message || String(err)).trim()));
        res({ stdout, stderr });
      });
  });
}
const reel = (args, o) => run(process.execPath, [REEL, ...args], o);

// The crop rect is in screen points; reel's mp4 is pixels. Read the display's
// point size from a lightweight frame envelope (meta needs no capture grant) so
// we can map one onto the other.
async function screenPoints(machine) {
  mkdirSync(TAPES_DIR, { recursive: true });
  const tmp = join(TAPES_DIR, `.geo-${process.pid}.jpg`);
  try {
    const { stdout } = await run(process.execPath,
      [FRAME, machine, "--screen", "--no-ocr", "--json", "--out", tmp], { timeoutMs: 30000 });
    const s = JSON.parse(stdout)?.meta?.screen;
    if (!s || !Number.isFinite(s.w) || !Number.isFinite(s.h)) {
      throw new Error(`no screen geometry from ${machine} (frame_doctor ${machine})`);
    }
    return { w: s.w, h: s.h };
  } finally {
    rmSync(tmp, { force: true });
  }
}

async function probe(path) {
  const { stdout } = await run(FFPROBE, ["-v", "error", "-select_streams", "v:0",
    "-show_entries", "stream=width,height,avg_frame_rate",
    "-show_entries", "format=duration", "-of", "json", path]);
  const p = JSON.parse(stdout);
  const st = p.streams?.[0] || {};
  const [num, den] = String(st.avg_frame_rate || "0/1").split("/").map(Number);
  return {
    width: st.width, height: st.height,
    fps: den ? Math.round((num / den) * 100) / 100 : null,
    duration: p.format?.duration != null ? Math.round(Number(p.format.duration) * 100) / 100 : null,
    bytes: statSync(path).size,
  };
}

// points → mp4 pixels; clamped to the frame, rounded to even (yuv420p needs it).
// Returns ffmpeg crop-filter order: w:h:x:y.
function pixelCrop([px, py, pw, ph], points, mp4) {
  const sx = mp4.width / points.w, sy = mp4.height / points.h;
  const even = (n) => Math.max(0, Math.floor(n / 2) * 2);
  let x = even(Math.max(0, px) * sx);
  let y = even(Math.max(0, py) * sy);
  let w = Math.max(2, even(pw * sx));
  let h = Math.max(2, even(ph * sy));
  if (x + w > mp4.width) w = Math.max(2, even(mp4.width - x));
  if (y + h > mp4.height) h = Math.max(2, even(mp4.height - y));
  return [w, h, x, y];
}

function slugify(label) {
  return String(label).toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "");
}

// Record `duration`s of `machine`'s screen (whole display, or `crop`=[x,y,w,h]
// in global points) at ~`fps`, save an mp4 to `out`, and return its metadata.
// The video is NEVER returned inline — callers read it from `path`.
export async function recordTape({ machine, duration = 6, crop, fps = 30,
                                   out, cursor = false, label } = {}) {
  if (!machine) throw new Error("`machine` is required (see frame_list)");
  duration = Math.min(MAX_DURATION, Math.max(0.5, Number(duration) || 6));
  fps = Math.min(60, Math.max(1, Math.round(Number(fps) || 30)));
  if (crop != null && (!Array.isArray(crop) || crop.length !== 4 || !crop.every(Number.isFinite))) {
    throw new Error("crop must be [x,y,w,h] in global screen points");
  }

  mkdirSync(TAPES_DIR, { recursive: true });
  const stamp = new Date().toISOString().replace(/[:.]/g, "-");
  const tag = label ? `-${slugify(label)}` : "";
  const finalOut = out ? resolve(out) : join(TAPES_DIR, `${machine}${tag}-${stamp}.mp4`);
  mkdirSync(dirname(finalOut), { recursive: true });
  // No crop → reel writes the deliverable straight to finalOut, so the hardware
  // h264 is never re-encoded. A crop records a full-display temp first.
  const rawOut = crop ? join(TAPES_DIR, `.raw-${process.pid}-${stamp}.mp4`) : finalOut;

  // Resolve geometry BEFORE recording so a bad crop fails fast, not after we've
  // tied up the recorder for `duration` seconds.
  const points = crop ? await screenPoints(machine) : null;

  await reel(["start", machine, "--fps", String(fps), ...(cursor ? ["--cursor"] : [])],
    { timeoutMs: 30000 });
  try {
    await sleep(duration * 1000);
  } finally {
    // Always stop — a dangling SCStream wedges the next recording ("already
    // recording"). reel's stop also pulls the file back and waits for the moov
    // atom to flush, so what lands at rawOut is complete.
    await reel(["stop", machine, "--out", rawOut], { timeoutMs: 30000 });
  }
  if (!existsSync(rawOut)) {
    throw new Error(`reel produced no file on ${machine} — is SlabMenubar running? (frame_doctor ${machine})`);
  }

  let region = null;
  if (crop) {
    const raw = await probe(rawOut);
    const [w, h, x, y] = pixelCrop(crop, points, raw);
    await run(FFMPEG, ["-hide_banner", "-loglevel", "error", "-y", "-i", rawOut,
      "-filter:v", `crop=${w}:${h}:${x}:${y}`,
      "-c:v", "libx264", "-preset", "veryfast", "-crf", "20",
      "-pix_fmt", "yuv420p", "-movflags", "+faststart", finalOut],
      { timeoutMs: Math.max(60000, duration * 4000) });
    rmSync(rawOut, { force: true });
    region = { x: crop[0], y: crop[1], w: crop[2], h: crop[3], units: "points" };
  }

  const meta = await probe(finalOut);
  return {
    path: finalOut,
    machine,
    scope: crop ? "region" : "display",
    region,
    duration: meta.duration,
    requestedDuration: duration,
    fps: meta.fps,
    requestedFps: fps,
    width: meta.width,
    height: meta.height,
    bytes: meta.bytes,
    cursor,
    label: label || null,
  };
}
