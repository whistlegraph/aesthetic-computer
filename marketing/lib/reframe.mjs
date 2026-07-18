// reframe.mjs — turn a jittery face track into a calm vertical crop.
//
// Vision tells us where the face is on every sampled frame. Pointing the crop
// straight at those numbers looks awful: the box twitches every frame and the
// footage reads as handheld-by-a-nervous-person. What we want is a camera
// operator — someone who holds still, and when the subject really does move,
// follows late and settles soft.
//
// So the raw boxes go through four stages, in this order:
//
//   1. resample  — Vision samples at its own rate; we need one box per frame.
//   2. median    — kill single-frame outliers (a hand near the chin, a blink of
//                  a false positive) without smearing real motion.
//   3. deadband  — inside a slack radius the camera simply does not move. This
//                  is what buys the "locked off" feel; smoothing alone can't.
//   4. spring    — outside the slack, chase the target with a critically damped
//                  spring, so it eases in and eases out and never overshoots.
//
// Clamping happens last, against the source edges, so the crop can never show
// letterbox.
//
//   import { faceTrack, buildReframer } from "../lib/reframe.mjs";
//   const track = await faceTrack("clip.mov");
//   const at = buildReframer(track, { w: 1080, h: 1920, fps: 30 });
//   const { sx, sy, sw, sh } = at(t);   // source rect to draw for time t

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const DETECTOR = join(HERE, "face-track.swift");

// Vision costs real seconds, and a track only depends on the file and the
// sample rate — so it caches. Delete the json to force a re-detect.
export function faceTrack(videoPath, { fps = 10, cachePath = null } = {}) {
  const cache = cachePath || join(HERE, "..", "talking-head", "out", ".track.json");
  if (existsSync(cache)) {
    const cached = JSON.parse(readFileSync(cache, "utf8"));
    if (cached.video === videoPath && cached.sampleFps === fps) return cached;
  }
  const json = execFileSync("swift", [DETECTOR, videoPath, "--fps", String(fps)], {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
    stdio: ["ignore", "pipe", "ignore"], // swift's deprecation warnings are noise
  });
  const track = JSON.parse(json);
  mkdirSync(dirname(cache), { recursive: true });
  writeFileSync(cache, json);
  return track;
}

const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));

function medianOf(values) {
  const s = [...values].sort((a, b) => a - b);
  return s[s.length >> 1];
}

export function buildReframer(
  track,
  {
    w = 1080, // output width
    h = 1920, // output height
    fps = 30,
    zoom = 1.0, // 1.0 = full source height (sharpest); >1 pushes in on the face
    headroom = 0.42, // where the face center sits vertically, 0 = top, 1 = bottom
    slack = 26, // deadband radius in source px — under this, the camera holds
    stiffness = 2.6, // spring rate; lower is lazier
    medianWindow = 5,
  } = {},
) {
  const { srcW, srcH, duration, samples } = track;

  // The crop is the tallest 9:16 box that fits, divided by zoom.
  const aspect = w / h;
  let sh = Math.min(srcH, srcW / aspect) / zoom;
  let sw = sh * aspect;
  if (sw > srcW) {
    sw = srcW;
    sh = sw / aspect;
  }

  const frames = Math.max(1, Math.round(duration * fps));

  // 1. Resample onto the frame grid. Faces can vanish for a beat (a turn of the
  //    head, a hand across the mouth); we hold the last good center rather than
  //    snapping to frame center, which would look like a flinch.
  const targets = new Array(frames);
  let cursor = 0;
  let lastKnown =
    samples.length > 0
      ? { cx: samples[0].x + samples[0].w / 2, cy: samples[0].y + samples[0].h / 2 }
      : { cx: srcW / 2, cy: srcH / 2 };

  for (let f = 0; f < frames; f += 1) {
    const t = f / fps;
    while (cursor < samples.length - 1 && samples[cursor + 1].t <= t) cursor += 1;
    const a = samples[cursor];
    const b = samples[cursor + 1];
    if (a && b && b.t > a.t && t >= a.t) {
      const k = clamp((t - a.t) / (b.t - a.t), 0, 1);
      const acx = a.x + a.w / 2;
      const bcx = b.x + b.w / 2;
      const acy = a.y + a.h / 2;
      const bcy = b.y + b.h / 2;
      lastKnown = { cx: acx + (bcx - acx) * k, cy: acy + (bcy - acy) * k };
    } else if (a) {
      lastKnown = { cx: a.x + a.w / 2, cy: a.y + a.h / 2 };
    }
    targets[f] = { ...lastKnown };
  }

  // 2. Median filter. A mean would drag the camera toward outliers; a median
  //    ignores them outright while leaving sustained motion intact.
  const half = medianWindow >> 1;
  const smoothed = targets.map((_, f) => {
    const lo = Math.max(0, f - half);
    const hi = Math.min(frames - 1, f + half);
    const window = targets.slice(lo, hi + 1);
    return {
      cx: medianOf(window.map((p) => p.cx)),
      cy: medianOf(window.map((p) => p.cy)),
    };
  });

  // 3 + 4. Deadband, then spring. Integrated forward once, at build time, so
  //        that lookups are a plain array read and the pan is identical on
  //        every render.
  const dt = 1 / fps;
  const damping = 2 * Math.sqrt(stiffness); // critical damping: no overshoot
  const path = new Array(frames);
  let px = smoothed[0].cx;
  let py = smoothed[0].cy;
  let vx = 0;
  let vy = 0;

  for (let f = 0; f < frames; f += 1) {
    const { cx, cy } = smoothed[f];

    // Only aim at the part of the error that escapes the slack radius. Inside
    // it the target *is* where we already are, so the spring has nothing to do.
    const ex = cx - px;
    const ey = cy - py;
    const dist = Math.hypot(ex, ey);
    let aimX = px;
    let aimY = py;
    if (dist > slack) {
      const pull = (dist - slack) / dist;
      aimX = px + ex * pull;
      aimY = py + ey * pull;
    }

    vx += (stiffness * (aimX - px) - damping * vx) * dt;
    vy += (stiffness * (aimY - py) - damping * vy) * dt;
    px += vx * dt;
    py += vy * dt;

    // The face sits at `headroom` down the frame, not dead center — centering a
    // head vertically always looks like it's sinking.
    const sx = clamp(px - sw / 2, 0, srcW - sw);
    const sy = clamp(py - sh * headroom, 0, srcH - sh);
    path[f] = { sx, sy, sw, sh };
  }

  return function at(t) {
    const f = clamp(Math.round(t * fps), 0, frames - 1);
    return path[f];
  };
}
