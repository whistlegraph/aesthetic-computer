// av-sync.mjs — is the mouth early, or is the sound?
//
// Consumer cameras and old webcam captures routinely bake in an A/V offset: the
// picture and the audio are written with timestamps that disagree by a frame or
// four, and nobody notices until you put captions under it and the lips start
// reading as dubbed.
//
// You can measure it. A talking mouth is a motion signal, and a talking voice is
// an amplitude signal, and they are the same signal — a mouth opens when a
// syllable lands. So: pull motion energy from the mouth region of every frame,
// pull an RMS envelope from the audio, and slide one against the other. The lag
// with the strongest correlation is the offset.
//
//   node marketing/lib/av-sync.mjs <video>            # just report
//   import { measureAvOffset } from "./av-sync.mjs";
//
// Returns { offsetMs, confidence, curve }.
//
//   offsetMs > 0  → the PICTURE is late (visuals behind the sound). To fix, the
//                   video must be pulled EARLIER, or the audio pushed later.
//   offsetMs < 0  → the SOUND is late.
//
// `confidence` is the peak correlation over the mean of the curve. Under about
// 1.15 the peak isn't really a peak and you should not trust the number — a shot
// with little mouth movement, or a noisy room, will not correlate.
//
// TWO THINGS THAT WILL WASTE YOUR AFTERNOON:
//
//   · Measure the RAW CLIP, never a finished reel. Music under the voice —
//     a 72bpm bed with a kick on 1 and 3 — pours energy into the envelope that
//     no mouth produced, which flattens the true peak and hands the win to a
//     spurious one. A reel that is genuinely in sync will measure as though it
//     is not.
//   · One pass lands within about a frame, not on the nose. The correlation is
//     only sampled every 33ms, so the discrete peak can sit a whole frame off
//     truth and the parabola then refines the wrong summit. On the YC clip a
//     single pass read 123ms when the answer was 95ms. If you want it exact:
//     apply the measurement, re-measure the corrected pair (bed-free), and add
//     the residual back. That converges in one step.

import { spawn } from "node:child_process";
import { faceTrack } from "./reframe.mjs";
import { decodeAudioMono, computeRmsEnvelope } from "../../pop/lib/preview-shared.mjs";

const FPS = 30;
const PROBE_W = 320; // the mouth is a big blob at this size; we want energy, not detail
const MAX_LAG = 12; // ±400ms at 30fps — beyond this it isn't a sync error, it's an edit

// Motion energy inside the mouth region, frame by frame.
async function mouthMotion(videoPath, track) {
  const scale = PROBE_W / track.srcW;
  const probeH = Math.round(track.srcH * scale);

  const dec = spawn("ffmpeg", [
    "-v", "error", "-i", videoPath,
    "-f", "rawvideo", "-pix_fmt", "gray",
    "-vf", `fps=${FPS},scale=${PROBE_W}:${probeH}`,
    "-",
  ], { stdio: ["ignore", "pipe", "inherit"] });

  const frameBytes = PROBE_W * probeH;
  const energy = [];
  let prev = null;
  let pending = Buffer.alloc(0);
  let f = 0;

  // The face track samples at its own rate; hold the nearest box for each frame.
  const boxAt = (t) => {
    let s = track.samples[0];
    for (const c of track.samples) {
      if (c.t > t) break;
      s = c;
    }
    return s;
  };

  for await (const chunk of dec.stdout) {
    pending = pending.length ? Buffer.concat([pending, chunk]) : chunk;
    while (pending.length >= frameBytes) {
      const g = pending.subarray(0, frameBytes);
      pending = pending.subarray(frameBytes);

      const b = boxAt(f / FPS);
      // The mouth lives in the bottom third of Vision's face box, centered. Using
      // the whole face would fold in head-bobs and blinks, which don't correlate
      // with speech and only add noise.
      const x0 = Math.max(0, Math.round((b.x + b.w * 0.22) * scale));
      const x1 = Math.min(PROBE_W, Math.round((b.x + b.w * 0.78) * scale));
      const y0 = Math.max(0, Math.round((b.y + b.h * 0.62) * scale));
      const y1 = Math.min(probeH, Math.round((b.y + b.h * 1.02) * scale));

      let sum = 0;
      let n = 0;
      if (prev) {
        for (let y = y0; y < y1; y += 1) {
          const row = y * PROBE_W;
          for (let x = x0; x < x1; x += 1) {
            sum += Math.abs(g[row + x] - prev[row + x]);
            n += 1;
          }
        }
      }
      energy.push(n > 0 ? sum / n : 0);
      prev = Buffer.from(g); // subarray aliases the pool; copy or it mutates
      f += 1;
    }
  }
  return energy;
}

const normalize = (xs) => {
  const mean = xs.reduce((a, b) => a + b, 0) / (xs.length || 1);
  const sd = Math.sqrt(xs.reduce((a, b) => a + (b - mean) ** 2, 0) / (xs.length || 1)) || 1;
  return xs.map((x) => (x - mean) / sd);
};

export async function measureAvOffset(videoPath, { track = null } = {}) {
  const tr = track || faceTrack(videoPath, { fps: FPS });

  const motion = normalize(await mouthMotion(videoPath, tr));
  const { audio, sr } = decodeAudioMono(videoPath);
  const env = normalize(Array.from(computeRmsEnvelope(audio, sr, FPS, tr.duration)));

  const n = Math.min(motion.length, env.length);
  const curve = [];

  // Slide the motion signal against the audio. `lag` is how many frames the
  // picture would need to move to line up: at lag k we compare motion[i + k]
  // with audio[i], so a positive k that wins means the mouth is moving LATER
  // than the sound — the picture is behind.
  for (let lag = -MAX_LAG; lag <= MAX_LAG; lag += 1) {
    let sum = 0;
    let count = 0;
    for (let i = 0; i < n; i += 1) {
      const j = i + lag;
      if (j < 0 || j >= n) continue;
      sum += motion[j] * env[i];
      count += 1;
    }
    curve.push({ lag, r: count > 0 ? sum / count : 0 });
  }

  const bestI = curve.reduce((bi, c, i) => (c.r > curve[bi].r ? i : bi), 0);
  const mean = curve.reduce((a, b) => a + Math.abs(b.r), 0) / curve.length;

  // The correlation is only sampled at whole frames, but the true offset is not
  // a whole number of frames — it's whatever the camera's encoder did. Fitting a
  // parabola through the peak and its two neighbours recovers the sub-frame
  // position of the real maximum, which matters because we correct on the audio
  // side, where we have sample precision to spend.
  let lag = curve[bestI].lag;
  const l = curve[bestI - 1];
  const c0 = curve[bestI];
  const r = curve[bestI + 1];
  if (l && r) {
    const denom = l.r - 2 * c0.r + r.r;
    if (denom !== 0) {
      const delta = (0.5 * (l.r - r.r)) / denom;
      if (Math.abs(delta) <= 1) lag += delta;
    }
  }

  return {
    offsetMs: Math.round((lag / FPS) * 1000),
    confidence: mean > 0 ? c0.r / mean : 0,
    curve,
  };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const video = process.argv[2];
  if (!video) {
    console.error("usage: av-sync.mjs <video>");
    process.exit(2);
  }
  const { offsetMs, confidence, curve } = await measureAvOffset(video);

  console.log("\nlag(ms)  correlation");
  for (const c of curve) {
    const ms = Math.round((c.lag / FPS) * 1000);
    const bar = "█".repeat(Math.max(0, Math.round(c.r * 60)));
    console.log(`${String(ms).padStart(5)}   ${bar} ${c.r.toFixed(3)}`);
  }
  console.log(`\noffset: ${offsetMs}ms   confidence: ${confidence.toFixed(2)}`);
  console.log(
    offsetMs > 0
      ? `the PICTURE is ${offsetMs}ms late — visuals behind the sound`
      : offsetMs < 0
        ? `the SOUND is ${-offsetMs}ms late`
        : "in sync",
  );
  if (confidence < 1.15) console.log("⚠ weak peak — don't trust this; too little mouth motion or too much noise");
}
