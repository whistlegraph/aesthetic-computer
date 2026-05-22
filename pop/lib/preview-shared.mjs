// pop/lib/preview-shared.mjs — shared rendering primitives for pop
// preview videos. All lanes (dance, chillwave, future) import from
// here so YWFT typography, waveform-inside-events, ken-burns, audio
// envelope extraction, and ffmpeg encoding live in one place.
//
// Identity (palette, section names, lane keys, structural pacing) is
// kept per-lane in the lane's own bin/preview-*.mjs.
//
// Canonical reference for the *shape* of these primitives is
// pop/dance/bin/cover-video.mjs (trancenwaltz). When a primitive
// evolves, evolve it here and pull across all lanes.

import { spawnSync, spawn } from "node:child_process";
import { mkdirSync, existsSync } from "node:fs";
import { homedir } from "node:os";
import { loadImage } from "canvas";

export const YWFT_PATH = `${homedir()}/Library/Fonts/ywft-processing-bold.ttf`;
export const AUDIO_SR_DEFAULT = 4000;       // 4 kHz mono is enough for waveform display
export const ENV_FPS_DEFAULT = 60;          // RMS envelope at 60 Hz drives bounces

// ── variable type: the shared preview pipeline's typeface is now a
// pipeline-level setting, not a hardcoded constant. tracks can pick a
// different face (e.g. a girly, less-"computery" one) via setPreviewFont.
const SYS = "/System/Library/Fonts/Supplemental";
export const FONTS = {
  ywft:    YWFT_PATH,                              // default — geometric/computery
  girly:   `${SYS}/Bradley Hand Bold.ttf`,         // handwritten, soft, girly
  rounded: `${SYS}/ChalkboardSE.ttc`,              // rounded playful
  party:   `${SYS}/PartyLET-plain.ttf`,            // bubbly
  script:  `${SYS}/SnellRoundhand.ttc`,            // elegant script
};
let activeFont = YWFT_PATH;
export function setPreviewFont(nameOrPath) {
  const p = FONTS[nameOrPath] || nameOrPath || YWFT_PATH;
  activeFont = existsSync(p) ? p : YWFT_PATH;
  return activeFont;
}
export function getPreviewFont() { return activeFont; }

export function checkYwftAvailable() {
  if (!existsSync(activeFont)) {
    throw new Error(
      `preview font not found at ${activeFont}\n` +
      `install it, or pick another via setPreviewFont()`,
    );
  }
}

// ── audio decode + envelope ──────────────────────────────────────────
// Decode an audio file to mono Float32 at AUDIO_SR_DEFAULT.
// Returns { audio: Float32Array, sr, audioPeak }.
export function decodeAudioMono(audioPath, sr = AUDIO_SR_DEFAULT) {
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-loglevel", "error",
    "-i", audioPath,
    "-f", "f32le", "-ar", String(sr), "-ac", "1",
    "-",
  ], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
  if (r.status !== 0 || !r.stdout) {
    throw new Error(`audio decode failed: ${r.stderr?.toString().slice(0, 400)}`);
  }
  const audio = new Float32Array(
    r.stdout.buffer, r.stdout.byteOffset, r.stdout.byteLength / 4
  );
  let audioPeak = 1e-6;
  for (let i = 0; i < audio.length; i++) {
    const a = Math.abs(audio[i]);
    if (a > audioPeak) audioPeak = a;
  }
  return { audio, sr, audioPeak };
}

// Build a per-output-frame RMS envelope from a Float32Array of mono
// audio at `audioSr`. Window is ~30ms centered on each frame.
// Returns a normalized Float32Array (peak=1).
export function computeRmsEnvelope(audio, audioSr, outFps, durationSec) {
  const length = Math.ceil(durationSec * outFps) + 4;
  const out = new Float32Array(length);
  const winSamples = Math.floor(0.030 * audioSr);
  const stepSamples = audioSr / outFps;
  for (let i = 0; i < length; i++) {
    const center = Math.floor(i * stepSamples);
    let sum = 0, n = 0;
    for (let j = -winSamples; j <= winSamples; j++) {
      const idx = center + j;
      if (idx < 0 || idx >= audio.length) continue;
      sum += audio[idx] * audio[idx];
      n++;
    }
    out[i] = Math.sqrt(sum / Math.max(1, n));
  }
  let envMax = 0;
  for (let i = 0; i < length; i++) if (out[i] > envMax) envMax = out[i];
  if (envMax > 0) for (let i = 0; i < length; i++) out[i] /= envMax;
  return out;
}

// ── YWFT typography (via ImageMagick) ────────────────────────────────
// Cairo/fontconfig can't resolve the YWFT TTF — we pre-rasterize text
// to PNGs and composite per-frame.

// Width in pixels of a YWFT label at `ptSize`.
export function magickMeasureWidth(text, ptSize) {
  const r = spawnSync("magick", [
    "-font", activeFont,
    "-pointsize", String(ptSize),
    `label:${text}`,
    "-format", "%w",
    "info:",
  ], { encoding: "utf8" });
  if (r.status !== 0) return 0;
  return parseInt(r.stdout.trim(), 10) || 0;
}

// Render `text` as a transparent PNG with YWFT-Processing-Bold.
// opts: { ptSize, fill, shadow?, padX=0, padY=0, outPath }
// Returns a loaded Image.
export async function magickRenderText(text, opts) {
  const { ptSize, fill, shadow, padX = 0, padY = 0, outPath } = opts;
  mkdirSync(outPath.replace(/\/[^\/]+$/, ""), { recursive: true });
  const args = [
    "-background", "none",
    "-fill", fill,
    "-font", activeFont,
    "-pointsize", String(ptSize),
    `label:${text}`,
  ];
  if (shadow) {
    args.push("(", "-clone", "0",
      "-fill", shadow,
      "-background", "none",
      "-shadow", "100x0+2+2", "+swap",
      "-background", "none",
      "-flatten", ")");
  }
  if (padX || padY) {
    args.push("-bordercolor", "none", "-border", `${padX}x${padY}`);
  }
  args.push(outPath);
  const r = spawnSync("magick", args);
  if (r.status !== 0) throw new Error(`magick failed rendering: ${text}`);
  return await loadImage(outPath);
}

// Render a label with a colored background block (for lane labels).
// opts: { ptSize, fill, bg, padX=8, padY=4, outPath }
export async function magickRenderLabel(text, opts) {
  const { ptSize, fill, bg, padX = 8, padY = 4, outPath } = opts;
  mkdirSync(outPath.replace(/\/[^\/]+$/, ""), { recursive: true });
  const r = spawnSync("magick", [
    "-background", bg,
    "-fill", fill,
    "-font", activeFont,
    "-pointsize", String(ptSize),
    `label:${text}`,
    "-bordercolor", bg,
    "-border", `${padX}x${padY}`,
    outPath,
  ]);
  if (r.status !== 0) throw new Error(`magick failed rendering label: ${text}`);
  return await loadImage(outPath);
}

// Pre-render every character of a title with the YWFT font.
// palette is a cycled list of fill colors; each char gets palette[i % len].
// Returns [{ char, img, prefixWidth }].
export async function prerenderTitleChars({ text, ptSize, palette, shadowColor, assetsDir }) {
  mkdirSync(assetsDir, { recursive: true });
  const chars = text.toLowerCase().split("");
  const out = [];
  let cum = "";
  let prefix = 0;
  for (let i = 0; i < chars.length; i++) {
    const ch = chars[i];
    let img = null;
    if (ch.trim().length > 0) {
      const color = palette[i % palette.length];
      const charBoxW = Math.ceil(ptSize * 1.4);
      const charBoxH = Math.ceil(ptSize * 1.7);
      const charBaselineY = Math.ceil(ptSize * 1.25);
      const outPath = `${assetsDir}/titlechar.${i}.png`;
      const args = [
        "-size", `${charBoxW}x${charBoxH}`,
        "xc:none",
        "-font", activeFont,
        "-pointsize", String(ptSize),
      ];
      if (shadowColor) {
        args.push("-fill", shadowColor, "-annotate", `+8+${charBaselineY + 5}`, ch);
      }
      args.push("-fill", color, "-annotate", `+3+${charBaselineY}`, ch, outPath);
      const r = spawnSync("magick", args);
      if (r.status !== 0) throw new Error(`magick title char '${ch}' failed`);
      img = await loadImage(outPath);
    }
    cum += ch;
    const cumWidth = magickMeasureWidth(cum, ptSize) || prefix + ptSize * 0.45;
    out.push({ char: ch, img, prefixWidth: prefix });
    prefix = cumWidth;
  }
  return { chars: out, totalWidth: prefix };
}

// ── cover ken-burns ──────────────────────────────────────────────────
// Matches the trancenwaltz cover-video.mjs camera:
//   • cover-fit + ~20% overshoot so the image always covers the canvas
//   • slow zoom "breath" — cosine oscillation across breathPeriodSec
//   • multi-frequency sin/cos wobble with envelope-driven boost
// opts: { baseScale=1.20, breathAmp=0.06, breathPeriodSec=18, env=0,
//         wobbleAmp=14, envWobbleAmp=18,
//         offsetX=0, offsetY=0, alpha=1, fillBackground=true }
// offsetX/offsetY are applied AFTER the cover-fit clamp (intentionally
// uncalmped) so a section can slide on/off canvas — used by the Star
// Wars-style slide+fade transition between per-section illustrations.
export function drawCoverKenBurns(ctx, cover, t, opts = {}) {
  const W = ctx.canvas.width, H = ctx.canvas.height;
  const baseScale = opts.baseScale ?? 1.20;
  const breathAmp = opts.breathAmp ?? 0.06;
  const breathT   = opts.breathPeriodSec ?? 18;
  const env       = opts.env ?? 0;
  const wAmp      = opts.wobbleAmp ?? 14;
  const wEnv      = opts.envWobbleAmp ?? 18;
  const offsetX   = opts.offsetX ?? 0;
  const offsetY   = opts.offsetY ?? 0;
  const alpha     = opts.alpha ?? 1;
  const fillBg    = opts.fillBackground ?? true;
  // `punch` (0..1) — a kick-drum-synced "snap": tiny zoom pop + a
  // self-composited overlay pass that lifts local contrast so the
  // whole illustration appears to sharpen on each kick.
  const punch     = Math.max(0, Math.min(1, opts.punch ?? 0));

  // ── mirror-tile mode — show the WHOLE illy un-cropped ──────────────
  // Fit the illustration with slack in BOTH axes, mirror-tile it across
  // the frame in a grid, and drift it gently in x AND y so its edges
  // read instead of being zoom-cropped away. The focal centre tile is
  // sharp; every surrounding mirror tile is BLURRED — a soft reflected
  // wrap. Tile sizes are integers and positions rounded, so the seams
  // butt exactly and never show a hairline gap. (opts.mirrorTile)
  if (opts.mirrorTile) {
    const fitFrac = opts.tileFit ?? 0.86;     // <1 → slack for x/y drift
    const fit = Math.min(W / cover.width, H / cover.height) * fitFrac;
    const dw = Math.round(cover.width * fit);
    const dh = Math.round(cover.height * fit);
    const xSlack = Math.max(0, W - dw), ySlack = Math.max(0, H - dh);
    const driftX = (xSlack * 0.5) * Math.sin(t * (2 * Math.PI / 26))
                 + env * 18 * Math.sin(t * 2.6);
    const driftY = (ySlack * 0.5) * Math.sin(t * (2 * Math.PI / 20))
                 + env * 22 * Math.sin(t * 3.1);
    const x0 = (W - dw) / 2 + driftX + offsetX;
    const y0 = (H - dh) / 2 + driftY + offsetY;
    const tileBlur = opts.tileBlur ?? 18;
    if (fillBg) { ctx.fillStyle = "#000"; ctx.fillRect(0, 0, W, H); }
    ctx.save();
    if (alpha < 1) ctx.globalAlpha = Math.max(0, Math.min(1, alpha));
    // one grid tile — flipped per its grid parity so every seam mirrors.
    const tile = (gx, gy) => {
      const px = Math.round(x0 + gx * dw);
      const py = Math.round(y0 + gy * dh);
      if (px >= W || py >= H || px + dw <= 0 || py + dh <= 0) return;
      const focal = gx === 0 && gy === 0;
      const fX = (gx & 1) !== 0, fY = (gy & 1) !== 0;
      ctx.save();
      if (!focal && tileBlur > 0) ctx.filter = `blur(${tileBlur}px)`;
      ctx.translate(fX ? px + dw : px, fY ? py + dh : py);
      ctx.scale(fX ? -1 : 1, fY ? -1 : 1);
      ctx.drawImage(cover, 0, 0, dw, dh);
      ctx.restore();
    };
    const giMin = Math.floor(-x0 / dw) - 1, giMax = Math.ceil((W - x0) / dw) + 1;
    const gjMin = Math.floor(-y0 / dh) - 1, gjMax = Math.ceil((H - y0) / dh) + 1;
    // blurred mirror tiles first, then the sharp focal tile on top.
    for (let gx = giMin; gx <= giMax; gx++)
      for (let gy = gjMin; gy <= gjMax; gy++)
        if (!(gx === 0 && gy === 0)) tile(gx, gy);
    tile(0, 0);
    ctx.restore();
    if (punch > 0.01) {
      ctx.save();
      ctx.globalCompositeOperation = "overlay";
      ctx.globalAlpha = punch * 0.22;
      ctx.drawImage(cover, Math.round(x0), Math.round(y0), dw, dh);
      ctx.restore();
    }
    return { x: x0, y: y0, scale: dw / cover.width };
  }

  // ── zoom mode — punch WAY into a figure's bounding box ─────────────
  // opts.zoomBox = {x,y,w,h} in panel-image coords. Cover-scales the
  // illy so that box fills the frame — a close-up on the figure — with
  // a gentle breath + wobble drift.
  if (opts.zoomBox) {
    const zb = opts.zoomBox;
    const pad = opts.zoomPad ?? 1.12;       // >1 → a little room around the box
    const breathZ = breathAmp * 0.5 * (0.5 - 0.5 * Math.cos(t * (2 * Math.PI / breathT)));
    const sc = Math.max(W / (zb.w * pad), H / (zb.h * pad)) * (1 + breathZ + punch * 0.02);
    const cx = (zb.x + zb.w / 2) + (12 * Math.sin(t * 1.3) + env * 16 * Math.sin(t * 3.4)) / sc;
    const cy = (zb.y + zb.h / 2) + (9 * Math.cos(t * 1.6) + env * 12 * Math.sin(t * 2.5)) / sc;
    let zx = W / 2 - cx * sc + offsetX;
    let zy = H / 2 - cy * sc + offsetY;
    const cw = cover.width * sc, chh = cover.height * sc;
    zx = Math.min(0, Math.max(W - cw, zx));   // keep the frame covered
    zy = Math.min(0, Math.max(H - chh, zy));
    if (fillBg) { ctx.fillStyle = "#000"; ctx.fillRect(0, 0, W, H); }
    ctx.save();
    if (alpha < 1) ctx.globalAlpha = Math.max(0, Math.min(1, alpha));
    ctx.drawImage(cover, zx, zy, cw, chh);
    ctx.restore();
    if (punch > 0.01) {
      ctx.save();
      ctx.globalCompositeOperation = "overlay";
      ctx.globalAlpha = punch * 0.22;
      ctx.drawImage(cover, zx, zy, cw, chh);
      ctx.restore();
    }
    return { x: zx, y: zy, scale: sc };
  }

  const coverScale = Math.max(W / cover.width, H / cover.height);
  const breath = breathAmp * (0.5 - 0.5 * Math.cos(t * (2 * Math.PI / breathT)));
  const zoom = baseScale + breath + punch * 0.018;
  const baseW = cover.width * coverScale * zoom;
  const baseH = cover.height * coverScale * zoom;
  const wobbleX = wAmp     * Math.sin(t * 1.35)
                + wAmp * 0.45 * Math.cos(t * 2.7)
                + env * wEnv * Math.sin(t * 4.0);
  const wobbleY = wAmp * 0.78 * Math.cos(t * 1.55)
                + wAmp * 0.35 * Math.sin(t * 2.3)
                + env * wEnv * 0.78 * Math.cos(t * 3.3);
  let drawX = (W - baseW) / 2 + wobbleX;
  let drawY = (H - baseH) / 2 + wobbleY;
  // Clamp so the canvas always stays covered…
  drawX = Math.min(0, Math.max(W - baseW, drawX));
  drawY = Math.min(0, Math.max(H - baseH, drawY));
  // …then apply the (uncalmped) transition slide on top.
  drawX += offsetX;
  drawY += offsetY;
  if (fillBg) {
    ctx.fillStyle = "#000";
    ctx.fillRect(0, 0, W, H);
  }
  if (alpha < 1) {
    ctx.save();
    ctx.globalAlpha = Math.max(0, Math.min(1, alpha));
    ctx.drawImage(cover, drawX, drawY, baseW, baseH);
    ctx.restore();
  } else {
    ctx.drawImage(cover, drawX, drawY, baseW, baseH);
  }
  // kick "sharpen" punch — re-stamp the same frame with an overlay
  // blend at low alpha (mid-tone local-contrast boost ≈ unsharp) so
  // the picture cracks into focus on the beat, then settles.
  if (punch > 0.01) {
    ctx.save();
    ctx.globalCompositeOperation = "overlay";
    ctx.globalAlpha = punch * 0.22;
    ctx.drawImage(cover, drawX, drawY, baseW, baseH);
    ctx.restore();
  }
  return { x: drawX, y: drawY, scale: coverScale * zoom };
}

// ── backlit illumination on the form bounding boxes ──────────────────
// Casts a soft additive glow at each figure's bbox (a "backlit" halo
// that the figures appear to be lit by). `forms` are bboxes in panel-
// image coords; `xform` is the {x,y,scale} returned by drawCoverKenBurns
// (panel → frame mapping). `intensity` 0..1 pulses it on the audio.
export function drawFormBacklight(ctx, forms, xform, intensity, rgb = "255,236,196") {
  if (!forms || !forms.length || !xform || intensity <= 0.01) return;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const f of forms) {
    const cx = xform.x + (f.x + f.w / 2) * xform.scale;
    const cy = xform.y + (f.y + f.h / 2) * xform.scale;
    const r  = Math.max(f.w, f.h) * xform.scale * 0.72;
    if (r <= 1) continue;
    const g = ctx.createRadialGradient(cx, cy, r * 0.12, cx, cy, r);
    g.addColorStop(0,   `rgba(${rgb},${(0.55 * intensity).toFixed(3)})`);
    g.addColorStop(0.6, `rgba(${rgb},${(0.18 * intensity).toFixed(3)})`);
    g.addColorStop(1,   `rgba(${rgb},0)`);
    ctx.fillStyle = g;
    ctx.fillRect(cx - r, cy - r, r * 2, r * 2);
  }
  ctx.restore();
}

// ── per-character bouncing title (audio-envelope visualizer) ─────────
// Each character is a vertical amplitude bar driven by the audio
// envelope sampled at audioT - i*charDelay — so chars to the right
// show slightly older envelope frames, and the title moves like a
// flowing spectrum-bar visualizer rather than a beat-clock sinusoid.
// Brightness + glow shadow blur scale with each char's envelope so
// loud moments make the title both jump AND brighten.
//
// opts: { chars, ptSize, baseX, baseY, audioT, env, getEnvAt?,
//         charDelay=0.025, bounceAmp=80, restAlpha=0.55,
//         glowThreshold=0.45, glowMax=18, wobbleAmp=3 }
// `getEnvAt(t)` (optional): returns normalized envelope at any t. If
// omitted, the static `env` is used for all chars.
export function drawTitleBounce(ctx, {
  chars, ptSize, baseX, baseY, audioT, env,
  getEnvAt, charDelay = 0.025, bounceAmp = 80,
  restAlpha = 0.55, glowThreshold = 0.45, glowMax = 18, wobbleAmp = 3,
}) {
  const charBaselineY = Math.ceil(ptSize * 1.25);
  const totalChars = chars.length;
  for (let i = 0; i < totalChars; i++) {
    const ch = chars[i];
    if (!ch.img) continue;
    const charT = audioT - i * charDelay;
    const charEnv = getEnvAt ? getEnvAt(charT) : env;
    // amplitude bar: char rises proportionally to envelope
    const lift = bounceAmp * charEnv;
    // small wobble keeps the title alive even during quiet
    const wobble = wobbleAmp * Math.sin(audioT * 6.0 + i * 1.3) * (0.3 + charEnv);
    const x = baseX + ch.prefixWidth - 3;
    const y = baseY - charBaselineY - lift + wobble;
    // alpha: dim at rest, full at peak
    const alpha = restAlpha + (1 - restAlpha) * charEnv;
    // glow only on loud chars (per-char shadowBlur is cheap when sparse)
    if (charEnv > glowThreshold) {
      ctx.save();
      ctx.globalAlpha = alpha;
      ctx.shadowColor = "rgba(255,255,180,0.95)";
      ctx.shadowBlur = glowMax * Math.min(1, (charEnv - glowThreshold) / (1 - glowThreshold));
      ctx.drawImage(ch.img, x, y);
      ctx.restore();
    } else {
      ctx.globalAlpha = alpha;
      ctx.drawImage(ch.img, x, y);
      ctx.globalAlpha = 1;
    }
  }
}

// ── waveform inside an event rectangle ───────────────────────────────
// Draws a tiny mini-DAW waveform of the audio between [t0, t1] inside
// the rect (x, y, w, h). audio: Float32Array mono; audioSr: rate.
// audioPeak: from decodeAudioMono. color: rgba string for the waveform.
// alpha: overall opacity 0..1.
export function drawEventWaveform(ctx, audio, audioSr, audioPeak, x, y, w, h, t0, t1, color, alpha = 1) {
  if (w <= 1 || h <= 1) return;
  const startSamp = Math.max(0, Math.floor(t0 * audioSr));
  const endSamp   = Math.min(audio.length - 1, Math.floor(t1 * audioSr));
  if (endSamp <= startSamp) return;
  const cols = Math.max(2, Math.floor(w));
  const samplesPerCol = (endSamp - startSamp) / cols;
  ctx.save();
  ctx.globalAlpha = alpha;
  ctx.strokeStyle = color;
  ctx.lineWidth = 1;
  const midY = y + h / 2;
  ctx.beginPath();
  for (let c = 0; c < cols; c++) {
    const s0 = startSamp + Math.floor(c * samplesPerCol);
    const s1 = Math.min(endSamp, startSamp + Math.floor((c + 1) * samplesPerCol));
    let peak = 0;
    for (let s = s0; s < s1; s++) {
      const a = Math.abs(audio[s]);
      if (a > peak) peak = a;
    }
    const norm = peak / audioPeak;        // 0..1
    const half = Math.max(1, (norm * h) / 2);
    const px = x + c;
    ctx.moveTo(px + 0.5, midY - half);
    ctx.lineTo(px + 0.5, midY + half);
  }
  ctx.stroke();
  ctx.restore();
}

// ── ffmpeg encode subprocess ─────────────────────────────────────────
// Spawns ffmpeg expecting raw BGRA frames on stdin at (W,H,fps) plus
// an audio input from `audioPath`. Returns the child process — caller
// writes BGRA buffers to .stdin and calls .stdin.end() when done.
export function spawnFFmpegEncode({ audioPath, w, h, fps, outPath, crf = 20 }) {
  return spawn("ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo",
    "-pix_fmt", "bgra",                  // node-canvas toBuffer("raw") native byte order
    "-s", `${w}x${h}`,
    "-r", String(fps),
    "-i", "-",
    "-i", audioPath,
    "-c:v", "libx264", "-preset", "medium", "-crf", String(crf),
    "-c:a", "aac", "-b:a", "192k",
    "-pix_fmt", "yuv420p",
    "-shortest",
    "-movflags", "+faststart",
    outPath,
  ], { stdio: ["pipe", "inherit", "inherit"] });
}
