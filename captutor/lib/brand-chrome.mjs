// Reusable, client-owned side chrome for Captutor deliveries.
//
// Captutor owns the behavior (responsive placement, inward-facing lockups,
// slow drift, tight separation shadow). A client theme owns the asset and
// tuning. Nothing in this module knows what Fuser—or any future client—looks
// like.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { basename, dirname, extname, join, resolve } from "node:path";

const FFMPEG = process.env.FFMPEG || "ffmpeg";
const FFPROBE = process.env.FFPROBE || "ffprobe";

const defaults = Object.freeze({
  opacity: 0.96,
  periodSec: 18,
  driftPx: 5,
  bobPx: 3,
  longSideFraction: 0.155,
  edgeFraction: 0.055,
  leftCenterY: 0.78,
  rightCenterY: 0.22,
  shadow: { opacity: 78, blur: 2, x: 2, y: 3 },
});

function finite(value, fallback) {
  return Number.isFinite(Number(value)) ? Number(value) : fallback;
}

function clamp(value, min, max) {
  return Math.max(min, Math.min(max, value));
}

export function layoutBrandChrome(theme, { width, height, format = "docs" }) {
  if (!theme || typeof theme !== "object") throw new Error("brand chrome needs a theme object");
  if (!theme.asset) throw new Error(`brand theme "${theme.id || "unnamed"}" needs an asset`);
  if (!(width > 0 && height > 0)) throw new Error(`invalid brand frame ${width}x${height}`);

  const override = theme.formats?.[format] || {};
  const merged = { ...defaults, ...theme, ...override };
  const short = Math.min(width, height);
  const longSide = Math.round(short * clamp(
    finite(merged.longSideFraction, defaults.longSideFraction), 0.06, 0.32,
  ));
  return {
    id: theme.id || "client",
    asset: resolve(String(theme.asset)),
    opacity: clamp(finite(merged.opacity, defaults.opacity), 0.1, 1),
    periodSec: clamp(finite(merged.periodSec, defaults.periodSec), 6, 90),
    driftPx: Math.round(short * finite(merged.driftFraction, 0)) ||
      Math.round(finite(merged.driftPx, defaults.driftPx)),
    bobPx: Math.round(short * finite(merged.bobFraction, 0)) ||
      Math.round(finite(merged.bobPx, defaults.bobPx)),
    longSide,
    edgePx: Math.round(width * clamp(
      finite(merged.edgeFraction, defaults.edgeFraction), 0.015, 0.18,
    )),
    leftCenterY: clamp(finite(merged.leftCenterY, defaults.leftCenterY), 0.08, 0.92),
    rightCenterY: clamp(finite(merged.rightCenterY, defaults.rightCenterY), 0.08, 0.92),
    shadow: { ...defaults.shadow, ...(theme.shadow || {}), ...(override.shadow || {}) },
    transparentColors: override.transparentColors || theme.transparentColors || [],
  };
}

function probeVideo(input) {
  const data = JSON.parse(execFileSync(FFPROBE, [
    "-v", "error", "-select_streams", "v:0",
    "-show_entries", "stream=width,height,r_frame_rate:format=duration",
    "-of", "json", input,
  ], { encoding: "utf8" }));
  const stream = data.streams?.[0];
  if (!stream) throw new Error(`no video stream: ${input}`);
  return {
    width: Number(stream.width),
    height: Number(stream.height),
    fps: stream.r_frame_rate || "30",
    duration: Number(data.format?.duration),
  };
}

function renderLockups(layout, workDir) {
  if (!existsSync(layout.asset)) throw new Error(`brand asset does not exist: ${layout.asset}`);
  mkdirSync(workDir, { recursive: true });
  const stem = `${layout.id}-${layout.longSide}`.replace(/[^a-z0-9_.-]+/gi, "-");
  const raster = join(workDir, `${stem}-source.png`);
  const transparentRaster = join(workDir, `${stem}-transparent.png`);
  const styled = join(workDir, `${stem}-styled.png`);
  const left = join(workDir, `${stem}-left.png`);
  const right = join(workDir, `${stem}-right.png`);

  const ext = extname(layout.asset).toLowerCase();
  if (ext === ".svg") {
    execFileSync("rsvg-convert", [
      "-w", String(layout.longSide * 2), "-o", raster, layout.asset,
    ], { stdio: "pipe" });
  } else {
    execFileSync("magick", [
      layout.asset, "-resize", `${layout.longSide * 2}x${layout.longSide * 2}>`, raster,
    ], { stdio: "pipe" });
  }

  // Some client lockup masters are designed as thumbnail badges. Themes may
  // key those flat backing colors away while retaining the authored glyph,
  // wordmark, alpha glow, and our compact separation shadow.
  let styledSource = raster;
  if (Array.isArray(layout.transparentColors) && layout.transparentColors.length) {
    const keyArgs = [raster];
    for (const entry of layout.transparentColors) {
      const color = typeof entry === "string" ? entry : entry.color;
      const fuzz = typeof entry === "string" ? 8 : finite(entry.fuzz, 8);
      if (!color) continue;
      keyArgs.push("-fuzz", `${clamp(fuzz, 0, 35)}%`, "-transparent", color);
    }
    keyArgs.push(transparentRaster);
    execFileSync("magick", keyArgs, { stdio:"pipe" });
    styledSource = transparentRaster;
  }

  // A compact, high-opacity shadow separates glowing lockups without producing
  // the broad gray fog that a large blur/spread creates over product UI.
  const shadow = layout.shadow;
  execFileSync("magick", [
    styledSource,
    "(", "+clone", "-channel", "A", "-evaluate", "multiply",
    String(clamp(finite(shadow.opacity, 78), 1, 100) / 100), "+channel",
    "-background", "black", "-shadow",
    `${clamp(finite(shadow.opacity, 78), 1, 100)}x${clamp(finite(shadow.blur, 2), 0, 12)}+${finite(shadow.x, 2)}+${finite(shadow.y, 3)}`,
    ")", "+swap", "-background", "none", "-layers", "merge", "+repage",
    "-resize", `${layout.longSide}x${layout.longSide}>`, styled,
  ], { stdio: "pipe" });
  execFileSync("magick", [styled, "-background", "none", "-rotate", "90", left], { stdio: "pipe" });
  execFileSync("magick", [styled, "-background", "none", "-rotate", "-90", right], { stdio: "pipe" });
  return { left, right };
}

export function brandChromeFilter(layout) {
  const phase = "2*PI*t/" + layout.periodSec.toFixed(3);
  const leftX = `${layout.edgePx}-overlay_w/2+${layout.driftPx}*sin(${phase})`;
  const rightX = `main_w-${layout.edgePx}-overlay_w/2-${layout.driftPx}*sin(${phase})`;
  const leftY = `main_h*${layout.leftCenterY.toFixed(5)}-overlay_h/2+${layout.bobPx}*sin(${phase}+1.3)`;
  const rightY = `main_h*${layout.rightCenterY.toFixed(5)}-overlay_h/2-${layout.bobPx}*sin(${phase}+1.3)`;
  return [
    `[1:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[left]`,
    `[2:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[right]`,
    `[0:v][left]overlay=x='${leftX}':y='${leftY}':eval=frame:shortest=1[one]`,
    `[one][right]overlay=x='${rightX}':y='${rightY}':eval=frame:shortest=1[outv]`,
  ].join(";");
}

export function applyBrandChrome({ input, out, theme, workDir, format = "docs" }) {
  if (!existsSync(input)) throw new Error(`brand input does not exist: ${input}`);
  const media = probeVideo(input);
  const layout = layoutBrandChrome(theme, { ...media, format });
  const assetsDir = join(workDir || dirname(out), "brand-chrome", layout.id);
  const lockups = renderLockups(layout, assetsDir);
  mkdirSync(dirname(out), { recursive: true });
  execFileSync(FFMPEG, [
    "-y", "-i", input,
    "-loop", "1", "-i", lockups.left,
    "-loop", "1", "-i", lockups.right,
    "-filter_complex_threads", "1",
    "-filter_complex", brandChromeFilter(layout),
    "-map", "[outv]", "-map", "0:a?",
    "-t", media.duration.toFixed(3), "-r", media.fps,
    "-c:v", "libx264", "-preset", "medium", "-crf", "18",
    "-pix_fmt", "yuv420p", "-c:a", "copy", "-movflags", "+faststart", out,
  ], { stdio: ["ignore", "ignore", "pipe"] });
  return { out, layout, lockups, source:basename(input) };
}
