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
  markColor: "#ffffff",
  labelColor: "#ffffff",
  shadow: { color: "#000000", opacity: 78, blur: 2, x: 2, y: 3 },
});

function finite(value, fallback) {
  return Number.isFinite(Number(value)) ? Number(value) : fallback;
}

function clamp(value, min, max) {
  return Math.max(min, Math.min(max, value));
}

export function layoutBrandChrome(theme, { width, height, format = "docs" }) {
  if (!theme || typeof theme !== "object") throw new Error("brand chrome needs a theme object");
  if (!theme.asset && !(theme.markAsset && (theme.label || theme.labelAsset))) {
    throw new Error(`brand theme "${theme.id || "unnamed"}" needs an asset or markAsset + label/labelAsset`);
  }
  if (!(width > 0 && height > 0)) throw new Error(`invalid brand frame ${width}x${height}`);

  const override = theme.formats?.[format] || {};
  const merged = { ...defaults, ...theme, ...override };
  const short = Math.min(width, height);
  const longSide = Math.round(short * clamp(
    finite(merged.longSideFraction, defaults.longSideFraction), 0.06, 0.32,
  ));
  return {
    id: theme.id || "client",
    asset: theme.asset ? resolve(String(theme.asset)) : null,
    markAsset: theme.markAsset ? resolve(String(theme.markAsset)) : null,
    label: theme.label || null,
    labelAsset: theme.labelAsset ? resolve(String(theme.labelAsset)) : null,
    labelAssetCrop: theme.labelAssetCrop || null,
    labelCharacterCuts: Array.isArray(theme.labelCharacterCuts)
      ? theme.labelCharacterCuts : null,
    font: theme.font ? resolve(String(theme.font)) : null,
    markColor: String(merged.markColor || defaults.markColor),
    labelColor: String(merged.labelColor || defaults.labelColor),
    labelWeight: Math.round(clamp(finite(merged.labelWeight, 500), 100, 900)),
    labelStrokeFraction: clamp(finite(merged.labelStrokeFraction, 0.03), 0, 0.08),
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
    markSide: Math.round(short * finite(merged.markSideFraction, 0.105)),
    labelPx: Math.round(short * finite(merged.labelPxFraction, 0.048)),
    leftMarkCenterY: finite(merged.leftMarkCenterY, 0.68),
    leftLabelCenterY: finite(merged.leftLabelCenterY, 0.82),
    rightMarkCenterY: finite(merged.rightMarkCenterY, 0.32),
    rightLabelCenterY: finite(merged.rightLabelCenterY, 0.18),
    characterMotion: merged.characterMotion && typeof merged.characterMotion === "object"
      ? {
          driftPx: Math.max(0, Math.round(short * finite(merged.characterMotion.driftFraction, 0.0014))),
          periodSec: clamp(finite(merged.characterMotion.periodSec, 3.2), 1.2, 12),
          shimmerPeriodSec: clamp(finite(merged.characterMotion.shimmerPeriodSec, 2.4), 1, 12),
          shimmerAmount: clamp(finite(merged.characterMotion.shimmerAmount, 0.18), 0, 0.35),
        }
      : null,
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
    "-background", String(shadow.color || defaults.shadow.color), "-shadow",
    `${clamp(finite(shadow.opacity, 78), 1, 100)}x${clamp(finite(shadow.blur, 2), 0, 12)}+${finite(shadow.x, 2)}+${finite(shadow.y, 3)}`,
    ")", "+swap", "-background", "none", "-layers", "merge", "+repage",
    "-resize", `${layout.longSide}x${layout.longSide}>`, styled,
  ], { stdio: "pipe" });
  execFileSync("magick", [styled, "-background", "none", "-rotate", "90", left], { stdio: "pipe" });
  execFileSync("magick", [styled, "-background", "none", "-rotate", "-90", right], { stdio: "pipe" });
  return { left, right };
}

function sharpShadow(input, output, shadow) {
  execFileSync("magick", [
    input,
    "(", "+clone", "-channel", "A", "-evaluate", "multiply",
    String(clamp(finite(shadow.opacity, 88), 1, 100) / 100), "+channel",
    "-background", String(shadow.color || defaults.shadow.color), "-shadow",
    `${clamp(finite(shadow.opacity, 88), 1, 100)}x${clamp(finite(shadow.blur, 1.4), 0, 8)}+${finite(shadow.x, 2)}+${finite(shadow.y, 3)}`,
    ")", "+swap", "-background", "none", "-layers", "merge", "+repage", output,
  ], { stdio:"pipe" });
}

function renderSeparateElements(layout, workDir) {
  if (!existsSync(layout.markAsset)) throw new Error(`brand mark does not exist: ${layout.markAsset}`);
  if (layout.labelAsset && !existsSync(layout.labelAsset)) {
    throw new Error(`brand wordmark does not exist: ${layout.labelAsset}`);
  }
  if (!layout.labelAsset && (!layout.font || !existsSync(layout.font))) {
    throw new Error(`brand font does not exist: ${layout.font}`);
  }
  mkdirSync(workDir, { recursive:true });
  const stem = `${layout.id}-${layout.markSide}-${layout.labelPx}`.replace(/[^a-z0-9_.-]+/gi, "-");
  const markMaster = join(workDir, `${stem}-mark-master.png`);
  const mark = join(workDir, `${stem}-mark.png`);
  const markShadow = join(workDir, `${stem}-mark-shadow.png`);
  const label = join(workDir, `${stem}-label.png`);
  const labelShadow = join(workDir, `${stem}-label-shadow.png`);
  execFileSync("rsvg-convert", [
    "-w", String(layout.markSide * 2), "-h", String(layout.markSide * 2),
    "-o", markMaster, layout.markAsset,
  ], { stdio:"pipe" });
  // Rasterize at 2× for clean SVG edges, then return to the contracted size.
  // Keeping the 2× raster as the overlay accidentally doubled client marks and
  // allowed a nominally small glyph to collide with the adjacent wordmark.
  execFileSync("magick", [
    markMaster, "-resize", `${layout.markSide}x${layout.markSide}!`,
    "-fill", layout.markColor, "-colorize", "100", mark,
  ], { stdio:"pipe" });
  if (layout.labelAsset) {
    const crop = layout.labelAssetCrop || { x:0, y:0, width:1, height:1 };
    const fullHeight = Math.max(64, Math.round((layout.labelPx * 2) / finite(crop.height, 1)));
    const fullWidth = Math.round(fullHeight * finite(crop.aspect, 202 / 161));
    const wordmarkMaster = join(workDir, `${stem}-wordmark-master.png`);
    execFileSync("rsvg-convert", [
      "-w", String(fullWidth), "-h", String(fullHeight), "-o", wordmarkMaster, layout.labelAsset,
    ], { stdio:"pipe" });
    const x = Math.round(fullWidth * finite(crop.x, 0));
    const y = Math.round(fullHeight * finite(crop.y, 0));
    const width = Math.max(1, Math.round(fullWidth * finite(crop.width, 1)));
    const height = Math.max(1, Math.round(fullHeight * finite(crop.height, 1)));
    execFileSync("magick", [
      wordmarkMaster, "-crop", `${width}x${height}+${x}+${y}`, "+repage",
      "-colorspace", "gray", "-level", "20%,100%", "-alpha", "copy",
      "-fill", layout.labelColor, "-colorize", "100",
      "-resize", `x${layout.labelPx}`, label,
    ], { stdio:"pipe" });
  } else {
    execFileSync("magick", [
      "-background", "none", "-fill", layout.labelColor, "-stroke", layout.shadow.color || defaults.shadow.color,
      "-strokewidth", String(Math.max(1, Math.round(layout.labelPx * layout.labelStrokeFraction))),
      "-weight", String(layout.labelWeight),
      "-font", layout.font, "-pointsize", String(layout.labelPx * 2),
      `label:${layout.label}`, "-resize", "50%", label,
    ], { stdio:"pipe" });
  }
  sharpShadow(mark, markShadow, layout.shadow);
  sharpShadow(label, labelShadow, layout.shadow);
  const rotate = (source, suffix, degrees) => {
    const output = join(workDir, `${stem}-${suffix}.png`);
    execFileSync("magick", [source, "-background", "none", "-rotate", String(degrees), output], { stdio:"pipe" });
    return output;
  };
  const result = {
    kind:"separate",
    markLeft:rotate(markShadow, "mark-left", 90),
    labelLeft:rotate(labelShadow, "label-left", 90),
    markRight:rotate(markShadow, "mark-right", -90),
    labelRight:rotate(labelShadow, "label-right", -90),
  };

  // Canonical client wordmarks can opt into the /pop-style traveling-letter
  // treatment without falling back to an approximate font. Each interval is a
  // normalized horizontal slice of the authored wordmark. The slices remain on
  // the original transparent canvas, so their kerning is preserved exactly.
  if (layout.characterMotion && layout.labelCharacterCuts?.length) {
    const [labelWidth, labelHeight] = execFileSync("identify", [
      "-format", "%w %h", label,
    ], { encoding:"utf8" }).trim().split(/\s+/).map(Number);
    result.labelLeftChars = [];
    result.labelRightChars = [];
    for (const [index, interval] of layout.labelCharacterCuts.entries()) {
      const from = clamp(finite(interval?.[0], 0), 0, 1);
      const to = clamp(finite(interval?.[1], 1), from, 1);
      const x = Math.round(labelWidth * from);
      const x2 = Math.max(x + 1, Math.round(labelWidth * to));
      const charCrop = join(workDir, `${stem}-char-${index}-crop.png`);
      const charCanvas = join(workDir, `${stem}-char-${index}.png`);
      const charShadow = join(workDir, `${stem}-char-${index}-shadow.png`);
      execFileSync("magick", [
        label, "-crop", `${x2 - x}x${labelHeight}+${x}+0`, "+repage", charCrop,
      ], { stdio:"pipe" });
      execFileSync("magick", [
        "-size", `${labelWidth}x${labelHeight}`, "canvas:none", charCrop,
        "-geometry", `+${x}+0`, "-composite", charCanvas,
      ], { stdio:"pipe" });
      sharpShadow(charCanvas, charShadow, layout.shadow);
      result.labelLeftChars.push(rotate(charShadow, `char-${index}-left`, 90));
      result.labelRightChars.push(rotate(charShadow, `char-${index}-right`, -90));
    }
  }
  return result;
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

export function separateBrandChromeFilter(layout, characterCount = 0) {
  const phase = "2*PI*t/" + layout.periodSec.toFixed(3);
  const edge = layout.edgePx;
  const leftX = `${edge}-overlay_w/2`;
  const rightX = `main_w-${edge}-overlay_w/2`;
  const layered = (sign = 1, offset = 0) => {
    const s = sign < 0 ? "-" : "+";
    return `${s}${layout.driftPx}*sin(${phase}+${offset})${s}${(layout.driftPx * 0.35).toFixed(2)}*sin(${phase}*.43+${offset + 1.1})`;
  };
  const y = (fraction, sign, offset) =>
    `main_h*${fraction.toFixed(5)}-overlay_h/2${sign}${offset}*sin(${phase})`;
  if (characterCount > 0 && layout.characterMotion) {
    const filters = [];
    const leftMarkIndex = 1;
    const leftCharStart = 2;
    const rightMarkIndex = leftCharStart + characterCount;
    const rightCharStart = rightMarkIndex + 1;
    filters.push(`[${leftMarkIndex}:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[ml]`);
    filters.push(`[${rightMarkIndex}:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[mr]`);
    const motion = layout.characterMotion;
    for (let i = 0; i < characterCount; i += 1) {
      const shimmerPhase = (i * 0.72).toFixed(3);
      const shimmer = `${(1 - motion.shimmerAmount).toFixed(3)}+${motion.shimmerAmount.toFixed(3)}*sin(2*PI*T/${motion.shimmerPeriodSec.toFixed(3)}-${shimmerPhase})`;
      for (const [input, name] of [
        [leftCharStart + i, `cl${i}`],
        [rightCharStart + i, `cr${i}`],
      ]) {
        filters.push(
          `[${input}:v]format=rgba,geq=r='r(X,Y)':g='g(X,Y)':b='b(X,Y)':a='alpha(X,Y)*${layout.opacity.toFixed(3)}*(${shimmer})'[${name}]`,
        );
      }
    }
    const leftPairY = y(layout.leftMarkCenterY, "+", layout.bobPx);
    const rightPairY = y(layout.rightMarkCenterY, "-", layout.bobPx);
    filters.push(`[0:v][ml]overlay=x='${leftX}${layered(1, 0)}':y='${leftPairY}':eval=frame:shortest=1[v0]`);
    let previous = "v0";
    for (let i = 0; i < characterCount; i += 1) {
      const drift = `${motion.driftPx}*sin(2*PI*t/${motion.periodSec.toFixed(3)}-${(i * 0.8).toFixed(3)})`;
      const next = `vl${i}`;
      filters.push(`[${previous}][cl${i}]overlay=x='${leftX}${layered(1, 0)}+${drift}':y='main_h*${layout.leftLabelCenterY.toFixed(5)}-overlay_h/2+${layout.bobPx}*sin(${phase})':eval=frame:shortest=1[${next}]`);
      previous = next;
    }
    filters.push(`[${previous}][mr]overlay=x='${rightX}${layered(-1, 0.6)}':y='${rightPairY}':eval=frame:shortest=1[vr0]`);
    previous = "vr0";
    for (let i = 0; i < characterCount; i += 1) {
      const drift = `${motion.driftPx}*sin(2*PI*t/${motion.periodSec.toFixed(3)}-${(i * 0.8 + 0.6).toFixed(3)})`;
      const next = i === characterCount - 1 ? "outv" : `vr${i + 1}`;
      filters.push(`[${previous}][cr${i}]overlay=x='${rightX}${layered(-1, 0.6)}-${drift}':y='main_h*${layout.rightLabelCenterY.toFixed(5)}-overlay_h/2-${layout.bobPx}*sin(${phase})':eval=frame:shortest=1[${next}]`);
      previous = next;
    }
    return filters.join(";");
  }
  return [
    `[1:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[ml]`,
    `[2:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[tl]`,
    `[3:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[mr]`,
    `[4:v]format=rgba,colorchannelmixer=aa=${layout.opacity.toFixed(3)}[tr]`,
    `[0:v][ml]overlay=x='${leftX}+${layout.driftPx}*sin(${phase})':y='${y(layout.leftMarkCenterY, "+", layout.bobPx)}':eval=frame:shortest=1[a]`,
    `[a][tl]overlay=x='${leftX}-${layout.driftPx}*sin(${phase}+1.7)':y='${y(layout.leftLabelCenterY, "-", layout.bobPx)}':eval=frame:shortest=1[b]`,
    `[b][mr]overlay=x='${rightX}-${layout.driftPx}*sin(${phase}+.6)':y='${y(layout.rightMarkCenterY, "-", layout.bobPx)}':eval=frame:shortest=1[c]`,
    `[c][tr]overlay=x='${rightX}+${layout.driftPx}*sin(${phase}+2.1)':y='${y(layout.rightLabelCenterY, "+", layout.bobPx)}':eval=frame:shortest=1[outv]`,
  ].join(";");
}

export function applyBrandChrome({ input, out, theme, workDir, format = "docs" }) {
  if (!existsSync(input)) throw new Error(`brand input does not exist: ${input}`);
  const media = probeVideo(input);
  const layout = layoutBrandChrome(theme, { ...media, format });
  const assetsDir = join(workDir || dirname(out), "brand-chrome", layout.id);
  const lockups = layout.markAsset && (layout.label || layout.labelAsset)
    ? renderSeparateElements(layout, assetsDir)
    : renderLockups(layout, assetsDir);
  mkdirSync(dirname(out), { recursive: true });
  const overlayInputs = lockups.kind === "separate"
    ? lockups.labelLeftChars?.length
      ? [lockups.markLeft, ...lockups.labelLeftChars, lockups.markRight, ...lockups.labelRightChars]
      : [lockups.markLeft, lockups.labelLeft, lockups.markRight, lockups.labelRight]
    : [lockups.left, lockups.right];
  const ffmpegInputs = overlayInputs.flatMap((path) => ["-loop", "1", "-i", path]);
  execFileSync(FFMPEG, [
    "-y", "-i", input, ...ffmpegInputs,
    "-filter_complex_threads", "1",
    "-filter_complex", lockups.kind === "separate"
      ? separateBrandChromeFilter(layout, lockups.labelLeftChars?.length || 0) : brandChromeFilter(layout),
    "-map", "[outv]", "-map", "0:a?",
    "-t", media.duration.toFixed(3), "-r", media.fps,
    "-c:v", "libx264", "-preset", "medium", "-crf", "18",
    "-pix_fmt", "yuv420p", "-c:a", "copy", "-movflags", "+faststart", out,
  ], { stdio: ["ignore", "ignore", "pipe"] });
  return { out, layout, lockups, source:basename(input) };
}
