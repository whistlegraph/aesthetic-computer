// deliver — burn captions and cut the take to standard formats.
//
// One recording, many deliverables. Everything here re-composes from `clip.mp4`
// plus `cues.json` (the measured beat offsets), so a new format or a caption
// restyle costs an ffmpeg pass — never another take. That is the point of
// keeping the cues: the app is driven once, and the edit is cheap forever after.
//
// Captions are BURNED with ImageMagick + ffmpeg `overlay`, not libass. This Mac's
// ffmpeg has no libass, no freetype and no fontconfig (`ffmpeg -version` shows
// none of them — the same reason recap's composer cannot run here), so
// `subtitles=` and `drawtext` are both unavailable. /pop hit this first and
// solved it the same way: pre-rasterize each caption to a transparent PNG in
// ImageMagick, which CAN load a font file directly, then composite the PNGs as
// timed overlays. Do not "simplify" this back to drawtext; it renders nothing.
//
// Soft subtitles still ship too (the mov_text track + .vtt sidecar). Burned text
// is for platforms that autoplay muted and strip tracks — reels, shorts, feeds.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, renameSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { captionPhrases, isHighlightableCaptionToken } from "./captions.mjs";
import { applyBrandChrome } from "./brand-chrome.mjs";

const FFMPEG = process.env.FFMPEG || "ffmpeg";
const HERE = dirname(fileURLToPath(import.meta.url));
const STAGE_MODE = process.env.CAPTUTOR_STAGE_MODE === "1";
const VERTICAL_MODE = process.env.CAPTUTOR_VERTICAL_MODE === "1";

// Deliberately ordinary subtitle typography. Captions are navigation, not a
// brand surface: Arial Bold stays readable over pale or busy UI and produces
// the familiar neutral shape people already recognize as subtitles.
const LATIN_FONT = process.env.CAPTUTOR_FONT
  || "/System/Library/Fonts/Supplemental/Arial Bold.ttf";
const CAPTION_STYLE = "outlined-color-caption-karaoke-v10-subtle-active-word";

// Arial does not cover every script, so non-Latin locales use the corresponding
// macOS system face instead of silently dropping glyphs.
const SCRIPT_FONTS = {
  "ko":    "/System/Library/Fonts/AppleSDGothicNeo.ttc",
  "zh-CN": "/System/Library/Fonts/Hiragino Sans GB.ttc",  // NOT PingFang — see below
  "hi":    "/System/Library/Fonts/Kohinoor.ttc",
  "fa":    "/System/Library/Fonts/Supplemental/GeezaPro.ttc",
};

/// Pick a font that can actually draw this locale — and refuse to guess.
///
/// A missing font file does not error: ImageMagick falls back, drops every glyph
/// it cannot draw, and hands back a caption containing only the punctuation. We
/// shipped a whole Chinese take that way (PingFang is NOT at
/// /System/Library/Fonts/PingFang.ttc, so it silently used the Latin face and the burned
/// captions read just "App"). Throwing here is the only way that stays fixed.
function fontFor(locale) {
  const alt = SCRIPT_FONTS[locale];
  if (!alt) return LATIN_FONT;
  if (!existsSync(alt)) {
    throw new Error(
      `no font for "${locale}" at ${alt}. The Latin face cannot draw this script, and ` +
      `falling back to it would silently produce blank captions.`);
  }
  return alt;
}

/// Did the glyphs actually land?
///
/// The backstop for the above: a caption with text in it must have ink in it. If
/// the rasterizer dropped the script, the PNG comes back essentially empty —
/// catch that here rather than in the finished video.
function assertHasInk(png, text) {
  const mean = +execFileSync("magick", [
    png, "-alpha", "extract", "-format", "%[fx:mean]", "info:",
  ], { encoding: "utf8" }).trim();
  if (mean < 0.002) {
    throw new Error(
      `caption rendered blank: ${JSON.stringify(text.slice(0, 40))}\n` +
      `  the font (${FONT}) has no glyphs for this script.`);
  }
}

let FONT = LATIN_FONT;  // set per-render by deliver()
const TEXT = "#ffffff";    // plain white — subtitles are not a brand surface
const ACTIVE_TEXT = "#d8c6e5"; // quiet Fuser lavender — one spoken word at a time
const BG = "#0a0a0a";      // neutral-950
const ACCENT = "#4f46e5";  // indigo-600 — the app's own action colour

// A clamshell Mac can expose a true HiDPI desktop that is taller than the
// 16:9 docs delivery while still matching its full output width. Keep that
// extra source resolution honest: accept only an exact-width, at-least-tall
// full-desktop negative, then remove equal amounts from the top and bottom.
// Smaller inputs and arbitrary window-shaped captures remain rejected.
export function fullDesktopCrop({ source, target }) {
  if (source.w !== target.w || source.h < target.h) {
    throw new Error(
      `full-desktop negative must be ${target.w}px wide and at least ${target.h}px tall; ` +
      `got ${source.w}x${source.h}`,
    );
  }
  if (source.h === target.h) return "";
  const y = Math.floor((source.h - target.h) / 2);
  return `crop=${target.w}:${target.h}:0:${y},`;
}

// NOTE: ImageMagick here has no fontconfig, so there is no default font at all —
// omit `-font` and it errors rather than guessing. That is a feature: a silent
// fallback to Helvetica would be off-brand and nobody would notice.

/// Target geometries.
///
/// `video` is where the recording sits in the frame; `cap` is the caption band.
/// Fractions are of the output frame, so a format is one small table entry and
/// not a pile of special cases.
/// EACH ASPECT RATIO IS ITS OWN RECORDING.
///
/// The obvious shortcut — film once in landscape and crop a vertical window out
/// of it for reels — is what makes every software reel on the internet look bad:
/// a letterboxed desktop with dead bars, UI too small to read, and a crop that is
/// always looking slightly at the wrong thing.
///
/// fuser's UI is responsive. So instead we RESIZE THE BROWSER WINDOW to the
/// target shape and drive the tutorial again. A portrait window gives the app's
/// real narrow layout, filmed natively at 9:16 — legible, correctly composed, and
/// not a crop of anything. It costs another take; takes are cheap and the app is
/// driven by a script.
///
/// `win` is the OUTER window in CSS points — it includes the title bar, tab strip
/// and URL bar, because `reel` films the window, not the page. Size the window to
/// the delivery aspect and the chrome is part of the composition rather than
/// something to crop off later.
export const FORMATS = {
  // Docs. A wide window at the shape a reader's own browser is: the clip should
  // look like the app they are looking at.
  docs: {
    // Stage Mode runs the display at 2× HiDPI. A 1190×630-point window becomes
    // 2380×1260 pixels: exactly 90 pixels of breathing room on every side of a
    // 2560×1440 delivery. The window is intentionally wider than 16:9 so the
    // frame's margins, rather than the browser's aspect ratio, set the geometry.
    win: STAGE_MODE ? { w: 1190, h: 630 } : { w: 1512, h: 945 },
    out: STAGE_MODE ? { w: 2560, h: 1440 } : { w: 1512, h: 945 },
    fps: STAGE_MODE ? 60 : 30,
    compose: STAGE_MODE ? { fullDesktop: true, badgeRepair: true } : undefined,
    // Stage recordings are viewed inside a docs player, often at half their
    // encoded size. Use presentation-scale captions so they remain readable,
    // and lift them slightly to give the classic outline breathing room.
    capWidth: 0.84,
    capPx: STAGE_MODE ? 58 : 44,
    capY: STAGE_MODE ? 0.86 : 0.90,
    bar: false,
  },
  // A real portrait desktop, not a landscape take cropped into a phone frame.
  // `bin/stage.mjs --vertical` rotates Panda and selects the panel's 2× mode;
  // 630×1190 points therefore records as 1260×2380 pixels with a uniform
  // 90-pixel surround in the native 1440×2560 delivery.
  vertical: {
    win: { w: 630, h: 1190 },
    out: { w: 1440, h: 2560 },
    fps: 60,
    compose: { fullDesktop: true, badgeRepair: true },
    capWidth: 0.88,
    capPx: 58,
    capY: 0.88,
    bar: false,
    requiresVerticalStage: !VERTICAL_MODE,
  },
  // YouTube. A true 16:9 window, so the frame IS the window — no pillarboxing.
  youtube: {
    win: { w: 1600, h: 900 },        // 16:9
    out: { w: 1920, h: 1080 },
    capWidth: 0.80, capPx: 44, capY: 0.90,
    bar: true,
  },
  // Reels / Shorts / TikTok.
  //
  // A native 9:16 window is IMPOSSIBLE here and it is worth saying why: this Mac
  // reports screen.availHeight = 851 points, so the tallest window Chrome will
  // give us is 851. A true 9:16 at that height would be 479px wide — narrower
  // than Chrome's minimum, and fuser would collapse to its phone layout.
  //
  // So the window is as TALL as the display allows and comfortably WIDE, and the
  // portrait frame is composed around it. The win is legibility: a 1000px-wide
  // window scaled into a 1080px-wide frame is 1.08× — the UI ends up BIGGER than
  // life. Fitting the old 2054px landscape window into the same frame was 0.52×,
  // which is precisely why that reel was unreadable.
  reel: {
    win: { w: 1000, h: 851 },        // as tall as the display permits
    out: { w: 1080, h: 1920 },
    compose: { videoY: 0.09 },       // rides high; the stage below is for type
    capWidth: 0.90, capPx: 56, capY: 0.66,
    bar: true,
  },
};

const metricCache = new Map();

// Caption art is expensive enough to cache, but its filename must carry the
// words and layout that produced it. Index-only names reused stale PNGs after a
// screenplay edit: fresh timings then composited old copy, causing ghost text
// and collisions that appeared only in a previously rendered format.
export function captionCacheKey({ words, width, px, font, color }) {
  return createHash("sha256").update(JSON.stringify({
    style:CAPTION_STYLE,
    words:words.map((word) => word.text),
    width, px, font, color:color || null,
  })).digest("hex").slice(0, 16);
}

function textMetrics(text, px) {
  const key = `${FONT}\0${px}\0${text}`;
  if (metricCache.has(key)) return metricCache.get(key);
  const [w, h] = execFileSync("magick", [
    "-background", "none", "-font", FONT, "-pointsize", String(px),
    `label:${text}`, "-format", "%w,%h", "info:",
  ], { encoding: "utf8" }).trim().split(",").map(Number);
  const value = { w, h };
  metricCache.set(key, value);
  return value;
}

/// Lay out explicit words rather than asking `caption:` to hide its wrapping
/// decisions. Besides making the result deterministic, this gives each word an
/// exact position so its fill can change while it is spoken without moving the
/// phrase by even one pixel.
function layoutWords(words, { width, px }) {
  const sample = textMetrics("Ag", px);
  const space = Math.max(1, textMetrics("A A", px).w - textMetrics("AA", px).w);
  const lineHeight = Math.ceil(sample.h * 1.16);
  const lines = [];
  let line = { words: [], width: 0 };
  for (const [index, word] of words.entries()) {
    const measured = textMetrics(word.text, px);
    const gap = line.words.length ? space : 0;
    if (line.words.length && line.width + gap + measured.w > width) {
      lines.push(line);
      line = { words: [], width: 0 };
    }
    const x = line.width + (line.words.length ? space : 0);
    line.words.push({ ...word, index, x, width: measured.w });
    line.width = x + measured.w;
  }
  if (line.words.length) lines.push(line);

  const height = Math.max(lineHeight, lines.length * lineHeight);
  return {
    width, height,
    boxes: lines.map((row, rowIndex) => {
      const padX = Math.round(px * 0.30);
      const padY = Math.round(px * 0.11);
      const inset = Math.round((width - row.width) / 2);
      return {
        x1: inset - padX,
        y1: rowIndex * lineHeight - padY,
        x2: inset + row.width + padX,
        y2: (rowIndex + 1) * lineHeight + padY,
      };
    }),
    words: lines.flatMap((row, rowIndex) => {
      const inset = Math.round((width - row.width) / 2);
      return row.words.map((word) => ({
        ...word,
        x: inset + word.x,
        // `-draw text` takes a BASELINE coordinate. Every word on a row shares
        // this exact value, unlike `-annotate`, which offsets each token from
        // its own glyph bounds and makes short words visibly bob up and down.
        baseline: Math.round(
          rowIndex * lineHeight + (lineHeight - sample.h) / 2 + sample.h * 0.79,
        ),
      }));
    }),
  };
}

/// Rasterize a large color-coded subtitle over a compact translucent black
/// box, with a tight outline and hanging shadow for legibility over product UI.
/// `activeIndex` changes only the spoken word's fill for timed tracking.
function cuePng(words, {
  width, px, out, activeIndex = -1, highlightOnly = false, color = TEXT,
}) {
  const layout = layoutWords(words, { width, px });
  const mvg = (text) => text.replaceAll("\\", "\\\\").replaceAll('"', '\\"');
  const args = ["-size", `${layout.width}x${layout.height}`, "xc:none"];
  if (!highlightOnly) {
    args.push("-fill", "rgba(0,0,0,0.68)", "-stroke", "none");
    for (const box of layout.boxes) {
      const radius = Math.round(px * 0.16);
      args.push(
        "-draw",
        `roundrectangle ${box.x1},${box.y1},${box.x2},${box.y2},${radius},${radius}`,
      );
    }
  }
  args.push(
    "-font", FONT, "-pointsize", String(px),
    "-stroke", "rgba(0,0,0,.98)", "-strokewidth", String(Math.max(1, Math.round(px * 0.022))),
  );
  // A compact, high-opacity hanging shadow keeps large outlined transcript
  // labels crisp over live browser UI. Draw it as a second glyph instead of a
  // broad blur so fine Devanagari counters and UI details remain clear.
  const shadowOffset = Math.max(2, Math.round(px * 0.038));
  for (const word of layout.words) {
    if (highlightOnly && word.index !== activeIndex) continue;
    args.push(
      "-fill", "rgba(24,18,31,.92)",
      "-stroke", "rgba(165,140,188,.86)",
      "-strokewidth", String(Math.max(1, Math.round(px * 0.014))),
      "-draw", `text ${word.x + 1},${word.baseline + shadowOffset} \"${mvg(word.text)}\"`,
    );
  }
  args.push(
    "-stroke", "rgba(0,0,0,.98)",
    "-strokewidth", String(Math.max(1, Math.round(px * 0.022))),
  );
  for (const word of layout.words) {
    if (highlightOnly && word.index !== activeIndex) continue;
    args.push(
      "-fill", highlightOnly ? ACTIVE_TEXT : (color || TEXT),
      "-draw", `text ${word.x},${word.baseline} \"${mvg(word.text)}\"`,
    );
  }
  args.push(out);
  execFileSync("magick", args);
  const ink = highlightOnly ? words[activeIndex]?.text : words.map((word) => word.text).join(" ");
  assertHasInk(out, ink || "highlight");
  return out;
}

function probeDims(clip) {
  const raw = execFileSync("ffprobe", [
    "-v", "error", "-select_streams", "v:0",
    "-show_entries", "stream=width,height", "-of", "csv=p=0", clip,
  ], { encoding: "utf8" }).trim().split(",");
  return { w: +raw[0], h: +raw[1] };
}

function duration(clip) {
  return +execFileSync("ffprobe", [
    "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", clip,
  ], { encoding: "utf8" }).trim();
}

function videoDuration(clip) {
  return +execFileSync("ffprobe", [
    "-v", "error", "-select_streams", "v:0",
    "-show_entries", "stream=duration", "-of", "csv=p=0", clip,
  ], { encoding: "utf8" }).trim();
}

function chapterColor(value, fallback = "#7c91d8") {
  return /^#[0-9a-f]{6}$/i.test(String(value || "")) ? String(value) : fallback;
}

function dimChapterColor(hex, amount = 0.16) {
  const rgb = [1, 3, 5].map((at) => Math.round(parseInt(hex.slice(at, at + 2), 16) * amount));
  return `#${rgb.map((part) => part.toString(16).padStart(2, "0")).join("")}`;
}

function ffmetadataEscape(value) {
  return String(value).replace(/([\\=;#])/g, "\\$1").replace(/\n/g, "\\\n");
}

function embedChapters({ video, chapters, durationSec, workDir }) {
  if (!Array.isArray(chapters) || chapters.length === 0) return;
  const ordered = chapters
    .map((chapter) => ({ ...chapter, startSec:Number(chapter.startSec) }))
    .filter((chapter) => Number.isFinite(chapter.startSec) && chapter.startSec >= 0)
    .sort((a, b) => a.startSec - b.startSec);
  if (!ordered.length) return;
  const metadata = [";FFMETADATA1"];
  ordered.forEach((chapter, index) => {
    const start = Math.round(chapter.startSec * 1000);
    const end = Math.round((ordered[index + 1]?.startSec ?? durationSec) * 1000);
    if (end <= start) return;
    metadata.push(
      "[CHAPTER]", "TIMEBASE=1/1000", `START=${start}`, `END=${end}`,
      `title=${ffmetadataEscape(chapter.title || `Chapter ${index + 1}`)}`,
    );
  });
  const metadataPath = join(workDir, "chapters.ffmetadata");
  const muxed = `${video}.chapters.mp4`;
  writeFileSync(metadataPath, `${metadata.join("\n")}\n`);
  execFileSync(FFMPEG, [
    "-y", "-i", video, "-i", metadataPath,
    "-map", "0", "-map_metadata", "1", "-map_chapters", "1",
    "-c", "copy", "-movflags", "+faststart", muxed,
  ], { stdio:["ignore", "ignore", "pipe"] });
  renameSync(muxed, video);
}

function renderTerminalCard({ clip, card, durationSec, width, height, workDir }) {
  if (!card?.title) return null;
  const holdSec = Math.max(1.5, Number(card.durationSec) || 4.9);
  const startSec = Math.max(0, durationSec - holdSec);
  const sampleSec = Math.min(durationSec - 0.1, startSec + holdSec * 0.5);
  const template = join(workDir, "terminal-card-template.png");
  const png = join(workDir, "terminal-card.png");
  execFileSync(FFMPEG, [
    "-y", "-ss", sampleSec.toFixed(3), "-i", clip, "-frames:v", "1", template,
  ], { stdio:["ignore", "ignore", "pipe"] });
  const bandTop = Math.round(height * 0.45);
  const bandBottom = Math.round(height * 0.62);
  const titleOffset = Math.round(height * 0.09);
  execFileSync("magick", [
    template,
    "-fill", "#f4f4f3", "-draw", `rectangle 0,${bandTop} ${width},${bandBottom}`,
    "-font", join(HERE, "..", "assets", "Marund.ttf"),
    "-pointsize", String(Math.round(height * 0.066)), "-weight", "700",
    "-fill", "#111111", "-gravity", "center",
    "-annotate", `+0+${titleOffset}`, String(card.title), png,
  ], { stdio:"pipe" });
  return { png, startSec };
}

export function deliver({
  clip, cues, format, out, workDir, locale = "en", brandChrome = null,
  geometry = null, captionPx = null, captionY = null, chapters = null,
  terminalCard = null, title = null,
}) {
  FONT = fontFor(locale);  // brand face for Latin, script-capable fallback otherwise
  const F = FORMATS[format];
  if (!F) throw new Error(`unknown format: ${format} (have: ${Object.keys(FORMATS).join(", ")})`);

  const src = probeDims(clip);
  const dur = duration(clip);
  const videoDur = videoDuration(clip);
  // ScreenCaptureKit may stop emitting frames when a desktop is perfectly
  // static even though narration is still running. Never let that truncate the
  // visual stream: hold its final valid frame through the audio duration.
  const pad = Math.max(0, dur - videoDur);
  // Give the hold a small overrun and let `-shortest` trim to narration. Some
  // ScreenCaptureKit files carry a final-frame timestamp almost one second
  // earlier than their reported stream duration; padding only the arithmetic
  // delta can therefore still leave a short video track in the final MP4.
  const holdLastFrame = pad > 0.02
    ? `tpad=stop_mode=clone:stop_duration=${(pad + 2).toFixed(3)},`
    : "";
  const W = geometry?.w || F.out.w;
  const H = geometry?.h || F.out.h;
  const capPx = captionPx || F.capPx;
  const capY = captionY ?? F.capY;
  const chapterList = Array.isArray(chapters) ? chapters : [];
  const showBar = chapterList.length > 0 || F.bar;
  const terminal = renderTerminalCard({
    clip, card:terminalCard, durationSec:dur, width:W, height:H, workDir,
  });
  const needsPostCaption = showBar || Boolean(terminal);

  // Caption PNGs are cached because multilingual rasterization is expensive.
  // Version the directory so a style change can never silently reuse an older
  // size or outline during a cheap recut of an existing take.
  const capDir = join(workDir, `caps-${format}-${CAPTION_STYLE}`);
  mkdirSync(capDir, { recursive: true });

  const band = Math.round(W * F.capWidth);
  const cuts = captionPhrases(cues);
  const pngs = cuts.map((c, i) => {
    const stem = String(i).padStart(3, "0");
    const cacheKey = captionCacheKey({
      words:c.words, width:band, px:capPx, font:FONT, color:c.color,
    });
    const base = join(capDir, `${stem}-${cacheKey}-base.png`);
    if (!existsSync(base)) cuePng(c.words, {
      width:band, px:capPx, out:base, color:c.color,
    });
    const highlights = c.words
      .map((word, wordIndex) => ({ word, wordIndex }))
      .filter(({ word }) => isHighlightableCaptionToken(word.text))
      .map(({ word, wordIndex }) => {
        const png = join(capDir,
          `${stem}-${cacheKey}-word-${String(wordIndex).padStart(2, "0")}.png`);
        if (!existsSync(png)) {
          cuePng(c.words, {
            width:band, px:capPx, out:png, activeIndex:wordIndex,
            highlightOnly:true, color:c.color,
          });
        }
        return { ...word, png };
      });
    return { ...c, png: base, highlights };
  });

  // Each phrase has one stable box + white-text base. Timed layers contain only
  // one transparent lavender word, so the box and inactive text are never stacked
  // twice. The old full-phrase highlight layers darkened the box on every word
  // and briefly doubled it at boundaries, producing the reported gray flicker.
  const captionLayers = pngs.flatMap((phrase) => [
    { from: phrase.from, to: phrase.to, png: phrase.png },
    ...phrase.highlights,
  ]);

  // ── video base ────────────────────────────────────────────────────────────
  const args = ["-y", "-i", clip];
  const firstCaptionInput = 1;
  // ffmpeg otherwise gives every PNG decoder the host's automatic thread
  // count. A karaoke-heavy lesson can have hundreds of caption inputs, so the
  // aggregate decoder pool exhausts macOS pthreads before frame zero. Pin each
  // still-image decoder to one worker; the filter graph is already serialized
  // below for the same reason.
  for (const p of captionLayers) args.push("-threads", "1", "-i", p.png);
  const terminalInput = firstCaptionInput + captionLayers.length;
  if (terminal) args.push("-threads", "1", "-i", terminal.png);

  const chain = [];
  if (F.compose?.fullDesktop) {
    let desktopCrop;
    try {
      desktopCrop = fullDesktopCrop({ source: src, target: { w: W, h: H } });
    } catch {
      throw new Error(
        `${format} Stage delivery needs a ${W}px-wide full-desktop negative at least ${H}px tall; ` +
        `got ${src.w}x${src.h}. Record a new take instead of enlarging or recutting a window capture.`,
      );
    }
    // Preserve the complete 16:9 center of the physical desktop. A taller
    // clamshell negative is cropped symmetrically, never enlarged. The sole repair is the tiny
    // ScreenCaptureKit status dot at the extreme top-right: clone a live 2×2
    // sample of the adjacent stage wallpaper over a 34×28 patch. No browser or
    // window pixels are touched.
    chain.push(
      `[0:v]${holdLastFrame}${desktopCrop}scale=${W}:${H},split=2[desktop][badgeSeed]`,
      `[badgeSeed]crop=2:2:${W - 62}:12,scale=34:28:flags=neighbor[badgePatch]`,
      `[desktop][badgePatch]overlay=${W - 34}:0:shortest=1[base]`);
  } else if (F.compose) {
    // Portrait frame, landscape-ish window: fill the WIDTH (so the UI is scaled
    // UP, not down) and ride high, leaving the lower third as a caption stage.
    const vw = Math.round(W / 2) * 2;
    const vh = Math.round((vw * src.h / src.w) / 2) * 2;
    const vy = Math.round(H * F.compose.videoY);
    chain.push(
      `color=c=${BG}:s=${W}x${H}:d=${dur.toFixed(3)},format=yuva420p[bg]`,
      `[0:v]${holdLastFrame}scale=${vw}:${vh}[vid]`,
      `[bg][vid]overlay=(W-w)/2:${vy}[base]`);
  } else {
    // Filmed at this exact shape already — just scale to the delivery size.
    chain.push(
      `[0:v]${holdLastFrame}scale=${W}:${H}:force_original_aspect_ratio=decrease,` +
      `pad=${W}:${H}:(ow-iw)/2:(oh-ih)/2:color=${BG},format=yuva420p[base]`);
  }

  // ── burned captions ───────────────────────────────────────────────────────
  // One timed overlay per phrase. `enable=between(t,…)` gates each PNG to the
  // window its words are actually spoken in — the offsets came from the real
  // take, so the type lands on the frame it describes.
  let last = "base";
  captionLayers.forEach((c, i) => {
    const label = i === captionLayers.length - 1 && !needsPostCaption ? "outv" : `o${i}`;
    const y = `${Math.round(H * capY)}-h/2`;
    chain.push(
      `[${last}][${firstCaptionInput + i}:v]overlay=(W-w)/2:${y}` +
      `:enable='between(t,${c.from.toFixed(3)},${c.to.toFixed(3)})'[${label}]`);
    last = label;
  });

  // A terminal closing card owns the final frame. Replacing the captured card
  // through the end removes both obsolete copy and the browser reveal that the
  // live signboard transition otherwise records after its hold.
  if (terminal) {
    const label = showBar ? "terminal-card" : "outv";
    chain.push(
      `[${last}][${terminalInput}:v]overlay=0:0:` +
      `enable='gte(t,${terminal.startSec.toFixed(3)})'[${label}]`,
    );
    last = label;
  }

  // ── progress bar ──────────────────────────────────────────────────────────
  if (chapterList.length) {
    const bh = Math.max(10, Math.round(H * 0.0153));
    const ordered = chapterList
      .map((chapter) => ({ ...chapter, startSec:Number(chapter.startSec) }))
      .filter((chapter) => Number.isFinite(chapter.startSec) && chapter.startSec >= 0)
      .sort((a, b) => a.startSec - b.startSec);
    // Build the muted segmented track first. The old drawbox width expression
    // referenced `t`, but drawbox evaluates geometry only at initialization,
    // so some ffmpeg builds painted a complete bar at frame zero.
    ordered.forEach((chapter, index) => {
      const x0 = index === 0 ? 0 : Math.round(W * chapter.startSec / dur);
      const endSec = ordered[index + 1]?.startSec ?? dur;
      const x1 = index === ordered.length - 1 ? W : Math.round(W * endSec / dur);
      const track = chapter.trackColor
        ? chapterColor(chapter.trackColor, "#d8d8d8")
        : dimChapterColor(chapterColor(chapter.color));
      const trackLabel = `chapter-track-${index}`;
      chain.push(
        `[${last}]drawbox=x=${x0}:y=${H - bh}:w=${Math.max(1, x1 - x0)}:h=${bh}` +
        `:color=${track}@0.58:t=fill[${trackLabel}]`,
      );
      last = trackLabel;
    });
    const fillColor = chapterColor(ordered[0]?.color, "#a58cbc");
    const progressLabel = "chapter-progressed";
    chain.push(
      `color=c=${fillColor}@0.92:s=${W}x${bh}:r=${F.fps || 30}:d=${dur.toFixed(3)},` +
      `format=rgba,scale=w='max(1,iw*t/${dur.toFixed(3)})':h=ih:eval=frame[chapter-progress]`,
      `[${last}][chapter-progress]overlay=0:${H - bh}:eval=frame:shortest=1[${progressLabel}]`,
    );
    last = progressLabel;
    ordered.forEach((chapter, index) => {
      if (index === ordered.length - 1) return;
      const endSec = ordered[index + 1]?.startSec ?? dur;
      const dividerX = Math.max(0, Math.round(W * endSec / dur) - 1);
      const dividerLabel = `chapter-divider-${index}`;
      chain.push(
        `[${last}]drawbox=x=${dividerX}:y=${H - bh}:w=2:h=${bh}` +
        `:color=#fffdf2@0.42:t=fill[${dividerLabel}]`,
      );
      last = dividerLabel;
    });
    const playheadWidth = Math.max(4, Math.round(W * 0.0023));
    chain.push(
      `color=c=#eee7f3@0.96:s=${playheadWidth}x${bh}:r=${F.fps || 30}:` +
      `d=${dur.toFixed(3)},format=rgba[chapter-playhead]`,
      `[${last}][chapter-playhead]overlay=` +
      `x='max(0,min(main_w-overlay_w,main_w*t/${dur.toFixed(3)}-overlay_w/2))':` +
      `y=${H - bh}:eval=frame:shortest=1[outv]`,
    );
    last = "outv";
  } else if (F.bar) {
    const bh = Math.max(6, Math.round(H * 0.006));
    chain.push(
      `[${last}]drawbox=x=0:y=${H - bh}:w='iw*t/${dur.toFixed(3)}':h=${bh}` +
      `:color=${ACCENT}@0.95:t=fill[outv]`);
    last = "outv";
  }
  if (last !== "outv") chain.push(`[${last}]null[outv]`);

  // The body is encoded to settings a title card can be concatenated onto:
  // fixed fps, yuv420p, stereo 48k. Without pinning these the two halves differ and
  // the concat demuxer silently drops one of them.
  const FPS = F.fps || 30;

  const encodedOut = brandChrome ? `${out}.pre-brand.mp4` : out;
  args.push(
    // Hundreds of short caption PNG inputs can make ffmpeg eagerly create more
    // scaler workers than macOS will grant, producing a nondeterministic
    // `Resource temporarily unavailable` before frame zero. One filter graph
    // thread is fast enough for this offline pass and removes that ceiling.
    "-filter_complex_threads", "1",
    "-filter_complex", chain.join(";"),
    "-map", "[outv]", "-map", "0:a?",
    "-r", String(FPS),
    "-c:v", "libx264",
    "-preset", STAGE_MODE ? "slow" : "medium",
    "-crf", STAGE_MODE ? "15" : "19",
    "-pix_fmt", "yuv420p",
    // Narration can end before a programmed closing signboard. Extend the
    // audio with digital silence through the full filmed negative so
    // `-shortest` trims to the picture, not to the last spoken word.
    "-af", `apad=whole_dur=${dur.toFixed(3)}`,
    "-c:a", "aac", "-b:a", "192k", "-ar", "48000", "-ac", "2",
    "-shortest",
    "-movflags", "+faststart",
    encodedOut);

  try {
    execFileSync(FFMPEG, args, { stdio: ["ignore", "ignore", "pipe"] });
  } catch (err) {
    // execFileSync throws with stderr as a raw Buffer; printed straight it is a
    // wall of byte codes. Surface the last few lines — that is where ffmpeg says
    // which filter it choked on.
    const msg = (err.stderr?.toString() || "").trim().split("\n").slice(-22).join("\n");
    throw new Error(`ffmpeg failed (${format}):\n${msg}`);
  }
  if (brandChrome) {
    try {
      applyBrandChrome({
        input:encodedOut, out, theme:brandChrome, workDir, format,
        context:{ title, chapters:chapterList },
      });
    } finally {
      if (existsSync(encodedOut)) unlinkSync(encodedOut);
    }
  }
  embedChapters({ video:out, chapters:chapterList, durationSec:dur, workDir });
  return { out, W, H, cues: pngs.length };
}
