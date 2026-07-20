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
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const FFMPEG = process.env.FFMPEG || "ffmpeg";
const HERE = new URL(".", import.meta.url).pathname;
const STAGE_MODE = process.env.CAPTUTOR_STAGE_MODE === "1";

// FUSER'S brand, not AC's. These are the client's deliverables, so the type is
// theirs: Marund, the face their own og-image renderer uses for every shareable
// Fuser image (apps/og-image/src/components/FuserTagline.tsx). It ships as a
// variable .woff2, which ImageMagick cannot read, so `assets/Marund.ttf` is that
// file decompressed — regenerate with wawoff2 if they update it.
//
// Colours are Tailwind's, which is what services/og-image/colors.ts resolves to.
const MARUND = process.env.CAPTUTOR_FONT || `${HERE}../assets/Marund.ttf`;
const CAPTION_STYLE = "large-white-black-outline-v2";

// Marund is a Latin face. It has NO Hangul, no Han, no Devanagari — ask it for
// Korean and ImageMagick returns a blank strip (it silently drops every glyph it
// cannot find and keeps the punctuation, which is a very easy thing to ship
// without noticing). So the brand face holds for the Latin locales, and the
// scripts it does not cover fall back to a system face designed for them.
//
// This is not a compromise of the brand; it is what fuser's own app does. A
// webfont stack degrades the same way in the browser.
const SCRIPT_FONTS = {
  "ko":    "/System/Library/Fonts/AppleSDGothicNeo.ttc",
  "zh-CN": "/System/Library/Fonts/Hiragino Sans GB.ttc",  // NOT PingFang — see below
  "hi":    "/System/Library/Fonts/Supplemental/Kohinoor.ttc",
  "fa":    "/System/Library/Fonts/Supplemental/GeezaPro.ttc",
};

/// Pick a font that can actually draw this locale — and refuse to guess.
///
/// A missing font file does not error: ImageMagick falls back, drops every glyph
/// it cannot draw, and hands back a caption containing only the punctuation. We
/// shipped a whole Chinese take that way (PingFang is NOT at
/// /System/Library/Fonts/PingFang.ttc, so it silently used Marund and the burned
/// captions read just "App"). Throwing here is the only way that stays fixed.
function fontFor(locale) {
  const alt = SCRIPT_FONTS[locale];
  if (!alt) return MARUND;  // Latin — the brand face covers it
  if (!existsSync(alt)) {
    throw new Error(
      `no font for "${locale}" at ${alt}. Marund cannot draw this script, and ` +
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

let FONT = MARUND;  // set per-render by deliver()
const TEXT = "#ffffff";    // plain white — subtitles are not a brand surface
const SHADOW = "#000000";
const BG = "#0a0a0a";      // neutral-950
const ACCENT = "#4f46e5";  // indigo-600 — the app's own action colour

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
    // Stage Mode runs the display at 2× HiDPI: 1008 logical points become a
    // crisp 2016×1260 browser inside a native 2560×1440 desktop frame.
    win: STAGE_MODE ? { w: 1008, h: 630 } : { w: 1512, h: 945 },
    out: STAGE_MODE ? { w: 2560, h: 1440 } : { w: 1512, h: 945 },
    // Stage recordings are viewed inside a docs player, often at half their
    // encoded size. Use presentation-scale captions so they remain readable,
    // and lift them slightly to give the thicker outline breathing room.
    capWidth: 0.84,
    capPx: STAGE_MODE ? 58 : 44,
    capY: STAGE_MODE ? 0.86 : 0.90,
    bar: false,
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

/// Rasterize one caption to a transparent PNG: Marund in neutral-50 over a hard
/// black shadow, auto-wrapped to the band width. ImageMagick loads the .ttf by
/// path, so nothing has to be installed into the system font book.
function cuePng(text, { width, px, out }) {
  const common = [
    "-background", "none",
    "-font", FONT,
    "-pointsize", String(px),
    "-size", `${width}x`,
    "-gravity", "center",
  ];
  // White type in a black OUTLINE — the subtitle convention, and the right one.
  // A drop shadow only darkens one side, so type stays legible over a dark UI and
  // smears over a light one. An outline surrounds every stroke, so the caption
  // holds on any background, which is the whole job when the thing behind it is a
  // UI that changes colour from shot to shot.
  //
  // Two passes, not one: ImageMagick draws the stroke ON TOP of the fill, so a
  // single stroked pass eats the letterforms from the inside and thin glyphs fill
  // in solid black. Draw the stroked copy, then lay the unstroked fill over it.
  // Proportional and strong enough to remain visibly black after a 2× HiDPI
  // take is scaled into the docs player. The white fill pass keeps the thicker
  // outside stroke from closing the counters or turning into bubble lettering.
  const stroke = Math.max(3, Math.round(px / 12));
  const outline = `${out}.o.png`;
  const fill = `${out}.f.png`;
  execFileSync("magick", [...common,
    "-stroke", SHADOW, "-strokewidth", String(stroke), "-fill", TEXT,
    `caption:${text}`, outline]);
  execFileSync("magick", [...common,
    "-stroke", "none", "-fill", TEXT,
    `caption:${text}`, fill]);
  execFileSync("magick", [outline, fill, "-composite", out]);
  assertHasInk(out, text);
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

/// Group each beat's words into on-screen caption phrases — same rule the VTT
/// uses, so burned and soft captions never disagree.
function phrases(cues, { maxWords = 6, pauseMs = 380 } = {}) {
  const out = [];
  const HAS_WORD = /[\p{L}\p{N}]/u;
  for (const beat of cues) {
    let cur = [];
    const flush = () => {
      if (!cur.length) return;
      // A cue of pure punctuation ("—") is not a caption. It renders as an almost
      // empty PNG, which the blank-caption guard correctly refuses — so never
      // make one: fold it into the phrase before it.
      if (!HAS_WORD.test(cur.map((w) => w.text).join(""))) {
        const prev = out[out.length - 1];
        if (prev) {
          prev.text += " " + cur.map((w) => w.text).join(" ");
          prev.to = beat.offsetSec + cur[cur.length - 1].toMs / 1000;
        }
        cur = [];
        return;
      }
      out.push({
        from: beat.offsetSec + cur[0].fromMs / 1000,
        to: beat.offsetSec + cur[cur.length - 1].toMs / 1000,
        text: cur.map((w) => w.text).join(" "),
      });
      cur = [];
    };
    for (const [i, w] of beat.words.entries()) {
      cur.push(w);
      const next = beat.words[i + 1];
      if (/[.!?,—]$/.test(w.text) || (next && next.fromMs - w.toMs >= pauseMs)
          || cur.length >= maxWords) flush();
    }
    flush();
  }
  return out;
}

export function deliver({ clip, cues, format, out, workDir, locale = "en" }) {
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
  const W = F.out.w;
  const H = F.out.h;

  // Caption PNGs are cached because multilingual rasterization is expensive.
  // Version the directory so a style change can never silently reuse an older
  // size or outline during a cheap recut of an existing take.
  const capDir = join(workDir, `caps-${format}-${CAPTION_STYLE}`);
  mkdirSync(capDir, { recursive: true });

  const band = Math.round(W * F.capWidth);
  const cuts = phrases(cues);
  const pngs = cuts.map((c, i) => {
    const p = join(capDir, `${String(i).padStart(3, "0")}.png`);
    if (!existsSync(p)) cuePng(c.text, { width: band, px: F.capPx, out: p });
    return { ...c, png: p };
  });

  // ── video base ────────────────────────────────────────────────────────────
  const args = ["-y", "-i", clip];
  for (const p of pngs) args.push("-i", p.png);

  const chain = [];
  if (F.compose) {
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
  pngs.forEach((c, i) => {
    const label = i === pngs.length - 1 && !F.bar ? "outv" : `o${i}`;
    const y = `${Math.round(H * F.capY)}-h/2`;
    chain.push(
      `[${last}][${i + 1}:v]overlay=(W-w)/2:${y}` +
      `:enable='between(t,${c.from.toFixed(3)},${c.to.toFixed(3)})'[${label}]`);
    last = label;
  });

  // ── progress bar ──────────────────────────────────────────────────────────
  if (F.bar) {
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
  const FPS = 30;

  args.push(
    "-filter_complex", chain.join(";"),
    "-map", "[outv]", "-map", "0:a?",
    "-r", String(FPS),
    "-c:v", "libx264",
    "-preset", STAGE_MODE ? "slow" : "medium",
    "-crf", STAGE_MODE ? "15" : "19",
    "-pix_fmt", "yuv420p",
    "-c:a", "aac", "-b:a", "192k", "-ar", "48000", "-ac", "2",
    "-shortest",
    "-movflags", "+faststart",
    out);

  try {
    execFileSync(FFMPEG, args, { stdio: ["ignore", "ignore", "pipe"] });
  } catch (err) {
    // execFileSync throws with stderr as a raw Buffer; printed straight it is a
    // wall of byte codes. Surface the last few lines — that is where ffmpeg says
    // which filter it choked on.
    const msg = (err.stderr?.toString() || "").trim().split("\n").slice(-22).join("\n");
    throw new Error(`ffmpeg failed (${format}):\n${msg}`);
  }
  return { out, W, H, cues: pngs.length };
}
