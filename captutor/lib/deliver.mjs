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
const STAGE_MODE = process.env.CAPTUTOR_STAGE_MODE === "1";
const VERTICAL_MODE = process.env.CAPTUTOR_VERTICAL_MODE === "1";

// Deliberately ordinary subtitle typography. Captions are navigation, not a
// brand surface: regular Arial stays readable over a busy UI and produces the
// familiar neutral shape people already recognize as subtitles.
const LATIN_FONT = process.env.CAPTUTOR_FONT
  || "/System/Library/Fonts/Supplemental/Arial.ttf";
const CAPTION_STYLE = "arial-caption-box-karaoke-v5";

// Arial does not cover every script, so non-Latin locales use the corresponding
// macOS system face instead of silently dropping glyphs.
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
const ACTIVE_TEXT = "#facc15"; // warm yellow — familiar, restrained karaoke cue
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

/// Rasterize a plain subtitle: regular Arial over a compact translucent black
/// box. There is no outline, shadow, gradient, or decorative treatment.
/// `activeIndex` changes only the spoken word's fill for timed tracking.
function cuePng(words, { width, px, out, activeIndex = -1 }) {
  const layout = layoutWords(words, { width, px });
  const mvg = (text) => text.replaceAll("\\", "\\\\").replaceAll('"', '\\"');
  const args = [
    "-size", `${layout.width}x${layout.height}`, "xc:none",
    "-fill", "rgba(0,0,0,0.68)", "-stroke", "none",
  ];
  for (const box of layout.boxes) {
    const radius = Math.round(px * 0.16);
    args.push(
      "-draw",
      `roundrectangle ${box.x1},${box.y1},${box.x2},${box.y2},${radius},${radius}`,
    );
  }
  args.push("-font", FONT, "-pointsize", String(px));
  for (const word of layout.words) {
    args.push(
      "-fill", word.index === activeIndex ? ACTIVE_TEXT : TEXT,
      "-draw", `text ${word.x},${word.baseline} \"${mvg(word.text)}\"`,
    );
  }
  args.push(out);
  execFileSync("magick", args);
  assertHasInk(out, words.map((word) => word.text).join(" "));
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
          prev.words.push(...cur.map((w) => ({
            text: w.text,
            from: beat.offsetSec + w.fromMs / 1000,
            to: beat.offsetSec + w.toMs / 1000,
          })));
        }
        cur = [];
        return;
      }
      out.push({
        from: beat.offsetSec + cur[0].fromMs / 1000,
        to: beat.offsetSec + cur[cur.length - 1].toMs / 1000,
        text: cur.map((w) => w.text).join(" "),
        words: cur.map((w) => ({
          text: w.text,
          from: beat.offsetSec + w.fromMs / 1000,
          to: beat.offsetSec + w.toMs / 1000,
        })),
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
    const stem = String(i).padStart(3, "0");
    const base = join(capDir, `${stem}-base.png`);
    if (!existsSync(base)) cuePng(c.words, { width: band, px: F.capPx, out: base });
    const highlights = c.words.map((word, wordIndex) => {
      const png = join(capDir, `${stem}-word-${String(wordIndex).padStart(2, "0")}.png`);
      if (!existsSync(png)) {
        cuePng(c.words, { width: band, px: F.capPx, out: png, activeIndex: wordIndex });
      }
      return { ...word, png };
    });
    return { ...c, png: base, highlights };
  });

  // Each phrase has one always-white base plus a full-phrase state for every
  // word. The highlighted state is gated to that word's measured speech window;
  // between words the clean white base remains visible.
  const captionLayers = pngs.flatMap((phrase) => [
    { from: phrase.from, to: phrase.to, png: phrase.png },
    ...phrase.highlights,
  ]);

  // ── video base ────────────────────────────────────────────────────────────
  const args = ["-y", "-i", clip];
  const firstCaptionInput = 1;
  for (const p of captionLayers) args.push("-i", p.png);

  const chain = [];
  if (F.compose?.fullDesktop) {
    if (src.w !== W || src.h !== H) {
      throw new Error(
        `${format} Stage delivery needs a ${W}x${H} full-desktop negative; ` +
        `got ${src.w}x${src.h}. Record a new take instead of recutting a window capture.`,
      );
    }
    // Preserve the complete physical desktop. The sole repair is the tiny
    // ScreenCaptureKit status dot at the extreme top-right: clone a live 2×2
    // sample of the adjacent stage wallpaper over a 34×28 patch. No browser or
    // window pixels are touched.
    chain.push(
      `[0:v]${holdLastFrame}scale=${W}:${H},split=2[desktop][badgeSeed]`,
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
    const label = i === captionLayers.length - 1 && !F.bar ? "outv" : `o${i}`;
    const y = `${Math.round(H * F.capY)}-h/2`;
    chain.push(
      `[${last}][${firstCaptionInput + i}:v]overlay=(W-w)/2:${y}` +
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
  const FPS = F.fps || 30;

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
