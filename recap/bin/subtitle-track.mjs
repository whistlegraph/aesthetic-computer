#!/usr/bin/env node
// subtitle-track.mjs — emit `out/subs.ass` (Advanced SubStation Alpha)
// from `out/subs.json`. The main compose then renders subtitles via
// ffmpeg's `subtitles=` filter (libass) — single filter, sub-second
// overhead, no alpha-codec gymnastics, frame-perfect timing.
//
// Why ASS over the pre-rendered PNG track:
//   - libvpx-vp9 silently dropped alpha on the oven (pix_fmt=yuv420p
//     instead of yuva420p), so the overlaid track came through opaque
//     black and covered the slides.
//   - Even with alpha working, the concat-demuxer + fps filter pattern
//     was sparse-PTS-prone and truncated the output stream.
//   - ASS bypasses both problems: ffmpeg renders text directly into the
//     video stream at composite time using the libass subtitle library.
//
// Pill styling — translucent dark background + cream text + magenta
// border — replicates the look of the previous PNG pill via ASS box
// drawing primitives (BorderStyle=4 = opaque box).
//
// Usage: node bin/subtitle-track.mjs [audience-name]

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const audienceName = process.argv[2] || "fia";
const subsPath = `${ROOT}/out/subs.json`;
const assPath = `${ROOT}/out/subs.ass`;
const layoutsPath = `${ROOT}/out/layouts.json`;
const segmentsPath = `${ROOT}/out/segments.json`;

// Per-chunk chapter color from layouts.json + segments.json — overrides
// the libass outline color per Dialogue line so the pill border follows
// the slide color story instead of being fixed magenta.
const layoutsForColors = existsSync(layoutsPath) ? JSON.parse(readFileSync(layoutsPath, "utf8")) : {};
const segmentsForColors = existsSync(segmentsPath) ? JSON.parse(readFileSync(segmentsPath, "utf8")) : [];
function chapterColorAt(secs) {
  for (const seg of segmentsForColors) {
    if (secs >= seg.startSec && secs < seg.endSec) {
      const layout = layoutsForColors[seg.name];
      if (layout && layout.color && layout.color.rgb) {
        const [r, g, b] = layout.color.rgb;
        return `&H${b.toString(16).padStart(2,"0")}${g.toString(16).padStart(2,"0")}${r.toString(16).padStart(2,"0")}&`;
      }
      return null;
    }
  }
  return null;
}

const subs = JSON.parse(readFileSync(subsPath, "utf8"));
if (!subs.length) {
  console.error(`✗ no subtitle chunks in ${subsPath}`);
  process.exit(1);
}

// ── ASS time format: H:MM:SS.cc ────────────────────────────────────────
function assTime(t) {
  const h = Math.floor(t / 3600);
  const m = Math.floor((t % 3600) / 60);
  const s = (t % 60).toFixed(2);
  return `${h}:${String(m).padStart(2, "0")}:${String(s).padStart(5, "0")}`;
}

// ASS color = &HAABBGGRR (alpha + BGR, hex). For opaque colors use AA=00.
// Style maps:
//   PrimaryColour   → text fill
//   OutlineColour   → text outline
//   BackColour      → translucent pill background (BorderStyle=4 enables a box)
//   BorderStyle=1   → outline + shadow only (no box)
//   BorderStyle=4   → opaque box behind the text (our pill)
//   Outline         → border thickness in pixels
//   Shadow          → drop shadow distance
//   Alignment       → 1..9 numpad: 2 = bottom-center, 5 = middle-center,
//                     8 = top-center
//   MarginV         → vertical margin from the corresponding edge
//
// We want a magenta border (3px) around a translucent dark-purple pill
// with cream-yellow text, anchored at the bottom of a 1080×1920 frame.
// Bottom anchor (Alignment=2) + MarginV=215 puts the pill base around
// y=1700 of 1920 (matches the prior PNG pill's y=1690 area).
const PRIMARY = "&H00C5F7FC"; // cream #FCF7C5
const OUTLINE = "&H00B469FF"; // magenta outline #FF69B4
const BACK    = "&H80200810"; // dark purple translucent ~50% (#100820 + alpha 80)

const lines = [
  "[Script Info]",
  "ScriptType: v4.00+",
  "PlayResX: 1080",
  "PlayResY: 1920",
  "WrapStyle: 0",
  "ScaledBorderAndShadow: yes",
  "",
  "[V4+ Styles]",
  "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding",
  `Style: Default,YWFTProcessing,64,${PRIMARY},&H00FFFFFF,${OUTLINE},${BACK},1,0,0,0,100,100,-1,0,4,3,0,2,60,60,215,1`,
  "",
  "[Events]",
  "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text",
];

for (const c of subs) {
  // Sanitize: ASS uses \N for newlines and treats { ... } as override blocks
  const text = (c.text || "")
    .replace(/[\r\n]+/g, " ")
    .replace(/\{/g, "(")
    .replace(/\}/g, ")")
    .trim();
  if (!text) continue;
  // Per-segment chapter color override on the outline (\3c). Falls back
  // to the Default style's outline (magenta) when the chunk lands
  // outside any segment or before the layout step ran.
  const chapHex = chapterColorAt(c.startSec);
  const colorOverride = chapHex ? `{\\3c${chapHex}}` : "";
  lines.push(`Dialogue: 0,${assTime(c.startSec)},${assTime(c.endSec)},Default,,0,0,0,,${colorOverride}${text}`);
}

writeFileSync(assPath, lines.join("\n") + "\n");
console.log(`✓ ${assPath} · ${subs.length} cues`);
