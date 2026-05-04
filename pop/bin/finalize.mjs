#!/usr/bin/env node
// finalize.mjs — emit a finalized mp3 with ID3v2 metadata + embedded
// auto-generated, timestamped cover art. Last leg of the
// pop/big-pictures lane: takes a rendered track (output of any of the
// align/pitchsnap/timefit pipeline) and produces a streaming-ready file.
//
// Cover generation runs entirely offline — no APIs, no canvas
// dependencies. PNG is hand-rolled with zlib + a 6x10 bitmap font
// extracted from fedac/native/src/font-6x10.h (public-domain X11
// terminal font), scaled up to draw big titles at 3000×3000.
//
// Usage:
//   node bin/finalize.mjs --in out/ac-mix.mp3 --slug ac --title "aesthetic 24"
//   node bin/finalize.mjs --in out/mary-tuned.mp3 --slug mary
//   node bin/finalize.mjs --in out/plork-chorus.mp3 --slug plork --cover my-cover.png
//   node bin/finalize.mjs --in out/ac-mix.mp3 --slug ac --out custom-final.mp3 --force

import { spawnSync } from "node:child_process";
import { writeFileSync, readFileSync, mkdirSync, existsSync, statSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { homedir } from "node:os";
import { deflateSync } from "node:zlib";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

// ── argv parsing (matches say.mjs / timefit.mjs idiom) ─────────────────
function parseArgs(argv) {
  const flags = {};
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--")) {
      const k = a.slice(2);
      const next = argv[i + 1];
      if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
      else flags[k] = true;
    } else positional.push(a);
  }
  return { flags, positional };
}

function expandHome(p) {
  if (!p || typeof p !== "string") return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const { flags } = parseArgs(process.argv.slice(2));

if (!flags.in || !flags.slug) {
  console.error("usage: node bin/finalize.mjs --in <mp3> --slug <slug> [--title \"...\"] [--cover <png>] [--out <path>] [--force]");
  process.exit(1);
}

const IN_PATH = resolve(process.cwd(), expandHome(flags.in));
if (!existsSync(IN_PATH)) {
  console.error(`✗ input mp3 not found: ${IN_PATH}`);
  process.exit(1);
}

const SLUG = String(flags.slug).trim().toLowerCase();
const TITLE = flags.title && flags.title !== true
  ? String(flags.title)
  : SLUG.replace(/[-_]+/g, " ");
const FORCE = flags.force === true;
const OUT_PATH = expandHome(flags.out)
  ? resolve(process.cwd(), expandHome(flags.out))
  : `${ROOT}/big-pictures/out/${SLUG}-final.mp3`;
const COVER_OVERRIDE = flags.cover && flags.cover !== true
  ? resolve(process.cwd(), expandHome(flags.cover))
  : null;

mkdirSync(dirname(OUT_PATH), { recursive: true });

// ── Probe input duration (mirror of timefit.mjs) ───────────────────────
const probe = spawnSync(
  "ffprobe",
  ["-v", "error", "-show_entries", "format=duration",
   "-of", "default=noprint_wrappers=1:nokey=1", IN_PATH],
  { encoding: "utf8" },
);
const inputDur = Number(probe.stdout.trim());
if (!(inputDur > 0)) {
  console.error(`✗ ffprobe could not read duration of ${IN_PATH}`);
  process.exit(1);
}

// ── Build metadata ─────────────────────────────────────────────────────
const now = new Date();
const pad = (n) => String(n).padStart(2, "0");
const isoDate = `${now.getUTCFullYear()}-${pad(now.getUTCMonth() + 1)}-${pad(now.getUTCDate())}`;
const isoYear = String(now.getUTCFullYear());
const isoStamp = now.toISOString();
const localStampShort = `${isoDate} ${pad(now.getUTCHours())}:${pad(now.getUTCMinutes())} UTC`;

const ARTIST = "@jeffrey";
const ALBUM = "big pictures";
const GENRE = "emo trap";

// Lyric file (sibling .txt) → USLT
const lyricPath = `${ROOT}/big-pictures/${SLUG}.txt`;
let lyricsText = null;
if (existsSync(lyricPath)) {
  lyricsText = readFileSync(lyricPath, "utf8").trim();
}

const meta = {
  title: TITLE,
  artist: ARTIST,
  album_artist: ARTIST,
  album: ALBUM,
  date: isoDate,
  year: isoYear,
  genre: GENRE,
  comment: `rendered ${isoStamp} · source: ${basename(IN_PATH)}`,
};

// ── Cache key (idempotent on input contents + key metadata) ────────────
const inputBuf = readFileSync(IN_PATH);
const cacheKey = createHash("sha256").update(inputBuf).update(JSON.stringify({
  slug: SLUG, title: TITLE, lyricsText, coverOverride: COVER_OVERRIDE,
})).digest("hex").slice(0, 16);
const hashFile = `${OUT_PATH}.hash`;

if (!FORCE && existsSync(OUT_PATH) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === cacheKey) {
    const size = (statSync(OUT_PATH).size / 1024).toFixed(0);
    console.log(`✓ ${OUT_PATH} cached (${size} KB · hash ${cacheKey}) — skipping finalize`);
    process.exit(0);
  }
}

// ── 6×10 bitmap font (public-domain X11 fixed, mined from fedac) ───────
// Each glyph is 10 bytes; high 6 bits of each byte are the row pixels
// (MSB = leftmost column). Glyph 0 = ASCII 32 (space). 95 glyphs total.
const FONT_W = 6, FONT_H = 10;
const FONT_B64 =
  "AAAAAAAAAAAAAAAgICAgIAAgAAAAUFBQAAAAAAAAAFBQ+FD4UFAAAAAgcKBwKHAgAAAASKhQIFCo" +
  "kAAAAECgoECokGgAAAAgICAAAAAAAAAAECBAQEAgEAAAAEAgEBAQIEAAAAAAiFD4UIgAAAAAACAg" +
  "+CAgAAAAAAAAAAAAMCBAAAAAAAD4AAAAAAAAAAAAAAAgcCAAAAgIECBAgIAAAAAgUIiIiFAgAAAA" +
  "IGCgICAg+AAAAHCICDBAgPgAAAD4CBAwCIhwAAAAEDBQkPgQEAAAAPiAsMgIiHAAAAAwQICwyIhw" +
  "AAAA+AgQECBAQAAAAHCIiHCIiHAAAABwiJhoCBBgAAAAACBwIAAgcCAAAAAgcCAAMCBAAAAIECBA" +
  "IBAIAAAAAAD4APgAAAAAAEAgEAgQIEAAAABwiBAgIAAgAAAAcIiYqLCAcAAAACBQiIj4iIgAAADw" +
  "SEhwSEjwAAAAcIiAgICIcAAAAPBISEhISPAAAAD4gIDwgID4AAAA+ICA8ICAgAAAAHCIgICYiHAA" +
  "AACIiIj4iIiIAAAAcCAgICAgcAAAADgQEBAQkGAAAACIkKDAoJCIAAAAgICAgICA+AAAAIiI2KiI" +
  "iIgAAACIiMiomIiIAAAAcIiIiIiIcAAAAPCIiPCAgIAAAABwiIiIiKhwCAAA8IiI8KCQiAAAAHCI" +
  "gHAIiHAAAAD4ICAgICAgAAAAiIiIiIiIcAAAAIiIiFBQUCAAAACIiIioqNiIAAAAiIhQIFCIiAAA" +
  "AIiIUCAgICAAAAD4CBAgQID4AAAAcEBAQEBAcAAAAICAQCAQCAgAAABwEBAQEBBwAAAAIFCIAAAA" +
  "AAAAAAAAAAAAAAD4ACAQAAAAAAAAAAAAAABwCHiIeAAAAICAsMiIyLAAAAAAAHCIgIhwAAAACAho" +
  "mIiYaAAAAAAAcIj4gHAAAAAwSEDwQEBAAAAAAAB4iIh4CIhwAICAsMiIiIgAAAAgAGAgICBwAAAA" +
  "CAAYCAgISEgwAICAiJDgkIgAAABgICAgICBwAAAAAADQqKioiAAAAAAAsMiIiIgAAAAAAHCIiIhw" +
  "AAAAAACwyIjIsICAAAAAaJiImGgICAAAALDIgICAAAAAAABwgHAI8AAAAEBA8EBASDAAAAAAAIiI" +
  "iJhoAAAAAACIiFBQIAAAAAAAiIioqFAAAAAAAIhQIFCIAAAAAACIiJhoCIhwAAAA+BAgQPgAAAAY" +
  "IBBgECAYAAAAICAgICAgIAAAAGAQIBggEGAAAABIqJAAAAAAAAA=";
const FONT = Buffer.from(FONT_B64, "base64");

// pixel sample at (x, y) in glyph space [0..6) × [0..10).
function glyphPixel(ch, x, y) {
  const code = ch.charCodeAt(0);
  if (code < 32 || code > 126) return false;
  const idx = code - 32;
  const row = FONT[idx * FONT_H + y];
  if (row === undefined) return false;
  // High 6 bits hold the columns; bit 7 = leftmost.
  return (row & (0x80 >> x)) !== 0;
}

// Measure text width at scale (no kerning, fixed-pitch font).
function textWidth(str, scale) {
  return str.length * FONT_W * scale;
}

// ── Pure-Node 8-bit RGB framebuffer ────────────────────────────────────
function makeFB(w, h, bg) {
  const buf = Buffer.alloc(w * h * 3);
  for (let i = 0; i < buf.length; i += 3) {
    buf[i] = bg[0]; buf[i + 1] = bg[1]; buf[i + 2] = bg[2];
  }
  return { w, h, buf };
}

function setPx(fb, x, y, rgb) {
  if (x < 0 || y < 0 || x >= fb.w || y >= fb.h) return;
  const i = (y * fb.w + x) * 3;
  fb.buf[i] = rgb[0]; fb.buf[i + 1] = rgb[1]; fb.buf[i + 2] = rgb[2];
}

function fillRect(fb, x0, y0, w, h, rgb) {
  for (let y = y0; y < y0 + h; y++) {
    for (let x = x0; x < x0 + w; x++) setPx(fb, x, y, rgb);
  }
}

function drawText(fb, str, x0, y0, scale, rgb) {
  for (let i = 0; i < str.length; i++) {
    const ch = str[i];
    const gx0 = x0 + i * FONT_W * scale;
    for (let gy = 0; gy < FONT_H; gy++) {
      for (let gx = 0; gx < FONT_W; gx++) {
        if (!glyphPixel(ch, gx, gy)) continue;
        // Stamp scale×scale square per pixel.
        fillRect(fb, gx0 + gx * scale, y0 + gy * scale, scale, scale, rgb);
      }
    }
  }
}

// ── Hand-rolled PNG writer (deflate, RGB8, no filtering) ───────────────
function crc32(buf) {
  let c, table = crc32.table;
  if (!table) {
    table = new Uint32Array(256);
    for (let n = 0; n < 256; n++) {
      c = n;
      for (let k = 0; k < 8; k++) c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
      table[n] = c >>> 0;
    }
    crc32.table = table;
  }
  c = 0xFFFFFFFF;
  for (let i = 0; i < buf.length; i++) c = table[(c ^ buf[i]) & 0xFF] ^ (c >>> 8);
  return (c ^ 0xFFFFFFFF) >>> 0;
}

function chunk(type, data) {
  const len = Buffer.alloc(4); len.writeUInt32BE(data.length, 0);
  const tbuf = Buffer.from(type, "ascii");
  const crcBuf = Buffer.alloc(4);
  crcBuf.writeUInt32BE(crc32(Buffer.concat([tbuf, data])), 0);
  return Buffer.concat([len, tbuf, data, crcBuf]);
}

function writePNG(fb) {
  const sig = Buffer.from([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]);
  // IHDR
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(fb.w, 0);
  ihdr.writeUInt32BE(fb.h, 4);
  ihdr[8] = 8;       // bit depth
  ihdr[9] = 2;       // color type: RGB
  ihdr[10] = 0;      // compression
  ihdr[11] = 0;      // filter
  ihdr[12] = 0;      // interlace
  // IDAT — prepend 0x00 filter byte to each scanline
  const stride = fb.w * 3;
  const filtered = Buffer.alloc(fb.h * (stride + 1));
  for (let y = 0; y < fb.h; y++) {
    filtered[y * (stride + 1)] = 0;
    fb.buf.copy(filtered, y * (stride + 1) + 1, y * stride, (y + 1) * stride);
  }
  const idat = deflateSync(filtered, { level: 6 });
  return Buffer.concat([sig, chunk("IHDR", ihdr), chunk("IDAT", idat), chunk("IEND", Buffer.alloc(0))]);
}

// ── Slug-deterministic palette (HSL → RGB, AC-saturated) ───────────────
function hslToRgb(h, s, l) {
  // h ∈ [0..360), s ∈ [0..1], l ∈ [0..1]
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const hp = h / 60;
  const x = c * (1 - Math.abs((hp % 2) - 1));
  let r1 = 0, g1 = 0, b1 = 0;
  if (hp < 1) { r1 = c; g1 = x; }
  else if (hp < 2) { r1 = x; g1 = c; }
  else if (hp < 3) { g1 = c; b1 = x; }
  else if (hp < 4) { g1 = x; b1 = c; }
  else if (hp < 5) { r1 = x; b1 = c; }
  else { r1 = c; b1 = x; }
  const m = l - c / 2;
  return [Math.round((r1 + m) * 255), Math.round((g1 + m) * 255), Math.round((b1 + m) * 255)];
}

function paletteFromSlug(slug) {
  const h = createHash("sha256").update(slug).digest();
  const hue = (h[0] / 256) * 360;
  // AC palette: rich, saturated, fairly dark backgrounds with a complementary accent
  const bg = hslToRgb(hue, 0.72, 0.16);
  const accent = hslToRgb((hue + 36) % 360, 0.85, 0.58);
  // Bright cream type for max contrast against the dark bg
  const fg = hslToRgb(hue, 0.10, 0.96);
  const dim = hslToRgb(hue, 0.40, 0.62);
  return { bg, fg, accent, dim };
}

// ── Compose the cover (3000×3000) ──────────────────────────────────────
function buildCover(slug, title, stampShort) {
  const W = 3000, H = 3000;
  const pal = paletteFromSlug(slug);
  const fb = makeFB(W, H, pal.bg);

  // Top decoration bars — geometric, hand-drawn-feeling.
  const h = createHash("sha256").update(slug).digest();
  const barCount = 5 + (h[1] % 5);
  for (let i = 0; i < barCount; i++) {
    fillRect(fb, 200, 90 + i * 22, W - 400, 4, pal.dim);
  }

  // ── Vertical bands (no overlap) ─────────────────────────────────────
  //  90..200   top bars
  // 280..1080  title (scale ≤80)
  // 1180..1480 waveform
  // 1560..1780 stamp
  // 1860..2230 album mark "big pictures"
  // 2310..2870 handle "@jeffrey"
  // 2900..2980 bottom bars

  // Title — large, occupies most of the top band.
  const titleStr = String(title || slug).toLowerCase();
  let titleScale = Math.floor((W - 400) / (titleStr.length * FONT_W));
  titleScale = Math.min(titleScale, 80);
  titleScale = Math.max(titleScale, 28);
  const titleW = textWidth(titleStr, titleScale);
  const titleX = Math.round((W - titleW) / 2);
  const titleH = FONT_H * titleScale;
  const titleY = 280 + Math.round((800 - titleH) / 2);
  const shadowOff = Math.max(4, Math.round(titleScale * 0.18));
  drawText(fb, titleStr, titleX + shadowOff, titleY + shadowOff, titleScale, pal.accent);
  drawText(fb, titleStr, titleX, titleY, titleScale, pal.fg);

  // Waveform glyph
  const wvBars = 32;
  const wvY = 1330;
  const wvW = Math.round(W * 0.66);
  const wvX = Math.round((W - wvW) / 2);
  const barWidth = Math.floor(wvW / wvBars) - 4;
  for (let i = 0; i < wvBars; i++) {
    const seed = h[i % h.length];
    const amp = 30 + ((seed * (i + 1)) % 130);
    const x = wvX + i * Math.floor(wvW / wvBars);
    fillRect(fb, x, wvY - amp, barWidth, amp * 2, pal.accent);
  }

  // Timestamp — small, centered.
  const stampScale = 22;
  const stampW = textWidth(stampShort, stampScale);
  drawText(fb, stampShort, Math.round((W - stampW) / 2), 1560, stampScale, pal.dim);

  // Album mark "big pictures" — auto-fit width.
  const mark = "big pictures";
  let markScale = Math.floor((W - 300) / (mark.length * FONT_W));
  markScale = Math.min(markScale, 38);
  markScale = Math.max(markScale, 24);
  const markW = textWidth(mark, markScale);
  const markH = FONT_H * markScale;
  const markY = 1860 + Math.round((370 - markH) / 2);
  drawText(fb, mark, Math.round((W - markW) / 2), markY, markScale, pal.fg);

  // Handle "@jeffrey" — biggest text on the cover, max-fit to width.
  const handle = "@jeffrey";
  let handleScale = Math.floor((W - 240) / (handle.length * FONT_W));
  handleScale = Math.min(handleScale, 70);
  handleScale = Math.max(handleScale, 28);
  const handleW = textWidth(handle, handleScale);
  const handleX = Math.round((W - handleW) / 2);
  const handleH = FONT_H * handleScale;
  const handleY = 2310 + Math.round((560 - handleH) / 2);
  const handleShadow = Math.max(6, Math.round(handleScale * 0.14));
  drawText(fb, handle, handleX + handleShadow, handleY + handleShadow, handleScale, pal.accent);
  drawText(fb, handle, handleX, handleY, handleScale, pal.fg);

  // Bottom decoration bars
  for (let i = 0; i < barCount; i++) {
    fillRect(fb, 200, H - 90 - i * 14, W - 400, 4, pal.dim);
  }

  return writePNG(fb);
}

// ── Resolve cover path ─────────────────────────────────────────────────
let coverPath;
if (COVER_OVERRIDE) {
  if (!existsSync(COVER_OVERRIDE)) {
    console.error(`✗ --cover not found: ${COVER_OVERRIDE}`);
    process.exit(1);
  }
  coverPath = COVER_OVERRIDE;
  console.log(`→ using provided cover: ${coverPath}`);
} else {
  coverPath = `${ROOT}/big-pictures/out/${SLUG}-cover.png`;
  console.log(`→ generating cover: ${coverPath} (3000×3000, ${localStampShort})`);
  const png = buildCover(SLUG, TITLE, localStampShort);
  writeFileSync(coverPath, png);
  console.log(`  wrote ${(png.length / 1024).toFixed(0)} KB`);
}

// ── ffmpeg: mux audio + cover, attach ID3v2 ────────────────────────────
// Notes:
//   * `-map 0:a -map 1` keeps audio + cover.
//   * `-c copy` preserves the source mp3 bitstream (no re-encode).
//   * `-disposition:v attached_pic` sets the APIC role.
//   * `-id3v2_version 3` keeps ID3v2.3 (best player support).
//   * `-write_id3v1 1` is a courtesy for legacy players.
//   * `-metadata:s:v` sets the per-stream cover description.
const args = [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", IN_PATH,
  "-i", coverPath,
  "-map", "0:a",
  "-map", "1",
  "-c", "copy",
  "-id3v2_version", "3",
  "-write_id3v1", "1",
  "-disposition:v", "attached_pic",
  "-metadata:s:v", "title=Album cover",
  "-metadata:s:v", "comment=Cover (front)",
];

for (const [k, v] of Object.entries(meta)) {
  args.push("-metadata", `${k}=${v}`);
}
if (lyricsText) {
  args.push("-metadata", `lyrics-eng=${lyricsText}`);
}

args.push(OUT_PATH);

console.log(`→ ffmpeg mux · in=${basename(IN_PATH)} cover=${basename(coverPath)} → ${basename(OUT_PATH)}`);
const ff = spawnSync("ffmpeg", args, { stdio: "inherit" });
if (ff.status !== 0) {
  console.error("✗ ffmpeg failed");
  process.exit(1);
}

// ── Verify output ──────────────────────────────────────────────────────
const verify = spawnSync(
  "ffprobe",
  ["-v", "error", "-show_entries",
   "format=duration:format_tags=title,artist,album,album_artist,date,genre,comment,lyrics-eng:stream=codec_type,codec_name,disposition:stream_tags=title,comment",
   "-of", "default=noprint_wrappers=1", OUT_PATH],
  { encoding: "utf8" },
);

const outDur = (() => {
  const m = (verify.stdout || "").match(/^duration=(.+)$/m);
  return m ? Number(m[1]) : null;
})();

const hasCover = (verify.stdout || "").includes("codec_type=video");
const drift = outDur !== null ? outDur - inputDur : null;

writeFileSync(hashFile, cacheKey + "\n");
const outSize = (statSync(OUT_PATH).size / 1024).toFixed(0);
console.log(`✓ ${OUT_PATH} (${outSize} KB · hash ${cacheKey})`);
console.log(`  duration ${outDur?.toFixed(3) ?? "?"}s` +
  (drift !== null ? ` (drift ${drift >= 0 ? "+" : ""}${drift.toFixed(3)}s)` : "") +
  ` · cover ${hasCover ? "embedded" : "MISSING"}` +
  ` · lyrics ${lyricsText ? "yes" : "no"}`);
if (!hasCover) {
  console.error("✗ cover stream not detected in output — verify ffprobe report:");
  console.error(verify.stdout);
  process.exit(1);
}
