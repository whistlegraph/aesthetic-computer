#!/usr/bin/env node
// paper-extract-scene.mjs — a "paper-extract" scene: shows a real AC-paper PDF
// page on screen with a yellow highlighter that wipes across the quoted line(s)
// word-by-word, synced to the spoken narration, while the page drifts up. The
// documentary / newscast "here's the cited passage" move. Outputs an opaque
// 1920x1080 clip used as that segment's background by compose-widescreen.mjs.
//
// Usage: node paper-extract-scene.mjs --pdf <path> --quote "<verbatim>" \
//          --out <clip.mp4> --start <segStartSec> --dur <segDurSec> \
//          [--cite "Author, Title (Year), p.N"]
import { createCanvas, loadImage } from "canvas";
import { readFileSync, mkdirSync } from "node:fs";
import { spawn, execSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const W = 1920, H = 1080, FPS = 30;

const argv = process.argv.slice(2);
const arg = (k, d) => { const i = argv.indexOf(k); return i >= 0 ? argv[i + 1] : d; };
const pdf = resolve(REPO, arg("--pdf"));
const quote = arg("--quote");
const OUT = arg("--out");
const segStart = Number(arg("--start"));
const dur = Number(arg("--dur"));
const cite = arg("--cite", "");
const DPI = Number(arg("--dpi", 300));      // zoomed-in close-up on the passage
const SCALE = DPI / 72;
const TMP = `/tmp/paper-extract`; mkdirSync(TMP, { recursive: true });

const norm = (s) => s.replace(/&[^;]+;/g, "").replace(/[^A-Za-z]/g, "").toLowerCase();

// ── 1) locate the quote: page + per-word boxes (pdftotext -bbox) ─────
const bboxHtml = execSync(`pdftotext -bbox "${pdf}" - 2>/dev/null`).toString();
const pages = [...bboxHtml.matchAll(/<page width="([\d.]+)" height="([\d.]+)">([\s\S]*?)<\/page>/g)];
const qWords = quote.split(/\s+/).filter(Boolean);
const qn = qWords.map(norm);
let found = null;
pages.forEach((p, pi) => {
  if (found) return;
  const ws = [...p[3].matchAll(/<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">([\s\S]*?)<\/word>/g)]
    .map((m) => ({ x0: +m[1], y0: +m[2], x1: +m[3], y1: +m[4], t: m[5] }));
  const wn = ws.map((w) => norm(w.t));
  for (let i = 0; i + qn.length <= wn.length; i++) {
    if (qn.every((q, k) => wn[i + k] === q)) {
      found = { page: pi + 1, boxes: ws.slice(i, i + qn.length) };
      break;
    }
  }
});
if (!found) { console.error(`✗ quote not found verbatim in ${pdf}`); process.exit(1); }
console.log(`▸ quote on page ${found.page}, ${found.boxes.length} words`);

// ── 2) render that page to PNG ───────────────────────────────────────
const pageBase = `${TMP}/page`;
execSync(`pdftoppm -png -r ${DPI} -f ${found.page} -l ${found.page} "${pdf}" "${pageBase}" 2>/dev/null`);
const pagePng = execSync(`ls ${pageBase}*.png | head -1`).toString().trim();
const pageImg = await loadImage(pagePng);

// quote boxes in page-pixel space + its centre (the close-up frames on this)
const boxes = found.boxes.map((b) => ({ x: b.x0 * SCALE, y: b.y0 * SCALE, w: (b.x1 - b.x0) * SCALE, h: (b.y1 - b.y0) * SCALE }));
const qTop = Math.min(...boxes.map((b) => b.y)), qBot = Math.max(...boxes.map((b) => b.y + b.h));
const qLeft = Math.min(...boxes.map((b) => b.x)), qRight = Math.max(...boxes.map((b) => b.x + b.w));
const qMid = (qTop + qBot) / 2, qCenterX = (qLeft + qRight) / 2;
// centre on the quote's column, but clamp so the paper always fills the frame
const pageX = Math.round(Math.max(W - pageImg.width, Math.min(0, W / 2 - qCenterX)));

// ── 3) spoken word timings for the wipe (words.json in the segment) ──
const words = JSON.parse(readFileSync(`${REPO}/recap/out/words.json`, "utf8"));
const segWords = words.filter((w) => (w.fromMs / 1000) >= segStart - 0.05 && (w.fromMs / 1000) < segStart + dur)
  .map((w) => ({ from: w.fromMs / 1000, to: w.toMs / 1000 }));
// zip in order with the quote boxes (segment narration IS the quote)
const reveal = boxes.map((b, i) => ({ box: b, w: segWords[i] || segWords[segWords.length - 1] || { from: segStart, to: segStart + dur } }));

// ── 4) render frames ─────────────────────────────────────────────────
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
ctx.imageSmoothingQuality = "high";
const nFrames = Math.max(1, Math.round(dur * FPS));
// LOSSLESS intermediate (qp 0, full chroma) — this clip gets re-encoded again
// by compose, and fine paper text must not be softened by a first lossy pass.
const ff = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-an", "-c:v", "libx264", "-qp", "0", "-pix_fmt", "yuv444p", "-preset", "medium", OUT],
  { stdio: ["pipe", "inherit", "inherit"] });
console.log(`▸ paper-extract · page ${found.page} · ${dur.toFixed(1)}s · ${nFrames} frames -> ${OUT}`);

let frame = 0;
function draw() {
  const t = frame / FPS;            // 0..dur, local to the segment (scroll)
  const tAbs = segStart + t;        // absolute time (matches words.json reveal)
  const u = t / Math.max(0.001, dur);

  // backdrop (soft dark, slight radial warmth)
  ctx.globalCompositeOperation = "source-over";
  const bg = ctx.createRadialGradient(W / 2, H / 2, 100, W / 2, H / 2, 1100);
  bg.addColorStop(0, "#14201a"); bg.addColorStop(1, "#06120c");
  ctx.fillStyle = bg; ctx.fillRect(0, 0, W, H);

  // page: quote sits ~just below centre at start, drifts up across the scene
  const pageY = Math.round(Math.max(H - pageImg.height, Math.min(0, (H * 0.56 - qMid) - 150 * u)));
  ctx.save();
  ctx.shadowColor = "rgba(0,0,0,0.55)"; ctx.shadowBlur = 38; ctx.shadowOffsetY = 10;
  ctx.drawImage(pageImg, pageX, pageY);
  ctx.restore();

  // yellow highlighter wipe (multiply → real marker over the paper)
  ctx.globalCompositeOperation = "multiply";
  for (const { box, w } of reveal) {
    const p = tAbs >= w.to ? 1 : tAbs < w.from ? 0 : (tAbs - w.from) / Math.max(0.001, w.to - w.from);
    if (p <= 0) continue;
    const pad = box.h * 0.18;
    ctx.fillStyle = "rgba(255,224,38,0.55)";
    ctx.fillRect(pageX + box.x - 2, pageY + box.y - pad, (box.w + 4) * p, box.h + pad * 2);
  }
  ctx.globalCompositeOperation = "source-over";

  // citation (plain font is fine — small meta text, not a YWFT headline)
  if (cite) {
    ctx.font = "26px Georgia, serif"; ctx.textBaseline = "alphabetic"; ctx.textAlign = "left";
    ctx.fillStyle = "rgba(0,0,0,0.5)"; ctx.fillText(cite, 70, H - 60 + 2);
    ctx.fillStyle = "rgba(232,255,232,0.92)"; ctx.fillText(cite, 68, H - 62);
  }

  const ok = ff.stdin.write(Buffer.from(canvas.toBuffer("raw")));
  if (++frame >= nFrames) { ff.stdin.end(); return; }
  if (ok) draw(); else ff.stdin.once("drain", draw);
}
ff.on("close", (c) => console.log(c === 0 ? `✓ ${OUT}` : `✗ ffmpeg exit ${c}`));
draw();
