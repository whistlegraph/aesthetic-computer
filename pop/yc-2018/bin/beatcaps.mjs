// beatcaps.mjs — captions that dance: the sailor-song lyric look on the YC reel.
//   node beatcaps.mjs in.mp4 out.mp4      (reads words-grid.json, grid2.json, deep/song.json)
// Letters light one by one across the word's voiced span (words-voiced.json, so
// a rest after a word isn't read as a held note) and pop as they land; unlit
// letters wait in grey. A held word just stays lit. The line jumps on every kick,
// letters bob in a wave and sway, and a ball hops word to word, resting on a held
// word and on the line's last. It starts plain and builds over the first 30 s,
// and settles in the breakdowns.
import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
const require = createRequire("/Users/jas/aesthetic-computer/package.json");
const { createCanvas, createImageData, registerFont } = require("canvas");

const [inp, out] = process.argv.slice(2);
const W = 1080, H = 1920, FPS = 30;
registerFont("/System/Library/Fonts/Supplemental/Arial Rounded Bold.ttf", { family: "Cap" });
const PX = 62, MAXW = 820, ROW = H * 0.68, LINE_GAP = 22;
const { bpm } = JSON.parse(readFileSync("grid2.json", "utf8"));
const { bars: END } = JSON.parse(readFileSync("deep/song.json", "utf8"));
const BEAT = 60 / bpm, BAR = BEAT * 4;
const words = JSON.parse(readFileSync("words-voiced.json", "utf8")).map((w) => ({ text: w.text, a: w.fromMs / 1000, b: w.toMs / 1000, slot: w.slotMs / 1000 }));

// the music's shape, same as deep.mjs: kicks from bar 2, none in the breakdowns
const quiet = (bar) => (bar >= 16 && bar < 20) || (bar >= 34 && bar < 38);
const kicks = [];
for (let b = 2; b < END; b++) if (!quiet(b)) for (let q = 0; q < 4; q++) kicks.push(b * BAR + q * BEAT);
kicks.push(END * BAR);
// dance: 0 → 1 over the first 30 s, half in the breakdowns (eased in and out)
const smooth = (x) => { x = Math.min(1, Math.max(0, x)); return x * x * (3 - 2 * x); };
function dance(t) {
  const bar = t / BAR, ramp = smooth(t / 30);
  const qd = Math.max(smooth((bar - 15.5) / 0.5) * (1 - smooth((bar - 19.5) / 0.5)), smooth((bar - 33.5) / 0.5) * (1 - smooth((bar - 37.5) / 0.5)));
  return ramp * (1 - 0.55 * qd);
}
function kickEnv(t) { let lo = 0, hi = kicks.length - 1; if (t < kicks[0]) return 0;
  while (lo < hi) { const m = (lo + hi + 1) >> 1; if (kicks[m] <= t) lo = m; else hi = m - 1; }
  return Math.exp(-(t - kicks[lo]) / 0.11); }

// ── lines: break at sentence ends or when the line is full ───────────────
const meas = createCanvas(10, 10).getContext("2d"); meas.font = `${PX}px Cap`;
const space = meas.measureText(" ").width;
const lines = []; let cur = null, x = 0;
for (const w of words) {
  w.w = meas.measureText(w.text).width;
  if (!cur || x + w.w > MAXW) { cur = { words: [] }; lines.push(cur); x = 0; }
  w.x = x; cur.words.push(w); x += w.w + space;
  if (/[.?!]$/.test(w.text)) cur = null;
}
for (const L of lines) {
  const width = L.words.at(-1).x + L.words.at(-1).w; const left = (W - width) / 2;
  for (const w of L.words) { w.x += left;
    w.chars = [...w.text].map((ch, k) => { const cx = meas.measureText(w.text.slice(0, k)).width;
      return { ch, x: w.x + cx, adv: meas.measureText(ch).width, ph: Math.sin(k * 12.9898 + w.a * 78.233) * 6.28, per: 1.6 + ((k * 7 + w.text.length) % 5) * 0.23 }; });
    const n = w.chars.length, span = Math.min(0.85 * (w.b - w.a), Math.max(0.12 * n, 0.6 * (w.b - w.a)));
    w.chars.forEach((c, k) => (c.lit = w.a + span * k / Math.max(1, n))); // the last letter lit by ~85% of the slot
    w.held = w.b - w.a >= 0.95; }
  L.a = L.words[0].a - 0.18; L.b = L.words.at(-1).slot + 0.25;
}
lines.forEach((L, i) => { const n = lines[i + 1]; if (n && L.b > n.a) L.b = n.a; });
const lineAt = (t) => lines.find((L) => t >= L.a && t < L.b);

// ── draw ─────────────────────────────────────────────────────────────────
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
ctx.font = `${PX}px Cap`; ctx.textBaseline = "alphabetic"; ctx.lineJoin = "round";
const rainbow = (t, k, wild) => { const h = ((t * bpm / 60 / 2 + k * 0.11) % 1) * 360; return `hsl(${h}, ${85 * wild + 0}%, ${60 + 30 * (1 - wild)}%)`; };
function drawLine(t, L) {
  const d = dance(t), e = kickEnv(t) * d, beatN = Math.floor(t / BEAT);
  const fadeIn = Math.min(1, (t - L.a) / 0.15), fadeOut = Math.min(1, (L.b - t) / 0.2), al = Math.max(0, Math.min(fadeIn, fadeOut));
  const lineScale = 1 + 0.07 * e;
  ctx.save(); ctx.translate(W / 2, ROW); ctx.scale(lineScale, lineScale); ctx.translate(-W / 2, -ROW);
  for (const w of L.words) {
    const on = t >= w.a, current = on && t < w.b;
    const wild = 0; // held words used to go rainbow and shake; now they just hold
    w.chars.forEach((c, k) => {
      const lit = t >= c.lit, pop = lit ? 1 + 0.18 * Math.max(0, 1 - (t - c.lit) / 0.12) : 1;
      const bob = -12 * e * (0.55 + 0.45 * Math.sin(k * 0.9 + w.x * 0.01 + beatN * 1.7));
      const sway = 4 * d * Math.sin(2 * Math.PI * t / c.per + c.ph);
      const jitY = wild * 9 * Math.sin(t * 31 + c.ph * 5), jitX = wild * 5 * Math.sin(t * 23 + c.ph * 3);
      const rot = (0.035 * d * Math.sin(2 * Math.PI * t / (c.per * 1.3) + c.ph)) * (1 + 2.5 * wild);
      const s = pop * (current ? 1.06 : 1) * (1 + 0.1 * wild);
      const cx = c.x + c.adv / 2 + jitX, cy = ROW + bob + sway + jitY;
      ctx.save(); ctx.globalAlpha = al * (lit ? 1 : 0.55);
      ctx.translate(cx, cy - PX * 0.33); ctx.rotate(rot); ctx.scale(s, s); ctx.translate(-c.adv / 2, PX * 0.33);
      ctx.fillStyle = "rgba(0,0,0,0.55)"; ctx.fillText(c.ch, 5, 6);
      ctx.strokeStyle = "#000"; ctx.lineWidth = 7; ctx.strokeText(c.ch, 0, 0);
      ctx.fillStyle = !lit ? "#b9b9b9" : wild > 0.02 ? rainbow(t, k, Math.min(1, wild * 1.4)) : "#ffffff";
      ctx.fillText(c.ch, 0, 0);
      ctx.restore();
    });
  }
  ctx.restore();
  if (!process.env.NO_BALL) drawBall(t, L, al, d);
}
// the ball: real hops. Each hop is a true parabola (linear time, no easing)
// that lands on a word's first letter exactly as the word is sung. A held
// word or a long gap is filled with in-place hops on the beat, so the ball
// never sits still; it squashes on every landing and stretches in flight.
function hopsFor(L) {
  if (L.hops) return L.hops;
  const ws = L.words, sx = (w) => w.chars[0].x + w.chars[0].adv / 2, hops = [];
  hops.push({ t0: L.a, t1: ws[0].a, x0: sx(ws[0]), x1: sx(ws[0]), drop: true });
  for (let i = 0; i < ws.length; i++) {
    const g = ws[i], n = ws[i + 1], end = n ? n.a : L.b, x = sx(g);
    // a held word or a long gap: the ball sits on the word, then hops late
    const t = n ? Math.max(g.a, end - 0.5) : end;
    if (t > g.a) hops.push({ t0: g.a, t1: t, x0: x, x1: x, rest: true, last: !n });
    if (n) hops.push({ t0: t, t1: end, x0: x, x1: sx(n) });
  }
  return (L.hops = hops);
}
function drawBall(t, L, al, d) {
  const hops = hopsFor(L), top = ROW - PX * 1.05, R = 10.5;
  let h = hops.find((h) => t >= h.t0 && t < h.t1) || hops.at(-1);
  const T = Math.max(0.05, h.t1 - h.t0), f = Math.min(1, Math.max(0, (t - h.t0) / T));
  const height = h.drop || h.rest ? 0 : Math.min(190, (h.small ? 24 : 45) + (h.small ? 100 : 340) * T * T) * (0.4 + 0.6 * d);
  let bx = h.x0 + (h.x1 - h.x0) * f, by = top - 4 * height * f * (1 - f);
  if (h.drop) by = top - (1 - f * f) * 160; // falls in from above, accelerating
  const vy = h.drop ? 2 * f : Math.abs(4 * (1 - 2 * f)) * height / 200; // flight speed, for the stretch
  let sqx = 1 - 0.12 * Math.min(1, vy), sqy = 1 + 0.18 * Math.min(1, vy);
  const since = t - h.t0; if (!h.drop && since < 0.075 && !(h.rest && hops[hops.indexOf(h) - 1]?.rest)) { const k = 1 - since / 0.075; sqx = 1 + 0.42 * k; sqy = 1 - 0.36 * k; }
  if (f > 0.93 && !h.rest) { const k = (f - 0.93) / 0.07; sqx = 1 + 0.3 * k; sqy = 1 - 0.25 * k; } // meets the floor
  by += R * (1 - sqy); // squash sits on the floor, not around its centre
  ctx.save(); ctx.globalAlpha = al * (0.5 + 0.5 * Math.max(0.35, d));
  const shadowK = 1 - Math.min(0.6, (top - by) / 300);
  ctx.beginPath(); ctx.ellipse(bx + 4, top + R + 4, R * 1.1 * shadowK, R * 0.35 * shadowK, 0, 0, Math.PI * 2); ctx.fillStyle = "rgba(0,0,0,0.35)"; ctx.fill();
  ctx.beginPath(); ctx.ellipse(bx, by, R * sqx + 2.5, R * sqy + 2.5, 0, 0, Math.PI * 2); ctx.fillStyle = "#000"; ctx.fill();
  ctx.beginPath(); ctx.ellipse(bx, by, R * sqx, R * sqy, 0, 0, Math.PI * 2);
  ctx.fillStyle = "#ffffff"; ctx.fill();
  ctx.restore();
}

// node-canvas raw is BGRA; ImageData is RGBA — decode rgba in, encode bgra out.
const dec = spawn("ffmpeg", ["-v", "error", "-i", inp, "-f", "rawvideo", "-pix_fmt", "rgba", "-"]);
const enc = spawn("ffmpeg", ["-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-c:v", "libx264", "-preset", "slow", "-crf", "17", "-pix_fmt", "yuv420p", "-profile:v", "high", "-movflags", "+faststart", out],
  { stdio: ["pipe", "inherit", "inherit"] });
const FRAME = W * H * 4; let buf = Buffer.alloc(0), n = 0;
const img = createImageData(new Uint8ClampedArray(FRAME), W, H);
dec.stdout.on("data", (chunk) => {
  buf = Buffer.concat([buf, chunk]);
  while (buf.length >= FRAME) {
    img.data.set(buf.subarray(0, FRAME)); buf = buf.subarray(FRAME);
    ctx.putImageData(img, 0, 0); const t = n / FPS, L = lineAt(t); if (L) drawLine(t, L); n++;
    if (!enc.stdin.write(canvas.toBuffer("raw"))) { dec.stdout.pause(); enc.stdin.once("drain", () => dec.stdout.resume()); }
    if (n % 300 === 0) process.stdout.write(`  ${n}`);
  }
});
dec.on("close", () => enc.stdin.end());
enc.on("close", (c) => console.log(`\nframes ${n} → ${out} (exit ${c}) · ${lines.length} lines`));
