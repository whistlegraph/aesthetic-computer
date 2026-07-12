// finger-gesture.mjs — a touch overlay that "plays" the piece in time with the
// music. Driven by the gesture track capture-av.mjs records (down/up paired by
// id; moves = drag path; taps get a short synthetic contact). notepat is
// MULTITOUCH so chords show several touch points; bubble is a DRAG so the point
// follows the pointer path. Each contact scales up + throws an expanding ripple
// (the animation plain clicks lacked).
//
// Glyph styles: "circle" (default) little touch dot · "paw" a little paw print.
//
//   const presses = buildPresses(gestures);
//   drawFingers(ctx, presses, tSeconds, W, H, { tint:[r,g,b], style:"circle" });

const APPROACH = 0.11;   // s a point appears before contact
const LIFT = 0.12;       // s a point lingers after release
const TAP_CONTACT = 0.15; // s a tap "holds"

export function buildPresses(gestures = []) {
  const byId = new Map();
  for (const g of gestures) {
    if (g.kind === "down" || g.kind === "tap") {
      byId.set(g.id, { id: g.id, x: g.x, y: g.y, t0: g.t, t1: g.kind === "tap" ? g.t + TAP_CONTACT : null, path: [{ t: g.t, x: g.x, y: g.y }] });
    } else if (g.kind === "move") {
      const p = byId.get(g.id); if (p) p.path.push({ t: g.t, x: g.x, y: g.y });
    } else if (g.kind === "up") {
      const p = byId.get(g.id); if (p) { if (p.t1 == null) p.t1 = g.t; p.path.push({ t: g.t, x: g.x, y: g.y }); }
    }
  }
  return [...byId.values()].filter((p) => p.t1 != null && p.t1 > p.t0).sort((a, b) => a.t0 - b.t0);
}

const easeInOut = (u) => (u < 0.5 ? 2 * u * u : 1 - Math.pow(-2 * u + 2, 2) / 2);
const clamp01 = (u) => Math.max(0, Math.min(1, u));

// fingertip position at time t — follows the drag path if the press has moves
function pathPos(p, t) {
  const path = p.path;
  if (!path || path.length < 2) return { x: p.x, y: p.y };
  if (t <= path[0].t) return path[0];
  for (let i = 0; i < path.length - 1; i++) {
    if (t <= path[i + 1].t) {
      const a = path[i], b = path[i + 1];
      const u = clamp01((t - a.t) / Math.max(0.001, b.t - a.t));
      return { x: a.x + (b.x - a.x) * u, y: a.y + (b.y - a.y) * u };
    }
  }
  return path[path.length - 1];
}

// a little touch dot: soft glow + solid disc + bright core, scales with press
function drawDot(ctx, x, y, press, tint, alpha = 1) {
  const r = 15 * (0.7 + 0.55 * press);
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  ctx.globalAlpha = (0.3 + 0.45 * press) * alpha;
  const g = ctx.createRadialGradient(x, y, 0, x, y, r * 2.4);
  g.addColorStop(0, `rgba(${tint.hi.join(",")},0.95)`);
  g.addColorStop(1, `rgba(${tint.hi.join(",")},0)`);
  ctx.fillStyle = g;
  ctx.beginPath(); ctx.arc(x, y, r * 2.4, 0, Math.PI * 2); ctx.fill();
  ctx.restore();
  ctx.save();
  ctx.globalAlpha = (0.6 + 0.35 * press) * alpha;
  ctx.fillStyle = `rgb(${tint.mid.join(",")})`;
  ctx.beginPath(); ctx.arc(x, y, r, 0, Math.PI * 2); ctx.fill();
  ctx.lineWidth = 3;
  ctx.strokeStyle = `rgba(255,255,255,${(0.5 + 0.4 * press) * alpha})`;
  ctx.stroke();
  ctx.globalAlpha = (0.55 + 0.45 * press) * alpha;
  ctx.fillStyle = "rgba(255,255,255,0.92)";
  ctx.beginPath(); ctx.arc(x - r * 0.2, y - r * 0.2, r * 0.34, 0, Math.PI * 2); ctx.fill();
  ctx.restore();
}

// a little paw print: a pad + four toe beans, scales with press
function drawPaw(ctx, x, y, press, tint, alpha = 1) {
  const s = 0.9 + 0.25 * press;
  ctx.save();
  ctx.translate(x, y); ctx.scale(s, s);
  ctx.globalAlpha = (0.72 + 0.24 * press) * alpha;
  ctx.fillStyle = `rgb(${tint.mid.join(",")})`;
  ctx.strokeStyle = `rgba(${tint.lo.join(",")},0.6)`;
  ctx.lineWidth = 2;
  const bean = (cx, cy, rx, ry) => { ctx.beginPath(); ctx.ellipse(cx, cy, rx, ry, 0, 0, Math.PI * 2); ctx.fill(); };
  bean(0, 9, 18, 14);                        // pad
  bean(-15, -9, 6.5, 8.5);                   // toes
  bean(-5, -17, 6.5, 9);
  bean(6, -17, 6.5, 9);
  bean(16, -9, 6.5, 8.5);
  // tiny highlight on the pad
  ctx.globalAlpha = (0.4 + 0.4 * press) * alpha;
  ctx.fillStyle = "rgba(255,255,255,0.7)";
  bean(-5, 5, 5, 4);
  ctx.restore();
}

// a soft, blurry, blended ring — screen-blended stroke with a glow halo
function drawRing(ctx, x, y, press, tint, alpha = 1) {
  const r = 20 * (0.85 + 0.5 * press);
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  ctx.shadowColor = `rgba(${tint.hi.join(",")},0.9)`;
  ctx.shadowBlur = 18 + 24 * press;
  ctx.lineWidth = 5 + press * 5;
  ctx.strokeStyle = `rgba(${tint.hi.join(",")},${(0.3 + 0.42 * press) * alpha})`;
  ctx.beginPath(); ctx.arc(x, y, r, 0, Math.PI * 2); ctx.stroke();
  ctx.shadowBlur = 8;
  ctx.lineWidth = 2.5;
  ctx.globalAlpha = (0.38 + 0.4 * press) * alpha;
  ctx.beginPath(); ctx.arc(x, y, r * 0.58, 0, Math.PI * 2); ctx.stroke();
  ctx.restore();
}

// a crisp little cursor pixel — a chunky square with a dark outline (reads on any
// background) + a white core; marks exactly where it clicks and rides the drag
function drawPixel(ctx, x, y, press, tint, alpha = 1) {
  const s = 14 + press * 12;
  const hx = Math.round(x), hy = Math.round(y);
  ctx.save();
  ctx.globalAlpha = (0.85) * alpha;
  ctx.fillStyle = "rgba(0,0,0,0.7)";
  ctx.fillRect(hx - s / 2 - 2, hy - s / 2 - 2, s + 4, s + 4);
  ctx.fillStyle = `rgb(${tint.hi.join(",")})`;
  ctx.fillRect(hx - s / 2, hy - s / 2, s, s);
  ctx.globalAlpha = (0.6 + 0.4 * press) * alpha;
  ctx.fillStyle = "rgba(255,255,255,0.95)";
  ctx.fillRect(hx - s / 4, hy - s / 4, s / 2, s / 2);
  ctx.restore();
}

// AC's real cursor images: precise (crosshair) when up/hovering, active (reticle
// ring) when the pointer is down. Centered on the hotspot (SVG center).
function drawCursor(ctx, x, y, press, imgs, alpha = 1) {
  const down = press > 0.5;
  const img = (down ? imgs.active : imgs.precise) || imgs.precise || imgs.active;
  if (!img) return;
  const s = (imgs.size || 52) * (down ? 1.06 : 1);
  ctx.save();
  ctx.globalAlpha = alpha;
  ctx.imageSmoothingEnabled = false; // keep the cursor crisp/pixelly
  ctx.drawImage(img, Math.round(x - s / 2), Math.round(y - s / 2), s, s);
  ctx.restore();
}

// Draw the cursor at an EXACT point + down-state (per-frame data, no interp).
export function drawCursorPoint(ctx, x, y, down, imgs, alpha = 1) {
  drawCursor(ctx, x, y, down ? 1 : 0, imgs, alpha);
}

function drawGlyph(ctx, x, y, press, style, tint, alpha, imgs) {
  if (style === "cursor" && imgs) drawCursor(ctx, x, y, press, imgs, alpha);
  else if (style === "paw") drawPaw(ctx, x, y, press, tint, alpha);
  else if (style === "dot" || style === "circle") drawDot(ctx, x, y, press, tint, alpha);
  else if (style === "pixel") drawPixel(ctx, x, y, press, tint, alpha);
  else drawRing(ctx, x, y, press, tint, alpha); // default: soft ring
}

function drawRipple(ctx, x, y, age, tint) {
  const dur = 0.5;
  if (age < 0 || age > dur) return;
  const u = age / dur;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  ctx.globalAlpha = 0.5 * (1 - u);
  ctx.lineWidth = 6 * (1 - u) + 1.5;
  ctx.strokeStyle = `rgb(${tint.hi.join(",")})`;
  ctx.beginPath(); ctx.arc(x, y, 14 + u * 88, 0, Math.PI * 2); ctx.stroke();
  ctx.restore();
}

function pressPhase(p, t) {
  if (t < p.t0) return clamp01(1 - (p.t0 - t) / APPROACH);
  if (t > p.t1) return clamp01(1 - (t - p.t1) / LIFT);
  return 0.55 + 0.45 * clamp01((t - p.t0) / 0.06);
}

export function drawFingers(ctx, presses, t, W, H, opts = {}) {
  if (!presses.length) return;
  const style = opts.style || "circle";
  const c = opts.tint || [235, 238, 245];
  const tint = {
    hi: [Math.min(255, c[0] + 30), Math.min(255, c[1] + 30), Math.min(255, c[2] + 30)],
    mid: c,
    lo: [Math.round(c[0] * 0.45), Math.round(c[1] * 0.45), Math.round(c[2] * 0.5)],
  };

  const imgs = opts.cursorImgs;
  // ripples for every recent contact (skip for the real-cursor style)
  if (style !== "cursor") for (const p of presses) drawRipple(ctx, pathPos(p, p.t0).x * W, pathPos(p, p.t0).y * H, t - p.t0, tint);

  // active presses → one glyph each (multitouch / drag)
  const active = presses.filter((p) => t >= p.t0 - APPROACH && t <= p.t1 + LIFT);
  if (active.length) {
    for (const p of active) {
      const pos = pathPos(p, t);
      drawGlyph(ctx, pos.x * W, pos.y * H, pressPhase(p, t), style, tint, 1, imgs);
    }
    return;
  }

  // between notes: a faint lead glyph glides to the next tile
  let prev = presses[0];
  for (const p of presses) { if (p.t0 <= t) prev = p; }
  const next = presses.find((p) => p.t0 > t) || presses[presses.length - 1];
  if (next === prev) return;
  const u = clamp01((t - prev.t1) / Math.max(0.001, next.t0 - prev.t1));
  const e = easeInOut(u);
  const x = (prev.x + (next.x - prev.x) * e) * W;
  const y = (prev.y + (next.y - prev.y) * e - Math.sin(Math.PI * u) * 0.05) * H;
  drawGlyph(ctx, x, y, 0, style, tint, 0.85, imgs);
}
