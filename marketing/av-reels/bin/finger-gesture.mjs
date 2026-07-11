// finger-gesture.mjs — an abstracted finger overlay that "plays" the piece in
// time with the music. Driven by the gesture track capture-av.mjs records
// (down/up paired by id; taps get a short synthetic contact). notepat is
// MULTITOUCH, so chords show several fingers fanning from a hand; between notes
// a lead finger travels to the next tile with an easing hop. Each contact makes
// a press dip + an expanding ripple (the animation plain clicks lacked).
//
//   const presses = buildPresses(gestures);
//   drawFingers(ctx, presses, tSeconds, W, H, { tint:[r,g,b] });

const APPROACH = 0.13;  // s a finger appears before contact
const LIFT = 0.13;      // s a finger lingers after release
const TAP_CONTACT = 0.15; // s a tap "holds" for the finger

export function buildPresses(gestures = []) {
  const byId = new Map();
  for (const g of gestures) {
    if (g.kind === "down" || g.kind === "tap") {
      byId.set(g.id, { id: g.id, x: g.x, y: g.y, t0: g.t, t1: g.kind === "tap" ? g.t + TAP_CONTACT : null });
    } else if (g.kind === "up") {
      const p = byId.get(g.id); if (p && p.t1 == null) p.t1 = g.t;
    }
  }
  return [...byId.values()].filter((p) => p.t1 != null && p.t1 > p.t0).sort((a, b) => a.t0 - b.t0);
}

const easeInOut = (u) => (u < 0.5 ? 2 * u * u : 1 - Math.pow(-2 * u + 2, 2) / 2);
const clamp01 = (u) => Math.max(0, Math.min(1, u));

// One abstracted finger: a tapered capsule from an off-frame palm up to the
// fingertip, with a nail highlight, a contact shadow, and (on press) a ripple.
function drawOneFinger(ctx, palm, tip, press, W, H, tint) {
  const ax = tip.x - palm.x, ay = tip.y - palm.y;
  const len = Math.hypot(ax, ay) || 1;
  const ux = ax / len, uy = ay / len;        // axis palm→tip
  const px = -uy, py = ux;                    // perpendicular
  const dip = press * 6;                      // press dip (px toward tile)
  const tx = tip.x + ux * dip, ty = tip.y + uy * dip;
  const wBase = 82, wTip = 44 - press * 4;

  // contact shadow under the fingertip
  ctx.save();
  ctx.globalCompositeOperation = "multiply";
  ctx.globalAlpha = 0.22 + 0.18 * press;
  ctx.fillStyle = "rgba(0,0,0,1)";
  ctx.beginPath();
  ctx.ellipse(tx, ty + 10, wTip * 0.7, wTip * 0.4, 0, 0, Math.PI * 2);
  ctx.fill();
  ctx.restore();

  // finger body
  const bl = { x: palm.x + px * wBase / 2, y: palm.y + py * wBase / 2 };
  const br = { x: palm.x - px * wBase / 2, y: palm.y - py * wBase / 2 };
  const tl = { x: tx + px * wTip / 2, y: ty + py * wTip / 2 };
  const tr = { x: tx - px * wTip / 2, y: ty - py * wTip / 2 };
  ctx.save();
  const grad = ctx.createLinearGradient(palm.x, palm.y, tx, ty);
  grad.addColorStop(0, `rgba(${tint.mid.join(",")},0.72)`);
  grad.addColorStop(1, `rgba(${tint.hi.join(",")},0.9)`);
  ctx.fillStyle = grad;
  ctx.beginPath();
  ctx.moveTo(bl.x, bl.y);
  ctx.lineTo(tl.x, tl.y);
  ctx.arc(tx, ty, wTip / 2, Math.atan2(tl.y - ty, tl.x - tx), Math.atan2(tr.y - ty, tr.x - tx), false);
  ctx.lineTo(br.x, br.y);
  ctx.closePath();
  ctx.fill();
  ctx.lineWidth = 2.5;
  ctx.strokeStyle = `rgba(${tint.lo.join(",")},0.55)`;
  ctx.stroke();
  // nail / tip highlight
  ctx.globalAlpha = 0.5 + 0.4 * press;
  ctx.fillStyle = `rgba(255,255,255,0.85)`;
  ctx.beginPath();
  ctx.ellipse(tx - ux * 10, ty - uy * 10, wTip * 0.32, wTip * 0.22, Math.atan2(uy, ux), 0, Math.PI * 2);
  ctx.fill();
  ctx.restore();
}

// ripple ring(s) radiating from a contact
function drawRipple(ctx, x, y, age, tint) {
  const dur = 0.5;
  if (age < 0 || age > dur) return;
  const u = age / dur;
  const r = 14 + u * 88;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  ctx.globalAlpha = 0.55 * (1 - u);
  ctx.lineWidth = 6 * (1 - u) + 1.5;
  ctx.strokeStyle = `rgb(${tint.hi.join(",")})`;
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI * 2);
  ctx.stroke();
  ctx.restore();
}

// pressed-ness of a press at time t: rises fast on contact, holds, releases
function pressPhase(p, t) {
  if (t < p.t0) return clamp01(1 - (p.t0 - t) / APPROACH);       // approaching
  if (t > p.t1) return clamp01(1 - (t - p.t1) / LIFT);           // lifting
  const inDip = clamp01((t - p.t0) / 0.06);                       // quick contact
  return 0.55 + 0.45 * inDip;
}

export function drawFingers(ctx, presses, t, W, H, opts = {}) {
  if (!presses.length) return;
  const c = opts.tint || [210, 205, 200];
  const tint = {
    hi: [Math.min(255, c[0] + 40), Math.min(255, c[1] + 40), Math.min(255, c[2] + 40)],
    mid: c,
    lo: [Math.round(c[0] * 0.5), Math.round(c[1] * 0.5), Math.round(c[2] * 0.55)],
  };
  const px = (p) => ({ x: p.x * W, y: p.y * H });

  // ripples for every recent contact (drawn under fingers)
  for (const p of presses) drawRipple(ctx, p.x * W, p.y * H, t - p.t0, tint);

  // active presses (with approach/lift halo) → one finger each (multitouch)
  const active = presses.filter((p) => t >= p.t0 - APPROACH && t <= p.t1 + LIFT);
  if (active.length) {
    // cluster by hand side so wide chords look like two hands
    const sides = { L: [], R: [] };
    for (const p of active) (p.x < 0.5 ? sides.L : sides.R).push(p);
    for (const key of ["L", "R"]) {
      const grp = sides[key]; if (!grp.length) continue;
      const avgX = grp.reduce((s, p) => s + p.x, 0) / grp.length;
      const palm = { x: avgX * W + (key === "L" ? -40 : 40), y: H * 1.18 };
      for (const p of grp) {
        const tip = px(p);
        drawOneFinger(ctx, palm, tip, pressPhase(p, t), W, H, tint);
      }
    }
    return;
  }

  // between notes: a single lead finger travels to the next tile with a hop
  let prev = presses[0], next = presses[0];
  for (const p of presses) { if (p.t0 <= t) prev = p; }
  next = presses.find((p) => p.t0 > t) || presses[presses.length - 1];
  if (next === prev) return;
  const u = clamp01((t - prev.t1) / Math.max(0.001, next.t0 - prev.t1));
  const e = easeInOut(u);
  const tip = { x: (prev.x + (next.x - prev.x) * e) * W, y: (prev.y + (next.y - prev.y) * e - Math.sin(Math.PI * u) * 0.05) * H };
  const palm = { x: tip.x + (tip.x < W / 2 ? -40 : 40), y: H * 1.18 };
  drawOneFinger(ctx, palm, tip, 0, W, H, tint);
}
