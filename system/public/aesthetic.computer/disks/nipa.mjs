// nipa, 26.09.15
// A smiley that watches the pen and blinks.

let t = 0;
let blink = 0; // frames of squint remaining

function sim() {
  t += 1;
  if (blink > 0) blink -= 1;
  else if (Math.random() < 0.005) blink = 9;
}

function paint({ wipe, ink, circle, line, box, pen, screen: { width: w, height: h } }) {
  wipe(148, 208, 240);

  const r = Math.min(w, h) * 0.34;
  const cx = w / 2;
  const cy = h / 2 + r * 0.06;

  // where the eyes look — toward the pen, capped inside the socket.
  const tx = pen?.x ?? cx + Math.cos(t / 90) * w;
  const ty = pen?.y ?? cy + Math.sin(t / 70) * h;
  const dx = tx - cx;
  const dy = ty - cy;
  const d = Math.hypot(dx, dy) || 1;
  const reach = Math.min(d, r * 0.5) / d;
  const gx = dx * reach * 0.22;
  const gy = dy * reach * 0.22;

  ink(255, 226, 96);
  circle(cx, cy, r, true);
  ink(190, 130, 30);
  circle(cx, cy, r, false, 2);

  const ex = r * 0.38;
  const ey = r * 0.3;
  const er = r * 0.17;

  for (const s of [-1, 1]) {
    const x = cx + s * ex + gx;
    const y = cy - ey + gy;
    if (blink > 0) {
      ink(40, 26, 10);
      box(x - er, y - er * 0.2, er * 2, Math.max(2, er * 0.35));
    } else {
      ink(255, 250, 240);
      circle(cx + s * ex, cy - ey, er, true);
      ink(30, 20, 40);
      circle(x, y, er * 0.52, true);
    }
  }

  // the smile, swept across the lower arc.
  const mr = r * 0.62;
  const thick = Math.max(2, r * 0.09);
  ink(120, 40, 50);
  let px, py;
  for (let i = 0; i <= 24; i += 1) {
    const a = Math.PI * (0.18 + (i / 24) * 0.64);
    const x = cx + Math.cos(a) * mr * -1;
    const y = cy + Math.sin(a) * mr * 0.78 + r * 0.06;
    if (i > 0) for (let o = 0; o < thick; o += 1) line(px, py + o, x, y + o);
    px = x;
    py = y;
  }
}

export { sim, paint };
