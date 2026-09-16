// fogozo, 26.09.15
// A drifting field of dots. Touch to push them around.

let dots = [];
let push; // where the finger is, while it's down
let t = 0; // the clock everything breathes on

function boot({ screen }) {
  dots = seed(screen, 120);
}

function seed({ width, height }, n) {
  return Array.from({ length: n }, () => ({
    x: Math.random() * width,
    y: Math.random() * height,
    vx: (Math.random() - 0.5) * 0.4,
    vy: (Math.random() - 0.5) * 0.4,
    r: 1 + Math.random() * 3,
    hue: Math.random() * 360,
    phase: Math.random() * Math.PI * 2, // so they don't all pulse together
  }));
}

function sim({ screen: { width, height } }) {
  t += 1 / 60;
  for (const d of dots) {
    // a slow swirl field, so nothing ever sits still
    const a = Math.sin(d.x * 0.012 + t * 0.7) + Math.cos(d.y * 0.012 - t * 0.5);
    d.vx += Math.cos(a * Math.PI) * 0.03;
    d.vy += Math.sin(a * Math.PI) * 0.03;
    d.hue = (d.hue + 0.3) % 360;
    if (push) {
      const dx = d.x - push.x,
        dy = d.y - push.y,
        d2 = dx * dx + dy * dy;
      if (d2 < 8100) {
        const dist = Math.sqrt(d2) || 1,
          f = (1 - dist / 90) * 0.9;
        d.vx += (dx / dist) * f;
        d.vy += (dy / dist) * f;
      }
    }
    d.x += d.vx;
    d.y += d.vy;
    d.vx *= 0.985;
    d.vy *= 0.985;
    if (d.x < 0) d.x += width;
    if (d.x > width) d.x -= width;
    if (d.y < 0) d.y += height;
    if (d.y > height) d.y -= height;
  }
}

const LINK = 58; // how close two dots have to be to hold a thread
const LINK2 = LINK * LINK;

// a LINK-sized bucket grid, so the link pass only asks neighbors, not everyone.
// reused across frames — rebuilding it would churn 100s of arrays a second.
let grid = [],
  cols = 0,
  rows = 0;

// the 4 neighbors below/right of a cell. the other 4 are covered when *they* look back.
const NEXT = [
  [1, 0],
  [-1, 1],
  [0, 1],
  [1, 1],
];

function bucket(width, height) {
  const c = Math.max(1, Math.ceil(width / LINK)),
    r = Math.max(1, Math.ceil(height / LINK));
  if (c !== cols || r !== rows) {
    cols = c;
    rows = r;
    grid = Array.from({ length: cols * rows }, () => []);
  }
  for (const cell of grid) cell.length = 0;
  for (const d of dots) {
    const cx = Math.min(cols - 1, Math.max(0, (d.x / LINK) | 0)),
      cy = Math.min(rows - 1, Math.max(0, (d.y / LINK) | 0));
    grid[cy * cols + cx].push(d);
  }
}

function paint({ wipe, ink, circle, line, screen }) {
  wipe(10, 12, 20);

  // one hsv per dot per frame instead of one per link
  for (const d of dots) {
    const c = hsv(d.hue, 0.6, 1);
    d.lr = c[0];
    d.lg = c[1];
    d.lb = c[2];
  }

  bucket(screen.width, screen.height);

  for (let cy = 0; cy < rows; cy += 1) {
    for (let cx = 0; cx < cols; cx += 1) {
      const here = grid[cy * cols + cx];
      if (here.length === 0) continue;
      for (let i = 0; i < here.length; i += 1) {
        for (let j = i + 1; j < here.length; j += 1)
          thread(here[i], here[j], ink, line);
        for (const [ox, oy] of NEXT) {
          const nx = cx + ox,
            ny = cy + oy;
          if (nx < 0 || nx >= cols || ny >= rows) continue;
          const there = grid[ny * cols + nx];
          for (let j = 0; j < there.length; j += 1)
            thread(here[i], there[j], ink, line);
        }
      }
    }
  }

  for (const d of dots) {
    const speed = Math.min(1, Math.sqrt(d.vx * d.vx + d.vy * d.vy) * 1.6);
    const beat = 0.7 + 0.5 * Math.sin(t * 2.2 + d.phase);
    const c = hsv(d.hue, 0.5 + speed * 0.5, 0.45 + speed * 0.55);
    ink(c[0], c[1], c[2]);
    circle(d.x, d.y, Math.max(1, d.r * beat + speed * 2), true);
  }
}

function thread(a, b, ink, line) {
  const dx = a.x - b.x,
    dy = a.y - b.y,
    d2 = dx * dx + dy * dy;
  if (d2 > LINK2) return;
  const near = 1 - Math.sqrt(d2) / LINK, // fades out as they drift apart
    k = 0.5 + near * 0.5;
  ink((a.lr * k) | 0, (a.lg * k) | 0, (a.lb * k) | 0, (20 + near * 140) | 0);
  line(a.x, a.y, b.x, b.y);
}

const rgb = [0, 0, 0]; // hsv's scratch pad — read it before the next call

// tiny hsv→rgb so the dots keep their own tint but brighten when they move.
// writes into `rgb` rather than returning a fresh array; it runs in a hot loop.
function hsv(h, s, v) {
  const c = v * s,
    x = c * (1 - Math.abs(((h / 60) % 2) - 1)),
    m = v - c,
    i = (h / 60) | 0;
  let r = 0,
    g = 0,
    b = 0;
  if (i === 0) (r = c), (g = x);
  else if (i === 1) (r = x), (g = c);
  else if (i === 2) (g = c), (b = x);
  else if (i === 3) (g = x), (b = c);
  else if (i === 4) (r = x), (b = c);
  else (r = c), (b = x);
  rgb[0] = ((r + m) * 255) | 0;
  rgb[1] = ((g + m) * 255) | 0;
  rgb[2] = ((b + m) * 255) | 0;
  return rgb;
}

function act({ event: e, screen }) {
  if (e.is("touch") || e.is("draw")) push = { x: e.x, y: e.y };
  if (e.is("lift")) push = undefined;
  if (e.is("reframed")) dots = seed(screen, dots.length);
}

export { boot, sim, paint, act };
