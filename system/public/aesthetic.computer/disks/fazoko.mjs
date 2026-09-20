// fazoko, 26.09.11
// an inchworm, going from twig to twig.

const L = 44; // body length, as arc
const SPAN = { min: 11, max: 38, reach: 54 };
const PACE = 0.62; // seconds per half step
const TICK = 1 / 120;

const twigs = []; // { s0, s1, a, b, dir, thick, shoots }
const worm = { backS: 0, frontS: 12, phase: "pull", t: 1, from: 0, to: 0, bridging: false };
const cam = { x: 0, y: 0, set: false };
let crossed = 0;
let time = 0;
let startled = 0;

const lerp = (a, b, t) => a + (b - a) * t;
const ease = (t) => t * t * (3 - 2 * t);

// 🌿 the world grows rightward, one twig at a time.
function grow() {
  const last = twigs[twigs.length - 1];
  let a, ang, s0;
  if (!last) {
    a = [0, 0];
    ang = 0;
    s0 = 0;
  } else {
    const gap = 11 + Math.random() * 16;
    const away = last.ang + (Math.random() - 0.5) * 0.7;
    a = [last.b[0] + Math.cos(away) * gap, last.b[1] + Math.sin(away) * gap];
    ang = last.ang * 0.5 + (Math.random() - 0.5) * 0.6;
    s0 = last.s1 + gap;
  }
  const len = 58 + Math.random() * 110;
  const dir = [Math.cos(ang), Math.sin(ang)];
  const shoots = [];
  for (let n = 0; n < 1 + Math.floor(Math.random() * 3); n += 1) {
    shoots.push({
      u: 0.15 + Math.random() * 0.7,
      ang: ang + (Math.random() < 0.5 ? -1 : 1) * (0.5 + Math.random() * 0.7),
      len: 6 + Math.random() * 13,
      leaf: Math.random() < 0.7,
    });
  }
  twigs.push({
    s0,
    s1: s0 + len,
    a,
    b: [a[0] + dir[0] * len, a[1] + dir[1] * len],
    dir,
    ang,
    thick: 2 + Math.floor(Math.random() * 2),
    shoots,
  });
}

function ensure(s) {
  while (twigs[twigs.length - 1].s1 < s + 200) grow();
  while (twigs.length > 2 && twigs[1].s0 < worm.backS - 900) twigs.shift();
}

function at(s) {
  ensure(s);
  for (let i = 0; i < twigs.length; i += 1) {
    const t = twigs[i];
    if (s > t.s1) continue;
    if (s >= t.s0) return [t.a[0] + t.dir[0] * (s - t.s0), t.a[1] + t.dir[1] * (s - t.s0)];
    const p = twigs[i - 1] || t;
    const u = t.s0 === p.s1 ? 0 : (s - p.s1) / (t.s0 - p.s1);
    return [lerp(p.b[0], t.a[0], u), lerp(p.b[1], t.a[1], u)];
  }
  const end = twigs[twigs.length - 1];
  return [...end.b];
}

function on(s) {
  ensure(s);
  return twigs.find((t) => s >= t.s0 && s <= t.s1);
}

function ahead(s) {
  ensure(s);
  return twigs.find((t) => t.s0 >= s);
}

function behind(s) {
  ensure(s);
  let lip = null;
  twigs.forEach((t) => {
    if (t.s1 <= s) lip = t.s1;
  });
  return lip;
}

// 🐛 alternate: reach the front out, then draw the back up.
function plan(sound) {
  const w = worm;
  w.t = 0;
  w.bridging = false;
  if (w.phase === "reach") {
    w.phase = "pull";
    w.from = w.backS;
    let to = w.frontS - SPAN.min;
    if (!on(to)) {
      to = on(w.frontS).s0;
      w.bridging = true;
    }
    w.to = Math.max(to, w.from);
  } else {
    w.phase = "reach";
    w.from = w.frontS;
    let to = w.backS + SPAN.max;
    if (!on(to)) {
      const next = ahead(to);
      if (next && next.s0 + 3 - w.backS <= SPAN.reach) {
        to = next.s0 + 3;
        w.bridging = true;
      } else {
        to = behind(to) - 1;
      }
    }
    w.to = Math.max(to, w.from);
  }
  w.dur = PACE * (w.bridging ? 1.7 : 1) * (0.85 + Math.random() * 0.3);
  const t = on(w.phase === "reach" ? w.from : w.frontS);
  sound?.synth?.({
    tone: 180 + (t ? (t.ang + 0.6) * 90 : 0),
    type: "sine",
    beats: 0.06,
    attack: 0.005,
    decay: 0.9,
    volume: 0.11,
  });
}

// 🐛 the two ends, one of them possibly in the air.
function ends() {
  const w = worm;
  const e = ease(Math.min(1, w.t));
  const air = (p0, p1, lift) => {
    const hop = Math.sin(Math.PI * e);
    const sway = w.bridging ? Math.sin(e * Math.PI * 5) * 3.5 * hop : 0;
    return [lerp(p0[0], p1[0], e) + sway, lerp(p0[1], p1[1], e) - lift * hop];
  };
  if (w.phase === "reach") {
    return [at(w.backS), air(at(w.from), at(w.to), w.bridging ? 17 : 9)];
  }
  return [air(at(w.from), at(w.to), w.bridging ? 12 : 6), at(w.frontS)];
}

function body() {
  const [pA, pB] = ends();
  const dx = pB[0] - pA[0];
  const dy = pB[1] - pA[1];
  const d = Math.hypot(dx, dy) || 0.01;
  let nx = -dy / d;
  let ny = dx / d;
  if (ny > 0) {
    nx = -nx;
    ny = -ny;
  }
  const rear = startled > 0 ? 9 * Math.min(1, startled * 3) : 0;
  const h = Math.sqrt(Math.max(0, L * L - d * d)) * 0.52 + 3 + rear;
  const breath = 1 + 0.035 * Math.sin(time * 5);
  const pts = [];
  for (let i = 0; i <= 16; i += 1) {
    const u = i / 16;
    const arc = Math.sin(Math.PI * u) * h * breath;
    pts.push([pA[0] + dx * u + nx * arc, pA[1] + dy * u + ny * arc]);
  }
  return pts;
}

function boot() {
  grow();
  for (let n = 0; n < 4; n += 1) grow();
}

function sim({ sound }) {
  time += TICK;
  if (startled > 0) {
    startled -= TICK;
    return;
  }
  const w = worm;
  w.t += TICK / (w.dur || PACE);
  if (w.t >= 1) {
    if (w.phase === "reach") {
      const was = on(w.frontS);
      w.frontS = w.to;
      if (was && on(w.frontS) !== was) crossed += 1;
    } else w.backS = w.to;
    plan(sound);
  }
}

function paint({ wipe, ink, screen }) {
  const mid = at((worm.backS + worm.frontS) / 2);
  const want = { x: mid[0] - screen.width * 0.38, y: mid[1] - screen.height * 0.58 };
  if (!cam.set) {
    cam.x = want.x;
    cam.y = want.y;
    cam.set = true;
  }
  cam.x = lerp(cam.x, want.x, 0.07);
  cam.y = lerp(cam.y, want.y, 0.04);

  wipe(24, 30, 52);
  for (let y = 0; y < screen.height; y += 1) {
    const u = y / screen.height;
    ink(
      lerp(28, 96, u * u),
      lerp(36, 78, u),
      lerp(62, 74, u),
    ).line(0, y, screen.width, y);
  }

  // three depths of branch, the far ones hazy and slow.
  [
    [0.22, 44, 130, [52, 62, 78]],
    [0.45, 26, 74, [64, 66, 68]],
  ].forEach(([par, dy, dx, c]) => {
    twigs.forEach((t) => {
      const ax = t.a[0] - cam.x * par + dx;
      const ay = t.a[1] - cam.y * par + dy;
      const bx = t.b[0] - cam.x * par + dx;
      const by = t.b[1] - cam.y * par + dy;
      ink(...c).line(ax, ay, bx, by, t.thick);
    });
  });

  twigs.forEach((t) => {
    const ax = t.a[0] - cam.x;
    const ay = t.a[1] - cam.y;
    const bx = t.b[0] - cam.x;
    const by = t.b[1] - cam.y;
    ink(38, 26, 22).line(ax, ay + 2, bx, by + 2, t.thick + 1);
    ink(104, 70, 44).line(ax, ay, bx, by, t.thick + 1);
    ink(148, 108, 68).line(ax, ay - 1, bx, by - 1, 1);
    t.shoots.forEach((s) => {
      const sx = lerp(ax, bx, s.u);
      const sy = lerp(ay, by, s.u);
      const ex = sx + Math.cos(s.ang) * s.len;
      const ey = sy + Math.sin(s.ang) * s.len;
      ink(96, 66, 42).line(sx, sy, ex, ey);
      if (s.leaf) ink(86, 150, 72).circle(ex, ey, 2.5, true);
    });
  });

  const pts = body().map((p) => [p[0] - cam.x, p[1] - cam.y]);
  pts.forEach((p, i) => {
    const u = i / (pts.length - 1);
    const taper = Math.min(1, 0.5 + 2.4 * Math.min(u, 1 - u));
    const r = (2.4 + 2.6 * Math.sin(Math.PI * u)) * taper;
    ink(30, 44, 28).circle(p[0], p[1] + 1, r, true);
    if (i % 2) ink(78, 148, 62).circle(p[0], p[1], r, true);
    else ink(132, 206, 96).circle(p[0], p[1], r, true);
  });

  // head, facing where it's going.
  const head = pts[pts.length - 1];
  const neck = pts[pts.length - 3];
  const hd = Math.hypot(head[0] - neck[0], head[1] - neck[1]) || 1;
  const fx = (head[0] - neck[0]) / hd;
  const fy = (head[1] - neck[1]) / hd;
  ink(146, 214, 104).circle(head[0] + fx, head[1] + fy, 4, true);
  for (const side of [-1, 1]) {
    const ex = head[0] + fx * 2.2 - fy * side * 1.8;
    const ey = head[1] + fy * 2.2 + fx * side * 1.8;
    ink(22, 28, 20).circle(ex, ey, 1.2, true);
    ink(60, 90, 50).line(ex, ey, ex + fx * 5 - fy * side * 3, ey + fy * 5 + fx * side * 3);
  }

  ink(210, 226, 200, 140).write(`twig ${crossed + 1}`, {
    x: 4,
    y: screen.height - 10,
    font: "MatrixChunky8",
  });
}

function act({ event: e, sound }) {
  if (e.is("touch")) {
    startled = 0.7;
    sound?.synth?.({ tone: 640, type: "sine", beats: 0.1, attack: 0.01, decay: 0.9, volume: 0.1 });
  }
}

export { boot, sim, paint, act };
