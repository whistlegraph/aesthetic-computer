// kokazo, 26.09.11
// A smiley with a body. It dances, and it runs under falling food to catch it.
// Tap the background to drop a snack. Tap the guy to poke him.

const { min, max, abs, sin, cos, atan2, hypot, PI } = Math;

let blink = 0, wait = 80, t = 0;
let grin = 0, hover = 0, poke = 0, chew = 0, mood = 1, lean = { x: 0, y: 0 };
let food = [], crumbs = [], dropIn = 120, caught = 0, missed = 0, chase = 0;
const head = { cx: 0, cy: 0, r: 1 }; // last painted geometry. (act hit-tests it)
const torso = { cx: 0, cy: 0, rx: 1, ry: 1 };
const stage = { s: 10, w: 100, groundY: 100 }; // last painted layout. (sim reads it)

const SNACKS = [
  { k: "berry", c: [240, 70, 90] },
  { k: "donut", c: [250, 180, 70] },
  { k: "pea", c: [140, 220, 110] },
  { k: "berry", c: [200, 110, 240] },
];

function hit(x, y) {
  return (
    hypot(x - head.cx, y - head.cy) < head.r ||
    (abs(x - torso.cx) < torso.rx && abs(y - torso.cy) < torso.ry)
  );
}

function lowest() {
  return food.reduce((a, f) => (!a || f.y > a.y ? f : a), null);
}

function drop(x) {
  const s = SNACKS[(Math.random() * SNACKS.length) | 0];
  food.push({ ...s, x, y: -stage.s * 0.5, vy: 0, spin: Math.random() * PI });
}

function burst(f, n) {
  for (let i = 0; i < n; i += 1) {
    crumbs.push({
      x: f.x, y: f.y, c: f.c,
      vx: (Math.random() - 0.5) * stage.s * 0.3,
      vy: -Math.random() * stage.s * 0.14,
      life: 18 + Math.random() * 16,
    });
  }
}

function sim({ pen, sound }) {
  t += 0.07 + grin * 0.09 + poke * 0.22; // the dance picks up when provoked.
  if (blink > 0) blink -= 1;
  else if (--wait <= 0) { blink = 7; wait = 60 + Math.random() * 180; }

  poke *= 0.88;
  chew = max(0, chew - 1);
  mood = ease(mood, 1, 0.03); // a miss stings, then passes.
  grin = ease(grin, pen?.drawing && poke < 0.05 ? 1 : 0, 0.12);
  hover = ease(hover, pen && hit(pen.x, pen.y) ? 1 : 0, 0.2);

  const pull = pen?.drawing ? 0.16 : 0;
  lean.x = ease(lean.x, pen ? clamp((pen.x - head.cx) / head.r, -1, 1) * head.r * pull : 0, 0.14);
  lean.y = ease(lean.y, pen ? clamp((pen.y - head.cy) / head.r, -1, 1) * head.r * pull : 0, 0.14);

  if (--dropIn <= 0) { // snacks keep coming whether or not you feed him.
    drop(stage.s + Math.random() * max(1, stage.w - stage.s * 2));
    dropIn = 150 + Math.random() * 170;
  }

  const g = stage.s * 0.013;
  // Terminal speed stays under the catch radius, or a fast snack tunnels past his hands.
  food.forEach((f) => { f.vy = min(f.vy + g, stage.s * 0.4); f.y += f.vy; f.spin += 0.08; });
  food = food.filter((f) => {
    if (f.y < stage.groundY) return true;
    missed += 1;
    mood = -1;
    burst(f, 9);
    sound?.synth?.({ type: "square", tone: 90, duration: 0.12, attack: 0.01, decay: 0.9, volume: 0.25 });
    return false;
  });

  crumbs.forEach((c) => { c.vy += g; c.x += c.vx; c.y += c.vy; c.life -= 1; });
  crumbs = crumbs.filter((c) => c.life > 0);

  // He scoots under whatever is closest to landing.
  const mark = lowest();
  const room = stage.w / 2 - stage.s * 1.1; // far enough to stand under an edge drop.
  chase = ease(chase, mark ? clamp(mark.x - stage.w / 2, -room, room) : 0, 0.07);
}

function paint({ wipe, ink, write, screen, pen, sound }) {
  const { width: w, height: h } = screen;
  const s = min(w / 7, h / 16); // zoomed out, but a tall phone shouldn't be all sky.
  const beat = sin(t), swing = sin(t / 2);
  const lift = abs(beat);

  const x = w / 2 + chase + swing * s * 0.35 + lean.x;
  const groundY = h - s * 1.6; // he stands near the bottom, sky above for the food.
  const top = max(26, groundY - s * 7.6) - lift * s * 0.3 + lean.y;
  const shoulderY = top + s * 2.95;
  const hipY = shoulderY + s * 2;

  stage.s = s; stage.w = w; stage.groundY = groundY;
  head.r = s * 1.5 * (1 + poke * 0.07);
  head.cx = x + swing * s * 0.12;
  head.cy = top + s * 1.5 + sin(t * 2) * s * 0.06;
  torso.cx = x + swing * s * 0.06;
  torso.cy = (shoulderY + hipY) / 2;
  torso.rx = s * 0.85;
  torso.ry = (hipY - shoulderY) / 2 + s * 0.35;

  wipe(40 - hover * 10, 60 + hover * 10, 110 + hover * 30);

  ink(26, 38, 82).box(0, groundY + s * 0.25, w, h - groundY); // the floor he dances on.
  ink(16, 24, 60, 170).oval(x, groundY + s * 0.25, s * (1.2 - lift * 0.25), s * 0.22, true);

  const skin = [255, 210 + poke * 30, 60];
  const limb = [190 + hover * 65, 130 + hover * 60, 20];

  crumbs.forEach((c) => ink(c.c[0], c.c[1], c.c[2], c.life * 8).circle(c.x, c.y, s * 0.09, true));
  food.forEach((f) => snack(ink, f, s));

  // Legs kick opposite the arms, one splaying as the other tucks.
  [-1, 1].forEach((side) => {
    const step = sin(t + (side < 0 ? PI : 0));
    const a1 = PI / 2 - side * (0.3 + 0.3 * step);
    const a2 = a1 + side * 0.22 * max(0, step);
    const foot = hose(ink, limb, x + side * s * 0.42, hipY + s * 0.2, a1, s * 1.2, a2, s * 1.1, s * 0.42);
    ink(...limb).oval(foot[0], foot[1] + s * 0.1, s * 0.3, s * 0.16, true);
  });

  ink(...skin).oval(torso.cx, torso.cy, torso.rx, torso.ry, true);
  ink(...limb).oval(torso.cx, torso.cy, torso.rx, torso.ry);

  // Arms throw up on the beat — unless there's food, which they reach for.
  const mark = lowest();
  [-1, 1].forEach((side) => {
    const sw = sin(t + (side < 0 ? 0 : PI));
    const sx = torso.cx + side * s * 0.8, sy = shoulderY + s * 0.2;
    let a1 = PI / 2 - side * (0.9 + 0.95 * sw);
    let a2 = a1 - side * (0.1 + 0.6 * max(0, sw));
    if (mark) {
      const aim = atan2(mark.y - sy, mark.x - sx);
      const reach = clamp(1 - (hypot(mark.x - sx, mark.y - sy) - s * 2) / (s * 2), 0, 1);
      a1 = a1 + (aim - a1) * reach;
      a2 = a2 + (aim - a2) * reach;
    }
    const hand = hose(ink, limb, sx, sy, a1, s * 1.15, a2, s * 1.05, s * 0.38);
    ink(...skin).circle(hand[0], hand[1], s * 0.26, true); // mitts, so a lowered arm still reads.

    food = food.filter((f) => {
      if (hypot(f.x - hand[0], f.y - hand[1]) > s * 0.6) return true;
      caught += 1;
      chew = 28;
      mood = 1;
      burst(f, 5);
      sound?.synth?.({ type: "sine", tone: 640 + caught * 6, duration: 0.09, attack: 0.01, decay: 0.96, volume: 0.4 });
      return false;
    });
  });

  const { cx, cy } = head;
  const R = head.r;
  ink(...skin).circle(cx, cy, R, true);
  ink(...limb).circle(cx, cy, R);

  if (hover > 0.05) { // blush that fades in under the cursor.
    [-1, 1].forEach((side) => {
      ink(255, 120, 130, hover * 90).circle(cx + side * R * 0.58, cy + R * 0.12, R * 0.14, true);
    });
  }

  // The eyes watch the food if there is any, otherwise the pen.
  const watch = mark || pen;
  const eyeR = R * (0.16 + poke * 0.06);
  const lookX = watch ? clamp((watch.x - cx) / R, -1, 1) : 0;
  const lookY = watch ? clamp((watch.y - cy) / R, -1, 1) : 0;

  [-1, 1].forEach((side) => {
    const ex = cx + side * R * 0.36, ey = cy - R * 0.22;
    if (blink > 0 && poke < 0.2) {
      ink(70, 30, 20).line(ex - eyeR, ey, ex + eyeR, ey);
    } else {
      ink(255, 245, 220).circle(ex, ey, eyeR, true);
      const pupil = eyeR * (poke > 0.2 ? 0.34 : 0.5);
      ink(30, 20, 10).circle(ex + lookX * eyeR * 0.5, ey + lookY * eyeR * 0.5, pupil, true);
    }
  });

  const my = cy + R * 0.08;
  if (chew > 0) { // chomping, jaw working.
    ink(70, 30, 20).oval(cx, my + R * 0.16, R * 0.26, R * (0.1 + 0.18 * abs(sin(chew * 0.6))), true);
  } else if (poke > 0.08) { // a startled O, wider the harder it was poked.
    ink(70, 30, 20).circle(cx, my + R * 0.18, R * (0.1 + poke * 0.22), true);
  } else {
    const mr = R * (0.5 + grin * 0.1); // the smile flips to a frown on a miss.
    const spread = PI * (0.22 + grin * 0.14);
    let px, py;
    for (let i = 0; i <= 24; i += 1) {
      const a = PI / 2 - spread + (spread * 2 * i) / 24;
      const mx = cx + cos(a) * mr, myy = my + sin(a) * mr * mood;
      if (i > 0) ink(70, 30, 20).line(px, py, mx, myy);
      px = mx; py = myy;
    }
  }

  ink(255, 230, 120).write(`caught ${caught}`, { x: 6, y: h - 22 });
  ink(150, 160, 200).write(`missed ${missed}`, { x: 6, y: h - 12 });
}

function act({ event: e, sound }) {
  if (e.is("touch")) {
    if (hit(e.x, e.y)) {
      poke = 1;
      grin = 0;
      sound?.synth?.({ type: "sine", tone: 520, duration: 0.06, attack: 0.01, decay: 0.96, volume: 0.5 });
    } else {
      drop(e.x); // anywhere else is a snack.
    }
  }
  if (e.is("lift") && poke > 0.1) { // he springs back with a little chirp.
    sound?.synth?.({ type: "sine", tone: 760, duration: 0.08, attack: 0.01, decay: 0.96, volume: 0.4 });
  }
}

function snack(ink, f, s) {
  const r = s * 0.34;
  if (f.k === "donut") {
    ink(...f.c).circle(f.x, f.y, r, true);
    ink(50, 40, 60).circle(f.x, f.y, r * 0.36, true);
  } else if (f.k === "pea") {
    ink(...f.c).oval(f.x, f.y, r, r * 0.68, true);
  } else {
    ink(...f.c).circle(f.x, f.y, r, true);
    ink(110, 200, 110).line(f.x, f.y - r, f.x + cos(f.spin) * r, f.y - r * 1.7);
  }
}

// A rubber-hose limb: two segments stamped as overlapping dots, so the width
// stays even and the joints round themselves. (pline tapers at the anchor)
function hose(ink, color, x, y, a1, l1, a2, l2, thick) {
  const jx = x + cos(a1) * l1, jy = y + sin(a1) * l1;
  const ex = jx + cos(a2) * l2, ey = jy + sin(a2) * l2;
  const r = thick / 2;
  stamp(ink, color, x, y, jx, jy, r);
  stamp(ink, color, jx, jy, ex, ey, r);
  return [ex, ey];
}

function stamp(ink, color, x0, y0, x1, y1, r) {
  const n = max(2, Math.round(hypot(x1 - x0, y1 - y0) / (r * 0.6)));
  for (let i = 0; i <= n; i += 1) {
    ink(...color).circle(x0 + ((x1 - x0) * i) / n, y0 + ((y1 - y0) * i) / n, r, true);
  }
}

function ease(n, to, k) {
  return n + (to - n) * k;
}

function clamp(n, lo, hi) {
  return n < lo ? lo : n > hi ? hi : n;
}

export { sim, paint, act };
