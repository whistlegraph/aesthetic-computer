// kana, 26.09.13

/* #region 📝 notes
  A bumblebee. It wanders; hold anywhere and it comes to your finger.
#endregion */

const { cos, sin, sqrt, min, max, hypot, PI } = Math;

const bee = { x: 0, y: 0, vx: 0, vy: 0, a: 0, flap: 0 };
let flowers = [];
let stars = [];
let pollen = []; // motes the bee knocks loose over a flower.
let target = null; // where it's headed. null until boot sizes the screen.
let called = false; // a finger is down, so the bee chases it.
let wait = 0; // frames until it's bored of this flower.
let buzz; // the held wingbeat voice, alive only while a finger is down.
let tick = 0;

const petalTints = [
  [235, 120, 180],
  [180, 150, 255],
  [250, 240, 240],
  [255, 160, 90],
];

// Bee-space -> screen. px is along the body (nose at +x), py across it.
function at(px, py, a = bee.a, x = bee.x, y = bee.y) {
  return [x + px * cos(a) - py * sin(a), y + px * sin(a) + py * cos(a)];
}

function ellipse(cx, cy, rx, ry, spin, steps = 14) {
  const pts = [];
  for (let i = 0; i < steps; i += 1) {
    const t = (i / steps) * PI * 2;
    const ex = rx * cos(t),
      ey = ry * sin(t);
    pts.push(at(cx + ex * cos(spin) - ey * sin(spin), cy + ex * sin(spin) + ey * cos(spin)));
  }
  return pts;
}

// Flowers on their own stems, rooted low, none under the corner label.
function plant({ width, height }) {
  const count = max(3, min(6, Math.round(width / 90)));
  flowers = [];
  for (let i = 0; i < count; i += 1) {
    const x = ((i + 0.5) / count) * width + (Math.random() - 0.5) * (width / count) * 0.6;
    flowers.push({
      x: max(14, min(width - 14, x)),
      y: max(34, height - 40 - Math.random() * max(8, height * 0.26)),
      r: 7 + Math.random() * 6,
      tint: petalTints[(Math.random() * petalTints.length) | 0],
      sway: Math.random() * PI * 2,
    });
  }

  stars = [];
  for (let i = 0; i < 40; i += 1) {
    stars.push({
      x: Math.random() * width,
      y: Math.random() * height * 0.55,
      t: Math.random() * PI * 2,
    });
  }
}

function visit() {
  const f = flowers[(Math.random() * flowers.length) | 0];
  target = f ? { x: f.x, y: f.y - f.r - 16 } : { x: bee.x, y: bee.y };
  wait = 70 + Math.random() * 140;
}

function boot({ screen }) {
  bee.x = screen.width / 2;
  bee.y = screen.height / 3;
  plant(screen);
  visit();
}

function sim({ pen }) {
  tick += 1;
  if (!target) visit();
  if (called && pen) target = { x: pen.x, y: pen.y };
  else if (--wait < 0) visit();

  const dx = target.x - bee.x,
    dy = target.y - bee.y,
    d = hypot(dx, dy) || 1;

  const pull = called ? 0.22 : 0.07;
  bee.vx += (dx / d) * pull;
  bee.vy += (dy / d) * pull;
  bee.vx *= 0.94;
  bee.vy *= 0.94;

  bee.x += bee.vx;
  bee.y += bee.vy;

  // A bee never holds still, even over a flower.
  bee.vx += sin(bee.flap * 0.043) * 0.08;
  bee.vy += cos(bee.flap * 0.061) * 0.08;

  const speed = hypot(bee.vx, bee.vy);
  if (speed > 0.2) bee.a += ((Math.atan2(bee.vy, bee.vx) - bee.a + PI * 3) % (PI * 2) - PI) * 0.2;
  bee.flap += 0.9 + speed * 0.12;

  // Wingbeat rises with effort — but not every frame, that floods the voice.
  if (buzz && tick % 6 === 0) buzz.update({ tone: 92 + speed * 16, duration: 0.1 });

  for (const f of flowers) f.sway += 0.012;
  for (const s of stars) s.t += 0.03;

  // Hovering close to a flower shakes pollen off it.
  const near = flowers.find((f) => hypot(f.x - bee.x, f.y - bee.y) < f.r + 14);
  if (near && pollen.length < 60 && tick % 4 === 0) {
    pollen.push({
      x: near.x + (Math.random() - 0.5) * near.r * 2,
      y: near.y + (Math.random() - 0.5) * near.r,
      vy: -0.15 - Math.random() * 0.25,
      life: 1,
    });
  }

  for (const p of pollen) {
    p.x += sin(p.life * 6) * 0.3;
    p.y += p.vy;
    p.life -= 0.008;
  }
  pollen = pollen.filter((p) => p.life > 0);
}

function paint({ wipe, ink, screen, paintCount }) {
  wipe(24, 30, 40);
  ink("fade:midnightblue-rebeccapurple-indianred:vertical").box(
    0,
    0,
    screen.width,
    screen.height,
  );

  for (const s of stars) {
    const twinkle = 60 + sin(s.t) * 50;
    ink(255, 245, 220, max(0, twinkle)).point(s.x, s.y);
  }

  const horizon = screen.height - 12;
  ink(34, 24, 34).box(0, horizon, screen.width, screen.height - horizon);

  for (const f of flowers) {
    const lean = sin(f.sway) * 4;
    ink(60, 120, 80).line(f.x, screen.height, f.x + lean, f.y);
    ink(70, 140, 90).line(f.x + lean * 0.5, f.y + f.r * 2.2, f.x + lean - 9, f.y + f.r * 1.6);
    for (let p = 0; p < 6; p += 1) {
      const a = (p / 6) * PI * 2 + f.sway * 0.3;
      ink(f.tint).circle(f.x + lean + cos(a) * f.r, f.y + sin(a) * f.r, f.r * 0.62, true);
    }
    ink(250, 205, 70).circle(f.x + lean, f.y, f.r * 0.5, true);
  }

  for (const p of pollen) ink(255, 225, 120, p.life * 200).point(p.x, p.y);

  const s = max(7, min(screen.width, screen.height) * 0.07); // half a body.
  const spread = 0.55 + sin(bee.flap) * 0.45;

  // Wings first, behind the fuzz.
  for (const side of [-1, 1]) {
    ink(225, 240, 255, 70).shape({ points: wing(s, side, spread), filled: true });
  }

  // Legs.
  for (let i = 0; i < 3; i += 1) {
    const lx = (0.3 - i * 0.45) * s;
    for (const side of [-1, 1]) {
      const swing = sin(bee.flap * 0.35 + i) * 0.12 * s;
      ink(30, 24, 18).line(
        ...at(lx, side * 0.45 * s),
        ...at(lx - 0.3 * s + swing, side * 0.95 * s),
      );
    }
  }

  // Striped abdomen: slices of one ellipse, dark and gold in turn.
  const slices = 7;
  for (let i = 0; i < slices; i += 1) {
    const t0 = -1 + (i / slices) * 1.9,
      t1 = -1 + ((i + 1) / slices) * 1.9;
    const h0 = 0.62 * s * sqrt(max(0, 1 - t0 * t0)),
      h1 = 0.62 * s * sqrt(max(0, 1 - t1 * t1));
    ink(i % 2 ? [250, 202, 48] : [28, 23, 20]).shape({
      points: [
        at(t0 * s, -h0),
        at(t1 * s, -h1),
        at(t1 * s, h1),
        at(t0 * s, h0),
      ],
      filled: true,
    });
  }

  // Stinger.
  ink(20, 16, 14).shape({
    points: [at(-0.98 * s, -0.14 * s), at(-1.4 * s, 0), at(-0.98 * s, 0.14 * s)],
    filled: true,
  });

  // Head and antennae.
  ink(26, 21, 18).shape({
    points: ellipse(1.0 * s, 0, 0.44 * s, 0.4 * s, 0),
    filled: true,
  });
  for (const side of [-1, 1]) {
    ink(26, 21, 18).line(
      ...at(1.2 * s, side * 0.2 * s),
      ...at(1.7 * s, side * 0.55 * s),
    );
    ink(240, 236, 220).circle(...at(1.12 * s, side * 0.26 * s), max(1, s * 0.1), true);
  }

  // A hint that shows itself once, then gets out of the way.
  const fade = 255 - max(0, paintCount - 260) * 4;
  if (fade > 0 && !called) {
    ink(255, 240, 210, min(150, fade)).write("hold to call", {
      x: screen.width - 78,
      y: screen.height - 26,
    });
  }
}

// One wing: an ellipse swung out from the shoulder, `spread` sweeping it back.
function wing(s, side, spread) {
  const a = side * (0.35 + spread * 0.7);
  const reach = 0.85 * s;
  return ellipse(
    0.1 * s + reach * cos(a),
    side * 0.18 * s + reach * sin(a),
    reach,
    0.26 * s,
    a,
    12,
  );
}

function act({ event: e, screen, sound }) {
  if (e.is("reframed")) plant(screen);

  if (e.is("touch")) {
    called = true;
    buzz ||= sound?.synth?.({
      type: "square",
      tone: 100,
      attack: 0.08,
      decay: 0.6,
      duration: "🔁",
      volume: 0.12,
    });
  }

  if (e.is("lift")) {
    called = false;
    buzz?.kill(0.4);
    buzz = undefined;
    visit();
  }
}

function leave() {
  buzz?.kill(0.1);
  buzz = undefined;
}

export { boot, sim, paint, act, leave };
