// dafu, 26.09.11
// A fountain of particles that rattles off boxes and collects in vessels.

const particles = [];
const MAX = 300;

let pointer; // { x, y } — last known pen/mouse position.
let dims = { width: 128, height: 128 };

let boxes = []; // Solid obstacles: { x, y, w, h }.
let vessels = []; // Open-topped cups: { x, y, w, h, fill, hue } + wall boxes.
let laidOutFor = ""; // "WxH" the current layout was built for.

const WALL = 2; // Vessel wall thickness in pixels.
const PER_PARTICLE = 0.6; // How much a settled particle raises a vessel.

// Rebuild the obstacle course whenever the screen size changes.
function layout(width, height) {
  const key = `${width}x${height}`;
  if (key === laidOutFor) return;
  laidOutFor = key;

  boxes = [];
  vessels = [];

  const vh = Math.max(12, Math.round(height * 0.18)); // Vessel height.
  const vy = height - vh;
  const count = 3;
  const gap = Math.round(width * 0.04);
  const vw = Math.floor((width - gap * (count + 1)) / count);

  for (let i = 0; i < count; i += 1) {
    const x = gap + i * (vw + gap);
    vessels.push({ x, y: vy, w: vw, h: vh, fill: 0, hue: 300 });
    // The walls are ordinary obstacles, so rims deflect like everything else.
    boxes.push({ x, y: vy, w: WALL, h: vh });
    boxes.push({ x: x + vw - WALL, y: vy, w: WALL, h: vh });
  }

  // A staggered field of bumpers between the fountain and the vessels.
  const bw = Math.max(6, Math.round(width * 0.16));
  const bh = Math.max(3, Math.round(height * 0.02));
  for (let row = 0; row < 3; row += 1) {
    const y = Math.round(height * (0.42 + row * 0.13));
    const offset = row % 2 ? Math.round(width * 0.25) : 0;
    for (let col = 0; col < 2; col += 1) {
      const x = offset + Math.round(width * 0.08) + col * Math.round(width * 0.5);
      if (x + bw > width) continue;
      boxes.push({ x, y, w: bw, h: bh });
    }
  }
}

// Push a particle back out of any solid box it just entered.
function collide(p, px, py) {
  for (const b of boxes) {
    if (p.x < b.x || p.x > b.x + b.w || p.y < b.y || p.y > b.y + b.h) continue;

    if (py <= b.y) {
      p.y = b.y; // Landed on top.
      p.yv *= -0.4;
      p.xv *= 0.85;
    } else if (py >= b.y + b.h) {
      p.y = b.y + b.h; // Knocked the underside.
      p.yv *= -0.4;
    } else if (px <= b.x) {
      p.x = b.x; // Came in from the left.
      p.xv *= -0.6;
    } else {
      p.x = b.x + b.w; // Came in from the right.
      p.xv *= -0.6;
    }
  }
}

// Settle a particle onto a vessel's surface, absorbing it once it stops.
// Returns true if the particle was absorbed.
function pour(p) {
  for (const v of vessels) {
    if (p.x < v.x + WALL || p.x > v.x + v.w - WALL) continue;
    const surface = v.y + v.h - Math.min(v.fill * PER_PARTICLE, v.h - WALL);
    if (p.y < surface) continue;

    p.y = surface;
    if (Math.abs(p.yv) < 0.6) {
      v.fill += 1;
      // Nudge the vessel's color toward the particle's, the short way around.
      const d = ((p.hue - v.hue + 540) % 360) - 180;
      v.hue = (v.hue + d * 0.25 + 360) % 360;
      return true;
    }
    p.yv *= -0.35; // Otherwise it splashes.
    p.xv *= 0.7;
  }
  return false;
}

function spawn({ num }, x, y, count = 6) {
  for (let i = 0; i < count; i += 1) {
    const angle = num.randIntRange(0, 360) * (Math.PI / 180);
    const speed = num.randInt(20) / 10 + 0.4;
    particles.push({
      x,
      y,
      xv: Math.cos(angle) * speed,
      yv: Math.sin(angle) * speed - 1.2, // A little upward kick.
      life: 1,
      decay: 0.006 + num.randInt(14) / 1000,
      hue: num.randIntRange(0, 360),
    });
  }
  while (particles.length > MAX) particles.shift();
}

function sim($) {
  const { screen } = $;
  if (screen) dims = { width: screen.width, height: screen.height };
  layout(dims.width, dims.height);

  const src = pointer || { x: dims.width / 2, y: dims.height * 0.2 };
  spawn($, src.x, src.y, 2);

  for (const v of vessels) v.fill = Math.max(0, v.fill - 0.03); // Slow seepage.

  for (let i = particles.length - 1; i >= 0; i -= 1) {
    const p = particles[i];
    const px = p.x;
    const py = p.y;

    p.x += p.xv;
    p.y += p.yv;
    p.yv += 0.06; // Gravity.
    p.xv *= 0.99;
    p.life -= p.decay;

    collide(p, px, py);

    if (pour(p)) {
      particles.splice(i, 1);
      continue;
    }

    if (p.y > dims.height) {
      // Bounce off the floor, losing most of the energy.
      p.y = dims.height;
      p.yv *= -0.45;
      p.xv *= 0.8;
    }

    if (p.life <= 0) particles.splice(i, 1);
  }
}

function paint({ wipe, ink, screen, num }) {
  dims = { width: screen.width, height: screen.height };
  layout(screen.width, screen.height);
  wipe(70, 50, 100);

  for (const v of vessels) {
    const level = Math.min(v.fill * PER_PARTICLE, v.h - WALL);
    if (level > 0) {
      const [r, g, b] = num.hslToRgb(v.hue, 70, 45);
      ink(r, g, b, 200).box(
        v.x + WALL,
        Math.round(v.y + v.h - level),
        v.w - WALL * 2,
        Math.round(level),
      );
    }
    ink(200, 190, 230).box(v.x, v.y, v.w, v.h, "outline"); // The cup itself.
  }

  for (const b of boxes) ink(150, 140, 190).box(b.x, b.y, b.w, b.h);

  for (const p of particles) {
    const [r, g, b] = num.hslToRgb(p.hue, 80, 40 + p.life * 30);
    const size = Math.max(1, Math.round(p.life * 3));
    ink(r, g, b, Math.round(p.life * 255)).box(
      Math.round(p.x) - (size >> 1),
      Math.round(p.y) - (size >> 1),
      size,
    );
  }

  ink(255, 100, 255).write("dafu", { x: screen.width / 2, y: 6, center: "x" });
}

function act($) {
  const e = $.event;
  if (e.is("move") || e.is("draw") || e.is("touch")) {
    pointer = { x: e.x, y: e.y };
    if (e.is("touch") || e.is("draw")) spawn($, e.x, e.y, 10); // Burst on contact.
  }
}

export { sim, paint, act };
