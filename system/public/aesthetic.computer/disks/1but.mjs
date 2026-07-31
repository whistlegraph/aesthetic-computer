// 1but, 26.04.26
// One button game — tap or press space to jump. Dodge the walls!

const { floor, abs } = Math;

const G = 0.52;       // gravity
const JUMP = -9.5;    // jump velocity
const PX = 38;        // player x
const PS = 10;        // player size
const WW = 13;        // wall width
const GH = 64;        // gap height

let player, walls, stars, score, speed, state, best, tick, deathFlash;

function reset(sh = 160) {
  player = { y: sh * 0.38, vy: 0, sq: 1 };
  walls = [];
  score = 0;
  speed = 2.4;
  tick = 0;
  deathFlash = 0;
  state = "playing";
}

function boot({ screen }) {
  best = 0;
  stars = Array.from({ length: 28 }, (_, i) => ({
    x: (i * 137 + 11) % screen.width,
    y: (i * 79 + 7) % screen.height,
    r: i % 3 === 0 ? 2 : 1,
    a: 30 + (i % 6) * 12,
  }));
  reset(screen.height);
}

function spawnWall(sw, sh) {
  const gapY = 20 + floor((((walls.length * 7 + tick) * 1.618) % 1) * (sh - GH - 40));
  walls.push({ x: sw + WW, gapY, passed: false });
}

function sim({ screen, sound }) {
  if (state !== "playing") {
    deathFlash = Math.max(0, deathFlash - 1);
    return;
  }
  tick++;
  speed += 0.00022;

  // Spawn walls
  const spacing = Math.max(95, 140 - speed * 6);
  if (!walls.length || walls[walls.length - 1].x < screen.width - spacing) {
    spawnWall(screen.width, screen.height);
  }

  // Scroll walls
  for (const w of walls) w.x -= speed;
  walls = walls.filter((w) => w.x > -WW - 2);

  // Score milestones
  for (const w of walls) {
    if (!w.passed && w.x + WW < PX) {
      w.passed = true;
      score++;
      if (score > best) best = score;
      if (score % 5 === 0) {
        sound.synth({ type: "triangle", tone: 1047, attack: 0, decay: 0.1, duration: 0.1, volume: 0.25 });
        sound.synth({ type: "triangle", tone: 1319, attack: 0.06, decay: 0.1, duration: 0.1, volume: 0.2 });
      }
    }
  }

  // Physics
  player.vy += G;
  player.y += player.vy;
  player.sq += (1 - player.sq) * 0.25;

  const floor_y = screen.height - PS;
  if (player.y >= floor_y) {
    player.y = floor_y;
    player.vy = 0;
    player.sq = 1.35;
  }
  if (player.y <= 0) { player.y = 0; player.vy = Math.max(0, player.vy); }

  // Collision
  for (const w of walls) {
    const hitX = PX + PS > w.x && PX < w.x + WW;
    const inGap = player.y + PS > w.gapY && player.y < w.gapY + GH;
    if (hitX && !inGap) {
      state = "dead";
      deathFlash = 12;
      sound.synth({ type: "sawtooth", tone: 280, attack: 0, decay: 0.14, duration: 0.14, volume: 0.55 });
      sound.synth({ type: "sawtooth", tone: 140, attack: 0.09, decay: 0.28, duration: 0.28, volume: 0.42 });
      return;
    }
  }
}

function paint({ wipe, ink, screen, write }) {
  const sw = screen.width, sh = screen.height;

  // Background
  if (deathFlash > 0) {
    wipe(deathFlash * 18, 10, 20);
  } else {
    wipe(8, 8, 18);
  }

  // Stars
  for (const s of stars) {
    ink(180, 200, 255, s.a).box(s.x, s.y, s.r, s.r);
  }

  // Walls
  for (const w of walls) {
    const wx = floor(w.x);
    ink(40, 180, 110).box(wx, 0, WW, w.gapY);
    ink(40, 180, 110).box(wx, w.gapY + GH, WW, sh - w.gapY - GH);
    // edge highlight
    ink(80, 240, 150, 100).box(wx, 0, 2, w.gapY);
    ink(80, 240, 150, 100).box(wx, w.gapY + GH, 2, sh - w.gapY - GH);
    // gap mouth hints
    ink(60, 220, 130, 60).box(wx, w.gapY - 1, WW, 1);
    ink(60, 220, 130, 60).box(wx, w.gapY + GH, WW, 1);
  }

  // Player
  const py = floor(player.y);
  const sq = player.sq;
  const pw = floor(PS * (1 / sq));
  const ph = floor(PS * sq);
  const pox = floor((PS - pw) / 2);
  const poy = PS - ph;
  const pcolor = state === "dead" ? [255, 90, 70] : [255, 215, 60];

  ink(...pcolor).box(PX + pox, py + poy, pw, ph);

  // Eyes — two tiny dots on right side
  if (state !== "dead") {
    ink(20, 20, 30)
      .plot(PX + pox + pw - 3, py + poy + 2)
      .plot(PX + pox + pw - 3, py + poy + ph - 3);
  } else {
    // X eyes when dead
    ink(255, 255, 255, 180)
      .plot(PX + pox + pw - 4, py + poy + 2)
      .plot(PX + pox + pw - 3, py + poy + 3)
      .plot(PX + pox + pw - 4, py + poy + 5)
      .plot(PX + pox + pw - 3, py + poy + 4);
  }

  // Speed streaks
  if (state === "playing" && speed > 3.2) {
    const a = floor((speed - 3.2) * 40);
    ink(255, 215, 60, Math.min(a, 80)).line(PX - 3, py + 3, PX - 8, py + 3);
    ink(255, 215, 60, Math.min(a, 50)).line(PX - 3, py + 7, PX - 11, py + 7);
  }

  // Score
  ink(255, 255, 255, 200).write(score, { x: sw / 2, y: 5, center: "x" });

  // Game over overlay
  if (state === "dead") {
    ink(0, 0, 0, 140).box(0, 0, sw, sh);
    ink(255, 90, 70).write("dead", { center: "xy" });
    const cy = floor(sh / 2);
    ink(255, 255, 255, 180).write(`score ${score}`, { x: sw / 2, y: cy + 18, center: "x" });
    if (best > 0) ink(200, 200, 255, 150).write(`best ${best}`, { x: sw / 2, y: cy + 30, center: "x" });
    ink(160, 160, 200, 130).write("tap to play", { x: sw / 2, y: cy + 46, center: "x" });
  }
}

function act({ event: e, screen, sound, needsPaint }) {
  const go = e.is("touch") || e.is("keyboard:down:space") || e.is("keyboard:down:arrowup");
  if (!go) return;

  if (state === "dead") {
    reset(screen.height);
    needsPaint();
    return;
  }

  if (state === "playing") {
    player.vy = JUMP;
    player.sq = 0.7; // squish on jump
    sound.synth({ type: "sine", tone: 740, attack: 0, decay: 0.09, duration: 0.09, volume: 0.35 });
  }
}

function meta() {
  return { title: "1but", desc: "One button game — tap to jump, dodge the walls!" };
}

export { boot, sim, paint, act, meta };
export const nohud = true;
