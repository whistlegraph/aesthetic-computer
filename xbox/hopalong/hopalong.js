// hopalong — a side-scrolling platformer for the Xbox native host (QuickJS).
// One script; the host's bindings are its only way out: wipe, triangles3d (or
// triangle3d), gamepad, runtime, synth, drum, telemetry. Nothing is called at
// top level: the console rejects a script that touches a binding before boot.
//
// Pad: LS / d-pad move · A jump (hold for height) · X, B or RT run · down in
// the air pounds · push into a wall while falling to slide, A kicks off ·
// Menu pause · View frame meter. Left alone on the title it plays itself.
//
// World units are tiles: x right, y up, z into the screen. Play happens on
// z = 0; a perspective camera DIST tiles back gives blocks their tops and
// sides and the far hills their parallax. The host's z-buffer sorts it all.

const TAU = Math.PI * 2, ROWS = 20, MAX_FACES = 8192, DIST = 22, VIEW = 13, DT = 1 / 60;
const EMPTY = 0, GROUND = 1, STONE = 2, TRUNK = 3, BRICK = 4, PRIZE = 5, BERRYBOX = 6, USED = 7,
  PLANK = 8, SPIKE = 9, SPRING = 10, GEM = 11, MOON = 12, FLAG = 13, GOAL = 14;
const isStatic = (t) => t >= GROUND && t <= TRUNK;
const isSolid = (t) => (t >= GROUND && t <= USED) || t === SPRING;
const CHARS = { "#": GROUND, S: STONE, T: TRUNK, B: BRICK, "?": PRIZE, "!": BERRYBOX, "=": PLANK,
  "^": SPIKE, J: SPRING, o: GEM, "*": MOON, C: FLAG, G: GOAL };
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const sign = (v) => (v > 0 ? 1 : v < 0 ? -1 : 0);

const WALK = 6.5, RUN = 10, ACC = 34, AIR = 22, FRIC = 30, JUMP = 19.5;
const G_UP = 48, G_CUT = 120, G_DOWN = 64, FALL = 24, SLIDE = 5;

// ——— levels: built by a few verbs onto a grid, fresh each time one starts ———

const THEMES = {
  meadow: { sky: [[78, 160, 255], [196, 232, 255]], hills: [[132, 208, 160], [92, 178, 128]],
    grass: [98, 204, 86], dirt: [172, 112, 68], stone: [152, 158, 176], trunk: [150, 104, 72],
    plank: [222, 178, 110], cloud: [255, 255, 255], sun: [255, 246, 196] },
  dusk: { sky: [[44, 36, 92], [244, 150, 118]], hills: [[104, 76, 130], [70, 52, 100]],
    grass: [74, 178, 156], dirt: [98, 78, 116], stone: [116, 108, 148], trunk: [84, 66, 100],
    plank: [186, 140, 160], cloud: [250, 196, 170], sun: [255, 214, 150] },
  cloud: { sky: [[36, 104, 222], [176, 224, 255]], hills: [[214, 232, 255], [176, 208, 246]],
    grass: [226, 240, 255], dirt: [112, 164, 220], stone: [170, 188, 222], trunk: [140, 160, 204],
    plank: [255, 255, 255], cloud: [255, 255, 255], sun: [255, 250, 220] },
};

function build(name, theme, cols, time, make) {
  const tiles = new Uint8Array(cols * ROWS), foes = [], lifts = [];
  const put = (x, y, t) => { if (x >= 0 && x < cols && y >= 0 && y < ROWS) tiles[y * cols + x] = t; };
  const top = (x) => { for (let y = ROWS - 1; y >= 0; y--) if (isSolid(tiles[y * cols + Math.floor(x)])) return y + 1; return 0; };
  const b = {
    put,
    fill(x0, y0, x1, y1, t) { for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) put(x, y, t); },
    ground(x0, x1, h = 3) { b.fill(x0, 0, x1 - 1, h - 1, GROUND); },
    row(x, y, s) { for (let i = 0; i < s.length; i++) if (CHARS[s[i]]) put(x + i, y, CHARS[s[i]]); },
    arc(x, y, n) { for (let i = 0; i < n; i++) put(x + i, y + Math.round(Math.sin((i + .5) / n * Math.PI) * 1.6), GEM); },
    stairs(x, y, n, dir) { for (let i = 0; i < n; i++) { const c = x + (dir > 0 ? i : n - 1 - i); b.fill(c, y, c, y + i, STONE); } },
    foe(kind, x, y) { foes.push({ kind, x: x + .5, y: y === undefined ? top(x) : y }); },
    lift(x, y, dx, dy, period = 5, w = 3) { lifts.push({ x0: x, y0: y, dx, dy, period, w }); },
  };
  make(b);
  let moons = 0, goal = cols - 6;
  for (let i = 0; i < tiles.length; i++) { if (tiles[i] === MOON) moons++; if (tiles[i] === GOAL) goal = i % cols; }
  return { name, theme: THEMES[theme], cols, time, tiles, foes, lifts, moons, goal };
}

const LEVELS = [
  () => build("MEADOW ONE", "meadow", 200, 300, (b) => {
    b.ground(0, 42);
    b.row(9, 6, "ooo"); b.row(15, 6, "?");
    b.foe("grub", 20);
    b.row(23, 6, "B!B?B"); b.row(25, 10, "?");
    b.fill(31, 3, 32, 4, TRUNK);
    b.foe("grub", 36); b.foe("grub", 39);
    b.arc(41, 5, 5);                              // over the first gap
    b.ground(45, 70);
    b.fill(50, 3, 56, 4, GROUND); b.fill(52, 5, 54, 5, GROUND);
    b.row(52, 9, "ooo");
    b.foe("hopper", 61);
    b.row(63, 6, "BB?BB"); b.row(64, 10, "o*o");
    b.foe("grub", 68);
    b.row(71, 4, "==");
    b.ground(74, 100);
    b.stairs(78, 3, 3, 1); b.stairs(84, 3, 3, -1);
    b.foe("grub", 82);
    b.put(89, 3, FLAG);
    b.row(92, 6, "?B!B?");
    b.foe("burr", 96);
    b.ground(103, 130);
    b.row(109, 3, "J"); b.row(108, 11, "o*o");
    b.fill(114, 3, 116, 5, STONE); b.row(114, 7, "ooo");
    b.foe("hopper", 121); b.foe("grub", 126);
    b.row(131, 5, "==");
    b.ground(134, 160);
    b.row(138, 6, "BBBB"); b.row(139, 10, "?!?");
    b.foe("flutter", 145, 6);
    b.fill(150, 3, 151, 5, TRUNK); b.fill(154, 3, 155, 6, TRUNK);
    b.put(152, 3, MOON); b.put(153, 3, GEM);
    b.ground(163, 200);
    b.arc(160, 5, 3);
    b.foe("grub", 167);
    b.stairs(171, 3, 6, 1);
    b.row(178, 9, "ooo");
    b.put(186, 3, GOAL);
  }),
  () => build("DUSK HOLLOW", "dusk", 220, 320, (b) => {
    b.ground(0, 22);
    b.row(6, 6, "ooo"); b.row(11, 6, "B?B");
    b.foe("burr", 19);
    b.lift(22, 3, 5, 0, 5);                       // a gap only the lift crosses
    b.arc(24, 5, 5);
    b.ground(30, 56);
    b.row(35, 3, "^^");
    b.fill(40, 7, 50, 7, STONE); b.row(41, 5, "ooooooooo");
    b.foe("grub", 43); b.foe("grub", 47);
    b.row(52, 6, "?!?");
    b.foe("hopper", 54);
    b.ground(59, 66);
    b.lift(66, 3, 0, 7, 6, 4);                    // up the cliff
    b.fill(70, 0, 73, 9, STONE);
    b.put(72, 10, FLAG); b.row(70, 13, "o*o");
    b.row(75, 8, "=="); b.row(79, 6, "==");
    b.ground(83, 110);
    b.row(88, 6, "BBBBBB"); b.row(90, 10, "?");
    b.foe("burr", 93); b.foe("flutter", 97, 7); b.foe("flutter", 102, 6);
    b.row(104, 3, "^^^");
    b.row(111, 4, "==");
    b.ground(114, 140);
    b.fill(120, 3, 121, 5, TRUNK);
    b.row(126, 3, "J"); b.row(125, 11, "o*o");
    b.fill(129, 3, 131, 5, STONE);
    b.foe("hopper", 135);
    b.lift(140, 3, 8, 0, 7);
    b.arc(143, 5, 6);
    b.ground(151, 220);
    b.fill(160, 6, 160, 14, STONE); b.fill(164, 6, 164, 14, STONE);   // a chimney to kick up
    b.put(162, 9, GEM); b.put(162, 11, GEM); b.put(162, 13, MOON);
    b.foe("burr", 177); b.foe("grub", 181); b.foe("hopper", 184);
    b.row(170, 6, "?B?");
    b.stairs(187, 3, 5, 1);
    b.put(200, 3, GOAL);
  }),
  () => build("CLOUD STAIR", "cloud", 212, 340, (b) => {
    b.ground(0, 14);
    b.row(8, 6, "?!?");
    b.row(16, 4, "==="); b.row(21, 6, "==="); b.row(26, 5, "===");
    b.row(17, 6, "o"); b.row(22, 8, "o"); b.row(27, 7, "o");
    b.ground(31, 44);
    b.foe("hopper", 36); b.row(38, 6, "BB?BB");
    b.lift(44, 3, 6, 0, 5.5);
    b.ground(53, 60);
    b.row(57, 3, "J"); b.row(55, 12, "====="); b.put(57, 14, MOON);
    b.row(62, 5, "=="); b.row(66, 7, "=="); b.row(70, 9, "=="); b.row(74, 11, "====");
    b.put(76, 12, FLAG);
    b.foe("flutter", 71, 12); b.foe("flutter", 76, 14.5);
    b.lift(79, 5, 0, 6, 6);
    b.ground(84, 98);
    b.foe("grub", 88); b.foe("grub", 93); b.row(87, 6, "?B?");
    b.row(95, 3, "^^");
    b.row(100, 4, "=="); b.row(104, 5, "==");
    b.lift(107, 5, 7, 0, 6);
    b.fill(118, 0, 129, 4, GROUND);
    b.foe("hopper", 124); b.row(121, 8, "o*o");
    b.row(132, 6, "==="); b.row(137, 8, "==="); b.row(142, 6, "===");
    b.row(138, 10, "o"); b.put(138, 12, MOON);
    b.foe("flutter", 140, 11.5);
    b.ground(147, 160);
    b.fill(153, 3, 154, 4, STONE); b.fill(156, 3, 158, 6, STONE);
    b.lift(160, 3, 8, 0, 6.5);
    b.arc(163, 5, 6);
    b.ground(171, 212);
    b.foe("grub", 175);
    b.stairs(179, 3, 6, 1);
    b.put(194, 3, GOAL);
  }),
];

// ——— state ———

let L = null, levelIndex = 0, columns = [];
let mode = "title", modeT = 0, tick = 0, demo = true;
let lives = 5, gems = 0, score = 0, timeLeft = 0, moonsGot = 0, checkpoint = null, deaths = 0;
let foes = [], lifts = [], items = [], parts = [], bumps = [], notes = [];
let camX = 0, camY = 7, shake = 0;
let W = 1920, H = 1080, F = 1697;
let buf = null, n = 0, batch = false, booted = false;
let prevDown = [], meter = false, fps = 0, fpsFrames = 0, fpsAt = 0, jsMs = 0, lastTris = 0, frames = 0;
const hero = { x: 3.5, y: 3, vx: 0, vy: 0, w: .6, h: .9, big: false, face: 1, ground: false, coyote: 0,
  buffer: 0, wall: 0, lock: 0, pound: 0, hang: 0, inv: 0, stride: 0, lift: null, hitX: 0, hitY: 0 };

function tileAt(cx, cy) {
  return cx < 0 || cx >= L.cols ? STONE : cy < 0 || cy >= ROWS ? EMPTY : L.tiles[cy * L.cols + cx];
}
const solid = (cx, cy) => isSolid(tileAt(cx, cy));
function setTile(cx, cy, t) { L.tiles[cy * L.cols + cx] = t; }

function loadLevel(i) {
  levelIndex = i; L = LEVELS[i]();
  bake();
  foes = L.foes.map((f) => {
    const size = { grub: [.8, .62], hopper: [.76, .58], burr: [.8, .6], flutter: [.7, .5] }[f.kind];
    return { kind: f.kind, x: f.x, y: f.y, hx: f.x, hy: f.y, vx: 0, vy: 0, w: size[0], h: size[1],
      dir: -1, t: (f.x * 7) % 3, dead: 0, ground: false, hitX: 0, hitY: 0 };
  });
  lifts = L.lifts.map((l) => ({ ...l, x: l.x0, top: l.y0, px: l.x0, ptop: l.y0, t: 0 }));
  items = []; parts = []; bumps = [];
  timeLeft = L.time; moonsGot = 0;
  const at = checkpoint && checkpoint.level === i ? checkpoint : { x: 3.5, y: 3 };
  Object.assign(hero, { x: at.x, y: at.y, vx: 0, vy: 0, h: .9, big: false, face: 1, ground: false, coyote: 0,
    buffer: 0, wall: 0, lock: 0, pound: 0, hang: 0, inv: 0, lift: null });
  camX = hero.x; camY = 7; botTarget = null; botStill = 0;
}

function enterTitle() { mode = "title"; modeT = 0; demo = true; checkpoint = null; loadLevel(0); }
function startGame(i = 0) {
  lives = 5; gems = 0; score = 0; deaths = 0; demo = false; checkpoint = null;
  loadLevel(i); mode = "intro"; modeT = 0;
}

// ——— sound: every host has synth; drum where there is one ———

function tone(f, d = .08, after = 0) {
  if (after) { notes.push({ at: tick + after, f, d }); return; }
  try { if (typeof synth === "function") synth(f, d); } catch (_) {}
}
function hit(name, amount = 1) { try { if (typeof drum === "function") drum(name, amount); } catch (_) {} }
function run(fs, step = 5, d = .09) { if (demo) return; for (let i = 0; i < fs.length; i++) tone(fs[i], d, i * step + 1); }
const sfx = {
  jump: () => run([523, 698], 3, .06), gem: () => run([1568, 2093], 3, .06), stomp: () => { if (!demo) hit("block"); run([330, 494], 3, .05); },
  bump: () => { if (!demo) hit("kick", .7); }, smash: () => { if (!demo) hit("snare", .8); },
  grow: () => run([523, 659, 784, 1047, 1319], 4), hurt: () => run([392, 311, 233], 5, .1),
  die: () => run([494, 466, 440, 349, 262, 196], 7, .14), spring: () => run([392, 784, 1175], 2, .06),
  flag: () => run([784, 988, 1175], 5), clear: () => run([523, 659, 784, 1047, 784, 1047, 1319], 8, .16),
  moon: () => run([880, 1109, 1319, 1760], 5, .1), kick: () => run([440, 660], 2, .05), life: () => run([659, 784, 1319, 1047, 1175, 1568], 5),
};

// ——— tiles the hero changes ———

function burst(x, y, count, r, g, b, speed = 6) {
  for (let i = 0; i < count; i++) {
    const a = i / count * TAU + x;
    parts.push({ x, y, vx: Math.cos(a) * speed * (.5 + (i % 3) * .3), vy: Math.sin(a) * speed + 5, t: .6, r, g, b, s: .16 });
  }
}
function addGems(k) { gems += k; score += 10 * k; if (gems >= 100) { gems -= 100; lives++; sfx.life(); } }

// Struck from below (or pounded from above): bricks break under a big hero,
// prize boxes pay out once, and anything standing on the tile is thrown.
function bump(cx, cy, hard) {
  const t = tileAt(cx, cy);
  if (t !== BRICK && t !== PRIZE && t !== BERRYBOX) { if (t !== EMPTY) sfx.bump(); return; }
  for (const f of foes) if (!f.dead && Math.abs(f.x - cx - .5) < .9 && Math.abs(f.y - cy - 1) < .3) toss(f);
  if (t === BRICK) {
    if (hero.big || hard) { setTile(cx, cy, EMPTY); burst(cx + .5, cy + .5, 8, 196, 110, 64, 7); score += 50; sfx.smash(); return; }
  } else {
    setTile(cx, cy, USED);
    if (t === PRIZE) { addGems(1); parts.push({ x: cx + .5, y: cy + 1.2, vx: 0, vy: 9, t: .5, r: 255, g: 220, b: 70, s: .3 }); sfx.gem(); }
    else { items.push({ x: cx + .5, y: cy + 1, t: 0 }); run([392, 523, 659], 4); }
  }
  bumps.push({ cx, cy, t: 0 }); sfx.bump();
}
function toss(f) { f.dead = 2; f.t = 0; f.vy = 11; score += 100; sfx.stomp(); }

function hurt() {
  if (hero.inv > 0 || mode !== "play") return;
  if (hero.big) { hero.big = false; hero.h = .9; hero.inv = 2; sfx.hurt(); return; }
  die();
}
function die() {
  if (mode !== "play") return;
  mode = "dead"; modeT = 0; hero.vx = 0; hero.vy = 17; hero.lift = null; deaths++; sfx.die();
}

// ——— movement shared by the hero and the foes ———

function moveX(e, dx) {
  e.x += dx;
  const hw = e.w / 2, y0 = Math.floor(e.y + .001), y1 = Math.floor(e.y + e.h - .001);
  const cx = Math.floor(dx > 0 ? e.x + hw : e.x - hw);
  for (let cy = y0; cy <= y1; cy++) if (solid(cx, cy)) { e.x = dx > 0 ? cx - hw - .001 : cx + 1 + hw + .001; return true; }
  return false;
}
// 1 landed, 2 struck a ceiling; e.hitX / e.hitY name the tile (the one nearest the middle).
function moveY(e, dy) {
  const prev = e.y; e.y += dy;
  const x0 = Math.floor(e.x - e.w / 2 + .001), x1 = Math.floor(e.x + e.w / 2 - .001), mid = Math.floor(e.x);
  const cy = dy <= 0 ? Math.floor(e.y) : Math.floor(e.y + e.h);
  let found = -1;
  for (let cx = x0; cx <= x1; cx++) {
    const t = tileAt(cx, cy);
    if (isSolid(t) || (dy <= 0 && t === PLANK && prev >= cy + 1 - .001)) if (found < 0 || cx === mid) found = cx;
  }
  if (found < 0) return 0;
  e.hitX = found; e.hitY = cy;
  if (dy <= 0) { e.y = cy + 1; return 1; }
  e.y = cy - e.h - .001; return 2;
}

// ——— the hero ———

function readPad() {
  if (hopalong.script) return hopalong.script;
  let pad = null;
  try { pad = typeof gamepad === "function" ? gamepad(0) : null; } catch (_) { pad = null; }
  const down = pad && Array.isArray(pad.down) ? pad.down : [];
  const has = (name) => down.indexOf(name) >= 0, pressed = (name) => has(name) && prevDown.indexOf(name) < 0;
  const sx = +(pad && pad.leftX) || 0, sy = +(pad && pad.leftY) || 0;
  const out = {
    lx: has("ArrowLeft") || sx < -.35 ? -1 : has("ArrowRight") || sx > .35 ? 1 : 0,
    down: has("ArrowDown") || sy < -.6,
    jump: has("A"), jumpPressed: pressed("A"),
    run: has("X") || has("B") || (+(pad && pad.rightTrigger) || 0) > .3,
    menu: pressed("Menu"), view: pressed("View"), any: down.length > 0,
  };
  out.downPressed = out.down && !readPad.wasDown; readPad.wasDown = out.down;
  prevDown = down.slice();
  return out;
}

function stepHero(pad) {
  const h = hero;
  if (h.inv > 0) h.inv -= DT;
  if (h.lock > 0) h.lock -= DT;
  if (h.lift) {                                   // carried
    const l = h.lift;
    if (h.x + h.w / 2 < l.x || h.x - h.w / 2 > l.x + l.w) h.lift = null;
    else { h.x += l.x - l.px; h.y = l.top; }
  }
  const lx = h.lock > 0 || h.pound ? 0 : pad.lx, max = pad.run ? RUN : WALK;
  if (h.pound) h.vx = 0;
  else if (lx) {
    const turning = h.ground && h.vx * lx < 0;
    h.vx += lx * (h.ground ? ACC : AIR) * (turning ? 2 : 1) * DT;
    if (Math.abs(h.vx) > max) h.vx = sign(h.vx) * Math.max(max, Math.abs(h.vx) - FRIC * DT);
    h.face = lx;
  } else if (h.lock <= 0) {
    const drag = (h.ground ? FRIC : 5) * DT;
    h.vx = Math.abs(h.vx) <= drag ? 0 : h.vx - sign(h.vx) * drag;
  }

  h.buffer = pad.jumpPressed ? .12 : h.buffer - DT;
  h.coyote = h.ground ? .1 : h.coyote - DT;
  if (h.buffer > 0 && h.coyote > 0 && !h.pound) {
    h.vy = JUMP + Math.abs(h.vx) * .22; h.ground = false; h.coyote = 0; h.buffer = 0; h.lift = null; sfx.jump();
  } else if (h.buffer > 0 && h.wall && !h.ground) {
    h.vx = -h.wall * 9.5; h.vy = 18; h.face = -h.wall; h.buffer = 0; h.lock = .16; h.wall = 0; sfx.kick();
    burst(h.x + h.face * -.3, h.y + .4, 4, 255, 255, 255, 3);
  }
  if (!h.ground && pad.downPressed && !h.pound) { h.pound = 1; h.hang = .14; h.vy = 0; }

  if (h.hang > 0) { h.hang -= DT; h.vy = 0; }
  else if (h.pound) h.vy = -30;
  else {
    h.vy -= (h.vy > 0 ? (pad.jump ? G_UP : G_CUT) : G_DOWN) * DT;
    const cap = h.wall ? -SLIDE : -FALL;
    if (h.vy < cap) h.vy = cap;
  }

  const prevY = h.y;
  if (moveX(h, h.vx * DT)) h.vx = 0;
  h.wall = 0;
  if (!h.ground && h.vy < 0 && pad.lx && h.lock <= 0) {
    const cx = Math.floor(h.x + pad.lx * (h.w / 2 + .06));
    if (solid(cx, Math.floor(h.y + .3)) && solid(cx, Math.floor(h.y + h.h - .1))) h.wall = pad.lx;
  }
  const was = h.ground;
  h.ground = false; h.lift = null;
  const struck = moveY(h, h.vy * DT);
  if (struck === 2) { h.vy = 0; bump(h.hitX, h.hitY, false); }
  let landed = struck === 1;
  if (landed && tileAt(h.hitX, h.hitY) === SPRING && !h.pound) {
    h.vy = pad.jump ? 30 : 24; landed = false; bumps.push({ cx: h.hitX, cy: h.hitY, t: 0 }); sfx.spring();
  }
  if (!landed && h.vy <= 0) for (const l of lifts) {
    if (h.x + h.w / 2 > l.x && h.x - h.w / 2 < l.x + l.w && prevY >= l.ptop - .06 && h.y <= l.top + .02) {
      h.y = l.top; h.lift = l; landed = true; break;
    }
  }
  if (landed) {
    h.ground = true;
    if (h.pound) {
      h.pound = 0; shake = .25; sfx.smash();
      if (!h.lift) for (let cx = Math.floor(h.x - h.w / 2); cx <= Math.floor(h.x + h.w / 2); cx++) bump(cx, h.hitY, h.big);
      for (const f of foes) if (!f.dead && Math.abs(f.x - h.x) < 1.8 && Math.abs(f.y - h.y) < .6) toss(f);
      burst(h.x, h.y + .1, 8, 255, 255, 255, 5);
    } else if (!was && h.vy < -12) burst(h.x, h.y + .05, 4, 255, 255, 255, 2.5);
    h.vy = 0;
  }
  if (h.ground && Math.abs(h.vx) > .5) h.stride += Math.abs(h.vx) * DT * 2.6;

  if (h.x < h.w / 2) { h.x = h.w / 2; h.vx = 0; }
  if (h.y < -3) { die(); return; }

  // what the body overlaps
  const x0 = Math.floor(h.x - h.w / 2), x1 = Math.floor(h.x + h.w / 2), y0 = Math.floor(h.y), y1 = Math.floor(h.y + h.h - .01);
  for (let cy = y0; cy <= y1; cy++) for (let cx = x0; cx <= x1; cx++) {
    const t = tileAt(cx, cy);
    if (t === GEM) { setTile(cx, cy, EMPTY); addGems(1); sfx.gem(); burst(cx + .5, cy + .5, 4, 255, 230, 90, 3); }
    else if (t === MOON) { setTile(cx, cy, EMPTY); moonsGot++; score += 1000; sfx.moon(); burst(cx + .5, cy + .5, 10, 200, 240, 255, 6); }
    else if (t === FLAG && !(checkpoint && checkpoint.x === cx + .5)) {
      checkpoint = { level: levelIndex, x: cx + .5, y: cy }; sfx.flag(); burst(cx + .5, cy + 1.5, 8, 255, 120, 180, 5);
    } else if (t === SPIKE && h.y < cy + .55 && Math.abs(h.x - cx - .5) < .62) hurt();
  }
  for (let i = items.length - 1; i >= 0; i--) {
    const it = items[i];
    if (Math.abs(it.x - h.x) < .7 && it.y < h.y + h.h && it.y + .7 > h.y) {
      items.splice(i, 1);
      if (!h.big && !solid(Math.floor(h.x), Math.floor(h.y + 1.3))) { h.big = true; h.h = 1.4; }
      score += 500; sfx.grow(); burst(h.x, h.y + .6, 10, 255, 110, 150, 6);
    }
  }
  if (h.x >= L.goal + .5 && mode === "play") {
    mode = "clear"; modeT = 0; sfx.clear();
    score += Math.ceil(timeLeft) * 10 + moonsGot * 500;
  }
}

// ——— foes ———

function stepFoe(f) {
  const h = hero;
  if (f.dead) { f.t += DT; if (f.dead === 2) { f.vy -= G_DOWN * DT; f.y += f.vy * DT; f.x += f.dir * -3 * DT; } return; }
  if (Math.abs(f.x - camX) > 22) return;          // asleep until the camera comes near
  if (f.kind === "flutter") {
    f.t += DT;
    const nx = f.hx + Math.sin(f.t * 1.2) * 3;
    f.dir = nx > f.x ? 1 : -1; f.x = nx; f.y = f.hy + Math.sin(f.t * 2.3) * .8;
  } else {
    if (f.kind === "hopper") {
      if (f.ground) { f.vx = 0; f.t += DT; if (f.t > .9) { f.t = 0; f.dir = h.x > f.x ? 1 : -1; f.vy = 14; f.vx = f.dir * 3.2; } }
    } else {
      f.vx = f.dir * (f.kind === "burr" ? 1.6 : 2.1);
      if (f.kind === "burr" && f.ground) {        // a burr keeps to its ledge
        const u = tileAt(Math.floor(f.x + f.dir * .5), Math.floor(f.y - .5));
        if (!isSolid(u) && u !== PLANK) f.dir = -f.dir;
      }
    }
    f.vy = Math.max(-FALL, f.vy - G_DOWN * DT);
    if (moveX(f, f.vx * DT)) { f.dir = -f.dir; f.vx = -f.vx; }
    f.ground = moveY(f, f.vy * DT) === 1;
    if (f.ground) f.vy = 0;
    if (f.y < -4) { f.dead = 3; return; }
  }
  if (mode !== "play") return;
  if (Math.abs(f.x - h.x) < (f.w + h.w) / 2 - .06 && h.y < f.y + f.h && h.y + h.h > f.y) {
    const above = h.vy < 0 && h.y - h.vy * DT > f.y + f.h * .3;   // where it was a tick ago: a long fall still lands on top
    if (above && f.kind !== "burr") {
      f.dead = 1; f.t = 0; score += 100; sfx.stomp();
      burst(f.x, f.y + .2, 5, 255, 255, 255, 3);
      if (h.pound) toss(f); else { h.vy = h.buffer > 0 || lastPad.jump ? 20 : 12; h.y = f.y + f.h; }
    } else hurt();
  }
}

// ——— the autopilot: the title's demo, and the proof a level can be finished ———

let botTarget = null, botStill = 0, botX = 0, botHold = 0, botLeapt = false;
function standable(cx, cy) {
  const under = tileAt(cx, cy - 1), at = tileAt(cx, cy);
  return (isSolid(under) || under === PLANK) && !isSolid(at) && at !== SPIKE && !solid(cx, cy + 1);
}
// The nearest footing past column c0 that a jump from `feet` could reach.
function landingPast(c0, feet, reach, low = feet - 9) {
  for (let d = 1; d <= reach; d++)
    for (let cy = feet + 3; cy >= Math.max(1, low); cy--)
      if (standable(c0 + d, cy)) return { x: c0 + d + (standable(c0 + d + 1, cy) ? 1 : .5), y: cy, lift: null };
  return null;
}
function bot() {
  const h = hero, out = { lx: 1, down: false, downPressed: false, jump: false, jumpPressed: false, run: false };
  const feet = Math.floor(h.y + .01);
  const leap = (target) => {
    const tx = !target ? h.x + 2.5 : target.lift ? target.lift.x + target.lift.w / 2 : target.x;
    for (const f of foes) if (f.kind === "flutter" && !f.dead && botStill < 240 &&
      Math.abs(f.x - (h.x + tx) / 2) < 2.6 && f.y > h.y - 1 && f.y < h.y + 5.5) { out.lx = 0; return; }
    out.jump = out.jumpPressed = true; botHold = 8; botTarget = target; botLeapt = true;
  };
  if (!h.ground) {
    if (!botLeapt && !botTarget) botTarget = landingPast(Math.floor(h.x) - 1, feet, 6);   // walked off a ledge: aim for what is below
    out.jump = botHold-- > 0 || h.vy > 0;
    if (botTarget) {
      const tx = botTarget.lift ? botTarget.lift.x + botTarget.lift.w / 2 : botTarget.x;
      const ty = botTarget.lift ? botTarget.lift.top : botTarget.y;
      const lead = h.x + h.vx * .22;
      out.lx = lead < tx - .15 ? 1 : lead > tx + .15 ? -1 : 0;
      out.run = tx - h.x > 3;
      if (h.vy > 0 && h.y > ty + 1.3 && tx - h.x < 2.5) out.jump = false;
    }
    return out;
  }
  botTarget = null; botLeapt = false;
  botStill = Math.abs(h.x - botX) < .02 ? botStill + 1 : 0; botX = h.x;

  if (h.lift) {                                   // ride until there is somewhere to step off
    const l = h.lift, edge = Math.floor(l.x + l.w - .01), t = landingPast(edge, feet, 4);
    if (t && t.x - h.x < 5.5) { out.lx = 1; if (h.x > l.x + l.w - 1) leap(t); }
    else out.lx = sign(Math.round((l.x + l.w / 2 - h.x) * 4));
    return out;
  }

  const c = Math.floor(h.x + .5 + Math.abs(h.vx) * .13);   // the column about to be underfoot
  let wall = 0;
  while (wall < 6 && solid(c, feet + wall)) wall++;
  if (wall) {
    if (tileAt(c, feet + wall - 1) === SPRING) leap(landingPast(c, feet, 6));
    else if (wall <= 3) leap({ x: c + .5, y: feet + wall, lift: null });
    else if (botStill > 120) leap(null);
    return out;
  }
  let drop = 0;
  while (drop <= 5 && !standable(c, feet - drop)) drop++;
  const across = drop >= 2 && drop <= 5 ? landingPast(c, feet, 4, feet - 1) : null;   // a dip better jumped than entered
  if (across) { leap(across); return out; }
  if (drop <= 5) {
    for (const f of foes) if (!f.dead && f.kind !== "flutter" && f.x - h.x > 0 && f.x - h.x < 2.1 + Math.abs(h.vx) * .12 && Math.abs(f.y - h.y) < 1.2) {
      const past = Math.floor(f.x + 1.5);
      leap(standable(past, feet) ? { x: past + .5, y: feet, lift: null } : landingPast(past, feet, 6)); break;
    }
    return out;
  }
  const t = landingPast(c - 1, feet, 6);
  if (t) { leap(t); out.run = t.x - h.x > 3; return out; }
  for (const l of lifts) {                        // nothing but a lift: wait for it at the edge
    if (l.x0 + l.dx + l.w < h.x || l.x0 > h.x + 10) continue;
    if (l.x <= h.x + 1.7 && l.x + l.w > h.x + .8 && Math.abs(l.top - h.y) < 1.3) leap({ lift: l });
    else out.lx = 0;
    return out;
  }
  leap(null);
  return out;
}

// ——— sim ———

let lastPad = { lx: 0, jump: false };
function stepWorld(pad) {
  lastPad = pad;
  for (const l of lifts) {
    l.px = l.x; l.ptop = l.top; l.t += DT;
    const k = .5 - .5 * Math.cos(l.t / l.period * TAU);
    l.x = l.x0 + l.dx * k; l.top = l.y0 + l.dy * k;
  }
  if (mode === "play") stepHero(pad);
  for (const f of foes) stepFoe(f);
  for (let i = parts.length - 1; i >= 0; i--) {
    const p = parts[i];
    p.t -= DT; p.vy -= 30 * DT; p.x += p.vx * DT; p.y += p.vy * DT;
    if (p.t <= 0) parts.splice(i, 1);
  }
  for (let i = bumps.length - 1; i >= 0; i--) if ((bumps[i].t += DT) > .2) bumps.splice(i, 1);
  for (const it of items) it.t += DT;
  if (shake > 0) shake -= DT;
  // the camera leads the way the hero faces, and only climbs when it must
  const half = VIEW * W / H / 2;
  camX += (clamp(hero.x + hero.face * 2.2, half, L.cols - half) - camX) * .09;
  camX = clamp(camX, half, L.cols - half);
  camY += (clamp(hero.y - 1.5, 7, ROWS - 7) - camY) * .06;
}

function sim() {
  if (!booted) return;
  tick++; modeT += DT;
  for (let i = notes.length - 1; i >= 0; i--) if (notes[i].at <= tick) { tone(notes[i].f, notes[i].d); notes.splice(i, 1); }
  const pad = readPad();
  if (pad.view) meter = !meter;
  if (mode === "title") {
    if (modeT > .5 && (pad.jumpPressed || pad.menu)) { startGame(0); return; }
    mode = "play"; stepWorld(bot());
    if (mode === "play") mode = "title"; else { const t = modeT; enterTitle(); modeT = t; }
  } else if (mode === "intro") {
    if (modeT > 1.8) { mode = "play"; modeT = 0; }
  } else if (mode === "play") {
    if (pad.menu) { mode = "pause"; return; }
    timeLeft -= DT;
    if (timeLeft <= 0) { timeLeft = 0; die(); }
    stepWorld(hopalong.auto ? bot() : pad);
  } else if (mode === "pause") {
    if (pad.menu || pad.jumpPressed) mode = "play";
  } else if (mode === "dead") {
    hero.vy -= G_DOWN * DT; hero.y += hero.vy * DT;
    stepWorld(pad);
    if (modeT > 2.2) {
      if (--lives <= 0) { mode = "over"; modeT = 0; }
      else { loadLevel(levelIndex); mode = "intro"; modeT = 0; }
    }
  } else if (mode === "clear") {
    hero.vx = WALK; hero.face = 1;                // strolls off through the gate
    hero.vy -= G_DOWN * DT;
    moveX(hero, hero.vx * DT);
    if (moveY(hero, hero.vy * DT) === 1) { hero.vy = 0; hero.ground = true; }
    hero.stride += hero.vx * DT * 2.6;
    stepWorld(pad);
    if (modeT > 4) {
      checkpoint = null;
      if (levelIndex + 1 < LEVELS.length) { loadLevel(levelIndex + 1); mode = "intro"; modeT = 0; }
      else { mode = "won"; modeT = 0; }
    }
  } else if (mode === "over" || mode === "won") {
    if (modeT > (mode === "won" ? 9 : 4) || (modeT > 1.5 && pad.jumpPressed)) enterTitle();
  }
}

// ——— drawing: everything is a triangle with a depth ———

function tri(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) {
  if (!batch) { triangle3d(x1, y1, z1, x2, y2, z2, x3, y3, z3, r > 255 ? 255 : r | 0, g > 255 ? 255 : g | 0, b > 255 ? 255 : b | 0); n++; return; }
  if (n >= MAX_FACES) return;
  const o = n * 12;
  buf[o] = x1; buf[o + 1] = y1; buf[o + 2] = z1; buf[o + 3] = x2; buf[o + 4] = y2; buf[o + 5] = z2;
  buf[o + 6] = x3; buf[o + 7] = y3; buf[o + 8] = z3;
  buf[o + 9] = r > 255 ? 255 : r; buf[o + 10] = g > 255 ? 255 : g; buf[o + 11] = b > 255 ? 255 : b;   // a lit face over 255 would wrap
  n++;
}
const zOf = (zv) => { const z = -1.25 + (zv - 18) * .026; return z > 1.36 ? 1.36 : z; };
let ox = 0, oy = 0;                               // the camera, with its shake
// An upright rectangle facing the camera at depth z.
function wall(x0, y0, x1, y1, z, r, g, b) {
  const zv = DIST + z, k = F / zv, d = zOf(zv);
  const a = W * .5 + (x0 - ox) * k, c = W * .5 + (x1 - ox) * k, t = H * .5 - (y1 - oy) * k, u = H * .5 - (y0 - oy) * k;
  if (c < 0 || a > W || u < 0 || t > H) return;
  tri(a, t, d, c, t, d, c, u, d, r, g, b); tri(a, t, d, c, u, d, a, u, d, r, g, b);
}
// A level rectangle at height y, from z0 back to z1.
function deck(x0, x1, y, z0, z1, r, g, b) {
  const k0 = F / (DIST + z0), k1 = F / (DIST + z1), d0 = zOf(DIST + z0), d1 = zOf(DIST + z1);
  const a = W * .5 + (x0 - ox) * k0, c = W * .5 + (x1 - ox) * k0, e = W * .5 + (x1 - ox) * k1, f = W * .5 + (x0 - ox) * k1;
  const v0 = H * .5 - (y - oy) * k0, v1 = H * .5 - (y - oy) * k1;
  tri(a, v0, d0, c, v0, d0, e, v1, d1, r, g, b); tri(a, v0, d0, e, v1, d1, f, v1, d1, r, g, b);
}
// A rectangle edge-on to the play plane at x.
function flank(x, y0, y1, z0, z1, r, g, b) {
  const k0 = F / (DIST + z0), k1 = F / (DIST + z1), d0 = zOf(DIST + z0), d1 = zOf(DIST + z1);
  const a = W * .5 + (x - ox) * k0, c = W * .5 + (x - ox) * k1;
  const t0 = H * .5 - (y1 - oy) * k0, u0 = H * .5 - (y0 - oy) * k0, t1 = H * .5 - (y1 - oy) * k1, u1 = H * .5 - (y0 - oy) * k1;
  tri(a, t0, d0, c, t1, d1, c, u1, d1, r, g, b); tri(a, t0, d0, c, u1, d1, a, u0, d0, r, g, b);
}
// A box: its face, and whichever of its other sides the camera can see.
function box(x0, y0, z0, x1, y1, z1, r, g, b) {
  wall(x0, y0, x1, y1, z0, r, g, b);
  if (oy > y1) deck(x0, x1, y1, z0, z1, r * 1.16, g * 1.16, b * 1.16);
  else if (oy < y0) deck(x0, x1, y0, z0, z1, r * .58, g * .58, b * .58);
  if (ox < x0) flank(x0, y0, y1, z0, z1, r * .76, g * .76, b * .76);
  else if (ox > x1) flank(x1, y0, y1, z0, z1, r * .76, g * .76, b * .76);
}
// Any triangle in the play plane's own coordinates.
function shard(x1, y1, x2, y2, x3, y3, z, r, g, b) {
  const zv = DIST + z, k = F / zv, d = zOf(zv);
  tri(W * .5 + (x1 - ox) * k, H * .5 - (y1 - oy) * k, d, W * .5 + (x2 - ox) * k, H * .5 - (y2 - oy) * k, d,
    W * .5 + (x3 - ox) * k, H * .5 - (y3 - oy) * k, d, r, g, b);
}
function pane(x0, y0, x1, y1, z, r, g, b) {       // screen space
  tri(x0, y0, z, x1, y0, z, x1, y1, z, r, g, b); tri(x0, y0, z, x1, y1, z, x0, y1, z, r, g, b);
}

// The ground never changes, so each column's faces are worked out once:
// [kind, x, y, r, g, b, back] with kind 0 face, 1 top, 2 underside, 3 left, 4 right,
// 5 the grass lip, 6 the face below it.
function bake() {
  const T = L.theme;
  columns = [];
  const still = (x, y) => x >= 0 && x < L.cols && y >= 0 && isStatic(tileAt(x, y));
  for (let x = 0; x < L.cols; x++) {
    const col = [];
    for (let y = 0; y < ROWS; y++) {
      const t = tileAt(x, y);
      if (!isStatic(t)) continue;
      const lit = ((x + y) & 1 ? 1 : .94), back = t === GROUND ? 3 : .5;
      let c = t === GROUND ? T.dirt : t === STONE ? T.stone : T.trunk, cap = c;
      if (t === GROUND) {
        let depth = 0;
        while (depth < 3 && tileAt(x, y + depth + 1) === GROUND) depth++;
        const k = lit * (1 - depth * .08);
        c = [c[0] * k, c[1] * k, c[2] * k];
        if (!depth && tileAt(x, y + 1) !== GROUND) { cap = T.grass; col.push(5, x, y, cap[0] * lit, cap[1] * lit, cap[2] * lit, back, 6, x, y, c[0], c[1], c[2], back); }
        else col.push(0, x, y, c[0], c[1], c[2], back);
      } else { c = [c[0] * lit, c[1] * lit, c[2] * lit]; cap = c; col.push(0, x, y, c[0], c[1], c[2], back); }
      if (!still(x, y + 1)) col.push(1, x, y, cap[0] * 1.14, cap[1] * 1.14, cap[2] * 1.14, back);
      if (y > 0 && !still(x, y - 1)) col.push(2, x, y, c[0] * .55, c[1] * .55, c[2] * .55, back);
      if (!still(x - 1, y) || x === 0) col.push(3, x, y, c[0] * .74, c[1] * .74, c[2] * .74, back);
      if (!still(x + 1, y)) col.push(4, x, y, c[0] * .74, c[1] * .74, c[2] * .74, back);
    }
    columns.push(col);
  }
}

function drawSky() {
  const T = L.theme, a = T.sky[0], b = T.sky[1], bands = 14;
  for (let i = 0; i < bands; i++) {
    const k = i / (bands - 1);
    pane(0, H * .8 * i / bands, W, H * .8 * (i + 1) / bands + 1, 1.38,
      a[0] + (b[0] - a[0]) * k, a[1] + (b[1] - a[1]) * k, a[2] + (b[2] - a[2]) * k);
  }
  const sx = W * .78 - camX * 1.5, sy = H * .2, rad = H * .075;
  for (let i = 0; i < 14; i++) {
    const p = i / 14 * TAU, q = (i + 1) / 14 * TAU;
    tri(sx, sy, 1.375, sx + Math.cos(p) * rad, sy + Math.sin(p) * rad, 1.375, sx + Math.cos(q) * rad, sy + Math.sin(q) * rad, 1.375, T.sun[0], T.sun[1], T.sun[2]);
  }
}
// A rolling ridge at depth z; perspective alone makes it drift slower than the ground.
function drawRidge(z, base, amp, freq, step, c) {
  const zv = DIST + z, k = F / zv, d = zOf(zv), half = W * .5 / k;
  const height = (x) => base + Math.sin(x * freq) * amp + Math.sin(x * freq * 2.7 + 1.3) * amp * .35;
  const from = Math.floor((ox - half) / step) * step;
  let px = W * .5 + (from - ox) * k, py = H * .5 - (height(from) - oy) * k;
  for (let x = from + step; x <= ox + half + step; x += step) {
    const qx = W * .5 + (x - ox) * k, qy = H * .5 - (height(x) - oy) * k;
    tri(px, py, d, qx, qy, d, qx, H + 2, d, c[0], c[1], c[2]); tri(px, py, d, qx, H + 2, d, px, H + 2, d, c[0], c[1], c[2]);
    px = qx; py = qy;
  }
}
function drawClouds() {
  const c = L.theme.cloud;
  for (const z of [16, 34]) {
    const half = W * .5 / (F / (DIST + z)), gap = 13;
    for (let i = Math.floor((ox - half - 6) / gap); i <= Math.floor((ox + half + 6) / gap); i++) {
      const s = Math.sin(i * 12.9898 + z) * 43758.5453, r = s - Math.floor(s);
      const x = i * gap + r * 7 + tick * .004 * (40 - z) / 10, y = 13 + r * 9 + z * .12, w = 2.4 + r * 2;
      wall(x - w, y, x + w, y + .9, z, c[0], c[1], c[2]);
      wall(x - w * .55, y + .8, x + w * .5, y + 1.7, z, c[0], c[1], c[2]);
      wall(x - w * .2, y + 1.6, x + w * .25, y + 2.2, z, c[0], c[1], c[2]);
    }
  }
}

function drawTiles() {
  const T = L.theme, half = W * .5 / (F / (DIST - .5)) + 1;
  const from = Math.max(0, Math.floor(ox - half)), to = Math.min(L.cols - 1, Math.floor(ox + half));
  for (let x = from; x <= to; x++) {
    const col = columns[x];
    for (let i = 0; i < col.length; i += 7) {
      const kind = col[i], y = col[i + 2], r = col[i + 3], g = col[i + 4], b = col[i + 5], back = col[i + 6];
      if (kind === 0) wall(x, y, x + 1, y + 1, -.5, r, g, b);
      else if (kind === 5) wall(x, y + .7, x + 1, y + 1, -.5, r, g, b);
      else if (kind === 6) wall(x, y, x + 1, y + .7, -.5, r, g, b);
      else if (kind === 1) { if (oy > y + 1) deck(x, x + 1, y + 1, -.5, back, r, g, b); }
      else if (kind === 2) { if (oy < y) deck(x, x + 1, y, -.5, back, r, g, b); }
      else if (kind === 3) { if (ox < x) flank(x, y, y + 1, -.5, back, r, g, b); }
      else if (ox > x + 1) flank(x + 1, y, y + 1, -.5, back, r, g, b);
    }
    for (let y = 0; y < ROWS; y++) {
      const t = L.tiles[y * L.cols + x];
      if (t < BRICK) continue;
      let lift = 0;
      for (const bp of bumps) if (bp.cx === x && bp.cy === y) lift = Math.sin(bp.t / .2 * Math.PI) * .3;
      const y0 = y + (t === SPRING ? 0 : lift);
      if (t === BRICK) {
        box(x, y0, -.5, x + 1, y0 + 1, .5, 200, 112, 66);
        wall(x, y0 + .46, x + 1, y0 + .54, -.51, 128, 66, 44);
        wall(x + .46, y0 + .54, x + .54, y0 + 1, -.51, 128, 66, 44);
        wall(x + .2, y0, x + .28, y0 + .46, -.51, 128, 66, 44); wall(x + .72, y0, x + .8, y0 + .46, -.51, 128, 66, 44);
      } else if (t === PRIZE || t === BERRYBOX) {
        const glow = 228 + Math.sin(tick * .1) * 26;
        box(x, y0, -.5, x + 1, y0 + 1, .5, 250, 196, 44);
        wall(x + .14, y0 + .14, x + .86, y0 + .86, -.51, 255, glow, 120);
        const m = t === BERRYBOX ? [236, 78, 120] : [150, 96, 20];
        shard(x + .5, y0 + .76, x + .74, y0 + .5, x + .5, y0 + .24, -.52, m[0], m[1], m[2]);
        shard(x + .5, y0 + .76, x + .5, y0 + .24, x + .26, y0 + .5, -.52, m[0], m[1], m[2]);
      } else if (t === USED) {
        box(x, y0, -.5, x + 1, y0 + 1, .5, 150, 128, 112);
        wall(x + .14, y0 + .14, x + .86, y0 + .86, -.51, 128, 108, 96);
      } else if (t === PLANK) {
        box(x, y + .72, -.45, x + 1, y + 1, .45, T.plank[0], T.plank[1], T.plank[2]);
      } else if (t === SPIKE) {
        for (let i = 0; i < 3; i++) {
          const a = x + i / 3;
          shard(a, y, a + 1 / 3, y, a + 1 / 6, y + .62, -.2, 226, 230, 240);
          shard(a + 1 / 6, y, a + 1 / 3, y, a + 1 / 6, y + .62, -.21, 156, 162, 184);
        }
      } else if (t === SPRING) {
        const press = lift * 1.2;
        box(x + .1, y, -.4, x + .9, y + .3, .4, 90, 96, 120);
        box(x + .42, y + .3, -.1, x + .58, y + .72 - press, .1, 200, 204, 220);
        box(x, y + .72 - press, -.45, x + 1, y + 1 - press, .45, 240, 84, 96);
      } else if (t === GEM) {
        const s = Math.cos(tick * .09 + x * .7) * .3, cy = y + .5 + Math.sin(tick * .06 + x) * .06;
        const lit = s > 0 ? 1 : .8;
        shard(x + .5 - s, cy, x + .5, cy + .4, x + .5 + s, cy, -.05, 255 * lit, 214 * lit, 60 * lit);
        shard(x + .5 - s, cy, x + .5 + s, cy, x + .5, cy - .4, -.05, 236 * lit, 170 * lit, 36 * lit);
      } else if (t === MOON) {
        const s = Math.cos(tick * .05) * .62, cy = y + .55, lit = Math.abs(s) * .25 + .75;
        for (let i = 0; i < 10; i++) {
          const p = i / 10 * TAU, q = (i + 1) / 10 * TAU;
          shard(x + .5, cy, x + .5 + Math.cos(p) * s, cy + Math.sin(p) * .62, x + .5 + Math.cos(q) * s, cy + Math.sin(q) * .62, -.05, 190 * lit, 236 * lit, 255 * lit);
        }
        wall(x + .5 - Math.abs(s) * .3, cy - .12, x + .5 + Math.abs(s) * .3, cy + .12, -.07, 255, 255, 255);
      } else if (t === FLAG) {
        const on = checkpoint && checkpoint.level === levelIndex && checkpoint.x === x + .5;
        box(x + .44, y, -.06, x + .56, y + 2.4, .06, 236, 236, 244);
        const wave = Math.sin(tick * .12) * .08, c = on ? [255, 96, 168] : [150, 156, 176];
        shard(x + .56, y + 2.4, x + 1.5, y + 2 + wave, x + .56, y + 1.6, 0, c[0], c[1], c[2]);
      } else if (t === GOAL) {
        box(x - .1, y, -.6, x + .2, y + 5, -.3, 250, 250, 255); box(x - .1, y, .3, x + .2, y + 5, .6, 214, 214, 226);
        box(x - .2, y + 5, -.7, x + .3, y + 5.5, .7, 255, 206, 64);
        for (let i = 0; i < 4; i++) for (let j = 0; j < 2; j++) {
          const dark = (i + j + (tick >> 4)) & 1, wave = Math.sin(tick * .1 + i) * .06;
          wall(x + .2 + i * .55, y + 3.9 + j * .55 + wave, x + .75 + i * .55, y + 4.45 + j * .55 + wave, 0, dark ? 40 : 250, dark ? 44 : 250, dark ? 60 : 250);
        }
      }
    }
  }
}

function drawLifts() {
  const p = L.theme.plank;
  for (const l of lifts) {
    box(l.x, l.top - .35, -.5, l.x + l.w, l.top, .5, p[0] * .9, p[1] * .9, p[2] * .9);
    wall(l.x + .1, l.top - .12, l.x + l.w - .1, l.top - .05, -.51, 255, 255, 255);
    box(l.x + l.w / 2 - .25, l.top - .6, -.2, l.x + l.w / 2 + .25, l.top - .35, .2, 96, 100, 128);
  }
}

// A dark patch on whatever is underfoot ties a jumper to the ground.
function drawShadow(x, y, w) {
  for (let cy = Math.floor(y + .01) - 1; cy >= Math.max(0, Math.floor(y) - 6); cy--) {
    const t = tileAt(Math.floor(x), cy);
    if (isSolid(t) || t === PLANK) {
      const s = w * Math.max(.4, 1 - (y - cy - 1) * .12);
      if (oy > cy + 1) deck(x - s, x + s, cy + 1.012, -.28, .28, 30, 40, 60);
      return;
    }
  }
}

function drawHero() {
  const h = hero;
  if (h.inv > 0 && (tick & 3) < 2) return;
  const s = h.big ? 1.5 : 1, f = h.face, air = !h.ground;
  const st = mode === "dead" ? 0 : h.pound ? -.2 : air ? clamp(h.vy * .012, -.16, .2) : 0;
  const sx = s * (1 - st * .6), sy = s * (1 + st);
  const P = (x0, y0, x1, y1, zh, r, g, b) => {
    const a = h.x + f * x0 * sx, c = h.x + f * x1 * sx;
    box(a < c ? a : c, h.y + y0 * sy, -zh * s, a < c ? c : a, h.y + y1 * sy, zh * s, r, g, b);
  };
  const moving = h.ground && Math.abs(h.vx) > .5, swing = moving ? Math.sin(h.stride) : 0;
  const sw = air ? .12 : swing * .17, up = air ? .08 : 0;
  const lean = clamp(Math.abs(h.vx) * .014 + (air ? -h.vy * .006 : 0), -.1, .2);
  drawShadow(h.x, h.y, .34 * s);
  P(-.2 + sw, up + Math.max(0, -swing) * .07, .04 + sw, .15 + up, .21, 240, 150, 56);
  P(-.04 - sw, up + Math.max(0, swing) * .07, .2 - sw, .15 + up, .15, 214, 124, 40);
  P(-.26, .13, .26, .5, .22, 248, 240, 222);                       // body
  P(-.4, .18, -.24, .34, .1, 255, 255, 255);                       // tail
  const wave = Math.sin(tick * .3) * .04 + lean * .6;
  P(-.56 - wave, .4 + wave, -.26, .5 + wave, .04, 255, 190, 40);   // scarf's end
  P(-.29, .43, .29, .54, .245, 255, 170, 30);
  P(-.3, .5, .3, .9, .24, 248, 240, 222);                          // head
  P(-.22 - lean, .86, -.08 - lean * 1.6, 1.32, .07, 248, 240, 222); // ears
  P(.0 - lean, .86, .14 - lean * 1.6, 1.24, .07, 232, 222, 204);
  P(-.185 - lean * 1.1, .94, -.115 - lean * 1.5, 1.24, .075, 255, 160, 180);
  if (mode === "dead") { P(.07, .7, .23, .74, .25, 40, 40, 60); P(.13, .64, .17, .8, .25, 40, 40, 60); }
  else P(.1, .64, .19, h.pound ? .7 : .8, .25, 40, 40, 60);        // eye
  P(.27, .58, .34, .65, .1, 255, 130, 160);                        // nose
  P(.02, .55, .12, .6, .245, 255, 188, 196);                       // cheek
}

function drawFoe(f) {
  if (f.dead === 3 || (f.dead === 1 && f.t > .45) || Math.abs(f.x - ox) > 22) return;
  const flat = f.dead === 1 ? .25 : 1, flip = f.dead === 2 ? -1 : 1, d = f.dir, w = f.w / 2;
  const P = (x0, y0, x1, y1, zh, r, g, b) => {
    const a = f.x + d * x0, c = f.x + d * x1, p = f.y + (flip < 0 ? f.h - y1 : y0) * flat, q = f.y + (flip < 0 ? f.h - y0 : y1) * flat;
    box(a < c ? a : c, p, -zh, a < c ? c : a, q, zh, r, g, b);
  };
  const step = f.dead ? 0 : Math.sin(tick * .3 + f.hx) * .08;
  if (!f.dead) drawShadow(f.x, f.y, w * .8);
  if (f.kind === "grub") {
    P(-w + .08 + step, 0, -.06 + step, .14, .2, 70, 40, 90); P(.06 - step, 0, w - .08 - step, .14, .2, 70, 40, 90);
    P(-w, .1, w, f.h, .26, 160, 88, 186);
    P(.06, .3, .3, .5, .27, 255, 255, 255); P(.2, .32, .3, .44, .28, 30, 20, 40);
    P(.0, .5, .34, .55, .28, 90, 40, 110);
  } else if (f.kind === "hopper") {
    const sy = f.ground ? 1 - Math.max(0, f.t - .6) * .5 : 1.15;
    P(-w, 0, w, f.h * sy, .26, 96, 198, 104);
    P(-w - .08, 0, -w + .2, .12, .3, 60, 150, 76); P(w - .2, 0, w + .08, .12, .3, 60, 150, 76);
    P(.08, f.h * sy - .06, .32, f.h * sy + .18, .2, 96, 198, 104);
    P(.16, f.h * sy, .28, f.h * sy + .13, .21, 30, 30, 40);
    P(-.1, .12, w, .18, .27, 60, 130, 70);
  } else if (f.kind === "burr") {
    P(-w, 0, w, f.h - .12, .26, 150, 50, 62);
    P(.1, .16, .32, .34, .27, 255, 236, 200); P(.22, .18, .32, .28, .28, 40, 20, 20);
    for (let i = 0; i < 4; i++) {
      const a = f.x - w + i * f.w / 4;
      shard(a, f.y + (f.h - .12) * flat, a + f.w / 4, f.y + (f.h - .12) * flat, a + f.w / 8, f.y + (f.h + .26) * flat, -.1, 255, 240, 214);
    }
  } else {
    const flap = Math.sin(tick * .5 + f.hx) * .3;
    P(-w + .08, 0, w - .08, f.h, .2, 110, 190, 255);
    P(.1, .22, .26, .38, .21, 255, 255, 255); P(.18, .24, .26, .34, .22, 30, 40, 70);
    P(w - .1, .14, w + .1, .24, .1, 255, 180, 60);
    shard(f.x - .1, f.y + .4, f.x + .2, f.y + .4, f.x - d * .1, f.y + .5 + flap + .35, -.22, 235, 246, 255);
  }
}

function drawWorld() {
  const jolt = shake > 0 ? shake * 1.2 : 0;
  ox = camX + Math.sin(tick * 1.9) * jolt; oy = camY + Math.cos(tick * 2.3) * jolt;
  const T = L.theme;
  drawSky();
  drawRidge(70, 4, 6, .05, 6, T.hills[0]);
  drawRidge(34, 1, 4, .09, 4, T.hills[1]);
  drawClouds();
  drawTiles();
  drawLifts();
  for (const it of items) {                        // a berry, bobbing on its box
    const y = it.y + Math.min(1, it.t * 5) * .1 + Math.sin(it.t * 5) * .05;
    box(it.x - .3, y, -.3, it.x + .3, y + .6, .3, 236, 70, 118);
    wall(it.x - .16, y + .36, it.x - .04, y + .48, -.31, 255, 200, 214);
    box(it.x - .05, y + .6, -.05, it.x + .2, y + .76, .05, 90, 190, 96);
  }
  for (const f of foes) drawFoe(f);
  drawHero();
  for (const p of parts) wall(p.x - p.s / 2, p.y - p.s / 2, p.x + p.s / 2, p.y + p.s / 2, -.6, p.r, p.g, p.b);
}

// ——— the HUD: a 5 × 7 block face, also triangles, nearest of all ———

const GLYPHS = {
  A: "0E11111F111111", B: "1E11111E11111E", C: "0E11101010110E", D: "1E11111111111E", E: "1F10101E10101F",
  F: "1F10101E101010", G: "0E11101711110F", H: "1111111F111111", I: "0E04040404040E", J: "0702020202120C",
  K: "11121418141211", L: "1010101010101F", M: "111B1515111111", N: "11111915131111", O: "0E11111111110E",
  P: "1E11111E101010", Q: "0E11111115120D", R: "1E11111E141211", S: "0F10100E01011E", T: "1F040404040404",
  U: "1111111111110E", V: "11111111110A04", W: "1111111515150A", X: "11110A040A1111", Y: "11110A04040404",
  Z: "1F01020408101F", 0: "0E11131519110E", 1: "040C040404040E", 2: "0E11010204081F", 3: "1F02040201110E",
  4: "02060A121F0202", 5: "1F101E0101110E", 6: "0608101E11110E", 7: "1F010204080808", 8: "0E11110E11110E",
  9: "0E11110F01020C", "-": "0000001F000000", ".": "00000000000C0C", "!": "04040404040004", ":": "000C0C000C0C00",
  "+": "0004041F040400",
};
const runsOf = {};                                // each glyph's lit runs, worked out on first use
function glyphRuns(ch) {
  if (runsOf[ch]) return runsOf[ch];
  const out = [], g = GLYPHS[ch];
  if (g) for (let row = 0; row < 7; row++) {
    const bits = parseInt(g.substr(row * 2, 2), 16);
    let from = -1;
    for (let c = 0; c <= 5; c++) {
      const on = c < 5 && (bits >> (4 - c)) & 1;
      if (on && from < 0) from = c;
      if (!on && from >= 0) { out.push(from, row, c); from = -1; }
    }
  }
  return (runsOf[ch] = out);
}
const textWidth = (s, px) => s.length * 6 * px - px;
function text(s, x, y, px, r, g, b, shadow = true) {
  for (let i = 0; i < s.length; i++) {
    const runs = glyphRuns(s[i]), cx = x + i * 6 * px;
    for (let k = 0; k < runs.length; k += 3) {
      const x0 = cx + runs[k] * px, y0 = y + runs[k + 1] * px, x1 = cx + runs[k + 2] * px;
      if (shadow) pane(x0 + px * .6, y0 + px * .6, x1 + px * .6, y0 + px * 1.6, -1.485, 24, 28, 48);
      pane(x0, y0, x1, y0 + px, -1.49, r, g, b);
    }
  }
}
const centred = (s, y, px, r, g, b) => text(s, (W - textWidth(s, px)) / 2, y, px, r, g, b);
const pad3 = (v) => String(Math.max(0, Math.floor(v))).padStart(3, "0");

function drawHud() {
  const s = H / 1080, px = 5 * s, top = 44 * s, left = 64 * s;
  if (mode !== "title") {
    // lives: a small head with ears
    pane(left, top + 8 * s, left + 30 * s, top + 34 * s, -1.49, 248, 240, 222);
    pane(left + 3 * s, top - 8 * s, left + 10 * s, top + 10 * s, -1.49, 248, 240, 222);
    pane(left + 15 * s, top - 4 * s, left + 22 * s, top + 10 * s, -1.49, 232, 222, 204);
    pane(left + 20 * s, top + 16 * s, left + 25 * s, top + 24 * s, -1.492, 40, 40, 60);
    text("X" + lives, left + 44 * s, top, px, 255, 255, 255);
    const gx = left + 190 * s, gy = top + 17 * s;
    tri(gx - 13 * s, gy, -1.49, gx, gy - 19 * s, -1.49, gx + 13 * s, gy, -1.49, 255, 214, 60);
    tri(gx - 13 * s, gy, -1.49, gx + 13 * s, gy, -1.49, gx, gy + 19 * s, -1.49, 236, 170, 36);
    text(pad3(gems), gx + 26 * s, top, px, 255, 255, 255);
    for (let i = 0; i < L.moons; i++) {
      const mx = left + 360 * s + i * 44 * s, got = i < moonsGot;
      pane(mx, top, mx + 34 * s, top + 34 * s, -1.487, 24, 28, 48);
      pane(mx + 4 * s, top + 4 * s, mx + 30 * s, top + 30 * s, -1.49, got ? 190 : 70, got ? 236 : 80, got ? 255 : 110);
    }
    centred(L.name, top, px, 255, 255, 255);
    const t = "TIME " + pad3(Math.ceil(timeLeft)), low = timeLeft < 60 && (tick & 31) < 16;
    text(t, W - left - textWidth(t, px), top, px, 255, low ? 110 : 255, low ? 110 : 255);
    const sc = String(score).padStart(6, "0");
    text(sc, W - left - textWidth(sc, px * .8), top + 50 * s, px * .8, 255, 232, 150);
  }
  const band = (y, h, r, g, b) => pane(0, y, W, y + h, -1.48, r, g, b);
  if (mode === "title") {
    const word = "HOPALONG", big = 26 * s, x0 = (W - textWidth(word, big)) / 2;
    const ink = [[255, 96, 120], [255, 170, 30], [255, 226, 80], [110, 220, 120], [90, 190, 255], [170, 130, 255], [255, 130, 200], [255, 255, 255]];
    for (let i = 0; i < word.length; i++)
      text(word[i], x0 + i * 6 * big, H * .2 + Math.sin(tick * .06 + i * .7) * 12 * s, big, ink[i][0], ink[i][1], ink[i][2]);
    if ((tick & 63) < 44) centred("PRESS A", H * .46, 9 * s, 255, 255, 255);
    centred("A JUMP   X RUN   DOWN POUND", H - 96 * s, 4 * s, 255, 255, 255);
  } else if (mode === "intro") {
    band(H * .38, H * .24, 24, 28, 48);
    centred("STAGE " + (levelIndex + 1), H * .42, 7 * s, 255, 214, 60, false);
    centred(L.name, H * .5, 11 * s, 255, 255, 255, false);
  } else if (mode === "pause") {
    band(H * .42, H * .16, 24, 28, 48);
    centred("PAUSED", H * .46, 12 * s, 255, 255, 255, false);
  } else if (mode === "clear") {
    centred("STAGE CLEAR!", H * .3, 14 * s, 255, 226, 80);
    if (modeT > 1) centred("TIME BONUS " + Math.ceil(timeLeft) * 10, H * .45, 6 * s, 255, 255, 255);
    if (modeT > 1.7) centred("MOONS " + moonsGot + " - " + L.moons, H * .52, 6 * s, 190, 236, 255);
  } else if (mode === "over") {
    band(0, H, 24, 28, 48);
    centred("GAME OVER", H * .42, 16 * s, 255, 96, 120, false);
  } else if (mode === "won") {
    band(H * .26, H * .46, 24, 28, 48);
    centred("YOU HOPPED IT ALL!", H * .32, 11 * s, 255, 226, 80, false);
    centred("SCORE " + score, H * .48, 9 * s, 255, 255, 255, false);
    centred("THANKS FOR PLAYING", H * .6, 6 * s, 190, 236, 255, false);
  }
  if (meter) text(fps + " FPS " + jsMs + " MS " + lastTris + " TRI", left, H - 60 * s, 3 * s, 255, 255, 255);
}

// ——— lifecycle ———

function boot() {
  batch = typeof triangles3d === "function";
  if (batch) buf = new Float32Array(MAX_FACES * 12);
  enterTitle();
  booted = true;
  if (typeof telemetry === "function") telemetry("HOPALONG_BOOT", JSON.stringify({ levels: LEVELS.length, batch }));
}

function paint() {
  if (!booted) return;
  const t0 = Date.now();
  let view = null;
  try { view = typeof runtime === "function" ? runtime() : null; } catch (_) { view = null; }
  if (view && view.width) { W = view.width; H = view.height; }
  F = H * DIST / VIEW;
  n = 0;
  const horizon = L.theme.sky[1];
  wipe(horizon[0], horizon[1], horizon[2]);
  if (mode !== "over") drawWorld();
  drawHud();
  if (batch && n) triangles3d(buf, n);
  lastTris = n; jsMs = Date.now() - t0;
  frames++; fpsFrames++;
  if (t0 - fpsAt >= 1000) { fps = fpsFrames; fpsFrames = 0; fpsAt = t0; }
  if (frames % 300 === 1 && typeof telemetry === "function")
    telemetry("HOPALONG", JSON.stringify({ fps, jsMs, tris: n, mode, level: levelIndex, w: W, h: H }));
}

function act() {}
function leave() {}

// A handle for the shells and the tests: stage a level, hand the pad to the
// autopilot, or script the pad outright.
const hopalong = {
  script: null, auto: false,
  start(level = 0, auto = false) { startGame(level); hopalong.auto = auto; mode = "play"; },
  get state() {
    return { mode, level: levelIndex, x: hero.x, y: hero.y, big: hero.big, ground: hero.ground, lives, gems, score,
      deaths, moons: moonsGot, time: timeLeft, tris: lastTris, cols: L ? L.cols : 0, goal: L ? L.goal : 0 };
  },
};
