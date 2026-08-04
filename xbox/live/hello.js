const floorY = 780;
const arenaLeft = 115;
const arenaRight = 1805;
const players = [
  { name: "OSKIE", pad: 0, x: 520, y: floorY, vx: 0, vy: 0, facing: 1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 232, 92], hit: 0 },
  { name: "JEFFREY", pad: 1, x: 1400, y: floorY, vx: 0, vy: 0, facing: -1,
    grounded: true, ducking: false, previous: [], lastButton: "NONE",
    lastButtonAt: -10000000, color: [255, 105, 190], hit: 0 },
];
const bullets = [];
let startedAt = 0;
let lastSimAt = 0;

const buttonLabel = (button) => ({
  ArrowUp: "UP", ArrowDown: "DOWN", ArrowLeft: "LEFT", ArrowRight: "RIGHT",
  LeftShoulder: "LB", RightShoulder: "RB", LeftStick: "LEFT STICK",
  RightStick: "RIGHT STICK", View: "VIEW", Menu: "MENU",
}[button] || String(button).toUpperCase());

function boot() {
  startedAt = runtime().monotonicUs;
  lastSimAt = startedAt;
}

function quantizedInput(pad) {
  const held = pad.down;
  let horizontal = (held.includes("ArrowRight") ? 1 : 0) -
    (held.includes("ArrowLeft") ? 1 : 0);
  let vertical = (held.includes("ArrowUp") ? 1 : 0) -
    (held.includes("ArrowDown") ? 1 : 0);
  if (!horizontal && Math.abs(pad.leftX) >= 0.48) horizontal = pad.leftX > 0 ? 1 : -1;
  if (!vertical && Math.abs(pad.leftY) >= 0.48) vertical = pad.leftY > 0 ? 1 : -1;
  return { horizontal, vertical };
}

function remember(player, button) {
  player.lastButton = buttonLabel(button);
  player.lastButtonAt = runtime().monotonicUs;
  telemetry("FIGHT_BUTTON", player.name + " " + player.lastButton);
}

function playButtonDrum(button, player) {
  const pan = player.pad === 0 ? -0.4 : 0.4;
  if (button === "A") drum("kick", 1.05, pan);
  else if (button === "B") drum("snare", 0.95, pan);
  else if (button === "X") drum("hat", 0.9, pan);
  else if (button === "Y") drum("clap", 0.95, pan);
  else if (!button.startsWith("Arrow")) drum("block", 0.75, pan);
}

function fire(player) {
  bullets.push({
    x: player.x + player.facing * 38,
    y: player.y - (player.ducking ? 45 : 78),
    vx: player.facing * 1050,
    owner: player.pad,
    life: 1.6,
  });
  while (bullets.length > 24) bullets.shift();
}

function updatePlayer(player, pad, dt) {
  const input = quantizedInput(pad);
  const upPressed = input.vertical > 0 && !player.previous.includes("MOVE_UP");
  player.ducking = input.vertical < 0 && player.grounded;

  if (input.horizontal) player.facing = input.horizontal;
  if (player.ducking) {
    player.vx *= Math.exp(-12 * dt);
  } else {
    player.vx += input.horizontal * 1900 * dt;
    if (!input.horizontal) player.vx *= Math.exp(-7.5 * dt);
  }
  player.vx = Math.max(-560, Math.min(560, player.vx));

  if (upPressed && player.grounded) {
    player.vy = -850;
    player.grounded = false;
    player.ducking = false;
    drum("block", 0.72, player.pad === 0 ? -0.4 : 0.4);
  }

  for (const button of pad.down) {
    if (!player.previous.includes(button)) {
      remember(player, button);
      playButtonDrum(button, player);
      if (button === "A") fire(player);
    }
  }

  player.vy += 1900 * dt;
  player.x += player.vx * dt;
  player.y += player.vy * dt;
  if (player.x < arenaLeft + 20) {
    player.x = arenaLeft + 20;
    player.vx = Math.abs(player.vx) * 0.28;
  } else if (player.x > arenaRight - 20) {
    player.x = arenaRight - 20;
    player.vx = -Math.abs(player.vx) * 0.28;
  }
  if (player.y >= floorY) {
    player.y = floorY;
    player.vy = 0;
    player.grounded = true;
  }
  player.hit = Math.max(0, player.hit - dt * 4);
  player.previous = pad.down.slice();
  if (input.vertical > 0) player.previous.push("MOVE_UP");
}

function sim() {
  const now = runtime().monotonicUs;
  const dt = Math.min(0.04, Math.max(0.001, (now - lastSimAt) / 1000000));
  lastSimAt = now;
  updatePlayer(players[0], gamepad(0), dt);
  updatePlayer(players[1], gamepad(1), dt);

  for (const bullet of bullets) {
    bullet.x += bullet.vx * dt;
    bullet.life -= dt;
    const target = players[bullet.owner === 0 ? 1 : 0];
    const targetTop = target.y - (target.ducking ? 70 : 140);
    if (bullet.life > 0 && Math.abs(bullet.x - target.x) < 28 &&
        bullet.y > targetTop && bullet.y < target.y + 8) {
      bullet.life = 0;
      target.vx += Math.sign(bullet.vx) * 360;
      target.vy = -230;
      target.grounded = false;
      target.hit = 1;
      drum("snare", 1.15, target.pad === 0 ? -0.4 : 0.4);
    }
    if (bullet.x < arenaLeft || bullet.x > arenaRight) bullet.life = 0;
  }
  while (bullets.length && bullets[0].life <= 0) bullets.shift();
}

function circle(x, y, radius, width, color) {
  let lastX = x + radius;
  let lastY = y;
  for (let i = 1; i <= 12; i++) {
    const angle = i * Math.PI * 2 / 12;
    const nextX = x + Math.cos(angle) * radius;
    const nextY = y + Math.sin(angle) * radius;
    line(lastX, lastY, nextX, nextY, width, ...color);
    lastX = nextX;
    lastY = nextY;
  }
}

function drawRunner(player, t) {
  const speed = Math.min(1, Math.abs(player.vx) / 360);
  const stride = Math.sin(t * (7 + speed * 9) + player.pad * Math.PI) * 25 * speed;
  const height = player.ducking ? 76 : 132;
  const lean = player.facing * speed * 11;
  const x = player.x;
  const feet = player.y;
  const hipY = feet - (player.ducking ? 28 : 43);
  const neckX = x + lean;
  const neckY = feet - height + 40;
  const headY = feet - height + 16;
  const color = player.hit > 0 ? [255, 255, 255] : player.color;

  circle(neckX + lean * .25, headY, 17, 7, color);
  line(neckX, neckY, x, hipY, 8, ...color);
  if (player.ducking) {
    line(x, hipY, x - 29, feet - 17, 8, ...color);
    line(x - 29, feet - 17, x - 4, feet, 8, ...color);
    line(x, hipY, x + 29, feet - 14, 8, ...color);
    line(x + 29, feet - 14, x + 46, feet, 8, ...color);
  } else if (player.grounded) {
    line(x, hipY, x - 10 + stride * .45, feet - 20, 8, ...color);
    line(x - 10 + stride * .45, feet - 20, x + stride, feet, 8, ...color);
    line(x, hipY, x + 10 - stride * .45, feet - 20, 8, ...color);
    line(x + 10 - stride * .45, feet - 20, x - stride, feet, 8, ...color);
  } else {
    line(x, hipY, x - 25, feet - 24, 8, ...color);
    line(x - 25, feet - 24, x - 5, feet - 8, 8, ...color);
    line(x, hipY, x + 24, feet - 32, 8, ...color);
    line(x + 24, feet - 32, x + 38, feet - 14, 8, ...color);
  }
  const arm = player.grounded ? -stride * .72 : 22;
  line(neckX, neckY + 8, x - 19 + arm, feet - 67, 7, ...color);
  line(x - 19 + arm, feet - 67, x - 7 + arm, feet - 47, 7, ...color);
  line(neckX, neckY + 8, x + 19 - arm, feet - 67, 7, ...color);
  line(x + 19 - arm, feet - 67, x + 7 - arm, feet - 47, 7, ...color);

  const labelX = x - (player.name === "JEFFREY" ? 72 : 54);
  systemWrite(player.name, labelX, headY - 62, 34, ...color);
}

function drawPlayerHud(player, x, pad) {
  const age = (runtime().monotonicUs - player.lastButtonAt) / 1000000;
  const pulse = Math.max(0, 1 - age * 2.2);
  const color = player.color;
  box(x, 842, 790, 176, 14 + Math.round(pulse * 20), 18, 45 + Math.round(pulse * 35));
  systemWrite(player.name, x + 36, 862, 34, ...color);
  write(player.lastButton, x + 36, 920, 54 + Math.round(pulse * 10), ...color);
  const held = pad.down.length ? pad.down.map(buttonLabel).join(" ") : "NONE";
  write("HELD " + held, x + 360, 876, 24, 195, 210, 230);
  write(pad.connected ? "CONTROLLER " + (player.pad + 1) : "CONNECT CONTROLLER " + (player.pad + 1),
    x + 360, 936, 20, pad.connected ? 115 : 255, pad.connected ? 225 : 105,
    pad.connected ? 165 : 105);
}

function paint() {
  const run = runtime();
  const t = (run.monotonicUs - startedAt) / 1000000;
  const titleX = 500 + Math.sin(t * 1.05) * 135;
  const titleY = 60 + Math.sin(t * 2.1) * 12;
  wipe(7, 8, 28);
  for (let i = 0; i < 8; i++) {
    const x = ((i * 310 + t * (38 + i * 3)) % 2300) - 190;
    box(x, 0, 5, 1080, 20 + i * 3, 32 + i * 4, 72 + i * 6);
  }
  box(titleX - 28, titleY - 24, 890, 144, 22, 28, 104);
  write("HELLO OSKIE", titleX, titleY, 96, 245, 248, 255);

  box(95, 238, 1730, 562, 10, 13, 30);
  box(arenaLeft, floorY, arenaRight - arenaLeft, 20, 72, 90, 125);
  box(arenaLeft, 300, 20, floorY - 300, 72, 90, 125);
  box(arenaRight - 20, 300, 20, floorY - 300, 72, 90, 125);
  for (const bullet of bullets) {
    if (bullet.life <= 0) continue;
    const color = players[bullet.owner].color;
    line(bullet.x - Math.sign(bullet.vx) * 34, bullet.y, bullet.x, bullet.y, 8, ...color);
    box(bullet.x - 7, bullet.y - 7, 14, 14, ...color);
  }
  drawRunner(players[0], t);
  drawRunner(players[1], t);
  drawPlayerHud(players[0], 120, gamepad(0));
  drawPlayerHud(players[1], 1010, gamepad(1));
}

function act() {}
function leave() {}
