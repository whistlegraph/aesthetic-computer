// OSKIEWAR, 26.08.04
// Phone-sized live spectator view for a pronounceable OSKIEWAR match ID.

const MATCH_WORD = "[bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou][bdfgklmnprstvz][aeiou]";
const MATCH_NAME = new RegExp(`^${MATCH_WORD}-${MATCH_WORD}-${MATCH_WORD}$`);
const CAMERA_ASPECT = 1920 / 920;
const WORLD_FLOOR = 12000;

let matchId = null;
let matchName = "";
let ws = null;
let reconnectTimer = null;
let reconnectMs = 500;
let alive = false;
let status = "waiting for match";
let live = false;
let viewers = 0;
let snapshot = null;
let shown = null;
let lastFrameAt = 0;

const clone = (value) => JSON.parse(JSON.stringify(value));

function canonicalMatch(value) {
  const raw = String(value || "").toLowerCase().replace(/^#/, "");
  const name = raw.startsWith("ow-") ? raw.slice(3) : raw;
  return MATCH_NAME.test(name) ? { id: `ow-${name}`, name } : null;
}

function sessionUrl() {
  const local = location.hostname === "localhost" || location.hostname === "127.0.0.1";
  const base = local ? `ws://${location.hostname}:8889`
    : "wss://session-server.aesthetic.computer";
  return `${base}/oskiewar-live?match=${encodeURIComponent(matchId)}`;
}

function connect() {
  if (!alive || !matchId || typeof WebSocket === "undefined") return;
  status = "connecting";
  try { ws = new WebSocket(sessionUrl()); }
  catch { return scheduleReconnect(); }
  ws.onopen = () => { status = "waiting for console"; reconnectMs = 500; };
  ws.onmessage = ({ data }) => {
    let message;
    try { message = JSON.parse(data); } catch { return; }
    if (message.type === "oskiewar:status") {
      live = Boolean(message.content?.live);
      viewers = Number(message.content?.viewers) || 0;
      status = live ? "live" : message.content?.hasState ? "match paused" : "waiting for console";
    } else if (message.type === "oskiewar:state") {
      if (!message.content || message.content.seq <= (snapshot?.seq ?? -1)) return;
      snapshot = message.content;
      lastFrameAt = Date.now();
      live = true;
      status = "live";
      if (!shown) shown = clone(snapshot);
    } else if (message.type === "oskiewar:error") {
      status = message.content?.message || "spectator unavailable";
    }
  };
  ws.onclose = () => { ws = null; live = false; scheduleReconnect(); };
  ws.onerror = () => ws?.close();
}

function scheduleReconnect() {
  if (!alive || reconnectTimer) return;
  status = snapshot ? "reconnecting" : "waiting for match";
  reconnectTimer = setTimeout(() => {
    reconnectTimer = null;
    connect();
  }, reconnectMs);
  reconnectMs = Math.min(8000, reconnectMs * 2);
}

function boot({ params = [], colon = [], hash = "", wipe }) {
  const parsed = canonicalMatch(colon[0] || params[0] || hash);
  alive = true;
  matchId = parsed?.id || null;
  matchName = parsed?.name || "";
  status = parsed ? "connecting" : "use oskiewar:match-name";
  snapshot = null;
  shown = null;
  live = false;
  viewers = 0;
  wipe(12, 15, 24);
  if (matchId) connect();
}

function sim() {
  if (!snapshot) return;
  if (!shown) shown = clone(snapshot);
  for (let index = 0; index < 2; index++) {
    const target = snapshot.fighters[index];
    const display = shown.fighters[index];
    for (const key of ["x", "y", "z"])
      display[key] += (target[key] - display[key]) * .24;
    for (const key of ["name", "color", "facing", "alive", "grounded",
      "ducking", "blocking", "score", "roundWins", "attack"])
      display[key] = target[key];
  }
  const targetBalls = snapshot.balls || [snapshot.ball];
  if (!shown.balls) shown.balls = clone(targetBalls);
  while (shown.balls.length < targetBalls.length)
    shown.balls.push(clone(targetBalls[shown.balls.length]));
  for (let index = 0; index < targetBalls.length; index++) {
    for (const key of ["x", "y", "z", "radius"])
      shown.balls[index][key] +=
        (targetBalls[index][key] - shown.balls[index][key]) * .3;
    shown.balls[index].active = targetBalls[index].active;
  }
  shown.ball = shown.balls[0];
  shown.camera = { ...snapshot.camera };
  shown.round = { ...snapshot.round };
  shown.phase = snapshot.phase;
  shown.seq = snapshot.seq;
  shown.replayUrl = snapshot.replayUrl;
  if (Date.now() - lastFrameAt > 5000 && live) {
    live = false;
    status = "match paused";
  }
}

function textWidth(text) { return String(text).length * 6; }

function drawFighter({ ink, line, circle }, fighter, project, tick) {
  const p = project(fighter.x, fighter.y);
  const size = Math.max(24, Math.min(54, project.scale * 260));
  const top = p.y - size * (fighter.ducking ? .8 : 1.4);
  const waist = p.y - size * .55;
  const facing = fighter.facing || 1;
  const swing = fighter.grounded && !fighter.blocking
    ? Math.sin(tick * .13 + fighter.x * .002) * size * .22 : 0;
  const [r, g, b] = fighter.color;
  ink(r, g, b, fighter.alive ? 255 : 80);
  if (!fighter.alive) {
    line(p.x - size, p.y, p.x + size, p.y);
    line(p.x + size * .6, p.y, p.x + size * 1.3, p.y - size * .25);
    ink(r, g, b, 180).write(fighter.name, {
      x: Math.max(3, p.x - textWidth(fighter.name) / 2), y: p.y + 5,
    });
    return;
  }
  circle(Math.round(p.x), Math.round(top), Math.max(2, size * .25), true);
  line(p.x, top + size * .25, p.x, waist);
  const reach = fighter.attack ? size * 1.15 : size * .62;
  const armY = fighter.blocking ? top + size * .35 : top + size * .55;
  line(p.x, armY, p.x + facing * reach, armY + (fighter.attack === "KICK" ? size * .2 : 0));
  line(p.x, armY, p.x - facing * size * .42, armY + size * .28);
  line(p.x, waist, p.x - size * .45 + swing, p.y);
  line(p.x, waist, p.x + size * .45 - swing, p.y);
  if (fighter.attack === "KICK")
    line(p.x, waist, p.x + facing * size * 1.2, waist + size * .18);
  const handle = `${fighter.name} ${fighter.score}`;
  ink(r, g, b).write(handle, {
    x: Math.max(3, p.x - textWidth(handle) / 2), y: p.y + 5,
  });
}

function paint({ wipe, ink, line, circle, screen }) {
  wipe(12, 15, 24);
  const sw = screen.width, sh = screen.height;
  const badge = live ? "LIVE" : status.toUpperCase();
  ink(live ? 255 : 130, live ? 70 : 138, live ? 80 : 155)
    .write(badge, { x: Math.max(5, sw - textWidth(badge) - 5), y: 26 });

  if (!shown) {
    ink(120, 132, 155).write(status, {
      x: Math.max(4, Math.floor((sw - textWidth(status)) / 2)),
      y: Math.floor(sh / 2) - 4,
    });
    if (matchName) ink(75, 86, 108).write(matchName, {
      x: Math.max(4, Math.floor((sw - textWidth(matchName)) / 2)),
      y: Math.floor(sh / 2) + 8,
    });
    return;
  }

  const top = 48, bottom = sh - 24;
  const camera = shown.camera;
  const worldHeight = camera.width / CAMERA_ASPECT;
  const scale = Math.min((sw - 8) / camera.width, (bottom - top) / worldHeight);
  const usedW = camera.width * scale, usedH = worldHeight * scale;
  const left = (sw - usedW) / 2;
  const stageTop = top + Math.max(0, (bottom - top - usedH) / 2);
  const project = (x, y) => ({ x: left + (x - (camera.x - camera.width / 2)) * scale,
    y: stageTop + (y - (camera.y - worldHeight / 2)) * scale });
  project.scale = scale;

  const floor = project(camera.x, WORLD_FLOOR).y;
  ink(24, 30, 45).box(Math.round(left), Math.round(stageTop),
    Math.round(usedW), Math.round(usedH));
  ink(70, 78, 96).line(Math.round(left), Math.round(floor),
    Math.round(left + usedW), Math.round(floor));
  const platformA = project(4500, 10400), platformB = project(7500, 10400);
  ink(54, 63, 82).line(platformA.x, platformA.y, platformB.x, platformB.y);

  for (const item of shown.balls || [shown.ball]) {
    if (!item.active) continue;
    const ball = project(item.x, item.y);
    const owner = shown.fighters[item.spawnOwner] || shown.fighters[0];
    ink(...(owner?.color || [250, 225, 105])).circle(ball.x, ball.y,
      Math.max(2, item.radius * scale), true);
  }
  const api = { ink, line, circle };
  shown.fighters.forEach((fighter) => drawFighter(api, fighter, project, shown.seq || 0));

  const clock = Math.ceil(shown.round.remainingMs / 1000).toString();
  ink(225, 226, 220).write(clock,
    { x: Math.floor((sw - textWidth(clock)) / 2), y: 26 });
  if (shown.round.result) {
    const result = shown.round.result.slice(0, Math.max(1, Math.floor((sw - 8) / 6)));
    ink(255, 236, 145).write(result, {
      x: Math.max(4, Math.floor((sw - textWidth(result)) / 2)),
      y: Math.max(28, Math.floor(sh / 2)),
    });
  }
  const footer = `${matchName} · ${viewers} watching`;
  ink(82, 94, 116).write(footer,
    { x: Math.max(4, Math.floor((sw - textWidth(footer)) / 2)), y: sh - 10 });
}

function leave() {
  alive = false;
  clearTimeout(reconnectTimer);
  reconnectTimer = null;
  ws?.close();
  ws = null;
}

function meta({ params = [], colon = [] } = {}) {
  const parsed = canonicalMatch(colon[0] || params[0]);
  return { title: parsed ? `OSKIEWAR ${parsed.name}` : "OSKIEWAR live",
    desc: parsed ? `Watch OSKIEWAR match ${parsed.name} live.`
      : "Watch a live OSKIEWAR match." };
}

export { boot, sim, paint, leave, meta };
