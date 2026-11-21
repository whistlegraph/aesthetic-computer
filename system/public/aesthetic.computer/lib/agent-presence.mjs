// Agent Presence (client), 26.09.03
// Connects this AC surface to the presence relay under the user's handle and
// shows the linked-agent mark while an agent is linked in — the platform-wide
// cousin of oskiewar's telemetry antenna. The mark sits bottom right, clear of
// the prompt's top-right block and its corner labels: a crab that waves, with
// the name of whoever is linked beside it.

const RELAY_HOST = "session-server.aesthetic.computer";
const RETRY_MS = 30 * 1000;

let socket = null;
let currentRoom = null;
let mark = null; // { box, canvas, label }
let retryTimer = null;
let ticker = null;
let frame = 0;

const W = 18, H = 14, SCALE = 2;
const SHELL = "rgb(217, 119, 87)", SHADE = "rgb(168, 82, 58)", LINK = "rgb(70, 200, 90)";

// The crab, 18 × 14: eyes on stalks, two claws that wave in turn, six legs
// that step, and a link dot over its shoulder that pulses while it is linked.
function drawMark(canvas, tick) {
  const ctx = canvas.getContext("2d");
  const px = (color, x, y, w = 1, h = 1) => { ctx.fillStyle = color; ctx.fillRect(x, y, w, h); };
  ctx.clearRect(0, 0, W, H);
  const step = tick % 2, wave = Math.floor(tick / 2) % 2, blink = tick % 9 === 8;
  // Shell: a rounded body with a darker belly line.
  px(SHELL, 5, 5, 8, 1);
  px(SHELL, 4, 6, 10, 4);
  px(SHELL, 5, 10, 8, 1);
  px(SHADE, 5, 9, 8, 1);
  // Eyes on stalks.
  for (const x of [6, 11]) {
    px(SHELL, x, 4);
    px("rgb(255, 255, 255)", x, 2, 1, 2);
    if (!blink) px("rgb(20, 20, 30)", x, 2);
    else px(SHELL, x, 2, 1, 2);
  }
  // Claws: an arm out from each shoulder, the pincer up on one side at a time.
  for (const [side, up] of [[-1, wave === 0], [1, wave === 1]]) {
    const sx = side < 0 ? 3 : 14, cx = side < 0 ? 1 : 15, cy = up ? 2 : 4;
    px(SHELL, sx, 6);
    px(SHELL, side < 0 ? 2 : 15, up ? 5 : 6);
    px(SHELL, cx, cy, 2, 2);
    px(SHADE, side < 0 ? cx : cx + 1, cy); // the pincer's notch
  }
  // Legs: three a side, stepping.
  for (let k = 0; k < 3; k += 1) {
    const down = (k + step) % 2;
    px(SHELL, 4 + k * 2 - 1, 11 + down);
    px(SHELL, 13 - k * 2 + 1, 11 + (1 - down));
  }
  // The link: a dot that pulses with a ring.
  px(LINK, 16, 9, 2, 2);
  if (tick % 4 < 2) { px(LINK, 16, 7, 2, 1); px(LINK, 16, 12, 2, 1); }
}

// The prompt keeps rax's reticle in its bottom-right corner: the mark stands above it there.
const raised = () => /^\/(prompt)?$/.test(location.pathname);
function place() {
  if (mark) mark.box.style.bottom = raised() ? "96px" : "8px";
}

function ensureMark() {
  if (mark) return mark;
  const box = document.createElement("div");
  box.style.cssText =
    "position:fixed;right:8px;bottom:8px;display:none;align-items:center;gap:5px;" +
    "z-index:1000;pointer-events:none;opacity:0.92;";
  box.dataset.acAgentMark = "true";
  const label = document.createElement("span");
  label.style.cssText =
    "font:10px/1 ui-monospace,Menlo,monospace;letter-spacing:0.04em;color:rgb(255,236,226);" +
    "background:rgba(40,22,18,0.78);border:1px solid rgb(217,119,87);padding:3px 5px;white-space:nowrap;";
  const canvas = document.createElement("canvas");
  canvas.width = W;
  canvas.height = H;
  canvas.style.cssText = `width:${W * SCALE}px;height:${H * SCALE}px;image-rendering:pixelated;`;
  box.append(label, canvas);
  document.body.appendChild(box);
  mark = { box, canvas, label };
  return mark;
}

export function showAgentMark(agents) {
  const { box, canvas, label } = ensureMark();
  clearInterval(ticker);
  ticker = null;
  if (agents.length === 0) {
    box.style.display = "none";
    return;
  }
  // Who is linked, by name; a crowd is counted.
  const names = [...new Set(agents.map((a) => String(a).slice(0, 24)))];
  label.textContent = names.length > 2 ? `${names[0]} +${names.length - 1}` : names.join(" · ");
  box.title = `linked: ${agents.join(", ")}`;
  box.style.display = "flex";
  place();
  drawMark(canvas, frame);
  ticker = setInterval(() => { frame += 1; place(); drawMark(canvas, frame); }, 400);
}
const show = showAgentMark;

function connect(room) {
  const dev = location.hostname === "localhost";
  const url = dev
    ? `ws://localhost:8889/agent-presence?room=${room}&role=surface`
    : `wss://${RELAY_HOST}/agent-presence?room=${room}&role=surface`;
  try {
    socket = new WebSocket(url);
  } catch {
    return; // No relay reachable; the mark just never lights.
  }
  socket.onmessage = (event) => {
    let message;
    try { message = JSON.parse(event.data); } catch { return; }
    if (message.type === "agent-presence:status") {
      show(message.content?.agents || []);
    }
  };
  const retry = () => {
    socket = null;
    show([]);
    if (currentRoom !== room) return; // Superseded by a newer start.
    clearTimeout(retryTimer);
    retryTimer = setTimeout(() => {
      if (currentRoom === room && !socket) connect(room);
    }, RETRY_MS);
  };
  socket.onclose = retry;
  socket.onerror = () => socket?.close();
}

// Idempotent: bios calls this whenever the handle lands or changes.
export function startAgentPresence({ room }) {
  const name = String(room || "").toLowerCase().replace(/^@/, "");
  if (!/^[a-z0-9_-]{1,32}$/.test(name)) return;
  if (name === currentRoom && socket) return;
  currentRoom = name;
  clearTimeout(retryTimer);
  if (socket) { try { socket.close(); } catch { /* replaced */ } socket = null; }
  connect(name);
}
