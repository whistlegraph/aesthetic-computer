// Agent Presence (client), 26.09.03
// Connects this AC surface to the presence relay under the user's handle and
// shows the linked-agent mark while an agent is linked in — the platform-wide
// cousin of oskiewar's telemetry antenna. The mark sits bottom right, clear of
// the prompt's top-right block and its corner labels. It is an agent in the
// abstract, whoever's it is: an antenna that signals, a head whose eyes look
// about, and how many are linked beside it.

const RELAY_HOST = "session-server.aesthetic.computer";
const RETRY_MS = 30 * 1000;

let socket = null;
let currentRoom = null;
let mark = null; // { box, canvas, label }
let retryTimer = null;
let ticker = null;
let frame = 0;

const W = 16, H = 16, SCALE = 2;
const GREEN = "rgb(70, 200, 90)", PINK = "rgb(180, 72, 135)", DEEP = "rgb(120, 44, 92)", WHITE = "rgb(255, 255, 255)";

// The agent, 16 × 16: an antenna up top ("linked") whose signal goes out in
// rings, a head below ("agent") with eyes that look left and right and blink,
// and a pair of shoulders.
function drawMark(canvas, tick) {
  const ctx = canvas.getContext("2d");
  const px = (color, x, y, w = 1, h = 1) => { ctx.fillStyle = color; ctx.fillRect(x, y, w, h); };
  ctx.clearRect(0, 0, W, H);
  const ring = tick % 4, look = [0, 0, 1, 1, 0, 0, -1, -1][tick % 8], blink = tick % 11 === 10;
  // Antenna: a stem, a tip, and the signal leaving it.
  px(GREEN, 7, 2, 2, 4);
  px(GREEN, 6, 1, 4, 2);
  if (ring >= 1) { px(GREEN, 4, 1, 1, 2); px(GREEN, 11, 1, 1, 2); }
  if (ring >= 2) { px(GREEN, 2, 0, 1, 4); px(GREEN, 13, 0, 1, 4); }
  if (ring >= 3) { px(GREEN, 0, 0, 1, 5); px(GREEN, 15, 0, 1, 5); }
  // Head: a block with its corners off and a darker jaw.
  px(PINK, 4, 6, 8, 1);
  px(PINK, 3, 7, 10, 5);
  px(DEEP, 4, 12, 8, 1);
  // Eyes.
  for (const x of [5, 9]) {
    if (blink) px(WHITE, x, 9, 2, 1);
    else { px(WHITE, x, 8, 2, 2); px(DEEP, x + (look > 0 ? 1 : 0), 8 + (look === 0 ? 1 : 0), 1, 1); }
  }
  // Shoulders.
  px(PINK, 6, 13, 4, 1);
  px(PINK, 2, 14, 12, 2);
}

// Where the mark stands. A piece that shows the handle (the prompt) says
// where the label ends and the mark is a small postfix there, raised like a
// superscript; anywhere else it keeps the bottom-right corner, with its count.
let anchor = null; // { x, y, h, canvas() } in the piece's own pixels
export function anchorAgentMark(at, canvas) {
  anchor = at && Number.isFinite(at.x) && Number.isFinite(at.y) ? { ...at, canvas } : null;
  place();
}
function place() {
  if (!mark) return;
  const { box, canvas, label } = mark;
  const piece = anchor?.canvas?.(), frame = piece?.getBoundingClientRect?.();
  const postfix = !!(frame && piece.width && frame.width);
  const size = postfix ? 1 : SCALE;
  canvas.style.width = `${W * size}px`;
  canvas.style.height = `${H * size}px`;
  label.style.display = postfix ? "none" : "";
  if (postfix) {
    const k = frame.width / piece.width;
    Object.assign(box.style, {
      left: `${Math.round(frame.left + anchor.x * k + 2)}px`,
      top: `${Math.round(frame.top + anchor.y * k - H * 0.45)}px`,
      right: "auto", bottom: "auto",
    });
  } else Object.assign(box.style, { left: "auto", top: "auto", right: "8px", bottom: "8px" });
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
    "font:10px/1 ui-monospace,Menlo,monospace;letter-spacing:0.04em;color:rgb(235,255,240);" +
    "background:rgba(20,28,24,0.78);border:1px solid rgb(70,200,90);padding:3px 5px;white-space:nowrap;";
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
  // How many are linked; who they are is in the tooltip.
  label.textContent = agents.length > 1 ? `${agents.length} linked` : "linked";
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
