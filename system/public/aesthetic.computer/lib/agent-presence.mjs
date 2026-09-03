// Agent Presence (client), 26.09.03
// Connects this AC surface to the presence relay under the user's handle and
// shows a small agent mark in the bottom-right corner while an agent is
// linked in — the platform-wide cousin of oskiewar's telemetry antenna.

const RELAY_HOST = "session-server.aesthetic.computer";
const RETRY_MS = 30 * 1000;

let socket = null;
let currentRoom = null;
let mark = null;
let retryTimer = null;

// A 12x12 linked-agent glyph: antenna up top ("linked"), head below ("agent").
function drawMark(canvas, agents) {
  const ctx = canvas.getContext("2d");
  ctx.clearRect(0, 0, 12, 12);
  ctx.fillStyle = "rgb(70, 200, 90)"; // acgreen antenna
  ctx.fillRect(5, 0, 2, 3);
  ctx.fillRect(2, 1, 2, 1);
  ctx.fillRect(8, 1, 2, 1);
  ctx.fillStyle = "rgb(180, 72, 135)"; // acpink head
  ctx.fillRect(3, 4, 6, 6);
  ctx.fillStyle = "rgb(255, 255, 255)"; // eyes
  ctx.fillRect(4, 6, 1, 2);
  ctx.fillRect(7, 6, 1, 2);
  canvas.title = `linked: ${agents.join(", ")}`;
}

function ensureMark() {
  if (mark) return mark;
  mark = document.createElement("canvas");
  mark.width = 12;
  mark.height = 12;
  mark.style.cssText =
    "position:fixed;right:8px;bottom:8px;width:24px;height:24px;" +
    "image-rendering:pixelated;z-index:1000;display:none;opacity:0.9;";
  mark.dataset.acAgentMark = "true";
  document.body.appendChild(mark);
  return mark;
}

function show(agents) {
  const canvas = ensureMark();
  if (agents.length > 0) {
    drawMark(canvas, agents);
    canvas.style.display = "block";
  } else {
    canvas.style.display = "none";
  }
}

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
