// notepat-remote, 26.4.20
// AC 🎹 Notepat Remote — receives notepat:midi events from session-server
// and bridges them to Max for Live via window.max.outlet.
//
// Meant to load inside jweb~ in AC-NotepatRemote.amxd. When opened standalone
// in a browser it still renders the connection status UI (MIDI out is a no-op).
//
// Wire path:
//   ThinkPad ac-native notepat.mjs
//     → UDP :10010 → session-server.aesthetic.computer
//     → WS fanout → this piece
//     → window.max.outlet(["note", pitch, vel]) → Max [route note] → [noteout]

const { floor, min, max } = Math;

const WS_URL = "wss://session-server.aesthetic.computer/";
const RECONNECT_FRAMES = 120; // ~2s @ 60fps

let ws = null;
let wsState = "idle"; // idle | connecting | open | closed | error
let wsError = "";
let reconnectAt = 0;

let sources = []; // [{ handle, machineId, piece, lastSeen }]
let pktCount = 0;
let noteOnCount = 0;
let noteOffCount = 0;
let lastNote = null; // { pitch, vel, chan, event, handle, ts, latencyMs }
let lastNoteFrame = -9999;

let frame = 0;
let lastSubscribedCh = -1;

// Max for Live jweb~ bridge (exposes window.max.outlet)
const maxBridge =
  typeof window !== "undefined" &&
  window.max &&
  typeof window.max.outlet === "function"
    ? window.max
    : null;

function emitMaxNote(pitch, velocity, channel) {
  if (!maxBridge) return;
  try {
    if (channel !== lastSubscribedCh) {
      maxBridge.outlet(["channel", channel]);
      lastSubscribedCh = channel;
    }
    maxBridge.outlet(["note", pitch, velocity]);
  } catch (_err) {}
}

function connectWs() {
  if (typeof WebSocket === "undefined") return;
  if (ws && (ws.readyState === 0 || ws.readyState === 1)) return;
  try {
    wsState = "connecting";
    wsError = "";
    ws = new WebSocket(WS_URL);
    ws.onopen = () => {
      wsState = "open";
      try {
        ws.send(
          JSON.stringify({
            type: "notepat:midi:subscribe",
            content: { all: true },
          }),
        );
      } catch (_e) {}
    };
    ws.onmessage = (ev) => {
      let msg;
      try {
        msg = JSON.parse(ev.data);
      } catch {
        return;
      }
      if (!msg || !msg.type) return;
      if (msg.type === "notepat:midi") {
        handleMidiEvent(msg.content || {});
      } else if (msg.type === "notepat:midi:sources") {
        const list = (msg.content && msg.content.sources) || [];
        sources = list.map((s) => ({
          handle: s.handle || "",
          machineId: s.machineId || "",
          piece: s.piece || "notepat",
          lastSeen: s.lastSeen || 0,
        }));
      }
    };
    ws.onerror = () => {
      wsState = "error";
      wsError = "ws error";
    };
    ws.onclose = () => {
      wsState = "closed";
      reconnectAt = frame + RECONNECT_FRAMES;
    };
  } catch (err) {
    wsState = "error";
    wsError = err?.message || "connect failed";
    reconnectAt = frame + RECONNECT_FRAMES;
  }
}

function handleMidiEvent(ev) {
  const pitch = Number(ev.note);
  const vel = Number(ev.velocity);
  const chan = Number(ev.channel) || 0;
  if (!Number.isFinite(pitch) || !Number.isFinite(vel)) return;
  pktCount += 1;
  const now = Date.now();
  const tsNum = Number(ev.ts);
  const latency = Number.isFinite(tsNum) ? max(0, now - tsNum) : 0;
  const isOff = ev.event === "note_off" || (ev.event === "note_on" && vel === 0);
  if (isOff) {
    noteOffCount += 1;
    emitMaxNote(pitch, 0, chan);
  } else {
    noteOnCount += 1;
    emitMaxNote(pitch, max(1, min(127, vel)), chan);
  }
  lastNote = {
    pitch,
    vel,
    chan,
    event: ev.event || (isOff ? "note_off" : "note_on"),
    handle: ev.handle || "",
    ts: now,
    latencyMs: latency,
  };
  lastNoteFrame = frame;
}

const PITCH_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
function pitchName(p) {
  const n = PITCH_NAMES[((p % 12) + 12) % 12];
  const o = floor(p / 12) - 1;
  return n + o;
}

function boot({ wipe, cursor }) {
  wipe(8, 10, 18);
  cursor?.("native");
  connectWs();
}

function sim() {
  frame += 1;
  if (wsState === "closed" && frame >= reconnectAt) connectWs();
}

function paint({ wipe, ink, box, line, screen }) {
  const bg = [8, 10, 18];
  const fg = [220, 225, 255];
  const fgDim = [130, 140, 170];
  const accent = [255, 170, 80];
  const good = [140, 255, 180];
  const bad = [255, 120, 120];
  const warn = [255, 220, 100];
  wipe(...bg);

  const W = screen.width;
  const H = screen.height;
  let y = 4;

  // Header + bridge badge
  ink(...accent).write("NOTEPAT-REMOTE", { x: 4, y, size: 1 });
  ink(...(maxBridge ? good : warn)).write(
    maxBridge ? "[M4L]" : "[solo]",
    { x: 112, y },
  );
  y += 10;

  // WS status line
  const stateColor =
    wsState === "open"
      ? good
      : wsState === "connecting"
        ? warn
        : wsState === "error" || wsState === "closed"
          ? bad
          : fgDim;
  ink(...fgDim).write("ws", { x: 4, y });
  ink(...stateColor).write(wsState.toUpperCase(), { x: 18, y });
  if (wsError) ink(...bad).write(wsError.slice(0, 24), { x: 60, y });
  y += 8;

  // Sources
  ink(...fgDim).write("src", { x: 4, y });
  if (sources.length === 0) {
    ink(...fgDim).write("(none — start relay on)", { x: 18, y });
  } else {
    const label = sources
      .slice(0, 4)
      .map((s) => (s.handle ? "@" + s.handle : s.machineId.slice(0, 6)))
      .join(" ");
    ink(...fg).write(label.slice(0, 36), { x: 18, y });
  }
  y += 8;

  // Counters
  ink(...fgDim).write(
    `pkt ${pktCount}  on ${noteOnCount}  off ${noteOffCount}`,
    { x: 4, y },
  );
  y += 10;

  // Last note — flashes accent, fades to fg over ~30 frames
  if (lastNote) {
    const age = frame - lastNoteFrame;
    const flashing = age < 30;
    const color = flashing ? accent : fg;
    const arrow = lastNote.vel === 0 || lastNote.event === "note_off" ? "v" : "^";
    const label = `${arrow} ${pitchName(lastNote.pitch)} (${lastNote.pitch}) vel ${lastNote.vel} ch ${lastNote.chan}`;
    ink(...color).write(label, { x: 4, y });
    y += 8;
    const who = lastNote.handle ? "@" + lastNote.handle : "?";
    ink(...fgDim).write(`${who}  ${lastNote.latencyMs}ms`, { x: 4, y });
    y += 10;
  } else {
    ink(...fgDim).write("(waiting for notes…)", { x: 4, y });
    y += 10;
  }

  // Indicator strip at bottom — one bar per source, bars flash on note
  if (H - y > 14) {
    const barY = H - 10;
    ink(...fgDim).line(2, barY - 1, W - 2, barY - 1);
    ink(...fg).write("hint: enable on ThinkPad: 'midi relay on'", { x: 4, y: H - 8 });
  }
}

function leave() {
  try {
    ws?.close();
  } catch {}
  ws = null;
}

function meta() {
  return {
    title: "Notepat Remote",
    desc: "Max for Live bridge: session-server notepat:midi → MIDI track",
  };
}

export { boot, sim, paint, leave, meta };
