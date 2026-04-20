// notepat-remote, 26.4.20
// AC 🎹 Notepat Remote — dual-mode Max for Live device:
//   1) Subscribes to session-server's notepat:midi fanout, so ac-native
//      notepat.mjs on a ThinkPad plays this track.
//   2) Accepts local computer-keyboard input (same key→note layout as the
//      standard notepat piece) so the device plays as a standalone
//      instrument too.
//
// Both paths emit MIDI to Max via window.max.outlet, which the surrounding
// patcher routes through [route note channel] → [noteout].

const { floor, min, max } = Math;

const WS_URL = "wss://session-server.aesthetic.computer/";
const RECONNECT_FRAMES = 120; // ~2s @ 60fps

// ─── Keyboard → MIDI pitch (mirrors notepat.mjs NOTE_TO_KEYBOARD_KEY) ───
// Row 1 (below default octave): z=A#3, x=B3
// Row 2 (C4..B4): c v d s e f w g r a q b
// Row 3 (C5..B5): h t i y j k u l o m p n
// Row 4 (C6+):    ; ' ]
const KEY_TO_PITCH = {
  z: 58, x: 59,
  c: 60, v: 61, d: 62, s: 63, e: 64, f: 65, w: 66,
  g: 67, r: 68, a: 69, q: 70, b: 71,
  h: 72, t: 73, i: 74, y: 75, j: 76, k: 77, u: 78,
  l: 79, o: 80, m: 81, p: 82, n: 83,
  ";": 84, "'": 85, "]": 86,
};

let ws = null;
let wsState = "idle"; // idle | connecting | open | closed | error
let wsError = "";
let reconnectAt = 0;

let sources = []; // [{ handle, machineId, piece, lastSeen }]
let pktCount = 0; // relay packets received
let noteOnCount = 0;
let noteOffCount = 0;

let localOnCount = 0; // keyboard-originated notes
let localOffCount = 0;

let lastNote = null; // { pitch, vel, chan, source, handle, ts, latencyMs }
let lastNoteFrame = -9999;

const heldKeys = new Set(); // currently-held local keyboard keys

let frame = 0;
let lastEmittedChannel = -1;

// Max for Live jweb~ bridge — looked up lazily because jweb~ injects
// window.max AFTER the page load, not before module evaluation.
function maxBridge() {
  if (typeof window === "undefined") return null;
  const m = window.max;
  if (!m || typeof m.outlet !== "function") return null;
  return m;
}

function emitMaxNote(pitch, velocity, channel) {
  const m = maxBridge();
  if (!m) return;
  try {
    if (channel !== lastEmittedChannel) {
      m.outlet("channel", channel);
      lastEmittedChannel = channel;
    }
    m.outlet("note", pitch, velocity);
    // Mirror to Max console so it's obvious when a note fired.
    console.log(`🎹 out note=${pitch} vel=${velocity} ch=${channel}`);
  } catch (err) {
    console.log("🎹 outlet err:", err?.message || err);
  }
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
        handleRelayEvent(msg.content || {});
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

function handleRelayEvent(ev) {
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
    source: "relay",
    handle: ev.handle || "",
    ts: now,
    latencyMs: latency,
  };
  lastNoteFrame = frame;
}

function pressLocalKey(key) {
  if (heldKeys.has(key)) return;
  const pitch = KEY_TO_PITCH[key];
  if (pitch === undefined) return;
  heldKeys.add(key);
  localOnCount += 1;
  emitMaxNote(pitch, 100, 0);
  lastNote = {
    pitch,
    vel: 100,
    chan: 0,
    source: "local",
    handle: "",
    ts: Date.now(),
    latencyMs: 0,
  };
  lastNoteFrame = frame;
}

function releaseLocalKey(key) {
  if (!heldKeys.has(key)) return;
  const pitch = KEY_TO_PITCH[key];
  heldKeys.delete(key);
  if (pitch === undefined) return;
  localOffCount += 1;
  emitMaxNote(pitch, 0, 0);
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

function act({ event: e }) {
  if (!e?.is) return;
  // Click-to-test-note: tap anywhere on the device UI fires C4 so we can
  // verify the Max bridge path without depending on keyboard focus.
  if (e.is("touch")) {
    pressLocalKey("c");
    return;
  }
  if (e.is("lift")) {
    releaseLocalKey("c");
    return;
  }
  // Best-effort debug: log raw keyboard events to Max console.
  if (e.is("keyboard:down") || e.is("keyboard:up")) {
    console.log(`🎹 kbd ${e.key || e.name || "?"}`);
  }
  for (const key of Object.keys(KEY_TO_PITCH)) {
    if (e.is(`keyboard:down:${key}`)) {
      pressLocalKey(key);
      return;
    }
    if (e.is(`keyboard:up:${key}`)) {
      releaseLocalKey(key);
      return;
    }
  }
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

  // Header — check bridge each frame in case jweb~ injected window.max late
  const bridgeActive = !!maxBridge();
  ink(...accent).write("NOTEPAT-REMOTE", { x: 4, y, size: 1 });
  ink(...(bridgeActive ? good : warn)).write(
    bridgeActive ? "[M4L]" : "[solo]",
    { x: 112, y },
  );
  y += 10;

  // WS + sources on one line
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
  if (sources.length === 0) {
    ink(...fgDim).write("src: (none)", { x: 80, y });
  } else {
    const label = sources
      .slice(0, 3)
      .map((s) => (s.handle ? "@" + s.handle : s.machineId.slice(0, 6)))
      .join(" ");
    ink(...fg).write("src:" + label.slice(0, 24), { x: 80, y });
  }
  y += 8;
  if (wsError) {
    ink(...bad).write(wsError.slice(0, 34), { x: 4, y });
    y += 8;
  }

  // Counters
  ink(...fgDim).write(
    `relay ${pktCount} (on ${noteOnCount} off ${noteOffCount})`,
    { x: 4, y },
  );
  y += 8;
  ink(...fgDim).write(
    `local ${localOnCount + localOffCount} (on ${localOnCount} off ${localOffCount})`,
    { x: 4, y },
  );
  y += 10;

  // Last note — flashes accent, fades to fg over ~30 frames
  if (lastNote) {
    const age = frame - lastNoteFrame;
    const flashing = age < 30;
    const color = flashing ? accent : fg;
    const arrow = lastNote.vel === 0 ? "v" : "^";
    const src = lastNote.source === "local" ? "kbd" : "@" + (lastNote.handle || "?");
    const label = `${arrow} ${pitchName(lastNote.pitch)} (${lastNote.pitch}) v${lastNote.vel}  ${src}`;
    ink(...color).write(label, { x: 4, y });
    y += 8;
  } else {
    ink(...fgDim).write("(no notes yet — type or start relay)", { x: 4, y });
    y += 8;
  }

  // Held-key strip — shows which keyboard keys are currently down
  y += 4;
  if (heldKeys.size > 0) {
    const held = Array.from(heldKeys)
      .map((k) => pitchName(KEY_TO_PITCH[k]))
      .join(" ");
    ink(...accent).write("held: " + held.slice(0, 32), { x: 4, y });
    y += 8;
  }

  // Footer hint
  if (H - y > 12) {
    ink(...fgDim).write("click=C4 | keys c-b=C4..B4", { x: 4, y: H - 8 });
  }
}

function leave() {
  try {
    ws?.close();
  } catch {}
  ws = null;
  heldKeys.clear();
}

function meta() {
  return {
    title: "Notepat Remote",
    desc: "M4L: ac-native notepat relay + local keyboard notes → MIDI track",
  };
}

export { boot, sim, paint, act, leave, meta };
