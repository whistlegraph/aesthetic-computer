// notepat-remote, 26.4.20
// AC 🎹 Notepat Remote — Max for Live device UI.
//   • Local keyboard input is owned by BIOS (bios.mjs dawKeyEmit) — it
//     captures keydown/keyup in the jweb iframe and calls window.max.outlet
//     with a finished MIDI pitch, routed straight to [noteout] in the
//     patcher for sub-ms latency. This piece only mirrors that state for
//     visual feedback.
//   • Session-server relay path: this piece opens a WebSocket to
//     wss://session-server.aesthetic.computer/ and forwards notepat:midi
//     events via send({type:"daw:midi",...}) which BIOS turns into
//     window.max.outlet("note"/"channel", ...). Same bridge, async path.
//   • Button grid under the status header lets you tap notes on touch
//     devices — each button fires the same note your keyboard would.
//   • When the iframe loses focus, Max's key listener stops receiving, so
//     the UI goes red + "TAP ME!" attract mode to prompt a click.

const { floor, min, max, abs, sin, PI } = Math;

const WS_URL = "wss://session-server.aesthetic.computer/";
const RECONNECT_FRAMES = 120;

// Key offsets relative to base-octave C. Mirrors bios.mjs _dawKeyOffsets.
const KEY_OFFSETS = {
  z: -2, x: -1,
  c: 0, v: 1, d: 2, s: 3, e: 4, f: 5, w: 6,
  g: 7, r: 8, a: 9, q: 10, b: 11,
  h: 12, t: 13, i: 14, y: 15, j: 16, k: 17, u: 18,
  l: 19, o: 20, m: 21, p: 22, n: 23,
  ";": 24, "'": 25, "]": 26,
};

// Chromatic octave blocks — 4 cols × 3 rows = 12 notes each.
// Matches notepat.mjs pad layout. Row 0 = low (C..D#), row 2 = high (G#..B).
// Two blocks render side-by-side (base octave left, +1 right) when the
// device is wide enough (always 360px in M4L).
const OCTAVE_GRIDS = [
  // Base octave (offsets 0-11 → C..B of baseOctave)
  [
    ["c", "v", "d", "s"],
    ["e", "f", "w", "g"],
    ["r", "a", "q", "b"],
  ],
  // +1 octave (offsets 12-23 → C..B of baseOctave+1)
  [
    ["h", "t", "i", "y"],
    ["j", "k", "u", "l"],
    ["o", "m", "p", "n"],
  ],
];

const PITCH_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
function pitchName(p) {
  return PITCH_NAMES[((p % 12) + 12) % 12] + (floor(p / 12) - 1);
}
function isBlackKey(p) {
  return [1, 3, 6, 8, 10].includes(((p % 12) + 12) % 12);
}

let ws = null;
let wsState = "idle";
let wsError = "";
let reconnectAt = 0;

let sources = [];
let relayCount = 0;

let lastNote = null;
let lastNoteFrame = -9999;

// Local state mirrored from keyboard events (visual only — BIOS emits MIDI).
let baseOctave = 4;
const heldKeys = new Set();
let focused = true;          // assume focused until we learn otherwise
let focusedChangedFrame = 0;
let lastInteractionFrame = -999;

let _send = null;
let frame = 0;

// Button grid layout (recomputed on each paint in case screen size changes).
let buttons = [];

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
        ws.send(JSON.stringify({
          type: "notepat:midi:subscribe",
          content: { all: true },
        }));
      } catch (_e) {}
    };
    ws.onmessage = (ev) => {
      let msg;
      try { msg = JSON.parse(ev.data); } catch { return; }
      if (!msg || !msg.type) return;
      if (msg.type === "notepat:midi") {
        handleRelay(msg.content || {});
      } else if (msg.type === "notepat:midi:sources") {
        const list = (msg.content && msg.content.sources) || [];
        sources = list.map((s) => ({
          handle: s.handle || "",
          machineId: s.machineId || "",
        }));
      }
    };
    ws.onerror = () => { wsState = "error"; wsError = "ws err"; };
    ws.onclose = () => { wsState = "closed"; reconnectAt = frame + RECONNECT_FRAMES; };
  } catch (err) {
    wsState = "error";
    wsError = err?.message || "connect fail";
    reconnectAt = frame + RECONNECT_FRAMES;
  }
}

function handleRelay(ev) {
  if (!_send) return;
  const pitch = Number(ev.note);
  const vel = Number(ev.velocity);
  const chan = Number(ev.channel) || 0;
  if (!Number.isFinite(pitch) || !Number.isFinite(vel)) return;
  relayCount += 1;
  const isOff = ev.event === "note_off" || (ev.event === "note_on" && vel === 0);
  _send({
    type: "daw:midi",
    content: {
      pitch,
      velocity: isOff ? 0 : max(1, min(127, vel)),
      channel: chan,
    },
  });
  lastNote = {
    pitch,
    vel,
    source: "relay",
    handle: ev.handle || "",
    ts: Date.now(),
  };
  lastNoteFrame = frame;
}

// Emit a note via the daw:midi pipe (touch/click path — keyboard notes go
// through BIOS directly for speed, not through here).
function tapNote(pitch, on) {
  if (!_send) return;
  _send({
    type: "daw:midi",
    content: { pitch, velocity: on ? 100 : 0, channel: 0 },
  });
  lastNote = { pitch, vel: on ? 100 : 0, source: "tap", handle: "", ts: Date.now() };
  lastNoteFrame = frame;
  lastInteractionFrame = frame;
}

function pitchForKey(key) {
  const off = KEY_OFFSETS[key];
  if (off === undefined) return null;
  return (baseOctave + 1) * 12 + off;
}

function boot({ wipe, cursor, hud, send }) {
  wipe(10, 12, 22);
  cursor?.("native");
  hud?.label?.("");
  _send = send;
  connectWs();
}

function sim() {
  frame += 1;
  if (wsState === "closed" && frame >= reconnectAt) connectWs();
}

let tappedButton = null; // button object currently pressed via touch

function act({ event: e }) {
  if (!e?.is) return;

  // Focus/blur signals from AC (if AC forwards them; harmless if it doesn't).
  if (e.is("focus")) {
    focused = true;
    focusedChangedFrame = frame;
    return;
  }
  if (e.is("blur")) {
    focused = false;
    focusedChangedFrame = frame;
    heldKeys.clear();
    return;
  }

  // Button grid taps — tapNote for touch, release on lift.
  if (e.is("touch")) {
    lastInteractionFrame = frame;
    const hit = hitButton(e.x, e.y);
    if (hit) {
      tappedButton = hit;
      tapNote(hit.pitch, true);
    }
    return;
  }
  if (e.is("lift")) {
    if (tappedButton) {
      tapNote(tappedButton.pitch, false);
      tappedButton = null;
    }
    return;
  }
  if (e.is("draw")) {
    // Allow drag across buttons (piano-roll tap). Release previous, press new.
    const hit = hitButton(e.x, e.y);
    if (hit && hit !== tappedButton) {
      if (tappedButton) tapNote(tappedButton.pitch, false);
      tappedButton = hit;
      tapNote(hit.pitch, true);
    }
    return;
  }

  // Octave hot-switch 1-9 (BIOS also tracks this, we mirror for UI).
  for (let n = 1; n <= 9; n += 1) {
    if (e.is(`keyboard:down:${n}`)) {
      baseOctave = n;
      focused = true;
      lastInteractionFrame = frame;
      return;
    }
  }

  // Track keyboard state for visual feedback only.
  for (const key of Object.keys(KEY_OFFSETS)) {
    if (e.is(`keyboard:down:${key}`)) {
      if (!heldKeys.has(key)) {
        heldKeys.add(key);
        const p = pitchForKey(key);
        if (p !== null) {
          lastNote = { pitch: p, vel: 100, source: "kbd", handle: "", ts: Date.now() };
          lastNoteFrame = frame;
        }
      }
      focused = true;
      lastInteractionFrame = frame;
      return;
    }
    if (e.is(`keyboard:up:${key}`)) {
      heldKeys.delete(key);
      return;
    }
  }
}

function hitButton(x, y) {
  if (typeof x !== "number" || typeof y !== "number") return null;
  for (const b of buttons) {
    if (x >= b.x && x < b.x + b.w && y >= b.y && y < b.y + b.h) return b;
  }
  return null;
}

function paint({ wipe, ink, box, line, screen }) {
  const W = screen.width;
  const H = screen.height;

  // Attract mode: blink brightness between 0.4 and 1.
  const blinkPhase = (sin(frame * 0.12) + 1) / 2; // 0..1
  const attractPulse = focused ? 1 : 0.5 + blinkPhase * 0.5;

  const accent = focused
    ? [floor(130 + blinkPhase * 40), 255, floor(170 + blinkPhase * 50)] // lime
    : [255, floor(70 + blinkPhase * 100), floor(70 + blinkPhase * 40)]; // red
  const dim = [130, 140, 170];
  const fg = [220, 225, 255];
  const bgBase = focused ? [10, 20, 18] : [22, 10, 10];
  wipe(
    floor(bgBase[0] * attractPulse),
    floor(bgBase[1] * attractPulse),
    floor(bgBase[2] * attractPulse),
  );

  const sinceNote = frame - lastNoteFrame;
  // Flash overlay on recent note-on.
  if (lastNote && sinceNote < 10) {
    const f = 1 - sinceNote / 10;
    const alpha = floor(45 * f);
    ink(...accent, alpha).box(0, 0, W, H, "fill");
  }

  // ── Header row: piece name + ws state
  let y = 2;
  ink(...accent).write("notepat-remote", { x: 4, y });
  const wsColor =
    wsState === "open" ? [140, 255, 180] :
    wsState === "connecting" ? [255, 220, 100] :
    wsState === "error" || wsState === "closed" ? [255, 120, 120] : dim;
  ink(...wsColor).write(wsState, { x: W - wsState.length * 6 - 4, y });
  y += 10;

  // ── Status row: ACTIVE / TAP ME + octave + last note
  if (focused) {
    ink(...accent).box(4, y + 1, 6, 6, "fill");
    ink(...fg).write("ACTIVE", { x: 14, y });
  } else {
    const blinkOn = blinkPhase > 0.35;
    ink(blinkOn ? 255 : 110, blinkOn ? 40 : 20, blinkOn ? 40 : 20)
      .write("TAP ME!", { x: 4, y });
  }
  ink(...dim).write(`oct ${baseOctave}`, { x: 70, y });
  // Right side: compact last-note readout
  if (lastNote) {
    const noteFresh = sinceNote < 30;
    const pn = pitchName(lastNote.pitch);
    const noteColor = noteFresh ? accent : fg;
    const srcTag =
      lastNote.source === "relay" ? "@" + (lastNote.handle || "?") :
      lastNote.source === "tap" ? "tap" : "kbd";
    const label = `${lastNote.vel === 0 ? "v" : "^"} ${pn} ${srcTag}`;
    ink(...noteColor).write(label, { x: W - label.length * 6 - 4, y });
  }
  y += 10;

  // ── Button grid area: two 4×3 octave blocks side-by-side
  const gridTop = y + 2;
  const gridBottom = H - 4; // leave 4px safe margin at bottom
  const gap = 6;
  const blockW = floor((W - 8 - gap) / 2);
  const blockH = gridBottom - gridTop;
  const cellW = floor(blockW / 4);
  const cellH = floor(blockH / 3);

  buttons = [];
  for (let octIdx = 0; octIdx < OCTAVE_GRIDS.length; octIdx += 1) {
    const grid = OCTAVE_GRIDS[octIdx];
    const blockX = 4 + octIdx * (blockW + gap);
    // Octave number label in the corner of each block
    const octNum = baseOctave + octIdx;
    ink(...dim).write(`o${octNum}`, { x: blockX + 1, y: gridTop - 1 });

    for (let rowIdx = 0; rowIdx < grid.length; rowIdx += 1) {
      const row = grid[rowIdx];
      for (let colIdx = 0; colIdx < row.length; colIdx += 1) {
        const key = row[colIdx];
        const offset = rowIdx * 4 + colIdx;
        const pitch = (baseOctave + 1 + octIdx) * 12 + offset;
        const b = {
          x: blockX + colIdx * cellW,
          y: gridTop + rowIdx * cellH,
          w: cellW - 1,
          h: cellH - 1,
          key,
          pitch,
        };
        buttons.push(b);

        const held =
          heldKeys.has(key) || (tappedButton && tappedButton.key === key);
        const recentFlash =
          lastNote && lastNote.pitch === pitch && sinceNote < 18;
        const black = isBlackKey(pitch);

        let fill;
        if (held) {
          fill = accent;
        } else if (recentFlash) {
          const f = 1 - sinceNote / 18;
          fill = [
            floor(bgBase[0] + (accent[0] - bgBase[0]) * f * 0.6),
            floor(bgBase[1] + (accent[1] - bgBase[1]) * f * 0.6),
            floor(bgBase[2] + (accent[2] - bgBase[2]) * f * 0.6),
          ];
        } else if (black) {
          fill = focused ? [22, 32, 28] : [48, 18, 18];
        } else {
          fill = focused ? [42, 52, 48] : [70, 28, 28];
        }
        ink(...fill).box(b.x, b.y, b.w, b.h, "fill");
        ink(...(held ? accent : [80, 90, 100])).box(b.x, b.y, b.w, b.h, "outline");

        // Letter centered in cell
        const labelX = b.x + floor(b.w / 2) - 2;
        const labelY = b.y + floor(b.h / 2) - 4;
        const labelColor = held
          ? [10, 20, 10]
          : black
            ? [200, 210, 220]
            : fg;
        ink(...labelColor).write(key.toUpperCase(), { x: labelX, y: labelY });
      }
    }
  }
}

function leave() {
  try { ws?.close(); } catch {}
  ws = null;
  heldKeys.clear();
}

function meta() {
  return {
    title: "Notepat Remote",
    desc: "M4L: native-latency keyboard + session-server relay → MIDI track",
  };
}

export { boot, sim, paint, act, leave, meta };
