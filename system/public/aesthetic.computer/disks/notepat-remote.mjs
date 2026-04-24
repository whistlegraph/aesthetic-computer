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

import { getNoteColorForOctave } from "../lib/note-colors.mjs";

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
// Short pitch name (no octave suffix) for tight pad labels.
function pitchNameShort(p) {
  return PITCH_NAMES[((p % 12) + 12) % 12];
}
// MIDI octave number (C4 = 60 in standard MIDI).
function pitchOctave(p) {
  return floor(p / 12) - 1;
}
function isBlackKey(p) {
  return [1, 3, 6, 8, 10].includes(((p % 12) + 12) % 12);
}

let ws = null;
let wsState = "idle";
let wsError = "";
let reconnectAt = 0;

// Dual-transport net channels. Raw WS above carries the authoritative
// notepat:midi subscription (reliable, cross-session). UDP via geckos.io
// carries the same events on a low-latency path — session-server fans out
// notepat:midi over both when a subscriber opens both.
let netSocket = null;
let netUdp = null;
let udpSubscribed = false;
let udpRelayCount = 0;

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
    console.log(`[notepat-remote] WS connecting → ${WS_URL}`);
    ws = new WebSocket(WS_URL);
    ws.onopen = () => {
      wsState = "open";
      console.log(`[notepat-remote] WS open, subscribing to notepat:midi`);
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
    ws.onerror = (e) => {
      wsState = "error"; wsError = "ws err";
      console.warn(`[notepat-remote] WS error`, e?.message || e);
    };
    ws.onclose = (e) => {
      wsState = "closed"; reconnectAt = frame + RECONNECT_FRAMES;
      console.log(`[notepat-remote] WS closed code=${e?.code} reason=${e?.reason || ""} — reconnect in ${RECONNECT_FRAMES} frames`);
    };
  } catch (err) {
    wsState = "error";
    wsError = err?.message || "connect fail";
    reconnectAt = frame + RECONNECT_FRAMES;
    console.warn(`[notepat-remote] WS connect threw: ${err?.message || err}`);
  }
}

function handleRelay(ev, transport = "ws") {
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
    transport,
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

function boot({ wipe, cursor, hud, send, net }) {
  wipe(10, 12, 22);
  cursor?.("native");
  hud?.label?.("");
  _send = send;
  connectWs();
  // Also open AC's session-scoped socket + udp purely so the transport
  // status indicator can show UDP connectivity. Callbacks are no-ops —
  // the notepat:midi subscription flows through the raw WS above.
  try {
    netUdp = net?.udp?.((type, content) => {
      if (type === "notepat:midi") {
        udpRelayCount += 1;
        handleRelay(content, "udp");
      } else if (type === "notepat:midi:subscribed") {
        console.log(`[notepat-remote] UDP subscribe ack`, content);
      }
    });
    console.log(`[notepat-remote] UDP channel opened (via net.udp)`);
  } catch (err) {
    console.warn(`[notepat-remote] UDP open threw: ${err?.message || err}`);
  }
  try {
    netSocket = net?.socket?.(() => {});
  } catch (err) {
    console.warn(`[notepat-remote] session WS open threw: ${err?.message || err}`);
  }
}

let lastStatusLogFrame = -9999;
const STATUS_LOG_INTERVAL = 60 * 3; // every ~3 seconds at 60fps

function sim() {
  frame += 1;
  if (wsState === "closed" && frame >= reconnectAt) connectWs();

  // Subscribe over UDP once the geckos channel comes up. Lost connection
  // resets the flag so we re-subscribe on reconnect.
  if (netUdp?.connected && !udpSubscribed) {
    udpSubscribed = true;
    try {
      netUdp.send("notepat:midi:subscribe", { all: true });
      console.log(`[notepat-remote] UDP subscribed to notepat:midi`);
    } catch (err) {
      console.warn(`[notepat-remote] UDP subscribe failed: ${err?.message || err}`);
      udpSubscribed = false;
    }
  } else if (!netUdp?.connected && udpSubscribed) {
    udpSubscribed = false;
  }

  // Periodic transport heartbeat so Max Console users can see without a
  // UI screenshot whether WS and UDP are actually linked.
  if (frame - lastStatusLogFrame >= STATUS_LOG_INTERVAL) {
    lastStatusLogFrame = frame;
    const udpOk = !!netUdp?.connected;
    const wsOk = wsState === "open";
    console.log(`[notepat-remote] status: WS=${wsState}${wsError?`(${wsError})`:""} UDP=${udpOk?(udpSubscribed?"subscribed":"connected"):"--"} ws-relays=${relayCount - udpRelayCount} udp-relays=${udpRelayCount}`);
  }
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

  // ── Palette ──────────────────────────────────────────────────────────
  // Ableton-tasteful: graphite background, amber/orange accent (Live's
  // MIDI color), muted grays. Unfocused = deeply dimmed + angry red blink
  // so you can't miss it.
  const blinkPhase = (sin(frame * 0.14) + 1) / 2; // 0..1 sinusoidal
  const blinkOn = blinkPhase > 0.5;

  // Focused theme (always-on)
  const focusedBg = [16, 18, 22];
  const focusedAccent = [255, 156, 60];   // Live orange
  const focusedAccentBright = [255, 196, 110];
  const focusedFg = [212, 216, 224];
  const focusedDim = [110, 116, 130];
  const focusedKeyWhite = [38, 42, 50];
  const focusedKeyBlack = [22, 24, 30];
  const focusedOutline = [70, 76, 88];

  // Unfocused theme — dark + red; flashes for attention.
  const unfocusedBg = blinkOn ? [48, 12, 12] : [22, 6, 6];
  const unfocusedAccent = blinkOn ? [255, 70, 70] : [180, 45, 45];
  const unfocusedFg = [180, 150, 150];
  const unfocusedDim = [100, 70, 70];
  const unfocusedKeyWhite = [40, 18, 18];
  const unfocusedKeyBlack = [24, 10, 10];
  const unfocusedOutline = [80, 30, 30];

  const bgBase = focused ? focusedBg : unfocusedBg;
  const accent = focused ? focusedAccent : unfocusedAccent;
  const accentBright = focused ? focusedAccentBright : unfocusedAccent;
  const fg = focused ? focusedFg : unfocusedFg;
  const dim = focused ? focusedDim : unfocusedDim;
  const keyWhite = focused ? focusedKeyWhite : unfocusedKeyWhite;
  const keyBlack = focused ? focusedKeyBlack : unfocusedKeyBlack;
  const outline = focused ? focusedOutline : unfocusedOutline;

  wipe(...bgBase);

  const sinceNote = frame - lastNoteFrame;
  if (lastNote && sinceNote < 10 && focused) {
    const f = 1 - sinceNote / 10;
    ink(...accentBright, floor(40 * f)).box(0, 0, W, H, "fill");
  }

  // ── Header row: piece name + transport state ─────────────────────────
  let y = 2;
  ink(...accent).write("notepat-remote", { x: 4, y });
  // Transport indicator mirrors arena.mjs: UDP (green) > WS (yellow) >
  // OFFLINE (red). UDP means net.udp is live (session geckos channel up);
  // WS means the raw notepat:midi subscription socket is open.
  const udpOk = !!netUdp?.connected;
  const wsOk = wsState === "open";
  const transport = udpOk && wsOk ? "UDP+WS"
                  : udpOk ? "UDP"
                  : wsOk ? "WS"
                  : wsState === "connecting" ? "..."
                  : "OFFLINE";
  const transportColor =
    !focused ? dim :
    udpOk ? [120, 220, 140] :
    wsOk ? [230, 200, 80] :
    wsState === "connecting" ? [255, 200, 90] :
    [255, 100, 100];
  ink(...transportColor).write(transport, { x: W - transport.length * 6 - 4, y });
  y += 10;

  // ── Status row: ACTIVE + octave + last note ──────────────────────────
  if (focused) {
    ink(...accent).box(4, y + 1, 6, 6, "fill");
    ink(...fg).write("ACTIVE", { x: 14, y });
  }
  ink(...dim).write(`oct ${baseOctave}`, { x: 70, y });
  if (lastNote) {
    const noteFresh = sinceNote < 30;
    const pn = pitchName(lastNote.pitch);
    const noteColor = noteFresh ? accent : dim;
    const srcTag =
      lastNote.source === "relay" ? (lastNote.transport === "udp" ? "⚡" : "") + "@" + (lastNote.handle || "?") :
      lastNote.source === "tap" ? "tap" : "kbd";
    const label = `${lastNote.vel === 0 ? "v" : "^"} ${pn} ${srcTag}`;
    ink(...noteColor).write(label, { x: W - label.length * 6 - 4, y });
  }
  y += 10;

  // ── Button grid area: two 4×3 octave blocks side-by-side ─────────────
  // Pixel-perfect: spans full width (0..W), fills from gridTop down to H.
  // Pads butt up against each other — rainbow palette gives enough natural
  // contrast that explicit separators add noise.
  const gridTop = y + 1;
  // Pre-compute row and column extents so every pixel is accounted for and
  // adjacent pads share the same boundary line (no 1px gaps or overlaps).
  const cols = 8; // 4 pads × 2 octave blocks
  const rows = 3;
  const colEdges = [];
  for (let c = 0; c <= cols; c += 1) colEdges.push(floor((c * W) / cols));
  const rowEdges = [];
  for (let r = 0; r <= rows; r += 1) rowEdges.push(gridTop + floor((r * (H - gridTop)) / rows));

  buttons = [];
  for (let octIdx = 0; octIdx < OCTAVE_GRIDS.length; octIdx += 1) {
    const grid = OCTAVE_GRIDS[octIdx];
    const octNum = baseOctave + octIdx;

    for (let rowIdx = 0; rowIdx < grid.length; rowIdx += 1) {
      const row = grid[rowIdx];
      for (let colIdx = 0; colIdx < row.length; colIdx += 1) {
        const key = row[colIdx];
        const offset = rowIdx * 4 + colIdx;
        const pitch = (baseOctave + 1 + octIdx) * 12 + offset;
        const globalCol = octIdx * 4 + colIdx;
        const x0 = colEdges[globalCol];
        const x1 = colEdges[globalCol + 1];
        const y0 = rowEdges[rowIdx];
        const y1 = rowEdges[rowIdx + 1];
        const b = { x: x0, y: y0, w: x1 - x0, h: y1 - y0, key, pitch };
        buttons.push(b);

        const held =
          heldKeys.has(key) || (tappedButton && tappedButton.key === key);
        const recentFlash =
          lastNote && lastNote.pitch === pitch && sinceNote < 18;
        const black = isBlackKey(pitch);

        // Rainbow ROYGBIV per note from note-colors.mjs — matches
        // notepat.mjs so the packed remote visually maps onto the main
        // piece's pad palette. Sharps/flats render black.
        const nameShort = pitchNameShort(pitch);
        const noteOctave = pitchOctave(pitch);
        const baseColor = black
          ? [20, 22, 28]
          : getNoteColorForOctave(nameShort.toLowerCase(), noteOctave, baseOctave);

        let fill;
        if (held && focused) {
          // Fully saturated note color when pressed.
          fill = baseColor;
        } else if (recentFlash && focused) {
          // Note color blended in at recent-flash intensity.
          const f = 1 - sinceNote / 18;
          fill = [
            floor(bgBase[0] + (baseColor[0] - bgBase[0]) * (0.35 + f * 0.5)),
            floor(bgBase[1] + (baseColor[1] - bgBase[1]) * (0.35 + f * 0.5)),
            floor(bgBase[2] + (baseColor[2] - bgBase[2]) * (0.35 + f * 0.5)),
          ];
        } else if (focused) {
          // Resting state: muted version of note color so the rainbow is
          // still readable without overwhelming the unpressed pads.
          fill = [
            floor(bgBase[0] + (baseColor[0] - bgBase[0]) * 0.35),
            floor(bgBase[1] + (baseColor[1] - bgBase[1]) * 0.35),
            floor(bgBase[2] + (baseColor[2] - bgBase[2]) * 0.35),
          ];
        } else {
          fill = black ? keyBlack : keyWhite;
        }
        ink(...fill).box(b.x, b.y, b.w, b.h, "fill");
        // Draw a thin separator on the top and left edges using a darker
        // shade of the pad's own color. This gives a consistent 1px grid
        // line without the awkward outline-over-fill double-draw artifact.
        // Rightmost / bottommost pads skip the edge since the device
        // frame handles that boundary.
        const edgeDark = [
          floor(fill[0] * 0.55),
          floor(fill[1] * 0.55),
          floor(fill[2] * 0.55),
        ];
        if (b.x > 0) ink(...edgeDark).line(b.x, b.y, b.x, b.y + b.h - 1);
        if (b.y > gridTop) ink(...edgeDark).line(b.x, b.y, b.x + b.w - 1, b.y);
        // Held state: bright accent border fully around the pad.
        if (held && focused) {
          ink(...accent).box(b.x, b.y, b.w, b.h, "outline");
        }

        // Main label = note name (C, C#, D…), small hint = keyboard key.
        const label = nameShort;
        const labelX = b.x + floor(b.w / 2) - floor(label.length * 6 / 2);
        const labelY = b.y + floor(b.h / 2) - 7;
        const labelColor =
          held && focused ? [10, 10, 14] :
          black ? [220, 225, 235] : [20, 22, 28];
        ink(...labelColor).write(label, { x: labelX, y: labelY });
        // Keyboard key hint (1 char), bottom-right corner of the pad.
        const hintX = b.x + b.w - 7;
        const hintY = b.y + b.h - 8;
        const hintColor =
          held && focused ? [10, 10, 14, 200] :
          black ? [180, 180, 190, 180] : [40, 44, 52, 180];
        ink(...hintColor).write(key.toUpperCase(), { x: hintX, y: hintY });
      }
    }
  }

  // ── Unfocused overlay: big blinking "TAP ME!" over everything ────────
  if (!focused) {
    // Semi-opaque scrim so the grid visibly darkens
    ink(4, 0, 0, 160).box(0, 0, W, H, "fill");

    // Thick pulsing red border that can't be missed
    const borderAlpha = floor(140 + blinkPhase * 115);
    for (let i = 0; i < 3; i += 1) {
      ink(255, 40, 40, borderAlpha).box(i, i, W - i * 2, H - i * 2, "outline");
    }

    // Huge "TAP ME!" centered in the device
    const msg = "TAP ME!";
    const msgSize = 2; // AC text scaling
    const charW = 6 * msgSize;
    const charH = 10 * msgSize;
    const msgW = msg.length * charW;
    const msgX = floor((W - msgW) / 2);
    const msgY = floor((H - charH) / 2) - 4;
    // Drop shadow
    ink(0, 0, 0, 180).write(msg, { x: msgX + 2, y: msgY + 2, size: msgSize });
    ink(255, blinkOn ? 80 : 40, blinkOn ? 80 : 40)
      .write(msg, { x: msgX, y: msgY, size: msgSize });

    // Subtitle — smaller, italic-y hint
    const sub = "click me to play";
    const subX = floor((W - sub.length * 6) / 2);
    const subY = msgY + charH + 4;
    ink(blinkOn ? 220 : 140, 120, 120)
      .write(sub, { x: subX, y: subY });
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
