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
// Row 0 = low (C..D#), row 2 = high (G#..B). Two blocks (base + base+1)
// stack vertically in paint().
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

// Smoothly-lerped background color so the notepat-native "color-reactive
// backdrop" vibe carries into the M4L device. Target is derived from the
// most recent note (darkened) and decays back to idle when nothing is held.
const bgColor = [4, 2, 6];

// Live track color, pushed in by the Max patcher via
// `window.acSetLiveTrackColor(int)` once `live.observer` resolves the
// device's parent track. Null until that lands; paint falls back to the
// rainbow/ambient scheme when it's null so this stays optional.
let liveTrackColor = null;
let lastFocusPollFrame = -9999;

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
  // Obvious fingerprint so the Max Console shows which version actually
  // loaded — live URL (aesthetic.computer) is served by lith and can be
  // stale if deploy hasn't run; offline chunked bundle is always latest.
  try {
    console.log(
      `[notepat-remote] boot build=2026-04-24-focus-hooks ` +
      `packed=${!!window.acPACK_MODE} forceDaw=${!!window.acFORCE_DAW}`,
    );
  } catch {}
  wipe(10, 12, 22);
  cursor?.("native");
  hud?.label?.("");
  _send = send;
  connectWs();

  // ── Focus detection ────────────────────────────────────────────────
  // jweb inside Max for Live doesn't always surface DOM blur/focus as AC
  // events. We listen on four paths: (1) AC act() events, (2) native
  // window blur/focus, (3) document visibilitychange, (4) Max [active]
  // object pushing via window.acSetLiveFocus. Each transition logs its
  // source so the Max Console shows which hook actually catches it.
  if (typeof window !== "undefined") {
    const setFocus = (f, source) => {
      if (focused !== f) {
        focused = f;
        focusedChangedFrame = frame;
        if (!f) heldKeys.clear();
        try { console.log(`[focus] ${source} → ${f ? "ACTIVE" : "inactive"}`); } catch {}
      }
    };
    try {
      window.addEventListener("blur", () => setFocus(false, "window.blur"));
      window.addEventListener("focus", () => setFocus(true, "window.focus"));
      document.addEventListener?.("visibilitychange", () =>
        setFocus(!document.hidden, "document.visibilitychange"));
    } catch {}
    // Max side pushes focus + theme state via these globals. Defining
    // them unconditionally lets the patcher call them whenever it likes.
    window.acSetLiveFocus = (f) => {
      try { console.log(`[focus] max.active received ${f}`); } catch {}
      setFocus(!!f, "max.active");
    };
    window.acSetLiveTrackColor = (colorInt) => {
      const n = Number(colorInt) >>> 0;
      if (!Number.isFinite(n)) return;
      liveTrackColor = [(n >> 16) & 255, (n >> 8) & 255, n & 255];
    };
    // Pull the current track color now — the Max patcher's
    // `live.observer` fires once on device load (before this boot
    // runs), so without an explicit re-request we'd never see it.
    try { window.max?.outlet?.("requestTrackColor", 1); } catch {}
    // Same story for the [active] focus signal — the initial fire
    // can happen before jweb navigates to the live URL.
    try { window.max?.outlet?.("requestFocus", 1); } catch {}
  }
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

  // Poll document.hasFocus() every ~250ms as a safety net — some jweb
  // focus transitions (user clicks the Live mixer header, browses a
  // menu) don't dispatch blur/focus events. Polling catches those.
  if (frame - lastFocusPollFrame > 15) {
    lastFocusPollFrame = frame;
    if (typeof document !== "undefined" && typeof document.hasFocus === "function") {
      const hf = document.hasFocus();
      if (hf !== focused) {
        focused = hf;
        focusedChangedFrame = frame;
        if (!hf) heldKeys.clear();
        try { console.log(`[focus] document.hasFocus() → ${hf ? "ACTIVE" : "inactive"}`); } catch {}
      }
    }
  }

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
    if (!focused) {
      focused = true;
      focusedChangedFrame = frame;
      try { console.log("[focus] ac-event:focus → ACTIVE"); } catch {}
    }
    return;
  }
  if (e.is("blur")) {
    if (focused) {
      focused = false;
      focusedChangedFrame = frame;
      heldKeys.clear();
      try { console.log("[focus] ac-event:blur → inactive"); } catch {}
    }
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
    // `buttons` is rebuilt every paint, so `hit` is a fresh object even when
    // the user hasn't moved off the current pad — compare by pitch instead
    // of object identity, otherwise every draw event retriggers the note.
    const hit = hitButton(e.x, e.y);
    if (hit && (!tappedButton || hit.pitch !== tappedButton.pitch)) {
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
  // Ported from notepat.mjs (web) + fedac/native/pieces/notepat.mjs so the
  // M4L device shares the "notepat look" instead of its own Ableton-orange
  // identity: warm near-black backdrop that color-reacts to incoming notes
  // (native does this on its full screen via `wipe(bgColor)`), burgundy-
  // graphite bar tones, and off-white chrome. Held pads glow in their own
  // rainbow note color — no global accent color to clash with Live's UI.
  const blinkPhase = (sin(frame * 0.14) + 1) / 2; // 0..1 sinusoidal
  const blinkOn = blinkPhase > 0.5;

  // Native's dark palette (fedac/native/pieces/notepat.mjs paint()).
  const BAR_BG = [35, 20, 30];
  const BAR_BORDER = [55, 35, 45];
  const PAD_SHARP = [18, 18, 20];        // black-key rest fill
  const IVORY = [240, 232, 215];         // white-key rest fill (piano-ivory)
  const BG_IDLE = [4, 2, 6];             // near-black when nothing playing

  // Focused theme — notepat palette, rainbow accent per note.
  const focusedFg = [220, 220, 220];
  const focusedDim = [130, 125, 130];

  // Unfocused theme — dark + red; flashes for attention.
  const unfocusedBg = blinkOn ? [48, 12, 12] : [22, 6, 6];
  const unfocusedAccent = blinkOn ? [255, 70, 70] : [180, 45, 45];
  const unfocusedFg = [180, 150, 150];
  const unfocusedDim = [100, 70, 70];
  const unfocusedKeyBlack = [24, 10, 10];
  const unfocusedKeyWhite = [40, 18, 18];

  const sinceNote = frame - lastNoteFrame;

  // Compute the target backdrop color (notepat-native style: darkened note
  // color when a note's active/fresh, otherwise fade toward BG_IDLE).
  const BG_DECAY = 48; // frames a note keeps tinting the backdrop
  let bgTarget;
  if (focused && lastNote && sinceNote < BG_DECAY) {
    const lastPitchName = pitchNameShort(lastNote.pitch).toLowerCase();
    const lastOct = pitchOctave(lastNote.pitch);
    const lastColor = getNoteColorForOctave(lastPitchName, lastOct, baseOctave);
    // Darker when old, brighter when fresh; caps at 0.42 so the bg never
    // competes with pad legibility.
    const freshness = max(0, 1 - sinceNote / BG_DECAY);
    const darken = 0.18 + freshness * 0.24;
    bgTarget = [
      floor(BG_IDLE[0] + (lastColor[0] * darken - BG_IDLE[0]) * freshness),
      floor(BG_IDLE[1] + (lastColor[1] * darken - BG_IDLE[1]) * freshness),
      floor(BG_IDLE[2] + (lastColor[2] * darken - BG_IDLE[2]) * freshness),
    ];
  } else if (!focused) {
    bgTarget = unfocusedBg;
  } else {
    bgTarget = BG_IDLE;
  }
  // Smooth lerp into target — snap on held (currently-pressed) notes.
  const hasHeld = focused && (heldKeys.size > 0 || !!tappedButton);
  const lerp = hasHeld ? 1 : 0.25;
  bgColor[0] += (bgTarget[0] - bgColor[0]) * lerp;
  bgColor[1] += (bgTarget[1] - bgColor[1]) * lerp;
  bgColor[2] += (bgTarget[2] - bgColor[2]) * lerp;
  const bgBase = [floor(bgColor[0]), floor(bgColor[1]), floor(bgColor[2])];

  // Fallback-ish "accent" — used only where chrome needs a non-fg color
  // (held pad borders); derived from the last note so it still tracks the
  // rainbow vibe. When nothing's been played, use a warm off-white.
  const lastNoteColor = lastNote
    ? getNoteColorForOctave(pitchNameShort(lastNote.pitch).toLowerCase(), pitchOctave(lastNote.pitch), baseOctave)
    : focusedFg;
  const accent = focused ? lastNoteColor : unfocusedAccent;
  const fg = focused ? focusedFg : unfocusedFg;
  const dim = focused ? focusedDim : unfocusedDim;
  const keyBlack = focused ? PAD_SHARP : unfocusedKeyBlack;

  wipe(...bgBase);

  // ── Button grid: 4 cols × 6 rows, fills the entire device ─────────
  // Base octave on top (rows 0-2), +1 on bottom (rows 3-5). No margins:
  // pads run edge-to-edge horizontally and vertically. A horizontal
  // divider bar sits between the two octave blocks — same stroke as
  // BAR_BORDER so it reads as a seam, not a void.
  const cols = 4;
  const rows = 6;
  const octaveGap = 2;
  const bezel = 2;      // visible bezel thickness
  const gridInset = 3;  // bezel + 1px bg so cell-gap-to-bezel matches pad-gap-between (both 2px)
  // colEdges span the inner area (inside bezel + 1px bg) exactly; last
  // cell absorbs any horizontal rounding remainder.
  const innerW = W - gridInset * 2;
  const innerH = H - gridInset * 2;
  const colEdges = [];
  for (let c = 0; c <= cols; c += 1) colEdges.push(gridInset + floor((c * innerW) / cols));
  // rowHeights distribute the available vertical space evenly and
  // absorb the leftover px into the first few rows so the grid ends
  // flush with (H - gridInset). Octave gap is inserted between row 2
  // and row 3.
  const avail = innerH - octaveGap;
  const rowBase = floor(avail / rows);
  const rowRem = avail - rowBase * rows;
  const rowHeights = [];
  for (let r = 0; r < rows; r += 1) rowHeights.push(rowBase + (r < rowRem ? 1 : 0));
  const rowEdges = [gridInset];
  for (let r = 0; r < rows; r += 1) {
    const gap = r === 3 ? octaveGap : 0;
    rowEdges.push(rowEdges[r] + rowHeights[r] + gap);
  }

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
        // Stacked layout: base octave on top (globalRow 0-2),
        // +1 octave below (globalRow 3-5). 4 cols per row.
        const globalCol = colIdx;
        const globalRow = octIdx * 3 + rowIdx;
        const x0 = colEdges[globalCol];
        const x1 = colEdges[globalCol + 1];
        const y0 = rowEdges[globalRow];
        // rowHeights absorbs the vertical rounding remainder so the
        // grid fills H precisely without dead pixels at the bottom.
        const b = { x: x0, y: y0, w: x1 - x0, h: rowHeights[globalRow], key, pitch };
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
        // Black keys use native's PAD_SHARP so the "sharp" rest tone is
        // tuned to the rest of the palette; white keys keep the rainbow.
        const baseColor = black
          ? PAD_SHARP
          : getNoteColorForOctave(nameShort.toLowerCase(), noteOctave, baseOctave);

        let fill;
        if (held && focused) {
          if (black) {
            // Black keys use PAD_SHARP at rest — if we kept that for the
            // held state they'd never visibly change. Brighten + warm-
            // tint using the live track color (when Max has pushed it)
            // or a muted purple fallback so the press actually reads.
            const tint = liveTrackColor || [120, 90, 140];
            fill = [
              floor(55 + tint[0] * 0.35),
              floor(55 + tint[1] * 0.35),
              floor(55 + tint[2] * 0.35),
            ];
          } else {
            // White keys: fully saturated rainbow note color on press.
            fill = baseColor;
          }
        } else if (recentFlash && focused) {
          // Recent-flash: blend the rest tone (ivory/PAD_SHARP) toward
          // the note color. Fades out over 18 frames.
          const restTone = black ? PAD_SHARP : IVORY;
          const f = 1 - sinceNote / 18;
          fill = [
            floor(restTone[0] + (baseColor[0] - restTone[0]) * (0.35 + f * 0.5)),
            floor(restTone[1] + (baseColor[1] - restTone[1]) * (0.35 + f * 0.5)),
            floor(restTone[2] + (baseColor[2] - restTone[2]) * (0.35 + f * 0.5)),
          ];
        } else if (focused) {
          // Resting state: piano-key look — ivory whites, near-black
          // sharps. The rainbow only comes out when a note is active.
          fill = black ? PAD_SHARP : IVORY;
        } else {
          fill = black ? keyBlack : unfocusedKeyWhite;
        }
        // Inset the drawn pad inside the cell so adjacent pads show a 1px
        // background gap between them. Hit area stays the full cell.
        const gap = 1;
        const px = b.x + gap;
        const py = b.y + gap;
        const pw = max(1, b.w - gap * 2);
        const ph = max(1, b.h - gap * 2);

        ink(...fill).box(px, py, pw, ph, "fill");
        // Held pads get a bright accent outline; resting pads are flat
        // (the 2px bg gap between cells is the separator). Stacking an
        // outline on every pad doubled up against the neighbor's gap
        // and the device bezel, making edges read as "off by a pixel".
        if (held && focused) {
          ink(...accent).box(px, py, pw, ph, "outline");
        }

        // Main label = keyboard key you actually press. Note name is
        // secondary — the QWERTY letter is what turns the pad into a
        // "playable thing"; pitch info is pinned as a tiny MatrixChunky8
        // hint in the bottom-right.
        const charW = 6;
        const charH = 10;
        const label = key.toUpperCase();
        const labelW = label.length * charW;
        const labelX = px + floor((pw - labelW) / 2);
        const labelY = py + floor((ph - charH) / 2);
        // Shake: 2px jitter while held, 1px briefly after release.
        const shakeAmount = held && focused
          ? 2
          : recentFlash && focused && sinceNote < 6
          ? 1
          : 0;
        const shakeX = shakeAmount > 0
          ? floor((Math.random() - 0.5) * shakeAmount * 2)
          : 0;
        const shakeY = shakeAmount > 0
          ? floor((Math.random() - 0.5) * shakeAmount * 2)
          : 0;
        // Black keys stay dark when held (fill ≈ graphite), so dark label
        // text disappears. Keep the light label for black keys even in
        // the held state; only white keys (bright rainbow fill) flip to
        // dark text on press.
        const labelColor =
          held && focused && !black ? [10, 10, 14] :
          black ? [220, 225, 235] : [20, 22, 28];
        // Drop shadow — opposite luminance of the label, slight offset.
        const shadowColor = labelColor[0] < 128 ? [240, 240, 240, 140] : [0, 0, 0, 180];
        ink(...shadowColor).write(label, { x: labelX + shakeX + 1, y: labelY + shakeY + 1 });
        ink(...labelColor).write(label, { x: labelX + shakeX, y: labelY + shakeY });

        // Tiny note-name hint (C, C#, …) in the bottom-right corner so
        // you can still read the pitch at a glance. MatrixChunky8 keeps
        // it readable at narrow pad widths.
        if (pw >= 14 && ph >= 14) {
          const hintColor =
            held && focused && !black ? [10, 10, 14, 220] :
            black ? [210, 210, 220, 180] : [30, 30, 40, 190];
          ink(...hintColor).write(
            nameShort,
            { x: px + pw - nameShort.length * 5 - 2, y: py + ph - 7 },
            undefined, undefined, false,
            "MatrixChunky8",
          );
        }
      }
    }
  }

  // ── Octave divider + device bezel ───────────────────────────────
  // Both pick up the Live track color when Max has pushed it, so the
  // non-pad chrome responds to the hosting track. Fallback is a warm
  // muted rose rather than a stark gray.
  if (focused) {
    const chrome = liveTrackColor || [150, 80, 110];
    // Divider runs the full inner width (skipping the bezel columns so
    // the horizontal & vertical chrome meet cleanly at the corners).
    const barY = rowEdges[3] - octaveGap;
    ink(...chrome).box(bezel, barY, W - bezel * 2, octaveGap, "fill");
    // 2px bezel as two concentric 1px outlines.
    for (let i = 0; i < bezel; i += 1) {
      ink(...chrome).box(i, i, W - i * 2, H - i * 2, "outline");
    }
  }

  // ── Unfocused overlay: big red X spanning the device ─────────────────
  // When the mjs piece doesn't have focus, the keyboard won't drive
  // notes — the red X is the at-a-glance signal for that.
  if (!focused) {
    // Darken the grid so the X reads as "inactive" not "on top of art"
    ink(4, 0, 0, 150).box(0, 0, W, H, "fill");

    // Pulsing red border — same cue scheme as before, just thinner
    const borderAlpha = floor(140 + blinkPhase * 115);
    for (let i = 0; i < 2; i += 1) {
      ink(255, 40, 40, borderAlpha).box(i, i, W - i * 2, H - i * 2, "outline");
    }

    // Thick red X from corner to corner. AC's line() is 1px, so stack
    // a handful of parallel lines to build a visible stroke.
    const xAlpha = floor(180 + blinkPhase * 75);
    const xThick = 2; // gives a 5px-equivalent X across (t from -2..2)
    for (let t = -xThick; t <= xThick; t += 1) {
      ink(255, 60, 60, xAlpha).line(0, t, W - 1, H - 1 + t);
      ink(255, 60, 60, xAlpha).line(W - 1, t, 0, H - 1 + t);
    }

    // Small hint badge at the top so the reason is readable
    const hint = "click to activate keys";
    const hintW = hint.length * 6;
    const hintBoxX = floor((W - hintW) / 2) - 4;
    const hintBoxY = 2;
    ink(0, 0, 0, 200).box(hintBoxX, hintBoxY, hintW + 8, 12, "fill");
    ink(255, 80, 80, borderAlpha).box(hintBoxX, hintBoxY, hintW + 8, 12, "outline");
    ink(blinkOn ? 255 : 200, 140, 140)
      .write(hint, { x: floor((W - hintW) / 2), y: hintBoxY + 2 });
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
