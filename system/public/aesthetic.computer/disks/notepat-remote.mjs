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

function clamp8(v) {
  return max(0, min(255, floor(v)));
}

// Subtle warm/cool tint per baseOctave so swapping octaves visibly recolors
// the resting pads + the meter. Negative delta (low octaves) warms toward
// red/yellow, positive delta cools toward blue/cyan. baseline = 4.
function octaveTint(oct) {
  const d = oct - 4;
  return [-d * 5, -d * 2, d * 8];
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
let shiftHeld = false;       // typography case toggle for pad labels
let focused = true;          // assume focused until we learn otherwise
let focusedChangedFrame = 0;
let lastInteractionFrame = -999;

let _send = null;
let frame = 0;

// Smoothly-lerped background color so the notepat-native "color-reactive
// backdrop" vibe carries into the M4L device. Target is derived from the
// most recent note (darkened) and decays back to idle when nothing is held.
const bgColor = [4, 2, 6];

// Case-doors animation. 0 = fully open (focused, pads visible), 1 =
// fully closed (unfocused, dark panels meet at a chrome seam in the
// middle). Lerps each paint toward the focus target.
let doorPhase = 0;

// Build env from bios (populated via e.is("env:info"). null until the
// bridge pushes the info, then:
//   { packMode, packGit, packDate, latestCommit }
let envInfo = null;

// Live track color, pushed in by the Max patcher via
// `window.acSetLiveTrackColor(int)` once `live.observer` resolves the
// device's parent track. Null until that lands; paint falls back to the
// rainbow/ambient scheme when it's null so this stays optional.
let liveTrackColor = null;

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

  // ── Re-request Max-side state from the piece (worker) side ───────
  // Pieces run in a Web Worker — no window/document here. The bios.mjs
  // main-thread bridge defines window.acSetLiveFocus / Live track
  // color handlers that forward into the worker via send() as
  // "focus-change" + "live:track-color" messages, which disk.mjs
  // dispatches as act() events. On boot we ask Max to re-emit both
  // because their initial fires can land before we've mounted.
  send({ type: "daw:request-focus" });
  send({ type: "daw:request-track-color" });
  send({ type: "env:request" });
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

  // (Worker has no document — focus state lands exclusively via the
  // "focus-change" event dispatched by disk.mjs from bios's main-thread
  // bridge, driven by Max [active] or DOM focus/blur.)

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

  // Focus/defocus from disk.mjs's "focus-change" dispatch. bios pushes
  // this both from DOM window focus/blur AND from Max's [active] via
  // window.acSetLiveFocus, so a single event handler covers all paths.
  if (e.is("focus")) {
    if (!focused) {
      focused = true;
      focusedChangedFrame = frame;
      console.log("[focus] focus-change → ACTIVE");
    }
    return;
  }
  if (e.is("defocus")) {
    if (focused) {
      focused = false;
      focusedChangedFrame = frame;
      heldKeys.clear();
      console.log("[focus] focus-change → inactive");
    }
    return;
  }

  // Live track color pushed by Max via bios → disk dispatch.
  if (e.is("live:track-color")) {
    const rgb = e.trackColor;
    if (Array.isArray(rgb) && rgb.length === 3) {
      liveTrackColor = rgb.slice();
      console.log(`[track-color] received rgb(${rgb.join(",")})`);
    }
    return;
  }

  // Build env info from bios (packed vs live + version).
  if (e.is("env:info")) {
    envInfo = e.env || null;
    if (envInfo) {
      console.log(
        `[env] packMode=${envInfo.packMode} packGit=${(envInfo.packGit || "").slice(0, 9)} latest=${(envInfo.latestCommit || "").slice(0, 9)}`,
      );
    }
    return;
  }

  // Button grid taps — tapNote for note pads, daw:transport for the
  // play/pause overlay (Live grabs spacebar globally so the keyboard
  // path can't reach jweb; this UI button is the fallback).
  if (e.is("touch")) {
    lastInteractionFrame = frame;
    const hit = hitButton(e.x, e.y);
    if (hit) {
      tappedButton = hit;
      if (hit.kind === "transport") {
        if (_send) _send({ type: "daw:transport" });
      } else {
        tapNote(hit.pitch, true);
      }
    }
    return;
  }
  if (e.is("lift")) {
    if (tappedButton) {
      if (tappedButton.kind !== "transport") {
        tapNote(tappedButton.pitch, false);
      }
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
    if (hit && hit.kind !== "transport" &&
        (!tappedButton || tappedButton.kind === "transport" || hit.pitch !== tappedButton.pitch)) {
      if (tappedButton && tappedButton.kind !== "transport") tapNote(tappedButton.pitch, false);
      tappedButton = hit;
      tapNote(hit.pitch, true);
    }
    return;
  }

  // Shift tracking — flips pad labels between lowercase (resting) and
  // uppercase (shift held) so the keyboard state is visible on the pads.
  if (e.is("keyboard:down:shift")) {
    shiftHeld = true;
    return;
  }
  if (e.is("keyboard:up:shift")) {
    shiftHeld = false;
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
  // Idle backdrop: biases toward a deeply darkened track color when
  // Max has pushed one, otherwise a flat warm near-black. Keeps the
  // track's flavor alive even when nothing is being played.
  const BG_IDLE = liveTrackColor
    ? [floor(liveTrackColor[0] * 0.08), floor(liveTrackColor[1] * 0.08), floor(liveTrackColor[2] * 0.08)]
    : [4, 2, 6];

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
  const bezel = 2;            // visible bezel thickness
  const TOP_METER_H = 4;      // octave-meter strip lives above the grid
  const TOP_INSET = bezel + TOP_METER_H;  // grid starts below meter
  const SIDE_INSET = 3;       // bezel + 1px bg gap, matches pad-gap-between
  const BOTTOM_INSET = 3;
  const innerW = W - SIDE_INSET * 2;
  const innerH = H - TOP_INSET - BOTTOM_INSET;
  const colEdges = [];
  for (let c = 0; c <= cols; c += 1) colEdges.push(SIDE_INSET + floor((c * innerW) / cols));
  // rowHeights distribute the available vertical space evenly and
  // absorb the leftover px into the first few rows so the grid ends
  // flush with (H - BOTTOM_INSET). Octave gap is inserted between row 2
  // and row 3.
  const avail = innerH - octaveGap;
  const rowBase = floor(avail / rows);
  const rowRem = avail - rowBase * rows;
  const rowHeights = [];
  for (let r = 0; r < rows; r += 1) rowHeights.push(rowBase + (r < rowRem ? 1 : 0));
  const rowEdges = [TOP_INSET];
  for (let r = 0; r < rows; r += 1) {
    const gap = r === 3 ? octaveGap : 0;
    rowEdges.push(rowEdges[r] + rowHeights[r] + gap);
  }

  // Octave tint reused by rest pads + meter — keeps the two visually linked.
  const octTint = octaveTint(baseOctave);

  buttons = [];
  // Play/pause button — top-right corner overlay. Live's host window
  // claims spacebar before jweb sees the keydown, so the keyboard
  // route can't reach this iframe. A tappable button is the only
  // way to toggle Live's transport from inside the device panel.
  // Pushed first so hitButton matches it before the underlying pad.
  const tBtnW = 14, tBtnH = 11;
  const transportBtn = {
    kind: "transport",
    x: W - tBtnW - 3,
    y: bezel + 1,
    w: tBtnW,
    h: tBtnH,
  };
  buttons.push(transportBtn);

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
          // sharps. When Max has pushed a track color, blend a touch of
          // it into the rest fill so every pad carries the flavor of
          // the hosting track (subtle — pads still read as piano keys).
          const base = black ? PAD_SHARP : IVORY;
          if (liveTrackColor) {
            const mix = black ? 0.22 : 0.12;
            fill = [
              floor(base[0] + (liveTrackColor[0] - base[0]) * mix),
              floor(base[1] + (liveTrackColor[1] - base[1]) * mix),
              floor(base[2] + (liveTrackColor[2] - base[2]) * mix),
            ];
          } else {
            fill = base.slice();
          }
          // Subtle octave tint — black keys get ~⅓ the shift so they
          // don't drift away from "near-black".
          const tintScale = black ? 0.35 : 1;
          fill = [
            clamp8(fill[0] + octTint[0] * tintScale),
            clamp8(fill[1] + octTint[1] * tintScale),
            clamp8(fill[2] + octTint[2] * tintScale),
          ];
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
        // Lowercase at rest, uppercase while Shift is held — echoes the
        // actual keyboard state on the pad face.
        const label = shiftHeld ? key.toUpperCase() : key.toLowerCase();
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

  // ── Octave divider ──────────────────────────────────────────────
  // Pulled out of the focused guard so the chrome stays consistent
  // even during the door transition.
  const chrome = liveTrackColor || [150, 80, 110];
  {
    const barY = rowEdges[3] - octaveGap;
    ink(...chrome).box(bezel, barY, W - bezel * 2, octaveGap, "fill");
  }

  // ── Case doors (unfocused overlay) ─────────────────────────────
  // Instrument-case metaphor: when the piece loses focus, two panels
  // slide in from the left and right and meet at a chrome seam. When
  // focused the panels retract off-screen and the pads are fully
  // visible. Lerps smoothly so focus transitions animate.
  const doorTarget = focused ? 0 : 1;
  doorPhase += (doorTarget - doorPhase) * 0.18;
  if (doorPhase > 0.01) {
    const halfW = floor(W / 2);
    const doorW = floor(halfW * doorPhase);
    // Translucent panel — pads ghost through so the layout stays legible
    // even when the case is closed. Alpha leans dark; bright/held pads
    // still poke through clearly.
    const panel = [18, 12, 18, 195];
    ink(...panel).box(0, 0, doorW, H, "fill");
    ink(...panel).box(W - doorW, 0, doorW, H, "fill");
    // Inner edges of each panel get a chrome stripe that matches the
    // bezel/divider color — becomes a 2px center seam when closed.
    // Drawn opaque so the seam reads cleanly against the ghosted pads.
    if (doorW > 0) {
      ink(...chrome).line(doorW - 1, 0, doorW - 1, H - 1);
      ink(...chrome).line(W - doorW, 0, W - doorW, H - 1);
    }

    // Once doors are nearly closed, display the piece's diagnostic
    // readout on top of them: transport online state + build mode +
    // staleness hint if the packed amxd is behind the live commit.
    if (doorPhase > 0.85) {
      const readoutAlpha = floor((doorPhase - 0.85) / 0.15 * 255);
      const udpOk = !!netUdp?.connected;
      const wsOk = wsState === "open";
      const online = udpOk || wsOk;
      const transportLine = online
        ? (udpOk && wsOk ? "ONLINE · UDP+WS" : udpOk ? "ONLINE · UDP" : "ONLINE · WS")
        : "OFFLINE";
      const transportColor = online ? [130, 230, 160] : [255, 110, 110];
      const modeLine = envInfo
        ? (envInfo.packMode ? "PACKED" : "LIVE")
        : "…";
      const stale = envInfo && envInfo.packMode && envInfo.packGit &&
        envInfo.latestCommit && envInfo.packGit !== envInfo.latestCommit;

      // Centered three-row readout, MatrixChunky8 keeps it legible at W≈150.
      const lineH = 9;
      const totalH = stale ? lineH * 3 + 4 : lineH * 2 + 2;
      let ty = floor((H - totalH) / 2);
      const writeCentered = (text, color, font) => {
        const charW = font === "MatrixChunky8" ? 5 : 6;
        const tx = floor((W - text.length * charW) / 2);
        ink(...color, readoutAlpha).write(
          text, { x: tx, y: ty },
          undefined, undefined, false, font,
        );
        ty += lineH;
      };
      writeCentered(transportLine, transportColor, "MatrixChunky8");
      writeCentered(modeLine, chrome, "MatrixChunky8");
      if (stale) {
        ty += 2;
        writeCentered("UPDATE AVAILABLE", [255, 180, 90], "MatrixChunky8");
      }
    }
  }

  // ── Octave meter (top strip, above bezel) ────────────────────────
  // 9 dashes for octaves 1-9 with the current one highlighted. No
  // text/numbers — the dashes are a non-verbal level indicator that
  // shifts color slightly per octave to match the pad rest tones.
  {
    const NUM_OCTAVES = 9;
    const meterY = bezel;
    const meterX = bezel + 1;
    const meterEndX = W - bezel - 1;
    const meterW = max(NUM_OCTAVES * 2, meterEndX - meterX);
    // Stable dark backdrop so the meter doesn't flicker with bg note color.
    ink(6, 4, 10).box(meterX, meterY, meterW, TOP_METER_H, "fill");
    const segGap = 1;
    const segW = max(1, floor((meterW - segGap * (NUM_OCTAVES - 1)) / NUM_OCTAVES));
    const dashH = 2;
    const dashY = meterY + floor((TOP_METER_H - dashH) / 2);
    for (let n = 1; n <= NUM_OCTAVES; n += 1) {
      const sx = meterX + (n - 1) * (segW + segGap);
      const tint = octaveTint(n);
      const isCur = n === baseOctave;
      if (isCur) {
        // Active dash: brighter, taller, tinted with the octave's tone.
        const ah = TOP_METER_H;
        const col = [clamp8(220 + tint[0]), clamp8(220 + tint[1]), clamp8(230 + tint[2])];
        ink(...col).box(sx, meterY, segW, ah, "fill");
      } else {
        const col = [clamp8(60 + tint[0]), clamp8(50 + tint[1]), clamp8(70 + tint[2])];
        ink(...col).box(sx, dashY, segW, dashH, "fill");
      }
    }
  }

  // ── Transport play/pause overlay (top-right corner) ──────────────
  // Tap target — sends `daw:transport` through bios → max → LiveAPI
  // toggle. Stays above the pads so it's reachable even with the
  // unfocus doors closing in. Visually: dark cell with a chrome
  // outline + a play triangle that tints when pressed.
  {
    const t = transportBtn;
    const pressed = !!(tappedButton && tappedButton.kind === "transport");
    const bgFill = pressed ? [120, 80, 50] : [22, 14, 22];
    const triColor = pressed ? [255, 220, 180] : [220, 230, 240];
    ink(...bgFill).box(t.x, t.y, t.w, t.h, "fill");
    ink(...chrome).box(t.x, t.y, t.w, t.h, "outline");
    // Filled play triangle pointing right — built from 1px scanlines.
    const padX = 3, padY = 2;
    const triX = t.x + padX;
    const triY = t.y + padY;
    const triW = max(1, t.w - padX * 2);
    const triH = max(1, t.h - padY * 2);
    const half = (triH - 1) / 2 || 1;
    for (let dy = 0; dy < triH; dy += 1) {
      const d = abs(dy - half);
      const w = max(1, floor(triW * (1 - d / half)));
      ink(...triColor).box(triX, triY + dy, w, 1, "fill");
    }
  }

  // ── Device bezel (drawn last, always on top) ─────────────────────
  // The "case frame" — stays visible whether or not the doors are
  // closed, so the device outline is always present.
  for (let i = 0; i < bezel; i += 1) {
    ink(...chrome).box(i, i, W - i * 2, H - i * 2, "outline");
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
