// radio.mjs — Live internet radio for AC Native.
// Corollary to dj.mjs: instead of USB tracks it tunes the same Menu Band
// stations (KPBJ, r8dio, NTS 1/2) through the DJ deck, which now streams
// http(s) URLs. The deck has no peaks/duration for a live stream, so the
// visualizer is a scrolling scope of the actual output (sound.speaker).

let T = __theme.update();

// Station list — mirrors slab/menuband KPBJRadioStream.swift. All plain
// Icecast/MP3 (NTS + r8dio 302 to the stream; the deck follows redirects).
const STATIONS = [
  { id: "kpbj",  label: "KPBJ",  name: "KPBJ.FM",  url: "https://stream.kpbj.fm/" },
  { id: "r8dio", label: "R8DIO", name: "r8dio.dk", url: "https://s3.radio.co/s7cd1ffe2f/listen" },
  { id: "nts1",  label: "NTS1",  name: "NTS 1",    url: "https://stream-relay-geo.ntslive.net/stream" },
  { id: "nts2",  label: "NTS2",  name: "NTS 2",    url: "https://stream-relay-geo.ntslive.net/stream2" },
];

let stationIdx = 0;
let state = "idle";        // idle | tuning | playing | error
let message = "";
let messageFrame = 0;
let frame = 0;
let buttons = [];          // station chips, computed in paint
let scopeOffset = 0;       // viewOffsetSec for drawStrip (0 = live edge)

function msg(t) { message = t; messageFrame = frame; }

function tune(sound, idx) {
  if (STATIONS.length === 0) return;
  stationIdx = (idx + STATIONS.length) % STATIONS.length;
  const st = STATIONS[stationIdx];
  state = "tuning";
  msg("tuning " + st.name + "…"); // load blocks briefly on the network connect
  const ok = sound?.deck?.load(0, st.url);
  if (ok) {
    sound.deck.play(0);
    sound.deck.setSpeed?.(0, 1);
    state = "playing";
    msg(st.name);
    sound?.speak?.(st.name);
  } else {
    state = "error";
    msg("can't reach " + st.name);
  }
}

function boot({ sound }) {
  tune(sound, stationIdx);
}

function act({ event: e, sound, system }) {
  const dk = sound?.deck;
  const d = dk?.decks?.[0];

  // Station chip taps.
  if (e.is("touch")) {
    for (const b of buttons) {
      if (e.x >= b.x && e.x <= b.x + b.w && e.y >= b.y && e.y <= b.y + b.h) {
        tune(sound, b.idx);
        return;
      }
    }
  }

  if (!e.is("keyboard:down")) return;
  if (e.is("keyboard:down:escape")) { system?.jump?.("prompt"); return; }

  // Space: stop / resume the stream.
  if (e.is("keyboard:down:space")) {
    if (d?.loaded) {
      if (d.playing) { dk.pause(0); state = "idle"; msg("stopped"); }
      else { dk.play(0); state = "playing"; msg(STATIONS[stationIdx].name); }
    }
    return;
  }

  // Next / prev station.
  if (e.is("keyboard:down:n") || e.is("keyboard:down:arrowright") ||
      e.is("keyboard:down:arrowdown")) { tune(sound, stationIdx + 1); return; }
  if (e.is("keyboard:down:p") || e.is("keyboard:down:arrowleft") ||
      e.is("keyboard:down:arrowup")) { tune(sound, stationIdx - 1); return; }

  // Volume.
  if (e.is("keyboard:down:-")) { if (d) dk.setVolume(0, Math.max(0, d.volume - 0.05)); return; }
  if (e.is("keyboard:down:=")) { if (d) dk.setVolume(0, Math.min(1, d.volume + 0.05)); return; }

  // Number keys 1-4 jump straight to a station.
  for (let i = 0; i < STATIONS.length && i < 9; i++) {
    if (e.is("keyboard:down:" + (i + 1))) { tune(sound, i); return; }
  }
}

function paint({ wipe, ink, box, write, screen, sound }) {
  frame++;
  T = __theme.update();
  const w = screen.width, h = screen.height;
  const F = "font_1";
  const CW = 6;
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const d = sound?.deck?.decks?.[0] || {};
  const fg = T.fg || 200;
  const dim = T.fgMute || 60;
  const st = STATIONS[stationIdx] || { name: "—", label: "—" };

  // Title.
  ink(fg, fg, fg);
  write("ac/radio", { x: 4, y: 6, size: 2, font: "matrix" });

  // LIVE tag (pulses while playing).
  if (d.playing && d.isStream) {
    const p = Math.floor(160 + 80 * Math.sin(frame * 0.15));
    ink(p, 40, 40);
    write("● LIVE", { x: w - 44, y: 8, size: 1, font: F });
  } else {
    ink(dim, dim, dim);
    write(state === "tuning" ? "tuning" : "○ off", { x: w - 44, y: 8, size: 1, font: F });
  }

  // Station name.
  ink(T.accent[0], T.accent[1], T.accent[2]);
  write(st.name, { x: 4, y: 28, size: 1, font: F });

  // --- Live output scope (fills the mid band) ---
  // drawStrip renders the actual mixed output — the radio audio shows here
  // since the deck mixes into master before capture.
  const scopeY = 42;
  const scopeH = h - scopeY - 46;
  if (sound?.speaker?.drawStrip && scopeH > 8) {
    sound.speaker.drawStrip(4, scopeY, w - 8, scopeH, 2.4, 1.0, scopeOffset);
  } else {
    // Fallback: 32-sample bars.
    const wf = sound?.speaker?.waveforms?.left;
    if (wf) {
      const mid = scopeY + scopeH / 2;
      ink(T.ok[0], T.ok[1], T.ok[2]);
      const step = (w - 8) / wf.length;
      for (let i = 0; i < wf.length; i++) {
        const a = (wf[i] || 0) * (scopeH / 2);
        box(4 + Math.floor(i * step), Math.floor(mid - a), 2, Math.max(1, Math.floor(a * 2)));
      }
    }
  }
  // Scope frame.
  ink(T.border[0], T.border[1], T.border[2]);
  box(4, scopeY, w - 8, scopeH, "outline");

  // Error readout.
  if (state === "error" || (d.error && d.error.length > 0)) {
    ink(T.err[0], T.err[1], T.err[2]);
    write((d.error || "stream error").slice(0, Math.floor(w / CW) - 2),
          { x: 4, y: scopeY + scopeH + 2, size: 1, font: F });
  } else {
    // Volume readout.
    ink(T.fgDim, T.fgDim, T.fgDim);
    const vol = `vol ${Math.round((d.volume ?? 1) * 100)}%`;
    write(vol, { x: 4, y: scopeY + scopeH + 2, size: 1, font: F });
  }

  // --- Station selector chips ---
  buttons = [];
  const chipY = h - 14;
  const chipH = 12;
  const gap = 3;
  const totalGap = gap * (STATIONS.length - 1);
  const chipW = Math.floor((w - 8 - totalGap) / STATIONS.length);
  for (let i = 0; i < STATIONS.length; i++) {
    const bx = 4 + i * (chipW + gap);
    const active = i === stationIdx;
    buttons.push({ x: bx, y: chipY, w: chipW, h: chipH, idx: i });
    ink(active ? T.accent[0] : T.bar[0], active ? T.accent[1] : T.bar[1], active ? T.accent[2] : T.bar[2]);
    box(bx, chipY, chipW, chipH);
    ink(T.border[0], T.border[1], T.border[2]);
    box(bx, chipY, chipW, chipH, "outline");
    const lbl = STATIONS[i].label;
    const lx = bx + Math.floor((chipW - lbl.length * CW) / 2);
    ink(active ? T.bg[0] : fg, active ? T.bg[1] : fg, active ? T.bg[2] : fg);
    write(lbl, { x: lx, y: chipY + 2, size: 1, font: F });
  }

  // Toast.
  if (message && frame - messageFrame < 150) {
    const fade = Math.max(0, 255 - Math.floor((frame - messageFrame) * 2));
    ink(T.warn[0], T.warn[1], T.warn[2], fade);
    write(message.slice(0, Math.floor(w / CW) - 2), { x: 4, y: 18, size: 1, font: F });
  }
}

function sim({ sound }) {
  // Surface a dropped stream (decoder latched after sustained failure).
  const d = sound?.deck?.decks?.[0];
  if (d?.error && d.error.length > 0 && state === "playing") {
    state = "error";
    msg(d.error);
  }
}

function leave() {}

export { boot, act, paint, sim, leave };
