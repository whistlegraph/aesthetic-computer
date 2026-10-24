// Oskey, 26.10.24
// Oskie's ThinkPad as a standalone instrument. Three modes on F1-F3
// (instrument, drums, sampler), a sequencer view on F4, nine tracks on the
// number row, a 16-step loop that records what you play, and the keyboard
// painted by role so the keycaps can be swapped to match. From Oskie's
// mockup of 2026-10-24 (docs/oskey/).

import { playPercussion, PERCUSSION_NAMES, PERCUSSION_COLORS } from "/lib/percussion.mjs";

const ROWS = ["zxcvbnm,./", "asdfghjkl;'", "qwertyuiop[]"]; // bottom row first
const DIGITS = "1234567890";
const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const SCALES = [
  ["minor", [0, 2, 3, 5, 7, 8, 10]], ["major", [0, 2, 4, 5, 7, 9, 11]],
  ["dorian", [0, 2, 3, 5, 7, 9, 10]], ["mixo", [0, 2, 4, 5, 7, 9, 10]],
  ["penta", [0, 3, 5, 7, 10]], ["harm", [0, 2, 3, 5, 7, 8, 11]],
];
// gm = General MIDI program for the piano engine; type alone = raw oscillator.
const PATCHES = [
  { name: "Saw", type: "sawtooth", a: 0.01, d: 0.12 }, { name: "Square", type: "square", a: 0.01, d: 0.1 },
  { name: "Tri", type: "triangle", a: 0.02, d: 0.15 }, { name: "Sine", type: "sine", a: 0.03, d: 0.2 },
  { name: "Piano", type: "piano", a: 0.005, d: 0.3 }, { name: "E.Piano", type: "piano", gm: 4, a: 0.005, d: 0.3 },
  { name: "Clavi", type: "piano", gm: 7, a: 0.003, d: 0.1 }, { name: "Organ", type: "piano", gm: 16, a: 0.02, d: 0.05 },
  { name: "Bass", type: "piano", gm: 38, a: 0.005, d: 0.2 }, { name: "Strings", type: "piano", gm: 48, a: 0.15, d: 0.5 },
  { name: "Lead", type: "piano", gm: 81, a: 0.01, d: 0.1 }, { name: "Pad", type: "piano", gm: 89, a: 0.3, d: 0.8 },
  { name: "Kalimba", type: "piano", gm: 108, a: 0.003, d: 0.4 }, { name: "Steel", type: "piano", gm: 114, a: 0.003, d: 0.4 },
];
const KIT = ["c", "d", "e", "f", "g", "a", "b", "c#", "d#", "f#", "g#", "a#"]; // left-hand pads
const ZOO = ["bird", "cat", "cow", "dog", "frog", "horse", "lion", "owl", "pig", "sheep", "snake", "whale"];
const TRACK_NAMES = ["Instrument", "Drums", "Sampler", "Loop A", "Loop B", "Empty", "Empty", "Empty", "Empty"];
const TRACK_RGB = [[90, 200, 90], [150, 150, 150], [90, 150, 230], [150, 90, 50], [230, 120, 200],
                   [200, 50, 130], [120, 200, 230], [60, 90, 230], [230, 200, 60]];
const DEGREE_RGB = [[230, 60, 60], [230, 140, 50], [230, 210, 60], [80, 200, 90], [60, 200, 200], [70, 110, 230], [170, 80, 220]];
const FX = [["room", 1], ["echo", 2], ["glitch", 3]];
const MODES = ["inst", "drums", "sampler", "seq"];
const STEPS = 16, ESC_WINDOW = 120;
const GLASS = [10, 11, 14], PANEL = [22, 24, 30], INK = [150, 160, 170], DIM = [60, 66, 78], LIT = [240, 240, 240];

let mode = "inst", track = 0, octave = 4, patch = 0, scale = 0, root = 0, chordOn = false;
let bpm = 120, playing = false, armed = false, click = false, loopSteps = STEPS;
let startAt = 0, lastStep = -1, taps = [];
let held = {};            // key → { voices, hit, downAt }
let mods = { shift: false, caps: false };
let tracks = TRACK_NAMES.map((name) => ({ name, hits: [], level: 0, mute: false }));
let cursor = { t: 0, s: 0 };
let recording = false, sampled = false, recStart = 0;
let xy = { x: 0.5, y: 0.5 }, sends = { echo: 0, room: 0 };
let fx = { room: false, echo: false, glitch: false };
let notice = null, escCount = 0, escAt = -1000, frame = 0, flashKey = {};

const freq = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const scaleNotes = () => SCALES[scale][1];
const stepMs = () => 60000 / bpm / 4;
const flash = (text) => { notice = { text, until: Date.now() + 1500 }; };

// Scale layer: +1 degree per key to the right, +2 per row up, so a diagonal
// of three keys is always a triad (Oskie's rule).
function degreeOf(k) {
  for (let r = 0; r < ROWS.length; r++) { const c = ROWS[r].indexOf(k); if (c >= 0) return c + r * 2; }
  return -1;
}
function midiOf(degree, oct = octave) {
  const n = scaleNotes(), o = Math.floor(degree / n.length), d = degree % n.length;
  return 12 * (oct + 1) + root + n[d] + o * 12;
}
function degreeLabel(degree) {
  const n = scaleNotes().length, o = Math.floor(degree / n);
  return `${(degree % n) + 1}${"'".repeat(o)}`;
}
function padOf(k) { // drums layer: cols 0-3 kit, cols 5-8 zoo, col 4 free
  for (let r = 0; r < ROWS.length; r++) {
    const c = ROWS[r].indexOf(k); if (c < 0) continue;
    if (c <= 3) return { kind: "drum", i: r * 4 + c };
    if (c >= 5 && c <= 8) return { kind: "zoo", i: r * 4 + (c - 5) };
  }
  return null;
}
function pan(k) {
  for (let r = 0; r < ROWS.length; r++) { const c = ROWS[r].indexOf(k); if (c >= 0) return ((c + r * 0.5) / 11 * 2 - 1) * 0.8; }
  return 0;
}

// A hit is everything needed to replay a key press without the key.
function hitFor(k, accent) {
  const vol = accent ? 1.0 : 0.7, p = pan(k);
  if (mode === "drums") {
    const pad = padOf(k); if (!pad) return null;
    return pad.kind === "drum" ? { kind: "drum", drum: KIT[pad.i], vol, pan: p }
                               : { kind: "zoo", name: ZOO[pad.i], pitch: Math.pow(2, octave - 4), vol, pan: p };
  }
  const degree = degreeOf(k); if (degree < 0) return null;
  if (mode === "sampler") return sampled ? { kind: "sample", midi: midiOf(degree), vol, pan: p } : null;
  const degrees = chordOn ? [degree, degree + 2, degree + 4] : [degree];
  return { kind: "note", midis: degrees.map((d) => midiOf(d)), patch, vol, pan: p, dur: 0.25 };
}

function fire(sound, hit, hold) {
  const voices = [];
  if (hit.kind === "drum") playPercussion(sound, hit.drum, { volume: hit.vol * 1.2, pan: hit.pan });
  else if (hit.kind === "zoo") sound.zoo?.play?.(hit.name, { volume: hit.vol, pan: hit.pan, pitch: hit.pitch });
  else if (hit.kind === "sample") {
    const v = sound.sample?.play?.({ tone: freq(hit.midi), base: freq(60), volume: hit.vol, pan: hit.pan, loop: false });
    if (v && hold) voices.push(v);
  } else {
    const p = PATCHES[hit.patch] || PATCHES[0];
    for (const [i, m] of hit.midis.entries()) {
      const v = sound.synth?.({ type: p.type, tone: freq(m), duration: hold ? Infinity : hit.dur, volume: hit.vol * 0.6,
                                 attack: p.a, decay: p.d, pan: Math.max(-0.9, Math.min(0.9, hit.pan + i * 0.05)), gmProgram: p.gm });
      if (v) voices.push(v);
    }
  }
  return voices;
}

function release(k, sound) {
  const h = held[k]; if (!h) return;
  for (const v of h.voices) sound.kill?.(v, 0.08);
  if (h.hit?.kind === "note") h.hit.dur = Math.max(0.1, Math.min(4, (Date.now() - h.downAt) / 1000));
  delete held[k];
}

// Record into the selected track at the nearest step of the running loop.
function record(hit) {
  if (!playing || !armed) return;
  const pos = ((Date.now() - startAt) / stepMs()) % loopSteps;
  const step = Math.round(pos) % loopSteps;
  tracks[track].hits.push({ step, ...hit });
}

function tick(sound) {
  if (!playing) return;
  const step = Math.floor((Date.now() - startAt) / stepMs()) % loopSteps;
  if (step === lastStep) return;
  lastStep = step;
  if (click && step % 4 === 0) sound.synth?.({ type: "square", tone: step === 0 ? 1760 : 1320, duration: 0.02, volume: 0.15, attack: 0.001, decay: 0.015 });
  for (const t of tracks) {
    if (t.mute) continue;
    for (const h of t.hits) if (h.step === step) { fire(sound, h, false); t.level = 1; }
  }
}

function stopAll(sound) {
  for (const k of Object.keys(held)) release(k, sound);
  sound.allNotesOff?.();
}

function tap() {
  const now = Date.now();
  taps = taps.filter((t) => now - t < 2500); taps.push(now);
  if (taps.length >= 2) {
    const gaps = taps.slice(1).map((t, i) => t - taps[i]);
    bpm = Math.round(60000 / (gaps.reduce((a, b) => a + b, 0) / gaps.length));
    bpm = Math.max(40, Math.min(240, bpm));
  }
  flash(`tempo ${bpm}${taps.length < 2 ? " (keep tapping)" : ""}`);
}

function applySends(sound) {
  sound.echo?.setMix?.(fx.echo ? sends.echo : 0);
  sound.room?.setMix?.(fx.room ? sends.room : 0);
}

function boot({ sound }) {
  frame = 0; held = {}; notice = null;
  sound?.bpm?.(bpm);
  const mic = sound?.microphone || {};
  sampled = (mic.sampleLength || 0) > 0;
  flash("oskey — F1 inst  F2 drums  F3 sampler  F4 seq  space play  F12 rec");
}

function act({ event: e, sound, system, screen }) {
  if (e.is("touch") || e.is("draw")) {
    xy = { x: Math.max(0, Math.min(1, e.x / screen.width)), y: Math.max(0, Math.min(1, 1 - e.y / screen.height)) };
    sends = { echo: xy.x * 0.8, room: xy.y * 0.8 };
    applySends(sound);
    return;
  }
  if (e.is("keyboard:up")) {
    const k = String(e.key || "").toLowerCase();
    if (k === "shift") mods.shift = false;
    if (k === "capslock") mods.caps = false;
    release(k, sound);
    return;
  }
  if (!e.is("keyboard:down")) return;
  const k = String(e.key || "").toLowerCase();
  if (e.repeat) return;
  flashKey[k] = frame;

  if (k === "escape") {
    if (frame - escAt > ESC_WINDOW) escCount = 0;
    escCount++; escAt = frame;
    if (escCount >= 3) { system?.jump?.("prompt"); return; }
    playing = false; armed = false; stopAll(sound);
    flash(`stopped (esc ${escCount}/3 → prompt)`);
    return;
  }
  if (k === "shift") { mods.shift = true; return; }
  if (k === "capslock") { mods.caps = true; return; }

  // Caps held: the number row becomes the effect rack, space stops.
  if (mods.caps) {
    const f = FX.find(([, n]) => String(n) === k);
    if (f) {
      const name = f[0];
      fx[name] = !fx[name];
      if (name === "glitch") sound.glitch?.setMix?.(fx.glitch ? 0.5 : 0);
      if (name !== "echo") sound[name]?.toggle?.();   // room and glitch are switched; echo is a send
      applySends(sound); flash(`${name} ${fx[name] ? "on" : "off"}`);
    }
    if (k === " ") { playing = false; stopAll(sound); flash("stop"); }
    return;
  }

  const modeKey = { f1: "inst", f2: "drums", f3: "sampler", f4: "seq" }[k];
  if (modeKey) {
    mode = modeKey;
    if (mode !== "seq") track = MODES.indexOf(mode);
    stopAll(sound); flash(mode);
    return;
  }
  if (k === "f5") { scale = (scale + 1) % SCALES.length; flash(`scale ${NAMES[root]} ${SCALES[scale][0]}`); return; }
  if (k === "f6") { chordOn = !chordOn; flash(`chord ${chordOn ? "on" : "off"}`); return; }
  if (k === "f7") { tap(); return; }
  if (k === "f8") { root = (root + 1) % 12; flash(`root ${NAMES[root]} ${SCALES[scale][0]}`); return; }
  if (k === "f9") {
    if (!recording) { recording = !!sound.microphone?.rec?.(); recStart = Date.now(); mode = "sampler"; track = 2; flash(recording ? "mic recording… F9 to cut" : "no mic"); }
    else { const len = sound.microphone?.cut?.() || 0; recording = false; sampled = len > 0; flash(sampled ? `sample ${(len / (sound.microphone?.sampleRate || 48000)).toFixed(2)}s` : "nothing recorded"); }
    return;
  }
  if (k === "f10") { loopSteps = loopSteps === STEPS ? STEPS * 2 : STEPS; flash(`loop ${loopSteps / 4} bars`); return; }
  if (k === "f11") { click = !click; flash(`click ${click ? "on" : "off"}`); return; }
  if (k === "f12") { armed = !armed; if (armed && !playing) { playing = true; startAt = Date.now(); lastStep = -1; } flash(armed ? "rec" : "rec off"); return; }
  if (k === " ") {
    playing = !playing;
    if (playing) { startAt = Date.now(); lastStep = -1; } else stopAll(sound);
    flash(playing ? "play" : "pause");
    return;
  }
  if (k === "-") { octave = Math.max(1, octave - 1); flash(`octave ${octave}`); return; }
  if (k === "=") { octave = Math.min(7, octave + 1); flash(`octave ${octave}`); return; }
  if (k === "tab") { patch = (patch + 1) % PATCHES.length; flash(`patch ${patch + 1} ${PATCHES[patch].name}`); return; }
  if (k === "insert") { const h = tracks[track].hits.pop(); flash(h ? "undo" : "nothing to undo"); return; }
  if (k === "delete") { tracks[track].hits = []; flash(`${tracks[track].name} cleared`); return; }
  if (k === "m" && mode === "seq") { tracks[cursor.t].mute = !tracks[cursor.t].mute; return; }

  const digit = DIGITS.indexOf(k);
  if (digit >= 0 && digit < 9) { track = digit; cursor.t = digit; flash(`track ${digit + 1} ${tracks[digit].name}`); return; }

  if (mode === "seq") {
    if (k === "arrowleft") cursor.s = (cursor.s - 1 + loopSteps) % loopSteps;
    if (k === "arrowright") cursor.s = (cursor.s + 1) % loopSteps;
    if (k === "arrowup") cursor.t = (cursor.t - 1 + 9) % 9;
    if (k === "arrowdown") cursor.t = (cursor.t + 1) % 9;
    if (k === "enter" || k === "backspace") {
      const t = tracks[cursor.t], at = t.hits.filter((h) => h.step === cursor.s);
      if (at.length || k === "backspace") t.hits = t.hits.filter((h) => h.step !== cursor.s);
      else {
        const kinds = ["note", "drum", "sample"], kind = kinds[cursor.t] || (t.hits[0]?.kind ?? "drum");
        const h = kind === "note" ? { kind, midis: [midiOf(0)], patch, vol: 0.7, pan: 0, dur: 0.25 }
                : kind === "sample" ? { kind, midi: midiOf(0), vol: 0.7, pan: 0 } : { kind: "drum", drum: "c", vol: 0.7, pan: 0 };
        t.hits.push({ step: cursor.s, ...h }); fire(sound, h, false);
      }
    }
    track = cursor.t;
    return;
  }

  if (held[k]) return;
  const hit = hitFor(k, mods.shift);
  if (!hit) { if (mode === "sampler" && degreeOf(k) >= 0) flash("no sample yet — F9 records one"); return; }
  held[k] = { voices: fire(sound, hit, true), hit, downAt: Date.now() };
  tracks[track].level = 1;
  record(hit);
}

function sim({ sound }) { tick(sound); for (const t of tracks) t.level *= 0.9; }

// Paint: the keyboard by role in the middle, patch on the left, tracks on
// the right, the loop grid and XY readout along the bottom.
function paint({ wipe, ink, box, line, write, screen }) {
  frame++;
  const w = screen.width, h = screen.height, S = w >= 900 ? 2 : 1, ch = 6 * S, lh = 10 * S, m = 6 * S;
  wipe(...GLASS);
  const txt = (t, x, y, c = INK) => { ink(...c); write(t, { x, y, size: S, font: "font_1" }); };

  // top bar — below the system's corner label zone
  let y = 22;
  const tabs = [["F1", "Inst"], ["F2", "Drums"], ["F3", "Sampler"], ["F4", "Seq"]];
  let x = m;
  for (const [i, [key, name]] of tabs.entries()) {
    const on = MODES[i] === mode, bw = (key.length + name.length + 3) * ch;
    ink(...(on ? LIT : PANEL)); box(x, y, bw, lh + 4);
    txt(`${key} ${name}`, x + ch, y + 2, on ? GLASS : INK);
    x += bw + m;
  }
  txt(`${bpm} bpm`, x + m, y + 2, LIT); x += 10 * ch;
  txt(`${NAMES[root]} ${SCALES[scale][0]}${chordOn ? " chord" : ""}`, x, y + 2, LIT); x += 16 * ch;
  if (recording) { ink(240, 60, 60); box(x, y, 7 * ch, lh + 4); txt("MIC", x + ch, y + 2, LIT); x += 8 * ch; }
  ink(...(armed ? [220, 50, 50] : PANEL)); box(x, y, 5 * ch, lh + 4); txt("REC", x + ch, y + 2, armed ? LIT : INK); x += 6 * ch;
  ink(...(playing ? [90, 200, 90] : PANEL)); box(x, y, 6 * ch, lh + 4); txt("PLAY", x + ch, y + 2, playing ? GLASS : INK);
  y += lh + 4 + m;

  // left: patch
  const lw = Math.floor(w * 0.22), top = y;
  ink(...PANEL); box(m, y, lw, Math.floor(h * 0.42));
  const p = PATCHES[patch];
  txt(mode === "drums" ? "KIT + ZOO" : mode === "sampler" ? (sampled ? "SAMPLE" : "NO SAMPLE") : `${String(patch + 1).padStart(2, "0")} ${p.name}`, m + ch, y + m, LIT);
  txt(`oct ${octave}  tab patch`, m + ch, y + m + lh, INK);
  const ox = m + ch, oy = y + m + lh * 2 + 4, ow = lw - ch * 2, oh = Math.floor(h * 0.1);
  ink(...DIM); box(ox, oy, ow, oh, "outline");
  ink(230, 170, 60);
  let px = ox, py = oy + oh / 2;
  for (let i = 1; i <= ow; i++) {
    const t = (i / ow) * 4 % 1, v = p.type === "sawtooth" ? t * 2 - 1 : p.type === "square" ? (t < 0.5 ? 1 : -1)
            : p.type === "triangle" ? 1 - Math.abs(t * 4 - 2) : Math.sin(t * Math.PI * 2);
    const nx = ox + i, ny = oy + oh / 2 - v * oh * 0.42;
    line(px, py, nx, ny); px = nx; py = ny;
  }
  const ey = oy + oh + lh, eh = Math.floor(h * 0.08);
  ink(...DIM); box(ox, ey, ow, eh, "outline");
  ink(90, 150, 230);
  const ax = ox + Math.min(ow * 0.3, p.a * ow), dx = ax + Math.min(ow * 0.3, p.d * ow * 0.6);
  line(ox, ey + eh, ax, ey + 2); line(ax, ey + 2, dx, ey + eh * 0.4); line(dx, ey + eh * 0.4, ox + ow * 0.8, ey + eh * 0.4); line(ox + ow * 0.8, ey + eh * 0.4, ox + ow, ey + eh);
  txt("A   D   S   R", ox, ey + eh + 2, DIM);
  txt(`xy  echo ${Math.round(sends.echo * 100)}  room ${Math.round(sends.room * 100)}`, ox, ey + eh + lh + 2, fx.echo || fx.room ? LIT : DIM);
  txt(`caps+ 1 room${fx.room ? "*" : ""} 2 echo${fx.echo ? "*" : ""} 3 glitch${fx.glitch ? "*" : ""}`, ox, ey + eh + lh * 2 + 2, DIM);

  // right: tracks
  const rw = Math.floor(w * 0.24), rx = w - rw - m;
  ink(...PANEL); box(rx, top, rw, Math.floor(h * 0.42));
  for (let i = 0; i < 9; i++) {
    const ty = top + m + i * (lh + 2), on = i === track, t = tracks[i];
    ink(...TRACK_RGB[i]); box(rx + ch, ty, ch + 2, lh);
    txt(`${i + 1} ${t.name}${t.mute ? " (m)" : ""}`, rx + ch * 3, ty, on ? LIT : INK);
    ink(...(on ? LIT : DIM)); box(rx + rw - ch * 8, ty + 3, Math.floor(ch * 7 * Math.min(1, t.level)), 3);
    if (t.hits.length) txt(String(t.hits.length), rx + rw - ch * 10, ty, DIM);
  }

  // middle: the keyboard by role
  const kx0 = m + lw + m, kw = Math.floor((rx - m - kx0) / 13.5), kh = Math.floor(kw * 0.8), gap = Math.max(1, Math.floor(kw * 0.1));
  let ky = top;
  const key = (x, y, w, h, rgb, label, down) => {
    ink(...(down ? LIT : rgb)); box(x, y, w - gap, h - gap);
    if (label) txt(label, x + 2, y + 2, down ? GLASS : [0, 0, 0]);
  };
  const isDown = (k) => held[k] || frame - (flashKey[k] ?? -99) < 6;
  const fnRow = [["esc", "Esc", DIM], ["f1", "Inst", [200, 200, 200]], ["f2", "Drum", [200, 200, 200]], ["f3", "Smpl", [200, 200, 200]], ["f4", "Seq", [200, 200, 200]],
                 ["f5", "Scal", [230, 200, 80]], ["f6", "Chrd", [230, 200, 80]], ["f7", "Tap", [230, 200, 80]], ["f8", "Root", [230, 200, 80]],
                 ["f9", "Mic", [230, 120, 80]], ["f10", "Loop", [230, 120, 80]], ["f11", "Clck", [230, 120, 80]], ["f12", "Rec", [230, 60, 60]]];
  for (const [i, [k, label, rgb]] of fnRow.entries()) key(kx0 + i * kw, ky, kw, kh, rgb, S > 1 ? label : label.slice(0, 2), isDown(k));
  ky += kh;
  for (let i = 0; i < 9; i++) key(kx0 + (i + 1) * kw, ky, kw, kh, TRACK_RGB[i], mods.caps && i < 3 ? FX[i][0].slice(0, 4) : `${i + 1}`, isDown(String(i + 1)));
  key(kx0, ky, kw, kh, DIM, "`"); key(kx0 + 10 * kw, ky, kw, kh, DIM, "0");
  key(kx0 + 11 * kw, ky, kw, kh, [120, 120, 140], "oct-", isDown("-")); key(kx0 + 12 * kw, ky, kw, kh, [120, 120, 140], "oct+", isDown("="));
  ky += kh;
  const rowX = [1.5, 1.75, 2.25]; // stagger: q row, a row, z row
  for (let r = ROWS.length - 1; r >= 0; r--) {
    const row = ROWS[r], ry = ky + (ROWS.length - 1 - r) * kh, rx0 = kx0 + rowX[ROWS.length - 1 - r] * kw;
    for (let c = 0; c < row.length; c++) {
      const k = row[c];
      let rgb = DIM, label = k;
      if (mode === "drums") {
        const pad = padOf(k);
        if (pad?.kind === "drum") { rgb = PERCUSSION_COLORS[KIT[pad.i]]; label = PERCUSSION_NAMES[KIT[pad.i]].slice(0, 4); }
        else if (pad?.kind === "zoo") { rgb = [120, 200, 120]; label = ZOO[pad.i].slice(0, 4); }
      } else {
        const d = degreeOf(k), n = scaleNotes().length;
        if (d >= 0) { rgb = DEGREE_RGB[d % n % 7]; if (Math.floor(d / n) % 2) rgb = rgb.map((v) => Math.min(255, v + 60)); label = degreeLabel(d); }
        if (mode === "sampler" && !sampled) rgb = rgb.map((v) => Math.floor(v * 0.4));
      }
      key(rx0 + c * kw, ry, kw, kh, rgb, label, isDown(k));
    }
  }
  key(kx0, ky, 1.5 * kw, kh, [230, 90, 160], "Tab patch", isDown("tab"));
  key(kx0, ky + kh, 1.75 * kw, kh, mods.caps ? LIT : [240, 240, 240], "Caps fx", mods.caps);
  key(kx0, ky + kh * 2, 2.25 * kw, kh, mods.shift ? LIT : [170, 170, 180], "Shift accent", mods.shift);
  ky += kh * 3;
  key(kx0 + 3 * kw, ky, 6 * kw, kh, playing ? [120, 230, 90] : [90, 180, 70], playing ? "Space pause" : "Space play", isDown(" "));
  key(kx0 + 9.5 * kw, ky, 1.5 * kw, kh, [120, 120, 140], "Ins undo", isDown("insert"));
  key(kx0 + 11 * kw, ky, 1.5 * kw, kh, [230, 120, 80], "Del clear", isDown("delete"));

  // bottom: loop grid + help
  const gy = Math.max(ky + kh + m, top + Math.floor(h * 0.42) + m), gw = w - m * 2, cw = Math.floor(gw / loopSteps), rh = Math.max(3, Math.floor((h - gy - lh * 2 - m) / 9));
  const stepNow = playing ? Math.floor((Date.now() - startAt) / stepMs()) % loopSteps : -1;
  for (let i = 0; i < 9; i++) {
    for (let s = 0; s < loopSteps; s++) {
      const on = tracks[i].hits.some((hh) => hh.step === s), cur = mode === "seq" && cursor.t === i && cursor.s === s;
      ink(...(cur ? LIT : on ? TRACK_RGB[i] : s === stepNow ? [40, 44, 56] : s % 4 === 0 ? [28, 30, 38] : PANEL));
      box(m + s * cw, gy + i * rh, cw - 1, rh - 1);
    }
  }
  if (stepNow >= 0) { ink(255, 255, 255, 90); box(m + stepNow * cw, gy, cw - 1, rh * 9 - 1); }
  // XY dot
  ink(230, 170, 60); box(m + Math.floor(xy.x * (gw - 4)), gy + Math.floor((1 - xy.y) * (rh * 9 - 4)), 4, 4);
  const help = mode === "seq" ? "arrows move  enter toggle  backspace clear  m mute  1-9 track"
             : mode === "drums" ? "left hand kit  right hand zoo  -/= pitch  shift accent  F12 rec"
             : mode === "sampler" ? "F9 record mic / cut  keys play it by degree  F5 scale  F8 root"
             : "keys = scale degrees  diagonal = triad  tab patch  -/= octave  trackpad = sends";
  txt(help, m, h - lh * 2 + 2, DIM);
  if (notice && Date.now() < notice.until) txt(notice.text, m, h - lh + 1, LIT);
  else txt(`track ${track + 1} ${tracks[track].name}  ·  esc×3 prompt`, m, h - lh + 1, DIM);
}

function leave({ sound }) { stopAll(sound); if (recording) sound.microphone?.cut?.(); }

export { boot, paint, act, sim, leave };
