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
  { name: "Hoover", type: "sawtooth", hoover: true, a: 0.01, d: 0.2 },
  { name: "Shepard", type: "sine", shepard: true, a: 0.04, d: 0.3 },
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
// Mix: one gain per sound family, so drums sit level with the synths.
// The drum lib accepts up to 2.2; notes go out around 0.4-0.6.
const MIX = { drums: 2.0, zoo: 1.0, sample: 1.0 };
const MODES = ["inst", "drums", "sampler", "seq"];
const STEPS = 16, ESC_WINDOW = 120, MAX_HELD_VOICES = 20;
// Light mode: paper, cream cards, dark ink; keys keep their role colors.
const GLASS = [247, 244, 236], PANEL = [232, 227, 214], INK = [90, 84, 78], DIM = [196, 190, 178], LIT = [34, 30, 28], CUTE = [230, 90, 140];

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
// Shepard tone: one pitch class stacked across octaves under a bell curve in
// log-frequency, so the scale climbs forever and octave stops mattering.
const SHEPARD_CENTER = 60, SHEPARD_WIDTH = 18; // midi centre, spread in semitones
function shepardPartials(midi) {
  const parts = [];
  for (let m = 24 + (((midi % 12) + 12) % 12); m <= 108; m += 12) {
    const g = Math.exp(-Math.pow((m - SHEPARD_CENTER) / SHEPARD_WIDTH, 2) / 2);
    if (g > 0.05) parts.push({ midi: m, gain: g });
  }
  const sum = parts.reduce((a, p) => a + p.gain, 0);
  return parts.map((p) => ({ midi: p.midi, gain: p.gain / sum }));
}
// Hoover: a detuned saw chorus with a sub octave, spread wide, that swoops up
// into pitch on every note. Offsets are semitones from the played note.
const HOOVER_LAYERS = [
  { off: 0, gain: 0.3, pan: 0 }, { off: -0.2, gain: 0.25, pan: -0.5 }, { off: 0.2, gain: 0.25, pan: 0.5 },
  { off: -12, gain: 0.3, pan: -0.2 }, { off: -11.9, gain: 0.2, pan: 0.2 },
];
const SWOOP_MS = 140, SWOOP_FROM = -5; // starts this many semitones flat
let swoops = []; // { at, voices: [{ v, f }] }
function swoopTick() {
  const now = Date.now();
  swoops = swoops.filter((s) => {
    const k = Math.min(1, (now - s.at) / SWOOP_MS), ratio = Math.pow(2, (SWOOP_FROM * (1 - k) * (1 - k)) / 12);
    for (const { v, f } of s.voices) v.update?.({ tone: f * ratio });
    return k < 1;
  });
}
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
  glow(tintOf(hit), 0.6, hit.vol);
  if (hit.kind === "drum") playPercussion(sound, hit.drum, { volume: Math.min(2.2, hit.vol * MIX.drums), pan: hit.pan });
  else if (hit.kind === "zoo") sound.zoo?.play?.(hit.name, { volume: hit.vol * MIX.zoo, pan: hit.pan, pitch: hit.pitch });
  else if (hit.kind === "sample") {
    const v = sound.sample?.play?.({ tone: freq(hit.midi), base: freq(60), volume: hit.vol * MIX.sample, pan: hit.pan, loop: false });
    if (v && hold) voices.push(v);
  } else {
    const p = PATCHES[hit.patch] || PATCHES[0], swoop = [];
    for (const [i, m] of hit.midis.entries()) {
      const pan = Math.max(-0.9, Math.min(0.9, hit.pan + i * 0.05));
      const parts = p.shepard ? shepardPartials(m).map((s) => ({ midi: s.midi, vol: hit.vol * 0.9 * s.gain, pan }))
                  : p.hoover ? HOOVER_LAYERS.map((l) => ({ midi: m + l.off, vol: hit.vol * 0.5 * l.gain, pan: Math.max(-0.9, Math.min(0.9, pan + l.pan)) }))
                  : [{ midi: m, vol: hit.vol * 0.6, pan }];
      for (const s of parts) {
        const f = freq(s.midi), start = p.hoover ? f * Math.pow(2, SWOOP_FROM / 12) : f;
        const v = sound.synth?.({ type: p.type, tone: start, duration: hold ? Infinity : hit.dur, volume: s.vol,
                                   attack: p.a, decay: p.d, pan: s.pan, gmProgram: p.gm });
        if (v) { voices.push(v); if (p.hoover) swoop.push({ v, f }); }
      }
    }
    if (swoop.length) swoops.push({ at: Date.now(), voices: swoop });
  }
  return voices;
}

// Background wash: whatever sounds tints the paper. Notes take a hue from
// their pitch class around the colour wheel, drums their kit colour.
let wash = [...GLASS], washAmt = 0;
const WASH_MAX = 0.45, WASH_FADE = 0.95;
function hueRgb(h) {
  const f = (n) => { const k = (n + h * 6) % 6; return 0.95 - 0.95 * 0.7 * Math.max(0, Math.min(k, 4 - k, 1)); };
  return [f(5), f(3), f(1)].map((v) => Math.round(v * 255));
}
function tintOf(hit) {
  if (hit.kind === "drum") return PERCUSSION_COLORS[hit.drum] || DIM;
  if (hit.kind === "zoo") return [120, 200, 120];
  if (hit.kind === "sample") return [90, 150, 230];
  return pitchRgb(hit.midis[0]);
}
function pitchRgb(midi) { return hueRgb((((midi % 12) + 12) % 12) / 12); }
function glow(rgb, pull, amt) {
  wash = wash.map((v, i) => v + (rgb[i] - v) * pull);
  washAmt = Math.min(1, Math.max(washAmt, amt));
}

function release(k, sound) {
  const h = held[k]; if (!h) return;
  for (const v of h.voices) sound.kill?.(v, 0.08);
  if (h.hit?.kind === "note") {
    h.hit.dur = Math.max(0.1, Math.min(4, (Date.now() - h.downAt) / 1000));
    if (h.rec) h.rec.dur = h.hit.dur;
  }
  delete held[k];
}

// Record into the selected track at the nearest step of the running loop.
function record(hit, into = track) {
  if (!playing || !armed) return;
  const pos = ((Date.now() - startAt) / stepMs()) % loopSteps;
  const step = Math.round(pos) % loopSteps;
  const entry = { step, ...hit };
  tracks[into].hits.push(entry);
  return entry; // kept on the held key so release can write the real length
}

function tick(sound) {
  if (!playing) return;
  const step = Math.floor((Date.now() - startAt) / stepMs()) % loopSteps;
  if (step === lastStep) return;
  lastStep = step;
  if (click && step % 4 === 0) sound.synth?.({ type: "square", tone: step === 0 ? 1760 : 1320, duration: 0.02, volume: 0.15, attack: 0.001, decay: 0.015 });
  for (const t of tracks) {
    if (t.mute) continue;
    for (const h of t.hits) if (h.step === step) { fire(sound, h, false); t.level = 1; lastHit = frame; }
  }
}

function stopAll(sound) {
  for (const k of Object.keys(held)) release(k, sound);
}

// Hard reset: voice ids count up from 1, and sound.kill takes a bare id, so a
// silent probe voice tells us the newest id and we kill everything below it.
// Clears drones orphaned by reloads. (Engine has 32 voice slots in total.)
function resetAudio(sound) {
  swoops = [];
  const probe = sound?.synth?.({ type: "sine", tone: 440, duration: 0.01, volume: 0 });
  const top = Number(probe?.id) || 0;
  for (let id = Math.max(1, top - 100000); id <= top; id++) sound.kill(id, 0.05);
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

// Grid labels: pitch with octave when the cell fits it ("C#4"), else just the
// name; drums and zoo show their first letters.
const noteName = (midi) => `${NAMES[((midi % 12) + 12) % 12]}${Math.floor(midi / 12) - 1}`;
function hitLabel(hit, fit) {
  if (!hit || fit < 1) return "";
  const full = hit.kind === "note" ? noteName(hit.midis[0]) : hit.kind === "sample" ? noteName(hit.midi)
             : hit.kind === "drum" ? PERCUSSION_NAMES[hit.drum] || hit.drum : hit.name || "";
  return full.length <= fit ? full : hit.kind === "note" || hit.kind === "sample" ? full.replace(/-?\d+$/, "").slice(0, fit) : full.slice(0, fit);
}

// Autoplay: boot straight into "Spring Sketch", an original slow piano piece
// in F major (two bars, F then Bb), so the loop plays without touching the
// ThinkPad. Set AUTOPLAY = false to boot silent and empty.
const AUTOPLAY = false;
const PIANO = PATCHES.findIndex((p) => p.name === "Piano");
function loadDemo(sound) {
  const note = (step, midi, vol, dur, pan) => ({ step, kind: "note", midis: [midi], patch: PIANO, vol, pan, dur });
  // left hand, track 4: rolling eighth-note arpeggios
  const left = [[0, 41], [2, 48], [4, 53], [6, 57], [8, 60], [10, 57], [12, 53], [14, 48],
                [16, 46], [18, 53], [20, 58], [22, 62], [24, 62], [26, 58], [28, 53], [30, 50]];
  // right hand, track 1: a sparse melody that lands back on A
  const right = [[0, 69, 1.6], [6, 72, 0.8], [8, 77, 1.4], [12, 76, 0.8], [14, 72, 0.8],
                 [16, 74, 1.6], [22, 77, 0.8], [24, 69, 1.4], [28, 67, 0.8], [30, 69, 0.8]];
  tracks[3].name = "Piano LH"; tracks[0].name = "Piano RH";
  tracks[3].hits = left.map(([s, m]) => note(s, m, 0.4, 1.2, -0.3));
  tracks[0].hits = right.map(([s, m, d]) => note(s, m, 0.6, d, 0.2));
  bpm = 76; loopSteps = STEPS * 2; sound?.bpm?.(bpm);
  fx.room = true; sends.room = 0.35; sound?.room?.toggle?.(); applySends(sound);
  playing = true; startAt = Date.now(); lastStep = -1;
}

// MIDI controllers: the kernel exposes each as /dev/snd/midiC<card>D<dev>. The OS
// cannot hand their notes to a piece yet, but we can see and name them.
let sys = null, midiDevs = [], midiScanAt = -999;
function scanMidi() {
  const list = sys?.listDir?.("/dev/snd") || [];
  const found = list.map((f) => /^midiC(\d+)D(\d+)$/.exec(f.name)).filter(Boolean).map((mm) => {
    const id = String(sys.readFile?.(`/sys/class/sound/card${mm[1]}/id`) || "").trim();
    return { dev: mm[0], name: id || `card ${mm[1]}` };
  });
  const names = found.map((d) => d.name).join(", ");
  if (names !== midiDevs.map((d) => d.name).join(", ")) {
    if (found.length) flash(`MIDI: ${names} seen (playing it needs an OS update)`);
    else if (midiDevs.length) flash("MIDI unplugged");
    console.log(`[oskey] midi devices: ${names || "none"}`);
  }
  midiDevs = found;
}

function boot({ sound, system }) {
  sys = system;
  frame = 0; held = {}; notice = null; swoops = [];
  resetAudio(sound); // clear anything a previous reload left droning
  sound?.bpm?.(bpm);
  const mic = sound?.microphone || {};
  sampled = (mic.sampleLength || 0) > 0;
  flash("oskey — F1 inst  F2 drums  F3 sampler  F4 seq  space play  F12 rec");
  scanMidi();
  if (AUTOPLAY && tracks.every((t) => !t.hits.length)) { loadDemo(sound); flash("Spring Sketch — piano, 76 bpm — space pauses"); }
}

// Key names vary by keyboard layer; fold the spellings of space into " ".
let lastKey = "";
function keyName(e) {
  const k = String(e.key || "").toLowerCase();
  return k === "space" || k === "spacebar" ? " " : k;
}

// Voice cap: steal the oldest held keys so stacked patches can't choke the mixer.
function capVoices(sound) {
  const live = () => Object.values(held).reduce((n, h) => n + h.voices.length, 0);
  while (live() > MAX_HELD_VOICES) {
    const oldest = Object.keys(held).sort((a, b) => held[a].downAt - held[b].downAt)[0];
    release(oldest, sound);
  }
}

// MIDI controller. Needs the OS midi-in update (see JEFFREY-midi-input.md);
// until then e.is("midi:…") is always false and none of this runs.
// Notes play the current mode: instrument patch, sampler, or drums (General
// MIDI drum map, and channel 10 is always drums). Velocity sets loudness, and
// notes record into the loop like keys. Mod wheel (CC 1) sets the echo send.
const GM_DRUMS = { 36: "c", 35: "c", 38: "d", 40: "d", 39: "e", 37: "f", 42: "g", 44: "g", 46: "a",
                   51: "b", 59: "b", 49: "c#", 57: "c#", 55: "d#", 56: "f#", 76: "g#", 77: "g#", 54: "a#" };
function midiHit(note, velocity, channel) {
  const vol = 0.3 + 0.7 * Math.min(1, velocity / 127), pan = Math.max(-0.6, Math.min(0.6, (note - 60) / 36));
  if (channel === 10 || mode === "drums") return { kind: "drum", drum: GM_DRUMS[note] || KIT[note % 12], vol, pan };
  if (mode === "sampler") return sampled ? { kind: "sample", midi: note, vol, pan } : null;
  return { kind: "note", midis: [note], patch, vol, pan, dur: 0.25 };
}
function midiEvent(e, sound) {
  if (e.is("midi:cc")) {
    if (e.control === 1) { sends.echo = (e.value / 127) * 0.8; fx.echo = sends.echo > 0; applySends(sound); }
    return;
  }
  const k = `midi:${e.note}`;
  if (e.is("midi:noteoff")) { release(k, sound); return; }
  lastKey = `midi ${noteName(e.note)}`;
  if (held[k]) release(k, sound);
  const hit = midiHit(e.note, e.velocity, e.channel);
  if (!hit) { flash("no sample yet — F9 records one"); return; }
  capVoices(sound);
  held[k] = { voices: fire(sound, hit, hit.kind !== "drum"), hit, downAt: Date.now() };
  const into = hit.kind === "drum" && mode !== "drums" ? 1 : track; // stray drums land on Drums
  tracks[into].level = 1; lastHit = frame;
  held[k].rec = record(hit, into);
}

function act({ event: e, sound, system, screen }) {
  if (e.is("midi")) { midiEvent(e, sound); return; }
  if (e.is("touch") || e.is("draw")) {
    xy = { x: Math.max(0, Math.min(1, e.x / screen.width)), y: Math.max(0, Math.min(1, 1 - e.y / screen.height)) };
    sends = { echo: xy.x * 0.8, room: xy.y * 0.8 };
    applySends(sound);
    return;
  }
  if (e.is("keyboard:up")) {
    const k = keyName(e);
    if (k === "shift") mods.shift = false;
    if (k === "capslock") mods.caps = false;
    release(k, sound);
    return;
  }
  if (!e.is("keyboard:down")) return;
  const k = keyName(e);
  lastKey = k;
  if (e.repeat) return;
  flashKey[k] = frame;

  if (k === "escape") {
    if (frame - escAt > ESC_WINDOW) escCount = 0;
    escCount++; escAt = frame;
    if (escCount >= 3) { system?.jump?.("prompt"); return; }
    playing = false; armed = false; stopAll(sound); resetAudio(sound);
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
    // Drum pads place that drum (or zoo animal) at the cursor; again removes it.
    // Several drums can share a step. Note tracks (1, 3, or any holding notes) are skipped.
    const pad = padOf(k);
    if (pad) {
      const t = tracks[cursor.t];
      if (cursor.t === 0 || cursor.t === 2 || t.hits.some((x) => x.kind === "note" || x.kind === "sample")) {
        flash("pads place drums on drum tracks (2, 4-9)");
      } else {
        const vol = mods.shift ? 1.0 : 0.7;
        const h = pad.kind === "drum" ? { kind: "drum", drum: KIT[pad.i], vol, pan: 0 } : { kind: "zoo", name: ZOO[pad.i], pitch: 1, vol, pan: 0 };
        const name = pad.kind === "drum" ? PERCUSSION_NAMES[h.drum] : h.name;
        const same = (x) => x.step === cursor.s && x.kind === h.kind && (x.drum ?? x.name) === (h.drum ?? h.name);
        if (t.hits.some(same)) { t.hits = t.hits.filter((x) => !same(x)); flash(`${name} off step ${cursor.s + 1}`); }
        else { t.hits.push({ step: cursor.s, ...h }); fire(sound, h, false); flash(`${name} on step ${cursor.s + 1}`); }
      }
      track = cursor.t;
      return;
    }
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
  capVoices(sound);
  held[k] = { voices: fire(sound, hit, true), hit, downAt: Date.now() };
  tracks[track].level = 1; lastHit = frame;
  held[k].rec = record(hit);
}

function sim({ sound }) {
  if (frame - midiScanAt > 120) { midiScanAt = frame; scanMidi(); }
  tick(sound); swoopTick();
  washAmt *= WASH_FADE;
  for (const h of Object.values(held)) if (h.hit) glow(tintOf(h.hit), 0.05, 0.6); // held notes keep it lit
  for (const t of tracks) t.level *= 0.9; }

// Paint: the keyboard by role in the middle, patch on the left, tracks on
// the right, the loop grid and XY readout along the bottom.
// The guy: a round pink pal who bops on the beat, blinks, and goes "o" when
// something is hit. Drawn from boxes so he survives any pixel scale.
let lastHit = -999;
function guy(ink, box, x, y, S) {
  const beat = playing ? Math.abs(Math.sin((Date.now() - startAt) / stepMs() * Math.PI / 4)) : 0.5 + Math.sin(frame / 20) * 0.5;
  const bob = Math.round(beat * 2 * S), hit = frame - lastHit < 8, blink = frame % 150 < 5;
  const u = S, bx = x, by = y + 4 * S - bob;
  ink(...CUTE); box(bx + 2 * u, by, 14 * u, 12 * u); box(bx, by + 2 * u, 18 * u, 8 * u);                 // body
  ink(255, 170, 200); box(bx + 2 * u, by + 7 * u, 3 * u, 2 * u); box(bx + 13 * u, by + 7 * u, 3 * u, 2 * u); // cheeks
  ink(34, 30, 28);
  if (blink) { box(bx + 5 * u, by + 5 * u, 3 * u, u); box(bx + 10 * u, by + 5 * u, 3 * u, u); }
  else { box(bx + 5 * u, by + 4 * u, 2 * u, 2 * u); box(bx + 11 * u, by + 4 * u, 2 * u, 2 * u); }          // eyes
  if (hit) { box(bx + 7 * u, by + 8 * u, 4 * u, 3 * u); ink(...CUTE); box(bx + 8 * u, by + 9 * u, 2 * u, u); } // "o"
  else box(bx + 7 * u, by + 9 * u, 4 * u, u);                                                            // smile
  ink(34, 30, 28); box(bx + 4 * u, by + 12 * u, 3 * u, 2 * u); box(bx + 11 * u, by + 12 * u, 3 * u, 2 * u); // feet
}

function paint({ wipe, ink, box, line, write, screen, comicWrite }) {
  frame++;
  const w = screen.width, h = screen.height, S = w >= 900 ? 2 : 1, ch = 6 * S, lh = 10 * S, m = 6 * S;
  const a = washAmt * WASH_MAX;
  wipe(...GLASS.map((v, i) => Math.round(v + (wash[i] - v) * a)));
  const txt = (t, x, y, c = INK, font = "font_1") => { ink(...c); write(t, { x, y, size: S, font }); };

  // the wordmark, comic and pink, in the corner
  comicWrite?.("oskey", m, 3, 16 * S, ...CUTE);

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
  if (recording) { ink(240, 60, 60); box(x, y, 7 * ch, lh + 4); txt("MIC", x + ch, y + 2, GLASS); x += 8 * ch; }
  ink(...(armed ? [220, 50, 50] : PANEL)); box(x, y, 5 * ch, lh + 4); txt("REC", x + ch, y + 2, armed ? GLASS : INK); x += 6 * ch;
  ink(...(playing ? [90, 200, 90] : PANEL)); box(x, y, 6 * ch, lh + 4); txt("PLAY", x + ch, y + 2, playing ? GLASS : INK);
  if (midiDevs.length) { x += 7 * ch; ink(...CUTE); box(x, y, 6 * ch, lh + 4); txt("MIDI", x + ch, y + 2, GLASS); }
  y += lh + 4 + m;

  const foot = 16 * S + lh + 6; // footer: one matrix help line + one 16px unifont notice line
  const footer = () => {
    const help = mode === "seq" ? "arrows move  drum pads place  enter toggle  bksp clear  m mute  1-9 track"
               : mode === "drums" ? "left hand kit  right hand zoo  -/= pitch  shift accent  F12 rec"
               : mode === "sampler" ? "F9 record mic / cut  keys play it by degree  F5 scale  F8 root"
               : "keys = scale degrees  diagonal = triad  tab patch  -/= octave  trackpad = sends";
    txt(help, m, h - foot + 2, INK, "matrix");
    if (notice && Date.now() < notice.until) txt(notice.text, m, h - 16 * S - 2, LIT, "unifont");
    else txt(`track ${track + 1} ${tracks[track].name}  -  esc x3 prompt  -  key [${lastKey === " " ? "space" : lastKey}]${mods.caps ? " CAPS" : ""}`, m, h - 16 * S + 2, INK, "matrix");
  };
  if (mode === "seq") { seqView({ ink, box, line, txt, w, h, y, m, ch, lh, foot }); footer(); return; }

  // left: patch
  const lw = Math.floor(w * 0.22), top = y;
  ink(...PANEL); box(m, y, lw, Math.floor(h * 0.42));
  const p = PATCHES[patch];
  txt(mode === "drums" ? "kit + zoo" : mode === "sampler" ? (sampled ? "sample" : "no sample") : `${String(patch + 1).padStart(2, "0")} ${p.name}`, m + ch, y + m, LIT, "unifont");
  txt(`oct ${octave}  tab patch`, m + ch, y + m + lh + 6, INK, "matrix");
  const ox = m + ch, oy = y + m + lh * 2 + 4, ow = lw - ch * 2, oh = Math.floor(h * 0.1);
  ink(...GLASS); box(ox, oy, ow, oh); ink(...DIM); box(ox, oy, ow, oh, "outline");
  ink(230, 140, 40);
  let px = ox, py = oy + oh / 2;
  for (let i = 1; i <= ow; i++) {
    const t = (i / ow) * 4 % 1, v = p.type === "sawtooth" ? t * 2 - 1 : p.type === "square" ? (t < 0.5 ? 1 : -1)
            : p.type === "triangle" ? 1 - Math.abs(t * 4 - 2) : Math.sin(t * Math.PI * 2);
    const nx = ox + i, ny = oy + oh / 2 - v * oh * 0.42;
    line(px, py, nx, ny); px = nx; py = ny;
  }
  const ey = oy + oh + lh, eh = Math.floor(h * 0.08);
  ink(...GLASS); box(ox, ey, ow, eh); ink(...DIM); box(ox, ey, ow, eh, "outline");
  ink(70, 130, 220);
  const ax = ox + Math.min(ow * 0.3, p.a * ow), dx = ax + Math.min(ow * 0.3, p.d * ow * 0.6);
  line(ox, ey + eh, ax, ey + 2); line(ax, ey + 2, dx, ey + eh * 0.4); line(dx, ey + eh * 0.4, ox + ow * 0.8, ey + eh * 0.4); line(ox + ow * 0.8, ey + eh * 0.4, ox + ow, ey + eh);
  txt("A   D   S   R", ox, ey + eh + 2, INK, "matrix");
  txt(`xy  echo ${Math.round(sends.echo * 100)}  room ${Math.round(sends.room * 100)}`, ox, ey + eh + lh + 2, fx.echo || fx.room ? LIT : INK, "matrix");
  txt(`caps+ 1 room${fx.room ? "*" : ""} 2 echo${fx.echo ? "*" : ""} 3 glitch${fx.glitch ? "*" : ""}`, ox, ey + eh + lh * 2 + 2, INK, "matrix");

  // right: tracks
  const rw = Math.floor(w * 0.24), rx = w - rw - m;
  ink(...PANEL); box(rx, top, rw, Math.floor(h * 0.42));
  for (let i = 0; i < 9; i++) {
    const ty = top + m + i * (lh + 2), on = i === track, t = tracks[i];
    ink(...TRACK_RGB[i]); box(rx + ch, ty, ch + 2, lh);
    txt(`${i + 1} ${t.name}${t.mute ? " (m)" : ""}`, rx + ch * 3, ty, on ? LIT : INK);
    ink(...(on ? TRACK_RGB[i] : DIM)); box(rx + rw - ch * 8, ty + 3, Math.floor(ch * 7 * Math.min(1, t.level)), 3);
    if (t.hits.length) txt(String(t.hits.length), rx + rw - ch * 10, ty + 1, INK, "matrix");
  }
  guy(ink, box, rx + rw - 40 * S, top + Math.floor(h * 0.42) - 30 * S, 2 * S);

  // middle: the keyboard by role
  const kx0 = m + lw + m, kw = Math.floor((rx - m - kx0) / 13.5), kh = Math.floor(kw * 0.8), gap = Math.max(1, Math.floor(kw * 0.1));
  let ky = top;
  const key = (x, y, w, h, rgb, label, down) => {
    ink(...(down ? LIT : rgb)); box(x, y, w - gap, h - gap);
    if (label) txt(label, x + 2, y + 2, down ? GLASS : [40, 34, 30]);
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
  key(kx0, ky + kh, 1.75 * kw, kh, mods.caps ? LIT : [255, 255, 255], "Caps fx", mods.caps);
  key(kx0, ky + kh * 2, 2.25 * kw, kh, mods.shift ? LIT : [170, 170, 180], "Shift accent", mods.shift);
  ky += kh * 3;
  key(kx0 + 3 * kw, ky, 6 * kw, kh, playing ? [120, 230, 90] : [90, 180, 70], playing ? "Space pause" : "Space play", isDown(" "));
  key(kx0 + 9.5 * kw, ky, 1.5 * kw, kh, [120, 120, 140], "Ins undo", isDown("insert"));
  key(kx0 + 11 * kw, ky, 1.5 * kw, kh, [230, 120, 80], "Del clear", isDown("delete"));

  // bottom: loop grid + help
  const gy = Math.max(ky + kh + m, top + Math.floor(h * 0.42) + m), gw = w - m * 2, cw = Math.floor(gw / loopSteps), rh = Math.max(3, Math.floor((h - gy - foot - 2) / 9));
  const stepNow = playing ? Math.floor((Date.now() - startAt) / stepMs()) % loopSteps : -1;
  for (let i = 0; i < 9; i++) {
    for (let s = 0; s < loopSteps; s++) {
      const hit = tracks[i].hits.find((hh) => hh.step === s), on = !!hit, cur = mode === "seq" && cursor.t === i && cursor.s === s;
      ink(...(cur ? LIT : on ? tintOf(hit) : s === stepNow ? [214, 208, 192] : s % 4 === 0 ? [224, 218, 204] : PANEL));
      box(m + s * cw, gy + i * rh, cw - 1, rh - 1);
      if (on && rh > lh) {
        const label = hitLabel(hit, Math.floor((cw - 2) / ch));
        if (label) txt(label, m + s * cw + 1, gy + i * rh + Math.floor((rh - lh) / 2), cur ? GLASS : LIT);
      }
    }
  }
  if (stepNow >= 0) { ink(34, 30, 28, 40); box(m + stepNow * cw, gy, cw - 1, rh * 9 - 1); }
  // XY dot
  ink(...CUTE); box(m + Math.floor(xy.x * (gw - 4)), gy + Math.floor((1 - xy.y) * (rh * 9 - 4)), 4, 4);
  footer();
}

// F4: full-screen sequencer. A piano roll on top (pitch up the screen, bars as
// long as each note rings), a drum lane under it, and the 9-track edit grid
// at the bottom, all sharing one step axis.
const BLACK_KEYS = [1, 3, 6, 8, 10];
function seqView({ ink, box, line, txt, w, h, y, m, ch, lh, foot }) {
  const gr = Math.max(5, Math.floor(lh * 0.9)), dl = Math.max(6, lh);
  const gridY = h - foot - 2 - 9 * gr, drumY = gridY - m - dl, rollTop = y, rollBot = drumY - 2;
  const x0 = m + 4 * ch, x1 = w - m, cw = (x1 - x0) / loopSteps, sx = (s) => Math.floor(x0 + s * cw);
  const stepSec = stepMs() / 1000, pos = playing ? ((Date.now() - startAt) / stepMs()) % loopSteps : -1;

  // pitch range: every note in the loop, padded, at least two octaves tall
  const notes = [], beats = [];
  tracks.forEach((t, i) => t.hits.forEach((hit) => {
    if (hit.kind === "note") for (const midi of hit.midis) notes.push({ midi, hit, i });
    else if (hit.kind === "sample") notes.push({ midi: hit.midi, hit, i });
    else beats.push({ hit, i });
  }));
  let lo = notes.length ? Math.min(...notes.map((n) => n.midi)) - 1 : 48, hi = notes.length ? Math.max(...notes.map((n) => n.midi)) + 1 : 72;
  while (hi - lo < 24) { lo--; if (hi - lo < 24) hi++; }
  const ph = (rollBot - rollTop) / (hi - lo + 1), py = (midi) => Math.floor(rollBot - (midi - lo + 1) * ph);

  // lanes: black-key rows shaded, C rows labelled in the gutter
  for (let p = lo; p <= hi; p++) {
    ink(...(BLACK_KEYS.includes(((p % 12) + 12) % 12) ? PANEL : [240, 236, 226])); box(x0, py(p), x1 - x0, Math.ceil(ph));
    if (p % 12 === 0) { ink(...DIM); box(x0, py(p) + Math.ceil(ph) - 1, x1 - x0, 1); txt(noteName(p), m, py(p) + Math.floor(ph / 2) - Math.floor(lh / 2), INK); }
  }
  for (let s = 0; s <= loopSteps; s += 4) { ink(...(s % 16 === 0 ? INK : DIM)); box(sx(s), rollTop, 1, drumY + dl - rollTop); }
  if (mode === "seq") { ink(34, 30, 28, 30); box(sx(cursor.s), rollTop, Math.max(1, Math.floor(cw)), drumY + dl - rollTop); }

  // notes: coloured by pitch, outlined while sounding, named when they fit
  const labels = [];
  for (const { midi, hit, i } of notes) {
    const len = Math.max(1, (hit.dur || 0.25) / stepSec), bx = sx(hit.step), bw = Math.max(2, Math.min(sx(hit.step + len), x1) - bx - 1);
    const by = py(midi), bh = Math.max(2, Math.ceil(ph) - 1), rgb = pitchRgb(midi);
    ink(...(tracks[i].mute ? rgb.map((v, k) => Math.round((v + PANEL[k]) / 2)) : rgb)); box(bx, by, bw, bh);
    if (pos >= hit.step && pos < hit.step + len) { ink(...LIT); box(bx, by, bw, bh, "outline"); }
    const name = noteName(midi);
    if (bw >= name.length * ch + 2) labels.push([name, bx + 2, by + Math.floor(bh / 2) - Math.floor(lh / 2)]);
  }
  for (const [name, lx, ly] of labels) txt(name, lx, ly, LIT);

  // drum lane
  ink(...PANEL); box(x0, drumY, x1 - x0, dl); txt("drm", m, drumY + Math.floor((dl - lh) / 2), INK);
  for (const { hit } of beats) { ink(...tintOf(hit)); box(sx(hit.step), drumY + 1, Math.max(2, Math.floor(cw) - 1), dl - 2); }

  // playhead
  if (pos >= 0) { ink(...CUTE); box(Math.floor(x0 + pos * cw), rollTop, 2, drumY + dl - rollTop); }

  // edit grid: one row per track, same columns as the roll
  for (let i = 0; i < 9; i++) {
    const gy = gridY + i * gr, on = i === cursor.t;
    ink(...TRACK_RGB[i]); box(m, gy, ch * 2, gr - 1);
    txt(String(i + 1), m + ch * 2 + 2, gy + Math.floor((gr - lh) / 2), on ? LIT : INK);
    for (let s = 0; s < loopSteps; s++) {
      const hit = tracks[i].hits.find((hh) => hh.step === s), cur = on && cursor.s === s;
      ink(...(cur ? LIT : hit ? (tracks[i].mute ? DIM : tintOf(hit)) : s % 4 === 0 ? [224, 218, 204] : PANEL));
      box(sx(s), gy, Math.max(1, Math.floor(cw) - 1), gr - 1);
    }
  }
}

function leave({ sound }) { swoops = []; stopAll(sound); if (recording) sound.microphone?.cut?.(); }

export { boot, paint, act, sim, leave };
