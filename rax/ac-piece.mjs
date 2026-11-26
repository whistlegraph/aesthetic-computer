// 🧩 A real AC piece on a world panel (panel.mjs): the disk itself, loaded
// from AC's own disks/ as it is written, and handed the parts of the api it
// asks for. disk.mjs is not loaded: what a piece takes beyond the painter is
// given here from AC's own libraries (num, geo, ui.Button), and its sound is
// the host's. A kiosk has a keyboard only while someone has stepped in to it (client.mjs), and no hardware MIDI, no general-purpose net and no
// device to keep things on: those parts are there, and quiet.
//
// A slug is read as the prompt reads it: `bleep:sine 4x4` is the piece
// `bleep`, colon ['sine'], params ['4x4'].
// AC's own libraries and disks are served beside rax's pages (server.mjs: /ac/lib, /ac/disks); a test names where they are on disk instead.
const AC = new URL('./ac/', import.meta.url).href;
import { glyphs as chunky, ascent } from './chunky-font.mjs';

// 🎼 A note's name as a frequency and back, as AC's `sound.freq` and `sound.note` say them (disk.mjs keeps its own table; this is the same one): `5C#`, `c#5`, `a`, or a number as it is.
const SEMITONES = { c: 0, 'c#': 1, db: 1, d: 2, 'd#': 3, eb: 3, e: 4, f: 5, 'f#': 6, gb: 6, g: 7, 'g#': 8, ab: 8, a: 9, 'a#': 10, bb: 10, b: 11 };
const C0 = 16.3516;
export function freq(input) {
  if (typeof input === 'number') return input;
  if (input === null || input === undefined) return null;
  const text = String(input).trim().toLowerCase();
  if (text === '') return null;
  if (isFinite(text)) return Number(text);
  if (['rest', 'pause', '_', 'speech'].includes(text)) return null;
  const pre = text.match(/^(-?\d+)([a-z#]+)[+-]*$/), post = text.match(/^([a-z#]+)(-?\d+)[+-]*$/);
  const octave = pre ? Number(pre[1]) : post ? Number(post[2]) : 4;
  const name = (pre ? pre[2] : post ? post[1] : text.replace(/[+-]+$/, '')).replace(/^([a-g])s$/, '$1#').replace(/^([a-g])f$/, '$1b');
  if (!(name in SEMITONES)) throw new Error('Note not found in the list');
  return C0 * 2 ** (octave + SEMITONES[name] / 12);
}
const NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'];
export function note(frequency) {
  if (!(frequency > 0)) return '';
  const n = Math.round(12 * Math.log2(frequency / C0));
  return `${NAMES[((n % 12) + 12) % 12]}${Math.floor(n / 12)}`;
}

let mounted = 0;
// The same dice on every page: a kiosk's piece rolls what it rolls (a bleep's
// tones) from its place's name, so everyone at it has the same pads.
function dice(seed) {
  let h = 2166136261;
  for (const ch of String(seed)) h = Math.imul(h ^ ch.charCodeAt(0), 16777619);
  return () => {
    h = (h + 0x6d2b79f5) | 0;
    let t = Math.imul(h ^ (h >>> 15), 1 | h);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
// 🔤 The one face a panel writes in, as a piece asks after it (`typeface.glyphs['0'].resolution`, `data.advances`): every letter is there, none still loading.
const FACE = { name: 'MatrixChunky8', glyphs: {}, data: { advances: {} }, blockWidth: 4, blockHeight: ascent };
for (const [ch, [w, , , , advance]] of Object.entries(chunky)) { FACE.glyphs[ch] = { resolution: [w, ascent] }; FACE.data.advances[ch] = advance; }
const SILENT = { kill() {}, update() {} };
// Use Notepat's browser sampler. Each press also belongs to its Rax pointer
// so leaving the room, resetting the kiosk, or losing a hand ends the note.
async function notepatControls(base, sound, tell) {
  const [controls, { createGmBridge }, { setGMAssetBase }] = await Promise.all(
    ['notepat-controls', 'notepat-gm-bridge', 'gm'].map(name => import(new URL(`lib/${name}.mjs`, base).href)));
  const cdn = 'https://assets.aesthetic.computer/gm';
  if (setGMAssetBase() !== cdn) setGMAssetBase(cdn);
  const none = () => {};
  return controls.createNotepatControls({
    send: tell, context: sound.sampleContext, destination: sound.sampleOutput, events: null, visibility: null,
    createMidi: () => ({ state: () => ({ supported: false, connected: false, inputs: [], outputs: [], inputId: 'all', outputId: '', channel: 0 }),
      connect: none, selectInput: none, selectOutput: none, selectChannel: none, program: none, noteOn: none, noteOff: none, panic: none, disconnect: none }),
    createGm(options) {
      const gm = createGmBridge(options), held = new Map();
      return {
        ...gm,
        note(event) {
          held.get(event.id)?.kill();
          const voice = sound.holdSample(() => {
            void gm.note(event);
            return { kill(fade) { held.delete(event.id); gm.stop(event.id, fade); } };
          });
          if (voice) held.set(event.id, voice);
        },
        stop(id, fade) { held.get(id)?.kill(fade); },
        panic() { for (const voice of held.values()) voice.kill(0.05); gm.panic(); },
      };
    },
  });
}

// sound: { synth } (the host's voice: → { kill(fade), update({ tone }) }); seed: the dice; clock: () => the shared clock's ms; base: where AC's lib/ and disks/ are; kept: what the kiosk's `store` holds from the start (a setting the piece would have kept on a device).
export async function acPiece(slug, { sound, seed = null, clock = Date.now, base = AC, kept = {} } = {}) {
  const [head, ...params] = String(slug).trim().split(/\s+/), [name, ...colon] = head.split(':');
  if (!/^[a-z0-9-]{1,32}$/.test(name)) throw Error(`no piece ${name}`);
  // (Each panel has the disk to itself: a piece keeps its state in its module, so two of one piece are two loads. AC's buttons are loaded for each panel too: ui.mjs keeps one list of the buttons held down for the whole page, and tidies it as one screen's, so with a row of kiosks one panel's press was let go, or left stuck, by another's: 2026-11-20.)
  const mine = ++mounted;
  const [num, geo, { Button, TextButtonSmall }] = await Promise.all(['num', 'geo', `ui`].map((lib) => import(`${new URL(`lib/${lib}.mjs`, base).href}${lib === 'ui' ? `?panel=${mine}` : ''}`)));
  const disk = await import(`${new URL(`disks/${name}.mjs`, base).href}?panel=${mine}`);
  // 🔤 A panel writes in the one small face (MatrixChunky8), so a piece's text buttons are AC's small ones.
  class TextButton extends TextButtonSmall {
    replaceLabel(text) { this.txt = text; }
  }
  const roll = seed === null ? null : dice(seed);
  const nums = roll ? { ...num, randInt: (n) => Math.floor(roll() * (n + 1)), randIntRange: (lo, hi) => lo + Math.floor(roll() * (hi - lo + 1)) } : num;
  // 🔊 A voice asked for with an end (`duration` in seconds) is let go then; one with none ('🔁') is held until the piece lets it go.
  const synth = (options = {}) => {
    const voice = sound.synth({ ...options, tone: freq(options.tone) });
    if (Number.isFinite(options.duration)) setTimeout(() => voice.kill(Math.min(0.08, options.duration * 0.2)), options.duration * 1000);
    return { startedAt: performance.now() / 1000, kill: (fade) => voice.kill(fade), update: (to = {}) => voice.update?.({ ...to, ...(to.tone === undefined ? {} : { tone: freq(to.tone) }) }) };
  };
  // 📮 What the page would answer a piece with (`notepat:state`, `notepat:control`) is heard as an event, after whatever the piece is in the middle of.
  let controls = null, last = null, busy = false, booting = false;
  const post = [];
  const deliver = () => {
    if (busy || booting || !last) return;
    busy = true;
    try { for (let m; (m = post.shift()); ) disk.act?.({ ...last, event: { ...m.content, name: m.type, device: m.type, is: (q) => q === m.type } }); }
    finally { busy = false; }
  };
  if (name === 'notepat') controls = await notepatControls(base, sound, message => { post.push(message); deliver(); });
  const send = message => { if (message?.type === 'notepat:host') controls?.message(message.content); };
  const host = {
    params, colon, num: nums, geo, ui: { Button, TextButton },
    // 🔇 The parts of AC's sound a kiosk has none of: no DAW to keep time with (`daw` is not there, as on any page outside one), a speaker that reports silence (so a piece's scope is a flat line), samples that play nothing (`play`: notepat's stample voice is silent here), no MIDI.
    sound: { ...sound, synth, freq, note, play: () => ({ ...SILENT }), sampleRate: 48000, get time() { return performance.now() / 1000; },
      speaker: { poll() {}, waveforms: { left: [], right: [] }, amplitudes: { left: 0, right: 0 } },
      kill() {}, room: { toggle() {}, set() {}, setMix() {}, get() {} }, glitch: { toggle() {}, set() {}, get() {} }, registerSample() {}, midi: { noteOn() {}, noteOff() {} } },
    // 🕰️ The shared clock, so a song played as a guide keeps the room's time.
    clock: { time: () => new Date(clock()), resync() {} },
    // 💾 A kiosk keeps nothing for a visitor: what a piece would store on a device is held only while the page is open.
    store: { ...kept, retrieve: async (key) => host.store[key], persist() {}, delete(key) { delete host.store[key]; } },
    send, jump() {}, beep() {}, dark: true, typeface: FACE, glyphs: () => FACE.glyphs, query: {}, handle: () => null, fps() {},
    // 🌐 No net: nothing is fetched (a sample asked for comes back as nothing), and nothing is told to a relay.
    net: { preload: async () => undefined, udp: () => ({ send() {}, receive() {}, connected: false }), socket: () => ({ send() {}, close() {}, connected: false }), log() {} },
    help: { resampleArray: (from, n) => Array.from({ length: n }, (_, k) => from[Math.floor((k / n) * from.length)] ?? 0) },
    // 🏷️ The corner label is AC's page's to draw, and a panel has none (its sign is over it).
    hud: { label() {}, suffix() {}, currentLabel: () => undefined }, needsPaint() {}, cursor() {},
  };
  // AC numbers its pointers (`touch:1`, `draw:1`, `lift:1`), and a piece may ask for any of them (`touch:any`).
  const act = ($) => {
    // ⌨️ A key is handed on as AC's own keyboard would say it (panel.mjs `keyEvent`).
    if ($.event.device === 'keyboard') {
      if (booting) return;
      busy = true;
      try { disk.act?.($); } finally { busy = false; }
      return deliver();
    }
    const e = $.event, n = e.pointer || 1, name = e.name === 'move' ? 'move' : `${e.name}:${n}`;
    // Touch gestures carry their own deltas; a drifting crosshair keeps its note steady.
    const delta = e.delta || { x: 0, y: 0 };
    if (booting) return;
    busy = true;
    try { disk.act?.({ ...$, event: { ...e, name, pointer: n, delta, is: (q) => name === q || name.startsWith(`${q}:`) || (q.endsWith(':any') && name.startsWith(q.slice(0, -3))) } }); }
    finally { busy = false; }
    deliver();
  };
  // ⏳ A piece whose boot waits on something (notepat's, on its store) is not painted, stepped or pressed until it has booted, as AC holds a piece: `ready()` is when. One that boots at once is running at once.
  let booted, isReady;
  const ready = new Promise((yes) => { isReady = yes; });
  const boot = ($) => {
    last = $;
    booting = true;
    booted = disk.boot?.($);
    if (!booted?.then) { booting = false; isReady(); return deliver(); }
    booted.catch((e) => console.warn(`rax: ${name} did not boot:`, e?.message || e)).then(() => { booting = false; isReady(); deliver(); });
  };
  const when = (f) => f && (($) => { if (!booting) { last = $; f($); } });
  const sim = ($) => { if (booting) return; last = $; deliver(); disk.sim?.($); };
  const leave = ($) => { disk.leave?.($); controls?.dispose(); };
  return { slug, piece: { boot, paint: when(disk.paint), sim, leave, act, keys: true }, host, ready: () => ready }; // (keys: an AC disk hears a keyboard)
}

// 🎹 A big notepat surface: notepat's two classic octaves of pads, laid out
// and coloured by notepat's own libraries (lib/notepat-layout.mjs,
// lib/notepat-render.mjs), each note in its hue. A pad held is a note held;
// a slide to the next pad moves the note; every hand in the room has its own
// (panel.mjs tells whose). This is not the notepat disk, only its playing
// surface: the disk itself is hosted by `acPiece('notepat…')`, and this
// stands in for it where the disk does not load.
const PAD_NOTES = ['c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b'];
export async function notepatPiece({ sound, wave = 'sine', octave = 4, base = AC } = {}) {
  const [{ classicNotepatLayout }, { notepatNoteColor, notepatPadColors, notepatPalette }] = await Promise.all(
    ['notepat-layout', 'notepat-render'].map((lib) => import(new URL(`lib/${lib}.mjs`, base).href)));
  const palette = notepatPalette(true);
  let pads = [];
  const held = new Map(); // pointer → { pad, voice }
  const padAt = (x, y) => pads.find((p) => x >= p.x && x < p.x + p.w && y >= p.y && y < p.y + p.h) || null;
  const start = (pointer, pad) => held.set(pointer, { pad, voice: sound.synth({ type: wave, tone: freq(`${pad.octave}${pad.note}`), duration: '🔁' }) });
  const stop = (pointer, fade = 0.08) => { held.get(pointer)?.voice?.kill(fade); held.delete(pointer); };
  return {
    boot({ screen }) {
      const lay = classicNotepatLayout(screen, { top: 11, bottom: 1 });
      pads = [];
      for (let side = 0; side < 2; side++)
        for (let n = 0; n < 12; n++) {
          const col = n % 4, row = Math.floor(n / 4);
          const x = (side ? lay.rightOctaveX : lay.leftOctaveX) + col * lay.buttonWidth;
          const y = lay.topButtonY + (lay.splitLayout ? row : row + side * 3) * lay.buttonHeight + (lay.splitLayout ? 0 : side * lay.octaveGap);
          pads.push({ note: PAD_NOTES[n], octave: octave + side, x: lay.splitLayout ? x : lay.leftOctaveX + col * lay.buttonWidth, y, w: lay.buttonWidth, h: lay.buttonHeight });
        }
    },
    paint({ wipe, ink, box, write, text, screen }) {
      wipe(palette.background);
      ink(palette.bar).box(0, 0, screen.width, 10);
      ink(palette.text).write('notepat', { x: 3, y: 1 });
      const sounding = [...held.values()].map((h) => `${h.pad.note}${h.pad.octave}`.toUpperCase()).join(' ');
      if (sounding) ink(palette.accent).write(sounding, { x: screen.width - text.width(sounding) - 3, y: 1 });
      for (const pad of pads) {
        const sharp = pad.note.includes('#'), active = [...held.values()].some((h) => h.pad === pad);
        const colors = notepatPadColors(notepatNoteColor(pad.note), sharp, true, active);
        ink(colors.fill).box(pad.x + 1, pad.y + 1, pad.w - 2, pad.h - 2);
        const label = pad.note.toUpperCase();
        ink(colors.text).write(label, { x: pad.x + Math.floor((pad.w - text.width(label)) / 2), y: pad.y + Math.floor((pad.h - text.height()) / 2) });
      }
    },
    act({ event: e }) {
      const pointer = e.pointer || 1, pad = e.x >= 0 ? padAt(e.x, e.y) : null;
      if (e.is('touch')) { stop(pointer, 0.02); if (pad) start(pointer, pad); }
      else if (e.is('draw')) { const now = held.get(pointer); if (now && pad && pad !== now.pad) { stop(pointer, 0.04); start(pointer, pad); } }
      else if (e.is('lift')) stop(pointer);
    },
    leave() { for (const pointer of [...held.keys()]) stop(pointer, 0.05); },
  };
}

// 🪧 The sign over a panel: what runs on it, as it would be typed.
// ↻ Given `press`, the sign is a button: pointed at it reads `reset`, and pressed it begins the piece under it again (a rule for every AC piece on a panel: its name is the way back to its beginning).
export const signPiece = (label, press = null) => {
  let over = false, down = false;
  return {
    paint({ wipe, ink, write, screen, text }) {
      const words = press && over ? 'reset' : label;
      wipe(...(press && down ? [70, 60, 20] : press && over ? [34, 40, 58] : [16, 20, 30]));
      // (Its name at the top left, where AC's own pages write theirs.)
      ink(...(press && over ? [255, 255, 255] : [255, 224, 102])).write(words, { x: 3, y: Math.floor((screen.height - text.height()) / 2) });
    },
    act({ event: e }) {
      if (!press) return;
      over = e.x >= 0 && e.y >= 0;
      if (e.is('touch') && over) down = true;
      if (e.is('lift')) { if (down && over) press(); down = false; }
    },
  };
};
