// 🧩 A real AC piece on a world panel (panel.mjs): the disk itself, loaded
// from AC's own disks/ as it is written, and handed the parts of the api it
// asks for. disk.mjs is not loaded: what a piece takes beyond the painter is
// given here from AC's own libraries (num, geo, ui.Button), and its sound is
// the host's.
//
// A slug is read as the prompt reads it: `bleep:sine 4x4` is the piece
// `bleep`, colon ['sine'], params ['4x4'].
// AC's own libraries and disks are served beside rax's pages (server.mjs: /ac/lib, /ac/disks); a test names where they are on disk instead.
const AC = new URL('./ac/', import.meta.url).href;

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
// sound: { synth } (the host's voice: → { kill(fade), update({ tone }) }); seed: the dice; clock: () => the shared clock's ms; base: where AC's lib/ and disks/ are.
export async function acPiece(slug, { sound, seed = null, clock = Date.now, base = AC } = {}) {
  const [head, ...params] = String(slug).trim().split(/\s+/), [name, ...colon] = head.split(':');
  if (!/^[a-z0-9-]{1,32}$/.test(name)) throw Error(`no piece ${name}`);
  const [num, geo, { Button, TextButtonSmall }] = await Promise.all(['num', 'geo', 'ui'].map((lib) => import(new URL(`lib/${lib}.mjs`, base).href)));
  // (Each panel has the disk to itself: a piece keeps its state in its module, so two of one piece are two loads.)
  const disk = await import(`${new URL(`disks/${name}.mjs`, base).href}?panel=${++mounted}`);
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
    return { kill: (fade) => voice.kill(fade), update: (to = {}) => voice.update?.({ ...to, ...(to.tone === undefined ? {} : { tone: freq(to.tone) }) }) };
  };
  const host = {
    params, colon, num: nums, geo, ui: { Button, TextButton },
    sound: { ...sound, synth, freq, note },
    // 🕰️ The shared clock, so a song played as a guide keeps the room's time.
    clock: { time: () => new Date(clock()), resync() {} },
    // 💾 A kiosk keeps nothing for a visitor: what a piece would store on a device is not kept here.
    store: { retrieve: async () => undefined, persist() {}, delete() {} },
    send() {}, jump() {}, dark: true,
    hud: { label() {} }, needsPaint() {}, cursor() {}, pens: () => [],
  };
  // AC numbers its pointers (`touch:1`, `draw:1`, `lift:1`), and a piece may ask for any of them (`touch:any`); a panel has the one.
  const act = ($) => {
    // (This page's own pointer is 1; anyone else's is their seat's, told by the panel: panel.mjs `hear`.)
    const e = $.event, n = e.pointer || 1, name = e.name === 'move' ? 'move' : `${e.name}:${n}`;
    // 🎯 The pointer here is a crosshair, and it is never quite still: a drag has no `delta`, so a note held is held true (song bends its pitch by a drag's delta, and wavered with every breath of the aim: 2026-11-20).
    const delta = { x: 0, y: 0 };
    disk.act?.({ ...$, event: { ...e, name, pointer: n, delta, is: (q) => name === q || name.startsWith(`${q}:`) || (q.endsWith(':any') && name.startsWith(q.slice(0, -3))) } });
  };
  return { slug, piece: { boot: disk.boot, paint: disk.paint, sim: disk.sim, leave: disk.leave, act }, host };
}

// 🪧 The sign over a panel: what runs on it, as it would be typed.
export const signPiece = (label) => ({
  paint({ wipe, ink, write, screen, text }) {
    wipe(16, 20, 30);
    ink(255, 224, 102).write(label, { x: Math.max(1, Math.floor((screen.width - text.width(label)) / 2)), y: Math.floor((screen.height - text.height()) / 2) });
  },
});
