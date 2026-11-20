// 🧩 A real AC piece on a world panel (panel.mjs): the disk itself, loaded
// from AC's own disks/ as it is written, and handed the parts of the api it
// asks for. disk.mjs is not loaded: what a piece takes beyond the painter is
// given here from AC's own libraries (num, geo, ui.Button), and its sound is
// the host's.
//
// A slug is read as the prompt reads it: `bleep:sine 4x4` is the piece
// `bleep`, colon ['sine'], params ['4x4'].
import * as num from './ac/lib/num.mjs';
import * as geo from './ac/lib/geo.mjs';
import { Button } from './ac/lib/ui.mjs';

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
export async function acPiece(slug, { sound, seed = null } = {}) {
  const [head, ...params] = String(slug).trim().split(/\s+/), [name, ...colon] = head.split(':');
  if (!/^[a-z0-9-]{1,32}$/.test(name)) throw Error(`no piece ${name}`);
  // (Each panel has the disk to itself: a piece keeps its state in its module, so two of one piece are two loads.)
  const disk = await import(`./ac/disks/${name}.mjs?panel=${++mounted}`);
  const roll = seed === null ? null : dice(seed);
  const nums = roll ? { ...num, randInt: (n) => Math.floor(roll() * (n + 1)), randIntRange: (lo, hi) => lo + Math.floor(roll() * (hi - lo + 1)) } : num;
  const host = { params, colon, num: nums, geo, ui: { Button }, sound, hud: { label() {} }, needsPaint() {}, cursor() {}, pens: () => [] };
  // AC numbers its pointers (`touch:1`, `draw:1`, `lift:1`), and a piece may ask for any of them (`touch:any`); a panel has the one.
  const act = ($) => {
    // (This page's own pointer is 1; anyone else's is their seat's, told by the panel: panel.mjs `hear`.)
    const e = $.event, n = e.pointer || 1, name = e.name === 'move' ? 'move' : `${e.name}:${n}`;
    disk.act?.({ ...$, event: { ...e, name, pointer: n, is: (q) => name === q || name.startsWith(`${q}:`) || (q.endsWith(':any') && name.startsWith(q.slice(0, -3))) } });
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
