// 🧩 What has been done to the pieces on a room's panels (the yard's kiosks:
// a song tapped along, a pad pressed), kept by the server so the piece is
// one piece for everyone. A piece keeps its own state in its own page, so
// the state is not sent: what is kept is what was done to it, each press and
// each letting go in order, and a page that comes later (or comes back, or
// loads again after the server did) does the same to its own copy, silently,
// and stands where everyone else's stands.
//
// For each room and panel: `total`, how many presses and lets-go there have
// ever been, and `events`, the latest of them ([name, x, y, seat]; a slide
// while down is not kept: it changes nothing that lasts). A page says how
// many it has done and does the rest. Kept in the state directory
// (kiosks.json) a few seconds after a change and when the server closes.
import { readFileSync } from 'node:fs';
import { mkdir, writeFile, rename } from 'node:fs/promises';
import { resolve } from 'node:path';

export const KIOSK_EVENTS = 2048; // kept for each panel: a song tapped through many times over
const NAMES = new Set(['touch', 'lift']);

export function createKiosks({ directory = null, limit = KIOSK_EVENTS, later = (fn, ms) => setTimeout(fn, ms).unref?.() } = {}) {
  const file = directory && resolve(directory, 'kiosks.json');
  let rooms = {};
  try {
    const was = file && JSON.parse(readFileSync(file, 'utf8'));
    for (const [room, panels] of Object.entries(was || {}))
      for (const [id, k] of Object.entries(panels || {}))
        if (Number.isInteger(k?.total) && Array.isArray(k.events))
          ((rooms[room] ||= {})[id] = { total: k.total, events: k.events.filter((e) => Array.isArray(e) && NAMES.has(e[0])).slice(-limit), ...(Number.isInteger(k.epoch) ? { epoch: k.epoch } : {}), ...(k.settings ? { settings: k.settings } : {}) });
  } catch {} // none kept yet
  let saved = Promise.resolve(), waiting = false, text = '';
  const keep = () => {
    waiting = false;
    if (!file) return saved;
    const next = JSON.stringify(rooms);
    if (next === text) return saved;
    text = next;
    return (saved = saved.then(async () => {
      await mkdir(directory, { recursive: true });
      await writeFile(`${file}.tmp`, next, { mode: 0o600 });
      await rename(`${file}.tmp`, file);
    }).catch(() => {}));
  };
  return {
    // A press or a letting go on a room's panel, by a seat.
    record(room, id, name, x, y, seat, finger = 1) {
      if (!NAMES.has(name)) return;
      const k = ((rooms[room] ||= {})[id] ||= { total: 0, events: [] });
      k.total++;
      k.events.push(finger === 1 ? [name, x, y, seat] : [name, x, y, seat, finger]);
      if (k.events.length > limit) k.events.splice(0, k.events.length - limit);
      if (!waiting) { waiting = true; later(keep, 5000); }
    },
    settings(room, id, settings) {
      const k = ((rooms[room] ||= {})[id] ||= { total: 0, events: [] });
      k.settings = settings;
      if (!waiting) { waiting = true; later(keep, 5000); }
    },
    // ↻ A piece begun again (its name over the kiosk was pressed): what was done to it is forgotten, and its `epoch` goes up by one, so a page that comes later, or was away, begins its own copy again too before doing what has been done since.
    reset(room, id) {
      const was = rooms[room]?.[id];
      (rooms[room] ||= {})[id] = { total: 0, events: [], epoch: (was?.epoch || 0) + 1 };
      if (!waiting) { waiting = true; later(keep, 5000); }
    },
    // What a page coming into a room is told: { id: { total, events, epoch? } }.
    of(room) { return rooms[room] || {}; },
    keep,
  };
}

// The page's side of the sum: `done` presses and lets-go already done to its
// copy, `kept` what the server has. → the events still to do, oldest first.
// (More behind than the server kept: all that is kept, the nearest it can come.)
export const behind = (done, kept) => (kept && kept.total > done ? kept.events.slice(-Math.min(kept.events.length, kept.total - done)) : []);
