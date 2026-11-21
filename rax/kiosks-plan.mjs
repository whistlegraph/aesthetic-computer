// 🧩 Which kiosks stand where: the pieces on panels that a room is given (the
// page mounts them, client.mjs mountYard; the server takes a press only on
// one of them and keeps what was done to each by its id, kiosks.mjs). The
// places are worked out from the tower's own measurements (elevator.mjs), so
// a wall or a door that moves takes its kiosks with it.
//
// A kiosk: { id, piece, x, y, z, yaw, w, h, cols, rows, sign }. `piece` is
// the slug as typed at AC's prompt; (x, y, z) the middle of its face in the
// room's metres; `yaw` the way it is turned (0 faces south, 90 west, 180
// north, 270 east: it is worked from in front of its face); w by h metres of
// cols by rows pixels; `sign` whether its slug hangs over it. An id is kept
// for good: what was done to a kiosk follows it.
import { LOBBY_FRONT, DOOR, lobbyWalls } from './elevator.mjs';

export const KIOSK_DEEP = 0.1; // a kiosk's slab, behind its face
const STEP = 0.25; // a kiosk's face stands this far off the glass it backs on to
const EYE = 1.35; // the middle of a pad or a song: a hand's height for whoever stands at it
const PAD = { w: 1.2, h: 1.2, cols: 64, rows: 64 }, SONG = { w: 1.1, h: 1.1, cols: 128, rows: 128 };
export const SONGS = ['twinkle', 'row', 'weasel', 'kid', 'mmm']; // disks/song.mjs knows these

function lobbyPlan() {
  const out = [];
  // 🔔 Two bleep pads side by side on the lobby's front, west of the door, facing the yard.
  for (let k = 0; k < 2; k++)
    out.push({ id: `yard-${k}`, piece: 'bleep:sine 4x4', x: -(DOOR + 1.3 + (1 - k) * 1.6), y: EYE, z: LOBBY_FRONT - STEP, yaw: 0, ...PAD, sign: true });
  // 🎶 The song hall: the songs AC knows in one row inside the lobby, along the
  // back of its front glass east of the door, facing in. They fill the wall
  // from a hand off the door's jamb to a step short of the hall's east end,
  // and are read from the left by whoever faces them: the first is nearest
  // the door. The walk from the door to the car passes beside them.
  const end = Math.min(...lobbyWalls().filter((w) => w.xMin > DOOR && w.zMax > LOBBY_FRONT + 1).map((w) => w.xMin)); // the east end's inner face
  const first = DOOR + 0.15 + SONG.w / 2, last = end - STEP - SONG.w / 2, n = SONGS.length;
  const pitch = Math.floor(1000 * (last - first) / (n - 1)) / 1000;
  SONGS.forEach((name, k) =>
    out.push({ id: `yard-${2 + k}`, piece: `song ${name}`, x: first + k * pitch, y: EYE, z: LOBBY_FRONT + STEP, yaw: 180, ...SONG, sign: true }));
  // 🎹 The big notepat board, across the courtyard's south-west side, facing the tower. (No measurement fixes its place: it is where it was put.)
  out.push({ id: 'yard-notepat', piece: 'notepat:sine', x: -4.9, y: 1.6, z: -7.6, yaw: 180, w: 4.8, h: 2.4, cols: 320, rows: 160, sign: false });
  return out.map((k) => ({ ...k, x: Math.round(k.x * 1000) / 1000, z: Math.round(k.z * 1000) / 1000 }));
}
// 🪧 A sign over a group of kiosks, saying what they are: { id, text, x, y, z, yaw, w, h, cols, rows }.
function lobbySigns(plan) {
  const songs = plan.filter((k) => k.piece.startsWith('song ')), xs = songs.map((k) => k.x);
  // (Over the songs' own signs, which end 0.27 m over their faces.)
  return [{ id: 'yard-songs-sign', text: 'songs', x: Math.round((Math.min(...xs) + Math.max(...xs)) * 500) / 1000, y: EYE + SONG.h / 2 + 0.27 + 0.1 + 0.3, z: songs[0].z, yaw: songs[0].yaw, w: 2.4, h: 0.6, cols: 48, rows: 12 }];
}

const plans = new Map(), signs = new Map();
// The kiosks standing in a room (none in most).
export function kioskPlan(room) {
  if (!plans.has(room)) plans.set(room, Object.freeze(room === 'lobby' ? lobbyPlan() : []));
  return plans.get(room);
}
// The signs that hang over its groups of kiosks.
export function kioskSigns(room) {
  if (!signs.has(room)) signs.set(room, Object.freeze(room === 'lobby' ? lobbySigns(kioskPlan(room)) : []));
  return signs.get(room);
}

// 📐 A kiosk as the box it fills, slab and all ({ type: 'box', xMin … }), and
// the ground in front of its face out to `reach` metres: where it is worked from.
const turned = (k) => { const a = (k.yaw * Math.PI) / 180; return { fx: -Math.sin(a), fz: -Math.cos(a) }; }; // the way its face looks (panel.mjs panelHit)
export function kioskBox(k, deep = KIOSK_DEEP) {
  const { fx, fz } = turned(k), along = Math.abs(fz) > 0.5 ? 'x' : 'z';
  const span = (mid, f) => (f ? [Math.min(mid, mid - f * deep), Math.max(mid, mid - f * deep)] : [mid - k.w / 2, mid + k.w / 2]);
  const [xMin, xMax] = span(k.x, along === 'x' ? 0 : Math.sign(fx)), [zMin, zMax] = span(k.z, along === 'x' ? Math.sign(fz) : 0);
  return { type: 'box', xMin, xMax, yMin: k.y - k.h / 2, yMax: k.y + k.h / 2, zMin, zMax };
}
export function kioskFront(k, distance) {
  const { fx, fz } = turned(k);
  return { x: k.x + fx * distance, z: k.z + fz * distance };
}
