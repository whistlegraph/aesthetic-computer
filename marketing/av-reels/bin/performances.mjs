// performances.mjs — timed input "scores" that play AC instruments during
// capture. AC pieces are silent without input; these make them sing. Actions
// carry a normalized x,y (0..1) that ALSO drives the on-screen finger overlay,
// and an `id` so the overlay can pair each press with its release (needed for
// notepat, which is MULTITOUCH — chords = several fingers down at once).
//
// Action shapes (t = ms from capture start):
//   { t, type:"down",  key, x, y, id }  hold a note (id pairs with its up)
//   { t, type:"up",    key, x, y, id }  release it
//   { t, type:"tap",   x, y, id }       click (finger taps + lifts)
//   { t, type:"mdown", x, y, id }/{t,type:"mup",id}  press-and-HOLD a pointer
//   { t, type:"type",  text }           type a string

// ── notepat tile map (measured at 1080×1920) ────────────────────────────────
// 4 cols × 6 rows; key → [col,row]. Triggering by KEY reliably plays the note
// AND lights the tile; we attach the tile's center so the finger presses it.
const NP_COLS = [0.125, 0.375, 0.625, 0.875];
const NP_ROWS = [0.262, 0.396, 0.530, 0.665, 0.798, 0.933];
const NP_KEY = {
  c: [0, 0], v: [1, 0], d: [2, 0], s: [3, 0],
  e: [0, 1], f: [1, 1], w: [2, 1], g: [3, 1],
  r: [0, 2], a: [1, 2], q: [2, 2], b: [3, 2],
  h: [0, 3], t: [1, 3], i: [2, 3], y: [3, 3],
  j: [0, 4], k: [1, 4], u: [2, 4], l: [3, 4],
  o: [0, 5], m: [1, 5], p: [2, 5], n: [3, 5],
};
export function npPos(key) { const [c, r] = NP_KEY[key]; return { x: NP_COLS[c], y: NP_ROWS[r] }; }

// Beethoven — "Für Elise" (WoO 59) opening melodic line on notepat keys, ending
// on a held A-minor chord (A4+C5+E5 = a+h+j) to show off multitouch (3 fingers).
//   E5 +E=j · D#5 +D#=y · B4 B=b · D5 +D=i · C5 +C=h · A4 A=a · C4 C=c · E4 E=e · G#4 G#=r
// Entry = [key(s), ms]; an array of keys is a chord (held together).
const FUR = [
  ["j", 260], ["y", 260], ["j", 260], ["y", 260], ["j", 260], ["b", 260], ["i", 260], ["h", 520],
  ["a", 720], ["c", 260], ["e", 260], ["a", 520],
  ["b", 720], ["e", 260], ["r", 260], ["b", 520],
  ["h", 720], ["e", 260],
  [["a", "h", "j"], 1600],
];
let _id = 0;
function buildFurElise() {
  const acts = []; let t = 700; const gap = 34;
  for (const [spec, dur] of FUR) {
    const keys = Array.isArray(spec) ? spec : [spec];
    for (const key of keys) {
      const p = npPos(key); const id = ++_id;
      acts.push({ t, type: "down", key, x: p.x, y: p.y, id });
      acts.push({ t: t + Math.max(90, dur - 60), type: "up", key, x: p.x, y: p.y, id });
    }
    t += dur + gap;
  }
  return acts;
}
const notepatFurElise = buildFurElise();
export const NOTEPAT_FURELISE_MS = notepatFurElise.reduce((m, a) => Math.max(m, a.t), 0) + 500;

function noteSeq(keys, start, gap, hold) {
  const acts = []; let t = start;
  for (const k of keys) {
    const p = npPos(k); const id = ++_id;
    acts.push({ t, type: "down", key: k, x: p.x, y: p.y, id });
    acts.push({ t: t + hold, type: "up", key: k, x: p.x, y: p.y, id });
    t += gap;
  }
  return acts;
}
const notepatMelody = noteSeq(["a", "s", "d", "f", "g", "f", "d", "s"], 400, 460, 380);

// Bubble: taps float reverbed tones; positions climb so pitch/pan sweep. Each
// tap gets an id so the finger taps + lifts cleanly.
const bubbleTaps = [
  [600, 0.3, 0.72], [1500, 0.55, 0.6], [2400, 0.7, 0.5], [3300, 0.4, 0.45],
  [4200, 0.6, 0.36], [5100, 0.35, 0.54], [6000, 0.65, 0.32], [6900, 0.5, 0.42],
  [7800, 0.45, 0.6], [8700, 0.6, 0.5],
].map(([t, x, y]) => ({ t, type: "tap", x, y, id: ++_id }));

export const PERFORMANCES = {
  "notepat-furelise": notepatFurElise,
  "notepat-melody": notepatMelody,
  "bubble-taps": bubbleTaps,
};
