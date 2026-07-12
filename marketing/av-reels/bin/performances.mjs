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
// Für Elise, full Section A (the famous melody). Played through TWICE so the
// reel runs ~24s, the second pass ending on a held A-minor chord (multitouch).
const A_SECTION = [
  ["j", 300], ["y", 300], ["j", 300], ["y", 300], ["j", 300], ["b", 300], ["i", 300], ["h", 300], ["a", 640],
  ["c", 300], ["e", 300], ["a", 300], ["b", 640],
  ["e", 300], ["r", 300], ["b", 300], ["h", 640],
  ["e", 300],
  ["j", 300], ["y", 300], ["j", 300], ["y", 300], ["j", 300], ["b", 300], ["i", 300], ["h", 300], ["a", 640],
  ["c", 300], ["e", 300], ["a", 300], ["b", 640],
  ["e", 300], ["h", 300], ["b", 300], ["a", 640],
];
const FUR = [
  ...A_SECTION,                      // full theme (~14s)
  ...A_SECTION.slice(0, 18),         // reprise the opening phrase (~7s)
  [["a", "h", "j"], 2000],           // ...resolving on a held A-minor chord
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

// Bubble is a DRAG instrument: touch-down starts a sustained tone, and dragging
// changes its size/pitch by distance from the screen CENTER (small at center →
// large at the edge). So we press and drag the pointer out-and-back, sweeping
// the bubble bigger/smaller. Each leg is subdivided into ~55ms mouse-moves so
// the sim samples a smooth path (a single jump would snap the size).
// ONE long continuous drag: a single sustained touch that slowly spirals so the
// radius (= bubble size/pitch) breathes in and out — smooth and hypnotic, NOT
// the staccato "rapid tap" feel of several separate touches. Sampled every
// ~55ms so the sim gets a fine, continuous path (esp. important at bubble's
// choppy headless paint rate).
const BUB_CX = 0.5, BUB_CY = 0.42;
const bubbleDrag = (() => {
  const acts = []; const id = ++_id;
  const T0 = 600, DUR = 9400, STEP = 55;
  acts.push({ t: T0, type: "mdown", x: BUB_CX, y: BUB_CY, id });
  for (let ms = STEP; ms <= DUR; ms += STEP) {
    const u = ms / DUR;
    // radius breathes small↔large ~3 times over the drag; angle rotates slowly
    const rad = 0.07 + 0.32 * (0.5 - 0.5 * Math.cos(u * Math.PI * 2 * 3));
    const ang = u * Math.PI * 2 * 1.25 + Math.sin(u * Math.PI * 4) * 0.5;
    let x = BUB_CX + Math.cos(ang) * rad * 0.62; // squash x so it stays in the 9:16 frame
    let y = BUB_CY + Math.sin(ang) * rad;
    x = Math.max(0.06, Math.min(0.94, x));
    y = Math.max(0.06, Math.min(0.9, y));
    acts.push({ t: T0 + ms, type: "move", x, y, id });
  }
  acts.push({ t: T0 + DUR + 140, type: "mup", id });
  return acts;
})();
export const BUBBLE_DRAG_MS = bubbleDrag.reduce((m, a) => Math.max(m, a.t), 0) + 400;

// line is a nopaint drawing piece: each held stroke draws + BAKES, so strokes
// accumulate into a picture. This draws a series of DIFFERENT lines — diagonal,
// wavy, arc, zigzag, spiral, vertical wave — each a continuous drag (proven to
// draw a clean connected polyline). The marker rides the pen; each stroke start
// drops a displacement "droplet."
function stroke(startT, dur, fn) {
  const acts = []; const id = ++_id; const N = Math.max(6, Math.round(dur / 45));
  const p0 = fn(0); acts.push({ t: startT, type: "mdown", x: p0[0], y: p0[1], id });
  for (let i = 1; i <= N; i++) { const u = i / N; const p = fn(u); acts.push({ t: Math.round(startT + dur * u), type: "move", x: p[0], y: p[1], id }); }
  acts.push({ t: startT + dur + 90, type: "mup", id });
  return { acts, end: startT + dur + 90 };
}
const lineDraw = (() => {
  const out = []; let t = 900;
  const shapes = [
    { dur: 3200, fn: (u) => [0.14 + 0.72 * u, 0.18 + 0.5 * u] },                                   // diagonal
    { dur: 3800, fn: (u) => [0.14 + 0.72 * u, 0.5 + 0.2 * Math.sin(u * Math.PI * 4)] },             // wavy
    { dur: 4200, fn: (u) => [0.5 + 0.34 * Math.cos(Math.PI * (0.15 + u * 1.7)), 0.46 + 0.34 * Math.sin(Math.PI * (0.15 + u * 1.7))] }, // arc
    { dur: 3800, fn: (u) => { const z = Math.floor(u * 6), f = u * 6 - z; return [0.16 + 0.68 * u, z % 2 ? 0.68 - 0.28 * f : 0.4 + 0.28 * f]; } }, // zigzag
    { dur: 4600, fn: (u) => { const a = u * Math.PI * 4.5, r = 0.04 + 0.3 * u; return [0.5 + Math.cos(a) * r * 0.62, 0.46 + Math.sin(a) * r]; } }, // spiral
    { dur: 3200, fn: (u) => [0.5 + 0.32 * Math.sin(u * Math.PI * 2.5), 0.16 + 0.64 * u] },          // vertical wave
  ];
  for (const s of shapes) { const g = stroke(t, s.dur, s.fn); out.push(...g.acts); t = g.end + 260; }
  return out;
})();
export const LINE_DRAW_MS = lineDraw.reduce((m, a) => Math.max(m, a.t), 0) + 500;

// ── line PAINTING SESSION: prompt ↔ line params, building one drawing ────────
// Start at the prompt; `new` resets the painting; then for each color we type
// `line:<n> <color>` at the prompt, draw strokes, and Escape back to the prompt.
// Strokes bake into ONE persistent painting → a house/sun/tree landscape.
// (Requires --grab capture: line/prompt go static between strokes.)
const AR = 0.56; // x-multiplier so circles read round in the 9:16 frame
// polyline stroke through normalized waypoints, even speed by arc-length
function polyStroke(startT, dur, pts) {
  const acts = []; const id = ++_id;
  const segs = []; let total = 0;
  for (let i = 0; i < pts.length - 1; i++) { const L = Math.hypot(pts[i + 1][0] - pts[i][0], pts[i + 1][1] - pts[i][1]); segs.push(L); total += L; }
  acts.push({ t: startT, type: "mdown", x: pts[0][0], y: pts[0][1], id });
  const N = Math.max(pts.length, Math.round(dur / 50));
  for (let s = 1; s <= N; s++) {
    let dist = (s / N) * total, i = 0;
    while (i < segs.length - 1 && dist > segs[i]) { dist -= segs[i]; i++; }
    const f = segs[i] > 0 ? Math.min(1, dist / segs[i]) : 0;
    acts.push({ t: Math.round(startT + dur * (s / N)), type: "move", x: pts[i][0] + (pts[i + 1][0] - pts[i][0]) * f, y: pts[i][1] + (pts[i + 1][1] - pts[i][1]) * f, id });
  }
  acts.push({ t: startT + dur + 90, type: "mup", id });
  return { acts, end: startT + dur + 90 };
}
function ring(cx, cy, r, turns, n) {
  const p = []; for (let i = 0; i <= n; i++) { const u = i / n, a = u * Math.PI * 2 * turns, rr = r * (0.32 + 0.68 * u); p.push([cx + Math.cos(a) * rr * AR, cy + Math.sin(a) * rr]); } return p;
}
const linePainting = (() => {
  const acts = []; let t = 800;
  const LOAD = 1900, ESC = 1500, GAP = 380;
  // pad before typing so the prompt is fully focused (first keystroke was racing
  // and dropping a char → "Did you mean 'line'?"); a throwaway key wakes focus.
  const cmd = (text) => {
    t += 450;
    acts.push({ t, type: "press", key: "ArrowLeft" }); t += 120;
    acts.push({ t, type: "type", text }); t += text.length * 55 + 150;
    acts.push({ t, type: "press", key: "Enter" }); t += 220 + LOAD;
  };
  const esc = () => { acts.push({ t, type: "press", key: "Escape" }); t += ESC; };
  const draw = (dur, pts) => { const s = polyStroke(t, dur, pts); acts.push(...s.acts); t = s.end + GAP; };

  cmd("new");                                    // blank canvas
  cmd("line:5 yellow");                           // ☀ sun
  draw(2600, ring(0.72, 0.24, 0.085, 2.4, 34));
  esc();
  cmd("line:4 green");                            // 🌿 ground + foliage
  draw(2200, [[0.12, 0.73], [0.32, 0.70], [0.5, 0.735], [0.7, 0.70], [0.88, 0.73]]);
  draw(2000, ring(0.24, 0.52, 0.065, 2.2, 26));
  esc();
  cmd("line:3 black");                            // 🏠 house + door + trunk
  draw(2600, [[0.42, 0.72], [0.42, 0.54], [0.60, 0.54], [0.60, 0.72], [0.42, 0.72]]);
  draw(1500, [[0.48, 0.72], [0.48, 0.63], [0.54, 0.63], [0.54, 0.72]]);
  draw(1200, [[0.24, 0.72], [0.24, 0.575]]);
  esc();
  cmd("line:4 red");                              // 🔺 roof
  draw(1800, [[0.40, 0.545], [0.51, 0.42], [0.62, 0.545]]);
  esc();
  cmd("line:2 blue");                             // 🐦 birds
  draw(1400, [[0.30, 0.30], [0.33, 0.27], [0.36, 0.30], [0.40, 0.28], [0.43, 0.25], [0.46, 0.28]]);
  return acts;
})();
export const LINE_PAINTING_MS = linePainting.reduce((m, a) => Math.max(m, a.t), 0) + 900;

export const PERFORMANCES = {
  "notepat-furelise": notepatFurElise,
  "notepat-melody": notepatMelody,
  "bubble-taps": bubbleTaps,
  "bubble-drag": bubbleDrag,
  "line-draw": lineDraw,
  "line-painting": linePainting,
};
