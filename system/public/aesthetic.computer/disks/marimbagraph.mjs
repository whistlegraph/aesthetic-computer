// marimbagraph, 2026.07.04
// A whistlegraph score for marimbaba (pop/marimba/marimbaba.np).
// The page starts blank, like every whistlegraph. Tap to begin: each
// note of the lullaby pulls one gesture. The pen obeys the note's
// amplitude envelope — it darts at the attack (fat, pressed hard) and
// crawls through the decay (thin, lifting) — so the segmentation of
// the melody is legible in the speed and pressure of every mark.
// The active gesture is wet dark ink; a beat after its note ends it
// bakes into the substrate in its section color.
//
// The finished image is one connected scene: a nightcapped crescent
// tucked into a zigzag blanket, exhaling a breeze that carries three
// z's up to a pentagram star, with a low ground line for the final
// "shhh" (F3).
//
// The score data (geometry, timing, envelopes) is exported as SCORE so
// pop/marimba/bin/render-whistlegraph.mjs renders the identical
// drawing to PNG/MP4 offline.

const BPM = 56; // marimbaba.np: slow 3/4, ~56 BPM
const BEAT = 60 / BPM;

const PAPER = [250, 247, 238];
const BLUE = [34, 48, 124]; // moon, face, blanket, ground
const RED = [206, 52, 74]; // star, sparkles, z's
const TEAL = [52, 116, 122]; // the breeze
const ROSE = [232, 142, 152]; // cheeks
const GRAPHITE = [120, 116, 104];
const WET = [30, 26, 38]; // active gesture, before it bakes
const BAKE_SEC = 0.9; // wet → section color, after the note ends

// ── note → Hz ─────────────────────────────────────────────────────────
const SEMI = { C: 0, Db: 1, D: 2, Eb: 3, E: 4, F: 5, Gb: 6, G: 7, Ab: 8, A: 9, Bb: 10, B: 11 };
function hz(note) {
  const m = note.match(/^([A-G]b?)(\d)$/);
  return 440 * 2 ** ((SEMI[m[1]] - 9 + (parseInt(m[2]) - 4) * 12) / 12);
}

// ── geometry on a 100 × 100 unit page ─────────────────────────────────
const RAD = Math.PI / 180;

function arc(cx, cy, r, a0, a1, n = 18) {
  const pts = [];
  for (let i = 0; i <= n; i += 1) {
    const a = (a0 + ((a1 - a0) * i) / n) * RAD;
    pts.push([cx + r * Math.cos(a), cy + r * Math.sin(a)]);
  }
  return pts;
}

function spiral(cx, cy, a0, a1, r0, r1, n = 40) {
  const pts = [];
  for (let i = 0; i <= n; i += 1) {
    const t = i / n;
    const a = (a0 + (a1 - a0) * t) * RAD;
    const r = r0 + (r1 - r0) * t;
    pts.push([cx + r * Math.cos(a), cy + r * Math.sin(a)]);
  }
  return pts;
}

// The pentagram star: one classic five-line stroke, each line split at
// its midpoint so ten twinkle notes each pull half a line.
const STAR_C = [72, 22];
const STAR_R = 12;
const starV = [270, 342, 54, 126, 198].map((a) => [
  STAR_C[0] + STAR_R * Math.cos(a * RAD),
  STAR_C[1] + STAR_R * Math.sin(a * RAD),
]);
const starOrder = [0, 2, 4, 1, 3, 0];
const starHalves = [];
for (let i = 0; i < 5; i += 1) {
  const a = starV[starOrder[i]];
  const b = starV[starOrder[i + 1]];
  const m = [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2];
  starHalves.push([a, m], [m, b]);
}

function starRay(deg) {
  return [
    [STAR_C[0] + 15 * Math.cos(deg * RAD), STAR_C[1] + 15 * Math.sin(deg * RAD)],
    [STAR_C[0] + 19 * Math.cos(deg * RAD), STAR_C[1] + 19 * Math.sin(deg * RAD)],
  ];
}

// The breeze: the moon's exhale, rising from its mouth to the star.
const breezePts = [];
for (let i = 0; i <= 60; i += 1) {
  const x = 37 + (49 * i) / 60;
  const lift = ((x - 37) / 49) * 12;
  breezePts.push([x, 50 - lift + 2.2 * Math.sin(((x - 37) / 49) * 3 * 2 * Math.PI)]);
}

// The blanket: eleven zigzag pulls that cross the moon's chin — tucked in.
const zigV = [];
for (let k = 0; k <= 11; k += 1) {
  zigV.push([12 + (76 * k) / 11, k % 2 === 1 ? 59.5 : 67.5]);
}

// ── the score: every note of marimbaba.np becomes one gesture ─────────
const EVENTS = [];
function ev(tones, beats, pts, color) {
  EVENTS.push({ tones, dur: beats, pts, color, start: 0, fired: false });
}

// [hush hush] bars 1–4 — the crescent moon and its nightcap
ev(["C5"], 1, arc(34, 44, 17, 305, 222, 20), BLUE); // hush — over the top
ev(["A4"], 1, arc(34, 44, 17, 222, 139, 20), BLUE); // hush — down the left
ev(["F4"], 1, arc(34, 44, 17, 139, 55, 20), BLUE); // hush — around the bottom
ev(["F4"], 3, arc(64.45, 44, 24.95, 146.05, 213.95, 26), BLUE); // held — the slow inner bite
ev(["A4"], 1.5, [[34, 27], [24, 22.5], [17, 21]], BLUE); // nightcap slouch
ev(["G4"], 1.5, [[17, 21], [19, 27], [21.4, 32.6]], BLUE); // nightcap brim
ev(["F4"], 3, arc(15.5, 19.5, 2.6, 20, 380, 18), BLUE); // held — pom-pom loop

// [twin-kle] bars 5–10 — the pentagram star, one twinkle per half-line
const twinkleTones = ["F5", "A5", "C6", "A5", "G5", "F5", "G5", "A5", "G5", "F5"];
const twinkleBeats = [1, 1, 0.5, 0.5, 3, 1, 1, 0.5, 0.5, 3];
for (let i = 0; i < 10; i += 1) ev([twinkleTones[i]], twinkleBeats[i], starHalves[i], RED);
ev(["Bb5"], 1, starRay(230), RED); // way —
ev(["D6"], 1, starRay(270), RED); // up — (highest note, highest mark)
ev(["Bb5"], 1, starRay(310), RED); // high
ev(["C6"], 1.5, [[46.6, 12.4], [49.4, 9.6]], RED); // up — rising sparkle tick
ev(["A5"], 1.5, [[88.8, 30.2], [91.2, 27.8]], RED); // there — another

// [wow wow wow] bars 11–14 — the exhale that connects moon to star
ev(["G5", "Bb5"], 6, breezePts, TEAL); // held third — one long rising breath
ev(["A5"], 1, arc(52, 48.5, 3.5, 180, 360, 14), TEAL); // wow
ev(["G5"], 1, arc(63, 45.8, 3.5, 180, 0, 14), TEAL); // wow
ev(["A5"], 1, arc(74, 43.1, 3.5, 180, 360, 14), TEAL); // wow
ev(["F5"], 3, spiral(84.5, 37.5, 200, 740, 3.5, 0.8, 40), TEAL); // held — curls beneath the star

// [ba-ba-ba bap] bars 15–18 — the blanket, tucked across the moon
const babaTones = ["A5", "G5", "A5", "F5", "C6", "Bb5", "C6", "A5", "G5", "F5", "E5"];
const babaBeats = [0.5, 0.5, 1, 1, 0.5, 0.5, 1, 1, 1, 1, 1];
for (let i = 0; i < 11; i += 1) ev([babaTones[i]], babaBeats[i], [zigV[i], zigV[i + 1]], BLUE);
ev(["F5"], 3, [[14, 73.8], [32, 75.1], [50, 75.5], [68, 75.1], [86, 73.8]], BLUE); // held — the hem

// [sleep now] bars 19–24 — the moon's face, its dream, and the ground
ev(["C5"], 1.5, arc(29, 40.5, 3.4, 25, 155, 12), BLUE); // sleep — closed eye
ev(["A4"], 1.5, arc(29, 35.5, 4.2, 205, 335, 12), BLUE); // now — soft brow
ev(["G4"], 1, arc(32.5, 49.5, 3, 25, 155, 10), BLUE); // lit — smile
ev(["F4"], 1, arc(23.5, 46.5, 1.3, 0, 360, 14), ROSE); // tle — cheek
ev(["F4"], 1, arc(37, 51, 1.3, 0, 360, 14), ROSE); // one — cheek
// One Z, three notes: every direction change lands on a note change —
// the compositional rule (turns ≥ ~30° only at note breaks).
ev(["A4"], 1, [[50.5, 29.5], [57.5, 29.5]], RED); // dream — Z top bar
ev(["G4"], 1, [[57.5, 29.5], [50.5, 36.5]], RED); // a — Z diagonal
ev(["F4"], 1, [[50.5, 36.5], [57.5, 36.5]], RED); // while — Z bottom bar
ev(["F4"], 3, arc(33, 52.8, 2.4, 25, 155, 10), BLUE); // held — sleepy chin
ev(["F3"], 3, [[18, 84.4], [34, 85.2], [50, 85.5], [66, 85.2], [82, 84.4]], BLUE); // shhh — the ground
ev([], 3, null, BLUE); // silence + tail — pen lifted, page complete

// ── timing, arc lengths, and the gesture envelope per event ───────────
// The gesture obeys the note's amplitude envelope: pen speed is the
// envelope integral (dart at the attack, crawl through the decay) and
// pressure is the envelope value (fat → thin along each mark).
// Each mark finishes at DRAW_K of its note; the remaining ring-out is
// when the pen lifts and travels to the next mark.
const ENV_N = 36;
const DRAW_K = 0.85;

function envAt(e, x) {
  const dur = e.dur * BEAT;
  const a = Math.min(0.09, dur * 0.18);
  return x < a ? x / a : Math.exp((-1.9 / dur) * (x - a));
}

let cursor = 0;
for (const e of EVENTS) {
  e.start = cursor;
  cursor += e.dur;
  if (!e.pts) continue;
  e.lens = [];
  e.total = 0;
  for (let i = 1; i < e.pts.length; i += 1) {
    const dx = e.pts[i][0] - e.pts[i - 1][0];
    const dy = e.pts[i][1] - e.pts[i - 1][1];
    const l = Math.sqrt(dx * dx + dy * dy);
    e.lens.push(l);
    e.total += l;
  }
  // gest: time-uniform samples of {arc position, pressure 0..1} across
  // the draw window (DRAW_K of the note; the rest is the pen lift).
  const drawDur = e.dur * BEAT * DRAW_K;
  const cum = [0];
  for (let i = 1; i <= ENV_N; i += 1) {
    const x0 = ((i - 1) / ENV_N) * drawDur;
    const x1 = (i / ENV_N) * drawDur;
    cum.push(cum[i - 1] + ((envAt(e, x0) + envAt(e, x1)) / 2) * (x1 - x0));
  }
  const C = cum[ENV_N];
  e.gest = [];
  for (let i = 0; i <= ENV_N; i += 1) {
    e.gest.push({ l: (cum[i] / C) * e.total, r: envAt(e, (i / ENV_N) * drawDur) });
  }
}
const TOTAL_BEATS = cursor; // 72 beats ≈ 77 s

// Pen lifts: between disconnected marks the pen rises and glides along
// a little quadratic arc, leaving the finished mark as its note rings
// out and landing exactly on the next attack.
function liftPath(t0, t1, a, b) {
  const up = Math.min(7, Math.hypot(b[0] - a[0], b[1] - a[1]) * 0.3 + 2);
  return { t0, t1, a, b, c: [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2 - up] };
}

const LIFTS = [];
{
  let prev = null; // { pt, t } — end of the previous mark
  for (const e of EVENTS) {
    if (!e.pts) continue;
    const t = e.start * BEAT;
    if (!prev) {
      LIFTS.push(liftPath(-0.5, 0, [50, -6], e.pts[0])); // entry drop-in
    } else if (Math.hypot(e.pts[0][0] - prev.pt[0], e.pts[0][1] - prev.pt[1]) > 0.6) {
      LIFTS.push(liftPath(prev.t, t, prev.pt, e.pts[0]));
    }
    prev = { pt: e.pts[e.pts.length - 1], t: t + e.dur * BEAT * DRAW_K };
  }
  LIFTS.push(liftPath(prev.t, prev.t + 1.5, prev.pt, [108, 68])); // exit flourish
}

function liftAt(t) {
  for (const L of LIFTS) {
    if (t >= L.t0 && t <= L.t1) {
      const u = (t - L.t0) / (L.t1 - L.t0);
      const v = 1 - u;
      return {
        x: v * v * L.a[0] + 2 * v * u * L.c[0] + u * u * L.b[0],
        y: v * v * L.a[1] + 2 * v * u * L.c[1] + u * u * L.b[1],
        u,
      };
    }
  }
  return null;
}

// Arc-length progress of a gesture at `x` seconds into its note.
function progressAt(e, x) {
  const drawDur = e.dur * BEAT * DRAW_K;
  if (x <= 0) return 0;
  if (x >= drawDur) return e.total;
  const i = Math.min(ENV_N - 1, Math.floor((x / drawDur) * ENV_N));
  const t0 = (i / ENV_N) * drawDur;
  const k = (x - t0) / (drawDur / ENV_N);
  return e.gest[i].l + (e.gest[i + 1].l - e.gest[i].l) * k;
}

// Pressure (0..1) at arc position `len` along a gesture.
function pressureAt(e, len) {
  const g = e.gest;
  for (let i = 1; i < g.length; i += 1) {
    if (g[i].l >= len) {
      const span = g[i].l - g[i - 1].l;
      const k = span > 0 ? (len - g[i - 1].l) / span : 1;
      return g[i - 1].r + (g[i].r - g[i - 1].r) * k;
    }
  }
  return g[g.length - 1].r;
}

function mixColor(a, b, k) {
  return [
    Math.round(a[0] + (b[0] - a[0]) * k),
    Math.round(a[1] + (b[1] - a[1]) * k),
    Math.round(a[2] + (b[2] - a[2]) * k),
  ];
}

export const SCORE = {
  bpm: BPM,
  beat: BEAT,
  size: 100,
  paper: PAPER,
  wet: WET,
  bakeSec: BAKE_SEC,
  drawK: DRAW_K,
  totalBeats: TOTAL_BEATS,
  events: EVENTS,
  lifts: LIFTS,
  hz,
  envAt,
  progressAt,
  pressureAt,
  liftAt,
};

// ── piece state ───────────────────────────────────────────────────────
let started = false;
let startAt = null; // sound.time when beat 0 lands
let tNow = -1; // seconds since beat 0
let done = false;

function reset() {
  started = false;
  startAt = null;
  tNow = -1;
  done = false;
  for (const e of EVENTS) e.fired = false;
}

function boot() {
  reset();
}

function act({ event: e }) {
  if (e.is("touch")) {
    if (!started) started = true;
    else if (done) reset();
  }
}

function sim({ sound }) {
  if (!started || !sound) return;
  if (startAt === null) {
    if (typeof sound.time === "number" && sound.time > 0) startAt = sound.time + 0.5;
    else return;
  }
  tNow = sound.time - startAt;
  for (const e of EVENTS) {
    if (e.fired || tNow < e.start * BEAT) continue;
    e.fired = true;
    if (tNow > (e.start + e.dur) * BEAT) continue; // missed while tab was hidden
    for (const tone of e.tones) {
      sound.synth({
        type: "sine",
        tone: hz(tone),
        duration: e.dur * BEAT * 0.92,
        attack: 0.012,
        decay: 0.96,
        volume: e.tones.length > 1 ? 0.22 : 0.3,
      });
    }
  }
  if (tNow > (TOTAL_BEATS + 1) * BEAT) done = true;
}

function paint({ wipe, ink, screen }) {
  const s = Math.min(screen.width, screen.height) / 100;
  const ox = (screen.width - 100 * s) / 2;
  const oy = (screen.height - 100 * s) / 2;
  wipe(...PAPER);

  const wMax = Math.min(4, Math.max(2, Math.round(s * 0.7)));
  let pen = null;

  for (const e of EVENTS) {
    if (!e.pts || !started || startAt === null || tNow < 0) continue;
    const x = tNow - e.start * BEAT;
    if (x <= 0) continue;
    const target = progressAt(e, x);
    if (target <= 0) continue;

    // Wet ink while sounding; bakes into the section color after.
    const bakeK = Math.min(1, Math.max(0, (x - e.dur * BEAT) / BAKE_SEC));
    const col = mixColor(WET, e.color, bakeK);

    let run = 0;
    let tip = null;
    for (let i = 1; i < e.pts.length; i += 1) {
      const l = e.lens[i - 1];
      const a = e.pts[i - 1];
      const b = e.pts[i];
      let bx = b[0];
      let by = b[1];
      let partial = false;
      if (run + l > target) {
        const k = (target - run) / l;
        bx = a[0] + (b[0] - a[0]) * k;
        by = a[1] + (b[1] - a[1]) * k;
        partial = true;
      }
      const w = Math.max(1, Math.round(1 + pressureAt(e, run + l / 2) * (wMax - 1)));
      for (let dx = 0; dx < w; dx += 1) {
        for (let dy = 0; dy < w; dy += 1) {
          ink(...col).line(
            ox + a[0] * s + dx, oy + a[1] * s + dy,
            ox + bx * s + dx, oy + by * s + dy,
          );
        }
      }
      tip = [ox + bx * s, oy + by * s];
      if (partial) break;
      run += l;
    }
    if (target < e.total) pen = tip;
  }

  const lift = started && startAt !== null ? liftAt(tNow) : null;
  if (lift) {
    // Airborne pen: lighter, swelling mid-arc as it nears the viewer.
    const r = wMax + Math.round(Math.sin(Math.PI * lift.u) * wMax);
    ink(...mixColor(GRAPHITE, PAPER, 0.35)).box(
      ox + lift.x * s - r, oy + lift.y * s - r, r * 2 + 1, r * 2 + 1,
    );
  } else if (pen) {
    ink(...GRAPHITE).box(pen[0] - wMax, pen[1] - wMax, wMax * 2 + 1, wMax * 2 + 1);
  }

  if (!started) {
    ink(...GRAPHITE).write("tap to whistle a lullaby", {
      center: "x",
      y: screen.height - 24,
      screen,
    });
  } else if (tNow > (TOTAL_BEATS - 3) * BEAT) {
    ink(...GRAPHITE).write("marimbaba — a whistlegraph score", {
      center: "x",
      y: screen.height - 24,
      screen,
    });
  }
}

function leave() {
  reset();
}

function meta() {
  return {
    title: "marimbagraph",
    desc: "The marimbaba lullaby drawn as a whistlegraph score — each note's attack and decay drives the speed and pressure of one gesture; wet ink bakes into a sleeping moon.",
  };
}

export { boot, paint, sim, act, leave, meta };
