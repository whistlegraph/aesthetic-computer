// torva, 26.07.12
// Delaunay/triangulation-web × ACID 303 — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` seconds-per-beat override
// e.g. `torva 0.25`, the tap/XY "pump", audio polling). This file only describes
// what makes torva torva: its acid score, its squelchy 303 voices, and its
// low-poly neon mesh paint.
//
// ALLEGORY — a shifting triangulated web of nodes. Every audible note PULSES a
// node bright (pitch → node height/hue) and lights its connected edges. The
// acid bassline SLIDES → the whole web warps/breathes. The beat = a wave of
// light travelling out through the mesh. The visual IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A classic acid pattern in A: root-heavy with octave jumps + a couple of
// gliding accents. Each step targets a node (step % NODE_COUNT).
const ACID = [
  "a1", "a2", "a1", "c2", "e2", "a1", "g2", "a1",
  "a1", "a2", "c3", "a2", "e2", "g1", "a1", "b1",
];
// Slide targets: where each step GLIDES toward (null = plain pluck). The 303's
// signature squelch is the portamento between notes.
const SLIDE = [
  null, "e2", null, "e2", null, null, "a2", null,
  null, "c3", null, "e2", null, "a1", null, "e2",
];

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(#?)(\d)$/.exec(n);
  if (!m) return 33;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + (m[2] ? 1 : 0);
}
const ACID_PITCHES = ACID.map(notePitch);
const PITCH_MIN = Math.min(...ACID_PITCHES);
const PITCH_MAX = Math.max(...ACID_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- Mesh --------------------------------------------------------------------
// A fixed set of nodes drifting on gentle sinusoids; each frame we (re)link each
// node to its K nearest neighbours → a shifting low-poly web (no true Delaunay
// needed). Node COUNT scales with ctx.quality to hold 60fps.
const BASE_NODES = 26; // at quality 1
const K = 3; // edges per node (nearest-k)
let nodes = []; // { bx, by, phx, phy, spx, spy, pulse, hue, height }
let webWarp = 0; // acid-bass warp amount (breathing)
let beatWave = 0; // 0..1 radius of the light wave through the mesh (from center)
let beatWaveLife = 0;

function buildNodes(n) {
  nodes = [];
  for (let i = 0; i < n; i++) {
    // Deterministic scatter so the mesh is stable frame-to-frame.
    const r = (i * 2654435761) >>> 0;
    const rx = (r & 0xffff) / 0xffff;
    const ry = ((r >>> 16) & 0xffff) / 0xffff;
    nodes.push({
      bx: 0.08 + rx * 0.84, // normalized home position
      by: 0.08 + ry * 0.84,
      phx: rx * Math.PI * 2,
      phy: ry * Math.PI * 2,
      spx: 0.4 + rx * 0.7, // drift speed
      spy: 0.4 + ry * 0.7,
      pulse: 0, // 0..1 note flash
      hue: 180 + rx * 120,
      height: 0, // pitch → 0..1 (raises node + shifts hue)
    });
  }
}

const CONFIG = {
  bpm: 138, // driving acid tempo
  steps: ACID.length,
  drawBursts: false, // torva draws its own tap stabs
  hooks: {
    onBoot({ sound }) {
      buildNodes(BASE_NODES);
      sound.room?.set?.({ enabled: true, mix: 0.22, feedback: 0.5 });
    },

    // A new UTC beat crossed — fire the acid step + pulse the matching node.
    onBeat({ idx, synth }) {
      const s = ((idx % ACID.length) + ACID.length) % ACID.length;
      const note = ACID[s];
      const slide = SLIDE[s];
      const pn = pitchNorm(note);
      const pan = Math.sin((s / ACID.length) * Math.PI * 2) * 0.5;

      // ACID 303: raw resonant sawtooth. Emulate portamento/squelch by firing a
      // short quick sequence of stacked saws gliding from note → slide target,
      // each with a fast decay (the classic sliding, squelchy bite).
      acid303(synth, note, slide, { pan, accent: pn > 0.5 });

      // ALLEGORY: pulse the node this step targets; pitch → height + hue shift.
      const node = nodes[s % nodes.length];
      if (node) {
        node.pulse = 1;
        node.height = pn;
        node.hue = 300 - pn * 220; // low = magenta/red, high = cyan/green
      }

      // Sub-bass root every other step — the floor you feel + the web warp.
      if (s % 2 === 0) {
        voices.sub(synth, note[0] + "1", { beats: 1.0, decay: 0.4, volume: 0.42 });
        webWarp = 1; // acid bass → warp the whole web
      }

      // Ticking hat drives the acid pulse.
      voices.hat(synth, { tone: 9000, beats: 0.08, volume: 0.13 });

      // Every step launches a wave of light from the mesh center.
      beatWave = 0;
      beatWaveLife = 1;
    },

    onSim() {
      webWarp *= 0.9;
      for (const nd of nodes) {
        nd.pulse *= 0.88;
        nd.height *= 0.94;
      }
      if (beatWaveLife > 0) {
        beatWave += 0.06;
        beatWaveLife -= 0.04;
      }
    },

    // Tap = pulse the NEAREST node + a squelchy 303 stab at the tap (X→pitch,
    // Y→brightness/slide). Engine already bumped pump.
    onTap({ x, y, ex, ey, synth, screen }) {
      const w = screen?.width || 1;
      const h = screen?.height || 1;
      // Find nearest node to the tap.
      let best = null;
      let bd = Infinity;
      for (const nd of nodes) {
        const nx = nd.bx * w;
        const ny = nd.by * h;
        const d = (nx - ex) ** 2 + (ny - ey) ** 2;
        if (d < bd) { bd = d; best = nd; }
      }
      if (best) {
        best.pulse = 1.3;
        best.height = 1 - y;
        best.hue = x * 300;
      }
      webWarp = Math.min(1.5, webWarp + 0.8);
      beatWave = 0;
      beatWaveLife = 1;

      // Squelchy stab: X picks the root, gliding up when tapped high.
      const roots = ["a1", "c2", "d2", "e2", "g2"];
      const note = roots[Math.floor(x * 5) % 5];
      const target = y < 0.5 ? note[0] + String(parseInt(note.slice(-1)) + 1) : null;
      acid303(synth, note, target, { pan: x * 2 - 1, accent: true, vol: 0.6 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const energy = Math.min(2, webWarp * 0.7 + bass * 1.2 + amp * 0.6 + pump * 0.5);

      // Scale node count by quality to hold 60fps (rebuild only when it changes).
      const want = Math.max(8, Math.round(BASE_NODES * quality));
      if (nodes.length !== want) {
        const old = nodes;
        buildNodes(want);
        // preserve pulse/height across resize where indices overlap
        for (let i = 0; i < Math.min(old.length, nodes.length); i++) {
          nodes[i].pulse = old[i].pulse;
          nodes[i].height = old[i].height;
          nodes[i].hue = old[i].hue;
        }
      }

      // Neon dark base with a faint trail (feedback breath).
      ink(4, 3, 12, 200).box(0, 0, w, h);

      const t = (s.simMs || 0) / 1000;
      const warp = webWarp; // 0..~1.5

      // Resolve live node screen positions (drift + acid-bass warp toward center).
      const pos = new Array(nodes.length);
      for (let i = 0; i < nodes.length; i++) {
        const nd = nodes[i];
        let nx = nd.bx + Math.sin(t * nd.spx + nd.phx) * 0.05;
        let ny = nd.by + Math.cos(t * nd.spy + nd.phy) * 0.05;
        // Acid warp: the whole web breathes in/out around center.
        const breathe = 1 + warp * 0.12 * Math.sin(t * 2 + nd.phx);
        nx = 0.5 + (nx - 0.5) * breathe;
        ny = 0.5 + (ny - 0.5) * breathe;
        // Pitch height lifts a pulsed node upward.
        const lift = nd.height * 0.06;
        pos[i] = [nx * w, (ny - lift) * h, i];
      }

      // Nearest-K edges (bounded: N*K lines). Compute per node.
      const maxDim = Math.max(w, h);
      for (let i = 0; i < pos.length; i++) {
        const [ax, ay] = pos[i];
        // find K nearest
        const dists = [];
        for (let j = 0; j < pos.length; j++) {
          if (j === i) continue;
          const dx = pos[j][0] - ax, dy = pos[j][1] - ay;
          dists.push([dx * dx + dy * dy, j]);
        }
        dists.sort((p, q) => p[0] - q[0]);
        const kk = Math.min(K, dists.length);
        for (let e = 0; e < kk; e++) {
          const j = dists[e][1];
          if (j < i) continue; // draw each undirected edge once
          const [bx, by] = pos[j];
          const na = nodes[i], nb = nodes[j];
          const glow = Math.max(na.pulse, nb.pulse); // lit if either endpoint fired
          // beat wave: edges near the wave radius flash white.
          const mx = (ax + bx) / 2, my = (ay + by) / 2;
          const md = Math.hypot(mx - cx, my - cy) / (maxDim * 0.6);
          const waveHit = beatWaveLife > 0 ? Math.max(0, 1 - Math.abs(md - beatWave) * 6) * beatWaveLife : 0;
          const lit = Math.min(1, glow + waveHit);
          const baseA = 26 + lit * 200;
          const hue = ((na.hue + nb.hue) / 2 + energy * 20) % 360;
          const [r, g, b] = num.hslToRgb(((hue % 360) + 360) % 360, 90, 40 + lit * 35);
          const wgt = 1 + lit * 2;
          ink(r, g, b, baseA).line(ax, ay, bx, by, wgt);
          if (waveHit > 0.3)
            ink(255, 255, 255, 180 * waveHit).line(ax, ay, bx, by, 1);
        }
      }

      // Nodes on top: bright pulsing dots; height/hue from pitch.
      for (let i = 0; i < pos.length; i++) {
        const [x, y] = pos[i];
        const nd = nodes[i];
        const p = nd.pulse;
        const [r, g, b] = num.hslToRgb(((nd.hue % 360) + 360) % 360, 100, 55 + p * 25);
        const rad = 2 + p * 8 + nd.height * 4;
        if (p > 0.05) {
          ink(r, g, b, 60 + 90 * p).circle(x, y, rad * 2.2, true); // halo
        }
        ink(r, g, b, 200).circle(x, y, rad, true);
        if (p > 0.2) ink(255, 255, 255, 200 * p).circle(x, y, rad * 0.5, true);
      }

      // TAP STABS: engine-tracked pump gives a central flash; also draw a soft
      // core so the mesh has a beating heart.
      const heartR = 3 + warp * 8 + bass * 12 + pump * 8;
      ink(120, 255, 220, 60 + warp * 80).circle(cx, cy, heartR * 1.8, true);
      ink(255, 255, 255, 120 + warp * 60).circle(cx, cy, heartR, true);

      if (warp > 0.6 || pump > 1.3) blur?.(1);
    },
  },
};

// --- Acid 303 voice ----------------------------------------------------------
// Raw resonant sawtooth with an emulated slide + squelch. We stack a couple of
// detuned saws and, when a slide target exists, fire a fast micro-sequence of
// short saw grains stepping from the source pitch to the target — the ear reads
// that rapid step-glide as portamento. Short decay + a bright detuned partial
// gives the resonant "squelch". No gm.
function acid303(synth, note, slide, o = {}) {
  const pan = o.pan ?? 0;
  const vol = o.vol ?? (o.accent ? 0.5 : 0.4);
  const p0 = notePitch(note);
  if (slide) {
    const p1 = notePitch(slide);
    const STEPS = 5;
    for (let i = 0; i < STEPS; i++) {
      const semi = p0 + ((p1 - p0) * i) / (STEPS - 1);
      const freq = 440 * Math.pow(2, (semi - 57) / 12); // A3=57 → 440Hz
      synth({
        type: "sawtooth",
        tone: freq,
        beats: 0.16,
        attack: 0.001,
        decay: 0.5,
        volume: (vol * (0.5 + (i / STEPS) * 0.5)) / 1.4,
        pan,
      });
      // Bright resonant partial (the squelchy "wah"), a 5th up, quick.
      synth({
        type: "sawtooth",
        tone: freq * 1.5,
        beats: 0.1,
        attack: 0.001,
        decay: 0.7,
        volume: vol * 0.14,
        pan,
      });
    }
  } else {
    // Plain accented saw hit + detuned twin + resonant partial.
    synth({ type: "sawtooth", tone: note, beats: 0.28, attack: 0.001, decay: 0.4, volume: vol, pan });
    const f = 440 * Math.pow(2, (p0 - 57) / 12);
    synth({ type: "sawtooth", tone: f * 1.008, beats: 0.24, attack: 0.001, decay: 0.4, volume: vol * 0.5, pan });
    synth({ type: "sawtooth", tone: f * 2.02, beats: 0.14, attack: 0.001, decay: 0.6, volume: vol * (o.accent ? 0.22 : 0.12), pan });
  }
}

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert this pad's config before the engine's
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
