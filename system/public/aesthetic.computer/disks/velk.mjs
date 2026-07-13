// velk, 26.07.12
// Neon-wireframe synthwave pad — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `velk 0.5`,
// the tap/XY "pump", audio polling, adaptive quality). This file only describes
// what makes velk velk: its synthwave score, its detuned-saw voices, and its
// rotating wireframe SOLID over a retro horizon grid.
//
// ALLEGORY — the visual IS the score:
//   • A glowing wireframe polyhedron (icosahedron) rotates in 3D; verts are
//     projected to 2D (perspective, pure vector = cheap).
//   • Each ARP note LIGHTS AN EDGE (pitch → which edge + hue) so the melody
//     literally traces around the solid.
//   • The BASS scale-PULSES the whole solid — it breathes on the root.
//   • The BEAT flashes a FACE (a bright vertex-cluster bloom).
//   • Retro-synthwave neon-on-grid look: a horizon grid + sun for vibe.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (synthwave, F# minor-ish neon) -----------------------------------
const ARP = [
  "f#3", "a3", "c#4", "f#4", "c#4", "a3", "e4", "c#4",
  "d3", "f#3", "a3", "d4", "a3", "f#3", "c#4", "a3",
];
const BASS = ["f#1", "f#1", "d1", "d1", "e1", "e1", "c#1", "c#1"]; // half-time
const PAD = [
  ["f#2", "a2", "c#3"], ["d2", "f#2", "a2"],
  ["e2", "g#2", "b2"], ["c#2", "e2", "g#2"],
];

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(#?)(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + (m[2] ? 1 : 0);
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- Polyhedron: icosahedron (12 verts, 30 edges) ---------------------------
// Golden-ratio icosahedron — cheap, symmetric, reads clearly when rotating.
const PHI = (1 + Math.sqrt(5)) / 2;
const RAW_VERTS = [
  [-1, PHI, 0], [1, PHI, 0], [-1, -PHI, 0], [1, -PHI, 0],
  [0, -1, PHI], [0, 1, PHI], [0, -1, -PHI], [0, 1, -PHI],
  [PHI, 0, -1], [PHI, 0, 1], [-PHI, 0, -1], [-PHI, 0, 1],
];
const VNORM = Math.hypot(1, PHI); // normalize to unit-ish radius
const VERTS = RAW_VERTS.map(([x, y, z]) => [x / VNORM, y / VNORM, z / VNORM]);
const EDGES = [
  [0, 1], [0, 5], [0, 7], [0, 10], [0, 11],
  [1, 5], [1, 7], [1, 8], [1, 9],
  [2, 3], [2, 4], [2, 6], [2, 10], [2, 11],
  [3, 4], [3, 6], [3, 8], [3, 9],
  [4, 5], [4, 9], [4, 11],
  [5, 9], [5, 11],
  [6, 7], [6, 8], [6, 10],
  [7, 8], [7, 10],
  [8, 9], [10, 11],
];

// --- velk-specific visual state (engine owns pump/bursts/rhythm) ------------
let rotX = 0.4, rotY = 0.0, rotZ = 0.0; // Euler angles
let spinX = 0.0, spinY = 0.0, spinZ = 0.0; // angular velocity (tap kicks)
let breathe = 0; // bass scale-pulse, 0..1 (decays)
let faceFlash = 0; // beat face flash, 0..1 (decays)
let litEdges = []; // { edge:[a,b], hue, life } — melody tracing the solid
let padVoices = [];

const CONFIG = {
  bpm: 118,
  steps: ARP.length,
  drawBursts: false, // velk draws its own neon tap rings
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 });
    },

    // A new UTC beat crossed — fire the synthwave score + light the solid.
    onBeat({ idx, synth }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.6;

      // RICH synthwave arp: two RAW detuned sawtooth voices (80s supersaw feel).
      synth({ tone: note, type: "sawtooth", beats: 0.85, attack: 0.006, decay: 0.6, volume: 0.24, pan });
      synth({ tone: note, type: "sawtooth", beats: 0.85, attack: 0.006, decay: 0.55, volume: 0.2, pan: pan * -0.6 + 0.06 });

      // ALLEGORY: pitch → which EDGE lights (melody traces around the solid).
      const ei = Math.floor(pn * (EDGES.length - 1));
      litEdges.push({ edge: EDGES[ei], hue: 180 + pn * 140, life: 1 });

      // Sub-bass root on the half-beat — you feel it AND the solid breathes.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.5 });
        breathe = 1;
      }

      // Lush held padChord (retune each bar) — the neon wash behind the solid.
      const chord = PAD[Math.floor(s / 4) % PAD.length];
      if (padVoices.length === 0) {
        padVoices = voices.padChord(synth, chord, { type: "sawtooth", attack: 0.5, decay: 0.9, volume: 0.06, spread: 0.4 });
      } else {
        for (let i = 0; i < padVoices.length; i++) padVoices[i]?.update?.({ tone: chord[i % chord.length] });
      }

      // Beat = hat tick + FACE FLASH.
      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.14 });
      faceFlash = 1;
    },

    onSim() {
      // Base rotation + tap-kicked spin (damped).
      rotX += 0.006 + spinX;
      rotY += 0.011 + spinY;
      rotZ += 0.003 + spinZ;
      spinX *= 0.92; spinY *= 0.92; spinZ *= 0.92;
      breathe *= 0.9;
      faceFlash *= 0.88;
      for (const e of litEdges) e.life -= 0.02;
      litEdges = litEdges.filter((e) => e.life > 0);
      if (litEdges.length > 20) litEdges = litEdges.slice(-20);
    },

    // Tap = spin/kick the solid + a saw stab; light the nearest edge to the tap.
    // X → pan/hue, Y → pitch (top = high).
    onTap({ x, y, synth }) {
      spinX += (y - 0.5) * 0.08;
      spinY += (x - 0.5) * 0.12;
      spinZ += (x - 0.5) * 0.04;
      breathe = Math.min(1, breathe + 0.6);

      const note = ["f#", "a", "c#", "e", "g#"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
      synth({ tone: note, type: "sawtooth", beats: 0.5, attack: 0.004, decay: 0.5, volume: 0.4, pan: x * 2 - 1 });
      synth({ tone: note, type: "sawtooth", beats: 0.5, attack: 0.004, decay: 0.45, volume: 0.32, pan: (x * 2 - 1) * -0.7 });

      // Light the edge whose projected midpoint is nearest the tap (approx via
      // current rotation → pick by a hue-mapped index around Y for cheapness).
      const ei = Math.floor(((x + (1 - y)) * 0.5) * (EDGES.length - 1));
      litEdges.push({ edge: EDGES[ei], hue: x * 360, life: 1.3 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const energy = Math.min(2, breathe * 0.7 + bass * 1.2 + amp * 0.6 + pump * 0.5);

      // --- Backdrop: synthwave sky wash (dark magenta→indigo) ------------------
      ink(10, 6, 24, 255).box(0, 0, w, h);
      // Neon sun glow low on the horizon.
      const horizon = h * 0.58;
      const sunR = Math.min(w, h) * (0.16 + energy * 0.03);
      for (let i = 4; i > 0; i--) {
        const [r0, g0, b0] = num.hslToRgb((330 + i * 6) % 360, 90, 55);
        ink(r0, g0, b0, 22).circle(cx, horizon, sunR * (i / 4), true);
      }

      // --- Retro horizon GRID (line count scales by quality → holds 60fps) -----
      const rows = Math.max(4, Math.round(10 * quality));
      const cols = Math.max(6, Math.round(16 * quality));
      const gridScroll = (s.step + s.beatProgress) * 0.06;
      ink(255, 40, 200, 90);
      // Receding horizontal rows (perspective: rows bunch near horizon).
      for (let ri = 1; ri <= rows; ri++) {
        const t = ((ri / rows) + gridScroll) % 1;
        const y = horizon + (h - horizon) * (t * t); // perspective squeeze
        ink(140, 30, 220, 60 + 100 * t).line(0, y, w, y, 1);
      }
      // Vanishing vertical lines converging at the horizon center.
      for (let ci = 0; ci <= cols; ci++) {
        const fx = (ci / cols) * 2 - 1; // -1..1
        const bx = cx + fx * w * 0.9;
        ink(80, 200, 255, 70).line(cx, horizon, bx, h, 1);
      }

      // --- The wireframe SOLID (project 3D verts to 2D, perspective) ----------
      const scale = Math.min(w, h) * (0.24 + breathe * 0.06 + energy * 0.02);
      const camZ = 3.2;
      const cxr = Math.cos(rotX), sxr = Math.sin(rotX);
      const cyr = Math.cos(rotY), syr = Math.sin(rotY);
      const czr = Math.cos(rotZ), szr = Math.sin(rotZ);
      const P = new Array(VERTS.length);
      for (let i = 0; i < VERTS.length; i++) {
        let [x, y, z] = VERTS[i];
        // Rotate X
        let y1 = y * cxr - z * sxr, z1 = y * sxr + z * cxr; y = y1; z = z1;
        // Rotate Y
        let x1 = x * cyr + z * syr; z1 = -x * syr + z * cyr; x = x1; z = z1;
        // Rotate Z
        let x2 = x * czr - y * szr, y2 = x * szr + y * czr; x = x2; y = y2;
        const persp = camZ / (camZ + z); // perspective divide
        P[i] = [cx + x * scale * persp, cy - h * 0.06 + y * scale * persp, z, persp];
      }

      // Lit-edge lookup (edge index → {hue,life}) for the melody trace.
      const lit = new Map();
      for (const e of litEdges) {
        const key = e.edge[0] * 100 + e.edge[1];
        const prev = lit.get(key);
        if (!prev || e.life > prev.life) lit.set(key, e);
      }

      // Draw all edges (dim neon base), lit ones brighter (melody trace).
      for (const [a, b] of EDGES) {
        const pa = P[a], pb = P[b];
        const key = a * 100 + b;
        const hit = lit.get(key);
        const depth = (pa[3] + pb[3]) * 0.5; // ~perspective → depth cue
        if (hit) {
          const [r0, g0, b0] = num.hslToRgb(((hit.hue % 360) + 360) % 360, 100, 55 + hit.life * 20);
          ink(r0, g0, b0, 120 + 135 * hit.life).line(pa[0], pa[1], pb[0], pb[1], 2 + hit.life * 2);
          // white core glint
          ink(255, 255, 255, 160 * hit.life).line(pa[0], pa[1], pb[0], pb[1], 1);
        } else {
          const a0 = 60 + depth * 90;
          ink(60, 220, 255, a0).line(pa[0], pa[1], pb[0], pb[1], 1);
        }
      }

      // Vertices as neon dots; BEAT FACE FLASH blooms them.
      for (let i = 0; i < P.length; i++) {
        const p = P[i];
        const dotR = 1.5 + p[3] * 2 + faceFlash * 4;
        const [r0, g0, b0] = num.hslToRgb((190 + i * 12) % 360, 90, 60);
        ink(r0, g0, b0, 120 + 120 * p[3]).circle(p[0], p[1], dotR, true);
      }
      if (faceFlash > 0.15) {
        // Bright central bloom = a face lit up on the beat.
        ink(255, 255, 255, 40 * faceFlash).circle(cx, cy - h * 0.06, scale * 0.5 * faceFlash, true);
      }

      // BASS breathe halo around the solid.
      if (breathe > 0.05 || bass > 0.05) {
        const bl = Math.max(breathe, bass);
        ink(255, 60, 200, 18 * bl).circle(cx, cy - h * 0.06, scale * (1.1 + bl * 0.4), true);
      }

      // --- TAP BURSTS (engine-tracked): expanding neon rings ------------------
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 60);
        ink(r0, g0, b0, 200 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
        ink(255, 255, 255, 130 * b.life).circle(b.x, b.y, b.r * 0.5, false, 1);
      }
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
