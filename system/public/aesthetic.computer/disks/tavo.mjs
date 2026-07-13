// tavo, 26.07.12
// Rising-bubbles bath — a thin wrapper over lib/pads.mjs (the shared pad engine:
// UTC-clock beat grid, `params[0]` rate override e.g. `tavo 0.5`, the tap/XY
// "pump", audio polling). This file only describes what makes tavo tavo: its
// watery score, its bubble-synth voices, and its translucent rising-bubble paint.
//
// ALLEGORY: every audible note BIRTHS a bubble at the water's floor. Its SIZE ∝
// pitch (small = high, big = low) and its HUE ∝ pitch too, so you SEE the melody
// as a shoal of buoyant translucent soap bubbles rising through the tank. Bubbles
// WOBBLE (surface-tension jiggle), MERGE when they touch, and POP at the surface —
// each pop plinks a bell. The BASS = the upward CURRENT strength: a fat low note
// speeds every bubble's rise and shoves a shimmer up the tank. AC's physical
// `type:"bubble"` synth is the sound (its frequency literally sets bubble size —
// a perfect match). The whole screen is a button: tap blows a big bubble from the
// tap point (X→hue, Y→size/pitch) with its own bubble tone.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A slow watery loop in A minor. Each beat births a bubble whose size/hue track
// this note's pitch. Low notes = big slow bubbles + a current surge; highs = tiny
// quick ones that pop fast near the surface.
const MELODY = [
  "a3", "e4", "c4", "a4", // rising shoal
  "e3", "b3", "e4", "g4",
  "f3", "c4", "a3", "f4",
  "c3", "g3", "d4", "e4",
];
const CURRENT = ["a1", "a1", "e2", "e2", "f1", "f1", "c2", "c2"]; // half-time bass current
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const PITCHES = MELODY.map(notePitch);
const PMIN = Math.min(...PITCHES);
const PMAX = Math.max(...PITCHES);
const pitchNorm = (n) => (notePitch(n) - PMIN) / Math.max(1, PMAX - PMIN); // 0 low .. 1 high

// map a pitch-note to the physical bubble synth's radius (mm-ish). Big radius =
// low tone; small radius = high tone. Matches the visual size ∝ (1 - pitch).
function bubbleTone(note) {
  const pn = pitchNorm(note); // 0 low .. 1 high
  return { pn, radius: 8 + (1 - pn) * 46 }; // 8 (tiny/high) .. 54 (fat/low)
}

// --- tavo-specific visual state (the engine owns pump/bursts/rhythm) ---------
let bubbles = []; // live rising bubbles: { x, y, r, vy, hue, wob, phase, life, popping, pop }
let pops = []; // transient pop flashes: { x, y, r, life, hue }
let current = 0; // eased bass level → upward-current strength
let surface = 0; // shimmer at the water surface, kicked by pops
let nextId = 1;

const MAX_BUBBLES = 40; // hard ceiling; quality trims this further

const CONFIG = {
  bpm: 96,
  steps: MELODY.length,
  drawBursts: false, // tavo blows its own bubbles from taps in onPaint/onTap
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 }); // watery tank verb
      bubbles = [];
      pops = [];
      current = 0;
      surface = 0;
      nextId = 1;
    },

    // A new UTC beat crossed — fire the score + birth the matching bubble.
    onBeat({ idx, synth, sound, pump }) {
      const s = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[s];
      const { pn, radius } = bubbleTone(note);
      const boost = 1 + Math.min(1, pump * 0.3);
      const pan = Math.sin((s / MELODY.length) * Math.PI * 2) * 0.7;

      // BUBBLE-SYNTH (physical model): radius sets the pitch — small=high, big=low.
      // This is the watery "bloop" you hear; its size ∝ the note's pitch, so the
      // sound literally mirrors the bubble you see rise.
      sound?.bubble?.({ radius, rise: 0.1 + pn * 0.5, volume: 0.9 * boost, pan });
      // A soft pitched bell doubles the note so the melody reads clearly over the
      // watery bloops (bloops alone are pitch-blurry).
      voices.bell(synth, note, { beats: 0.6, volume: 0.14 * boost, pan });

      // ALLEGORY: birth a rising bubble; size ∝ (1 - pitch), hue ∝ pitch.
      birthBubble(0.16 + Math.random() * 0.68, pn, radius, pan);

      // BASS CURRENT on the half-beat — the upward push you feel + SEE.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % CURRENT.length) + CURRENT.length) % CURRENT.length;
        voices.sub(synth, CURRENT[bi], { beats: 1.8, decay: 0.5, volume: 0.5 * boost });
        current = Math.min(1.6, current + 0.7);
      }

      // Airy waterline tick.
      voices.hat(synth, { tone: 6500, beats: 0.1, volume: 0.1 * boost });

      lastSynth = synth; // so sim-time pops can plink even with no taps
    },

    onSim({ pump, band }) {
      // Bass current eases toward the live low end; decays otherwise.
      current *= 0.96;
      if (!Number.isFinite(current)) current = 0;
      current = Math.max(current, band("subBass") * 1.2);
      surface *= 0.9;
      if (surface < 0.001) surface = 0;

      const rise = 0.0022 + current * 0.006 + pump * 0.0018; // px-frac per frame

      for (let i = bubbles.length - 1; i >= 0; i--) {
        const b = bubbles[i];
        if (!Number.isFinite(b.y)) { bubbles.splice(i, 1); continue; }
        // Rise: smaller (higher-pitched) bubbles rise faster (buoyancy).
        b.y -= rise * (0.7 + (1 - b.rNorm) * 0.9);
        b.phase += 0.12 + b.wob * 0.06; // surface-tension jiggle
        b.x += Math.sin(b.phase) * 0.0016 * (0.5 + b.wob);
        b.life = Math.min(1, b.life + 0.06); // fade-in
        // POP at the surface (top ~8% of tank).
        if (b.y <= 0.08 && !b.popping) popBubble(b);
      }

      // MERGE: nearby bubbles glom into the bigger one (surface tension).
      for (let i = 0; i < bubbles.length; i++) {
        const a = bubbles[i];
        if (a.popping) continue;
        for (let j = i + 1; j < bubbles.length; j++) {
          const c = bubbles[j];
          if (c.popping) continue;
          const dx = (a.x - c.x), dy = (a.y - c.y) * 1.4;
          const d = Math.hypot(dx, dy);
          if (d < (a.r + c.r) * 0.42) {
            // conserve area: merged radius = sqrt(r1^2 + r2^2)
            const big = a.r >= c.r ? a : c;
            const small = a.r >= c.r ? c : a;
            big.r = Math.min(0.24, Math.sqrt(big.r * big.r + small.r * small.r * 0.8));
            big.rNorm = Math.min(big.rNorm, small.rNorm);
            big.wob = Math.min(1.4, big.wob + 0.25); // jiggle from the merge
            small.popping = true; small.pop = 0.6; small.merged = true;
          }
        }
      }

      // Expire popping bubbles + advance pop flashes.
      for (let i = bubbles.length - 1; i >= 0; i--) {
        const b = bubbles[i];
        if (b.popping) { b.pop -= 0.12; if (b.pop <= 0) bubbles.splice(i, 1); }
      }
      for (let i = pops.length - 1; i >= 0; i--) {
        const p = pops[i];
        p.r += 0.01; p.life -= 0.08;
        if (p.life <= 0) pops.splice(i, 1);
      }
    },

    // POP fires a plink (bell/pluck) — a note release. Called from popBubble via
    // the synth handle stashed on beat; but pops happen in sim, so we plink from
    // the audio-bearing hooks instead (see popBubble using deferred flag).

    // Tap = blow a big bubble from the tap point + a bubble tone.
    // X→hue, Y→size/pitch (top = smaller/higher, bottom = bigger/lower).
    onTap({ x, y, synth, sound, isDraw, pump }) {
      const pn = Math.max(0, Math.min(1, 1 - y)); // top = high pitch
      const scale = ["a", "c", "d", "e", "g"];
      const deg = Math.floor(pn * (scale.length - 1));
      const oct = 2 + Math.floor(pn * 3);
      const note = scale[Math.max(0, Math.min(scale.length - 1, deg))] + oct;
      // Big bubble from a tap → bigger radius than the melody floor so it reads as
      // a fat blown bubble; Y sets pitch/size (top = small/high, bottom = big/low).
      const radius = 12 + (1 - pn) * 52;
      const boost = 1 + Math.min(1, pump * 0.3);
      const pan = x * 2 - 1;

      sound?.bubble?.({ radius, rise: 0.05 + pn * 0.5, volume: (isDraw ? 0.7 : 1.0) * boost, pan });
      voices.bell(synth, note, { beats: 0.4, volume: 0.16 * boost, pan });

      // Blow a fresh bubble right at the tap; hue from X, size from Y.
      const b = birthBubble(x, pn, radius, pan, isDraw ? 0.6 : 1);
      if (b) { b.x = x; b.y = Math.min(0.95, y); b.hue = x * 360; b.r = Math.min(0.2, b.r * 1.3); }
      current = Math.min(1.6, current + (1 - y) * 0.25); // taps stir the current

      // Stash the synth so sim-time pops can plink (see onPaint tick).
      lastSynth = synth;
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, quality, amp } = s;
      const { width: w, height: h } = screen;
      const q = quality ?? 1;

      // Water tank background: a deep blue→teal vertical gradient (cheap: a few bands).
      const bands = q >= 0.85 ? 8 : 5;
      for (let i = 0; i < bands; i++) {
        const t = i / bands;
        const [r, g, bl] = num.hslToRgb(200 - t * 30, 70, 8 + t * 14);
        ink(r, g, bl, 255).box(0, (t * h) | 0, w, Math.ceil(h / bands));
      }

      // CURRENT: faint upward streaks (rising shimmer), only when moving/quality up.
      if (current > 0.05 && q >= 0.6) {
        const streaks = Math.round(6 * q);
        for (let i = 0; i < streaks; i++) {
          const sx = ((i / streaks) * w + Math.sin(i * 2.1) * 20) % w;
          const [r, g, bl] = num.hslToRgb(190, 60, 40);
          ink(r, g, bl, 22 + current * 40).box(sx | 0, 0, 2, h);
        }
      }

      // Quality caps how many bubbles we actually render (cheapest live budget).
      const cap = Math.max(8, Math.round(MAX_BUBBLES * q));
      const draw = bubbles.length > cap ? bubbles.slice(-cap) : bubbles;

      const minWH = Math.min(w, h);
      for (const b of draw) {
        const bx = b.x * w;
        const by = b.y * h;
        const wob = 1 + Math.sin(b.phase) * 0.08 * b.wob; // wobble squash
        const rad = b.r * minWH * (b.popping ? 1 + (0.6 - b.pop) * 1.2 : 1) * wob;
        const alpha = (b.popping ? b.pop : b.life);
        const [r, g, bl] = num.hslToRgb(((b.hue % 360) + 360) % 360, 80, 62);

        // Translucent soap body (two soft fills → depth), cheap filled circles.
        ink(r, g, bl, Math.round(48 * alpha)).circle(bx, by, rad, true);
        ink(r, g, bl, Math.round(30 * alpha)).circle(bx, by, rad * 0.7, true);
        // Rim ring (surface tension) — bright thin outline.
        ink(Math.min(255, r + 60), Math.min(255, g + 60), Math.min(255, bl + 60),
          Math.round(150 * alpha)).circle(bx, by, rad, false, 1.5);
        // Specular highlight (upper-left) — the classic bubble glint.
        ink(255, 255, 255, Math.round(190 * alpha))
          .circle(bx - rad * 0.32, by - rad * 0.34, Math.max(1, rad * 0.14), true);
        // A tiny secondary sparkle for big bubbles.
        if (rad > 12) ink(255, 255, 255, Math.round(90 * alpha))
          .circle(bx + rad * 0.22, by - rad * 0.1, Math.max(1, rad * 0.06), true);
      }

      // POP flashes: quick expanding rings + droplet spray hint.
      for (const p of pops) {
        const [r, g, bl] = num.hslToRgb(((p.hue % 360) + 360) % 360, 85, 70);
        ink(r, g, bl, Math.round(180 * p.life)).circle(p.x * w, p.y * h, p.r * minWH, false, 2);
        ink(255, 255, 255, Math.round(120 * p.life)).circle(p.x * w, p.y * h, p.r * minWH * 0.5, false, 1);
      }

      // Surface shimmer line near the top (where bubbles pop).
      if (surface > 0.02) {
        const sy = h * 0.06;
        ink(180, 230, 255, Math.round(120 * surface)).box(0, sy | 0, w, 2);
      }

      // Subtle blur on strong beats/taps → dreamy watery bloom (only when cheap).
      if ((surface > 0.5 || pump > 1.3 || amp > 0.5) && q >= 0.85) blur?.(1);

      // Plink any bubbles that popped during sim (deferred so a synth is in hand).
      if (pendingPlinks.length && lastSynth) {
        for (const pl of pendingPlinks) {
          voices.bell(lastSynth, pl.note, { beats: 0.35, volume: 0.14, pan: pl.pan });
          voices.pluck(lastSynth, pl.note, { beats: 0.3, decay: 0.5, volume: 0.12, pan: pl.pan });
        }
        pendingPlinks.length = 0;
      }
    },
  },
};

// --- helpers ----------------------------------------------------------------
let lastSynth = null; // most recent synth handle (for sim-time pop plinks)
const pendingPlinks = []; // pops queued in sim, played next paint

// Birth a rising bubble at the floor. rNorm = pitch (0 low/big .. 1 high/small).
function birthBubble(x, pn, radiusMm, pan, lifeMul = 1) {
  if (bubbles.length >= MAX_BUBBLES) bubbles.shift(); // drop the oldest
  const rNorm = Math.max(0, Math.min(1, pn));
  // Visual radius (screen-frac): big for low pitch, small for high. Derived from
  // the same physical radius handed to the bubble synth so sound + sight agree.
  const r = 0.02 + (radiusMm / 54) * 0.11; // 0.02 (tiny/high) .. ~0.13 (fat/low)
  const b = {
    id: nextId++,
    x,
    y: 0.96, // born at the floor
    r,
    rNorm,
    hue: 190 + (1 - rNorm) * 130, // low = warm (magenta/red), high = cool (cyan)
    wob: 0.4 + Math.random() * 0.6,
    phase: Math.random() * Math.PI * 2,
    life: 0.1 * lifeMul,
    popping: false,
    pop: 0,
    note: null,
    pan,
  };
  // remember the note letter for the pop plink (approximate from pitch band).
  b.note = ["a2", "e3", "a3", "c4", "e4", "a4"][Math.round(rNorm * 5)];
  bubbles.push(b);
  return b;
}

// Pop a bubble: flag it, spawn a flash, kick the surface shimmer, queue a plink.
function popBubble(b) {
  b.popping = true;
  b.pop = 0.6;
  pops.push({ x: b.x, y: b.y, r: b.r * 0.5, life: 1, hue: b.hue });
  surface = Math.min(1.4, surface + 0.5);
  pendingPlinks.push({ note: b.note || "a4", pan: b.pan || 0 });
}

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
