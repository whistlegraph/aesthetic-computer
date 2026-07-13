// trido, 26.07.12
// Boids-schooling pad — a thin wrapper over lib/pads.mjs (the shared pad engine:
// UTC-clock beat grid, `params[0]` rate override e.g. `trido 0.5`, the tap/XY
// "pump", audio polling). This file only describes what makes trido trido: a
// school of oriented ARROW/FISH agents that flock (alignment + cohesion +
// separation) while a flute/whistle waveguide voices its allegory.
//
// STRONG GRAPHIC↔SONIC ALLEGORY —
//   • the held DRONE bed STEERS the whole school's heading — the bass note maps
//     to a compass direction (a UTC-continuous "current") that every fish leans
//     into, so the flock's overall drift IS the drone;
//   • each mid NOTE that sounds SPAWNS A BRIGHT LEADER the flock darts toward
//     (pitch → the leader's HEIGHT and HUE) → you SEE the melody as glowing bait
//     the school chases;
//   • the BEAT = a synchronized FLASH + TURN (the whole school snaps + brightens);
//   • amplitude → school tightness/brightness.
//
// Distinct from murmo (a flow-field dot murmuration): trido is TRUE boids with
// visible oriented triangle-fish agents that dart after leaders. Cheap O(n): each
// fish steers off a few sampled neighbors + a shared flock average, never O(n²).

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A whistled pentatonic line over an 8-beat loop that resolves to the start so
// the audio loops as cleanly as the school's drift. Warm, airy, always-agreeing.
const MELODY = ["e4", "g4", "a4", "b4", "d5", "b4", "a4", "g4"];
const BASS = ["e2", "e2", "c2", "c2", "g2", "g2", "a2", "a2"]; // steers heading
const BPM = 96;

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToHz(note) {
  const m = /^([a-g])(#|s|b)?(\d)$/.exec(note.toLowerCase());
  if (!m) return 220;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "#" || m[2] === "s") semi += 1;
  else if (m[2] === "b") semi -= 1;
  const midi = semi + (+m[3] + 1) * 12;
  return 440 * Math.pow(2, (midi - 69) / 12);
}
// Map a note's Hz into a normalized 0..1 pitch band (~e2..d6).
function pitchNorm(hz) {
  const lo = noteToHz("e2"), hi = noteToHz("d6");
  const n = Math.log2(hz / lo) / Math.log2(hi / lo);
  return Math.max(0, Math.min(1, n));
}
// Map a bass note to a compass heading (0..2π) — the current the school steers.
function bassHeading(note) {
  const hz = noteToHz(note);
  const pn = pitchNorm(hz);
  // Full sweep of the compass across the bass range → a moving current.
  return pn * Math.PI * 2;
}

// --- trido-specific state (engine owns pump/bursts/rhythm/clock) -------------
const BASE_AGENTS = 220; // scaled by ctx.quality → holds 60fps
let fish = []; // { x, y, vx, vy } — position + velocity (heading = atan2(vy,vx))
let leaders = []; // { x, y, hue, life, strength } — bright bait the school darts to
let heading = 0; // current drone heading (radians) — steers the whole school
let headingTarget = 0; // where the drone wants the school to point
let flashT = 0; // beat flash/turn envelope (decays)
let droneStarted = false;
let seeded = false;
let W = 360, H = 640;

function seedSchool(w, h, n) {
  W = w; H = h;
  fish = [];
  for (let i = 0; i < n; i++) {
    const a = Math.random() * Math.PI * 2;
    const sp = 0.6 + Math.random() * 0.6;
    fish.push({
      x: Math.random() * w,
      y: Math.random() * h,
      vx: Math.cos(a) * sp,
      vy: Math.sin(a) * sp,
    });
  }
}

const CONFIG = {
  bpm: BPM,
  steps: MELODY.length, // seamless 8-beat cycle
  drawBursts: false, // trido draws its own tap wakes in onPaint
  hooks: {
    onBoot({ sound, screen }) {
      // A wide airy hall — the whistle line blooms and the school hisses along.
      sound.room?.set?.({ enabled: true, mix: 0.42, feedback: 0.56 });

      // Held DRONE bed = the CURRENT. A detuned low pad chord that literally is
      // the steering bass — root + faint beat partner + fifth for body.
      if (!droneStarted) {
        droneStarted = true;
        voices.padChord(
          sound.synth,
          [BASS[0], noteToHz(BASS[0]) * 1.007, "b2"],
          { volume: 0.11, attack: 1.5, decay: 0.9, spread: 0.4 },
        );
      }

      const w = screen?.width || 360;
      const h = screen?.height || 640;
      seedSchool(w, h, Math.round(BASE_AGENTS));
      seeded = true;
    },

    // A new UTC beat crossed — voice the whistle line + spawn the leader/bait.
    onBeat({ idx, synth }) {
      const step = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[step];
      const hz = noteToHz(note);
      const pN = pitchNorm(hz); // 0..1 → leader height + hue
      const pan = Math.sin(idx * 0.6) * 0.5; // slow stereo drift

      // Richened WHISTLE lead: a breathy waveguide flute + a detuned sine partner
      // (airy beating) + a soft octave-up glisten on high steps → true flute/pipe.
      voices.flute(synth, note, {
        beats: 1.8,
        attack: 0.08,
        decay: 0.55,
        volume: 0.3,
        pan,
      });
      synth({
        tone: hz * 1.006, // gently detuned breathy partner → whistle shimmer
        type: "sine",
        beats: 1.6,
        attack: 0.12,
        decay: 0.5,
        volume: 0.16,
        pan: -pan,
      });
      if (pN > 0.55)
        voices.bell(synth, hz * 2, { beats: 0.9, volume: 0.07, pan: -pan });

      // The STEERING bass on the half-beat — sets the current the school leans
      // into. New heading = where this bass note points.
      if (step % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, attack: 0.05, decay: 0.6, volume: 0.34 });
        headingTarget = bassHeading(BASS[bi]);
      }

      // Airy breath tick — the school's collective hiss/turn.
      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.1 });

      // The VISIBLE counterpart: a bright LEADER the school darts to. pitch → y
      // (top = high) + hue; brighter/bigger the higher it flies.
      leaders.push({
        x: W * (0.2 + 0.6 * ((step + 0.5) / MELODY.length)),
        y: H * (1 - pN) * 0.9 + H * 0.05,
        hue: 20 + pN * 300,
        life: 1,
        strength: 1,
      });
      if (leaders.length > 6) leaders.shift();

      flashT = 1; // synchronized FLASH + TURN
    },

    onSim(api) {
      const { simMs, pump } = api;

      // Ease the school heading toward the drone's target current (a gentle turn,
      // not a snap) — angle-wrapped so it takes the short way around.
      let d = headingTarget - heading;
      while (d > Math.PI) d -= Math.PI * 2;
      while (d < -Math.PI) d += Math.PI * 2;
      heading += d * 0.04;

      // A slow UTC-continuous wander added to the current so the drift breathes
      // and loops with the cycle (keeps two instances broadly aligned).
      const beatMs = api.beatSeconds * 1000;
      const cyclePos = (simMs / (beatMs * MELODY.length)) % 1;
      heading += Math.sin(cyclePos * Math.PI * 2) * 0.006;

      flashT *= 0.9;
      for (const L of leaders) L.life -= 0.016; // ~1s bait fade
      leaders = leaders.filter((L) => L.life > 0);

      void pump;
    },

    // Tap = drop a LEADER at the tap point the school chases + a whistle note.
    // X → pan/hue, Y → pitch (top = high). Engine already bumped pump + burst.
    onTap({ x, y, ex, ey, isDraw, synth, burst }) {
      const hue = (x * 300 + 20) % 360;
      burst.hue = hue;
      burst.grow = 5 + (isDraw ? 4 : 9);

      // A bright leader exactly at the tap → the visible bait matches the audible.
      leaders.push({
        x: ex,
        y: ey,
        hue,
        life: 1.2,
        strength: isDraw ? 0.7 : 1.2,
      });
      if (leaders.length > 8) leaders.shift();

      // Whistle note — pentatonic so it agrees with the line. Y→pitch, X→pan.
      const scale = ["e", "g", "a", "b", "d"];
      const noteName = scale[Math.min(4, Math.floor(x * 5))];
      const oct = 3 + Math.round((1 - y) * 3); // higher tap = higher whistle
      const pan = x * 2 - 1;
      const g = isDraw ? 0.6 : 1;
      voices.flute(synth, noteName + oct, {
        beats: 1.4,
        attack: 0.06,
        decay: 0.55,
        volume: 0.34 * g,
        pan,
      });
      synth({
        tone: noteToHz(noteName + oct) * 1.006,
        type: "sine",
        beats: 1.1,
        attack: 0.08,
        decay: 0.5,
        volume: 0.16 * g,
        pan: -pan,
      });
      flashT = Math.max(flashT, 0.6);
    },

    onPaint(api, s) {
      const { ink, box, screen, num } = api;
      const { pump, band, amp, quality, bursts } = s;
      const { width: w, height: h } = screen;
      W = w; H = h;

      // Re-seed / re-scale the school to quality (particle pads scale counts).
      const want = Math.max(60, Math.round(BASE_AGENTS * quality));
      if (!seeded) { seedSchool(w, h, want); seeded = true; }
      if (Math.abs(fish.length - want) > 40) {
        if (fish.length < want)
          for (let i = fish.length; i < want; i++) {
            const a = Math.random() * Math.PI * 2;
            fish.push({ x: Math.random() * w, y: Math.random() * h, vx: Math.cos(a), vy: Math.sin(a) });
          }
        else fish.length = want;
      }

      // —— audio reads (make the field react) ——
      const bass = band("subBass") + band("lowMid") * 0.6;
      const air = band("air") + band("treble") * 0.6;
      const energy = Math.min(2, flashT * 0.7 + bass * 1.1 + amp * 0.6 + pump * 0.4);

      // Deep-water twilight wash — a translucent gradient veil each frame → silky
      // wakes (indigo→teal depth). (Diagnostics showed the named gradient is not
      // the fps bottleneck; the aesthetic is worth it.)
      ink("fade:midnightblue-teal:vertical", 44).box(0, 0, w, h);

      // Current arrow HINT — a faint drift indicator so the drone's steer reads.
      const cx = w / 2, cy = h / 2;
      const chx = Math.cos(heading), chy = Math.sin(heading);
      ink(120, 180, 220, 30 + energy * 25).line(
        cx - chx * w * 0.4, cy - chy * h * 0.4,
        cx + chx * w * 0.4, cy + chy * h * 0.4, 2,
      );

      // —— flock advance (cheap O(n) boids) ——
      // Shared flock average (cohesion + alignment centroid) computed once per
      // frame; each fish also samples a couple of RANDOM neighbors for separation.
      let mx = 0, my = 0, mvx = 0, mvy = 0;
      for (const f of fish) { mx += f.x; my += f.y; mvx += f.vx; mvy += f.vy; }
      const n = fish.length || 1;
      mx /= n; my /= n; mvx /= n; mvy /= n;
      const avgSpeed = Math.hypot(mvx, mvy) || 1;

      // Speed scales with energy so the school surges on loud beats.
      const cruise = 1.3 + energy * 0.9 + pump * 0.5;
      const turnBoost = 1 + flashT * 2; // FLASH beat = a sharp collective turn

      // Pick the strongest live leader as the primary bait target (bright bait).
      let bait = null, baitS = 0;
      for (const L of leaders) {
        const sVal = L.strength * L.life;
        if (sVal > baitS) { baitS = sVal; bait = L; }
      }

      // Precompute a small hue→RGB palette ONCE per frame (hslToRgb is costly;
      // 12 buckets is plenty for a shimmering teal school warmed toward the bait).
      const light = Math.min(80, 58 + energy * 12 + flashT * 20);
      const baitHue = bait ? bait.hue : 175;
      const PAL = 12;
      const pal = new Array(PAL);
      for (let p = 0; p < PAL; p++) {
        // base teal-cyan (155..195) blended toward bait hue when chasing.
        let hue = 155 + (p / (PAL - 1)) * 40;
        if (bait) hue = hue * (1 - baitS * 0.4) + baitHue * baitS * 0.4;
        pal[p] = num.hslToRgb(((hue % 360) + 360) % 360, 85, light);
      }
      const fishAlpha = 190 + flashT * 60;

      for (let i = 0; i < fish.length; i++) {
        const f = fish[i];
        let ax = 0, ay = 0;

        // COHESION → toward flock centroid.
        ax += (mx - f.x) * 0.0009;
        ay += (my - f.y) * 0.0009;

        // ALIGNMENT → match flock mean heading.
        ax += (mvx - f.vx) * 0.03;
        ay += (mvy - f.vy) * 0.03;

        // SEPARATION → push off a couple of sampled neighbors (cheap, not O(n²)).
        for (let k = 0; k < 2; k++) {
          const g = fish[(i + 1 + ((Math.random() * (n - 1)) | 0)) % n];
          const dx = f.x - g.x, dy = f.y - g.y;
          const d2 = dx * dx + dy * dy;
          if (d2 < 900 && d2 > 0.01) {
            const inv = 1 / d2;
            ax += dx * inv * 24;
            ay += dy * inv * 24;
          }
        }

        // DRONE CURRENT → the whole school leans into the drone heading.
        ax += chx * 0.05 * turnBoost;
        ay += chy * 0.05 * turnBoost;

        // LEADER DART → the fish accelerate toward the brightest bait (pitch bait).
        if (bait) {
          const dx = bait.x - f.x, dy = bait.y - f.y;
          const dd = Math.hypot(dx, dy) || 1;
          const pullW = 0.14 * baitS * turnBoost;
          ax += (dx / dd) * pullW;
          ay += (dy / dd) * pullW;
        }

        f.vx += ax;
        f.vy += ay;

        // Clamp toward cruise speed.
        const sp = Math.hypot(f.vx, f.vy) || 1;
        const target = cruise;
        f.vx = (f.vx / sp) * (sp + (target - sp) * 0.1);
        f.vy = (f.vy / sp) * (sp + (target - sp) * 0.1);

        f.x += f.vx;
        f.y += f.vy;

        // Wrap the pond edges (toroidal so the school streams through).
        if (f.x < -8) f.x += w + 16; else if (f.x > w + 8) f.x -= w + 16;
        if (f.y < -8) f.y += h + 16; else if (f.y > h + 8) f.y -= h + 16;

        // Draw the oriented FISH as a cheap CHEVRON/ARROW (two lines) along
        // velocity — a filled triangle per fish (scanline .shape) is far too
        // costly at hundreds of agents; a chevron reads as an oriented arrow and
        // holds 60fps. Unit-vector form avoids extra cos/sin per fish.
        const spd = Math.hypot(f.vx, f.vy) || 1;
        const ux = f.vx / spd, uy = f.vy / spd; // forward
        const px = -uy, py = ux; // perpendicular
        const len = 5 + spd * 1.6;
        const wid = 2.6;
        const nx = f.x + ux * len, ny = f.y + uy * len; // nose
        const bx = f.x - ux * 2, by = f.y - uy * 2; // body base
        const lx = bx + px * wid, ly = by + py * wid; // left fin
        const rx = bx - px * wid, ry = by - py * wid; // right fin

        // Palette bucket by position (cheap shimmer) — no per-fish HSL.
        const pi = (((f.x * 0.03 + f.y * 0.03) | 0) % PAL + PAL) % PAL;
        const c = pal[pi];
        ink(c[0], c[1], c[2], fishAlpha)
          .line(nx, ny, lx, ly)
          .line(nx, ny, rx, ry);
      }

      // —— LEADERS: bright bait the school chases (glowing lures) ——
      for (const L of leaders) {
        const [r, gc, b] = num.hslToRgb(((L.hue % 360) + 360) % 360, 95, 62);
        const rad = 4 + L.strength * 8 + L.life * 6;
        ink(r, gc, b, 60 * L.life).circle(L.x, L.y, rad * 2.2, true);
        ink(r, gc, b, 200 * L.life).circle(L.x, L.y, rad, true);
        ink(255, 255, 255, 220 * L.life).circle(L.x, L.y, rad * 0.4, true);
      }

      // —— TAP WAKES (engine-tracked bursts): expanding hollow rings ——
      for (const bst of bursts) {
        const [r, gc, b] = num.hslToRgb(((bst.hue % 360) + 360) % 360, 90, 60);
        ink(r, gc, b, 170 * bst.life).circle(bst.x, bst.y, bst.r, false, 2);
      }

      // Beat FLASH veil — the whole pond brightens + snaps on the beat.
      if (flashT > 0.05 || air > 0.1) {
        const fa = Math.min(90, flashT * 70 + air * 40);
        ink(160, 220, 255, fa).box(0, 0, w, h);
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
