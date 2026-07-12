// voop, 26.07.12
// Bouncing-droplet pentatonic score — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `voop 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes voop voop: its bounce pattern, its rubbery voices, and its droplet paint.
// Every audible note is a BOUNCE IMPACT — the droplet hits the floor, emits a
// ripple ring, and its post-impact arc HEIGHT ∝ pitch, HUE ∝ pitch. A viewer
// literally READS the melody as the height + color of the bounces. Bass = a
// central floor shockwave; sparkles fly up on impact; a comet ghost traces the
// melodic contour. The visual IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// One looping bar of a bright pentatonic bounce (12 steps). Step 0 == the seam.
const BOUNCE = [
  "c3", "e3", "g3", "a3", "g3", "c4",
  "a3", "g3", "e3", "d3", "g3", "e3",
];
const SUB = ["c2", "c2", "g1", "a1", "g1", "c2"]; // half-time floor thump root

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const BOUNCE_PITCHES = BOUNCE.map(notePitch);
const PITCH_MIN = Math.min(...BOUNCE_PITCHES);
const PITCH_MAX = Math.max(...BOUNCE_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN); // 0..1

// --- voop-specific visual state (the engine owns pump/bursts/rhythm) ---------
// The main droplet (the score's ball) flies a parabolic arc between impacts. On
// each impact we set its arc height ∝ pitch and it lands one beat later — so the
// arc, drawn live from ctx.beatProgress, IS the melody.
let dropX = 0.5; // 0..1 horizontal this frame (lerped across the flight)
let dropXNext = 0.5; // target X for the current flight
let dropXPrev = 0.5;
let arcHeight = 0.4; // 0..1 apex height of the current flight (pitch)
let dropHue = 180; // hue of the droplet this flight (pitch)
let squash = 0; // impact squash, 1 at impact, decays — glossy bounce feel

let ripples = []; // floor impact rings { x, r, life, hue }
let sparkles = []; // tiny droplets flung up on impact { x, y, vx, vy, life, hue }
let ghost = []; // motion-blur comet trail { x, yPhase, hue, life }
let ghostTick = 0; // throttle counter for ghost sampling
let floorFlash = 0; // floor line flash on sub-thump / taps, decays
let subThump = 0; // central low shockwave from bass, decays

// Extra tap-flung droplets: independent bouncing balls the viewer flings in.
let flung = []; // { x, y, vx, vy, r, hue, life, bounces }

// Parabolic arc height for a flight: 0 at the impacts (t=0,1), 1 at apex (t=.5).
const arcParab = (t) => 4 * t * (1 - t);

const CONFIG = {
  bpm: 132, // springy, hypnotic
  steps: BOUNCE.length,
  drawBursts: false, // voop draws its own tap-flung droplets in onPaint
  hooks: {
    onBoot({ sound }) {
      // A gentle rubbery room so each "voop" has a springy tail.
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.5 });
    },

    // A new UTC beat crossed — this note is a BOUNCE IMPACT. Play the voop,
    // emit a ripple, set the next arc height ∝ pitch; bass thumps the floor.
    onBeat({ idx, synth, screen }) {
      const w = screen?.width || 360;
      const h = screen?.height || 640;
      const s = ((idx % BOUNCE.length) + BOUNCE.length) % BOUNCE.length;
      const note = BOUNCE[s];
      const pn = pitchNorm(note); // 0 (low) .. 1 (high)

      // The droplet just LANDED here (end of previous flight). Impact!
      const impactX = dropXNext;
      dropXPrev = impactX;

      // ALLEGORY: bounce HEIGHT after impact ∝ pitch; HUE ∝ pitch.
      arcHeight = 0.22 + pn * 0.6; // high note = high arc
      dropHue = 200 - pn * 200; // low→cyan/blue, high→warm pink (candy)
      if (dropHue < 0) dropHue += 360;

      // Next landing X: step across the floor so the arcs march left↔right,
      // reading as a bouncing melody scanning the width. Deterministic for loop.
      dropXNext = 0.12 + (s / (BOUNCE.length - 1)) * 0.76;
      dropX = impactX;
      squash = 1; // fat impact squash → gloss

      // Richened rubbery "voop": a Karplus–Strong pluck + a rubbery sub body.
      const pan = impactX * 2 - 1;
      voices.pluck(synth, note, { beats: 0.55, decay: 0.62, volume: 0.42, pan });
      voices.sub(synth, note, { beats: 0.28, attack: 0.004, decay: 0.4, volume: 0.2, pan });

      // Floor ripple ring born exactly at the impact point.
      ripples.push({ x: impactX * w, r: 6, life: 1, hue: dropHue });

      // Sparkle droplets flung up on impact — a few tiny secondary droplets.
      const nSpark = 4 + Math.round(pn * 5);
      for (let i = 0; i < nSpark; i++) {
        const ang = -Math.PI / 2 + (Math.random() - 0.5) * 1.6;
        const spd = 2 + Math.random() * (3 + pn * 4);
        sparkles.push({
          x: impactX * w,
          y: h * 0.82,
          vx: Math.cos(ang) * spd,
          vy: Math.sin(ang) * spd,
          life: 1,
          hue: dropHue,
        });
      }

      // A bright bell shimmer on high steps — the flung droplets' sparkle.
      if (pn > 0.6) voices.bell(synth, note, { beats: 0.35, volume: 0.16, pan: -pan });

      // Sub thump on the half-beat — the floor shockwave you can feel + SEE.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % SUB.length) + SUB.length) % SUB.length;
        voices.sub(synth, SUB[bi], { beats: 1.4, attack: 0.008, decay: 0.5, volume: 0.5 });
        subThump = 1; // ALLEGORY: bass = central floor shockwave
        floorFlash = Math.max(floorFlash, 0.8);
      }
    },

    onSim({ screen, beatProgress }) {
      const w = screen?.width || 360;
      const h = screen?.height || 640;

      // Interpolate the droplet's horizontal position across the flight
      // (impact→next), driven by the engine's UTC beatProgress.
      dropX = dropXPrev + (dropXNext - dropXPrev) * beatProgress;

      // Ghost trail: sample the droplet's current position each few ticks so
      // the trail reads as a comet, not a solid line.
      ghostTick = (ghostTick + 1) % 2;
      if (ghostTick === 0) {
        ghost.push({ x: dropX, yPhase: beatProgress, hue: dropHue, life: 1 });
        if (ghost.length > 40) ghost.shift();
      }

      // Decays (per tick).
      squash *= 0.86;
      subThump *= 0.9;
      floorFlash *= 0.9;

      // Advance / cull floor ripples.
      for (const rp of ripples) {
        rp.r += 5 + rp.life * 7;
        rp.life -= 0.05;
      }
      ripples = ripples.filter((rp) => rp.life > 0);

      // Advance sparkle droplets under gravity.
      for (const sp of sparkles) {
        sp.x += sp.vx;
        sp.y += sp.vy;
        sp.vy += 0.35; // gravity
        sp.life -= 0.035;
      }
      sparkles = sparkles.filter((sp) => sp.life > 0 && sp.y < h + 20);

      // Fade the ghost trail.
      for (const g of ghost) g.life -= 0.03;
      ghost = ghost.filter((g) => g.life > 0);

      // Advance tap-flung droplets: full bouncing physics on the floor line.
      const floorY = h * 0.82;
      for (const f of flung) {
        f.x += f.vx;
        f.y += f.vy;
        f.vy += 0.45; // gravity
        if (f.y >= floorY && f.vy > 0) {
          f.y = floorY;
          f.vy *= -0.72; // bouncy restitution
          f.vx *= 0.94;
          f.bounces++;
          ripples.push({ x: f.x, r: 5, life: 1, hue: f.hue });
          floorFlash = Math.max(floorFlash, 0.5);
          if (Math.abs(f.vy) > 1.2 && f.bounces < 6) {
            const note = ["c", "e", "g", "a", "c"][Math.min(4, f.bounces)] + 3;
            voices.pluck(f.synth, note, {
              beats: 0.4,
              decay: 0.5,
              volume: 0.38,
              pan: (f.x / w) * 2 - 1,
            });
          }
        }
        f.life -= 0.006;
      }
      flung = flung.filter(
        (f) => f.life > 0 && (Math.abs(f.vy) > 0.2 || f.bounces < 3),
      );
      if (flung.length > 8) flung = flung.slice(-8);
    },

    // Tap = FLING an extra droplet from the touch point that bounces + voops.
    // Engine already bumped pump + pushed the burst; X→pan/hue, Y→launch/pitch.
    onTap({ x, y, ex, ey, synth }) {
      floorFlash = Math.max(floorFlash, 0.7);
      const hue = x * 300; // candy hue across the width
      // Launch height ∝ (1 - y): tapping high flings hard, low = gentle bounce.
      const launch = -(4 + (1 - y) * 12);
      flung.push({
        x: ex,
        y: ey,
        vx: (Math.random() - 0.5) * 4,
        vy: launch,
        r: 6 + (1 - y) * 8,
        hue,
        life: 1,
        bounces: 0,
        synth, // captured so onSim's bounce voops can sound
      });

      // SONIC BOOST — X→pitch/pan, Y→octave, pentatonic candy palette.
      const note =
        ["c", "d", "e", "g", "a"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
      voices.pluck(synth, note, { beats: 0.5, decay: 0.6, volume: 0.5, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.25, volume: 0.28 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, beatProgress, amp, band } = s;
      const { width: w, height: h } = screen;
      const floorY = h * 0.82;

      // --- Live audio reads ---------------------------------------------------
      const bass = band("subBass");
      const energy = Math.min(2, subThump * 0.6 + bass * 1.1 + amp * 0.5 + pump * 0.5);

      // --- Background gradient (hand-rolled scanline ramp) --------------------
      // Repainted opaque each frame — the gooey comet trail is carried by the
      // `ghost` object list, so we DON'T rely on a partial veil (which muddies
      // to gray, and a `fade:` bg rendered muddy in capture). Deep candy tones.
      {
        const bandH = Math.max(2, Math.ceil(h / 48));
        for (let by = 0; by < h; by += bandH) {
          const t = by / h; // 0 top .. 1 bottom
          const r = 14 + t * 30; // deep indigo → violet
          const g = 6 + t * 8;
          const bl = 40 + t * 50;
          ink(r, g, bl).box(0, by, w, bandH);
        }
      }

      // --- Floor line ---------------------------------------------------------
      const ff = Math.min(1, floorFlash + subThump * 0.6);
      const [fr, fg, fb] = num.hslToRgb((dropHue + 180) % 360, 80, 55 + ff * 30);
      ink(fr, fg, fb, 90 + ff * 130).box(0, floorY - 1 - ff * 2, w, 2 + ff * 4);
      ink(fr, fg, fb, 24 + ff * 40).box(0, floorY, w, h - floorY); // soft glow band

      // Central floor shockwave from the sub-thump (bass = shockwave).
      if (subThump > 0.02) {
        const sr = w * (0.1 + subThump * 0.55);
        for (let i = 3; i > 0; i--) {
          ink(fr, fg, fb, 22 * subThump).oval(w / 2, floorY, sr * (i / 3), sr * (i / 3) * 0.28, true);
        }
      }

      // --- Motion-blur ghost trail (melodic contour) --------------------------
      for (const g of ghost) {
        const gy = floorY - arcParab(g.yPhase) * arcHeight * (h * 0.62);
        const gx = g.x * w;
        const [r0, g0, b0] = num.hslToRgb(((g.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, 45 * g.life).circle(gx, gy, 5 + g.life * 9, true); // gooey halo
        ink(Math.min(255, r0 + 40), Math.min(255, g0 + 40), Math.min(255, b0 + 40), 110 * g.life)
          .circle(gx, gy, 2 + g.life * 5, true); // brighter core
      }

      // --- Floor ripples (each note = one impact ripple) ----------------------
      for (const rp of ripples) {
        const [r0, g0, b0] = num.hslToRgb(((rp.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, 180 * rp.life).oval(rp.x, floorY, rp.r, rp.r * 0.32, false);
        ink(255, 255, 255, 120 * rp.life).oval(rp.x, floorY, rp.r * 0.55, rp.r * 0.18, false);
      }

      // --- The main droplet (the score's ball) --------------------------------
      const arc = arcParab(beatProgress);
      const dy = floorY - arc * arcHeight * (h * 0.62);
      const dx = dropX * w;
      const [dr, dg, db] = num.hslToRgb(((dropHue % 360) + 360) % 360, 95, 62);
      const baseR = (12 + pump * 5 + energy * 4) * (1 + subThump * 0.2);
      // Squash & stretch: wide + flat at impact (arc~0), round at apex.
      const sq = squash * (1 - arc);
      const rw = baseR * (1 + sq * 0.9);
      const rh = baseR * (1 - sq * 0.55);

      // Glossy droplet: soft halo, body, bright specular highlight.
      ink(dr, dg, db, 70).oval(dx, dy, rw * 1.9, rh * 1.9, true); // halo
      ink(dr, dg, db, 235).oval(dx, dy, rw, rh, true); // body
      ink(Math.min(255, dr + 70), Math.min(255, dg + 70), Math.min(255, db + 70), 220)
        .oval(dx - rw * 0.25, dy - rh * 0.3, rw * 0.45, rh * 0.4, true); // inner sheen
      ink(255, 255, 255, 230).oval(dx - rw * 0.3, dy - rh * 0.35, rw * 0.2, rh * 0.2, true); // specular

      // --- Sparkle droplets flung up on impact --------------------------------
      for (const sp of sparkles) {
        const [r0, g0, b0] = num.hslToRgb(((sp.hue % 360) + 360) % 360, 100, 70);
        ink(r0, g0, b0, 220 * sp.life).circle(sp.x, sp.y, 1 + sp.life * 3, true);
      }

      // --- Tap-flung droplets -------------------------------------------------
      for (const f of flung) {
        const [r0, g0, b0] = num.hslToRgb(((f.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, 80 * f.life).circle(f.x, f.y, f.r * 1.7, true); // halo
        ink(r0, g0, b0, 240 * f.life).circle(f.x, f.y, f.r, true); // body
        ink(255, 255, 255, 220 * f.life).circle(f.x - f.r * 0.3, f.y - f.r * 0.3, f.r * 0.28, true);
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
