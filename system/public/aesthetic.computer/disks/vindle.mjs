// vindle, 26.07.12
// A GROWING-VINE instrument — the melody literally DRAWS a plant. A thin wrapper
// over lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `vindle 0.5`, the tap/XY "pump", audio polling). This file only
// describes what makes vindle vindle: its score, its woody voices, and its vine
// paint. Every arp note APPENDS a vine segment (turn angle ∝ pitch, hue ∝ pitch);
// accents bud a flower; bass thickens the trunk; hats flick a leaf; the vine
// fades + regrows at the loop seam — the visual IS the score, a drawn plant.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// One woody plucked note per beat → one new vine segment. D minor pentatonic-ish,
// rising then settling → the vine climbs then curls. The vine REGROWS every
// ARP.length beats (the loop seam), so the whole phrase is one plant.
const ARP = [
  "d3", "a3", "d4", "f4",
  "a4", "g4", "e4", "c4",
  "d4", "f4", "a4", "d5",
  "c5", "a4", "f4", "d4",
];
const LOOP_LEN = ARP.length; // 16 segments per grown vine
const BASS = ["d2", "d2", "f2", "a2"]; // warm woody trunk, one per bar (4 beats)
const ACCENT_STEPS = new Set([4, 11, 12]); // buds sprout at the phrase's peaks

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. Wraps num.hslToRgb (wants hue in
// DEGREES + sat/light 0..100, and already returns 0..255 — do NOT ×255).
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb((((hue % 1) + 1) % 1) * 360, sat * 100, light * 100);
}

// --- vindle-specific visual state (the engine owns pump/bursts/rhythm) -------
// The vine is an array of segments appended on each beat. Each records its
// endpoints, thickness, hue and any bud — so older growth persists and the whole
// melody is legible as the drawn plant.
let vine = []; // [{ x0,y0,x1,y1, hue, thick, born, bud, budHue, isFlower }]
let tipX = 0, tipY = 0; // current growing tip (screen coords)
let tipAngle = -Math.PI / 2; // heading; -90° = straight up
let vineFade = 1; // 1 = solid, → 0 as the vine fades at the loop seam
let rootPulse = 0; // decays; blooms from the base on the bass/trunk beat
let trunkThick = 0; // decays; extra base-stem girth on the bass beat
let beatCount = 0; // total beats seen (for the tip-glow born counter)

let shoots = []; // tap-sprouted mini vines: { segs, tipX,tipY,angle, hue,pan, grow, life }
let flecks = []; // tiny leaf-flicker flecks (hats + tap sparkle)

// The engine's pump lives in ctx; onBeat fires from inside sim so we stash the
// latest pump each onSim and read it in onBeat's geometry helpers.
let curPump = 0;
const pump = () => curPump;
let grounded = false; // has the deep-forest ground been laid solid yet?

// Reset the vine to a fresh sprout at the base — called at each loop seam.
function resprout(w, h) {
  vine = [];
  tipX = w / 2;
  tipY = h * 0.92; // grow up from near the bottom
  tipAngle = -Math.PI / 2; // straight up
  vineFade = 1;
}

const CONFIG = {
  bpm: 108, // patient, hypnotic (quarter-note grid; one segment per beat)
  steps: LOOP_LEN,
  drawBursts: false, // vindle draws its own leaf-fleck sprouts in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.18, feedback: 0.45 }); // woody room
    },

    // A new UTC beat crossed — append a vine segment + pluck the note. Use idx to
    // detect the loop seam (idx % LOOP_LEN === 0) → fade has finished, resprout.
    onBeat({ idx, synth, screen }) {
      const w = screen.width, h = screen.height;
      const s = ((idx % LOOP_LEN) + LOOP_LEN) % LOOP_LEN;

      // Lazily seed the first sprout, and resprout cleanly at each loop seam.
      if (s === 0 || (vine.length === 0 && tipX === 0 && tipY === 0)) resprout(w, h);

      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const accent = ACCENT_STEPS.has(s);
      const pan = -0.5 + (s / (LOOP_LEN - 1)) * 1.0;

      // --- SONIC: woody plucked note (harp Karplus–Strong + rounder sine body).
      voices.pluck(synth, note, { beats: 1.1, decay: 0.5, volume: 0.55, pan });
      synth({ tone: note, type: "sine", beats: 0.8, attack: 0.003, decay: 0.5,
        volume: 0.28, pan }); // rounder woody body
      if (accent) {
        // A brighter bell bloom when a leaf/flower buds.
        voices.bell(synth, note, { beats: 0.9, volume: 0.28, pan });
      }

      // --- TRUNK BASS every bar (every 4 beats) → root pulse + trunk thickens.
      if (s % 4 === 0) {
        const bn = BASS[(s / 4) % BASS.length];
        voices.sub(synth, bn, { beats: 1.8, decay: 0.4, volume: 0.55, pan: 0 });
        rootPulse = 1;
        trunkThick = 1;
      }

      // --- HAT on off-beats → soft leaf-rustle + a tiny flicker fleck.
      if (s % 2 === 1) {
        voices.hat(synth, { tone: 1200, beats: 0.2, volume: s % 4 === 3 ? 0.2 : 0.12 });
        flecks.push({ x: tipX, y: tipY, vx: (Math.random() - 0.5) * 1.6,
          vy: -Math.random() * 1.2, life: 1, hue: 0.28 });
      }

      // --- GRAPHIC: extend the vine by one segment. TURN ANGLE ∝ pitch: a high
      // note bends the vine UP (toward -90°), a low note bends it DOWN. Centered
      // on straight-up so the melodic contour becomes the vine's shape.
      const turn = (pn - 0.5) * (Math.PI * 0.9); // -0.45π (low)..+0.45π (high)
      const targetAngle = -Math.PI / 2 + -turn; // high pitch → more upward
      tipAngle = tipAngle * 0.55 + targetAngle * 0.45;
      tipAngle += Math.sin(idx * 0.7) * 0.12; // organic sway

      const segLen = Math.min(w, h) * (0.05 + pn * 0.03) * (1 + pump() * 0.35);
      const nx = tipX + Math.cos(tipAngle) * segLen;
      const ny = tipY + Math.sin(tipAngle) * segLen;
      const hue = 0.22 + pn * 0.22; // green (low) → chartreuse/gold (high)
      const thick = (2 + pn * 3) * (1 + pump() * 0.5);

      vine.push({
        x0: tipX, y0: tipY, x1: nx, y1: ny,
        hue, thick, born: beatCount, pan,
        bud: accent || pump() > 1.2, // buds on accents (and lushly when pumped)
        budHue: accent ? 0.06 + pn * 0.08 : hue + 0.05, // gold/coral flower vs leaf
        isFlower: accent,
      });
      tipX = nx;
      tipY = ny;
      beatCount++;

      // Keep the tip legible in-frame.
      if (tipX < w * 0.12) tipAngle += 0.25;
      if (tipX > w * 0.88) tipAngle -= 0.25;
      if (tipY < h * 0.12) tipAngle = -tipAngle * 0.3 + 0.4; // ceiling → curl down
    },

    onSim({ pump: p, step, beatProgress, screen }) {
      const w = screen.width, h = screen.height;
      if (vine.length === 0 && tipX === 0 && tipY === 0) resprout(w, h);
      curPump = p; // stash engine pump for onBeat geometry (fires inside sim)

      // Fade the vine across the last part of its life so it gracefully dissolves
      // just before the seam → seamless regrow. local phase 0..1 across the loop.
      const phase = (step + beatProgress) / LOOP_LEN; // 0..1 through this vine
      vineFade = phase > 0.86 ? Math.max(0, 1 - (phase - 0.86) / 0.14) : 1;

      // Advance flecks (leaf flickers).
      for (const f of flecks) {
        f.x += f.vx; f.y += f.vy; f.vy += 0.08; f.vx *= 0.97; f.life -= 0.035;
      }
      flecks = flecks.filter((f) => f.life > 0);

      // Advance tap-sprouted shoots — each grows its own little vine over time.
      for (const sh of shoots) {
        sh.grow += 0.06 + p * 0.05; // grow faster when pumped
        while (sh.segs.length < Math.floor(sh.grow) && sh.segs.length < 9) {
          const segLen = Math.min(w, h) * 0.045 * (1 + p * 0.3);
          sh.angle += (Math.random() - 0.5) * 0.5 - 0.12; // curl upward-ish
          const nx = sh.tipX + Math.cos(sh.angle) * segLen;
          const ny = sh.tipY + Math.sin(sh.angle) * segLen;
          sh.segs.push({ x0: sh.tipX, y0: sh.tipY, x1: nx, y1: ny,
            bud: sh.segs.length === 8 });
          sh.tipX = nx; sh.tipY = ny;
        }
        sh.life -= 0.006;
      }
      shoots = shoots.filter((sh) => sh.life > 0);

      // Decay pulses (the engine decays pump itself).
      rootPulse *= 0.9;
      trunkThick *= 0.92;
    },

    // Tap = a NEW SHOOT sprouts from the touch point + a plucked boost (engine
    // already bumped pump + pushed the burst; X→pan/hue, Y→pitch, top = high).
    onTap({ x, y, ex, ey, synth, isDraw }) {
      const drag = isDraw;
      const hue = 0.22 + (1 - y) * 0.24; // higher tap → brighter/golder shoot
      shoots.push({
        segs: [], tipX: ex, tipY: ey,
        angle: -Math.PI / 2 + (x - 0.5) * 1.0, // X biases the initial lean
        hue, pan: x * 2 - 1, grow: 0, life: 1,
      });
      // A little burst of leaf flecks at the sprout point.
      const n = drag ? 3 : 9;
      for (let i = 0; i < n && flecks.length < 200; i++) {
        const ang = Math.random() * Math.PI * 2;
        const sp = 1 + Math.random() * (drag ? 2 : 4);
        flecks.push({ x: ex, y: ey, vx: Math.cos(ang) * sp,
          vy: Math.sin(ang) * sp - 1, life: 1, hue });
      }

      // SONIC pluck — X→pan/hue, Y→pitch (top = high). Woody harp + sine body.
      const scale = ["d", "e", "f", "a", "c"];
      const oct = 2 + Math.floor((1 - y) * 3); // 2..4, higher tap = higher
      const note = scale[Math.min(4, Math.floor(x * 5))] + oct;
      voices.pluck(synth, note, { beats: drag ? 0.5 : 1.0, decay: 0.5,
        volume: drag ? 0.42 : 0.72, pan: x * 2 - 1 });
      synth({ tone: note, type: "sine", beats: drag ? 0.3 : 0.6, attack: 0.003,
        decay: 0.5, volume: 0.28, pan: x * 2 - 1 }); // woody body
    },

    onPaint(api, s) {
      const { ink, box, screen, num, paintCount } = api;
      const { pump, band } = s;
      const w = screen.width, h = screen.height;
      const baseX = w / 2, baseY = h * 0.92;

      // Lay a SOLID deep-forest ground on the first frames so the field reads dark
      // green (a translucent veil alone converges to neutral gray). After that, a
      // translucent green veil each frame → older growth persists then fades.
      if (!grounded || Number(paintCount) < 3) {
        ink("fade:#06180c-#010604:vertical").box(0, 0, w, h);
        grounded = true;
      }
      ink(5, 20, 11, Math.max(80, 110 - pump * 8)).box(0, 0, w, h);
      ink("fade:#06180c-#010604:vertical", 40).box(0, 0, w, h);

      // --- Live audio garnish (visuals never DEPEND on it) ---
      const bass = band("subBass");
      const air = band("air");

      // === ROOT PULSE — the bass/trunk beat blooms from the base of the plant ===
      const bloom = Math.max(rootPulse, 0) * (1 + pump * 0.3);
      if (bloom > 0.02) {
        const R = Math.min(w, h) * (0.1 + bass * 0.4) * bloom;
        const [r0, g0, b0] = hue2rgb(num, 0.11, 0.7, 0.35); // warm earthy glow
        for (let i = 4; i > 0; i--) {
          ink(r0, g0, b0, 34 * bloom).circle(baseX, baseY, R * (i / 4), true);
        }
      }

      // === THE TRUNK — a thick woody stem rising from the base into the vine ===
      const trunkH = Math.min(h, w) * 0.08;
      const trunkW = 4 + trunkThick * 5 + pump * 2;
      const [tr, tg, tb] = hue2rgb(num, 0.09, 0.55, 0.28); // deep brown-green
      ink(tr, tg, tb, 235 * vineFade)
        .line(baseX, baseY, baseX, baseY - trunkH, trunkW);

      // === THE VINE — the drawn melody. Each segment's angle+hue ∝ its note. ===
      // Draw oldest→newest so the tip sits on top. Recently-born segments glow.
      const now = beatCount;
      for (const seg of vine) {
        const age = now - seg.born; // beats since it grew
        const fresh = Math.max(0, 1 - age * 0.16); // recent segments glow brighter
        const [r, g, b] = hue2rgb(num, seg.hue, 0.85, 0.42 + fresh * 0.28);
        const a = 240 * vineFade;
        const th = seg.thick * (1 + fresh * 0.6 + bass * 0.5);
        ink(r, g, b, a).line(seg.x0, seg.y0, seg.x1, seg.y1, Math.max(1, th));
        // A soft highlight core down the stem.
        if (th > 2) {
          const [hr, hg, hb] = hue2rgb(num, seg.hue, 0.5, 0.72);
          ink(hr, hg, hb, 120 * vineFade)
            .line(seg.x0, seg.y0, seg.x1, seg.y1, Math.max(1, th * 0.4));
        }

        // === LEAF / FLOWER bud at the tip of accented segments ===
        if (seg.bud) {
          const budGrow = Math.min(1, 0.3 + fresh); // pops as it's born
          const br = (3 + budGrow * 5) * (1 + pump * 0.3);
          const [pr, pg, pb] = hue2rgb(num, seg.budHue, seg.isFlower ? 0.9 : 0.7,
            seg.isFlower ? 0.6 : 0.45);
          if (seg.isFlower) {
            // A little flower: petals around the tip.
            for (let p = 0; p < 5; p++) {
              const ang = (p / 5) * Math.PI * 2 + now * 0.05;
              ink(pr, pg, pb, 220 * vineFade)
                .circle(seg.x1 + Math.cos(ang) * br, seg.y1 + Math.sin(ang) * br,
                  br * 0.6 + budGrow * 2, true);
            }
            ink(255, 230, 120, 230 * vineFade).circle(seg.x1, seg.y1, br * 0.7, true); // pollen
          } else {
            ink(pr, pg, pb, 220 * vineFade).circle(seg.x1, seg.y1, br, true); // leaf
          }
        }
      }

      // === The growing TIP — a bright bud that swells within each beat ===
      if (vine.length > 0 && vineFade > 0.02) {
        const tipR = (3 + s.beatProgress * 4 + bass * 8) * (1 + pump * 0.4);
        ink(230, 255, 180, 220 * vineFade).circle(tipX, tipY, tipR, true);
        ink(255, 255, 255, 160 * vineFade).circle(tipX, tipY, tipR * 0.45, true);
      }

      // === TAP SHOOTS — the whole-piece "button" made visible as new plants ===
      for (const sh of shoots) {
        const [r, g, b] = hue2rgb(num, sh.hue, 0.85, 0.5);
        for (const sg of sh.segs) {
          ink(r, g, b, 235 * sh.life).line(sg.x0, sg.y0, sg.x1, sg.y1,
            Math.max(1, (2 + pump) * sh.life));
          if (sg.bud) {
            const [br2, bg2, bb2] = hue2rgb(num, sh.hue + 0.05, 0.9, 0.6);
            ink(br2, bg2, bb2, 230 * sh.life).circle(sg.x1, sg.y1, 4 + pump, true);
          }
        }
        ink(230, 255, 180, 200 * sh.life).circle(sh.tipX, sh.tipY, 3 + pump, true);
      }

      // === HAT flecks = tiny leaf flickers ===
      for (const f of flecks) {
        const [r, g, b] = hue2rgb(num, f.hue, 0.7, 0.55);
        ink(r, g, b, 220 * f.life).circle(f.x, f.y, 1 + f.life * 2, true);
      }

      // Air-band shimmer accent (garnish when audio is live) — dew on the leaves.
      if (air > 0.05) {
        ink(200, 255, 210, air * 90)
          .circle(baseX, baseY - trunkH, Math.min(w, h) * 0.2, false, 1);
      }
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared singleton
// — each entry must re-assert THIS pad's config before the engine boot/sim/paint/
// act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
