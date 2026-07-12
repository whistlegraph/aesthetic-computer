// lavabath, 26.07.12
// Raw-pixel metaballs plasma — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `lavabath 0.5`,
// the tap/XY "pump", audio polling). This file only describes what makes
// lavabath lavabath: its groove, its richened voices, its held field drone, and
// its metaball paint.
//
// ALLEGORY — each metaball CHARGE is a VOICE: it brightens/pulses on its note,
// its color IS its pitch (pitch→hue); the bass is the overall field HEAT; the
// BEAT literally tears the color channels (a channel-shift glitch on bar
// crossings). Read voices as blobs, the beat as the glitch.
//
// PAINT approach (KEEP): the heavy inverse-square metaball loop renders into a
// SMALL offscreen page() buffer (BUF_W×BUF_H) then `paste`s full-screen at
// native res; crisp voice halos + tap rings are drawn on top. No resolution().

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
const BASS = ["c2", "g1", "c2", "a1", "c2", "g1", "d2", "a1"]; // per-step low
const ARP = ["c4", "e4", "g4", "b4", "c5", "g4", "e4", "d4"]; // per-step lead
const PATTERN_LEN = 8;
const BAR_LEN = 4; // steps per bar → glitch fires on bar crossings
const NCHARGES = 5; // seed voices (orbiting)
const MAX_CHARGES = 12; // cap incl. tap-spawned voices (keeps the loop cheap)

// Note → hue so a charge's color reads as its pitch.
const NOTE_HUE = { c: 0, d: 45, e: 90, f: 140, g: 200, a: 260, b: 310 };
function noteHue(tone) {
  if (typeof tone !== "string") return 0;
  const letter = tone[0].toLowerCase();
  const oct = parseInt(tone.replace(/[^0-9]/g, ""), 10) || 4;
  return (NOTE_HUE[letter] ?? 0 + (oct - 2) * 8) % 360;
}
const TAP_NOTES = ["c", "d", "e", "g", "a"]; // pentatonic for taps

// --- lavabath-specific state (the engine owns pump/bursts/rhythm/clock) ------
let field = null; // small offscreen buffer {pixels,width,height}
let BUF_W = 132; // small → the N-charge inverse-square loop stays smooth
let BUF_H = 234;
let charges = []; // { seed,cx,cy,orbitRX,orbitRY,speed,phase,hue,x,y,strength,life,fade,pulse }
let time = 0; // smooth animation clock (seconds-ish)
let downbeatPulse = 0; // decays after each beat, spikes on bar starts
let glitchAmt = 0; // channel-shift glitch strength (decays)
let paletteShift = 0; // global hue rotation, snaps on the bar
let liveBass = 0; // smoothed subBass amplitude
let drone = null; // sustained low pad = the field's continuous heat/hum

function makeSeedCharge(i) {
  return {
    seed: true,
    // Periodic orbits → seamless loop. Each charge on its own ellipse.
    cx: BUF_W * (0.3 + 0.4 * ((i % 3) / 2)),
    cy: BUF_H * (0.22 + 0.56 * (i % 2)),
    orbitRX: BUF_W * (0.18 + (0.1 * ((i * 7) % 5)) / 5),
    orbitRY: BUF_H * (0.12 + (0.09 * ((i * 3) % 4)) / 4),
    speed: 0.4 + 0.22 * (i % 4), // integer-ish ratios → clean loop
    phase: (i / NCHARGES) * Math.PI * 2,
    hue: (i / NCHARGES) * 360,
    x: BUF_W / 2,
    y: BUF_H / 2,
    strength: 120 + i * 26,
    life: 1, // seed voices never die
    fade: 1, // current visibility 0..1 (seeds pinned to 1)
    pulse: 0, // brightens when this voice's note sounds
  };
}

const CONFIG = {
  bpm: 132,
  steps: PATTERN_LEN,
  drawBursts: false, // lavabath draws its own crisp tap rings in onPaint
  hooks: {
    onBoot({ sound, screen }) {
      // Reverb for the wet field bloom.
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.6 });

      // Size the small buffer to the screen's aspect so the paste-up isn't skewed.
      if (screen?.width && screen?.height) {
        BUF_W = 132;
        BUF_H = Math.max(120, Math.round(BUF_W * (screen.height / screen.width)));
      }

      // Seed the orbiting voices.
      charges = [];
      for (let i = 0; i < NCHARGES; i++) charges.push(makeSeedCharge(i));

      // Start the held field drone (continuous heat/hum). Swelled in onSim.
      time = 0;
      glitchAmt = 0;
      paletteShift = 0;
      liveBass = 0;
      drone = sound.synth?.({
        tone: "c2",
        type: "sine",
        duration: "🔁",
        attack: 0.4,
        decay: 0.9,
        volume: 0.001,
      });
    },

    // A new UTC beat crossed — fire the groove + light the matching charge.
    onBeat({ idx, synth }) {
      const step = ((idx % PATTERN_LEN) + PATTERN_LEN) % PATTERN_LEN;
      const bass = BASS[step];
      const lead = ARP[step];

      // Bass pulse — the overall field HEAT reads off these low notes.
      voices.sub(synth, bass, { beats: 0.95, decay: 0.55, volume: 0.6 });
      // Extra saw squelch on the root via a raw bubble — wet, viscous body.
      synth({ tone: bass, type: "bubble", beats: 0.5, attack: 0.005, decay: 0.4, volume: 0.24 });

      // Lead VOICE for this step — a pitched pluck; light its charge.
      const pan = -0.4 + (step / (PATTERN_LEN - 1)) * 0.8;
      voices.pluck(synth, lead, { beats: 0.5, decay: 0.4, volume: 0.42, pan });

      // Shimmer bell on the high steps.
      const hue = noteHue(lead);
      if (step % PATTERN_LEN >= 4) voices.bell(synth, lead, { beats: 0.5, volume: 0.16, pan: -pan });

      // Hat on the offbeats.
      if (step % 2 === 1) voices.hat(synth, { tone: 1200, beats: 0.12, volume: 0.18 });

      // Light the seed charge that owns this step: brighten + recolor to pitch.
      const seeds = charges.filter((c) => c.seed);
      if (seeds.length) {
        const c = seeds[step % seeds.length];
        c.pulse = 1; // synchronized visible birth at the moment it sounds
        c.hue = hue; // color = pitch
      }

      downbeatPulse = 1;
      if (step % BAR_LEN === 0) {
        glitchAmt = 1; // bar start → hard channel-shift glitch (you SEE the beat)
        paletteShift = (paletteShift + 47) % 360;
      }
    },

    onSim({ band, beatHit, pump }) {
      // Swell the held drone with live bass + tap pump.
      if (drone) drone.update?.({ volume: 0.16 + liveBass * 0.22 + Math.min(0.2, pump * 0.12) });

      // Smooth subBass → the field breathes with the groove.
      const bass = band("subBass");
      liveBass += (bass - liveBass) * 0.25;
      if (beatHit) glitchAmt = Math.max(glitchAmt, 0.6);

      // Advance the animation clock; faster on the beat + with energy + pump.
      time += 0.02 + liveBass * 0.05 + downbeatPulse * 0.01 + pump * 0.03;

      // Decay transient pulses.
      downbeatPulse *= 0.9;
      glitchAmt *= 0.88;

      // Move + age charges.
      for (let i = charges.length - 1; i >= 0; i--) {
        const c = charges[i];
        const a = time * c.speed + c.phase;
        const kick = 1 + liveBass * 0.8 + downbeatPulse * 0.35 + pump * 0.4;
        c.x = c.cx + Math.cos(a) * c.orbitRX * kick;
        c.y = c.cy + Math.sin(a * 1.3) * c.orbitRY * kick;
        c.pulse *= 0.9; // note-flash decays back to ambient

        if (c.seed) {
          c.fade = 1;
        } else {
          c.life -= 0.008; // tap-spawned voices decay + drift, then die
          c.fade = Math.max(0, Math.min(1, c.life * 1.4));
          if (c.life <= 0) charges.splice(i, 1);
        }
      }
    },

    // Tap = a fresh hot charge (a new VOICE) at the touch point + a boom (the
    // engine already bumped pump + pushed the burst). X→note/pan/hue, Y→pitch.
    onTap({ x, y, ex, ey, synth, burst }) {
      const letter = TAP_NOTES[Math.min(TAP_NOTES.length - 1, Math.floor(x * TAP_NOTES.length))];
      const oct = 2 + Math.floor((1 - y) * 4); // top = higher
      const tone = letter + oct;
      const hue = noteHue(tone);
      const pan = x * 2 - 1;

      // BOOM — the poke's audible burst.
      synth({ tone, type: "sine", beats: 0.5, attack: 0.004, decay: 0.6, volume: 0.55, pan });
      voices.pluck(synth, tone, { beats: 0.3, decay: 0.3, volume: 0.3 * (1 - y), pan });

      // Recolor the engine's burst to this pitch.
      if (burst) burst.hue = hue;

      // Spawn a fresh hot charge (a new VOICE) at the tap point in buffer space.
      if (charges.length < MAX_CHARGES) {
        const bx = x * BUF_W;
        const by = y * BUF_H;
        charges.push({
          seed: false,
          cx: bx,
          cy: by,
          orbitRX: BUF_W * 0.06,
          orbitRY: BUF_H * 0.05,
          speed: 0.7 + Math.random() * 0.6,
          phase: Math.random() * Math.PI * 2,
          hue,
          x: bx,
          y: by,
          strength: 150, // hot on birth (not so hot it whites out the field)
          life: 1,
          fade: 1,
          pulse: 1, // flashes as it sounds
        });
      }
    },

    onPaint(api, s) {
      const { screen, num, page, paste, ink, painting } = api;
      const { pump, bursts, amp, band } = s;

      // Lazily (re)build the offscreen field buffer at the right size.
      const wantH = Math.max(120, Math.round(BUF_W * (screen.height / screen.width)));
      if (!field || field.width !== BUF_W || field.height !== wantH) {
        BUF_H = wantH;
        field = painting(BUF_W, BUF_H, () => {});
      }

      // ── draw the heavy metaball field into the SMALL offscreen buffer ──
      page(field);
      const w = field.width;
      const h = field.height;
      const pixels = field.pixels;
      const nb = charges.length;

      const bass = band("subBass");

      // Precompute per-charge values out of the inner loop.
      const cx = new Float32Array(nb);
      const cy = new Float32Array(nb);
      const cr = new Float32Array(nb);
      const cg = new Float32Array(nb);
      const cbb = new Float32Array(nb);
      const cstr = new Float32Array(nb);
      // Bass = overall field heat; pump adds heat; each voice's pulse brightens.
      // Keep heat bounded so a hard pump thickens/brightens blobs without washing
      // the black field to white (lava-on-black must survive heavy tapping).
      const heat = 1 + liveBass * 1.0 + downbeatPulse * 0.8 + Math.min(0.9, pump * 0.35);
      for (let i = 0; i < nb; i++) {
        const c = charges[i];
        cx[i] = c.x;
        cy[i] = c.y;
        const hue = (c.hue + time * 20 + paletteShift) % 360;
        const light = Math.min(64, 42 + c.pulse * 22); // note-flash lifts lightness
        const rgb = num.hslToRgb(hue, 100, light); // returns 0-255
        cr[i] = rgb[0];
        cg[i] = rgb[1];
        cbb[i] = rgb[2];
        cstr[i] = c.strength * heat * (0.6 + c.pulse * 0.9) * (c.fade ?? 1);
      }

      // ── raw per-pixel inverse-square field (squared distance, no sqrt) ──
      let px4 = 0;
      for (let y = 0; y < h; y++) {
        for (let x = 0; x < w; x++) {
          let tw = 0;
          let r = 0;
          let g = 0;
          let b = 0;
          for (let i = 0; i < nb; i++) {
            const dx = x - cx[i];
            const dy = y - cy[i];
            const wgt = cstr[i] / (dx * dx + dy * dy + 22);
            tw += wgt;
            r += cr[i] * wgt;
            g += cg[i] * wgt;
            b += cbb[i] * wgt;
          }
          const inv = 1 / (tw || 1);
          r *= inv;
          g *= inv;
          b *= inv;
          // Threshold → lava blobs on black. A hot field makes blobs more
          // viscous (threshold drops a touch), but the floor stays firmly black.
          const floor = 0.12 - Math.min(0.03, pump * 0.012);
          let glow = (tw - floor) * (3.0 + liveBass * 1.4 + Math.min(0.5, pump * 0.2));
          if (glow < 0) glow = 0;
          else if (glow > 1.5) glow = 1.5; // ceiling keeps merged cores colored, not white
          glow += 0.04; // faint nebula floor
          r *= glow;
          g *= glow;
          b *= glow;
          const dith = ((x + y) & 1) * 8;
          r -= dith;
          g -= dith;
          b -= dith;
          pixels[px4] = r < 0 ? 0 : r > 255 ? 255 : r;
          pixels[px4 + 1] = g < 0 ? 0 : g > 255 ? 255 : g;
          pixels[px4 + 2] = b < 0 ? 0 : b > 255 ? 255 : b;
          pixels[px4 + 3] = 255;
          px4 += 4;
        }
      }

      // ── the BEAT = channel-shift glitch (torn rows) IN the small buffer ──
      if (glitchAmt > 0.05) {
        const shift = 2 + ((glitchAmt * 8) | 0);
        const rows = (glitchAmt * h * 0.5) | 0;
        for (let n = 0; n < rows; n++) {
          const yy = (Math.random() * h) | 0;
          const base = yy * w * 4;
          for (let x = w - 1; x >= shift; x--) {
            const di = base + x * 4;
            const sr = base + (x - shift) * 4; // red lags left
            const sb = base + (x - (shift >> 1)) * 4; // blue lags half
            pixels[di] = pixels[sr];
            pixels[di + 2] = pixels[sb + 2];
          }
        }
      }

      // ── composite the field up to NATIVE resolution ──
      page(screen);
      paste(field, 0, 0, { width: screen.width, height: screen.height });

      // ── crisp native-res overlays: voice cores + tap rings ──
      const sx = screen.width / w;
      const sy = screen.height / h;
      for (let i = 0; i < charges.length; i++) {
        const c = charges[i];
        if (c.pulse < 0.04) continue; // only sounding voices get a crisp halo
        const hue = (c.hue + time * 20 + paletteShift) % 360;
        const [rr, gg, bb] = num.hslToRgb(hue, 100, 60);
        const cpx = c.x * sx;
        const cpy = c.y * sy;
        const rad = (10 + c.pulse * 34) * (0.6 + (c.fade ?? 1));
        ink(rr, gg, bb, 90 * c.pulse * (c.fade ?? 1)).circle(cpx, cpy, rad, true);
        ink(255, 255, 255, 180 * c.pulse * (c.fade ?? 1)).circle(cpx, cpy, 3 + c.pulse * 5, true);
      }

      // Tap rings — the poke's visible shock (engine-tracked bursts).
      for (const b of bursts) {
        const [rr, gg, bb] = num.hslToRgb(((b.hue % 360) + 360) % 360, 100, 62);
        ink(rr, gg, bb, 200 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
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

// Kill the held drone on the way out.
function leave() {
  drone?.kill?.(0.3);
  drone = null;
}

export { boot, sim, paint, act, leave };
