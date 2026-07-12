// lavabath, 26.07.11
// Self-running A/V metaballs field: orbiting inverse-square charges paint
// glowing blobs that merge & split (lava-lamp plasma). Vivid color cycles;
// a bass pulse + arpeggio drives motion, and each downbeat snaps a
// channel-shift glitch + palette flash. Loops seamlessly (charges orbit
// periodic paths). Raw screen.pixels per-pixel — pinned to 180x320.

// ── tuning ────────────────────────────────────────────────────────────
const BPM = 132;
const NCHARGES = 6;
// One bar of driving synth: a bass pulse + a bright arpeggio interleaved.
const BASS = ["c2", "c2", "g1", "a1"]; // downbeat bass pulse (per beat)
const ARP = ["c4", "e4", "g4", "b4", "c5", "g4", "e4", "d4"]; // eighths

// ── state ─────────────────────────────────────────────────────────────
let charges = []; // { baseAngle, orbitR, speed, phase, hue, x, y, energy }
let time = 0; // smooth animation clock (seconds-ish)
let beatCount = 0;
let beatStart = 0;
let beatProgress = 0; // 0..1 through current beat
let downbeatPulse = 0; // decays after each beat, spikes on bar starts
let glitchAmt = 0; // channel-shift glitch strength (decays)
let paletteShift = 0; // global hue rotation, snaps on the bar
let liveBass = 0; // smoothed subBass amplitude
let liveAmp = 0; // smoothed overall amplitude
let W = 180;
let H = 320;

function boot({ resolution }) {
  // Pin the logical grid: ~57K px keeps the N-charge x per-pixel loop smooth.
  resolution(W, H);
  charges = [];
  for (let i = 0; i < NCHARGES; i++) {
    charges.push({
      // Periodic orbits → seamless loop. Each charge on its own ellipse.
      cx: W * (0.3 + 0.4 * ((i % 3) / 2)),
      cy: H * (0.22 + 0.56 * ((i % 2))),
      orbitRX: W * (0.18 + 0.1 * ((i * 7) % 5) / 5),
      orbitRY: H * (0.12 + 0.09 * ((i * 3) % 4) / 4),
      speed: 0.4 + 0.22 * ((i % 4)), // integer-ish ratios → clean loop
      phase: (i / NCHARGES) * Math.PI * 2,
      hue: (i / NCHARGES) * 360,
      x: W / 2,
      y: H / 2,
      strength: 130 + i * 30,
    });
  }
}

// Schedule the groove on the metronome — audio locks to the beat grid.
function beat({ sound: { bpm, synth, time: t } }) {
  bpm(BPM);
  const bar = beatCount % 4;

  // Bass pulse — one punchy note per beat.
  synth({
    tone: BASS[bar],
    type: "sawtooth",
    beats: 0.9,
    attack: 0.005,
    decay: 0.55,
    volume: 0.62,
    pan: 0,
  });
  // Sub sine reinforcement so the low end reads on phone speakers.
  synth({
    tone: BASS[bar],
    type: "sine",
    beats: 0.6,
    attack: 0.005,
    decay: 0.5,
    volume: 0.5,
  });

  // Two arpeggio eighths per beat → a running bright line.
  const a0 = ARP[(beatCount * 2) % ARP.length];
  const a1 = ARP[(beatCount * 2 + 1) % ARP.length];
  synth({ tone: a0, type: "triangle", beats: 0.45, attack: 0.005, decay: 0.4, volume: 0.34, pan: -0.4 });
  synth({ tone: a1, type: "square", beats: 0.45, attack: 0.005, decay: 0.35, volume: 0.26, pan: 0.4 });

  // Hat on the offbeat (noise → TYPE, never tone).
  synth({ type: "noise-white", tone: 1200, beats: 0.12, attack: 0.001, decay: 0.2, volume: 0.18 });

  downbeatPulse = 1;
  if (bar === 0) {
    glitchAmt = 1; // bar start → hard channel-shift glitch
    paletteShift = (paletteShift + 47) % 360; // snap the palette
  }

  beatCount = (beatCount + 1) % 8; // loop the score forever
  beatStart = t;
  beatProgress = 0;
}

function sim({ sound: { speaker, time: t, bpm } }) {
  speaker?.poll(); // mandatory before reading audio
  beatProgress = (t - beatStart) / (60 / bpm()) || 0;

  // Read live audio → smooth it so motion breathes with the groove.
  const bands = speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const amp = speaker?.amplitudes?.left || 0;
  liveBass += (bass - liveBass) * 0.25;
  liveAmp += (amp - liveAmp) * 0.2;
  if (speaker?.beat?.detected) glitchAmt = Math.max(glitchAmt, speaker.beat.strength || 0.6);

  // Advance the animation clock; faster on the beat + with more energy.
  time += 0.02 + liveBass * 0.05 + downbeatPulse * 0.01;

  // Decay the transient pulses.
  downbeatPulse *= 0.9;
  glitchAmt *= 0.88;

  // Move charges on their periodic ellipses; audio jitters their energy.
  for (let i = 0; i < charges.length; i++) {
    const c = charges[i];
    const a = time * c.speed + c.phase;
    const kick = 1 + liveBass * 0.8 + downbeatPulse * 0.35;
    c.x = c.cx + Math.cos(a) * c.orbitRX * kick;
    c.y = c.cy + Math.sin(a * 1.3) * c.orbitRY * kick;
  }
}

function paint({ screen, num }) {
  const { width: w, height: h, pixels } = screen;
  const nb = charges.length;

  // Precompute per-charge values out of the inner loop.
  const cx = new Float32Array(nb);
  const cy = new Float32Array(nb);
  const cr = new Float32Array(nb);
  const cg = new Float32Array(nb);
  const cb = new Float32Array(nb);
  const cs = new Float32Array(nb);
  const beatBoost = 1 + downbeatPulse * 1.6 + liveBass * 1.2;
  for (let i = 0; i < nb; i++) {
    const c = charges[i];
    cx[i] = c.x;
    cy[i] = c.y;
    // Cycle hue over time + palette snap → vivid drifting color.
    const hue = (c.hue + time * 34 + paletteShift) % 360;
    const rgb = num.hslToRgb(hue, 100, 55); // returns 0-255 already
    cr[i] = rgb[0];
    cg[i] = rgb[1];
    cb[i] = rgb[2];
    cs[i] = c.strength * beatBoost; // brighter/hotter on the beat
  }

  // ── raw per-pixel inverse-square field (squared distance, no sqrt) ──
  let idx = 0;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      let tw = 0;
      let r = 0;
      let g = 0;
      let b = 0;
      for (let i = 0; i < nb; i++) {
        const dx = x - cx[i];
        const dy = y - cy[i];
        // Inverse-square falloff. Scaled strength → bright glowing cores.
        const wgt = cs[i] / (dx * dx + dy * dy + 22);
        tw += wgt;
        r += cr[i] * wgt;
        g += cg[i] * wgt;
        b += cb[i] * wgt;
      }
      // Normalize by total weight → smooth blends where blobs merge.
      const inv = 1 / tw;
      r *= inv;
      g *= inv;
      b *= inv;
      // Field intensity → brightness envelope: dark background, hot cores.
      // Threshold the field so only near-core regions light up (lava blobs on
      // black). tw is tiny between charges, spikes near them.
      let glow = (tw - 0.12) * 3.2;
      if (glow < 0) glow = 0;
      else if (glow > 1.7) glow = 1.7;
      // Faint nebula floor so background isn't dead black.
      glow += 0.04;
      r *= glow;
      g *= glow;
      b *= glow;
      // 2x2 ordered dither → retro grain, kills banding.
      const dith = ((x + y) & 1) * 8;
      r -= dith;
      g -= dith;
      b -= dith;
      pixels[idx] = r < 0 ? 0 : r > 255 ? 255 : r;
      pixels[idx + 1] = g < 0 ? 0 : g > 255 ? 255 : g;
      pixels[idx + 2] = b < 0 ? 0 : b > 255 ? 255 : b;
      pixels[idx + 3] = 255;
      idx += 4;
    }
  }

  // ── beat glitch: horizontal RGB channel-shift on scattered rows ──
  if (glitchAmt > 0.05) {
    const shift = 2 + ((glitchAmt * 10) | 0); // px offset, grows with strength
    const rows = (glitchAmt * h * 0.5) | 0; // how many rows get torn
    for (let n = 0; n < rows; n++) {
      const yy = (Math.random() * h) | 0;
      const base = yy * w * 4;
      for (let x = w - 1; x >= shift; x--) {
        const di = base + x * 4;
        const sr = base + (x - shift) * 4; // red lags left
        const sb = base + (x - (shift >> 1)) * 4; // blue lags half
        pixels[di] = pixels[sr]; // shifted red channel
        pixels[di + 2] = pixels[sb + 2]; // shifted blue channel
      }
    }
  }
}

export { boot, beat, sim, paint };
