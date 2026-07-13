// vroon, 26.07.12
// Warp-tunnel drone — a thin wrapper over lib/pads.mjs (the shared pad engine:
// UTC-clock beat grid, `params[0]` rate override e.g. `vroon 0.5`, the tap/XY
// "pump", audio polling). This file only describes what makes vroon vroon: a big
// SWEEPING saw/PWM DRONE that rises + falls ("vrooon") over a deep sub, plus the
// transform-feedback warp tunnel it paints.
//
// SWEEP = TUNNEL SPEED. The sweep is the star: as it rises in pitch the tunnel of
// radial light-streaks rushes FASTER outward and the hue shifts up (low sweep =
// slow warm rush, high sweep = fast bright rush). The sub is the tunnel's core
// glow; accents/taps blast shockwave rings. You literally SEE the sweep as
// acceleration through a warp tunnel. The sweep PHASE is derived from ctx.simMs
// (the engine's guarded UTC ms) so two instances warp in lock-step.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score / timing ---------------------------------------------------------
const BPM = 128; // brisk, propulsive
const BEAT_MS = 60000 / BPM; // nominal ms/beat (visual sweep window; audio grid = engine's beatSeconds)
// The SWEEP cycles over a whole musical bar (4 beats) so it loops seamlessly:
// phase 0 == the seam. One full "vrooon" per bar. Multiple of a beat → phase-locked.
const SWEEP_MS = BEAT_MS * 4; // ms per full sweep rise+fall

// Accent stabs (one bar, 8 steps). Root motion under the sweep. Step 0 = seam.
const STABS = ["e1", "e1", "e2", "e1", "c2", "c2", "g1", "b1"];
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 36;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}

// --- vroon-specific state (the engine owns pump/bursts/rhythm) ---------------
let streaks = []; // radial light-streaks rushing outward { ang, dist, len, hue, life }
let rings = []; // shockwave rings from stabs + taps { x, y, r, life, hue, thick }
let coreGlow = 0; // central tunnel-core glow, kicked by the sub, decays
let spawnAcc = 0; // fractional accumulator so streak spawn rate ∝ speed

let sweep = 0; // 0..1 the sweep's rise/fall value (derived from ctx.simMs each sim)
let sweepTarget = 0.5; // tap-set bias the sweep drifts toward (Y of last tap)

// --- The sustained drone voices (built once, mutated live every sim) --------
let droneSaw = null; // the sweeping saw — the "vrooon"
let droneSaw2 = null; // a detuned saw partner for a fuller, cinematic body
let dronePwm = null; // a square layer for PWM-ish grit
let subVoice = null; // the deep sub under it all
let started = false; // guard so the drone builds exactly once
let lastPan = 0; // last tap's pan, biases the drone pan on a rev

// Build the sustained sweeping drone + sub once (AC is silent without a voice).
function startDrone(synth) {
  if (started) return;
  started = true;
  // Layered detuned saws → a thick, cinematic "vrooon" that we live-sweep.
  droneSaw = synth({ tone: "e2", type: "sawtooth", duration: "🔁",
    attack: 0.4, decay: 0.9, volume: 0.34, pan: -0.12 });
  droneSaw2 = synth({ tone: "e2", type: "sawtooth", duration: "🔁",
    attack: 0.5, decay: 0.9, volume: 0.24, pan: 0.12 });
  dronePwm = synth({ tone: "e3", type: "square", duration: "🔁",
    attack: 0.4, decay: 0.9, volume: 0.14, pan: 0 });
  subVoice = synth({ tone: "e1", type: "sine", duration: "🔁",
    attack: 0.5, decay: 0.9, volume: 0.5, pan: 0 });
}

const CONFIG = {
  bpm: BPM,
  steps: STABS.length,
  drawBursts: false, // vroon draws its own shockwave rings in onPaint
  hooks: {
    onBoot({ sound }) {
      // Cavernous tail so the warp feels deep + endless.
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.62 });
    },

    // A new UTC beat crossed — fire the accent stab + blast a shockwave ring.
    onBeat({ idx, synth, screen }) {
      const s = ((idx % STABS.length) + STABS.length) % STABS.length;
      const note = STABS[s];
      const pan = Math.sin((s / STABS.length) * Math.PI * 2) * 0.5;

      // Accent stab — a plucked saw-ish attack keyed off idx.
      voices.pluck(synth, note, { beats: 0.4, decay: 0.45, volume: 0.4, pan });
      // A crisp tick keeps the grid legible (noise → type, NOT tone).
      voices.hat(synth, { tone: 6000, beats: 0.1, volume: 0.13 });

      // Sub thumps on the downbeats → drives the tunnel CORE GLOW.
      if (s % 2 === 0) {
        voices.sub(synth, note, { beats: 1.2, decay: 0.5, volume: 0.4 });
        coreGlow = 1;
      }
      // High steps shimmer a bell octave-up for cinematic sparkle.
      if (s % 4 === 2) voices.bell(synth, note, { beats: 0.5, volume: 0.14, pan: -pan });

      // ALLEGORY: the stab BLASTS a shockwave ring from the vanishing point.
      const cx = screen.width / 2, cy = screen.height / 2;
      rings.push({ x: cx, y: cy, r: Math.min(screen.width, screen.height) * 0.05,
        life: 1, hue: 200 + (notePitch(note) % 12) * 10, thick: 3 });
    },

    // Live-update the sweep drone + advance eased visual energies. The sweep
    // PHASE derives from ctx.simMs (guarded UTC ms) so it stays UTC-phase-locked.
    onSim(ctx) {
      const { simMs, pump } = ctx;
      startDrone(ctx.sound.synth); // build the sustained drone on the first tick

      // --- THE SWEEP (the star): a UTC-phase-locked rise+fall, 0..1. Absolute
      // from epoch → every instance computes the identical sweep at the same
      // wall ms. SWEEP_MS is a multiple of a beat so phase 0 lands on the seam. ---
      const sweepPhase = ((simMs % SWEEP_MS) + SWEEP_MS) % SWEEP_MS / SWEEP_MS; // 0..1
      const raw = 0.5 - 0.5 * Math.cos(sweepPhase * Math.PI * 2); // 0→1→0 smooth
      sweep = Math.min(1, raw * 0.7 + sweepTarget * 0.3 + pump * 0.12);

      // --- Steer the sustained drone LIVE from the sweep. The saws rise across
      // ~1.5 octaves of E (the audible "vrooon"); PWM brightens; volume swells at
      // the peak. This is the sweep you HEAR that you also SEE as tunnel speed. ---
      if (droneSaw) {
        const semis = sweep * 18; // 0..18 semitones above E2
        const sawHz = 82.41 * Math.pow(2, semis / 12); // E2 = 82.41 Hz
        droneSaw.update?.({ tone: sawHz, volume: 0.28 + sweep * 0.16, pan: -0.12 + lastPan * 0.3 });
        droneSaw2?.update?.({ tone: sawHz * 1.006, volume: 0.2 + sweep * 0.12, pan: 0.12 + lastPan * 0.3 }); // detune partner
        dronePwm?.update?.({ tone: sawHz * 2, volume: 0.08 + sweep * 0.13, pan: -lastPan * 0.4 });
        subVoice?.update?.({ volume: 0.42 + coreGlow * 0.14 });
      }

      // --- Spawn radial streaks at a rate ∝ warp SPEED (sweep + pump). Faster
      // sweep → more streaks born per tick → the tunnel visibly RUSHES faster. ---
      const speed = 0.5 + sweep * 2.5 + pump * 1.2;
      spawnAcc += speed * 2.2; // dense streaks — the tunnel is MADE of them
      while (spawnAcc >= 1) {
        spawnAcc -= 1;
        streaks.push({
          ang: Math.random() * Math.PI * 2,
          dist: 0.02 + Math.random() * 0.04, // born near the vanishing point
          len: 0,
          hue: 200 + sweep * 130, // low sweep = warm blue/violet, high = bright cyan
          life: 1,
        });
      }

      // --- Advance streaks OUTWARD at the warp speed (the rush you read as sweep). ---
      for (const st of streaks) {
        st.dist *= 1 + 0.05 + speed * 0.03; // exponential rush outward (perspective)
        st.len = Math.min(0.25, st.len + 0.01 + speed * 0.004);
        st.life -= 0.02 + speed * 0.004;
      }
      streaks = streaks.filter((st) => st.life > 0 && st.dist < 1.4);
      if (streaks.length > 420) streaks = streaks.slice(-420); // safety cap

      // --- Advance shockwave rings ---
      for (const r of rings) {
        r.r += 5 + r.life * 12 + pump * 4;
        r.life -= 0.03;
      }
      rings = rings.filter((r) => r.life > 0);

      coreGlow *= 0.9;
    },

    // Tap = blast a shockwave ring from the tap point + a drone REV (engine
    // already bumped pump + pushed a burst). X→pan/hue, Y→pitch/sweep target.
    onTap({ x, y, ex, ey, synth }) {
      sweepTarget = 1 - y; // top = high/fast, bottom = low/slow
      lastPan = x * 2 - 1;

      // SHOCKWAVE RING blasting outward FROM the tap point.
      rings.push({ x: ex, y: ey, r: 4, life: 1.2, hue: 200 + x * 160, thick: 4 });
      // A burst of streaks radiating from the tap → a visible rev.
      for (let i = 0; i < 10; i++) {
        streaks.push({ ang: Math.random() * Math.PI * 2,
          dist: 0.03 + Math.random() * 0.05, len: 0, hue: 200 + x * 160, life: 1.2 });
      }

      // SONIC BOOST — a REV of the drone. X→pan, Y→pitch/brightness.
      const note = ["e", "g", "a", "b", "d"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 3));
      voices.pluck(synth, note, { beats: 0.6, decay: 0.55, volume: 0.5, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.3, volume: 0.24 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num, zoom, blur } = api;
      const { pump, amp, band, quality = 1 } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;
      const maxR = Math.max(w, h) * 0.72; // tunnel reaches past the corners

      const sub = band("subBass");

      // Warp SPEED (matches onSim's read so the picture tracks the sweep).
      const speed = 0.5 + sweep * 2.5 + pump * 1.2;

      // --- TRANSFORM-FEEDBACK TUNNEL: zoom the previous frame OUTWARD from the
      // vanishing point every frame — faster sweep = stronger zoom = the frame's
      // light rushes past you faster. A balanced veil recycles it into an endless
      // tunnel without blowing out to white. The sweep (which you HEAR rising)
      // directly sets the zoom (the speed you SEE). ---
      const zoomAmt = 1.03 + sweep * 0.06 + pump * 0.04; // >1 → outward rush
      zoom?.(zoomAmt, 0.5, 0.5);
      // Deep-space veil — alpha tuned so streaks smear into a legible tunnel but
      // the frame keeps decaying (never accumulates to white).
      ink(4, 3, 14, 150).box(0, 0, w, h);

      // --- CORE GLOW: the sub = the tunnel's core light at the vanishing point.
      // A small hot pinpoint + a tight soft halo — kept small so the STREAKS star. ---
      const glow = Math.max(coreGlow, sub * 1.4);
      const coreR = Math.min(w, h) * (0.012 + glow * 0.05 + sweep * 0.02);
      for (let i = 3; i > 0; i--) {
        const t = i / 3;
        ink(150, 180, 255, 22 * glow + 8).circle(cx, cy, coreR * t * 2.4, true);
      }
      ink(235, 248, 255, 220).circle(cx, cy, coreR, true); // the hot pinpoint core

      // --- RADIAL STREAKS rushing outward. Each is a line segment from its inner
      // point toward the rim; length + brightness ∝ amp and speed. Hue climbs with
      // the sweep (warm→bright). Together they read as a warp tunnel accelerating. ---
      // ADAPTIVE COST: the streak loop is vroon's heaviest work (per-streak
      // hslToRgb + line + circle). Scale how many we draw by ctx.quality — at
      // quality=1 we draw the whole live population (identical look); when the
      // engine cuts quality we thin the herd (drop the OLDEST/faintest first, so
      // the freshest streaks near the core survive). Cap keeps the array ≤420.
      const drawCount = Math.max(8, Math.round(streaks.length * quality));
      const streakStart = streaks.length - drawCount; // skip the oldest when thinned
      // The comet-TIP circle is a second ink() per streak (doubles the streak op
      // count). It's an extra sparkle pass, not the tunnel itself, so drop it when
      // the engine has cut quality — the line (the streak's body) always draws. At
      // quality=1 (drawTips true) every tip renders exactly as before.
      const drawTips = quality >= 0.6;
      for (let si = streakStart; si < streaks.length; si++) {
        const st = streaks[si];
        const d0 = st.dist;
        const d1 = Math.min(1.4, st.dist + st.len * (1 + speed * 0.3));
        const x0 = cx + Math.cos(st.ang) * d0 * maxR;
        const y0 = cy + Math.sin(st.ang) * d0 * maxR;
        const x1 = cx + Math.cos(st.ang) * d1 * maxR;
        const y1 = cy + Math.sin(st.ang) * d1 * maxR;
        const hue = ((st.hue + sweep * 40) % 360 + 360) % 360;
        const [r0, g0, b0] = num.hslToRgb(hue, 95, Math.min(100, 58 + sweep * 22 + amp * 15));
        const a = 235 * st.life * (0.75 + amp * 0.8 + sweep * 0.4);
        const thick = 1.2 + st.dist * (2.2 + speed * 0.6);
        ink(r0, g0, b0, Math.min(255, a)).line(x0, y0, x1, y1, thick);
        // A brighter leading tip near the outer end for a comet feel.
        if (drawTips)
          ink(Math.min(255, r0 + 80), Math.min(255, g0 + 80), 255,
            Math.min(255, 220 * st.life)).circle(x1, y1, 1 + st.dist * 2.5, true);
      }

      // --- SHOCKWAVE RINGS (stabs + taps blast expanding rings outward). ---
      for (const r of rings) {
        const hue = ((r.hue % 360) + 360) % 360;
        const [rr, gg, bb] = num.hslToRgb(hue, 95, 62);
        ink(rr, gg, bb, 190 * r.life).circle(r.x, r.y, r.r, false, r.thick * r.life + 1);
        ink(255, 255, 255, 120 * r.life).circle(r.x, r.y, r.r * 0.6, false, 1);
      }

      // --- Bloom on loud peaks so the tunnel feels like it's punching through.
      // The blur is a full-frame extra pass — the priciest single op here — so
      // skip it when the engine has cut quality (< 0.6). Full at quality=1. ---
      if (quality >= 0.6 && (amp > 0.4 || pump > 1)) blur?.(1);
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

// Kill the held drone when leaving so it doesn't sustain into the next piece.
function leave() {
  droneSaw?.kill?.();
  droneSaw2?.kill?.();
  dronePwm?.kill?.();
  subVoice?.kill?.();
  droneSaw = droneSaw2 = dronePwm = subVoice = null;
  started = false;
}

export { boot, sim, paint, act, leave };
