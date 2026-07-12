// wobbo, 26.07.12
// Self-running WOBBLE-BASS OSCILLOSCOPE instrument — you literally SEE the bass.
// A fat detuned wobble bass whose timbre is pumped by an LFO, a punchy grid kick,
// and off-beat stabs. The CENTER of the screen is one big rubbery WAVEFORM line
// driven by the live audio; the wobble LFO visibly stretches/squashes the line's
// amplitude in time with the bass → the membrane bulges as the bass wobbles.
// The KICK punches a shockwave outward across the membrane; STABS flash bright
// spikes on the line; bass energy = the membrane's girth + neon glow.
//
// FOUR PRINCIPLES (marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res (vector polyline, no buffer).
//   2. WHOLE PIECE IS A BUTTON — tap / XY-drag PUNCHES the membrane at the tap
//      point (a bulge + a bass boom, pitch by Y, pan by X); repeated taps
//      accumulate decaying `pump` that fattens the wobble + brightens the line.
//   3. UTC-SYNCED RHYTHM — the wobble LFO PHASE + kick + stabs are all derived
//      from clock.time() in sim (NOT beat()), indexed by floor(globalBeat) % len,
//      so two instances opened anywhere wobble in perfect lock-step.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — the oscilloscope line IS the score.
//
// BPM ~140, half-time dubstep feel. Loops seamlessly (score seams every 8 steps).

// --- Score ------------------------------------------------------------------
const BPM = 140;
const BEAT_MS = 60000 / BPM; // ms per beat — the UTC grid unit
const WOBBLE_RATE = 2; // LFO cycles per beat (the wobble you SEE + HEAR)

// One looping bar (8 steps). Root notes for the wobble bass; step 0 == the seam.
const BASS_NOTES = ["e1", "e1", "g1", "e1", "a1", "a1", "d1", "b1"];
// Off-beat stab notes (only fire on certain steps) — bright rubbery accents.
const STAB_NOTES = [null, "e3", null, "b3", null, "g3", null, "d3"];
const KICK_STEPS = [0, 2, 4, 6]; // punchy four-on-the-grid kick (half-time feel)

// --- Rhythm state (UTC-driven) ---------------------------------------------
let lastBeat = -1; // last integer global-beat we fired
let step = 0; // current step index (0..7), from UTC
let beatProgress = 0; // 0..1 within the current beat (smooth visuals)
let wobblePhase = 0; // 0..1 LFO phase within a wobble cycle (UTC-derived)
let simTime = 0; // last UTC ms

// --- Voices ----------------------------------------------------------------
let bassA = null; // detuned saw voice A (sustained wobble bass)
let bassB = null; // detuned saw voice B
let bassSub = null; // clean sub sine under it

// --- Allegory state ---------------------------------------------------------
let kickShock = 0; // shockwave punch on the membrane, kicked by kicks, decays
let stabFlash = 0; // bright spike flash, kicked by stabs, decays
let bassBloom = 0; // membrane girth/glow, driven by bass energy, decays
let wobbleAmt = 0; // eased wobble depth for the visual squash

// --- Button / pump state (§2) ----------------------------------------------
let pump = 0; // decaying tap energy, 0..~3 → fattens wobble + brightens line
let bulges = []; // tap-spawned membrane bulges { x, y, r, life, hue, punch }

function boot({ sound, clock }) {
  sound.bpm(BPM); // only the internal metronome; the UTC scheduler is boss
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  sound.room?.set?.({ enabled: true, mix: 0.22, feedback: 0.5 }); // rubbery tail

  // Autostart the sustained wobble-bass stack (detuned saws + clean sub) so the
  // reel has sound from frame 0. Their tone/volume are wobbled live in sim.
  bassA = sound.synth({ tone: "e1", type: "sawtooth", duration: "🔁", attack: 0.02, decay: 0.9, volume: 0.0 });
  bassB = sound.synth({ tone: "e1", type: "sawtooth", duration: "🔁", attack: 0.02, decay: 0.9, volume: 0.0, pan: 0.1 });
  bassSub = sound.synth({ tone: "e1", type: "sine", duration: "🔁", attack: 0.02, decay: 0.9, volume: 0.0 });
}

// Fire one step of the score — called the instant a new global beat crosses in
// sim, so every onset is UTC-aligned and every sonic event spawns its visual.
function fireStep(idx, synth) {
  const s = ((idx % BASS_NOTES.length) + BASS_NOTES.length) % BASS_NOTES.length;
  step = s;

  // Retarget the sustained wobble bass to this step's root (voice stays alive).
  const root = BASS_NOTES[s];
  bassA?.update?.({ tone: root });
  bassB?.update?.({ tone: root });
  bassSub?.update?.({ tone: root });

  // KICK — a punchy grid thump that PUNCHES the membrane outward (§4).
  if (KICK_STEPS.includes(s)) {
    synth({ tone: "c1", type: "sine", beats: 0.5, attack: 0.001, decay: 0.28, volume: 0.6 });
    synth({ type: "noise-white", tone: 200, beats: 0.05, attack: 0.001, decay: 0.1, volume: 0.35 }); // click
    kickShock = 1; // ALLEGORY: kick = shockwave across the membrane
  }

  // STAB — a bright off-beat rubbery accent → a spike flash on the line (§4).
  const stab = STAB_NOTES[s];
  if (stab) {
    const pan = Math.sin((s / BASS_NOTES.length) * Math.PI * 2) * 0.6;
    synth({ tone: stab, type: "square", beats: 0.35, attack: 0.003, decay: 0.4, volume: 0.28, pan });
    synth({ tone: stab, type: "triangle", beats: 0.2, attack: 0.002, decay: 0.3, volume: 0.16, pan: -pan });
    stabFlash = 1; // ALLEGORY: stab = bright spike flash on the line
  }

  bassBloom = 1; // ALLEGORY: each step re-energizes the membrane girth/glow
}

// Drive audio + rhythm + the wobble LFO from UTC in sim (NOT beat()) so onsets
// and the wobble itself align across instances. Also decay eased visual energy.
function sim({ sound: { speaker, synth }, clock, num }) {
  speaker?.poll(); // mandatory before reading audio

  // clock.time() can return an *Invalid Date* before UTC sync completes (its
  // getTime() is NaN, and it's not null so `??` won't catch it) — guard with
  // Date.now() so the rhythm + wobble always run, locking once synced.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();
  simTime = ms;

  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1 through the current beat

  // The WOBBLE LFO — deterministic from UTC time so two instances wobble in
  // perfect sync. Phase runs WOBBLE_RATE cycles per beat.
  wobblePhase = (globalBeat * WOBBLE_RATE) % 1;
  const lfo = 0.5 + 0.5 * Math.sin(wobblePhase * Math.PI * 2); // 0..1 wobble

  if (idx !== lastBeat) {
    lastBeat = idx;
    fireStep(idx, synth); // a new UTC beat crossed → fire its step
  }

  // WOBBLE the sustained bass: LFO modulates volume + a slight detune spread so
  // you HEAR the filter-ish wobble. Pump fattens it; bloom keeps it present.
  const drive = 0.32 + pump * 0.14;
  const wob = 0.28 + lfo * 0.72; // never fully silent → rubbery, not gated
  const vA = drive * wob;
  bassA?.update?.({ volume: vA });
  bassB?.update?.({ volume: vA * 0.9, pan: (lfo - 0.5) * 0.5 }); // detune-pan sway
  bassSub?.update?.({ volume: (0.22 + pump * 0.05) * (0.6 + lfo * 0.4) });

  // Eased wobble depth for the VISUAL squash (so the membrane bulge matches).
  wobbleAmt = num.lerp(wobbleAmt, lfo, 0.4);

  // Decays (per 120 Hz tick).
  pump *= 0.965;
  kickShock *= 0.86;
  stabFlash *= 0.8;
  bassBloom *= 0.94;

  // Advance / cull tap bulges.
  for (const b of bulges) {
    b.r += 4 + b.life * 8;
    b.life -= 0.04;
  }
  bulges = bulges.filter((b) => b.life > 0);
}

// THE WHOLE PIECE IS A BUTTON (§2): any tap / XY-drag PUNCHES the membrane at the
// tap point — a bulge + a bass boom whose pitch is set by Y and pan by X.
function act({ event: e, sound: { synth }, screen }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width; // 0..1
    const y = e.y / screen.height; // 0..1
    // Taps punch, drags feed — accumulate decaying pump energy (fattens wobble).
    pump = Math.min(3, pump + (e.is("draw") ? 0.1 : 0.9));

    // A membrane bulge at the tap point, hue by X, punch by tap-vs-drag.
    bulges.push({ x: e.x, y: e.y, r: 0, life: 1, hue: x * 360, punch: e.is("draw") ? 0.4 : 1 });
    kickShock = Math.min(1.2, kickShock + (e.is("draw") ? 0.2 : 0.7));

    // SONIC BOOST — a bass boom: pitch by Y (top = higher), pan by X.
    const note = ["e1", "g1", "a1", "b1", "d2"][Math.floor((1 - y) * 5)] || "e1";
    synth({ tone: note, type: "sawtooth", beats: 0.6, attack: 0.005, decay: 0.5, volume: 0.5, pan: x * 2 - 1 });
    synth({ tone: note, type: "sine", beats: 0.5, attack: 0.002, decay: 0.4, volume: 0.35, pan: x * 2 - 1 });
    // A bright rubbery tip an octave up, brighter higher on the screen.
    synth({ tone: ["e3", "g3", "a3", "b3", "d4"][Math.floor(x * 5)] || "e3",
      type: "square", beats: 0.25, attack: 0.002, decay: 0.3, volume: 0.2 * (1 - y * 0.6), pan: x * 2 - 1 });
    stabFlash = 1;
  }
}

function paint({ ink, box, line, circle, screen, sound, num, help, paintCount }) {
  const { width: w, height: h } = screen;
  const cx = w / 2, cy = h / 2;

  // --- Live audio reads -----------------------------------------------------
  const bands = sound.speaker?.frequencies?.left || [];
  const bandAmp = (name) => bands.find((b) => b.name === name)?.amplitude || 0;
  const bass = bandAmp("subBass");
  const lowMid = bandAmp("lowMid");
  const air = bandAmp("air");
  const amp = sound.speaker?.amplitudes?.left || 0;
  const wf = sound.speaker?.waveforms?.left || [];

  // Combined "energy" — bass + wobble LFO + tap pump. Drives girth + glow.
  const energy = Math.min(2, bass * 1.4 + amp * 0.6 + pump * 0.5 + bassBloom * 0.4);

  // --- Trail veil (near-black) → glowing neon trails on the line ------------
  // Slightly translucent so the membrane leaves a rubbery ghost behind it.
  ink(4, 2, 10, 150).box(0, 0, w, h);

  // --- The WOBBLE OSCILLOSCOPE MEMBRANE (the graphic score, §4) -------------
  // A thick glowing polyline across the full width. Its base amplitude is the
  // live waveform; the wobble LFO (wobbleAmt) stretches/squashes that amplitude
  // in time → you SEE the bass wobble as the membrane bulging. The kick punches
  // a travelling shockwave; the tap bulges deform it locally.
  const N = 128; // points across the membrane
  const half = h * 0.32; // vertical reach of the line
  // Wobble envelope: 0.35..1.0, pumped by taps → the visible squash.
  const wobEnv = 0.35 + wobbleAmt * 0.65 * (1 + pump * 0.3);
  // Girth/glow of the line, driven by bass energy.
  const girth = 2 + energy * 6 + bassBloom * 4;

  // Precompute waveform samples fit to the line width.
  const scope = wf.length ? help.resampleArray(wf, N) : null;

  // Build the membrane points.
  const pts = [];
  const tphase = (paintCount % 240) / 240; // wrapping phase (seamless drift)
  for (let i = 0; i < N; i++) {
    const fx = i / (N - 1); // 0..1 across
    const x = fx * w;

    // Base: live waveform (or a gentle idle sine so it's never dead).
    let v = scope ? scope[i] : Math.sin(fx * Math.PI * 6 + tphase * Math.PI * 2) * 0.2;

    // WOBBLE: a slow spatial sine tied to the LFO phase bulges the whole line,
    // squashing/stretching its amplitude in time with the wobble you HEAR.
    const spatial = Math.sin(fx * Math.PI * 2 + wobblePhase * Math.PI * 2);
    v *= wobEnv; // amplitude breathes with the wobble
    v += spatial * 0.14 * wobbleAmt * (0.6 + energy * 0.7); // rubbery membrane sway

    // KICK shockwave: a gaussian pulse travelling outward from center punches
    // the membrane at kick time.
    if (kickShock > 0.01) {
      const front = kickShock; // 1 → center, 0 → edges
      const d = Math.abs(fx - 0.5) * 2; // 0 center .. 1 edge
      const near = Math.exp(-((d - (1 - front)) ** 2) / 0.02);
      v += near * kickShock * 0.55; // outward punch travelling from center
    }

    // TAP bulges: local gaussian deformation at each active bulge's x.
    for (const b of bulges) {
      const bfx = b.x / w;
      const d = Math.abs(fx - bfx);
      const g = Math.exp(-(d * d) / 0.004);
      v += g * b.life * b.punch * 0.7 * ((b.y < cy) ? -1 : 1);
    }

    const y = cy + v * half;
    pts.push([x, y]);
  }

  // Draw the membrane: a fat neon-magenta core with an acid-green glow halo.
  // Girth + brightness scale with bass energy so louder = fatter + brighter.
  const glowA = Math.min(220, 60 + energy * 120);
  // Halo pass (acid green), thick, translucent → the glow.
  for (let g = 0; g < N - 1; g++) {
    ink(80, 255, 120, glowA * 0.35).line(pts[g][0], pts[g][1], pts[g + 1][0], pts[g + 1][1], girth + 5);
  }
  // Core pass (neon magenta / hot pink), tight + bright.
  const [cr, cg, cb] = num.hslToRgb((310 + air * 40 + wobbleAmt * 20) % 360, 90, 62);
  for (let g = 0; g < N - 1; g++) {
    ink(cr, cg, cb, 235).line(pts[g][0], pts[g][1], pts[g + 1][0], pts[g + 1][1], Math.max(1, girth * 0.5));
  }
  // Bright white crest on stabs → the spike flash.
  if (stabFlash > 0.05) {
    for (let g = 0; g < N - 1; g++) {
      ink(255, 255, 255, 200 * stabFlash).line(pts[g][0], pts[g][1], pts[g + 1][0], pts[g + 1][1], 1);
    }
  }

  // --- Bass girth bloom behind the line (the membrane's "body", §4) ---------
  if (bassBloom > 0.02 || bass > 0.04) {
    const bloom = Math.max(bassBloom, bass * 1.3);
    const bH = half * (0.4 + bloom * 0.6);
    ink(120, 40, 200, 22 * bloom).box(cx, cy, w, bH * 2, "*center");
  }

  // --- Kick shockwave rings from center (§4) --------------------------------
  if (kickShock > 0.03) {
    const kr = (1 - kickShock) * Math.max(w, h) * 0.7;
    ink(80, 255, 160, 160 * kickShock).circle(cx, cy, kr, false, 2 + kickShock * 4);
  }

  // --- Tap bulge rings at their punch points --------------------------------
  for (const b of bulges) {
    const [br, bg, bb] = num.hslToRgb(b.hue, 90, 60);
    ink(br, bg, bb, 180 * b.life).circle(b.x, b.y, b.r, false, 1 + b.life * 3);
  }

  // --- UTC wobble phase marker: a subtle pumping side-glow that breathes with
  // the LFO so the whole frame reads as "wobbling" even on a still glance. -----
  const sideA = 24 + wobbleAmt * 40;
  ink(80, 255, 120, sideA).box(0, cy, 6, half * 2 * wobEnv, "*center");
  ink(255, 40, 180, sideA).box(w - 3, cy, 6, half * 2 * wobEnv, "*center");
}

function leave() {
  bassA?.kill?.(0.3);
  bassB?.kill?.(0.3);
  bassSub?.kill?.(0.3);
}

export { boot, sim, act, paint, leave };
