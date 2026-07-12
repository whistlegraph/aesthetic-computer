// murmo, 26.07.12
// Self-running A/V loop: a MURMURING-SWARM instrument — a starling murmuration
// rendered as living sound. A low humming DRONE bed + a soft chorus of detuned
// mid "murmur" voices that swell and ebb + a slow breathing pulse drive a swarm
// of a few hundred lightweight agents flocking across the screen. Hypnotic,
// flocking, breathing. Zero input — autostarts on boot and loops seamlessly.
//
// FOUR PRINCIPLES (reel canon):
//   1. NO resolution() — native res; cheap vector agents (no per-pixel loop, no
//      buffer). A shared sine-driven flow field moves the whole flock, so it's
//      hundreds of dots but only a handful of trig calls per frame.
//   2. WHOLE PIECE IS A BUTTON — a tap/XY-drag scatters/ignites the swarm from
//      the tap point: a shock that ripples outward through the flock + a swelling
//      murmur voice (X→pan/hue, Y→pitch). Repeated taps accumulate a decaying
//      `pump` that agitates the whole murmuration.
//   3. UTC-SYNCED RHYTHM — the murmur-voice ignitions + the breath pulse are
//      derived from clock.time() in sim() (NOT beat()), indexed by
//      floor(globalBeat) % len, so two instances anywhere auto-align. BPM 84,
//      seamless 8-beat cycle.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY —
//        • the low DRONE = the big slow collective UNDULATION of the whole flock
//          (the flock's overall wave motion is driven by the drone bed's phase);
//        • each mid VOICE that sounds = a cluster of the swarm IGNITING BRIGHT at
//          a height ∝ its pitch, HUE = pitch → you SEE the harmony as glowing
//          bands rippling through the flock;
//        • the PULSE = the whole swarm contracting/expanding (a breath);
//        • amplitude → flock density/brightness.
//      A viewer reads the drone as the flock's motion and the voices as bright
//      pitch-height bands.

// —— Harmony ——
// A slow murmuring chorus. One mid "voice" ignites per beat over an 8-beat cycle
// that resolves back to the start so the audio loops as cleanly as the visuals.
// A stack of pentatonic tones — soft, always-agreeing, twilight-warm.
const VOICES = ["a3", "c4", "e4", "d4", "g4", "e4", "c4", "d4"];
// The drone bed: two low detuned sines held forever (the flock's collective body).
const DRONE = ["a1", "a2"]; // root + octave, gently detuned for a beating hum

const BPM = 84;
const BEAT_MS = 60000 / BPM; // ms per beat
const LOOP = VOICES.length; // 8-beat cycle

// —— UTC rhythm state (driven in sim, never beat) ——
let lastBeat = -1; // last integer global beat we fired
let beatCount = 0; // current step within the loop (0..LOOP-1)
let beatProgress = 0; // 0..1 through the current beat (smooth visuals)
let globalCycle = 0; // (globalBeat / LOOP) continuous — for seamless phase
let droneStarted = false; // held drone bed created once

// —— live audio voices ——
let drone = []; // sustained drone voices (the flock's collective body)

// —— the swarm ——
// Each agent is a lightweight dot advected by a SHARED sine flow field (cheap).
// It carries a little home-phase so the flock spreads across the frame and a
// per-agent "band" position so voice ignitions can light up a height slice.
const AGENTS = 320; // a few hundred — reasonable for smooth fps at density 3
let agents = [];

// A "band" is a voice ignition made visible: a bright height slice that ripples
// through the flock. pitch → y (top = high), pitch → hue. Spawned in onBeat and
// by taps; fades over ~1s so every audible onset has a visible birth.
let bands = []; // {y, hue, life, strength, pan}

// —— tap "button" state ——
let pump = 0; // decaying global agitation energy, 0..~2.6
let shocks = []; // radial ripples spawned by taps {x, y, r, life, hue}
let dragId = null; // active held-drag pointer id

// —— flow-field phase (the drone's slow undulation) ——
// The whole flock's collective wave motion is driven by these phases; the drone
// pushes them so the low hum literally IS the big slow undulation you see.
let flowPhase = 0; // advances slowly — the collective undulation
let breath = 1; // whole-swarm contraction/expansion (the pulse), ~0.85..1.2

// Convert a note string to Hz for pitch→y/hue mapping (local, no deps).
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToHz(note) {
  const m = /^([a-g])(#|s|b)?(\d)$/.exec(note.toLowerCase());
  if (!m) return 220;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "#" || m[2] === "s") semi += 1;
  else if (m[2] === "b") semi -= 1;
  const octave = +m[3];
  const midi = semi + (octave + 1) * 12;
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// Map a note's Hz into a normalized 0..1 pitch band (roughly a2..c6).
function pitchNorm(hz) {
  const n = Math.log2(hz / noteToHz("a2")) / Math.log2(noteToHz("c6") / noteToHz("a2"));
  return Math.max(0, Math.min(1, n));
}

function boot({ sound, clock, screen, num }) {
  sound.bpm(BPM); // only affects the internal metronome; UTC below is authoritative
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  // A wide, airy hall so the murmur chorus blooms and blurs into a haze.
  sound.room?.set?.({ enabled: true, mix: 0.48, feedback: 0.58 });

  // Seed the swarm. Positions are re-derived from the flow field each frame, so
  // we only need each agent's phase seed + a home spread. `num.randInt` is
  // 0..n inclusive; keep it simple and cheap.
  const w = screen?.width || 360;
  const h = screen?.height || 640;
  agents = [];
  for (let i = 0; i < AGENTS; i++) {
    agents.push({
      // A seed angle/phase so the flock spreads and each dot has its own wobble.
      seed: (i / AGENTS) * Math.PI * 2,
      spread: Math.random(), // 0..1 across the flow field
      // Current on-screen position (advected + eased each frame).
      x: Math.random() * w,
      y: Math.random() * h,
      wob: Math.random() * Math.PI * 2, // personal flap phase
      // Ignition: raised when a band at this agent's height lights it up.
      glow: 0,
      hue: 0.6,
    });
  }
}

// Fire one beat's worth of sound. Called from sim on each UTC beat crossing so
// two instances anywhere sound the identical step at the identical wall-clock ms.
function onBeat(idx, synth) {
  const step = ((idx % LOOP) + LOOP) % LOOP;
  beatCount = step;

  // The murmur VOICE that ignites this beat. A soft detuned sine chorus (two
  // slightly-detuned sines) → a breathy, flocking mid tone that swells and ebbs.
  const note = VOICES[step];
  const hz = noteToHz(note);
  const pN = pitchNorm(hz); // 0..1 → visual height + hue
  const pan = Math.sin(idx * 0.7) * 0.4; // slow stereo drift
  const hue = pN; // pitch → hue (0..1), twilight igniting warm at the top

  // Two detuned sines = the murmuring chorus body (soft swell, long ebb).
  synth({
    tone: note,
    type: "sine",
    beats: 2.4, // overlap generously → a continuous murmuring haze
    attack: 0.35,
    decay: 0.7,
    volume: 0.3,
    pan,
  });
  synth({
    tone: hz * 1.008, // gently detuned partner → beating shimmer
    type: "sine",
    beats: 2.2,
    attack: 0.4,
    decay: 0.65,
    volume: 0.22,
    pan: -pan,
  });
  // A faint triangle an octave up on alternating beats — a high glint in the flock.
  if (step % 2 === 0) {
    synth({
      tone: hz * 2,
      type: "triangle",
      beats: 1.2,
      attack: 0.25,
      decay: 0.6,
      volume: 0.1,
      pan,
    });
  }

  // The slow breath PULSE — a soft low thump every other beat: the whole swarm
  // contracting/expanding. Sine so it's felt, not clicky.
  if (step % 2 === 0) {
    synth({
      tone: "a1",
      type: "sine",
      beats: 1.4,
      attack: 0.06,
      decay: 0.5,
      volume: 0.34,
      pan: 0,
    });
  }

  // The VISIBLE counterpart of the voice: a bright height band (pitch → y, pitch
  // → hue) that ripples through the flock and lights up agents at that height.
  bands.push({ y: 1 - pN, hue, life: 1, strength: 1, pan });
  if (bands.length > 12) bands.shift();
}

// —— THE WHOLE PIECE IS A BUTTON ——
// A tap/XY-drag scatters/ignites the swarm from the tap point (a shock that
// ripples through the flock) + a swelling murmur voice. X → pan + hue, Y → pitch
// (top = high). Taps punch a bigger pump; drags feed it.
function act({ event: e, sound: { synth }, screen, num }) {
  const w = screen.width || 1;
  const h = screen.height || 1;

  if (e.is("touch") || e.is("draw")) {
    const x = num.clamp(e.x / w, 0, 1);
    const y = num.clamp(e.y / h, 0, 1);
    const isDraw = e.is("draw");

    // Accumulate a decaying agitation — taps punch the flock, drags stir it.
    pump = Math.min(2.6, pump + (isDraw ? 0.06 : 0.85));

    // A shock ripples outward from the touch point through the swarm.
    const hue = (x * 0.5 + 0.55) % 1; // X → hue across the twilight arc
    shocks.push({ x: e.x, y: e.y, r: 0, life: 1, hue, vol: isDraw ? 0.5 : 1 });
    if (shocks.length > 20) shocks.shift();

    // A bright band at the tap height too, so the visible ignition matches the
    // audible one exactly.
    bands.push({ y: 1 - y, hue, life: 1, strength: isDraw ? 0.7 : 1, pan: x * 2 - 1 });
    if (bands.length > 12) bands.shift();

    // SONIC BOOST — a swelling murmur voice. Pentatonic so it always agrees with
    // the chorus. Y → pitch (top = high), X → pan/hue. Soft attack → a swell, a
    // murmur igniting, not a click. Quieter on a drag so a sweep stays tender.
    const scale = ["c", "d", "e", "g", "a"];
    const noteName = scale[Math.min(4, Math.floor(x * 5))];
    const oct = 3 + Math.round((1 - y) * 3); // 3..6 — higher up = higher note
    const pan = x * 2 - 1;
    synth({
      tone: noteName + oct,
      type: "sine",
      beats: 1.6,
      attack: 0.05,
      decay: 0.8,
      volume: 0.32 * (isDraw ? 0.6 : 1),
      pan,
    });
    // A detuned partner → the same beating murmur shimmer as the chorus.
    synth({
      tone: noteToHz(noteName + oct) * 1.008,
      type: "sine",
      beats: 1.3,
      attack: 0.06,
      decay: 0.7,
      volume: 0.18 * (isDraw ? 0.6 : 1),
      pan: -pan,
    });

    if (e.is("touch")) dragId = e.pointer ?? e.id ?? 0;
  }

  if (e.is("lift")) dragId = null;
}

// Poll live audio (mandatory) + advance the UTC beat grid + move the flock.
function sim({ sound: { speaker, synth }, clock, screen }) {
  speaker?.poll();

  // —— UTC-synced rhythm ——
  // Absolute beat count from epoch ms → every instance computes the same step.
  // clock.time() can momentarily return an invalid Date (offset mid-sync); the
  // `??` won't catch that (it's a Date object, not null) so guard NaN explicitly
  // and fall back to local wall-clock time — the grid stays continuous.
  const ct = clock?.time?.();
  let ms = ct ? ct.getTime() : NaN;
  if (!Number.isFinite(ms)) ms = Date.now();
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1 within the current beat
  globalCycle = globalBeat / LOOP; // continuous cycle position (seamless phase)

  // Start the held DRONE bed once — the flock's collective body / low hum. Two
  // slightly-detuned low sines beating together = the humming drone bed.
  if (!droneStarted) {
    droneStarted = true;
    drone.push(
      synth({ tone: DRONE[0], type: "sine", duration: "🔁", attack: 1.2, decay: 0.9, volume: 0.16, pan: -0.2 }),
      synth({ tone: noteToHz(DRONE[0]) * 1.006, type: "sine", duration: "🔁", attack: 1.4, decay: 0.9, volume: 0.13, pan: 0.2 }),
      synth({ tone: DRONE[1], type: "sine", duration: "🔁", attack: 1.6, decay: 0.9, volume: 0.1, pan: 0 }),
    );
  }

  if (idx !== lastBeat) {
    lastBeat = idx;
    onBeat(idx, synth); // fire this beat's murmur voice + breath pulse
  }

  // The DRONE undulation drives the flock: advance the shared flow phase slowly.
  // Tie its speed to the seamless global cycle so it wraps with the loop and to
  // pump so taps agitate the whole murmuration.
  flowPhase = globalCycle * Math.PI * 2 * 2; // 2 undulations per 8-beat loop → seamless

  // The breath pulse: the whole swarm contracts/expands. Brightest/tightest on
  // the beat, easing to the next; every-other beat has the audible thump.
  const beatPulse = 1 - beatProgress;
  breath = 1 + 0.14 * Math.sin(globalCycle * Math.PI * 2 * 2) + beatPulse * 0.04 + pump * 0.04;

  // Decay the tap agitation + advance/expire shocks + fade bands.
  pump = Math.max(0, pump - 0.014);
  for (const s of shocks) {
    s.r += 5 + s.vol * 5 + pump * 3; // ripple races outward through the flock
    s.life -= 0.02;
  }
  shocks = shocks.filter((s) => s.life > 0);
  for (const b of bands) b.life -= 0.02; // ~1s ignition fade
  bands = bands.filter((b) => b.life > 0);

  // —— move the flock ——
  // A SHARED sine flow field (cheap): each agent samples a smooth vector field
  // driven by flowPhase (the drone undulation) + its own seed/spread, plus a
  // gentle pull toward the flock center scaled by breath (contraction/expansion),
  // plus a shock push. Ease toward the target position for silky motion.
  const w = screen.width || 360;
  const h = screen.height || 640;
  const cx = w / 2;
  const cy = h / 2;
  const spanX = w * 0.42;
  const spanY = h * 0.34;

  for (const a of agents) {
    a.wob += 0.06;
    // Flow-field target: a broad, slowly-rolling murmuration ribbon. The whole
    // pattern undulates with flowPhase (the drone) and breathes with `breath`.
    const p = a.spread; // 0..1 across the flock
    const angle = a.seed + flowPhase + p * Math.PI * 2;
    // Big collective wave (the drone's undulation) — the flock's overall motion.
    const wave = Math.sin(flowPhase + p * 3.4) * 0.6 + Math.sin(flowPhase * 0.6 + a.seed) * 0.4;
    let tx = cx + Math.cos(angle) * spanX * (0.5 + 0.5 * Math.cos(flowPhase + p * 2)) / breath;
    let ty =
      cy +
      wave * spanY * breath + // vertical undulation = the drone
      Math.sin(a.wob) * 6; // tiny personal flap so it shimmers

    // Shock push — each ripple front shoves nearby agents outward (the scatter).
    for (const s of shocks) {
      const dx = a.x - s.x;
      const dy = a.y - s.y;
      const d = Math.hypot(dx, dy) || 1;
      const front = Math.abs(d - s.r);
      if (front < 40) {
        const push = (1 - front / 40) * s.life * 26 * s.vol;
        tx += (dx / d) * push;
        ty += (dy / d) * push;
      }
    }

    // Ease toward the target → smooth, flocking motion. More agitation (pump)
    // makes the flock snappier/more nervous.
    const ease = 0.06 + pump * 0.05;
    a.x += (tx - a.x) * ease;
    a.y += (ty - a.y) * ease;

    // Ignition: if a bright band is near this agent's height, it lights up. This
    // is how you SEE the harmony — voices become glowing height slices.
    let g = 0;
    let gh = a.hue;
    for (const b of bands) {
      const by = b.y * h;
      const dy = Math.abs(a.y - by);
      if (dy < 85) {
        // Soft-shoulder falloff so agents right in the band light near-full.
        const prox = 1 - dy / 85;
        const lit = prox * prox * b.life * b.strength * 1.6;
        if (lit > g) {
          g = lit;
          gh = b.hue;
        }
      }
    }
    a.glow += (Math.min(1, g) - a.glow) * 0.3; // ease glow so ignitions bloom & fade
    a.hue += (gh - a.hue) * 0.25;
  }
}

function paint({ ink, box, circle, screen, sound, num, paintCount }) {
  const { width: w, height: h } = screen;

  // Twilight wash — a translucent gradient veil each frame so the swarm leaves a
  // silky motion-smear trail rather than snapping. Indigo→teal twilight bed.
  ink("fade:midnightblue-teal:vertical", 40).box(0, 0, w, h);

  // —— audio reads (make the ambient field react too) ——
  const bandsAudio = sound.speaker?.frequencies?.left || [];
  const subBass = bandsAudio.find((b) => b.name === "subBass")?.amplitude || 0;
  const lowMid = bandsAudio.find((b) => b.name === "lowMid")?.amplitude || 0;
  const air = bandsAudio.find((b) => b.name === "air")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;

  // Density/brightness of the whole flock rides amplitude + the low drone.
  const density = 0.5 + amp * 1.2 + subBass * 0.8 + pump * 0.4;

  // —— draw the flock ——
  // Each agent is a soft additive dot. Base twilight color (indigo/teal) blends
  // toward its ignition hue (warm) by its glow → bright pitch-bands within the
  // flock. Dots near the flock core are denser/brighter.
  for (const a of agents) {
    // Base twilight color: indigo↔teal, drifting slowly with the undulation.
    const baseHueDeg = (200 + Math.sin(a.seed + flowPhase) * 40 + 360) % 360; // ~160..240 teal↔indigo
    // Warp the glow so even a partial ignition tips decisively WARM (avoids the
    // cool↔warm blend lingering in the green midpoint).
    const glow = Math.pow(Math.min(1, a.glow), 0.5);
    const [br, bg, bb] = num.hslToRgb(baseHueDeg, 65, 42 + amp * 12); // cool body
    // Ignition color: a WARM igniting voice (a.hue 0..1 → amber↔rose ~35..70°).
    // Blend in RGB space (NOT hue) so cool→warm never sweeps through green.
    const litHueDeg = a.hue * 40 + 30; // 30..70 amber↔gold (kept off green)
    const [lr, lg, lb] = num.hslToRgb(litHueDeg, 95, 62); // hot warm pop
    const r = br * (1 - glow) + lr * glow;
    const g = bg * (1 - glow) + lg * glow;
    const b = bb * (1 - glow) + lb * glow;

    // Dot size + alpha swell with ignition, density, and the low end.
    const size = 1.4 + glow * 3.4 + subBass * 1.6 + air * 1.4;
    const alpha = Math.min(230, 60 + glow * 150 + density * 40);
    // Soft additive halo for igniting agents so bands GLOW, not just recolor.
    if (glow > 0.15) {
      ink(r, g, b, alpha * 0.4).circle(a.x, a.y, size * 2.2, true);
    }
    ink(r, g, b, alpha).circle(a.x, a.y, size, true);
  }

  // —— tap shocks: expanding rings rippling through the flock ——
  for (const s of shocks) {
    const [r, g, b] = num.hslToRgb(s.hue * 55 + 32, 88, 66); // warm, matches ignitions
    ink(r, g, b, 170 * s.life).circle(s.x, s.y, s.r, false, 2 + s.vol * 2);
    ink(r, g, b, 90 * s.life).circle(s.x, s.y, s.r * 0.7, false, 1);
  }

  // —— pitch-band guide glow ——
  // A soft warm horizontal wash at each live voice's height so you read the
  // harmony as bands even in sparse flock regions (the "graphic notation" layer).
  for (const b of bands) {
    const by = b.y * h;
    const [r, g, b2] = num.hslToRgb(b.hue * 55 + 32, 78, 60);
    ink(r, g, b2, 34 * b.life * b.strength).box(0, by - 24, w, 48);
  }
}

export { boot, act, sim, paint };
