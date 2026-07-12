// sundo, 26.07.12
// Self-running A/V loop: a slow SUNRISE DRONE — warm evolving ambient PAD chords
// that shift every few beats over a deep sub foundation and soft airy shimmer.
// A sun disc breathes over a horizon of dawn-color bands. Meditative, spacious,
// luscious; loops seamlessly. Zero input — autostarts its own audio on boot.
//
// FOUR PRINCIPLES (reel canon):
//   1. NO resolution() — native res; pure vector box/poly/circle work, no buffer.
//   2. WHOLE PIECE IS A BUTTON — a soft tap/drag sends a warm light-ripple across
//      the sky + a gentle swelling chord tone (X→pan/hue, Y→pitch). Repeated taps
//      accumulate a soft decaying `pump` that brightens the whole dawn.
//   3. UTC-SYNCED RHYTHM — the slow chord changes + sub pulses are scheduled from
//      clock.time() in sim() (NOT beat()), indexed floor(globalBeat) % len, so two
//      instances anywhere auto-align. Slow BPM 56; seamless harmonic loop; the
//      held pad is retuned on beat crossings.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — each PAD chord tone IS one horizon LIGHT
//      BAND whose height ∝ its pitch (low tone = low warm band, high tone = high
//      pale band) → the held chord literally IS the stacked dawn sky. The SUB is
//      the SUN disc's size + glow. Chord changes = the sky re-coloring (slow
//      gradient sweep). Airy shimmer = drifting light motes near the sun.
//      Amplitude → overall glow/brightness. You read the harmony as the bands.

// —— Harmony ——
// A slow-moving held triad that shifts every couple of beats over a deep sub
// pedal. Warm, drifting, dawn-hued; resolves back to the start so audio loops as
// cleanly as the visuals. One SUB note per beat (the sun's heartbeat pedal).
const SUB = ["a1", "a1", "a1", "a1", "e1", "e1", "e1", "e1"]; // deep foundation
// Pad chord tones (held triad), retuned on downbeats. Each tone becomes one
// horizon light band; band height ∝ that tone's pitch (low→low band).
const PAD = [
  ["a2", "c3", "e3"], // Am — the resting dawn
  ["a2", "c3", "e3"],
  ["c3", "e3", "g3"], // C — the sky warms
  ["c3", "e3", "g3"],
  ["e2", "g2", "b2"], // Em — deeper, slower
  ["e2", "g2", "b2"],
  ["d3", "f3", "a3"], // Dm — the horizon lifts
  ["e2", "g2", "b2"],
];

const BPM = 56; // slow, meditative
const BEAT_MS = 60000 / BPM; // ms per beat
const LOOP = PAD.length; // 8-beat harmonic cycle

// —— UTC rhythm state (driven in sim, never beat) ——
let lastBeat = -1; // last integer global beat we fired
let beatCount = 0; // current step within the loop (0..LOOP-1)
let beatProgress = 0; // 0..1 through the current beat (smooth visuals)
let globalCycle = 0; // (globalBeat / LOOP) continuous — seamless phase

// —— live audio voices ——
let pad = []; // sustained pad voices (one per chord tone)
let chord = PAD[0]; // the chord currently sounding (drives the bands)
let prevChord = PAD[0]; // the chord we're easing away from (gradient sweep)
let chordBlend = 1; // 0..1 crossfade from prevChord→chord on each change
let subVoice = null; // sustained sub pedal (the sun's foundation)
let subFreq = 55; // current sub note frequency (drives sun size + glow)

// —— tap "button" state ——
let pump = 0; // decaying global brightness energy, 0..~2.4
let ripples = []; // soft light-ripples spawned by taps {x,y,r,life,hue,vol}
let dragId = null; // active held-drag pointer id

// —— drifting light motes (the airy shimmer) ——
let motes = [];

// Convert a note string to Hz for pitch→band-height/hue mapping (local, no deps).
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToHz(note) {
  const m = /^([a-g])(#|s|b)?(\d)$/.exec(note.toLowerCase());
  if (!m) return 220;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "#" || m[2] === "s") semi += 1;
  else if (m[2] === "b") semi -= 1;
  const octave = +m[3];
  const midi = semi + (octave + 1) * 12; // MIDI note number
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// Normalize a chord's pitches to 0..1 band-height positions (low tone → low).
// Pitch drives the *position within the dawn arc* (absolute log2 window keeps it
// consistent across chords), then we spread the triad so its three tones read as
// three clearly separated horizontal strata (blend absolute pitch with rank).
function bandHeights(ch) {
  const hz = ch.map(noteToHz);
  const n = ch.length;
  return hz.map((f, i) => {
    // Absolute pitch position in a warm pad window (~a2..a4 fills 0..1).
    const abs = Math.max(0, Math.min(1, Math.log2(f / 100) / 2.4));
    // Rank position (0 for lowest tone, 1 for highest) guarantees separation.
    const rank = n > 1 ? i / (n - 1) : 0.5;
    // Blend: keeps chord-to-chord pitch feel while always stacking 3 distinct
    // bands. Compressed into the mid band so bands never touch the extremes.
    return 0.12 + (0.45 * abs + 0.55 * rank) * 0.76;
  });
}

function boot({ sound, clock }) {
  sound.bpm(BPM); // only affects the internal metronome; UTC below is authoritative
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  // Big, spacious dawn reverb tail across everything.
  sound.room?.set?.({ enabled: true, mix: 0.55, feedback: 0.66 });
  // Seed a scatter of drifting light motes near where the sun will be.
  for (let i = 0; i < 26; i++) {
    motes.push({
      a: Math.random() * Math.PI * 2, // orbit angle around the sun
      rad: 0.2 + Math.random() * 0.9, // orbit radius (× sun radius)
      spd: 0.05 + Math.random() * 0.14, // drift speed
      ph: Math.random() * Math.PI * 2, // twinkle phase
      size: 0.6 + Math.random() * 1.6,
    });
  }
}

// Fire one beat's worth of sound. Called from sim on each UTC beat crossing so
// two instances anywhere sound the identical step at the identical wall-clock ms.
function onBeat(idx, synth) {
  const step = ((idx % LOOP) + LOOP) % LOOP;
  beatCount = step;

  // —— deep SUB pedal (the sun's foundation) ——
  // A soft held sine sub; retuned each beat. Create once, then update the held
  // voice. Its pitch drives the sun disc's size + glow.
  const subNote = SUB[step];
  subFreq = noteToHz(subNote);
  if (!subVoice) {
    subVoice = synth({
      tone: subNote,
      type: "sine",
      duration: "🔁", // held forever until updated/killed
      attack: 1.2, // very slow swell in
      decay: 0.95,
      volume: 0.34,
      pan: 0,
    });
  } else {
    subVoice.update?.({ tone: subNote });
  }
  // A gentle re-articulation on the downbeat so the sub breathes audibly.
  synth({
    tone: subNote,
    type: "sine",
    beats: 3.0, // long overlap → seamless legato swell
    attack: 0.9,
    decay: 0.8,
    volume: 0.22,
    pan: 0,
  });

  // —— the held PAD chord (the horizon bands) ——
  // Only retune when the chord actually changes → trigger the sky re-color sweep.
  const nextChord = PAD[step];
  const changed = nextChord.join() !== chord.join();
  if (changed) {
    prevChord = chord;
    chord = nextChord;
    chordBlend = 0; // begin the slow gradient sweep to the new sky
  }
  if (pad.length === 0) {
    for (let i = 0; i < chord.length; i++) {
      pad.push(
        synth({
          tone: chord[i],
          type: "sine",
          duration: "🔁", // held forever
          attack: 1.4, // luscious slow fade-in
          decay: 0.95,
          volume: 0.13,
          pan: (i - 1) * 0.4, // spread the triad across the field
        }),
      );
    }
  } else if (changed) {
    for (let i = 0; i < pad.length; i++) {
      pad[i]?.update?.({ tone: chord[i % chord.length] });
    }
  }

  // —— soft airy shimmer on top (the motes' sound) ——
  // A very quiet high triangle a couple octaves above the top pad tone on the
  // sky-changing beats — a pale glimmer, never a click.
  if (changed) {
    const top = chord[chord.length - 1];
    synth({
      tone: top.replace(/\d/, (d) => String(Math.min(7, +d + 2))),
      type: "triangle",
      beats: 4.0,
      attack: 1.0,
      decay: 0.7,
      volume: 0.06,
      pan: 0.2,
    });
  }
}

// —— THE WHOLE PIECE IS A BUTTON ——
// A soft tap/drag sends a warm light-ripple across the sky + a gentle swelling
// chord tone. X → pan + hue, Y → pitch (top = high). Taps punch a little pump;
// drags feed it. Keep every burst SOFT — a swell, never a stab.
function act({ event: e, sound: { synth }, screen, num }) {
  const w = screen.width || 1;
  const h = screen.height || 1;

  if (e.is("touch") || e.is("draw")) {
    const x = num.clamp(e.x / w, 0, 1);
    const y = num.clamp(e.y / h, 0, 1);
    const isDraw = e.is("draw");

    // Accumulate a soft, decaying brightness swell — taps punch gently, drags feed.
    pump = Math.min(2.4, pump + (isDraw ? 0.05 : 0.5));

    // A warm light-ripple spreads outward from the touch point (translucent, no
    // hard edge). Hue rides X across the dawn arc (rose→amber→gold).
    const hue = (x * 0.16 + 0.02) % 1; // ~violet-rose..amber-gold band
    ripples.push({
      x: e.x,
      y: e.y,
      r: 0,
      life: 1,
      hue,
      vol: isDraw ? 0.5 : 1,
    });
    if (ripples.length > 20) ripples.shift();

    // Gentle swelling chord tone — pentatonic so it always agrees with the pad.
    // Y → pitch (top = high), X → pan. Slow attack, long decay → a soft swell.
    const scale = ["c", "d", "e", "g", "a"];
    const noteName = scale[Math.min(4, Math.floor(x * 5))];
    const oct = 3 + Math.round((1 - y) * 3); // 3..6, higher up = higher note
    const pan = x * 2 - 1;
    synth({
      tone: noteName + oct,
      type: "sine",
      beats: 1.6,
      attack: 0.15, // soft swell, not a stab
      decay: 0.85,
      volume: 0.3 * (isDraw ? 0.6 : 1),
      pan,
    });
    // A faint triangle shimmer an octave up — a little glimmer, softer high up.
    synth({
      tone: noteName + Math.min(7, oct + 1),
      type: "triangle",
      beats: 0.9,
      attack: 0.1,
      decay: 0.6,
      volume: 0.09 * (isDraw ? 0.5 : 1),
      pan,
    });

    if (e.is("touch")) dragId = e.pointer ?? e.id ?? 0;
  }

  if (e.is("lift")) dragId = null;
}

// Poll live audio (mandatory) + advance the UTC beat grid + smooth motion.
function sim({ sound: { speaker, synth }, clock }) {
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

  if (idx !== lastBeat) {
    lastBeat = idx;
    onBeat(idx, synth); // fire this beat's sub pedal + pad retune + shimmer
  }

  // Slow crossfade of the sky when the chord changes (gradient re-color sweep).
  chordBlend = Math.min(1, chordBlend + 0.02);

  // Soft decay of the tap swell + advance/expire the light-ripples.
  pump = Math.max(0, pump - 0.01);
  for (const r of ripples) {
    r.r += 2.0 + r.vol * 1.8; // gentle, lingering outward spread
    r.life -= 0.01; // slow fade so the ripple reads clearly
  }
  ripples = ripples.filter((r) => r.life > 0);

  // Drift the light motes slowly around the sun.
  for (const m of motes) {
    m.a += m.spd * 0.01;
    m.ph += 0.02;
  }
}

function paint({ ink, box, screen, sound, num, paintCount }) {
  const { width: w, height: h } = screen;
  const base = Math.min(w, h);

  // —— audio reads ——
  const bands = sound.speaker?.frequencies?.left || [];
  const sub = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const lowMid = bands.find((b) => b.name === "lowMid")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;

  // Seamless global phase from the UTC cycle (wraps every LOOP beats).
  const cyclePhase = ((globalCycle % 1) + 1) % 1; // 0..1
  const drift = cyclePhase * Math.PI * 2;

  // Every beat = a gentle whole-sky breath: brightest right on the beat, easing
  // to the next. Tap pump adds a soft extra brightening.
  const beatPulse = 1 - beatProgress; // 1 on the beat → 0 just before next
  const glow = 1 + 0.04 * beatPulse + pump * 0.14 + amp * 0.3; // overall brightness

  // The sun sits low on the screen, near the horizon — a rising dawn. Keep it
  // low so the sky above has room for the horizon light bands to read clearly.
  const horizonY = h * 0.74; // where sky meets the warm base bands
  const sunX = w * 0.5;
  // Sun breathes up/down gently with the cycle (the "rising" motion).
  const sunY = horizonY + Math.sin(drift) * base * 0.025 - base * 0.015 * beatPulse;

  // —— DEEP SKY WASH (soft dawn gradient veil, feedback trails) ——
  // A translucent vertical gradient each frame: deep indigo up top easing to a
  // warm rose toward the horizon. The veil (not a hard wipe) lets bands + motes
  // leave soft trails. Brightens subtly with the overall glow.
  const veilA = 40 + amp * 22 + pump * 14;
  ink("fade:midnightblue-indigo-mediumvioletred:vertical", veilA).box(0, 0, w, h);

  // —— THE HORIZON LIGHT BANDS (the pad chord made visible) ——
  // Each of the three sustained chord tones is ONE horizontal light band. The
  // band's vertical CENTER ∝ that tone's pitch: low tone → a low warm band near
  // the horizon, high tone → a high pale band up in the sky. The held triad is
  // literally the stacked dawn. Bands crossfade (prev→current) on chord change
  // for a slow gradient re-color sweep. They breathe up/down with the cycle.
  const curH = bandHeights(chord); // 0..1 per tone
  const prvH = bandHeights(prevChord);
  const heights = curH.map((c, i) =>
    num.lerp(prvH[i] ?? c, c, chordBlend),
  );
  // Sort so we draw low bands (near horizon) first, pale bands on top.
  const order = heights.map((v, i) => i).sort((a, b) => heights[a] - heights[b]);

  // The band stack spans from just above the horizon up into the mid-sky, so the
  // three tones read as three clearly separated horizontal strata of dawn light.
  const bandTop = horizonY - base * 0.62; // highest a band can sit
  const bandBot = horizonY - base * 0.02; // lowest (nearest the horizon)
  for (const i of order) {
    const bh = heights[i]; // 0 (low/warm) .. 1 (high/pale)
    // Band center Y: pitch 0 → just above horizon, pitch 1 → high in the sky.
    const bandY = num.lerp(bandBot, bandTop, bh);
    // Soft breathing thickness; a touch thicker for the low warm bands.
    const thick =
      base * (0.06 + (1 - bh) * 0.05) *
      (1 + 0.12 * Math.sin(drift * 1.3 + i) + sub * 0.4 + air * 0.25) *
      glow;
    // Hue rides the pitch across the dawn arc: low = deep amber/rose (~14°),
    // high = pale gold/cream (~50°). Higher band = paler + brighter.
    const hueDeg = 12 + bh * 42; // ~amber → gold
    const sat = 84 - bh * 36; // paler up high
    const light = 50 + bh * 26 + amp * 8; // brighter up high
    const [br, bg, bb] = num.hslToRgb(hueDeg, sat, Math.min(88, light));
    // Feathered band: a few stacked translucent boxes, widest+faintest first,
    // so there's no hard edge — a soft glowing stratum of dawn light. A brighter
    // crisp core box on top lets the stratum read as a distinct band.
    const layers = 4;
    for (let L = layers; L >= 1; L--) {
      const f = L / layers; // 1 = widest/faintest
      const th = thick * f;
      const a = (34 + (1 - bh) * 28 + amp * 18 + pump * 14) * (1 - f * 0.42);
      ink(br, bg, bb, a).box(0, bandY - th / 2, w, th);
    }
    // Crisp bright core stratum → the band is unmistakably readable.
    ink(br, bg, bb, 120 + amp * 40 + pump * 20).box(
      0, bandY - thick * 0.16, w, thick * 0.32,
    );
  }

  // —— THE SUN DISC (the sub pedal made visible) ——
  // A soft glowing sun whose SIZE + GLOW ARE the sub note (lower sub = larger,
  // warmer sun) and which flares gently on each beat — the heartbeat. Layered
  // translucent circles (widest + faintest first) build a feathered corona with
  // no hard edge; the innermost warms toward white-gold. Pure circles.
  const subN = num.clamp(Math.log2(subFreq / 30) / 2.2, 0, 1); // 0=low..1=high
  const sunR =
    base *
    (0.075 + (1 - subN) * 0.03) * // lower sub → larger sun (kept modest)
    (1 + sub * 0.7) *
    glow +
    beatPulse * base * 0.012;
  // Sun hue: warm amber-gold, warming a touch with the sub energy.
  const sunHue = 40 + sub * 8;
  const [sr, sg, sb] = num.hslToRgb(sunHue, 82, 62); // 0..255, no ×255
  // Soft corona/haze first (contained so it doesn't swamp the bands), then the
  // disc, then a hot core.
  const CORONA = 7;
  for (let i = CORONA; i >= 1; i--) {
    const f = i / CORONA; // 1 = outermost/faintest corona ring
    // Warm gold at the edge, fading toward cream-white at the middle.
    const wr = sr + (255 - sr) * (1 - f);
    const wg = sg + (250 - sg) * (1 - f);
    const wb = sb + (225 - sb) * (1 - f);
    const a = (11 + beatPulse * 12 + sub * 20 + amp * 14 + pump * 12) * (1 - f * 0.62);
    ink(wr, wg, wb, a).circle(sunX, sunY, sunR * (0.55 + f * 1.15), true);
  }
  // The hot little core.
  ink(255, 250, 236, 130 + beatPulse * 30 + amp * 40).circle(
    sunX, sunY, sunR * 0.5, true,
  );

  // —— DRIFTING LIGHT MOTES (the airy shimmer made visible) ——
  // Soft round glows orbiting the sun, twinkling with the air band + amp. Never
  // hard points — feathered little suns.
  for (const m of motes) {
    const orb = sunR * (1.1 + m.rad * 1.4);
    const mx = sunX + Math.cos(m.a) * orb;
    const my = sunY + Math.sin(m.a) * orb * 0.7; // flatter orbit (over horizon)
    const tw = 0.5 + 0.5 * Math.sin(m.ph); // twinkle 0..1
    const mr = m.size * (1 + air * 3 + amp * 2) * (0.6 + tw * 0.8);
    const a = (18 + air * 60 + amp * 30 + pump * 16) * (0.4 + tw * 0.6);
    ink(255, 244, 214, a * 0.5).circle(mx, my, mr * 2.2, true);
    ink(255, 250, 232, a).circle(mx, my, mr, true);
  }

  // —— tap light-ripples (the "button" made visible) ——
  // Each tap's warm ripple expands across the sky and fades — translucent rings
  // + a soft warm center, never a hard box. Hue rides where you touched.
  for (const r of ripples) {
    const rHueDeg = 8 + ((r.hue % 1) + 1) % 1 * 52; // amber→gold arc
    const [rr, rg, rb] = num.hslToRgb(rHueDeg, 74, 66);
    const la = r.life * r.life; // ease-out fade
    // Two expanding soft rings — a warm light-ripple spreading outward.
    ink(rr, rg, rb, 90 * la * r.vol).circle(r.x, r.y, r.r, false, 3);
    ink(rr, rg, rb, 50 * la * r.vol).circle(r.x, r.y, r.r * 0.62, false, 2);
    // Feathered warm glow at the source (widest+faintest first → no hard edge).
    const srcR = 14 + r.r * 0.1;
    ink(255, 240, 210, 42 * la * r.vol).circle(r.x, r.y, srcR, true);
    ink(255, 246, 222, 78 * la * r.vol).circle(r.x, r.y, srcR * 0.55, true);
    ink(255, 251, 236, 140 * la * r.vol).circle(r.x, r.y, srcR * 0.28, true);
  }

  // A single slow drifting high mote, wrapping every 600 frames so the top-layer
  // motion also loops cleanly. A soft round glow, not a hard box.
  const dt = (Number(paintCount) % 600) / 600; // paintCount is a Number
  const da = dt * Math.PI * 2;
  const gx = sunX + Math.cos(da) * base * 0.34;
  const gy = sunY - base * 0.18 + Math.sin(da * 1.5) * base * 0.1;
  const gr2 = (4 + air * 18 + sub * 10) * glow;
  ink(255, 248, 224, 24 + amp * 44).circle(gx, gy, gr2 * 1.9, true);
  ink(255, 252, 238, 60 + amp * 80).circle(gx, gy, gr2, true);
}

function leave() {
  pad.forEach((v) => v?.kill?.(0.8));
  pad = [];
  subVoice?.kill?.(0.8);
  subVoice = null;
}

export { boot, sim, paint, act, leave };
