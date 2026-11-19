// Acid, by @oskie. Written by Oskie on his own ThinkPad (ac13); taken into the tree as he left it, 26.11.19.
// ThinkPad_Acid: a black screen, a neon-green cursor, and lanes you change by clicking:
// kick (tr 909 / 808 / 606 + pattern), a distorted MC-909 snare, and a hardstyle offbeat bass.
// Push: curl -X PUT --data-binary @acid.mjs "http://IP/pieces/acid.mjs?jump=1"

// The only two colours this version uses.
const BLACK = [0, 0, 0];
const GREEN = [57, 255, 20]; // neon green #39FF14

// "display" tells the engine not to draw its own white/cyan crosshair cursor
// over this piece (ac-native.c skips the cursor overlay for this mode).
// We draw our own in GREEN instead.
export const surface = "display";

let escs = 0; // Esc three times in a row leaves to the prompt
let cursor = null; // { x, y } in screen pixels, set once the pointer moves
// pen.x/y come in display pixels; the screen we draw on is smaller by the engine's
// pixel_scale (2 on ac13: display 1366x768 -> screen 683x384, from /logs).
// A touch/draw event gives both at once, so recalibrate from that when we can.
let scale = 2;

// ── Sound bank ─────────────────────────────────────────────────────────────
// Every sound is modelled on one machine, the Roland MC-909 groovebox (Oskie's choice), in the
// dirty, pushed way Container uses it. New sounds should follow that character.
const BANK_NAME = "mc 909";
// Our own sounds, built from engine voices. Each kick is fire(sound, vol).
//
// Engine facts used here (audio.c):
// - A voice's tone glides toward a new target exponentially, at 0.0003 per sample.
//   At 192 kHz that's a time constant of about 17 ms. So start a sine high and
//   immediately retarget it low, and you get a drum-machine pitch sweep for free.
// - Envelopes are linear: attack ramps up, then the last `decay` seconds of
//   `duration` ramp to zero. Layering a short loud voice over a long quiet one
//   approximates an exponential amp decay.
// `decay` defaults to the whole note (a fade from the start); pass a short one for a held, punchy shape.
function sweep(sound, type, from, to, duration, volume, attack = 0.0008, decay = duration - attack) {
  const v = sound.synth?.({ type, tone: from, duration, volume, attack, decay });
  if (to !== from) v?.update?.({ tone: to }); // glide from `from` to `to`
  return v;
}
const blip = (sound, type, tone, duration, volume) =>
  sound.synth?.({ type, tone, duration, volume, attack: 0.0002, decay: duration - 0.0002 });

const KICKS = {
  // TR-909: a punchy sine sweep ~230 -> 50 Hz, a short punch layer, a triangle
  // "knock" for the slightly squared 909 tone, and a click on top.
  "tr 909"(sound, vol = 1) {
    sweep(sound, "sine", 230, 50, 0.50, 1.0 * vol); // body + tail
    sweep(sound, "triangle", 230, 52, 0.11, 0.6 * vol); // punch + knock in one (was two voices)
    blip(sound, "noise", 7000, 0.006, 0.45 * vol); // click
  },
  // TR-808: a long, round, low boom. Almost a pure sine with only a gentle pitch drop,
  // a soft attack and a long tail; just a tiny tick on the front.
  "tr 808"(sound, vol = 1) {
    sweep(sound, "sine", 95, 49, 1.1, 1.05 * vol, 0.002); // long boom
    sweep(sound, "sine", 120, 52, 0.09, 0.45 * vol, 0.001); // soft front
    blip(sound, "sine", 900, 0.003, 0.12 * vol); // tick
  },
  // TR-606: small, tight and dry. Higher pitched, a short body, and more click
  // than boom.
  "tr 606"(sound, vol = 1) {
    sweep(sound, "sine", 260, 78, 0.16, 0.9 * vol); // short body
    sweep(sound, "triangle", 300, 90, 0.03, 0.35 * vol); // snap
    blip(sound, "noise", 5000, 0.009, 0.5 * vol); // click
  },
};

// Snares are fire(sound, vol).
const SNARES = {
  // Container-style blown-out MC-909 snare. The engine has no distortion per voice, so
  // we fake what clipping does to a snare:
  // - Squashed dynamics: noise layers HOLD at full level and then cut, instead of decaying
  //   naturally (a clipped snare sounds "flat-topped" and loud all the way through).
  // - Added harmonics: the 909's two tuned shells (~185 Hz and ~330 Hz) are squares, not sines,
  //   so they come out hard-edged and buzzy, the way a sine sounds once it's clipped.
  // - Grit: a short high square pair on the front, and a mid noise band for the crunchy middle.
  "mc 909 dist"(sound, vol = 1) {
    const len = 0.17, cut = 0.05;
    sweep(sound, "square", 185 * 1.2, 185, 0.07, 0.3 * vol, 0.0005, 0.04); // low shell
    sweep(sound, "square", 330 * 1.2, 330, 0.05, 0.2 * vol, 0.0005, 0.03); // high shell
    sweep(sound, "noise", 7000, 7000, len, 0.55 * vol, 0.0005, cut); // bright snares, held
    sweep(sound, "noise", 2600, 2600, len * 0.8, 0.5 * vol, 0.0005, cut); // crunchy middle
    blip(sound, "square", 1750, 0.012, 0.14 * vol); // grit
    blip(sound, "noise", 9000, 0.004, 0.4 * vol); // stick crack
  },
  // TR-808 snare: two tuned sines (the 808's ~238 Hz and an octave-ish partner) and a bright
  // snappy noise; more tone, less crunch. Slightly pushed so it fits the bank.
  "808 snare"(sound, vol = 1) {
    sweep(sound, "sine", 238 * 1.1, 238, 0.11, 0.5 * vol, 0.0005, 0.08);
    sweep(sound, "sine", 476 * 1.1, 476, 0.07, 0.3 * vol, 0.0005, 0.05);
    sweep(sound, "noise", 8000, 8000, 0.13, 0.5 * vol, 0.0005, 0.09);
    blip(sound, "noise", 10000, 0.004, 0.35 * vol);
  },
  // 909 crack: short, tight and bright, almost all attack. Cuts through busy hats.
  "909 crack"(sound, vol = 1) {
    sweep(sound, "square", 200 * 1.3, 200, 0.03, 0.25 * vol, 0.0003, 0.02);
    sweep(sound, "noise", 9500, 9500, 0.07, 0.7 * vol, 0.0003, 0.04);
    sweep(sound, "noise", 4500, 4500, 0.05, 0.45 * vol, 0.0003, 0.03);
    blip(sound, "noise", 12000, 0.003, 0.5 * vol);
  },
  // Noise burst: a wall of white noise, held and cut, no tone at all. The blunt Container hit.
  "noise burst"(sound, vol = 1) {
    sweep(sound, "noise", 12000, 12000, 0.22, 0.6 * vol, 0.0005, 0.06);
    sweep(sound, "noise", 3000, 3000, 0.22, 0.45 * vol, 0.0005, 0.06);
    blip(sound, "noise", 800, 0.03, 0.4 * vol); // thump under it
  },
  // Lo-fi snare: a crunchy, mid-heavy snare like a low-bitrate sample: square partials and
  // a band of mid noise, little top end.
  "lofi snare"(sound, vol = 1) {
    sweep(sound, "square", 210, 200, 0.06, 0.22 * vol, 0.0005, 0.04);
    sweep(sound, "square", 640, 620, 0.04, 0.12 * vol, 0.0005, 0.03);
    sweep(sound, "noise", 2200, 2200, 0.16, 0.6 * vol, 0.0005, 0.07);
    sweep(sound, "noise", 1200, 1200, 0.08, 0.35 * vol, 0.0005, 0.05);
  },
  // Clean TR-909 for contrast: the same shells as sines, with noise that decays naturally.
  "tr 909"(sound, vol = 1) {
    sweep(sound, "sine", 185 * 1.15, 185, 0.06, 0.45 * vol);
    sweep(sound, "sine", 330 * 1.15, 330, 0.04, 0.3 * vol);
    sweep(sound, "noise", 6500, 6500, 0.2, 0.55 * vol);
    blip(sound, "noise", 9000, 0.004, 0.35 * vol);
  },
};

// Hats are fire(sound, kind, vol), where kind is "closed" or "open".
// A closed hat chokes a ringing open one, like on a real 909 / MC-909.
let openHat = []; // voices of the open hat still ringing
const HAT_METAL = [3150, 4470, 5340, 6870, 8120]; // inharmonic partials: the metallic 909 shimmer
// The engine on ac13 has only 32 voices for everything, so hats use the two partials that
// carry the most metal (a closed hat was 7 voices, now 3).
const HAT_METAL_LEAN = [3150, 5340];
const HATS = {
  // MC-909 hats: bright, metallic, a bit dirty like the snare (held then cut, not a soft fade).
  "909 hats"(sound, kind, vol = 1) {
    if (kind === "open") {
      for (const v of openHat) v?.kill?.(0.012); // a new open hat chokes the last one too (one hat, like a real 909)
      const len = 0.32;
      openHat = HAT_METAL_LEAN.map((f) => sweep(sound, "square", f, f, len, 0.07 * vol, 0.0005));
      openHat.push(sweep(sound, "noise", 9500, 9500, len, 0.3 * vol, 0.0005, len * 0.7));
      openHat.push(sweep(sound, "noise", 6000, 6000, len * 0.6, 0.12 * vol, 0.0005));
    } else {
      for (const v of openHat) v?.kill?.(0.012); // choke
      openHat = [];
      const len = 0.04;
      for (const f of HAT_METAL_LEAN) sweep(sound, "square", f, f, len, 0.065 * vol, 0.0003, 0.025);
      sweep(sound, "noise", 10000, 10000, len, 0.36 * vol, 0.0003, 0.025);
    }
  },
};

// Percussion is fire(sound, vol). MC-909 percussion played dirty: held-then-cut shapes,
// squares where a clean machine would use sines.
const PERCS = {
  // Rimshot: a hard, high knock, all attack.
  "909 rim"(sound, vol = 1) {
    sweep(sound, "triangle", 1750, 1700, 0.025, 0.4 * vol, 0.0003, 0.015);
    sweep(sound, "square", 520, 480, 0.03, 0.22 * vol, 0.0003, 0.02);
    blip(sound, "noise", 6000, 0.008, 0.3 * vol);
  },
  // Clap: three quick noise slaps, then a short tail. The engine can't start a voice
  // later, so the slaps are staggered with longer and longer attack ramps instead.
  "909 clap"(sound, vol = 1) {
    sweep(sound, "noise", 1500, 1500, 0.012, 0.6 * vol, 0.0005, 0.008);
    sweep(sound, "noise", 1700, 1700, 0.024, 0.55 * vol, 0.011, 0.008);
    sweep(sound, "noise", 1400, 1400, 0.036, 0.55 * vol, 0.022, 0.008);
    sweep(sound, "noise", 2400, 2400, 0.16, 0.45 * vol, 0.03, 0.08); // tail
  },
  // Cowbell: two detuned squares, short and clanky.
  "cowbell"(sound, vol = 1) {
    sweep(sound, "square", 545, 540, 0.11, 0.2 * vol, 0.0005, 0.07);
    sweep(sound, "square", 810, 800, 0.11, 0.16 * vol, 0.0005, 0.07);
    blip(sound, "noise", 4000, 0.004, 0.15 * vol);
  },
  // Tom: a pitched drop with a square edge, like a pushed 909 low tom.
  "909 tom"(sound, vol = 1, pitch = 1) {
    sweep(sound, "sine", 230 * pitch, 140 * pitch, 0.22, 0.6 * vol, 0.0008, 0.12);
    sweep(sound, "square", 230 * pitch, 140 * pitch, 0.09, 0.15 * vol, 0.0008, 0.06);
    blip(sound, "noise", 3000, 0.01, 0.25 * vol);
  },
};

// Weird hats: same call shape as HATS, fire(sound, kind, vol), but beeps, bubbles,
// zaps and chopped animal sounds. Pitched ones pick from F minor pentatonic so they sit
// with the bass. Animal sounds come from the engine's built-in zoo samples
// (sound.zoo.play), cut short with a scheduled kill so they act like hats.
const WEIRD_NOTES = [2093, 2489, 2794, 3322, 3729]; // C7 Eb7 F7 Ab7 Bb7
const pick = (a) => a[Math.floor(Math.random() * a.length)];
// Timed actions: run fn(sound) `ms` from now. sim() checks them every frame (so ~16 ms
// resolution), which is fine for cutting sounds off or bending a pitch mid-note.
let timers = []; // { at, fn }
const later = (ms, fn) => timers.push({ at: Date.now() + ms, fn });
function chop(sound, v, ms, zoo = false) {
  if (v) later(ms, (snd) => (zoo ? snd.zoo?.kill?.(v, 0.015) : v?.kill?.(0.015)));
}
function animal(sound, name, pitch, ms, vol) {
  const v = sound.zoo?.play?.(name, { volume: vol, pitch });
  chop(sound, v, ms, true);
}
const ANIMALS = ["bird", "cat", "cow", "dog", "frog", "horse", "lion", "owl", "pig", "sheep", "snake", "whale"];
const WEIRD_HATS = {
  // Short square beeps, like a sampler's test tones.
  beeps(sound, kind, vol = 1) {
    const f = pick(WEIRD_NOTES), len = kind === "open" ? 0.12 : 0.035;
    sweep(sound, "square", f, f, len, 0.12 * vol, 0.0005, len * 0.5);
    sweep(sound, "sine", f * 2, f * 2, len * 0.6, 0.08 * vol, 0.0005);
  },
  // Rising sine "blips": each starts low and glides up fast, like a bubble popping.
  bubbles(sound, kind, vol = 1) {
    const f = 500 + Math.random() * 600, len = kind === "open" ? 0.11 : 0.05;
    sweep(sound, "sine", f, f * 2.6, len, 0.35 * vol, 0.002);
    if (kind === "open") sweep(sound, "sine", f * 1.5, f * 3.5, len * 0.7, 0.18 * vol, 0.02); // a second, smaller bubble
  },
  // Laser zaps: a fast dive from high to low.
  zaps(sound, kind, vol = 1) {
    const len = kind === "open" ? 0.1 : 0.04;
    sweep(sound, "square", 4200, 380, len, 0.12 * vol, 0.0005);
    sweep(sound, "sine", 3000, 300, len, 0.2 * vol, 0.0005);
  },
  // Chopped zoo samples, pitched up so they tick like hats.
  birds(sound, kind, vol = 1) {
    animal(sound, "bird", 1.5 + Math.random() * 0.7, kind === "open" ? 260 : 90, 0.7 * vol);
  },
  frogs(sound, kind, vol = 1) {
    animal(sound, "frog", 1.3 + Math.random() * 0.4, kind === "open" ? 220 : 100, 0.8 * vol);
  },
  zoo(sound, kind, vol = 1) {
    animal(sound, pick(ANIMALS), 1.8 + Math.random() * 0.8, kind === "open" ? 200 : 70, 0.7 * vol);
  },
};

// Bleepbloops: novelty sounds in the style of budots, the Davao (Philippines) street-dance
// music: cheap-keyboard stabs, laser "pews", tongue clicks, referee whistles, airhorns, sirens
// and cartoon boings. fire(sound, vol). Pitched ones stay in F minor pentatonic.
const BLEEPBLOOPS = {
  // Laser "pew": a fast square dive, the most budots sound there is.
  pew(sound, vol = 1) {
    sweep(sound, "square", 2600, 320, 0.09, 0.14 * vol, 0.001, 0.03);
    sweep(sound, "sine", 2000, 250, 0.09, 0.2 * vol, 0.001, 0.03);
  },
  // "Tsk": a tongue click, short high noise with a tiny pitched pop.
  tsk(sound, vol = 1) {
    blip(sound, "noise", 9000, 0.012, 0.45 * vol);
    blip(sound, "sine", 2400, 0.006, 0.15 * vol);
  },
  // Referee whistle: a quick trill between two notes.
  whistle(sound, vol = 1) {
    const f = 1864; // Bb6
    const v = sound.synth?.({ type: "whistle", tone: f, duration: 0.16, volume: 0.35 * vol, attack: 0.008, decay: 0.05 });
    for (let i = 1; i < 4; i++) later(i * 35, () => v?.update?.({ tone: i % 2 ? f * 1.12 : f }));
  },
  // Casio stab: a tinny, cheap-keyboard minor chord, high and short.
  "casio stab"(sound, vol = 1) {
    for (const n of [0, 3, 7]) {
      const g = semi(698.5, n); // F5 minor triad
      sweep(sound, "square", g, g, 0.07, 0.07 * vol, 0.001, 0.03);
    }
  },
  // Airhorn: a brassy root-and-fifth blast that bends up into pitch.
  airhorn(sound, vol = 1) {
    const vs = [];
    for (const [g, d] of [[349.2, 0.99], [349.2, 1.01], [523.3, 1]]) vs.push([sweep(sound, "sawtooth", g * d * 0.85, g * d * 0.85, 0.28, 0.08 * vol, 0.004, 0.06), g * d]);
    later(25, () => vs.forEach(([v, g]) => v?.update?.({ tone: g })));
  },
  // Siren: a sine climbing for most of a beat.
  siren(sound, vol = 1) {
    const v = sweep(sound, "sine", 500, 500, 0.4, 0.25 * vol, 0.01, 0.06);
    [700, 950, 1250, 1500].forEach((f, i) => later((i + 1) * 70, () => v?.update?.({ tone: f })));
  },
  // The "bu-dots!" chant: "bu" on the hit, "dots" a 16th later. b = a low thump, then u;
  // d = a click, then o, then "ts" = a burst of hiss.
  "budots chant"(sound, vol = 1) {
    const f0 = 165, t0 = Date.now(), gap = stepMs();
    blip(sound, "sine", 110, 0.012, 0.3 * vol);
    vowel(sound, "u", f0, 0.085, vol);
    atTime(t0 + gap, (snd) => {
      blip(snd, "noise", 3000, 0.006, 0.3 * vol);
      vowel(snd, "o", f0 * 1.12, 0.09, vol);
    });
    atTime(t0 + gap + 85, (snd) => sweep(snd, "noise", 7500, 7500, 0.05, 0.3 * vol, 0.004, 0.03));
  },
  // "Hey!": a breathy h, then a shouted e that falls in pitch.
  "hey!"(sound, vol = 1) {
    blip(sound, "noise", 4000, 0.025, 0.2 * vol);
    vowel(sound, "e", 190, 0.14, vol, 1.25);
  },
  // "Oy!": o sliding into i.
  "oy!"(sound, vol = 1) {
    const t0 = Date.now();
    vowel(sound, "o", 175, 0.08, vol, 1.15);
    atTime(t0 + 70, (snd) => vowel(snd, "i", 185, 0.08, vol, 1));
  },
  // Cartoon spring: shoots up, then droops.
  boing(sound, vol = 1) {
    const v = sweep(sound, "sine", 140, 720, 0.22, 0.45 * vol, 0.002, 0.1);
    later(45, () => v?.update?.({ tone: 95 }));
  },
};

// ── Synthesized voice ──────────────────────────────────────────────────────
// The engine's speech (flite) renders on a background thread, so words come out at an
// unpredictable delay, off the beat. Instead, build vowels from sines: a voiced sound is a
// stack of harmonics of the pitch (f0), and a vowel is which harmonics are loud, the ones
// near its formants (resonances of the mouth). That gives a robotic chant that lands
// exactly on the grid, which suits budots' chopped, pitched vocal hooks anyway.
const VOWELS = { // formant frequencies in Hz (F1, F2, F3), for an adult male voice
  u: [300, 870, 2240], o: [520, 880, 2400], e: [530, 1840, 2480], a: [730, 1090, 2440], i: [280, 2250, 2900],
};
function vowel(sound, name, f0, dur, vol, from = 1.12) {
  const F = VOWELS[name], weights = [1, 0.55, 0.28], widths = [110, 150, 220];
  const parts = [];
  for (let k = 1; k * f0 < 3300; k++) {
    const h = k * f0;
    let amp = 0;
    F.forEach((fq, i) => (amp += weights[i] * Math.exp(-((h - fq) ** 2) / (2 * widths[i] ** 2))));
    parts.push([h, amp]);
  }
  // Only the 6 loudest harmonics (was ~11): the 32-voice engine can't spare more per syllable.
  parts.sort((a, b) => b[1] - a[1]).slice(0, 6).forEach(([h, amp]) =>
    sweep(sound, "sine", h * from, h, dur, 0.26 * amp * vol, 0.004, dur * 0.4)); // shouted: starts high, falls onto pitch
}

// Precisely timed extra hits: like the step clock, these are fired with a spin-wait, so a
// second syllable lands exactly a 16th after the first (later() is only frame-accurate).
let precise = []; // { at, fn }
const atTime = (t, fn) => precise.push({ at: t, fn });

// Budots rhythms. Budots bounces on the tresillo (3+3+2), the same Caribbean cell that
// drives dembow and reggaeton, laid over a straight kick. Patterns can still be any length.
const BLEEPBLOOP_PATTERNS = {
  "budots bounce": "x..x..x.x..x..x.", // 3+3+2, twice a bar
  "dembow": "...x..x....x..x.", // the "tsk" answer between the kicks
  "tresillo": "x..x..x.", // one 3+3+2 cell per half bar
  "call out": "x.x.x...........", // a burst at the top of the bar, like a chant
  "stutter end": "............xxxx", // a burst into the next bar
};

// Synths are fire(sound, f, lane), where f is the lane's note in octave 4 (F4 = 349 Hz).
// MC-909 synth sounds. Chords are minor, built on the note, so they sit with the bass
// and the pentatonic candy sounds.
const MINOR = [0, 3, 7], MINOR7 = [0, 3, 7, 10];
const semi = (f, n) => f * Math.pow(2, n / 12);
const SYNTHS = {
  // Rave stab: a short, hard minor chord of detuned saws with a sub-octave square, and a slight dive.
  "rave stab"(sound, f) {
    for (const n of MINOR) {
      const g = semi(f, n);
      sweep(sound, "sawtooth", g * 1.03, g * 1.007, 0.18, 0.11, 0.002, 0.06);
      sweep(sound, "sawtooth", g * 1.03, g * 0.993, 0.18, 0.11, 0.002, 0.06);
    }
    sweep(sound, "square", f * 0.5 * 1.03, f * 0.5, 0.18, 0.1, 0.002, 0.06);
  },
  // Organ stab: a 90s house organ, minor 7th. Each note is three "drawbars": a square plus sines
  // an octave and a twelfth up.
  "organ stab"(sound, f) {
    for (const n of MINOR7) {
      const g = semi(f, n);
      sweep(sound, "square", g, g, 0.15, 0.06, 0.002, 0.05);
      sweep(sound, "sine", g * 2, g * 2, 0.15, 0.06, 0.002, 0.05);
    }
  },
  // Hoover: the rave classic. Detuned saws on root, fifth and octave that bend up into the note
  // (done in stages with timers, slower than the engine's own glide), then hold.
  hoover(sound, f) {
    const voices = [];
    for (const n of [0, 7, 12]) {
      for (const d of [0.988, 1.012]) { // 2 detuned saws per note (was 3)
        const g = semi(f, n) * d * 0.5;
        voices.push([sweep(sound, "sawtooth", g * 0.7, g * 0.7, 0.45, 0.07, 0.004, 0.15), g]);
      }
    }
    later(30, () => voices.forEach(([v, g]) => v?.update?.({ tone: g * 0.9 })));
    later(75, () => voices.forEach(([v, g]) => v?.update?.({ tone: g })));
  },
  // Pluck arp: one note per hit, walking up and down F minor pentatonic over two octaves.
  "pluck arp"(sound, f, lane) {
    const steps = [0, 3, 5, 7, 10, 12, 15, 17, 15, 12, 10, 7, 5, 3];
    const g = semi(f, steps[(lane.count = (lane.count || 0) + 1) % steps.length]);
    sweep(sound, "triangle", g, g, 0.09, 0.3, 0.001);
    sweep(sound, "square", g, g, 0.05, 0.08, 0.001);
  },
  // Pad: a soft minor chord that fades in and holds for the rest of the bar.
  pad(sound, f) {
    const len = (stepMs() * 16) / 1000;
    for (const n of MINOR) {
      const g = semi(f, n);
      sweep(sound, "sawtooth", g * 0.997, g * 0.997, len, 0.05, len * 0.3, len * 0.3);
      sweep(sound, "triangle", g * 1.003, g * 1.003, len, 0.09, len * 0.3, len * 0.3);
    }
  },
};

// Leads are fire(sound, f, seconds), where f is the note's pitch and seconds is how long it's
// held (from the pattern). Each note holds at full level and cuts at the end, so held
// notes sing and short ones bite.
const LEADS = {
  // Supersaw: the trance/hardstyle lead. Five detuned saws plus a sine to anchor the pitch.
  supersaw(sound, f, sec) {
    for (const d of [0.99, 1, 1.01]) sweep(sound, "sawtooth", f * d, f * d, sec, 0.09, 0.004, 0.03); // 3 saws (was 5)
    sweep(sound, "sine", f, f, sec, 0.15, 0.004, 0.03);
  },
  // Screech: a hardstyle screech. Wide-detuned saws an octave apart, each diving onto the note.
  screech(sound, f, sec) {
    for (const [m, d, a] of [[1, 0.985, 0.08], [1, 1.015, 0.08], [2, 0.99, 0.06], [2, 1.01, 0.06]])
      sweep(sound, "sawtooth", f * m * d * 1.3, f * m * d, sec, a, 0.002, 0.03);
  },
  // Acid lead: one saw with a sub-octave square, a little dive into each note. Raw MC-909 acid.
  "acid lead"(sound, f, sec) {
    sweep(sound, "sawtooth", f * 1.03, f, sec, 0.16, 0.002, 0.025);
    sweep(sound, "square", f * 0.5 * 1.03, f * 0.5, sec, 0.08, 0.002, 0.025);
  },
  // Square lead: hollow and chippy, a square with a sine an octave up.
  "square lead"(sound, f, sec) {
    sweep(sound, "square", f, f, sec, 0.12, 0.003, 0.03);
    sweep(sound, "sine", f * 2, f * 2, sec, 0.06, 0.003, 0.03);
  },
  // Sine lead: soft and pure, almost a whistle.
  "sine lead"(sound, f, sec) {
    sweep(sound, "sine", f, f, sec, 0.3, 0.012, 0.04);
    sweep(sound, "triangle", f * 2, f * 2, sec, 0.05, 0.012, 0.04);
  },
};

// Lead melodies. One character per 16th: a note from F natural minor, "-" holds the note
// before it, "." is a rest. From the lane's note (F) up:
//   0 F  1 G  2 Ab  3 Bb  4 C  5 Db  6 Eb  7 F'  8 G'  9 Ab'  a Bb'  b C''  c Db''
// Written to Oskie's melodic taste: held notes sit on chord tones (F, Ab, C); tension is
// only a short half-step sigh (Db->C); phrases fall to a low point and leap back up;
// each half ends on a stable note.
const LEAD_STEPS = { 0: 0, 1: 2, 2: 3, 3: 5, 4: 7, 5: 8, 6: 10, 7: 12, 8: 14, 9: 15, a: 17, b: 19, c: 20 };
const LEAD_PATTERNS = {
  "hook": "5-4-2-------0-..7-----6-4-------", // Db-C sigh, Ab held, down to F, leap to F', settle on C
  "sigh": "c-b-9-------7-6.4-------b-------", // the same shape an octave up, ending with a leap to C''
  "hard riff": "0.0.7.0.0.4.0.2.", // hardstyle stutter around the root
  "rising": "0.2.4.7.2.4.7.9.b-------........", // climbs the chord, holds the top
  "call": "........4-2-0---", // a short answer at the end of the bar
};
// How many 16ths a lead note lasts: itself plus any "-" after it.
function noteSteps(p, i) {
  let n = 1;
  while (p[(i + n) % p.length] === "-" && n < p.length) n++;
  return n;
}

// Basses are fire(sound, freq, vol), where freq is the note's pitch in Hz.
// No filter in the engine, so brightness comes from which waves are stacked.
const BASSES = {
  // Hardstyle offbeat bass, take 2 (take 1 was thin and fuzzy).
  // - Fuzz came from detuned saws beating against each other, so: no detune, one quiet saw.
  // - Thin came from a fade over the whole note and from the low F2 fundamental that laptop speakers
  //   can't play. Now the note HOLDS at full level and only cuts off in the last 45 ms, and the
  //   weight sits in squares/triangles at the note and an octave up (the low mids speakers do play).
  // - Aggression: every layer starts 1.5x sharp and dives onto the note (~17 ms glide), a hard
  //   "zap" at the front, plus a short noise transient.
  hardstyle(sound, f, vol = 1) {
    const len = 0.15, hold = 0.045; // hold = the cutoff at the end
    const hit = (type, mult, volume) => sweep(sound, type, f * mult * 1.5, f * mult, len, volume * vol, 0.001, hold);
    hit("square", 1, 0.34); // hollow, aggressive core
    hit("square", 2, 0.2); // octave up: the part you hear on small speakers
    hit("sawtooth", 2, 0.16); // octave edge (replaces the triangle + low saw, to save voices)
    hit("sine", 1, 0.55); // pitch anchor / low end
    blip(sound, "noise", 2500, 0.012, 0.22 * vol); // attack transient
  },
};

// Notes for pitched lanes, in octave 2 (C2 = 65.41 Hz up to B2): bass range.
const NOTES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const noteHz = (n) => 65.406 * Math.pow(2, NOTES.indexOf(n) / 12);

// ── Patterns ───────────────────────────────────────────────────────────────
// One bar of 16th notes: "x" = hit, "." = rest.
const KICK_PATTERNS = {
  "four on the floor": "x...x...x...x...",
  "half time": "x.......x.......",
  "two step": "x.........x.....",
  "breakbeat": "x.x.......x.....",
  "rolling": "x...x...x...x.xx",
  "offbeat": "..x...x...x...x.",
};
const SNARE_PATTERNS = {
  "every beat": "x...x...x...x...", // on every downbeat, with the kick
  "backbeat": "....x.......x...", // 2 and 4
  "rush": "x...x...x...x.xx", // every beat plus a roll into the next bar
};

// Hat patterns use more letters: x = closed, X = accented closed, o = open, O = accented open.
const HAT_PATTERNS = {
  "groove": "xxOxxxOxxxOxxXOx", // closed 16ths, open hat on every offbeat, a push before the last one
  "16ths": "xxXxxxXxxxXxxxXx", // straight 16ths, accents on the offbeats
  "offbeat open": "..O...O...O...O.", // just the open hat between the kicks
  "shuffle": "x.xXx.xXx.xXx.xX", // skippy, swung-feeling accents
};

// Claps. The original "909 clap" got lost under the snare and hats, so the others are built to
// cut through: louder, brighter, and either held (dirty) or spread wide in stereo.
// A slap: a short noise burst that starts after a delay, done with a longer attack ramp
// (the engine can't start a voice later).
const slap = (sound, tone, delay, vol, pan = 0, len = 0.01) =>
  sound.synth?.({ type: "noise", tone, duration: delay + len, volume: vol, attack: Math.max(0.0005, delay), decay: len * 0.8, pan });
const CLAPS = {
  "909 clap": (sound, vol) => PERCS["909 clap"](sound, vol),
  // Dirty clap: MC-909 pushed hard like the snare. Held-then-cut noise and square grit in the
  // 1-2 kHz range, the spot the snare and hats leave open.
  "dirty clap"(sound, vol = 1) {
    slap(sound, 1800, 0, 0.8 * vol);
    slap(sound, 2000, 0.01, 0.8 * vol);
    slap(sound, 1700, 0.02, 0.8 * vol);
    sweep(sound, "noise", 2200, 2200, 0.2, 0.75 * vol, 0.028, 0.07); // held body
    sweep(sound, "noise", 4500, 4500, 0.14, 0.4 * vol, 0.028, 0.06); // brightness
    sweep(sound, "square", 1150, 1100, 0.06, 0.1 * vol, 0.028, 0.04); // grit
    sweep(sound, "square", 2250, 2200, 0.04, 0.07 * vol, 0.028, 0.03); // grit
  },
  // Big clap: a crowd of hands. Slaps panned left, right and centre for width, and a long bright tail.
  "big clap"(sound, vol = 1) {
    slap(sound, 1600, 0, 0.7 * vol, -0.5);
    slap(sound, 2100, 0.008, 0.7 * vol, 0.5);
    slap(sound, 1400, 0.016, 0.7 * vol, -0.2);
    slap(sound, 2600, 0.024, 0.7 * vol, 0.3);
    sound.synth?.({ type: "noise", tone: 2400, duration: 0.3, volume: 0.6 * vol, attack: 0.03, decay: 0.27, pan: -0.35 });
    sound.synth?.({ type: "noise", tone: 3200, duration: 0.3, volume: 0.6 * vol, attack: 0.03, decay: 0.27, pan: 0.35 });
  },
  // Real clap: one person clapping. A real clap is a few fast, slightly irregular skin
  // impacts (5-15 ms apart), each a bandlimited noise burst around 1-2.5 kHz with a short
  // palm-cavity ring, then a little room. It's humanized: the gaps, levels, ring pitch
  // and pan change slightly on every hit, so no two claps are identical.
  "real clap"(sound, vol = 1) {
    const r = (a, b) => a + Math.random() * (b - a);
    const pan = r(-0.08, 0.08);
    let t = 0;
    const n = 3 + Math.floor(Math.random() * 2); // 3-4 impacts
    for (let i = 0; i < n; i++) {
      slap(sound, r(1800, 2600), t, r(0.5, 0.75) * vol, pan, r(0.006, 0.01));
      t += r(0.005, 0.014);
    }
    const ring = r(1000, 1500);
    sound.synth?.({ type: "triangle", tone: ring, duration: t + 0.03, volume: 0.07 * vol, attack: t, decay: 0.028, pan }); // palm ring
    // Room: a quick bright decay plus a softer, longer one (together roughly exponential).
    sound.synth?.({ type: "noise", tone: 2400, duration: t + 0.12, volume: 0.42 * vol, attack: t, decay: 0.12, pan });
    sound.synth?.({ type: "noise", tone: 1500, duration: t + 0.26, volume: 0.16 * vol, attack: t + 0.01, decay: 0.25, pan: -pan });
  },
  // Hand claps: two or three people clapping together, never perfectly in time; each one
  // a smaller real clap, offset 0-25 ms and placed across the stereo field.
  "hand claps"(sound, vol = 1) {
    const r = (a, b) => a + Math.random() * (b - a);
    const people = [[-0.45, 0], [0.4, r(0.006, 0.02)], [0.05, r(0.01, 0.025)]];
    for (const [pan, start] of people) {
      let t = start;
      for (let i = 0; i < 2; i++) { // 2 slaps per person (was 3), to save voices
        slap(sound, r(1600, 2700), t, r(0.36, 0.5) * vol, pan, r(0.006, 0.009));
        t += r(0.005, 0.012);
      }
      sound.synth?.({ type: "noise", tone: r(2000, 2800), duration: t + 0.1, volume: 0.22 * vol, attack: t, decay: 0.1, pan });
    }
  },
  // Snap clap: tight and cracking, a clap with a snare-like crack on top. Short, so it pokes out without washing.
  "snap clap"(sound, vol = 1) {
    blip(sound, "noise", 9000, 0.005, 0.7 * vol); // crack
    slap(sound, 3000, 0, 0.75 * vol);
    slap(sound, 3400, 0.007, 0.75 * vol);
    sweep(sound, "noise", 3800, 3800, 0.09, 0.7 * vol, 0.014, 0.05);
    sweep(sound, "triangle", 1900, 1800, 0.03, 0.18 * vol, 0.0005, 0.02); // snap tone
  },
};
const CLAP_PATTERNS = {
  "2 and 4": "....x.......x...",
  "4 only": "............x...",
  "2 and 4 + ghost": "....x.......x.x.", // a quiet-ish extra slap before the bar turns
};

const PERC_PATTERNS = {
  "syncopated": "...x..x....x.x..", // off-grid knocks that sit between kick, snare and bass
  "clave": "x..x..x...x.x...", // 3-2 son clave
  "end of bar": "............x.xx", // a fill into the next bar
  "tumble": ".x..x.x..x..x.x.", // busy, rolling
};

const WEIRD_HAT_PATTERNS = {
  "scatter": ".x...xX..x.o...x", // sparse and off-grid, around the main hats
  "16ths": "xxXxxxXxxxXxxxXx",
  "offbeat": "..x...x...x...x.",
  "answer": "........x.xX.o..", // quiet first half, a little phrase in the second
};


const SYNTH_PATTERNS = {
  "syncopated stabs": "x..x..x...x..x..",
  "offbeat stabs": "..x...x...x...x.", // with the bass: full hardstyle
  "answer": "........x..x.x..", // quiet first half, a phrase in the second
  "one per bar": "x...............", // for pads and hoovers
  "16th arp": "xxxxxxxxxxxxxxxx", // for the pluck arp
};

const BASS_PATTERNS = {
  "offbeat": "..x...x...x...x.", // the classic: between every kick
  "rolling": ".xxx.xxx.xxx.xxx", // fills every 16th the kick leaves free
  "gallop": "..xx..xx..xx..xx", // two stabs after each kick
};

// The lanes, as a musician would describe them. Each has its own lists to pick from.
// `fields` are the clickable words shown after the lane name, in order.
const lanes = [
  { name: "kick", sounds: KICKS, patterns: KICK_PATTERNS, sound: "tr 909", pattern: "four on the floor",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound) },
  { name: "snare", sounds: SNARES, patterns: SNARE_PATTERNS, sound: "mc 909 dist", pattern: "rush",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound) },
  { name: "clap", sounds: CLAPS, patterns: CLAP_PATTERNS, sound: "909 clap", pattern: "2 and 4",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound) },
  { name: "hats", sounds: HATS, patterns: HAT_PATTERNS, sound: "909 hats", pattern: "shuffle",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l, ch) =>
      l.sounds[l.sound](sound, ch === "o" || ch === "O" ? "open" : "closed", ch === "X" || ch === "O" ? 1.25 : 0.8) },
  { name: "candy", was: ["hats 2"], sounds: WEIRD_HATS, patterns: WEIRD_HAT_PATTERNS, sound: "bubbles", pattern: "scatter",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l, ch) =>
      l.sounds[l.sound](sound, ch === "o" || ch === "O" ? "open" : "closed", ch === "X" || ch === "O" ? 1.25 : 0.8) },
  { name: "bleepbloops", was: ["weirdo"], sounds: BLEEPBLOOPS, patterns: BLEEPBLOOP_PATTERNS, sound: "pew", pattern: "budots bounce",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound) },
  { name: "perc", sounds: PERCS, patterns: PERC_PATTERNS, sound: "909 rim", pattern: "syncopated",
    fields: ["sound", "pattern"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound) },
  { name: "synth", sounds: SYNTHS, patterns: SYNTH_PATTERNS, sound: "rave stab", pattern: "syncopated stabs", note: "F",
    fields: ["sound", "pattern", "note"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound, noteHz(l.note) * 4, l) },
  { name: "lead", sounds: LEADS, patterns: LEAD_PATTERNS, sound: "supersaw", pattern: "hook", note: "F",
    fields: ["sound", "pattern", "note"], lastFiredAt: 0,
    fire: (sound, l, ch, n) => {
      const p = l.patterns[l.pattern];
      const f = noteHz(l.note) * 4 * Math.pow(2, (LEAD_STEPS[ch] ?? 0) / 12);
      const sec = (noteSteps(p, n % p.length) * stepMs()) / 1000;
      l.sounds[l.sound](sound, f, sec);
    } },
  { name: "bass", sounds: BASSES, patterns: BASS_PATTERNS, sound: "hardstyle", pattern: "offbeat", note: "F",
    fields: ["sound", "pattern", "note"], lastFiredAt: 0,
    fire: (sound, l) => l.sounds[l.sound](sound, noteHz(l.note)) },
];
// ── Groove ─────────────────────────────────────────────────────────────────
// Feel for the whole track, like the MC-909's shuffle. Swing delays every second 16th
// (the "e" and "a" of each beat): `swing` is how far through the pair it lands, so
// 0.5 = straight and 0.66 = full triplet shuffle. "drunk" adds a small random push or drag
// to every step instead (the same amount for a given step, so the clock stays consistent).
const GROOVES = {
  "straight": { swing: 0.5 },
  "light swing": { swing: 0.54 },
  "swing": { swing: 0.58 },
  "heavy swing": { swing: 0.62 },
  "shuffle": { swing: 0.66 },
  "drunk": { swing: 0.52, drunk: 9 }, // up to +-9 ms per step
};
// The groove isn't a lane, but it's picked from a list the same way, so it lives in a small
// lane-like object that the menu code can change.
const feel = { name: "groove", groove: "straight", sweep: "high pass", fields: ["groove", "sweep"] };
const SWEEPS = ["high pass", "low pass"];

const choices = (l, field) =>
  field === "sound" ? Object.keys(l.sounds) : field === "pattern" ? Object.keys(l.patterns)
  : field === "groove" ? Object.keys(GROOVES) : field === "sweep" ? SWEEPS : NOTES;

// ── Clock ──────────────────────────────────────────────────────────────────
// Runs in 16th-note steps and starts as soon as the piece loads. Space stops/starts it.
//
// Timing: sim() only runs once per frame (~16.7 ms at 60 fps), and the engine has
// no way to schedule a voice for later. So when a hit falls before the next frame
// is due, wait out the last few ms here and fire on time. The audio thread starts
// a voice within ~1 ms. Step times are absolute (startAt + n * stepMs), so nothing
// drifts. Known leftover: the system freezes the piece ~120 ms every ~3 s, and that
// can make the odd hit late. That needs an engine fix (see CLAUDE.md).
let bpm = 124;
let playing = true;
let startAt = Date.now();
let nextStep = 0; // index of the next 16th step to consider
let lastFrameAt = 0;
let frameMs = 16.7; // measured average frame interval
const stepMs = () => 60000 / bpm / 4;
const BPM_MIN = 60, BPM_MAX = 200;

// Change tempo without a jump: keep the current position in the bar, so the
// groove carries straight on at the new speed.
function setBpm(v) {
  v = Math.max(BPM_MIN, Math.min(BPM_MAX, Math.round(v)));
  if (v === bpm) return;
  const now = Date.now();
  const pos = (now - startAt) / stepMs(); // steps since start, at the old tempo
  bpm = v;
  startAt = now - pos * stepMs();
}
// When step n plays: on the grid, plus the groove's offset.
function stepTime(n) {
  const g = GROOVES[feel.groove] || GROOVES.straight;
  let t = startAt + n * stepMs();
  if (n % 2 === 1) t += (g.swing - 0.5) * 2 * stepMs(); // swing: push the off-16th later
  if (g.drunk) {
    const h = Math.sin(n * 12.9898) * 43758.5453; // a fixed pseudo-random value per step
    t += (h - Math.floor(h) - 0.5) * 2 * g.drunk;
  }
  return t;
}
// Mute and solo, like a mixer: if any lane is soloed, only soloed lanes play; mute always wins.
const audible = (l) => !l.mute && (!lanes.some((o) => o.solo) || !!l.solo);
// ── Fill ───────────────────────────────────────────────────────────────────
// Hold Tab for a fill: while it's down, the drum lanes swap to fill patterns (one bar, by step
// position). A quick tap (under 250 ms) plays the fill to the end of the bar instead.
// Lanes not listed keep their own pattern, so the music carries on under the fill.
// Nine fills: press 1-9 at any time (or click a number on screen) to pick the one Tab plays.
// It switches straight away, even mid-fill, and stays picked. A lane whose fill is all "." goes silent during that fill.
const FILL_SETS = [
  { name: "roll", snareBuild: true, // 1: the original, a building snare roll
    kick: "x.......x...x.xx", // drops out, then stumbles back in
    snare: "xxxxxxxxxxxxxxxx", // a 16th roll that builds (see fillVolume)
    clap: "............x.x.",
    hats: "o...o...o...o.oo", // open hats, quickening at the end
    bleepbloops: "....x...x...x.xx", // (was 8ths into 16ths: too many voices with the chants)
    perc: "x.xxx.xxx.xxxxxx" },
  { name: "stutter", // 2: everything chops into 8ths, then 16ths
    kick: "x.x.x.x.x.x.x.x.",
    snare: "x.x.x.x.xxxxxxxx",
    clap: "x..x..x..x..x.x.",
    hats: "xxxxxxxxxxxxxxxx",
    bleepbloops: "x.x.....x.x.....",
    perc: "....x.......x.xx" },
  { name: "toms", toms: true, // 3: a falling tom run in the second half of the bar
    kick: "x...x...x.......",
    snare: "................",
    clap: "................",
    hats: "x.x.x.x.x.x.....",
    bleepbloops: "................",
    perc: "........xxxxxxxx" }, // played as toms, falling in pitch (see tomFill)
  { name: "drop out", // 4: the beat vanishes, then a snare pickup brings it back
    kick: "x...............",
    snare: "............x.xx",
    clap: "................",
    hats: "................",
    bleepbloops: "................",
    perc: "................" },
  { name: "triplets", // 5: groups of three against the 4/4
    kick: "x.....x.....x...",
    snare: "x..x..x..x..x.xx",
    clap: "......x.....x...",
    hats: "x..x..x..x..x..x",
    bleepbloops: "................",
    perc: "..x..x..x..x..x." },
  { name: "clap roll", clapBuild: true, // 6: claps roll in the second half, getting louder
    kick: "x...x...x.......",
    snare: "................",
    clap: "x.......xxxxxxxx",
    hats: "o.o.o.o.........",
    bleepbloops: "................",
    perc: "................" },
  { name: "kick roll", // 7: the kick doubles into 16ths for the last two beats
    kick: "x...x...xxxxxxxx",
    snare: "....x.......x...",
    clap: "................",
    hats: "x.x.x.x.x.x.x.x.",
    bleepbloops: "................",
    perc: "................" },
  { name: "breakbeat", // 8: an amen-style break
    kick: "x.x.......xx....",
    snare: "....x..x.x..x..x",
    clap: "................",
    hats: "x.x.x.x.x.x.x.x.",
    bleepbloops: "................",
    perc: "................" },
  { name: "everything", snareBuild: true, // 9: all the drums at once, the snare building (kept lean: 32 voices)
    kick: "x...x...x...x.xx",
    snare: "x.x.x.x.xxxxxxxx", // 8ths into a 16th roll (a full-bar 16th roll ran past 32 voices)
    clap: "....x.......x...",
    hats: "xxxxxxxxxxxxxxxx",
    bleepbloops: "x...x...x.x.x.x.",
    perc: "..x...x...x...xx" },
];
let fillSet = 0;
const FILLS_NOW = () => FILL_SETS[fillSet] || FILL_SETS[0];
const FILL_TAP_MS = 250;
const fill = { held: false, downAt: 0, until: 0 }; // until: a tapped fill runs until this time
let fillNow = false; // is the step being played right now part of a fill?
const fillActive = (t) => fill.held || t < fill.until;
// Fill 3's toms: whatever the perc lane's sound is, the fill plays 909 toms that fall from high to low.
const tomFill = (sound, n) => PERCS["909 tom"](sound, 1, 1.9 - 1.1 * (((n % 16) - 8) / 7));
// The snare roll builds from soft to loud across the bar.
const fillVolume = (n) => 0.5 + 0.7 * ((n % 16) / 15);

// Patterns loop at their own length (16 for most; some are shorter or longer, e.g. tresillo is 8).
const stepChar = (l, n) => {
  const fs = FILLS_NOW();
  const p = fillNow && fs[l.name] ? fs[l.name] : l.patterns[l.pattern];
  return p[n % p.length];
};
const hitOn = (l, n) => {
  const c = stepChar(l, n);
  return c === "?" ? Math.random() < 0.4 : c !== "." && c !== "-"; // "-" = still holding the last note
};

// Timing stats, written every ~4 s to /pieces/acid-stats.json so they can be read over the LAN
// (curl http://IP/pieces/acid-stats.json). console.log never reaches /logs on this device.
const stats = { hits: 0, late: 0, maxLateMs: 0, maxFrameMs: 0, frameMs: 0, bpm: 0, loadedFrom: null, lanes: "", writes: 0, writeOk: null };
let statsAt = 0;

function writeStats(system, now) {
  statsAt = now;
  stats.frameMs = Math.round(frameMs * 10) / 10;
  stats.bpm = bpm;
  stats.lanes = lanes.map((l) => [l.name, ...l.fields.map((f) => l[f])].join(" // ")).join(" | ");
  stats.writes++;
  try {
    const r = system?.writeFile?.("/pieces/acid-stats.json", JSON.stringify(stats), false); // false: no disk sync, it's only for reading over the LAN
    stats.writeOk = r === undefined ? "undef" : !!r;
  } catch (err) {
    stats.writeOk = "threw: " + err;
  }
  stats.maxFrameMs = 0; // per-window peak
}

// ── Saved settings ─────────────────────────────────────────────────────────
// Every push reloads the piece from scratch, which used to put every lane back to its default.
// Now each change (a pick from a list, a tempo drag) is saved, and boot() loads it back.
// /mnt survives reboots; the /pieces copy can be read over the LAN. Lanes or choices that
// no longer exist are skipped, and new lanes start on their defaults.
const STATE_FILES = ["/mnt/acid-state.json", "/pieces/acid-state.json"];
let sys = null; // the system API, kept from boot/sim so menu clicks can save

function saveState() {
  const state = { bpm, groove: feel.groove, sweep: feel.sweep, fill: fillSet, lanes: {} };
  for (const l of lanes) {
    state.lanes[l.name] = {};
    for (const f of l.fields) state.lanes[l.name][f] = l[f];
    state.lanes[l.name].mute = !!l.mute;
    state.lanes[l.name].solo = !!l.solo;
  }
  const json = JSON.stringify(state);
  for (const path of STATE_FILES) {
    try { sys?.writeFile?.(path, json); } catch (err) {}
  }
}

function loadState() {
  for (const path of STATE_FILES) {
    let state = null;
    try { state = JSON.parse(sys?.readFile?.(path) || "null"); } catch (err) {}
    if (!state) continue;
    if (state.bpm >= BPM_MIN && state.bpm <= BPM_MAX) bpm = Math.round(state.bpm);
    if (GROOVES[state.groove]) feel.groove = state.groove;
    if (SWEEPS.includes(state.sweep)) feel.sweep = state.sweep;
    if (FILL_SETS[state.fill]) fillSet = state.fill;
    for (const l of lanes) {
      // `was` lists a lane's old names, so a rename keeps its saved settings.
      const saved = state.lanes?.[l.name] ?? (l.was || []).map((n) => state.lanes?.[n]).find(Boolean);
      if (!saved) continue;
      for (const f of l.fields) if (choices(l, f).includes(saved[f])) l[f] = saved[f];
      l.mute = saved.mute === true;
      l.solo = saved.solo === true;
    }
    return path;
  }
  return null;
}

function restartClock() {
  startAt = Date.now() + 50; // a little lead-in so the first hit is on time too
  nextStep = 0;
}

// ── Filter (hold Shift) ────────────────────────────────────────────
// Hold Shift and move up for a high pass, down for a low pass, relative to where the cursor was
// when Shift went down. (There was a delay too, the engine echo; it's off now, see ECHO_WITH_FILTER.)
//
// The engine has no filter, but every sound here is built per hit, so the filter is applied
// as each voice is made (fx() wraps sound.synth):
//  - level: each voice is scaled by a 2-pole response at its pitch (a noise voice's "tone" is
//    its own low-pass cutoff, so a low pass also pulls that cutoff down);
//  - brightness: under a low pass close to a voice's pitch, saws and squares turn into
//    triangles and then sines, which is what a real low pass does to their harmonics.
// A sweep therefore affects new hits; most sounds are short, so it feels live.
// Take 3 (Oskie): Shift is an AUTO sweep, no mouse. Hold it and the filter sweeps on its own over
// SWEEP_BARS; let go and it glides back open over RETURN_BEATS. The direction (high pass or
// low pass) is picked on the top line ("sweep: high pass").
const SWEEP_BARS = 0.5; // half a bar (Oskie); was 2, then 1
const RETURN_BEATS = 0; // 0 = snap straight back open on release, no tail (Oskie); was 1
const filt = { on: false, at: 0, releaseAt: 0, relLevel: 0 };
const sweepLevel = (now) => {
  const full = SWEEP_BARS * 16 * stepMs();
  if (filt.on) return Math.min(1, (now - filt.at) / full);
  if (!filt.releaseAt || RETURN_BEATS <= 0) return 0; // no return glide: open the moment Shift is let go
  const back = filt.relLevel * (1 - (now - filt.releaseAt) / (RETURN_BEATS * 4 * stepMs()));
  return Math.max(0, back);
};
const ECHO_WITH_FILTER = 0; // was 0.38
let echoMix = -1; // unknown at boot, so the first sim call sets it

// -1..1: <0 low pass, >0 high pass, 0 = open.
const filterAmount = () => {
  const lvl = sweepLevel(Date.now());
  return feel.sweep === "low pass" ? -lvl : lvl;
};
function filterSettings() {
  const a = filterAmount();
  // The riser adds its own creeping high pass (up to ~700 Hz), unless you're pulling a low pass.
  const auto = riser.amount > 0 && a > -0.06 ? 40 * Math.pow(700 / 40, riser.amount) : 0;
  if (a < -0.06) return { type: "low pass", fc: 20000 * Math.pow(150 / 20000, -a) }; // 20 kHz -> 150 Hz
  const user = a > 0.06 ? 30 * Math.pow(4000 / 30, a) : 0; // 30 Hz -> 4 kHz
  const fc = Math.max(user, auto);
  return fc > 0 ? { type: "high pass", fc } : null; // small dead zone around the start point
}

// ── Riser (also on Shift) ──────────────────────────────────────────────────
// In the spirit of Dada Life's Endless Smile: one gesture that builds tension the longer you hold.
// `amount` climbs from 0 to 1 over RISER_BARS while Shift is down, and drives:
//  - a white-noise whoosh that gets louder and brighter (four noise layers, dark to bright,
//    crossfaded, because a noise voice's brightness is fixed when it's created);
//  - a tone sweeping up three octaves;
//  - a creeping high pass on everything (see filterSettings);
//  - a snare build: 8ths from halfway, 16ths from 80%, getting louder.
// Letting go of Shift cuts it all at once: the drop.
const RISER_BARS = 8; // was 4: Oskie wanted a longer build
const riser = { at: 0, amount: 0, voices: null, dropAt: 0 };
// Saved for something else (Oskie): Shift is a plain auto filter sweep for now. Set true to put the
// riser (whoosh, rising tone, auto high pass, snare build, drop on the bar line) back on Shift.
const RISER_ENABLED = false; // dropAt: when a released riser drops (next bar line)
const RISER_DUCK = 0.45; // at full build, everything else plays at 55% so the riser sits on top

function riserUpdate(raw, now) {
  if (!RISER_ENABLED || !filt.on) {
    riser.amount = 0;
    return;
  }
  riser.amount = Math.min(1, (now - riser.at) / (RISER_BARS * 16 * stepMs()));
  stats.riserMax = Math.max(stats.riserMax || 0, Math.round(riser.amount * 100));
}

// The riser's sound, played as short overlapping pieces on every 16th (riserStep, from the
// step clock). Take 2 used endless voices, but the engine has only 32 voices and steals the
// oldest, so as soon as the beat played they were stolen: audible when stopped, gone when
// playing. Pieces 1.6 steps long with soft ends overlap into a continuous whoosh, and a stolen
// piece is replaced a 16th later. A bonus: each piece's noise brightness can be set exactly.
function riserStep(raw) {
  const a = riser.amount;
  if (!filt.on || a <= 0) return;
  const len = (stepMs() * 1.6) / 1000, edge = len * 0.35;
  raw.synth?.({ type: "noise", tone: 700 * Math.pow(17, a), duration: len, volume: 0.15 + 0.75 * a, attack: edge, decay: edge }); // 700 Hz -> 12 kHz
  raw.synth?.({ type: "sawtooth", tone: 220 * Math.pow(8, a), duration: len, volume: 0.06 + 0.2 * a, attack: edge, decay: edge }); // 220 -> 1760 Hz
}

// The drop: a big low boom and a long crash of noise, on the downbeat where the riser ends.
// Switched off at Oskie's request; set DROP_IMPACT = true to bring it back.
const DROP_IMPACT = false;
function impact(sound) {
  sweep(sound, "sine", 160, 42, 1.3, 1.0, 0.002, 1.1); // boom
  sweep(sound, "triangle", 120, 45, 0.25, 0.35, 0.002, 0.2); // knock
  sweep(sound, "noise", 9000, 9000, 1.6, 0.4, 0.002, 1.5); // crash
  sweep(sound, "noise", 3500, 3500, 0.9, 0.3, 0.002, 0.85);
}

// The snare build: which steps get a hit at the current amount, and how loud.
const riserSnare = (n) => (riser.amount >= 0.8 ? true : riser.amount >= 0.5 ? n % 2 === 0 : false);
const fmtHz = (f) => (f >= 1000 ? (f / 1000).toFixed(1) + "k" : Math.round(f) + "");

// Take 2 of the filter (take 1 "not very exciting"): steeper, live, and resonant.
//  - Steeper: 4-pole (24 dB/octave) instead of 2-pole, so the sweep bites harder.
//  - Live: every voice made in the last couple of seconds is tracked, and sim() re-sets its
//    volume each frame as the cutoff moves, so held sounds (kick tails, leads, pads) sweep
//    smoothly too, not just new hits.
//  - Resonant: a real resonant filter rings at its cutoff when the beat hits it. So on every
//    16th while the filter is in, we ping a short tone AT the cutoff (resPing). As the cutoff
//    moves, that ringing whistles up or down: the squelchy, zappy part of a classic sweep.
// Take 3 (Oskie: "more aggressive resonance and noise, like a CDJ sweep filter"), without the
// pitched pings: the resonance now lives in the music itself. A resonant filter boosts whatever
// sits right at its cutoff, so each voice gets a resonant peak when its pitch (or, for saws and
// squares, one of its first harmonics) is within about half an octave of the cutoff. As the
// cutoff sweeps, each sound "pops" and wails as the peak passes through it, like a DJ mixer filter.
const RES = 2.6; // peak boost: 1 + RES at the cutoff
function resPeak(fc, tone, wave) {
  const bell = (f) => Math.exp(-(Math.log2(f / fc) ** 2) / (2 * 0.33 ** 2));
  let p = bell(tone);
  if (wave === "sawtooth" || wave === "square") for (let k = 2; k <= 6; k++) p = Math.max(p, bell(tone * k) / k * 1.5);
  return 1 + RES * Math.min(1, p);
}
function filterGain(f, tone, wave) {
  if (!f) return 1;
  let g;
  if (f.type === "low pass") {
    if (wave === "noise" || wave === "noise-white") g = Math.pow(1 / Math.sqrt(1 + Math.pow(tone / f.fc / 4, 8)), 0.5);
    else g = 1 / Math.sqrt(1 + Math.pow(tone / f.fc, 8));
  } else {
    g = 1 / Math.sqrt(1 + Math.pow(f.fc / tone, 8));
    if (wave === "sawtooth" || wave === "square" || wave === "noise") g = Math.max(g, 0.15); // harmonics above the cut survive
  }
  // Resonance grows with how far the filter is in, so a barely-touched filter stays clean.
  const depth = Math.min(1, Math.abs(filterAmount()) * 2);
  const peak = 1 + (resPeak(f.fc, tone, wave) - 1) * depth;
  return Math.min(2.5, g * peak);
}

// The CDJ "noise" side: a hiss that goes through the filter with the music, louder the further
// the filter is in. Retriggered on every 16th in short overlapping pieces (stealing-proof, like
// the riser): rumbling under a low pass, a bright airy hiss under a high pass.
function filterNoise(raw) {
  const f = filterSettings();
  if (!f) return;
  const depth = Math.abs(filterAmount());
  const len = (stepMs() * 1.6) / 1000, edge = len * 0.35;
  const tone = f.type === "low pass" ? Math.max(200, f.fc) : Math.min(16000, f.fc * 4 + 2000);
  raw.synth?.({ type: "noise", tone, duration: len, volume: 0.06 + 0.34 * depth, attack: edge, decay: edge });
}

function filtered(o, f) {
  if (!o) return o;
  const out = { ...o };
  const tone = o.tone || 440;
  const wave = o.type;
  if (f.type === "low pass") {
    if (wave === "noise" || wave === "noise-white") out.tone = Math.min(tone, f.fc);
    else {
      const r = f.fc / tone; // how much room above the fundamental
      if ((wave === "sawtooth" || wave === "square") && r < 2.5) out.type = r < 1.2 ? "sine" : "triangle";
      else if (wave === "triangle" && r < 1.2) out.type = "sine";
    }
  }
  out.volume = (o.volume ?? 0.5) * filterGain(f, tone, wave);
  return out;
}

// Voices still sounding, so the filter can keep moving them: { v, tone, wave, base, end }.
let liveVoices = [];
let filterWasOn = false;

// The sound API the lanes play through: always wrapped, so every voice is tracked.
function fx(sound) {
  const f = filterSettings();
  const duck = 1 - RISER_DUCK * riser.amount;
  return {
    ...sound,
    synth: (o) => {
      if (!o) return null;
      const base = (o.volume ?? 0.5) * duck;
      const v = f ? filtered(o, f) : { ...o };
      v.volume = f ? v.volume * duck : base;
      if (f && v.volume < 0.01) return null; // filtered to near silence: don't spend one of the 32 voices on it
      const voice = sound.synth(v);
      const dur = o.duration === Infinity ? 2 : o.duration || 0.5;
      if (voice && dur > 0.06) liveVoices.push({ v: voice, tone: o.tone || 440, wave: v.type, base, end: Date.now() + dur * 1000 });
      return voice;
    },
  };
}

// Each frame: move the volume of every still-sounding voice to match the current cutoff.
function filterLive(now) {
  liveVoices = liveVoices.filter((x) => x.end > now);
  if (liveVoices.length > 40) liveVoices = liveVoices.slice(-40);
  const f = filterSettings();
  if (!f && !filterWasOn) return;
  for (const x of liveVoices) x.v?.update?.({ volume: x.base * filterGain(f, x.tone, x.wave) });
  filterWasOn = !!f;
}

const RES_PINGS = false;
// The resonance: a short ping at the cutoff on a 16th, louder the further the filter is in.
function resPing(raw) {
  const f = filterSettings();
  if (!f) return;
  const depth = Math.abs(filterAmount());
  const tone = Math.max(70, Math.min(5000, f.fc));
  raw.synth?.({ type: "triangle", tone, duration: 0.11, volume: (0.06 + 0.2 * depth) * (tone < 200 ? 1.6 : 1), attack: 0.001, decay: 0.1 });
}

function boot({ system }) {
  sys = system;
  stats.loadedFrom = loadState();
  if (!stats.loadedFrom) saveState(); // first run: store the starting setup straight away
  restartClock();
}

function sim({ sound: raw, system }) {
  if (system) sys = system;
  const now = Date.now();
  // The delay was taken off at Oskie's request: keep the engine echo at 0 (it can be left on
  // from another piece). Set ECHO_WITH_FILTER above 0 to bring it back with Shift.
  const wantEcho = filt.on ? ECHO_WITH_FILTER : 0;
  if (wantEcho !== echoMix) {
    echoMix = wantEcho;
    raw.echo?.setMix?.(echoMix);
  }
  riserUpdate(raw, now);
  filterLive(now);
  let sound = fx(raw); // every hit below goes through the filter
  // Run timed actions that are due (chopped zoo samples, pitch bends, stutters).
  if (timers.length) {
    const due = timers.filter((t) => now >= t.at);
    timers = timers.filter((t) => now < t.at);
    for (const t of due) t.fn(sound);
  }
  if (lastFrameAt) {
    const dt = now - lastFrameAt;
    if (dt > 0 && dt < 100) frameMs += (dt - frameMs) * 0.05; // smooth average
    if (dt > stats.maxFrameMs) stats.maxFrameMs = dt;
  }
  lastFrameAt = now;
  if (now - statsAt > 4000) writeStats(system, now);
  if (!playing) return;

  // Steps already passed (e.g. after a stall) are skipped, not played in a burst.
  while (stepTime(nextStep + 1) <= now) nextStep++;

  // Play every step that lands before the next frame.
  while (stepTime(nextStep) - Date.now() <= frameMs * 1.25) {
    const due = stepTime(nextStep);
    // The drop: Shift was let go, and this is the bar line it was waiting for.
    let dropping = false;
    if (riser.dropAt && due >= riser.dropAt - 1) {
      riser.dropAt = 0;
      filt.on = false;
      riserUpdate(raw, due); // riser off (its pieces just stop being played)
      sound = fx(raw); // filter off from here on (fx still tracks voices)
      dropping = true;
    }
    fillNow = fillActive(due);
    const riserOn = filt.on && riser.amount > 0;
    // Resonance pings are off: Oskie didn't like the pitched notes they made. RES_PINGS = true brings them back.
    const resOn = RES_PINGS && !!filterSettings() && Math.abs(filterAmount()) > 0.04;
    const noiseOn = !!filterSettings() && Math.abs(filterAmount()) > 0.04;
    const build = riserSnare(nextStep); // the riser's snare build replaces the snare's own pattern
    const hits = lanes.filter((l) => audible(l) && (l.name === "snare" && riser.amount >= 0.5 ? build : hitOn(l, nextStep)));
    if (hits.length || dropping || riserOn || resOn || noiseOn) {
      while (Date.now() < due) {} // at most ~one frame; a skipped frame is harmless here
      const fired = Date.now();
      if (dropping && DROP_IMPACT) impact(raw);
      if (riserOn) riserStep(raw);
      if (resOn) resPing(raw);
      if (noiseOn) filterNoise(raw);
      for (const l of hits) {
        if (l.name === "snare" && riser.amount >= 0.5) l.sounds[l.sound](sound, 0.5 + 0.7 * riser.amount); // riser build
        else if (fillNow && l.name === "snare" && FILLS_NOW().snareBuild) l.sounds[l.sound](sound, fillVolume(nextStep)); // the building roll
        else if (fillNow && l.name === "perc" && FILLS_NOW().toms) tomFill(sound, nextStep); // the falling toms
        else if (fillNow && l.name === "clap" && FILLS_NOW().clapBuild) l.sounds[l.sound](sound, fillVolume(nextStep)); // the building claps
        else l.fire(sound, l, stepChar(l, nextStep), nextStep);
        l.lastFiredAt = fired;
      }
      stats.hits++;
      const late = fired - due;
      if (late >= 1) stats.late++;
      if (late > stats.maxLateMs) stats.maxLateMs = late;
    }
    nextStep++;
  }
  fillNow = false;

  // Precisely timed extra hits (second syllables of chants, etc.), same spin-wait as steps.
  if (precise.length) {
    precise.sort((a, b) => a.at - b.at);
    while (precise.length && precise[0].at - Date.now() <= frameMs * 1.25) {
      const { at, fn } = precise.shift();
      while (Date.now() < at) {}
      fn(sound);
    }
  }
}

// ── Screen ─────────────────────────────────────────────────────────────────
// Plain words for musicians, laid out on font_1's 6 px character grid:
//   124 bpm // mc 909
//   kick // tr 909 // four on the floor
//   snare // mc 909 dist // rush
//   clap // 909 clap // 2 and 4
//   hats // 909 hats // shuffle
//   candy // bubbles // scatter
//   bleepbloops // pew // budots bounce
//   perc // 909 rim // syncopated
//   synth // rave stab // syncopated stabs // F
//   lead // supersaw // hook // F
//   bass // hardstyle // offbeat // F
// Underlined words open a list when clicked. A lane's name lights up on each of its hits.
const X0 = 8, Y0 = 8, LH = 14; // left margin, top, line height
const CHAR_W = 6, CHAR_H = 10; // font_1 at size 1
const FLASH_MS = 110; // how long a lane name stays lit after its hit
const SEP = " // ";
const LANE_INDENT = 8; // characters reserved in front of each lane for the m / s toggles

let menu = null; // the open list: { lane, field: "sound" | "pattern" | "note", x, y, items }
let regions = []; // clickable areas from the last paint: { x, y, w, h, action }
// Tempo drag: press on the bpm, then slide up (faster) or down (slower).
const BPM_PER_PX = 0.5;
let bpmRegion = null; // where the bpm text sits, from the last paint
let drag = null; // { y0, bpm0, travel } while dragging

const text = (write, t, x, y) => write?.(t, { x, y, size: 1, font: "font_1" });

// Draw text inverted: a green block with black letters.
function invert(ink, box, write, t, x, y) {
  ink(...GREEN);
  box(x - 2, y - 2, t.length * CHAR_W + 3, CHAR_H + 2);
  ink(...BLACK);
  text(write, t, x, y);
  ink(...GREEN);
}

const over = (r) => cursor && cursor.x >= r.x && cursor.x < r.x + r.w && cursor.y >= r.y && cursor.y < r.y + r.h;

function paint({ wipe, ink, box, write, pen, trackpad }) {
  // The engine only sends events while the pad is pressed, so for plain hovering read pen every frame.
  if (pen && (pen.x || pen.y)) cursor = { x: Math.floor(pen.x / scale), y: Math.floor(pen.y / scale) };

  wipe(...BLACK);
  ink(...GREEN);
  regions = [];

  // Tempo: underlined like the other changeable words; inverted while you drag it.
  // The cursor stops at the screen edge, and the bpm sits at the very top, so dragging up by
  // cursor position would barely move. Instead add up the pad's raw movement
  // (trackpad.dy, per frame, in display px, not clamped). Fall back to the cursor if it's missing.
  if (drag && press.down === 2) {
    if (trackpad && typeof trackpad.dy === "number") drag.travel -= trackpad.dy / scale;
    else if (cursor) drag.travel = drag.y0 - cursor.y;
    setBpm(drag.bpm0 + drag.travel * BPM_PER_PX);
  }
  const bpmText = `${bpm} bpm`;
  bpmRegion = { x: X0 - 3, y: Y0 - 3, w: bpmText.length * CHAR_W + 6, h: LH };
  if (drag) invert(ink, box, write, bpmText, X0, Y0);
  else {
    text(write, bpmText, X0, Y0);
    box(X0, Y0 + CHAR_H, bpmText.length * CHAR_W, 1);
  }
  // The sound bank every lane is built on.
  const bank = `${SEP}${BANK_NAME}${SEP}`;
  text(write, bank, X0 + bpmText.length * CHAR_W, Y0);
  // The groove: clickable like a lane's words, opens the groove list.
  const gx = X0 + (bpmText.length + bank.length) * CHAR_W, gw = feel.groove.length * CHAR_W;
  const gOpen = menu && menu.lane === feel && menu.field === "groove";
  if (gOpen) invert(ink, box, write, feel.groove, gx, Y0);
  else {
    text(write, feel.groove, gx, Y0);
    box(gx, Y0 + CHAR_H, gw, 1);
  }
  const gr = { x: gx - 3, y: Y0 - 3, w: gw + 6, h: LH };
  gr.action = () => openMenu(feel, "groove", gx, Y0 + LH + 2);
  regions.push(gr);
  // Sweep direction for Shift, clickable the same way.
  const sweepPre = `${SEP}shift sweep: `;
  text(write, sweepPre, gx + gw, Y0);
  const sx = gx + gw + sweepPre.length * CHAR_W, sw = feel.sweep.length * CHAR_W;
  const sOpen = menu && menu.lane === feel && menu.field === "sweep";
  if (sOpen) invert(ink, box, write, feel.sweep, sx, Y0);
  else {
    text(write, feel.sweep, sx, Y0);
    box(sx, Y0 + CHAR_H, sw, 1);
  }
  const sr = { x: sx - 3, y: Y0 - 3, w: sw + 6, h: LH };
  sr.action = () => openMenu(feel, "sweep", sx, Y0 + LH + 2);
  regions.push(sr);

  // Status goes on a line of its own under the top line: the top line is full now.
  // Fill picker, under the lanes: "fill  1 2 3 4 5 6 7 8 9  roll". The current one is inverted;
  // each number is clickable, and the number keys pick them too.
  const fy = Y0 + LH + 4 + lanes.length * LH + 6;
  text(write, "fill", X0, fy);
  FILL_SETS.forEach((fsx, i) => {
    const nx = X0 + (6 + i * 3) * CHAR_W, t = String(i + 1);
    if (i === fillSet) invert(ink, box, write, t, nx, fy);
    else text(write, t, nx, fy);
    const r = { x: nx - CHAR_W, y: fy - 2, w: 3 * CHAR_W, h: LH };
    r.action = () => { fillSet = i; saveState(); };
    regions.push(r);
  });
  text(write, FILLS_NOW().name, X0 + (6 + FILL_SETS.length * 3 + 1) * CHAR_W, fy);

  const hintX = X0, hintY = fy + LH + 4;
  const fset = filterSettings();
  if (fset) invert(ink, box, write, `${fset.type} ${fmtHz(fset.fc)}`, hintX, hintY);
  else if (playing && fillActive(Date.now())) invert(ink, box, write, `filling: ${FILLS_NOW().name}`, hintX, hintY);
  else if (drag) text(write, "slide up = faster, down = slower", hintX, hintY);
  else if (!playing) text(write, "(stopped, press space)", hintX, hintY);

  // One line per lane, built from segments so we know where each word sits:
  //   kick // tr 909 // four on the floor
  //   snare // mc 909 dist // rush
  //   clap // 909 clap // 2 and 4
  //   hats // 909 hats // shuffle
  //   candy // bubbles // scatter
  //   bleepbloops // pew // budots bounce
  //   perc // 909 rim // syncopated
  //   synth // rave stab // syncopated stabs // F
  //   lead // supersaw // hook // F
  //   bass // hardstyle // offbeat // F
  lanes.forEach((l, li) => {
    const y = Y0 + LH + 4 + li * LH;
    // Mute / solo toggles in front of each lane, like a mixer strip: inverted = on.
    // Each toggle owns a 3-character-wide, full-row-high target, so it's easy to hit with
    // the trackpad; a thin box outlines the one under the cursor.
    [["m", "mute"], ["s", "solo"]].forEach(([t, key], i) => {
      const bx = X0 + 1 * CHAR_W + i * 3 * CHAR_W;
      const r = { x: bx - CHAR_W, y: y - 2, w: 3 * CHAR_W, h: LH };
      if (l[key]) invert(ink, box, write, t, bx, y);
      else text(write, t, bx, y);

      r.action = () => {
        l[key] = !l[key];
        saveState();
      };
      regions.push(r);
    });

    const segs = [{ t: l.name, id: "name" }];
    for (const f of l.fields) segs.push({ t: SEP }, { t: l[f], id: f });
    let x = X0 + LANE_INDENT * CHAR_W;
    const silent = !audible(l);
    for (const s of segs) {
      const w = s.t.length * CHAR_W;
      const lit = s.id === "name" && playing && Date.now() - l.lastFiredAt < FLASH_MS;
      const open = menu && menu.lane === l && menu.field === s.id;
      if (lit || open) invert(ink, box, write, s.t, x, y);
      else text(write, s.t, x, y);
      if (s.id === "name" && silent) box(x - 1, y + 4, w + 2, 1); // struck through = not playing

      if (s.id && s.id !== "name") {
        if (!open) box(x, y + CHAR_H, w, 1); // underline = clickable
        const field = s.id, wx = x; // copy x: the loop keeps moving it
        const r = { x: x - 3, y: y - 2, w: w + 6, h: LH }; // exactly one row: rows never overlap
        r.action = () => openMenu(l, field, wx, y + LH);
        regions.push(r);
      }
      x += w;
    }
  });

  if (menu) drawMenu(ink, box, write);

  // Hover outline on whatever a click would hit right now (the same topmost-region search as
  // click()), so you can see exactly what you're about to change before you press.
  if (cursor && !drag) {
    const hit = regionAt(cursor.x, cursor.y);
    if (hit && hit.outline !== false) outline(ink, box, hit);
    else if (!hit && bpmRegion && inside(bpmRegion, cursor.x, cursor.y)) outline(ink, box, bpmRegion);
  }
  if (cursor) drawCursor(ink, box, cursor.x, cursor.y);
}

function openMenu(lane, field, x, y) {
  if (menu && menu.lane === lane && menu.field === field) { menu = null; return; } // clicking the same word again closes it
  menu = { lane, field, x, y, items: choices(lane, field) };
}

function drawMenu(ink, box, write) {
  const { lane, field, x, y, items } = menu;
  const w = Math.max(...items.map((t) => t.length)) * CHAR_W + 12;
  const h = items.length * LH + 6;
  // A black panel with a green outline, so the list sits cleanly over anything behind it.
  ink(...BLACK);
  box(x - 4, y - 4, w, h);
  ink(...GREEN);
  box(x - 4, y - 4, w, 1);
  box(x - 4, y - 4 + h - 1, w, 1);
  box(x - 4, y - 4, 1, h);
  box(x - 4 + w - 1, y - 4, 1, h);

  items.forEach((t, i) => {
    const iy = y + i * LH;
    const r = { x: x - 4, y: iy - 3, w, h: LH, outline: false }; // menu items invert on hover instead
    r.action = () => {
      lane[field] = t;
      menu = null;
      saveState();
    };
    const current = lane[field] === t;
    if (over(r)) invert(ink, box, write, t, x + 2, iy); // hover
    else text(write, t, x + 2, iy);
    if (current) box(x - 1, iy + 1, 2, CHAR_H - 3); // a small bar marks the one playing now
    regions.push(r);
  });
}

// Same crosshair shape as the system cursor: four arms with a gap, plus a centre dot.
function drawCursor(ink, box, x, y) {
  ink(...GREEN);
  box(x, y - 10, 1, 6); // up
  box(x, y + 5, 1, 6); // down
  box(x - 10, y, 6, 1); // left
  box(x + 5, y, 6, 1); // right
  box(x, y, 1, 1); // centre
}

// ── Input ──────────────────────────────────────────────────────────────────
const inside = (r, x, y) => x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h;

function regionAt(x, y) {
  // Menu items were added last, so search from the end: the open list wins over the words under it.
  for (let i = regions.length - 1; i >= 0; i--) if (inside(regions[i], x, y)) return regions[i];
  return null;
}

function outline(ink, box, r) {
  ink(...GREEN);
  box(r.x, r.y, r.w, 1);
  box(r.x, r.y + r.h - 1, r.w, 1);
  box(r.x, r.y, 1, r.h);
  box(r.x + r.w - 1, r.y, 1, r.h);
}

function click(x, y) {
  const r = regionAt(x, y); // the same search the hover outline uses, so what's outlined is what's clicked
  if (r) r.action();
  else menu = null; // clicked empty space: close any open list
}

// What counts as a click. The engine sends the same "touch" event for two things:
// a finger landing on the trackpad (BTN_TOUCH, which happens every time you start
// moving the cursor) and a real button press (BTN_LEFT). Treating every touch as a
// click closed the menu whenever you moved toward it. So a click is either:
//  - a button press while a finger is already on the pad (a second touch in one gesture), or
//  - a short touch that doesn't move: a tap, or the TrackPoint's own buttons.
// Only one click per gesture.
const TAP_MS = 350, TAP_MOVE = 6; // a tap: shorter than this, moving no more than this many px
const DEBOUNCE_MS = 120; // ignore a second click this soon after the last one
const press = { down: 0, at: 0, x: 0, y: 0, pressed: false };
let lastClickAt = 0;

function fireClick() {
  if (!cursor) return;
  const now = Date.now();
  if (now - lastClickAt < DEBOUNCE_MS) return;
  lastClickAt = now;
  click(cursor.x, cursor.y);
}

function pressDown() {
  if (press.down === 0) {
    // A new gesture: the first finger (or a TrackPoint button) went down.
    press.at = Date.now();
    press.x = cursor ? cursor.x : 0;
    press.y = cursor ? cursor.y : 0;
    press.pressed = false;
  }
  press.down = Math.min(press.down + 1, 2);
  if (press.down !== 2) return;

  // The pad was pressed down while a finger rests on it: that's a click, every time.
  // (Before, only the first press per finger-landing counted, so clicking m/s again
  // without lifting the finger did nothing.)
  press.pressed = true;
  if (cursor && bpmRegion && inside(bpmRegion, cursor.x, cursor.y)) {
    // Pressing on the bpm starts a tempo drag instead of a click.
    menu = null;
    drag = { y0: cursor.y, bpm0: bpm, travel: 0 };
    return;
  }
  fireClick();
}

function pressUp() {
  press.down = Math.max(press.down - 1, 0);
  if (press.down < 2 && drag) { drag = null; saveState(); } // letting go of the pad ends a tempo drag
  if (press.down > 0 || press.pressed || !cursor) return;
  // Gesture over with no pad press: count it as a tap if it was short and didn't move.
  const still = Math.abs(cursor.x - press.x) <= TAP_MOVE && Math.abs(cursor.y - press.y) <= TAP_MOVE;
  if (still && Date.now() - press.at < TAP_MS) fireClick();
}

function act({ event: e, system, pen }) {
  // touch/draw/lift carry the position already in screen pixels.
  if ((e.is("touch") || e.is("draw") || e.is("lift")) && e.pointer) {
    cursor = { x: e.pointer.x, y: e.pointer.y };
    if (pen && e.pointer.x > 20) scale = Math.max(1, Math.round(pen.x / e.pointer.x));
  }
  if (e.is("touch")) pressDown();
  if (e.is("lift")) pressUp();

  // Tab: hold for a fill; a quick tap fills to the end of the bar.
  // Shift: hold for the filter; letting go opens it again.
  const keyName = String(e.key || "").toLowerCase();
  if (keyName === "shift" || keyName === "shiftleft" || keyName === "shiftright") {
    if (RISER_ENABLED) {
      // (Saved for later: the riser + drop gesture. Off for now; see RISER_ENABLED.)
      if (e.is("keyboard:down") && !e.repeat) {
        if (riser.dropAt) riser.dropAt = 0;
        else if (!filt.on) { filt.on = true; filt.at = Date.now(); riser.at = Date.now(); }
      }
      if (e.is("keyboard:up") && filt.on && !riser.dropAt) {
        if (playing && riser.amount > 0.05) {
          const bar = Math.floor((Date.now() - startAt) / (stepMs() * 16)) + 1;
          riser.dropAt = startAt + bar * 16 * stepMs();
        } else filt.on = false;
      }
    } else {
      // Auto sweep: hold to sweep, let go to glide back open (starting from wherever it got to).
      const now = Date.now();
      if (e.is("keyboard:down") && !e.repeat && !filt.on) {
        const cur = sweepLevel(now); // pressing again mid-return carries on from there
        filt.on = true;
        filt.at = now - cur * SWEEP_BARS * 16 * stepMs();
      }
      if (e.is("keyboard:up") && filt.on) {
        filt.relLevel = sweepLevel(now);
        filt.on = false;
        filt.releaseAt = now;
      }
    }
    if (e.is("keyboard:down") || e.is("keyboard:up")) return;
  }
  if (e.is("keyboard:up") && String(e.key || "").toLowerCase() === "tab") {
    const now = Date.now();
    if (now - fill.downAt < FILL_TAP_MS && playing) {
      const bar = Math.floor((now - startAt) / (stepMs() * 16)) + 1; // the next bar line
      fill.until = startAt + bar * 16 * stepMs();
    }
    fill.held = false;
    return;
  }

  if (!e.is("keyboard:down") || e.repeat) return;
  const k = String(e.key || "").toLowerCase();
  if (k.length === 1 && k >= "1" && k <= "9") {
    fillSet = Number(k) - 1; // 1-9 pick the fill, with or without Tab held
    saveState();
    return;
  }
  if (k === "tab") {
    fill.held = true;
    fill.downAt = Date.now();
    escs = 0;
    return;
  }
  if (k === "space" || k === "spacebar" || k === " ") {
    playing = !playing;
    if (playing) restartClock();
    escs = 0;
  } else if (k === "escape") {
    if (menu) { menu = null; escs = 0; return; } // Esc closes an open list first
    if (++escs >= 3) system?.jump?.("prompt");
  } else {
    escs = 0;
  }
}

export { boot, paint, act, sim };
