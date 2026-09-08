// npscore — performs an .npscore (melody + percussion + light cues) on
// bare metal: notes down the GM synth lane, drums through the shared
// percussion kit, lights out the DMX lane. Format doc lives in
// tools/mbscore-to-npscore.mjs; tools/npscore-gen.mjs composes long ones.
//
// Light model (notepat's, roomward): sustained cues average, weighted by
// their velocity gain; flash cues (decay, no dur) ADD on top and fade —
// a kick punches white over the held chord color. DMX sends cap at 40Hz,
// the honest wire ceiling (a 492-slot frame is ~22ms of 250kbaud).
//
// Score selection: `npscore:name` colon param, else /pieces/npscore-current.txt
// (pointer file — lanserv /jump can't carry colon params).

import { playPercussion } from "/lib/percussion.mjs";

// Same 16 GM family fallbacks as notepat — the C GM voice takes over where
// a program is implemented; these shape the `type` path for the rest.
const FAMILY = [
  { wave: "piano", attack: 0.002, decay: 0.9, volume: 0.55 },
  { wave: "triangle", attack: 0.001, decay: 0.35, volume: 0.5 },
  { wave: "square", attack: 0.004, decay: 0.2, volume: 0.4 },
  { wave: "harp", attack: 0.001, decay: 0.6, volume: 0.5 },
  { wave: "triangle", attack: 0.002, decay: 0.5, volume: 0.6 },
  { wave: "sawtooth", attack: 0.08, decay: 0.4, volume: 0.4 },
  { wave: "sawtooth", attack: 0.06, decay: 0.4, volume: 0.38 },
  { wave: "sawtooth", attack: 0.02, decay: 0.3, volume: 0.45 },
  { wave: "square", attack: 0.01, decay: 0.3, volume: 0.4 },
  { wave: "whistle", attack: 0.03, decay: 0.3, volume: 0.42 },
  { wave: "square", attack: 0.004, decay: 0.3, volume: 0.42 },
  { wave: "sine", attack: 0.12, decay: 0.5, volume: 0.4 },
  { wave: "sine", attack: 0.06, decay: 0.4, volume: 0.38 },
  { wave: "harp", attack: 0.001, decay: 0.6, volume: 0.5 },
  { wave: "triangle", attack: 0.001, decay: 0.18, volume: 0.5 },
  { wave: "noise", attack: 0.001, decay: 0.3, volume: 0.45 },
];

// notepat pitch-class palette (sharps black).
const PC_RGB = [
  [255, 50, 50], [0, 0, 0], [255, 160, 0], [0, 0, 0], [255, 230, 0],
  [50, 200, 50], [0, 0, 0], [50, 120, 255], [0, 0, 0], [130, 50, 200],
  [0, 0, 0], [180, 80, 255],
];

// Drum name → percussion-kit letter (lib/percussion.mjs speaks letters).
const DRUM_LETTER = {
  kick: "c", snare: "d", clap: "e", snap: "f", "hat-c": "g", "hat-o": "a",
  ride: "b", crash: "c#", splash: "d#", cowbell: "f#", block: "g#", tambo: "a#",
};

// House fixture map — Chauvet Wedge Tri, 3ch at d.490 (see notepat.mjs).
const dmxSlots = new Array(492).fill(0);
const dmxMap = (r, g, b) => {
  dmxSlots[489] = r; dmxSlots[490] = g; dmxSlots[491] = b;
  return dmxSlots;
};

let score = null;
let scoreName = "";
let err = null;
let events = []; // [{ at, kind, … }] sorted by at
let lights = []; // [{ start, dur?, decay?, rgb, gain }] sorted by start
let cursor = 0;
let liteLo = 0;
let t0 = 0;
let total = 0;
let state = "load"; // load | play | done
let dmxLast = [-1, -1, -1];
let dmxStamp = 0;

const hz = (m) => 440 * Math.pow(2, (m - 69) / 12);

function loadScore(system) {
  // readFile tails at 64KB (it's a log reader) — scores go through the
  // uncapped byte lane and decode here; score JSON is ASCII by design.
  let raw = null;
  const buf = system?.readFileBytes?.("/pieces/" + scoreName + ".npscore");
  if (buf) {
    const bytes = new Uint8Array(buf);
    const parts = [];
    for (let i = 0; i < bytes.length; i += 4096)
      parts.push(String.fromCharCode.apply(null, bytes.subarray(i, i + 4096)));
    raw = parts.join("");
  } else {
    raw = system?.readFile?.("/pieces/" + scoreName + ".npscore");
  }
  if (!raw) { err = "no /pieces/" + scoreName + ".npscore"; return; }
  try { score = JSON.parse(raw); } catch (e) { err = "bad json: " + e.message; return; }
  const lead = score.leadSeconds || 0;
  events = [];
  for (const v of score.voices || []) {
    if (v.kind === "percussion") {
      for (const n of v.notes || []) {
        const letter = DRUM_LETTER[n.drum] || n.drum;
        events.push({
          at: lead + n.start, kind: "drum", letter,
          vol: (n.velocity ?? v.velocity ?? 100) / 127, dur: 0.05,
        });
      }
      continue;
    }
    const fam = FAMILY[Math.floor((v.program || 0) / 8)] || FAMILY[0];
    for (const n of v.notes || []) {
      const vel = (n.velocity ?? v.velocity ?? 100) / 127;
      events.push({
        at: lead + n.start, kind: "tone", dur: n.dur, midi: n.midi,
        hz: hz(n.midi), gm: v.program || 0, fam, vel, vol: vel * fam.volume,
      });
    }
  }
  events.sort((a, b) => a.at - b.at);

  if (Array.isArray(score.lights) && score.lights.length) {
    lights = score.lights.map(l => ({
      ...l, start: l.start + lead, gain: l.gain ?? 1,
    }));
  } else {
    // Derive: tones hold their pitch color at their velocity; drums flash
    // white and fade — kicks a beat longer than hats.
    lights = events.map(e => e.kind === "tone"
      ? { start: e.at, dur: e.dur, rgb: PC_RGB[((e.midi % 12) + 12) % 12], gain: e.vel }
      : { start: e.at, decay: e.letter === "c" ? 0.18 : 0.1, rgb: [255, 255, 255], gain: e.vol });
  }
  lights.sort((a, b) => a.start - b.start);

  total = events.reduce((m, e) => Math.max(m, e.at + e.dur), 0) + (score.tailSeconds ?? 0.5);
  cursor = 0;
  liteLo = 0;
  t0 = Date.now() + 400; // breath so note zero isn't late
  state = "play";
}

function boot({ system, colon, params }) {
  scoreName = colon?.[0] || params?.[0] || "";
  if (!scoreName) {
    scoreName = (system?.readFile?.("/pieces/npscore-current.txt") || "").trim();
  }
  if (!scoreName) { err = "no score named (npscore:name or npscore-current.txt)"; return; }
  loadScore(system);
}

function sim({ sound }) {
  if (state !== "play") return;
  const t = (Date.now() - t0) / 1000;
  while (cursor < events.length && events[cursor].at <= t) {
    const e = events[cursor++];
    if (e.at + Math.max(e.dur, 0.1) <= t) continue; // clock already passed it
    if (e.kind === "drum") {
      playPercussion(sound, e.letter, { volume: e.vol });
    } else {
      sound?.synth?.({
        type: e.fam.wave, tone: e.hz, duration: e.dur, volume: e.vol,
        attack: e.fam.attack, decay: e.fam.decay, gmProgram: e.gm,
      });
    }
  }
  if (t > total) state = "done";
}

function paint({ wipe, ink, write, system }) {
  if (err) {
    wipe(40, 10, 10);
    ink(255, 200, 200);
    write("npscore: " + err, { x: 8, y: 8, size: 1 });
    return;
  }
  const t = (Date.now() - t0) / 1000;

  // Base layer: velocity-weighted average of active sustained cues.
  // Flash layer: decaying additive bursts. Head pointer skips spent cues.
  while (liteLo < lights.length &&
         lights[liteLo].start + (lights[liteLo].dur ?? lights[liteLo].decay ?? 0.2) < t) liteLo++;
  let r = 0, g = 0, b = 0, w = 0, fr = 0, fg = 0, fb = 0;
  for (let i = liteLo; i < lights.length; i++) {
    const l = lights[i];
    if (l.start > t) break;
    if (l.dur != null) {
      if (t < l.start + l.dur) {
        r += l.rgb[0] * l.gain; g += l.rgb[1] * l.gain; b += l.rgb[2] * l.gain;
        w += l.gain;
      }
    } else {
      const k = Math.max(0, 1 - (t - l.start) / (l.decay ?? 0.12)) * l.gain;
      fr += l.rgb[0] * k; fg += l.rgb[1] * k; fb += l.rgb[2] * k;
    }
  }
  if (w) { r /= w; g /= w; b /= w; }
  const R = Math.min(255, Math.round(r + fr));
  const G = Math.min(255, Math.round(g + fg));
  const B = Math.min(255, Math.round(b + fb));

  const changed = R !== dmxLast[0] || G !== dmxLast[1] || B !== dmxLast[2];
  const now = Date.now();
  if (system?.dmxSend && ((changed && now - dmxStamp >= 25) || now - dmxStamp > 1000)) {
    if (system.dmxSend(dmxMap(R, G, B))) dmxLast = [R, G, B];
    dmxStamp = now;
  }

  wipe(R, G, B);
  const bright = R + G + B > 380;
  ink(bright ? 0 : 255, bright ? 0 : 255, bright ? 0 : 255);
  write(score?.name || scoreName, { x: 8, y: 8, size: 1 });
  if (state === "done") {
    write("done — enter replays, esc leaves", { x: 8, y: 24, size: 1 });
  } else {
    const pct = Math.max(0, Math.min(1, t / total));
    write(Math.round(pct * 100) + "%", { x: 8, y: 24, size: 1 });
    ink(bright ? 40 : 220, bright ? 40 : 220, bright ? 40 : 220);
    write("#".repeat(Math.round(pct * 24)) || "-", { x: 8, y: 40, size: 1 });
  }
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape")) system?.jump?.("prompt");
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    if (state === "done") loadScore(system);
  }
}

export { boot, paint, act, sim };
