// npscore — performs an .npscore (melody voices + light cues) on bare
// metal: notes through the GM synth lane, lights out the DMX lane, and
// the backdrop mirrors the room. Format doc lives in
// tools/mbscore-to-npscore.mjs, which translates Menu Band .mbscore files.
//
// Score selection: `npscore:name` colon param, else /pieces/npscore-current.txt
// (a pointer file — lanserv PUT can't pass colon params through /jump).
// Scores land beside pieces as /pieces/<name>.npscore via lanserv PUT.

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

// notepat pitch-class palette (sharps black) — the player's own fallback
// when a score ships without a lights track.
const PC_RGB = [
  [255, 50, 50], [0, 0, 0], [255, 160, 0], [0, 0, 0], [255, 230, 0],
  [50, 200, 50], [0, 0, 0], [50, 120, 255], [0, 0, 0], [130, 50, 200],
  [0, 0, 0], [180, 80, 255],
];

// House fixture map — Chauvet Wedge Tri, 3ch at d.490 (see notepat.mjs).
const dmxSlots = new Array(492).fill(0);
const dmxMap = (r, g, b) => {
  dmxSlots[489] = r; dmxSlots[490] = g; dmxSlots[491] = b;
  return dmxSlots;
};

let score = null;
let scoreName = "";
let err = null;
let events = []; // [{ at, dur, hz, gm, vol }] sorted by at
let cursor = 0;
let t0 = 0; // Date.now() when playback began
let total = 0;
let state = "load"; // load | play | done
let dmxLast = [-1, -1, -1];
let dmxStamp = 0;

const hz = (m) => 440 * Math.pow(2, (m - 69) / 12);

function loadScore(system) {
  const raw = system?.readFile?.("/pieces/" + scoreName + ".npscore");
  if (!raw) { err = "no /pieces/" + scoreName + ".npscore"; return; }
  try { score = JSON.parse(raw); } catch (e) { err = "bad json: " + e.message; return; }
  const lead = score.leadSeconds || 0;
  events = [];
  for (const v of score.voices || []) {
    const fam = FAMILY[Math.floor((v.program || 0) / 8)] || FAMILY[0];
    for (const n of v.notes || []) {
      events.push({
        at: lead + n.start, dur: n.dur, midi: n.midi, hz: hz(n.midi),
        gm: v.program || 0, fam,
        vol: ((n.velocity ?? v.velocity ?? 100) / 127) * fam.volume,
      });
    }
  }
  events.sort((a, b) => a.at - b.at);
  if (!Array.isArray(score.lights) || !score.lights.length) {
    score.lights = events.map(e => ({
      start: e.at, dur: e.dur, rgb: PC_RGB[((e.midi % 12) + 12) % 12],
    }));
  } else if (lead) {
    score.lights = score.lights.map(l => ({ ...l, start: l.start + lead }));
  }
  total = events.reduce((m, e) => Math.max(m, e.at + e.dur), 0) + (score.tailSeconds ?? 0.5);
  cursor = 0;
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
    if (e.at + e.dur > t) { // never fire a note the clock already passed
      sound?.synth?.({
        type: e.fam.wave, tone: e.hz, duration: e.dur, volume: e.vol,
        attack: e.fam.attack, decay: e.fam.decay, gmProgram: e.gm,
      });
    }
  }
  if (t > total) state = "done";
}

function paint({ wipe, ink, write, screen, system }) {
  if (err) {
    wipe(40, 10, 10);
    ink(255, 200, 200);
    write("npscore: " + err, { x: 8, y: 8, size: 1 });
    return;
  }
  const t = (Date.now() - t0) / 1000;

  // Average every active light cue — notepat's held-tone blend, roomward.
  let r = 0, g = 0, b = 0, w = 0;
  for (const l of score.lights) {
    if (l.start <= t && t < l.start + l.dur) { r += l.rgb[0]; g += l.rgb[1]; b += l.rgb[2]; w++; }
  }
  if (w) { r = Math.round(r / w); g = Math.round(g / w); b = Math.round(b / w); }

  const changed = r !== dmxLast[0] || g !== dmxLast[1] || b !== dmxLast[2];
  const now = Date.now();
  if (system?.dmxSend && ((changed && now - dmxStamp > 33) || now - dmxStamp > 1000)) {
    if (system.dmxSend(dmxMap(r, g, b))) dmxLast = [r, g, b];
    dmxStamp = now;
  }

  wipe(r, g, b);
  const bright = r + g + b > 380;
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

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down:escape")) system?.jump?.("prompt");
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    if (state === "done") loadScore(system);
  }
}

export { boot, paint, act, sim };
