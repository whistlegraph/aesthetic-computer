// netmelody.mjs — a melody pushed over the LAN and played on the device.
// Landed here by `PUT /pieces/netmelody.mjs?jump=1` from another machine on
// the network. Notes fire off wall-clock deltas, not frame counts, so the
// tune holds its tempo whatever the device's frame rate is doing.

const BPM = 132;
const BEAT = 60000 / BPM;

// C major, two octaves.
const N = {
  c: 261.63, d: 293.66, e: 329.63, f: 349.23,
  g: 392.0,  a: 440.0,  b: 493.88,
  C: 523.25, D: 587.33, E: 659.25, G: 783.99,
};

// [note or null for a rest, length in beats]
const TUNE = [
  ["c", 0.5], ["e", 0.5], ["g", 0.5], ["C", 0.5],
  ["b", 0.5], ["g", 0.5], ["e", 0.5], ["g", 0.5],
  ["f", 0.5], ["a", 0.5], ["C", 0.5], ["D", 0.5],
  ["C", 1.0], ["g", 1.0],
  ["e", 0.5], ["f", 0.5], ["g", 0.5], ["a", 0.5],
  ["b", 0.5], ["C", 0.5], ["D", 0.5], ["E", 0.5],
  ["G", 1.5], [null, 0.5],
  ["E", 0.5], ["C", 0.5], ["g", 0.5], ["e", 0.5],
  ["c", 2.0], [null, 1.0],
];

// Absolute start time of each note, plus where the phrase wraps.
const SCHEDULE = [];
let LENGTH = 0;
for (const [note, beats] of TUNE) {
  SCHEDULE.push({ note, at: LENGTH, ms: beats * BEAT });
  LENGTH += beats * BEAT;
}

let started = 0;   // wall-clock ms when the phrase last began
let next = 0;      // index of the note we are waiting on
let loops = 0;
let nowPlaying = "";
let flash = 0;     // counts down after each note, drives the pulse

function restart() {
  started = Date.now();
  next = 0;
}

function boot() {
  restart();
}

function sim({ sound }) {
  const t = Date.now() - started;

  while (next < SCHEDULE.length && t >= SCHEDULE[next].at) {
    const step = SCHEDULE[next];
    if (step.note) {
      const tone = N[step.note];
      // Voice: soft triangle lead with a square shadow an octave down.
      sound?.synth?.({
        type: "triangle", tone,
        duration: Math.min(step.ms / 1000, 0.5),
        volume: 0.14, attack: 0.004, decay: step.ms / 1000,
      });
      sound?.synth?.({
        type: "square", tone: tone / 2,
        duration: Math.min(step.ms / 1000, 0.3),
        volume: 0.04, attack: 0.006, decay: step.ms / 2000,
      });
      nowPlaying = step.note;
      flash = 12;
    } else {
      nowPlaying = "";
    }
    next++;
  }

  if (t >= LENGTH) {
    loops++;
    restart();
  }
  if (flash > 0) flash--;
}

function act({ event: e, system }) {
  if (!e.is("keyboard:down")) return;
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
    return;
  }
  if (e.is("keyboard:down:space")) restart();
}

function paint({ wipe, ink, box, write, screen }) {
  const w = screen.width, h = screen.height;
  const font = "font_1";
  const glow = flash * 8;

  wipe(8 + glow / 3, 6, 18 + glow / 2);

  // Playhead across the phrase.
  const progress = Math.min(1, (Date.now() - started) / LENGTH);
  ink(60, 40, 110);
  box(0, h - 6, w, 4);
  ink(200 - glow, 120 + glow, 255);
  box(0, h - 6, Math.round(w * progress), 4);

  // Each scheduled note as a tick on the timeline.
  for (let i = 0; i < SCHEDULE.length; i++) {
    if (!SCHEDULE[i].note) continue;
    const x = Math.round((SCHEDULE[i].at / LENGTH) * (w - 2));
    ink(i < next ? 160 : 70, i < next ? 220 : 60, 255);
    box(x, h - 12, 2, 4);
  }

  ink(230, 230, 255);
  write("netmelody", { x: 10, y: 10, size: 2, font: "matrix" });

  ink(150, 140, 190);
  write("pushed over the LAN", { x: 10, y: 36, size: 1, font });
  write(`${BPM} bpm · loop ${loops}`, { x: 10, y: 48, size: 1, font });

  // The note, centered and large.
  if (nowPlaying) {
    ink(255, 200 + Math.min(55, glow), 120 + glow);
    write(nowPlaying.toUpperCase(), {
      x: Math.round(w / 2) - 12,
      y: Math.round(h / 2) - 16,
      size: 4,
      font: "matrix",
    });
  }

  ink(110, 100, 140);
  write("space: restart   esc: prompt", { x: 10, y: h - 26, size: 1, font });
}

export { boot, paint, act, sim };
