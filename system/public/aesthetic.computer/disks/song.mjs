// Song, 2023.7.02.09.27.28
// Notate both a melody and lyrics to sing along to.

/* #region 📚 README
  Tap or hold the big button (or space) to sing the next syllable; each one
  also draws a stroke of the song's picture. Drag while holding to bend pitch.
  - `song row`, `song weasel`, `song kid`... or tap the song name for the list.
  - `play` (or enter) runs the song in time as a guide.
  - `draw` records your own picture, one stroke per syllable. It saves on this
    device and copies the strokes as JSON, to paste in as a song's `drawing`.
  - `song:sine row` picks the wave.
#endregion */

/* #region 🏁 TODO
  - [] Score tapped timing against the song's beats.
  - [] Add some kind of violin mode.
  - [] How to vary volume during playback?
  - [] Dragging left and right adjusts the pan.
#endregion */

let btn, pickBtn, playBtn, drawBtn; // The big note button and the bottom row.
let rows = []; // One button per song, shown while the list is open.
let voice,
  tone = 800;

// ✏️ Drawings: one stroke per syllable, how-to-draw style. A stroke is a list
// of polylines in a unit square, so a single syllable can lift the pen.
const rad = Math.PI / 180;
function arc(cx, cy, r, from, to, steps = 14) {
  const points = [];
  for (let i = 0; i <= steps; i += 1) {
    const a = (from + ((to - from) * i) / steps) * rad;
    points.push([cx + r * Math.cos(a), cy + r * Math.sin(a)]);
  }
  return points;
}
const ring = (cx, cy, r) => [arc(cx, cy, r, 0, 360, 20)];
const sparkle = (cx, cy, r) => [
  [[cx - r, cy], [cx + r, cy]],
  [[cx, cy - r], [cx, cy + r]],
];
const cloud = (cx, cy, s = 1) => [
  [
    ...arc(cx - 0.05 * s, cy, 0.03 * s, 90, 270),
    ...arc(cx, cy - 0.02 * s, 0.045 * s, 200, 340),
    ...arc(cx + 0.05 * s, cy, 0.03 * s, 270, 450),
    [cx - 0.05 * s, cy + 0.03 * s],
  ],
];
const ripple = (x, y, w) => [
  [...arc(x + w / 4, y, w / 4, 180, 360, 8), ...arc(x + (w * 3) / 4, y, w / 4, 180, 0, 8)].map(
    ([px, py]) => [px, y + (py - y) * 0.4], // Flatten the half circles into swells.
  ),
];
const bird = (x, y, s = 0.04) => [
  [...arc(x - s / 2, y, s / 2, 200, 350, 6), ...arc(x + s / 2, y, s / 2, 190, 340, 6)],
];

const tip = (k) => arc(0.5, 0.3, 0.16, -90 + 72 * k, -90 + 72 * k, 1)[0];
const ray = (k) => [[
  arc(0.5, 0.3, 0.095, -54 + 72 * k, 0, 1)[0],
  arc(0.5, 0.3, 0.135, -54 + 72 * k, 0, 1)[0],
]];

const twinkleDrawing = [
  // twin- -kle twin- -kle lit- -tle star
  [[tip(0), tip(2)]], [[tip(2), tip(4)]], [[tip(4), tip(1)]],
  [[tip(1), tip(3)]], [[tip(3), tip(0)]],
  [[[0.47, 0.27], [0.47, 0.295]], [[0.53, 0.27], [0.53, 0.295]]], // Eyes
  [arc(0.5, 0.305, 0.03, 20, 160)], // Smile
  // how i won- -der what you are
  ring(0.2, 0.7, 0.04),
  [[[0.2, 0.74], [0.2, 0.86]]],
  [[[0.12, 0.73], [0.2, 0.79], [0.28, 0.73]]],
  [[[0.14, 0.96], [0.2, 0.86], [0.26, 0.96]]],
  [[...arc(0.08, 0.58, 0.03, 180, 450), [0.08, 0.64]]], // ?
  [[[0.08, 0.67], [0.08, 0.68]]],
  [[[0.25, 0.66], [0.27, 0.632]], [[0.29, 0.604], [0.31, 0.576]], [[0.33, 0.548], [0.35, 0.52]]], // Gaze
  // up a- -bove the world so high
  [[[0.335, 0.5], [0.365, 0.495], [0.37, 0.53]]],
  cloud(0.13, 0.33),
  cloud(0.82, 0.52),
  [arc(0.5, 2.94, 2, 256, 270)],
  [arc(0.5, 2.94, 2, 270, 284)],
  [[[0.76, 0.955], [0.76, 0.89], [0.86, 0.89], [0.86, 0.955]], [[0.74, 0.89], [0.81, 0.84], [0.88, 0.89]]],
  [[...arc(0.86, 0.14, 0.06, 60, 300), ...arc(0.905, 0.14, 0.054, 254, 106)]], // Moon
  // like a dia- -mond in the sky
  [[[0.5, 0.06], [0.74, 0.3]]], [[[0.74, 0.3], [0.5, 0.56]]],
  [[[0.5, 0.56], [0.26, 0.3]]], [[[0.26, 0.3], [0.5, 0.06]]],
  sparkle(0.12, 0.12, 0.035), sparkle(0.68, 0.07, 0.025), sparkle(0.93, 0.36, 0.03),
  // twin- -kle twin- -kle lit- -tle star
  ray(0), ray(1), ray(2), ray(3), ray(4),
  sparkle(0.6, 0.66, 0.03), sparkle(0.5, 0.76, 0.04),
  // how i won- -der what you are
  sparkle(0.32, 0.12, 0.02), sparkle(0.05, 0.2, 0.02), sparkle(0.93, 0.66, 0.02),
  sparkle(0.68, 0.76, 0.02), sparkle(0.42, 0.86, 0.02), sparkle(0.58, 0.87, 0.02),
  sparkle(0.4, 0.68, 0.02),
];

const rowDrawing = [
  // row row row your boat
  [[[0.22, 0.62], [0.3, 0.74], [0.7, 0.74], [0.78, 0.62]]],
  [[[0.22, 0.62], [0.78, 0.62]]],
  [[[0.55, 0.62], [0.55, 0.28]]],
  [[[0.55, 0.31], [0.72, 0.56], [0.55, 0.56]]],
  [[[0.55, 0.28], [0.62, 0.305], [0.55, 0.33]]],
  // gent- -ly down the stream
  ripple(0.04, 0.82, 0.22), ripple(0.28, 0.82, 0.22),
  ripple(0.52, 0.82, 0.22), ripple(0.76, 0.82, 0.22),
  [[...ripple(0.05, 0.92, 0.3)[0], ...ripple(0.35, 0.92, 0.3)[0], ...ripple(0.65, 0.92, 0.3)[0]]],
  // mer- -ri- -ly: the rower
  ring(0.36, 0.5, 0.045),
  [[[0.36, 0.545], [0.36, 0.62]]],
  [[[0.36, 0.57], [0.45, 0.59]]],
  // mer- -ri- -ly: the oar
  [[[0.47, 0.57], [0.14, 0.75]]],
  [[[0.14, 0.75], [0.08, 0.75], [0.1, 0.8], [0.14, 0.75]]],
  [[[0.06, 0.72], [0.03, 0.69]], [[0.09, 0.71], [0.09, 0.67]], [[0.12, 0.72], [0.15, 0.69]]],
  // mer- -ri- -ly: the sun
  ring(0.84, 0.17, 0.065),
  [[[0.84, 0.085], [0.84, 0.05]], [[0.84, 0.255], [0.84, 0.29]], [[0.755, 0.17], [0.72, 0.17]], [[0.925, 0.17], [0.96, 0.17]]],
  [[[0.78, 0.11], [0.755, 0.085]], [[0.9, 0.11], [0.925, 0.085]], [[0.78, 0.23], [0.755, 0.255]], [[0.9, 0.23], [0.925, 0.255]]],
  // mer- -ri- -ly: three birds
  bird(0.4, 0.16), bird(0.5, 0.09), bird(0.62, 0.17),
  // life is but a dream
  ring(0.31, 0.42, 0.012),
  ring(0.27, 0.36, 0.02),
  cloud(0.18, 0.24, 1.7),
  [[[0.12, 0.215], [0.16, 0.215], [0.12, 0.255], [0.16, 0.255]]],
  [[[0.19, 0.19], [0.25, 0.19], [0.19, 0.26], [0.25, 0.26]]],
];

// Each song pairs a melody with its lyrics and drawing, one syllable per note.
// `beats` are note lengths for the guide (one beat each when left out).
// Pick one with a param: `song twinkle`, `song row`, `song weasel`, `song kid`.
const words = (text) => text.trim().split(/\s+/);

const songs = {
  twinkle: {
    title: "Twinkle Twinkle Little Star",
    beats: Array(6).fill([1, 1, 1, 1, 1, 1, 2]).flat(),
    notes: words(`C C G G A A G
                  F F E E D D C
                  G G F F E E D
                  G G F F E E D
                  C C G G A A G
                  F F E E D D C`),
    lyrics: words(`twin- -kle twin- -kle lit- -tle star
                   how i won- -der what you are
                   up a- -bove the world so high
                   like a dia- -mond in the sky
                   twin- -kle twin- -kle lit- -tle star
                   how i won- -der what you are`),
    drawing: twinkleDrawing,
  },
  row: {
    title: "Row Row Row Your Boat",
    beats: [
      1.5, 1.5, 1, 0.5, 1.5,
      1, 0.5, 1, 0.5, 3,
      ...Array(12).fill(0.5),
      1, 0.5, 1, 0.5, 3,
    ],
    notes: words(`C C C D E
                  E D E F G
                  C5 C5 C5 G G G E E E C C C
                  G F E D C`),
    lyrics: words(`row row row your boat
                   gent- -ly down the stream
                   mer- -ri- -ly mer- -ri- -ly mer- -ri- -ly mer- -ri- -ly
                   life is but a dream`),
    drawing: rowDrawing,
  },
  weasel: {
    title: "Pop Goes the Weasel",
    beats: [
      0.5, 1, 0.5, 1, 0.5, 0.5, 0.5, 0.5, 1,
      0.5, 1, 0.5, 1, 0.5, 1.5, 1.5,
      1, 0.5, 1, 0.5, 0.5, 0.5, 0.5, 1.5,
      1.5, 1, 0.5, 1.5, 1.5,
    ],
    notes: words(`G C5 C5 D5 D5 E5 G5 E5 C5
                  G C5 C5 D5 D5 E5 C5
                  C5 C5 D5 D5 E5 G5 E5 C5
                  A5 D5 F5 E5 C5`),
    lyrics: words(`oh up and down the ci- -i- -ty road
                   and in and out the ea- -gle
                   that's the way the mo- -o- -ney goes
                   pop! goes the wea- -sel!`),
  },
  // 🟡 Loose: 46 notes against 44 syllables, so the words drift by the end.
  kid: {
    title: "Starry Eyes",
    notes: words(`E D C E G A B A G F G E E D
                  B A G F G E G E G G B B B G
                  A F E D E D E D E D E G A F
                  G E D C`),
    lyrics: words(`did u ev- -er meet the kid with star- -ry eyes
                   CROSSED OUT EYES! al- -ways thought of life as just
                   one big BOX OF LIES! paths to hope- -ful pla- -a- -a- -a- -a- -a- -a- -ces. Bridge- -less like torn pa- -ges.`),
  },
  mmm: {
    title: "Mmm",
    notes: words(`B F# B F# B F# B D#5 F#5
                  E5 C#5 E5 C#5 E5 C#5 A# C#5 F#`),
    lyrics: [],
  },
};

const aliases = { star: "twinkle", boat: "row", pgtw: "weasel", pop: "weasel" };
const names = Object.keys(songs);
const BPM = 120; // Beats count from Unix epoch zero on the shared clock.

let name = "twinkle";
let song, lyrics, drawing, beats, starts; // The chosen song, laid out.
let index = 0;
let shown = 0, // How many strokes are finished.
  progress = 0; // How much of the current stroke is drawn, from 0 to 1.
let mode = "tap"; // "tap", "guide" (plays itself) or "draw" (records strokes).
let listOpen = false;
let guide; // { start, at }: the guide's first beat and the note it is on.
let recorded, previous; // Strokes being drawn, and the drawing they replace.
const m = 24;
let wave = "triangle";
let keyHeld = false;

// 🥾 Boot
function boot({ wipe, screen, ui, sound, colon, params, geo, store, clock, needsPaint }) {
  wipe(127);
  btn = new ui.Button(m, m, screen.width - m * 2, screen.height - m * 2);
  btn.multitouch = false;
  rows = names.map(() => new ui.Button(0, 0, 1, 1));
  pickBtn = new ui.TextButton(name);
  playBtn = new ui.TextButton("play");
  drawBtn = new ui.TextButton("draw");
  wave = colon[0] || wave;
  clock?.resync?.();
  select(params[0]?.toLowerCase(), { store, sound, needsPaint });
  layout(screen, geo);
}

// Load a song by name or alias, with any drawing saved on this device.
function select(pick, { store, sound, needsPaint }) {
  voice?.kill(0.05);
  pick = aliases[pick] || pick;
  name = songs[pick] ? pick : "twinkle";
  song = songs[name].notes;
  lyrics = songs[name].lyrics;
  drawing = songs[name].drawing;
  beats = songs[name].beats || song.map(() => 1);
  starts = [];
  let total = 0;
  for (const beat of beats) {
    starts.push(total);
    total += beat;
  }
  starts.push(total); // The last entry is the song's whole length.
  index = shown = progress = 0;
  mode = "tap";
  listOpen = false;
  playBtn.replaceLabel("play");
  drawBtn.replaceLabel("draw");
  tone = sound.freq(song[0]);
  const chosen = name;
  store?.retrieve?.(`song:${name}`, "local")?.then?.((saved) => {
    if (chosen !== name || mode === "draw") return;
    if (Array.isArray(saved) && saved.length) drawing = saved;
    needsPaint?.();
  });
}

// Place every button for the current screen.
function layout(screen, geo) {
  btn.box = new geo.Box(m, m, screen.width - m * 2, screen.height - m * 2);
  pickBtn.reposition({ left: 6, bottom: 3, screen }, name);
  playBtn.reposition({ right: 6, bottom: 3, screen });
  drawBtn.reposition({ right: 10 + playBtn.btn.box.w, bottom: 3, screen });
  const { x, y, w, h } = btn.box;
  const rowHeight = Math.min(24, (h - 8) / rows.length);
  rows.forEach((row, i) => {
    row.box = new geo.Box(x + 4, y + 4 + i * rowHeight, w - 8, rowHeight - 2);
  });
}

// The square the drawing sits in, between the two rows of text.
function frame() {
  const { x, y, w, h } = btn.box;
  const size = Math.max(8, Math.min(w, h - 36));
  return { left: x + (w - size) / 2, top: y + 14 + (h - 36 - size) / 2, size };
}

// 🎨 Paint
function paint($) {
  const { ink, line, screen, sound } = $;
  const note = sound.note(tone);
  const { x, y, w, h } = btn.box;
  const { left, top, size } = frame();
  const place = ([px, py]) => [left + px * size, top + py * size];

  $.wipe(127);
  ink(btn.down ? [245, 220, 50] : [230, 210, 100]).box(btn.box);

  if (drawing) {
    ink(60, 40, 0);
    for (let i = 0; i < shown; i += 1) trace(line, drawing[i], 1, place);
    if (btn.down || mode === "guide") {
      ink(255, 0, 0);
      trace(line, drawing[index], progress, place);
    }
  }

  const readout =
    mode === "draw" ? "one stroke per syllable" : `${note} - ${tone.toFixed(2)}`;
  ink(255).write(readout, { center: "x", y: y + 3 });
  ink("red").write(lyrics[index] || "", { center: "x", y: y + h - 14 });

  if (listOpen) {
    ink(0, 0, 0, 180).box(btn.box);
    const room = Math.floor((w - 16) / 6); // Characters that fit in a row.
    rows.forEach((row, i) => {
      const current = names[i] === name;
      ink(row.down ? [245, 220, 50] : current ? [230, 210, 100] : [90, 80, 40]).box(row.box);
      ink(row.down || current ? 0 : 255).write(songs[names[i]].title.slice(0, room), {
        x: row.box.x + 4,
        y: row.box.y + Math.floor((row.box.h - 10) / 2),
      });
    });
  }

  pickBtn.paint($);
  drawBtn.paint($);
  playBtn.paint($);

  ink(0).line(0, screen.height - 1, screen.width, screen.height - 1);
  ink(255).line(0, screen.height - 1, ((index + 1) / song.length) * screen.width, screen.height - 1);
  return false; // Repaint on request only.
}

// Draw the first `amount` (0 to 1) of a stroke's length.
function trace(line, stroke, amount, place) {
  if (!stroke) return;
  const length = ([ax, ay], [bx, by]) => Math.hypot(bx - ax, by - ay);
  let total = 0;
  for (const points of stroke) {
    for (let i = 1; i < points.length; i += 1) total += length(points[i - 1], points[i]);
  }
  let budget = total * amount;
  for (const points of stroke) {
    for (let i = 1; i < points.length; i += 1) {
      if (budget <= 0) return;
      const span = length(points[i - 1], points[i]);
      const part = span > 0 ? Math.min(1, budget / span) : 1;
      const [ax, ay] = place(points[i - 1]);
      const [bx, by] = place(points[i]);
      line(ax, ay, ax + (bx - ax) * part, ay + (by - ay) * part);
      budget -= span;
    }
  }
}

// 🧮 Sim
function sim({ needsPaint, clock, sound: { synth, freq } }) {
  if (mode === "guide") {
    const beat = (clock.time().getTime() * BPM) / 60000 - guide.start;
    if (beat < 0) return; // Waiting for the next beat to join on.
    if (beat >= starts[song.length]) {
      mode = "tap";
      playBtn.replaceLabel("play");
      shown = song.length; // Leave the finished picture up.
      index = 0;
      tone = freq(song[0]);
      needsPaint();
      return;
    }
    let at = guide.at < 0 ? 0 : guide.at;
    while (starts[at + 1] <= beat) at += 1; // A stall skips notes, never replays.
    if (at !== guide.at) {
      guide.at = index = shown = at;
      tone = freq(song[at]);
      voice?.kill(0.02);
      const duration = (beats[at] * 60) / BPM;
      voice = synth({ type: wave, tone, volume: 1, attack: 0.01, decay: 0.9, duration: duration * 0.95 });
    }
    progress = Math.min(1, (beat - starts[at]) / (beats[at] * 0.8));
    needsPaint();
    return;
  }
  if (mode === "draw" || !drawing || !btn?.down || progress >= 1) return;
  progress = Math.min(1, progress + 1 / 30); // A stroke takes a quarter second.
  needsPaint();
}

// 🎪 Act
function act({
  event: e,
  sound: { synth, freq },
  needsPaint,
  screen,
  num,
  geo,
  store,
  send,
  clock,
}) {
  if (e.is("reframed")) layout(screen, geo);

  // Start or stop the guide, joining on the next beat of the shared clock.
  function toggleGuide() {
    if (mode === "draw") return;
    voice?.kill(0.05);
    if (mode === "guide") {
      mode = "tap"; // Carry on by hand from wherever the guide got to.
    } else {
      mode = "guide";
      listOpen = false;
      clock.resync();
      guide = { start: Math.floor((clock.time().getTime() * BPM) / 60000) + 1, at: -1 };
      index = shown = progress = 0;
    }
    playBtn.replaceLabel(mode === "guide" ? "stop" : "play");
    needsPaint();
  }

  // Start recording strokes, or quit and put the old drawing back.
  function toggleDraw() {
    if (mode === "guide") return;
    voice?.kill(0.05);
    if (mode === "draw") {
      const key = `song:${name}`;
      if (recorded.length === 0) {
        // Quitting before any stroke forgets the saved drawing too.
        store[key] = null;
        store.persist(key, "local");
        drawing = songs[name].drawing;
      } else drawing = previous;
      mode = "tap";
      index = shown = 0;
    } else {
      previous = drawing;
      drawing = recorded = [];
      mode = "draw";
      listOpen = false;
      index = shown = 0;
      progress = 1;
      tone = freq(song[0]);
    }
    drawBtn.replaceLabel(mode === "draw" ? "quit" : "draw");
    needsPaint();
  }

  // Keep the finished recording: on this device, and on the clipboard as JSON.
  function keep() {
    const round = (n) => Math.round(n * 1000) / 1000;
    drawing = recorded.map((stroke) => stroke.map((points) => points.map((p) => p.map(round))));
    const key = `song:${name}`;
    store[key] = drawing;
    store.persist(key, "local");
    const json = JSON.stringify(drawing);
    send({ type: "copy", content: json });
    console.log(`✏️ ${name} drawing:`, json);
    mode = "tap";
    drawBtn.replaceLabel("draw");
  }

  const point = () => {
    const { left, top, size } = frame();
    return [(e.x - left) / size, (e.y - top) / size];
  };

  pickBtn.btn.act(e, () => {
    if (mode !== "tap") return;
    listOpen = !listOpen;
    needsPaint();
  });
  playBtn.btn.act(e, () => toggleGuide());
  drawBtn.btn.act(e, () => toggleDraw());
  if (e.is("keyboard:down:enter")) toggleGuide();

  if (listOpen) {
    rows.forEach((row, i) => {
      row.act(e, {
        down: () => needsPaint(),
        cancel: () => needsPaint(),
        push: () => {
          select(names[i], { store, sound: { freq }, needsPaint });
          pickBtn.reposition({ left: 6, bottom: 3, screen }, name);
          needsPaint();
        },
      });
    });
    return;
  }
  if (mode === "guide") return;

  if (btn.down && e.is("draw")) {
    if (mode === "draw") {
      // Add to the stroke, skipping points that barely moved.
      const points = recorded[index]?.[0];
      const next = point();
      const last = points?.[points.length - 1];
      if (last && Math.hypot(next[0] - last[0], next[1] - last[1]) > 0.008) points.push(next);
    } else {
      // Drag up and down to change pitch.
      tone = num.clamp(tone - e.delta.y, 100, 1200);
      voice?.update({ tone });
    }
    needsPaint();
  }

  function push() {
    voice?.kill();
    needsPaint();
    if (mode === "draw") {
      const points = recorded[index]?.[0];
      if (!points) return;
      if (points.length === 1) points.push([points[0][0] + 0.004, points[0][1]]); // A dot.
      if (index + 1 === song.length) keep();
    }
    shown = index + 1; // The held stroke is finished, however far it got.
    index = (index + 1) % song.length; // Cycle through notes.
    tone = freq(song[index]);
  }

  function down() {
    if (index === 0) shown = 0; // A new verse starts a clean page.
    if (mode === "draw") {
      if (index === 0) recorded.length = 0;
      recorded[index] = [[point()]];
    } else progress = 0;
    voice = synth({ type: wave, tone, volume: 1.0, duration: "🔁" });
    needsPaint();
  }

  function cancel() {
    voice?.kill(0.1);
    if (mode === "draw") recorded.length = index; // Drop the unfinished stroke.
    needsPaint();
  }

  if (mode === "tap" && e.is("keyboard:down:space") && !btn.down) {
    btn.down = true;
    keyHeld = true;
    down();
  }

  if (keyHeld && e.is("keyboard:up:space")) {
    btn.down = false;
    keyHeld = false;
    push();
  }

  if (!keyHeld) {
    btn.act(e, {
      push: () => push(),
      down: () => down(),
      cancel: () => cancel(),
    });
  }

  if (e.is("defocus")) cancel();
}

// 👋 Leave
function leave() {
  voice?.kill(0.05);
}

// 📰 Meta
function meta() {
  return {
    title: "Song",
    desc: "Tap through a song: each syllable sings a note and draws a stroke.",
  };
}

export { boot, paint, sim, act, leave, meta };

// 📚 Library
