// analysis-demos.mjs — interactive browser demos for the pop analysis
// primitives: envelopeFollower, pitchTrack, audioGate, and the full
// audio-to-rhythm pipeline. Loaded by pop/demos.html via import().
//
// All DSP runs on plain Float32Arrays at kit.SR (no Node imports).

import * as kit from "../lib/demo-kit.mjs";
import { pitchTrack, audioGate } from "../lib/analysis.mjs";
import { envelopeFollower, invertControl } from "../dance/synths/fx.mjs";

// ── helpers ───────────────────────────────────────────────────────────

// Place a mono buffer `src` into a longer buffer `dst` at sample offset
// `offset`. Adds into existing content (mix-friendly).
function place(dst, src, offset) {
  const end = Math.min(dst.length, offset + src.length);
  for (let i = offset; i < end; i++) dst[i] += src[i - offset];
}

// Build a short rhythmic source buffer: several drum/bell hits spread
// across `durSec` seconds.
function makeRhythmicSource(durSec = 2.5) {
  const n = Math.floor(durSec * kit.SR);
  const buf = new Float32Array(n);
  // hit schedule: [offsetSec, kind, dur, gain]
  const schedule = [
    [0.00, "kick",  0.30, 0.8],
    [0.25, "hat",   0.12, 0.5],
    [0.50, "snare", 0.25, 0.7],
    [0.75, "hat",   0.12, 0.4],
    [1.00, "kick",  0.30, 0.8],
    [1.25, "hat",   0.12, 0.5],
    [1.50, "kick",  0.30, 0.7],
    [1.75, "snare", 0.25, 0.6],
    [2.00, "kick",  0.30, 0.8],
    [2.20, "hat",   0.12, 0.4],
  ];
  for (const [t, kind, dur, gain] of schedule) {
    const hit = kit.drum(kind, dur, { gain });
    place(buf, hit, Math.floor(t * kit.SR));
  }
  // add a couple of bell accents for melodic interest
  place(buf, kit.bell(440, 0.4, { gain: 0.3 }), Math.floor(0.50 * kit.SR));
  place(buf, kit.bell(660, 0.4, { gain: 0.2 }), Math.floor(1.50 * kit.SR));
  return buf;
}

// Generate a sine sweep from `fStart` Hz to `fEnd` Hz over `durSec`.
function sineSweep(fStart, fEnd, durSec) {
  const n = Math.floor(durSec * kit.SR);
  const out = new Float32Array(n);
  let phase = 0;
  for (let i = 0; i < n; i++) {
    const t = i / n; // 0..1 normalised time
    const freq = fStart * Math.pow(fEnd / fStart, t); // exponential glide
    phase += freq / kit.SR;
    if (phase >= 1) phase -= Math.floor(phase);
    out[i] = Math.sin(phase * 2 * Math.PI) * 0.6;
  }
  // brief fade-in / fade-out to avoid clicks
  const fade = Math.floor(0.01 * kit.SR);
  for (let i = 0; i < fade && i < n; i++) out[i] *= i / fade;
  for (let i = n - fade; i < n; i++) out[i] *= (n - i) / fade;
  return out;
}

// Build a generated percussive rhythm loop for the gate / audio-to-rhythm
// demos, spread across `durSec` seconds.
function makePercussiveLoop(durSec = 3.0) {
  const n = Math.floor(durSec * kit.SR);
  const buf = new Float32Array(n);
  const pattern = [
    { t: 0.00, kind: "kick" },
    { t: 0.25, kind: "hat"  },
    { t: 0.50, kind: "tom"  },
    { t: 0.75, kind: "hat"  },
    { t: 1.00, kind: "kick" },
    { t: 1.25, kind: "hat"  },
    { t: 1.50, kind: "snare"},
    { t: 1.75, kind: "hat"  },
    { t: 2.00, kind: "kick" },
    { t: 2.25, kind: "hat"  },
    { t: 2.50, kind: "kick" },
    { t: 2.75, kind: "hat"  },
  ];
  for (const { t, kind } of pattern) {
    const hit = kit.drum(kind, 0.25, { gain: kind === "hat" ? 0.5 : 0.75 });
    place(buf, hit, Math.floor(t * kit.SR));
  }
  return buf;
}

// ── zero-crossing rate classify (mirrors bin/audio-to-rhythm.mjs) ─────
function classifyZcr(buf, startSec, sampleRate) {
  const start = Math.floor(startSec * sampleRate);
  const win = Math.min(1024, buf.length - start);
  if (win < 32) return { note: "C4", syll: "t", voice: "tom" };
  let crossings = 0;
  for (let j = 1; j < win; j++) {
    if ((buf[start + j - 1] < 0) !== (buf[start + j] < 0)) crossings++;
  }
  const zcr = crossings / win;
  if (zcr < 0.06) return { note: "C3", syll: "k", voice: "kick" };
  if (zcr < 0.18) return { note: "C4", syll: "t", voice: "tom"  };
  return             { note: "C5", syll: "h", voice: "hat"  };
}

// ── Demo 1 — envelope-follower ────────────────────────────────────────
function mountEnvelopeFollower(rootEl) {
  // pre-built rhythmic source (rebuilt on slider change only when needed)
  let source = makeRhythmicSource(2.5);

  const canvas = kit.el("canvas");
  const atkRow = kit.slider("attack ms",  { min: 1,  max: 80,  step: 1,  value: 5,  unit: " ms" }, redraw);
  const relRow = kit.slider("release ms", { min: 10, max: 400, step: 5,  value: 80, unit: " ms" }, redraw);

  const btnRow = kit.el("div", { class: "demo-row" },
    kit.button("▶ play", () => { kit.resume(); kit.stopAll(); kit.play(source, { gain: 0.85 }); }),
    atkRow, relRow,
  );

  rootEl.append(btnRow, canvas);

  function redraw() {
    const env = envelopeFollower(source, {
      sampleRate: kit.SR,
      attackMs:  atkRow.value,
      releaseMs: relRow.value,
    });
    const inv = invertControl(env);
    kit.plot(canvas, {
      layers: [
        { data: source, color: "#8a7f66", mode: "wave", max: 1 },
        { data: env,    color: "#39c0d8", mode: "line", min: 0, max: 1 },
        { data: inv,    color: "#e8a05a", mode: "line", min: 0, max: 1 },
      ],
    });
  }

  redraw();
}

// ── Demo 2 — pitch-track ──────────────────────────────────────────────
function mountPitchTrack(rootEl) {
  let source = null;
  let frames = [];

  const canvas  = kit.el("canvas");
  const noteOut = kit.el("div", { class: "demo-out" }, "— no source yet —");
  const stat    = kit.status("use demo or record to load a source.");

  // Map frames → per-sample MIDI float array for plotting (NaN = unvoiced)
  function framesToMidiArray(buf, frms) {
    const out = new Float32Array(buf.length).fill(NaN);
    if (!frms.length) return out;
    for (let fi = 0; fi < frms.length; fi++) {
      const startSample = Math.round(frms[fi].time * kit.SR);
      const endSample   = fi < frms.length - 1
        ? Math.round(frms[fi + 1].time * kit.SR)
        : buf.length;
      const val = frms[fi].midi != null ? frms[fi].midi : NaN;
      for (let s = startSample; s < endSample && s < out.length; s++) out[s] = val;
    }
    return out;
  }

  function runAndPlot(buf) {
    source = buf;
    frames = pitchTrack(buf, {
      sampleRate: kit.SR,
      fmin: 65, fmax: 1200,
      hopMs: 10, winMs: 40,
      clarityGate: 0.5,
    });
    const midiArr = framesToMidiArray(buf, frames);
    kit.plot(canvas, {
      layers: [
        { data: buf,     color: "#8a7f66", mode: "wave", max: 1 },
        { data: midiArr, color: "#39c0d8", mode: "line", min: 36, max: 84 },
      ],
    });
    // summarise voiced frames
    const voiced = frames.filter((f) => f.midi != null);
    if (voiced.length === 0) {
      noteOut.textContent = "no pitched frames detected.";
    } else {
      // sample up to 12 evenly-spaced voiced frames
      const step = Math.max(1, Math.floor(voiced.length / 12));
      const sampled = voiced.filter((_, i) => i % step === 0).slice(0, 12);
      noteOut.textContent = sampled
        .map((f) => `${f.time.toFixed(2)}s  ${kit.midiToName(Math.round(f.midi))}  (${f.hz.toFixed(1)} Hz)`)
        .join("\n");
    }
    stat.say(`${voiced.length} voiced frames / ${frames.length} total`);
  }

  const btnDemo = kit.button("demo (sine sweep)", () => {
    kit.resume();
    const buf = sineSweep(120, 600, 2.5);
    stat.say("running pitch tracker…");
    runAndPlot(buf);
  });

  const btnRec = kit.button("● record 3s", async () => {
    kit.resume();
    btnRec.disabled = true;
    try {
      stat.say("recording… hum or sing.");
      const buf = await kit.recordMic(3, (p) => stat.say(`recording ${Math.round(p * 100)}%…`));
      stat.say("running pitch tracker…");
      runAndPlot(buf);
    } catch (err) {
      stat.say("mic denied or unavailable — use demo instead.");
    } finally {
      btnRec.disabled = false;
    }
  });

  const btnPlay = kit.button("▶ play", () => {
    if (!source) { stat.say("load a source first."); return; }
    kit.resume(); kit.stopAll(); kit.play(source, { gain: 0.85 });
  });

  rootEl.append(
    kit.el("div", { class: "demo-row" }, btnDemo, btnRec, btnPlay),
    canvas,
    noteOut,
    stat,
  );
}

// ── Demo 3 — audio-gate ───────────────────────────────────────────────
function mountAudioGate(rootEl) {
  let source     = makePercussiveLoop(3.0);
  let triggers   = [];
  let currentThresh = 0.06;

  const canvas   = kit.el("canvas");
  const stat     = kit.status("ready.");

  function recompute() {
    triggers = audioGate(source, {
      sampleRate: kit.SR,
      threshold: currentThresh,
      attackMs: 2,
      releaseMs: 40,
      minGapMs: 60,
    });
    const durSec = source.length / kit.SR;
    const marks = triggers.map((tr) => ({
      at: tr.time / durSec,
      color: "#ff5d4a",
    }));
    kit.plot(canvas, {
      layers: [{ data: source, color: "#8a7f66", mode: "wave", max: 1 }],
      marks,
    });
    stat.say(`${triggers.length} trigger${triggers.length !== 1 ? "s" : ""} detected.`);
  }

  const threshRow = kit.slider("threshold", { min: 0.01, max: 0.3, step: 0.005, value: 0.06 }, (v) => {
    currentThresh = v;
    recompute();
  });

  const btnDemo = kit.button("demo loop", () => {
    kit.resume();
    source = makePercussiveLoop(3.0);
    recompute();
  });

  const btnRec = kit.button("● record 3s", async () => {
    kit.resume();
    btnRec.disabled = true;
    try {
      stat.say("recording… beatbox or tap.");
      source = await kit.recordMic(3, (p) => stat.say(`recording ${Math.round(p * 100)}%…`));
      recompute();
    } catch {
      stat.say("mic denied — use demo loop instead.");
    } finally {
      btnRec.disabled = false;
    }
  });

  const btnPlay = kit.button("▶ play with clicks", () => {
    kit.resume(); kit.stopAll();
    if (!triggers.length) { kit.play(source, { gain: 0.8 }); return; }

    // Mix hat clicks at each trigger time into a copy of the source
    const mixed  = new Float32Array(source);           // copy, don't mutate
    const hatBuf = kit.drum("hat", 0.05, { gain: 0.6 });
    for (const tr of triggers) {
      place(mixed, hatBuf, Math.floor(tr.time * kit.SR));
    }
    kit.play(mixed, { gain: 0.85 });
  });

  rootEl.append(
    kit.el("div", { class: "demo-row" }, btnDemo, btnRec, btnPlay, threshRow),
    canvas,
    stat,
  );

  recompute(); // initial plot with demo loop
}

// ── Demo 4 — audio-to-rhythm ──────────────────────────────────────────
function mountAudioToRhythm(rootEl) {
  let source = null;
  let lastNp = "";

  const npOut  = kit.el("div", { class: "demo-out" }, "— record or load a demo, then build .np —");
  const stat   = kit.status("ready.");

  const bpmRow  = kit.slider("bpm",  { min: 60, max: 160, step: 1, value: 120, unit: " bpm" }, () => {});
  const gridRow = kit.picker("grid", [["0.25","16ths"],["0.5","8ths"],["1","quarters"]], "0.25", () => {});
  const threshRow = kit.slider("threshold", { min: 0.01, max: 0.3, step: 0.005, value: 0.06 }, () => {});

  function buildNp(buf) {
    const bpm    = bpmRow.value;
    const grid   = parseFloat(gridRow.value);
    const thresh = threshRow.value;
    const sr     = kit.SR;

    const onsets = audioGate(buf, { sampleRate: sr, threshold: thresh, attackMs: 2, releaseMs: 40, minGapMs: 60 });
    if (onsets.length === 0) {
      npOut.textContent = "no onsets detected — try a lower threshold.";
      stat.say("0 hits.");
      return;
    }

    const beatPerSec = bpm / 60;
    const hits = [];
    let lastStep = -1;
    for (const o of onsets) {
      const beat = o.time * beatPerSec;
      const step = Math.round(beat / grid);
      if (step === lastStep) continue;
      lastStep = step;
      hits.push({ step, ...classifyZcr(buf, o.time, sr) });
    }

    // build cells — weight = beats until next hit
    const cells = [];
    for (let i = 0; i < hits.length; i++) {
      const beats = i < hits.length - 1
        ? (hits[i + 1].step - hits[i].step) * grid
        : grid;
      const w = Number(beats.toFixed(3));
      cells.push({ cell: `${hits[i].note}:${hits[i].syll}*${w}`, voice: hits[i].voice, beats: w });
    }

    // lay 4 beats per line
    const lines = [];
    let lineBeats = 0, line = [];
    for (let i = 0; i < cells.length; i++) {
      line.push(cells[i].cell);
      lineBeats += cells[i].beats;
      if (lineBeats >= 4) { lines.push(line.join(" ")); line = []; lineBeats = 0; }
    }
    if (line.length) lines.push(line.join(" "));

    const totalBeats = (hits[hits.length - 1].step - hits[0].step) * grid + grid;
    const bars = Math.max(1, Math.ceil(totalBeats / 4));
    lastNp =
      `# rhythm ${bars} — browser capture @ ${bpm} bpm\n` +
      `# ${hits.length} hits · ${grid}-beat grid · k=kick t=tom h=hat\n\n` +
      lines.join("\n") + "\n";

    npOut.textContent = lastNp;
    stat.say(`${hits.length} hits, ${bars} bar${bars !== 1 ? "s" : ""}.`);
    return cells;
  }

  const btnDemo = kit.button("demo loop", () => {
    kit.resume();
    source = makePercussiveLoop(4.0);
    buildNp(source);
  });

  const btnRec = kit.button("● record 4s", async () => {
    kit.resume();
    btnRec.disabled = true;
    try {
      stat.say("recording… beatbox a loop.");
      source = await kit.recordMic(4, (p) => stat.say(`recording ${Math.round(p * 100)}%…`));
      stat.say("building .np…");
      buildNp(source);
    } catch {
      stat.say("mic denied — use demo loop instead.");
    } finally {
      btnRec.disabled = false;
    }
  });

  const btnBuild = kit.button("rebuild .np", () => {
    if (!source) { stat.say("load a source first."); return; }
    buildNp(source);
  });

  const btnPlay = kit.button("▶ play rhythm", () => {
    if (!source) { stat.say("load a source first."); return; }
    kit.resume(); kit.stopAll();

    // re-run the pipeline fresh to get the hit list for synthesis
    const bpm    = bpmRow.value;
    const grid   = parseFloat(gridRow.value);
    const thresh = threshRow.value;
    const sr     = kit.SR;
    const secPerBeat = 60 / bpm;

    const onsets = audioGate(source, { sampleRate: sr, threshold: thresh, attackMs: 2, releaseMs: 40, minGapMs: 60 });
    if (onsets.length === 0) { stat.say("no hits to play."); return; }

    const beatPerSec = bpm / 60;
    const hits = [];
    let lastStep = -1;
    for (const o of onsets) {
      const step = Math.round(o.time * beatPerSec / grid);
      if (step === lastStep) continue;
      lastStep = step;
      hits.push({ step, ...classifyZcr(source, o.time, sr) });
    }

    // build a single mixed playback buffer at the quantized grid
    const firstStep = hits[0].step;
    const lastStep2 = hits[hits.length - 1].step;
    const totalBeats = (lastStep2 - firstStep + 1) * grid;
    const totalSamples = Math.ceil(totalBeats * secPerBeat * sr) + sr; // +1s tail
    const playBuf = new Float32Array(totalSamples);

    for (const h of hits) {
      const offsetBeats = (h.step - firstStep) * grid;
      const offsetSamples = Math.floor(offsetBeats * secPerBeat * sr);
      const hitBuf = kit.drum(h.voice, 0.3, {
        gain: h.voice === "hat" ? 0.5 : 0.75,
      });
      place(playBuf, hitBuf, offsetSamples);
    }

    kit.play(playBuf, { gain: 0.9 });
    stat.say(`playing ${hits.length} hits.`);
  });

  rootEl.append(
    kit.el("div", { class: "demo-row" }, btnDemo, btnRec),
    kit.el("div", { class: "demo-row" }, bpmRow, gridRow, threshRow),
    kit.el("div", { class: "demo-row" }, btnBuild, btnPlay),
    npOut,
    stat,
  );
}

// ── export ────────────────────────────────────────────────────────────
export const demos = {
  "envelope-follower": {
    title:  "envelope-follower",
    blurb:  "tracks the amplitude contour of a signal into a 0..1 control curve — plus its inverse.",
    accent: "#1d7a8a",
    mount:  mountEnvelopeFollower,
  },
  "pitch-track": {
    title:  "pitch-track",
    blurb:  "autocorrelation pitch detection — hum into the mic and watch the f0 curve.",
    accent: "#1d7a8a",
    mount:  mountPitchTrack,
  },
  "audio-gate": {
    title:  "audio-gate",
    blurb:  "detects amplitude onsets — every trigger fires a click.",
    accent: "#1d7a8a",
    mount:  mountAudioGate,
  },
  "audio-to-rhythm": {
    title:  "audio-to-rhythm",
    blurb:  "beatbox a loop → onset-detected, quantized drum .np you can play back.",
    accent: "#5a6b1f",
    mount:  mountAudioToRhythm,
  },
};
