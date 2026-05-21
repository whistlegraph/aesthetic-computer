// structure-demos.mjs — interactive demos for meter.mjs and counterpoint.mjs.
// Covers Fibonacci meter / partition, .np note subdivision, and first-species
// counterpoint generation + rule-checking. Browser-only; no Node imports.

import * as kit from "../lib/demo-kit.mjs";
import {
  fibSequence,
  fibPartition,
  fibMeter,
  subdivideNp,
} from "../lib/meter.mjs";
import {
  checkFirstSpecies,
  generateFirstSpecies,
} from "../lib/counterpoint.mjs";

// ── helpers ───────────────────────────────────────────────────────────────

// Concatenate an array of Float32Arrays into one.
function concat(bufs) {
  const total = bufs.reduce((s, b) => s + b.length, 0);
  const out = new Float32Array(total);
  let off = 0;
  for (const b of bufs) { out.set(b, off); off += b.length; }
  return out;
}

// Parse one .np cell token: `NOTE:syll*weight` or chord `A4+C5:syll*weight`.
// Returns { note: string, weight: number } or null.
function parseCell(tok) {
  const m = tok.match(/^([^:]+):([^*]*)\*([\d.]+)$/);
  if (!m) return null;
  const notePart = m[1].split("+")[0]; // first note of a chord
  return { note: notePart, weight: parseFloat(m[3]) };
}

// Parse a whole .np text into [{note, weight}], ignoring comment lines.
function parseNp(text) {
  const cells = [];
  for (const line of text.split("\n")) {
    const t = line.trim();
    if (t === "" || t.startsWith("#")) continue;
    for (const tok of t.split(/\s+/)) {
      const c = parseCell(tok);
      if (c) cells.push(c);
    }
  }
  return cells;
}

// Synthesise a .np cell sequence at ~120 bpm (1 beat = 0.5 s).
function renderNp(cells, beatSec = 0.5) {
  const bufs = cells.map(({ note, weight }) => {
    const freq = kit.noteToFreq(note);
    if (!freq) return new Float32Array(Math.floor(weight * beatSec * kit.SR));
    return kit.bell(freq, weight * beatSec, { gain: 0.45 });
  });
  return bufs.length ? concat(bufs) : new Float32Array(1);
}

// ── demo 1: fib-meter ─────────────────────────────────────────────────────

function mountFibMeter(rootEl) {
  // ── state
  let bars = 16;

  // ── DOM: slider
  const sl = kit.slider(
    "bars",
    { min: 4, max: 64, step: 1, value: bars, unit: " bars" },
    (v) => { bars = v; redraw(); },
  );

  // ── sequence text
  const seqOut = kit.el("div", { class: "demo-out", style: "min-height:2em" });

  // ── partition blocks
  const partRow = kit.el("div", {
    style:
      "display:flex;gap:0;width:100%;height:36px;border:1px solid #b3a98f;overflow:hidden",
  });

  // ── meter timeline
  const meterRow = kit.el("div", {
    style:
      "display:flex;gap:0;width:100%;height:36px;border:1px solid #b3a98f;overflow:hidden",
  });

  // ── play button
  const playBtn = kit.button("▶ play accents", () => {
    kit.resume();
    kit.stopAll();
    const [a] = fibPartition(bars);
    // beat = 0.25s; accent on beat 0, and at bar `a`
    const beatSec = 0.25;
    const totalSec = bars * beatSec + 0.5;
    const n = Math.floor(totalSec * kit.SR);
    const buf = new Float32Array(n);
    const write = (sec, kind) => {
      const d = kit.drum(kind, 0.18, { gain: 0.8 });
      const off = Math.floor(sec * kit.SR);
      for (let i = 0; i < d.length && off + i < n; i++) buf[off + i] += d[i];
    };
    write(0, "kick");
    write(a * beatSec, "snare");
    kit.play(buf, { gain: 0.9 });
  });

  const COLORS_PART = ["#8a5223", "#c49a6c"];
  const COLORS_METER = [
    "#8a5223", "#b07040", "#c49a6c", "#d4b88a", "#a06838",
    "#7a4218", "#e0c8a0", "#6b3614",
  ];

  function redraw() {
    // sequence
    const seq = fibSequence(10).filter((v) => v <= bars);
    seqOut.textContent = "fib[≤" + bars + "]: " + seq.join(", ");

    // partition
    const [a, b] = fibPartition(bars);
    partRow.innerHTML = "";
    for (const [val, ci] of [[a, 0], [b, 1]]) {
      const pct = (val / bars) * 100;
      const blk = kit.el(
        "div",
        {
          style: `flex:0 0 ${pct}%;background:${COLORS_PART[ci]};` +
            "display:flex;align-items:center;justify-content:center;" +
            "color:#f4efe2;font-size:11px;overflow:hidden;white-space:nowrap",
        },
        String(val),
      );
      partRow.append(blk);
    }

    // meter
    const sections = fibMeter(bars);
    meterRow.innerHTML = "";
    sections.forEach((len, idx) => {
      const pct = (len / bars) * 100;
      const color = COLORS_METER[idx % COLORS_METER.length];
      const blk = kit.el(
        "div",
        {
          style: `flex:0 0 ${pct}%;background:${color};` +
            "display:flex;align-items:center;justify-content:center;" +
            "color:#f4efe2;font-size:10px;overflow:hidden;white-space:nowrap;" +
            "border-right:1px solid rgba(0,0,0,0.18)",
        },
        String(len),
      );
      meterRow.append(blk);
    });
  }

  rootEl.append(
    kit.el("div", { class: "demo-row" }, sl),
    kit.el("div", { class: "demo-row" },
      kit.el("span", { style: "font-size:10px;color:#6b6353" }, "fibonacci sequence ≤ bars"),
    ),
    seqOut,
    kit.el("div", { class: "demo-row" },
      kit.el("span", { style: "font-size:10px;color:#6b6353" }, "golden partition (two blocks)"),
    ),
    partRow,
    kit.el("div", { class: "demo-row" },
      kit.el("span", { style: "font-size:10px;color:#6b6353" }, "fibMeter sections (additive)"),
    ),
    meterRow,
    kit.el("div", { class: "demo-row" }, playBtn),
  );

  redraw();
}

// ── demo 2: note-subdiv ───────────────────────────────────────────────────

const SUBDIV_DEFAULT = `# phrase A
C4:_*2 E4:_*2 G4:_*4
# phrase B
A4:_*1 G4:_*2 F4:_*1 E4:_*4`;

function mountNoteSubdiv(rootEl) {
  let parts = 2;
  let minBeats = 0;

  const ta = kit.el("textarea", { rows: 4 }, SUBDIV_DEFAULT);
  ta.value = SUBDIV_DEFAULT;

  const outBox = kit.el("div", { class: "demo-out", style: "min-height:3em" });

  const partsPicker = kit.picker(
    "parts",
    ["2", "3", "4", "8", "16"],
    "2",
    (v) => { parts = parseInt(v, 10); redraw(); },
  );

  const mbSlider = kit.slider(
    "minBeats",
    { min: 0, max: 4, step: 1, value: 0, unit: " beats" },
    (v) => { minBeats = v; redraw(); },
  );

  const playBeforeBtn = kit.button("▶ play before", () => {
    kit.resume();
    kit.stopAll();
    const cells = parseNp(ta.value);
    if (!cells.length) return;
    kit.play(renderNp(cells), { gain: 0.85 });
  });

  const playAfterBtn = kit.button("▶ play after", () => {
    kit.resume();
    kit.stopAll();
    const divided = subdivideNp(ta.value, { parts, minBeats });
    const cells = parseNp(divided);
    if (!cells.length) return;
    kit.play(renderNp(cells), { gain: 0.85 });
  });

  function redraw() {
    try {
      outBox.textContent = subdivideNp(ta.value, { parts, minBeats });
      outBox.classList.remove("demo-err");
    } catch (err) {
      outBox.textContent = "error: " + err.message;
      outBox.classList.add("demo-err");
    }
  }

  ta.addEventListener("input", redraw);

  rootEl.append(
    ta,
    kit.el("div", { class: "demo-row" }, partsPicker, mbSlider),
    kit.el("div", { class: "demo-row" },
      kit.el("span", { style: "font-size:10px;color:#6b6353" }, "subdivided output"),
    ),
    outBox,
    kit.el("div", { class: "demo-row" }, playBeforeBtn, playAfterBtn),
  );

  redraw();
}

// ── demo 3: species-counterpoint ─────────────────────────────────────────

const CANTUS_PRESETS = [
  {
    label: "Fux D-dorian",
    midi: [62, 65, 64, 62, 67, 65, 69, 67, 65, 64, 62],
    scale: [0, 2, 3, 5, 7, 8, 10], // natural minor / dorian share these degrees
  },
  {
    label: "A-minor short",
    midi: [57, 60, 59, 57, 62, 60, 64, 62, 60, 59, 57],
    scale: [0, 2, 3, 5, 7, 8, 10],
  },
  {
    label: "C-major short",
    midi: [60, 62, 64, 62, 65, 64, 67, 65, 64, 62, 60],
    scale: [0, 2, 4, 5, 7, 9, 11],
  },
];

function mountSpeciesCounterpoint(rootEl) {
  let presetIdx = 0;
  let counter = null;

  const canvas = kit.el("canvas", { style: "width:100%;height:140px" });
  const checkOut = kit.el("div", { class: "demo-out", style: "min-height:2.5em" });
  const st = kit.status("— generate a counterpoint to begin —");

  const presetPicker = kit.picker(
    "cantus",
    CANTUS_PRESETS.map((p, i) => [String(i), p.label]),
    "0",
    (v) => { presetIdx = parseInt(v, 10); counter = null; redraw(); },
  );

  const genBtn = kit.button("generate", () => {
    const p = CANTUS_PRESETS[presetIdx];
    const tonic = p.midi[0] % 12;
    st.say("generating…");
    // generateFirstSpecies is synchronous — run immediately
    const result = generateFirstSpecies(p.midi, { tonic, scale: p.scale });
    if (!result) {
      counter = null;
      st.say("no valid first-species found for this cantus.");
    } else {
      counter = result;
      st.say("generated · " + result.map((m) => kit.midiToName(m)).join(" "));
    }
    redraw();
  });

  const playBtn = kit.button("▶ play", () => {
    if (!counter) return;
    kit.resume();
    kit.stopAll();
    const p = CANTUS_PRESETS[presetIdx];
    const noteSec = 0.55;
    const n = p.midi.length;
    const totalSamples = Math.floor(n * noteSec * kit.SR);
    const buf = new Float32Array(totalSamples);
    for (let i = 0; i < n; i++) {
      const cantusBuf = kit.bell(kit.midiToFreq(p.midi[i]), noteSec, { gain: 0.45 });
      const counterBuf = kit.bell(kit.midiToFreq(counter[i]), noteSec, { gain: 0.45 });
      const off = Math.floor(i * noteSec * kit.SR);
      for (let j = 0; j < cantusBuf.length && off + j < totalSamples; j++)
        buf[off + j] += cantusBuf[j];
      for (let j = 0; j < counterBuf.length && off + j < totalSamples; j++)
        buf[off + j] += counterBuf[j];
    }
    kit.play(buf, { gain: 0.9 });
  });

  function drawRoll() {
    const p = CANTUS_PRESETS[presetIdx];
    const cantus = p.midi;
    const dpr = window.devicePixelRatio || 1;
    const w = canvas.clientWidth || 600;
    const h = canvas.clientHeight || 140;
    canvas.width = w * dpr;
    canvas.height = h * dpr;
    const g = canvas.getContext("2d");
    g.scale(dpr, dpr);
    g.fillStyle = "#1c1a17";
    g.fillRect(0, 0, w, h);

    const allMidi = counter ? [...cantus, ...counter] : [...cantus];
    const lo = Math.min(...allMidi) - 1;
    const hi = Math.max(...allMidi) + 1;
    const range = hi - lo || 1;
    const n = cantus.length;
    const cellW = w / n;
    const pad = 3;

    const midiY = (m) => h - ((m - lo) / range) * (h - pad * 2) - pad;

    // draw grid lines (light)
    g.strokeStyle = "rgba(255,255,255,0.06)";
    g.lineWidth = 0.5;
    for (let m = lo; m <= hi; m++) {
      const y = midiY(m);
      g.beginPath(); g.moveTo(0, y); g.lineTo(w, y); g.stroke();
    }

    // cantus
    const barH = Math.max(3, Math.floor(((h - pad * 2) / range) * 0.7));
    for (let i = 0; i < n; i++) {
      const x = i * cellW + 1;
      const y = midiY(cantus[i]) - barH / 2;
      g.fillStyle = "#8a7f66";
      g.fillRect(x, y, cellW - 2, barH);
      // note name label
      g.fillStyle = "#f4efe2";
      g.font = `${Math.min(9, barH - 1)}px monospace`;
      g.fillText(kit.midiToName(cantus[i]), x + 2, y + barH - 1);
    }

    // counterpoint
    if (counter) {
      for (let i = 0; i < n; i++) {
        const x = i * cellW + 1;
        const y = midiY(counter[i]) - barH / 2;
        g.fillStyle = "#8a5223";
        g.fillRect(x, y, cellW - 2, barH);
        g.fillStyle = "#f4efe2";
        g.font = `${Math.min(9, barH - 1)}px monospace`;
        g.fillText(kit.midiToName(counter[i]), x + 2, y + barH - 1);
      }
      // connect voices with faint lines
      g.strokeStyle = "rgba(200,180,140,0.2)";
      g.lineWidth = 0.7;
      for (let i = 0; i < n; i++) {
        const x = i * cellW + cellW / 2;
        g.beginPath();
        g.moveTo(x, midiY(cantus[i]));
        g.lineTo(x, midiY(counter[i]));
        g.stroke();
      }
    }
  }

  function showCheck() {
    if (!counter) {
      checkOut.textContent = "";
      return;
    }
    const p = CANTUS_PRESETS[presetIdx];
    const result = checkFirstSpecies(p.midi, counter);
    const lines = [];
    if (result.ok && result.warnings.length === 0) {
      lines.push("✓ valid first species — no violations, no warnings");
    } else if (result.ok) {
      lines.push("✓ valid (no violations)");
    } else {
      lines.push("✗ violations:");
      for (const v of result.violations)
        lines.push(`  [${v.index}] ${v.rule}: ${v.detail}`);
    }
    if (result.warnings.length) {
      lines.push("⚠ warnings:");
      for (const w of result.warnings)
        lines.push(`  [${w.index}] ${w.rule}: ${w.detail}`);
    }
    checkOut.textContent = lines.join("\n");
  }

  function redraw() {
    drawRoll();
    showCheck();
  }

  rootEl.append(
    kit.el("div", { class: "demo-row" }, presetPicker, genBtn, playBtn),
    canvas,
    kit.el("div", { class: "demo-row" },
      kit.el("span", { style: "font-size:10px;color:#6b6353" },
        "cantus firmus (tan) · counterpoint (brown) — one rect per note"),
    ),
    checkOut,
    st,
  );

  redraw();
}

// ── exports ───────────────────────────────────────────────────────────────

export const demos = {
  "fib-meter": {
    title: "fib-meter",
    blurb:
      "Fibonacci division & addition for bars — the golden split of a measure and a whole form.",
    accent: "#8a5223",
    mount: mountFibMeter,
  },
  "note-subdiv": {
    title: "note-subdiv",
    blurb:
      "split .np notes into 1/2 · 1/4 · 1/8 · 1/16 — Ableton-style subdivision.",
    accent: "#5a6b1f",
    mount: mountNoteSubdiv,
  },
  "species-counterpoint": {
    title: "species-counterpoint",
    blurb:
      "generate a Fux first-species counterpoint above a cantus firmus — then rule-check it.",
    accent: "#8a5223",
    mount: mountSpeciesCounterpoint,
  },
};
