// demo-kit.mjs — shared browser helpers for pop/demos.html. Audio
// playback, signal generators, mic capture, canvas plotting, a tiny
// drum/bell synth, music-note math, and DOM builders. Demo modules
// (pop/demos/*-demos.mjs) build on this; the real DSP they show off
// lives in dance/synths/fx.mjs, lib/analysis.mjs, lib/meter.mjs and
// lib/counterpoint.mjs.
//
// Browser-only — uses Web Audio + DOM. Not for Node.

// ── audio context ─────────────────────────────────────────────────────
export const ac = new (window.AudioContext || window.webkitAudioContext)();
export const SR = ac.sampleRate;
let liveSources = [];

// Web Audio needs a user gesture before it'll make sound — call this
// from any click handler (idempotent).
export function resume() { return ac.resume(); }

// Play a mono Float32Array. Returns the source node; tracked so stopAll
// can cut everything.
export function play(f32, { sampleRate = SR, gain = 1 } = {}) {
  const b = ac.createBuffer(1, f32.length, sampleRate);
  b.getChannelData(0).set(f32);
  const src = ac.createBufferSource();
  src.buffer = b;
  const g = ac.createGain();
  g.gain.value = gain;
  src.connect(g).connect(ac.destination);
  src.start();
  liveSources.push(src);
  src.onended = () => { liveSources = liveSources.filter((s) => s !== src); };
  return src;
}

export function stopAll() {
  for (const s of liveSources) { try { s.stop(); } catch {} }
  liveSources = [];
}

// ── signal generators (Float32Array at SR) ────────────────────────────
// type: "sine" | "saw" | "square" | "tri"
export function osc(freq, durSec, { type = "saw", gain = 0.5 } = {}) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  let phase = 0;
  const dt = freq / SR;
  for (let i = 0; i < n; i++) {
    let v;
    if (type === "sine") v = Math.sin(phase * 2 * Math.PI);
    else if (type === "square") v = phase < 0.5 ? 1 : -1;
    else if (type === "tri") v = Math.abs(phase * 2 - 1) * 2 - 1;
    else v = phase * 2 - 1; // saw
    out[i] = v * gain;
    phase += dt;
    if (phase >= 1) phase -= 1;
  }
  return out;
}

// A short A–D amplitude envelope applied in place, so generated tones
// don't click. attack/release in seconds.
export function shape(buf, { attack = 0.01, release = 0.05 } = {}) {
  const a = Math.max(1, Math.floor(attack * SR));
  const r = Math.max(1, Math.floor(release * SR));
  for (let i = 0; i < buf.length; i++) {
    let g = 1;
    if (i < a) g = i / a;
    if (i > buf.length - r) g = Math.min(g, (buf.length - i) / r);
    buf[i] *= g;
  }
  return buf;
}

export function noise(durSec, { gain = 0.5 } = {}) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) out[i] = (Math.random() * 2 - 1) * gain;
  return out;
}

// Mix several equal-length-ish buffers into one (longest wins).
export function mix(...bufs) {
  const n = Math.max(0, ...bufs.map((b) => b.length));
  const out = new Float32Array(n);
  for (const b of bufs) for (let i = 0; i < b.length; i++) out[i] += b[i];
  return out;
}

// ── mic capture ───────────────────────────────────────────────────────
// Records `durSec` of mono mic audio to a Float32Array at SR. onLevel,
// if given, gets a 0..1 progress value while recording.
export async function recordMic(durSec, onLevel) {
  await resume();
  const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
  const src = ac.createMediaStreamSource(stream);
  const proc = ac.createScriptProcessor(4096, 1, 1);
  const sink = ac.createGain();
  sink.gain.value = 0; // silent — don't echo the mic
  const need = Math.ceil(durSec * SR);
  const out = new Float32Array(need);
  let o = 0;
  return new Promise((resolve) => {
    proc.onaudioprocess = (e) => {
      const ch = e.inputBuffer.getChannelData(0);
      for (let i = 0; i < ch.length && o < need; i++) out[o++] = ch[i];
      if (onLevel) onLevel(Math.min(1, o / need));
      if (o >= need) {
        proc.onaudioprocess = null;
        src.disconnect(); proc.disconnect(); sink.disconnect();
        stream.getTracks().forEach((t) => t.stop());
        resolve(out);
      }
    };
    src.connect(proc); proc.connect(sink); sink.connect(ac.destination);
  });
}

// ── canvas plotting ───────────────────────────────────────────────────
// plot(canvas, { layers, marks }) draws onto a cream panel.
//   layers: [{ data: Float32Array, color, mode, min, max }]
//     mode "wave"  — bipolar, 0 at center, clamps ±(max||1)
//     mode "line"  — maps [min,max] (default 0..1) to full height
//     mode "fill"  — like line, filled to the baseline
//   marks:  [{ at: 0..1, color, label }]  — vertical markers
export function plot(canvas, { layers = [], marks = [] } = {}) {
  const dpr = window.devicePixelRatio || 1;
  const w = canvas.clientWidth || 600;
  const h = canvas.clientHeight || 140;
  canvas.width = w * dpr;
  canvas.height = h * dpr;
  const g = canvas.getContext("2d");
  g.scale(dpr, dpr);
  g.fillStyle = "#1c1a17";
  g.fillRect(0, 0, w, h);
  // center line
  g.strokeStyle = "rgba(255,255,255,0.12)";
  g.beginPath(); g.moveTo(0, h / 2); g.lineTo(w, h / 2); g.stroke();

  for (const L of layers) {
    const d = L.data;
    if (!d || d.length === 0) continue;
    const step = d.length / w;
    g.strokeStyle = L.color || "#e8c170";
    g.fillStyle = L.color || "#e8c170";
    g.lineWidth = 1.2;
    g.beginPath();
    for (let x = 0; x < w; x++) {
      const v = d[Math.floor(x * step)] || 0;
      let y;
      if (L.mode === "line" || L.mode === "fill") {
        const lo = L.min ?? 0, hi = L.max ?? 1;
        y = h - ((v - lo) / (hi - lo)) * h;
      } else {
        const m = L.max || 1;
        y = h / 2 - (v / m) * (h / 2);
      }
      if (x === 0) g.moveTo(x, y); else g.lineTo(x, y);
    }
    g.stroke();
    if (L.mode === "fill") {
      g.lineTo(w, h); g.lineTo(0, h); g.closePath();
      g.globalAlpha = 0.25; g.fill(); g.globalAlpha = 1;
    }
  }
  for (const m of marks) {
    const x = (m.at || 0) * w;
    g.strokeStyle = m.color || "#ff5d4a";
    g.lineWidth = 1.5;
    g.beginPath(); g.moveTo(x, 0); g.lineTo(x, h); g.stroke();
    if (m.label) {
      g.fillStyle = m.color || "#ff5d4a";
      g.font = "9px monospace";
      g.fillText(m.label, x + 2, 10);
    }
  }
}

// ── music note math ───────────────────────────────────────────────────
const NOTE_BASE = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };

// "A4" / "C#5" / "Eb3" -> MIDI number. Returns null on a bad name.
export function noteToMidi(name) {
  const m = String(name).trim().match(/^([A-Ga-g])([#b]?)(-?\d+)$/);
  if (!m) return null;
  let semi = NOTE_BASE[m[1].toLowerCase()];
  if (m[2] === "#") semi++;
  if (m[2] === "b") semi--;
  return semi + (parseInt(m[3], 10) + 1) * 12;
}
export function midiToFreq(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }
export function noteToFreq(name) {
  const m = noteToMidi(name);
  return m == null ? null : midiToFreq(m);
}
const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
export function midiToName(midi) {
  const m = Math.round(midi);
  return NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
}

// ── tiny synths (Float32Array at SR) ──────────────────────────────────
// Struck-bell tone — a few inharmonic partials with exponential decay.
export function bell(freq, durSec = 0.6, { gain = 0.5 } = {}) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  const partials = [[1, 1], [2.01, 0.5], [3.01, 0.28], [4.7, 0.16]];
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    let v = 0;
    for (const [r, amp] of partials)
      v += Math.sin(2 * Math.PI * freq * r * t) * amp * Math.exp(-t * (3 + r));
    out[i] = v * gain;
  }
  return out;
}

// One-shot drum voice. kind: "kick" | "tom" | "hat" | "snare".
export function drum(kind, durSec = 0.25, { gain = 0.7 } = {}) {
  const n = Math.floor(durSec * SR);
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    if (kind === "hat") {
      out[i] = (Math.random() * 2 - 1) * Math.exp(-t * 90) * gain;
    } else if (kind === "snare") {
      const tone = Math.sin(2 * Math.PI * 190 * t) * Math.exp(-t * 24);
      const nz = (Math.random() * 2 - 1) * Math.exp(-t * 34);
      out[i] = (tone * 0.5 + nz * 0.7) * gain;
    } else {
      const f = kind === "tom" ? 180 : 110;
      const pitch = f * (1 + 5 * Math.exp(-t * 40)); // pitch drop
      out[i] = Math.sin(2 * Math.PI * pitch * t) * Math.exp(-t * (kind === "tom" ? 16 : 11)) * gain;
    }
  }
  return out;
}

// ── DOM builders ──────────────────────────────────────────────────────
export function el(tag, attrs = {}, ...kids) {
  const n = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === "class") n.className = v;
    else if (k === "style") n.style.cssText = v;
    else if (k === "html") n.innerHTML = v;
    else if (k.startsWith("on") && typeof v === "function") n.addEventListener(k.slice(2), v);
    else if (v != null) n.setAttribute(k, v);
  }
  for (const c of kids.flat()) {
    if (c == null || c === false) continue;
    n.append(c.nodeType ? c : document.createTextNode(String(c)));
  }
  return n;
}

export function button(label, onclick) {
  return el("button", { class: "demo-btn", onclick }, label);
}

// Labelled range slider. Returns a row element with live `.value`
// (number) and `.display` updated; `oninput(value)` fires on drag.
export function slider(label, { min = 0, max = 1, step = 0.01, value = 0, unit = "" } = {}, oninput) {
  const out = el("span", { class: "demo-num" }, value + unit);
  const input = el("input", {
    type: "range", min, max, step, value,
    oninput: (e) => {
      const v = parseFloat(e.target.value);
      row.value = v;
      out.textContent = (Math.round(v * 1000) / 1000) + unit;
      oninput && oninput(v);
    },
  });
  const row = el("label", { class: "demo-ctl" }, el("span", { class: "demo-lbl" }, label), input, out);
  row.value = value;
  row.set = (v) => { input.value = v; input.dispatchEvent(new Event("input")); };
  return row;
}

// Labelled <select>. options: [value,…] or [[value,label],…].
export function picker(label, options, value, onchange) {
  const sel = el("select", {
    class: "demo-sel",
    onchange: (e) => { row.value = e.target.value; onchange && onchange(e.target.value); },
  });
  for (const o of options) {
    const [v, l] = Array.isArray(o) ? o : [o, o];
    sel.append(el("option", { value: v, ...(v === value ? { selected: "" } : {}) }, l));
  }
  const row = el("label", { class: "demo-ctl" }, el("span", { class: "demo-lbl" }, label), sel);
  row.value = value;
  return row;
}

// A status line for a demo — el with `.say(text)`.
export function status(initial = "") {
  const n = el("div", { class: "demo-status" }, initial);
  n.say = (t) => { n.textContent = t; };
  return n;
}
