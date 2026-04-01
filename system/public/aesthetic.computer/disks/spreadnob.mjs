// Spreadnob, 2026.03.31
// AC-native UI for the Ableton spreadnob device.
// Big knob display — white keys A..L map linearly across the parameter range.
// M4L → HTML bridge (acSn*) → bios send → disk → sound.spreadnob

import { getNoteColorWithOctave } from "../lib/note-colors.mjs";

const FONT = "YWFTProcessing-Regular";
const MINI_FONT = "MatrixChunky8";

const KEY_LABELS = ["A", "S", "D", "F", "G", "H", "J", "K", "L"];
const KEY_COUNT = KEY_LABELS.length;
const WHITE_OFFSETS = [0, 2, 4, 5, 7, 9, 11, 12, 14];
const PITCH_CLASSES = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"];
const DEFAULT_BASE = 48;

const ARC_START = 210;
const ARC_END = -30;
const ARC_SWEEP = ARC_START - ARC_END;

let active = true;
let target = "";
let value = null;
let currentNote = null;
let base = DEFAULT_BASE;
let paramMin = 0;
let paramMax = 1;
let flash = 0;
let noteHits = 0;
let lastSnNote = null;

function clamp(n, lo, hi) { return Math.max(lo, Math.min(hi, n)); }
function deg2rad(d) { return (d * Math.PI) / 180; }
function midiForKey(i) { return base + WHITE_OFFSETS[i]; }

function whiteKeyIndex(midi) {
  if (!Number.isFinite(midi)) return -1;
  return WHITE_OFFSETS.indexOf(midi - base);
}

function valueForIndex(i) {
  return (i / (KEY_COUNT - 1)) * (paramMax - paramMin) + paramMin;
}

function noteLabel(midi) {
  if (!Number.isFinite(midi)) return "--";
  const pitch = PITCH_CLASSES[((midi % 12) + 12) % 12].toUpperCase();
  return `${pitch}${Math.floor(midi / 12) - 1}`;
}

function noteColor(midi) {
  if (!Number.isFinite(midi)) return [190, 190, 190];
  const pitch = PITCH_CLASSES[((midi % 12) + 12) % 12];
  return getNoteColorWithOctave(pitch, Math.floor(midi / 12) - 1);
}

function angleForIndex(i) {
  return ARC_START - (i / (KEY_COUNT - 1)) * ARC_SWEEP;
}

function angleForValue(v) {
  if (paramMax === paramMin) return ARC_START;
  return ARC_START - clamp((v - paramMin) / (paramMax - paramMin), 0, 1) * ARC_SWEEP;
}

function arcXY(cx, cy, r, deg) {
  const rad = deg2rad(deg);
  return [cx + r * Math.cos(rad), cy - r * Math.sin(rad)];
}

function boot() {}

function sim({ sound, needsPaint }) {
  let dirty = false;
  const sn = sound?.spreadnob;
  if (sn) {
    if (sn.active !== null && sn.active !== undefined) {
      active = !!Number(sn.active);
      sn.active = null;
      dirty = true;
    }
    if (sn.target !== null && sn.target !== undefined) {
      target = String(sn.target).trim();
      sn.target = null;
      dirty = true;
    }
    if (sn.min !== null && sn.min !== undefined) {
      const n = Number(sn.min);
      if (Number.isFinite(n)) paramMin = n;
      sn.min = null;
      dirty = true;
    }
    if (sn.max !== null && sn.max !== undefined) {
      const n = Number(sn.max);
      if (Number.isFinite(n)) paramMax = n;
      sn.max = null;
      dirty = true;
    }
    if (sn.value !== null && sn.value !== undefined) {
      const n = Number(sn.value);
      value = Number.isFinite(n) ? n : null;
      sn.value = null;
      dirty = true;
    }
    if (sn.note !== null && sn.note !== undefined) {
      const n = Number(sn.note);
      if (n !== lastSnNote) {
        currentNote = Number.isFinite(n) ? n : null;
        lastSnNote = n;
        const idx = whiteKeyIndex(currentNote);
        if (idx >= 0) {
          value = valueForIndex(idx);
          noteHits++;
          flash = active ? 1 : 0.45;
        }
        dirty = true;
      }
    }
  }

  if (flash > 0) {
    flash *= 0.82;
    if (flash < 0.025) flash = 0;
    dirty = true;
  }
  if (dirty) needsPaint();
}

function paint({ wipe, ink, screen }) {
  const w = screen.width;
  const h = screen.height;
  const flashAlpha = Math.round(100 * flash);
  const hitIdx = whiteKeyIndex(currentNote);

  const cx = Math.round(w / 2);
  const cy = Math.round(h * 0.54);
  const outerR = Math.min(w * 0.14, h * 0.32);
  const trackR = outerR - 2;
  const tickOuter = outerR + 4;
  const tickInner = outerR;
  const labelR = outerR + 14;

  wipe(8, 10, 12);
  ink(active ? 16 : 30, active ? 28 : 14, active ? 18 : 20).box(0, 0, w, h);

  if (flashAlpha > 0) {
    ink(active ? 80 : 200, active ? 200 : 80, 120, flashAlpha).box(0, 0, w, h);
  }

  // Track arc
  const arcSteps = 40;
  const arcStep = (ARC_START - ARC_END) / arcSteps;
  for (let i = 0; i < arcSteps; i++) {
    const a1 = ARC_END + i * arcStep;
    const a2 = a1 + arcStep;
    const [x1, y1] = arcXY(cx, cy, trackR, a1);
    const [x2, y2] = arcXY(cx, cy, trackR, a2);
    ink(45, 40, 55).line(Math.round(x1), Math.round(y1), Math.round(x2), Math.round(y2));
  }

  // Value arc
  if (value !== null) {
    const valAngle = angleForValue(value);
    const vr = active ? 110 : 255;
    const vg = active ? 255 : 110;
    const vb = active ? 150 : 170;
    const sweepStart = Math.min(valAngle, ARC_START);
    const sweepEnd = Math.max(valAngle, ARC_START);
    const valSteps = Math.max(4, Math.round(Math.abs(sweepEnd - sweepStart) / 6));
    const valStep = (sweepEnd - sweepStart) / valSteps;
    for (let i = 0; i < valSteps; i++) {
      const a1 = sweepStart + i * valStep;
      const a2 = a1 + valStep;
      const [x1, y1] = arcXY(cx, cy, trackR, a1);
      const [x2, y2] = arcXY(cx, cy, trackR, a2);
      ink(vr, vg, vb).line(Math.round(x1), Math.round(y1), Math.round(x2), Math.round(y2));
    }
    const [vx, vy] = arcXY(cx, cy, trackR, valAngle);
    ink(255, 255, 255).box(Math.round(vx) - 2, Math.round(vy) - 2, 5, 5);
  }

  // Tick marks and key labels
  for (let i = 0; i < KEY_COUNT; i++) {
    const angle = angleForIndex(i);
    const [ox, oy] = arcXY(cx, cy, tickOuter, angle);
    const [ix, iy] = arcXY(cx, cy, tickInner, angle);
    const [lx, ly] = arcXY(cx, cy, labelR, angle);

    const isCurrent = hitIdx === i;
    const midi = midiForKey(i);
    const rgb = noteColor(midi);

    const tr = isCurrent ? 255 : Math.round(rgb[0] * 0.7);
    const tg = isCurrent ? 255 : Math.round(rgb[1] * 0.7);
    const tb = isCurrent ? 255 : Math.round(rgb[2] * 0.7);
    ink(tr, tg, tb).line(Math.round(ox), Math.round(oy), Math.round(ix), Math.round(iy));

    const lr = isCurrent ? 255 : Math.round(rgb[0] * 0.55);
    const lg = isCurrent ? 255 : Math.round(rgb[1] * 0.55);
    const lb = isCurrent ? 255 : Math.round(rgb[2] * 0.55);
    ink(lr, lg, lb).write(
      KEY_LABELS[i],
      { x: Math.round(lx) - 3, y: Math.round(ly) - 4 },
      undefined, undefined, false, MINI_FONT,
    );
  }

  // Center info
  const targetName = target || "click a knob";
  ink(255, 190, 220).write(targetName, { x: cx - 28, y: cy - 12 }, undefined, 56, true, FONT);

  const valueText = value === null ? "--" : value.toFixed(2);
  ink(active ? 130 : 255, active ? 255 : 130, active ? 170 : 190).write(
    valueText, { x: cx - 16, y: cy + 2 }, undefined, undefined, false, FONT,
  );

  // ON/OFF pill
  const pFill = active ? [90, 220, 100] : [220, 70, 80];
  ink(...pFill).box(w - 36, 5, 30, 10);
  ink(10, 16, 12).write(active ? "ON" : "OFF", { x: w - 32, y: 6 }, undefined, undefined, false, MINI_FONT);

  // Title
  ink(200, 120, 170).write("spreadnob", { x: 6, y: 4 }, undefined, undefined, false, MINI_FONT);
}

function act({ event, screen, needsPaint }) {
  needsPaint();
}

function leave() {}

function meta() {
  return {
    title: "Spreadnob",
    desc: "Spread any Ableton knob across your QWERTY white keys.",
  };
}

export const nohud = true;
export { boot, sim, paint, act, leave, meta };
