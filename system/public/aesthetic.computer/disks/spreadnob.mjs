// Spreadnob, 2026.03.31
// AC-native UI for the Ableton spreadnob device.
// M4L → HTML bridge (acSn*) → bios send → disk → sound.spreadnob

import { getNoteColorWithOctave } from "../lib/note-colors.mjs";

const FONT = "YWFTProcessing-Regular";
const MINI_FONT = "MatrixChunky8";

const QWERTY_KEYS = ["A", "W", "S", "E", "D", "F", "T", "G", "Y", "H", "U", "J", "K", "O", "L"];
const BLACK_KEY_INDICES = new Set([1, 3, 6, 8, 10, 13]);
const PITCH_CLASSES = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"];

const DEFAULT_QWERTY_LOW = 48;
const DEFAULT_QWERTY_HIGH = 62;
const ARC_START = 214;
const ARC_END = -34;
const ARC_SWEEP = ARC_START - ARC_END;

let active = true;
let target = "";
let value = null;
let currentNote = null;
let rawNote = null;
let qwertyLow = DEFAULT_QWERTY_LOW;
let qwertyHigh = DEFAULT_QWERTY_HIGH;
let qwertyShift = 0;
let qwertyLocked = false;
let qwertyAmbiguous = false;
let paramMin = 0;
let paramMax = 1;
let flash = 0;
let noteHits = 0;

function clamp(n, lo, hi) { return Math.max(lo, Math.min(hi, n)); }
function deg2rad(d) { return (d * Math.PI) / 180; }

function noteLabel(midi) {
  if (!Number.isFinite(midi)) return "--";
  const pitch = PITCH_CLASSES[((midi % 12) + 12) % 12].toUpperCase();
  return `${pitch}${Math.floor(midi / 12) - 1}`;
}

function noteColor(midi) {
  if (!Number.isFinite(midi)) return [180, 180, 185];
  const pitch = PITCH_CLASSES[((midi % 12) + 12) % 12];
  return getNoteColorWithOctave(pitch, Math.floor(midi / 12) - 1);
}

function qwertyMidiForIndex(i) {
  return qwertyLow + i;
}

function qwertyIndex(note) {
  if (!Number.isFinite(note)) return -1;
  const idx = Math.round(note - qwertyLow);
  return idx >= 0 && idx < QWERTY_KEYS.length ? idx : -1;
}

function angleForProgress(t) {
  return ARC_START - clamp(t, 0, 1) * ARC_SWEEP;
}

function angleForValue(v) {
  if (!Number.isFinite(v) || paramMax === paramMin) return ARC_START;
  return angleForProgress((v - paramMin) / (paramMax - paramMin));
}

function arcXY(cx, cy, r, deg) {
  const rad = deg2rad(deg);
  return [cx + r * Math.cos(rad), cy - r * Math.sin(rad)];
}

function formatShift(shift) {
  if (!Number.isFinite(shift) || shift === 0) return "DEFAULT";
  return `${shift > 0 ? "+" : ""}${shift} OCT`;
}

function getLayout(w, h) {
  const pad = clamp(Math.round(Math.min(w, h) * 0.045), 6, 12);
  const headerH = clamp(Math.round(h * 0.18), 22, 30);
  const footerH = clamp(Math.round(h * 0.34), 48, 64);
  const mainY = headerH + 4;
  const mainH = Math.max(42, h - headerH - footerH - pad - 2);
  const leftW = Math.max(140, Math.round(w * 0.46));
  const rightX = pad + leftW + pad;
  const rightW = Math.max(92, w - rightX - pad);
  const valueH = clamp(Math.round(mainH * 0.42), 22, 34);
  const statusH = clamp(Math.round(mainH * 0.34), 18, 30);

  return {
    pad,
    headerH,
    footerH,
    mainY,
    mainH,
    leftW,
    rightX,
    rightW,
    dialCx: pad + Math.round(leftW * 0.5),
    dialCy: mainY + Math.round(mainH * 0.58),
    outerR: Math.max(28, Math.round(Math.min(leftW * 0.31, mainH * 0.56))),
    targetX: rightX,
    targetY: mainY,
    targetH: Math.max(20, mainH - valueH - statusH - 10),
    valueX: rightX,
    valueY: mainY + Math.max(20, mainH - valueH - statusH - 10) + 4,
    valueH,
    metaGap: 4,
    statusH,
    footerY: h - footerH,
  };
}

function handleNote(normalizedNote) {
  currentNote = Number.isFinite(normalizedNote) ? normalizedNote : null;
  flash = active ? 1 : 0.45;
  noteHits++;
}

function boot({ needsPaint }) {
  needsPaint();
}

function sim({ sound, needsPaint }) {
  let dirty = false;
  const daw = sound?.daw;
  let normalizedHandled = false;

  if (daw) {
    if (daw.snActive !== undefined && daw.snActive !== null) {
      active = !!Number(daw.snActive);
      daw.snActive = null;
      dirty = true;
    }
    if (daw.snTarget !== undefined && daw.snTarget !== null) {
      target = String(daw.snTarget).trim();
      daw.snTarget = null;
      dirty = true;
    }
    if (daw.snMin !== undefined && daw.snMin !== null) {
      const n = Number(daw.snMin);
      if (Number.isFinite(n)) paramMin = n;
      daw.snMin = null;
      dirty = true;
    }
    if (daw.snMax !== undefined && daw.snMax !== null) {
      const n = Number(daw.snMax);
      if (Number.isFinite(n)) paramMax = n;
      daw.snMax = null;
      dirty = true;
    }
    if (daw.snValue !== undefined && daw.snValue !== null) {
      const n = Number(daw.snValue);
      value = Number.isFinite(n) ? n : null;
      daw.snValue = null;
      dirty = true;
    }
    if (daw.snQwertyLow !== undefined && daw.snQwertyLow !== null) {
      const n = Number(daw.snQwertyLow);
      if (Number.isFinite(n)) qwertyLow = n;
      daw.snQwertyLow = null;
      dirty = true;
    }
    if (daw.snQwertyHigh !== undefined && daw.snQwertyHigh !== null) {
      const n = Number(daw.snQwertyHigh);
      if (Number.isFinite(n)) qwertyHigh = n;
      daw.snQwertyHigh = null;
      dirty = true;
    }
    if (
      daw.snRawNote !== undefined || daw.snNormalizedNote !== undefined ||
      daw.snShift !== undefined || daw.snLocked !== undefined || daw.snAmbiguous !== undefined
    ) {
      const raw = Number(daw.snRawNote);
      const normalized = Number(daw.snNormalizedNote);
      const shift = Number(daw.snShift);
      rawNote = Number.isFinite(raw) ? raw : rawNote;
      if (Number.isFinite(normalized)) {
        handleNote(normalized);
        normalizedHandled = true;
      }
      if (Number.isFinite(shift)) qwertyShift = shift;
      if (daw.snLocked !== undefined && daw.snLocked !== null) qwertyLocked = !!Number(daw.snLocked);
      if (daw.snAmbiguous !== undefined && daw.snAmbiguous !== null) qwertyAmbiguous = !!Number(daw.snAmbiguous);
      daw.snRawNote = null;
      daw.snNormalizedNote = null;
      daw.snShift = null;
      daw.snLocked = null;
      daw.snAmbiguous = null;
      dirty = true;
    }
    if (daw.snNote !== undefined && daw.snNote !== null) {
      const normalized = Number(daw.snNote);
      if (Number.isFinite(normalized) && !normalizedHandled) handleNote(normalized);
      daw.snNote = null;
      dirty = true;
    }
  }

  if (flash > 0) {
    flash *= 0.84;
    if (flash < 0.025) flash = 0;
    dirty = true;
  }

  if (dirty) needsPaint();
}

function paint({ wipe, ink, screen }) {
  const w = screen.width;
  const h = screen.height;
  const layout = getLayout(w, h);
  const qIndex = qwertyIndex(currentNote);
  const valAngle = angleForValue(value);
  const targetName = target || "click a knob";
  const actualLow = qwertyLow + qwertyShift * 12;
  const actualHigh = qwertyHigh + qwertyShift * 12;
  const octaveWindow = `${noteLabel(actualLow)}..${noteLabel(actualHigh)}`;
  const mappedText = currentNote === null ? "--" : noteLabel(currentNote);
  const rawText = rawNote === null ? "--" : noteLabel(rawNote);
  const valueText = value === null ? "--" : value.toFixed(3);
  const flashAlpha = Math.round(95 * flash);
  const statusText = rawNote === null
    ? "click a knob, then play qwerty"
    : !qwertyLocked && qwertyAmbiguous
      ? "octave unclear - tap E, F, G, A, or B to lock it in"
      : rawNote !== null && currentNote !== null && rawNote !== currentNote
        ? `${formatShift(qwertyShift)} normalized - any qwerty octave works now`
        : "qwerty octave locked and mapped";
  const statusTint = !qwertyLocked && qwertyAmbiguous
    ? [255, 214, 98]
    : rawNote !== null && currentNote !== null && rawNote !== currentNote
      ? [102, 255, 212]
      : [255, 118, 182];

  wipe(6, 8, 12);
  ink(14, 16, 22).box(0, 0, w, h);
  ink(255, 94, 164, 26).box(0, 0, w, layout.headerH);
  ink(70, 255, 220, 14).box(0, layout.headerH - 2, w, h - layout.headerH + 2);
  ink(255, 212, 96, 12).box(0, h - layout.footerH, w, layout.footerH);

  if (flashAlpha > 0) {
    ink(statusTint[0], statusTint[1], statusTint[2], flashAlpha).box(0, 0, w, h);
  }

  ink(255, 126, 188).box(layout.pad, layout.pad, w - layout.pad * 2, h - layout.pad * 2);
  ink(40, 20, 36).box(layout.pad + 1, layout.pad + 1, w - layout.pad * 2 - 2, h - layout.pad * 2 - 2);
  ink(255, 120, 184, 92).line(layout.pad + 4, layout.headerH, w - layout.pad - 4, layout.headerH);
  ink(102, 255, 212, 64).line(layout.pad + 4, h - layout.footerH - 2, w - layout.pad - 4, h - layout.footerH - 2);

  ink(255, 126, 188).write("spreadnob", { x: layout.pad + 6, y: layout.pad + 2 }, undefined, undefined, false, FONT);
  ink(114, 242, 224).write("aesthetic.computer", { x: layout.pad + 7, y: layout.pad + 12 }, undefined, undefined, false, MINI_FONT);

  const pillFill = active ? [92, 255, 136] : [255, 92, 112];
  const pillW = 46;
  const pillH = 14;
  ink(...pillFill).box(w - layout.pad - pillW - 4, layout.pad + 4, pillW, pillH);
  ink(8, 12, 10).write(active ? "ON" : "OFF", { x: w - layout.pad - pillW + 7, y: layout.pad + 6 }, undefined, undefined, false, MINI_FONT);

  ink(24, 16, 30).box(layout.pad + 8, layout.mainY + 2, layout.leftW - 14, layout.mainH - 4);
  ink(255, 118, 182, 36).box(layout.pad + 8, layout.mainY + 2, layout.leftW - 14, 1);

  const arcSteps = 64;
  const trackR = layout.outerR;
  for (let i = 0; i < arcSteps; i++) {
    const a1 = ARC_END + (i / arcSteps) * (ARC_START - ARC_END);
    const a2 = ARC_END + ((i + 1) / arcSteps) * (ARC_START - ARC_END);
    const [x1, y1] = arcXY(layout.dialCx, layout.dialCy, trackR, a1);
    const [x2, y2] = arcXY(layout.dialCx, layout.dialCy, trackR, a2);
    ink(56, 52, 70).line(Math.round(x1), Math.round(y1), Math.round(x2), Math.round(y2));
  }

  if (value !== null) {
    const sweepStart = Math.min(valAngle, ARC_START);
    const sweepEnd = Math.max(valAngle, ARC_START);
    const valueSteps = Math.max(4, Math.round(Math.abs(sweepEnd - sweepStart) / 5));
    for (let i = 0; i < valueSteps; i++) {
      const a1 = sweepStart + (i / valueSteps) * (sweepEnd - sweepStart);
      const a2 = sweepStart + ((i + 1) / valueSteps) * (sweepEnd - sweepStart);
      const [x1, y1] = arcXY(layout.dialCx, layout.dialCy, trackR, a1);
      const [x2, y2] = arcXY(layout.dialCx, layout.dialCy, trackR, a2);
      ink(active ? 94 : 255, active ? 255 : 124, active ? 210 : 176).line(Math.round(x1), Math.round(y1), Math.round(x2), Math.round(y2));
    }
    const [vx, vy] = arcXY(layout.dialCx, layout.dialCy, trackR, valAngle);
    ink(255, 250, 255).box(Math.round(vx) - 3, Math.round(vy) - 3, 7, 7);
  }

  for (let i = 0; i < QWERTY_KEYS.length; i++) {
    const angle = angleForProgress(i / (QWERTY_KEYS.length - 1));
    const [mx, my] = arcXY(layout.dialCx, layout.dialCy, layout.outerR + 8, angle);
    const rgb = noteColor(qwertyMidiForIndex(i));
    const hit = qIndex === i;
    ink(
      hit ? 255 : Math.round(rgb[0] * 0.85),
      hit ? 255 : Math.round(rgb[1] * 0.85),
      hit ? 255 : Math.round(rgb[2] * 0.85),
    ).box(Math.round(mx) - (hit ? 2 : 1), Math.round(my) - (hit ? 2 : 1), hit ? 5 : 3, hit ? 5 : 3);
  }

  ink(255, 188, 228).write(targetName, { x: layout.targetX + 4, y: layout.targetY + 3 }, undefined, layout.rightW - 8, true, FONT);
  ink(155, 150, 170).write("selected ableton knob", { x: layout.targetX + 4, y: layout.targetY + 13 }, undefined, layout.rightW - 8, true, MINI_FONT);

  ink(28, 16, 30).box(layout.valueX, layout.valueY, layout.rightW, layout.valueH);
  ink(255, 110, 178, 80).box(layout.valueX, layout.valueY, layout.rightW, 1);
  ink(active ? 102 : 255, active ? 255 : 130, active ? 212 : 170).write(
    valueText,
    { x: layout.valueX + 4, y: layout.valueY + 6 },
    undefined,
    layout.rightW - 8,
    true,
    FONT,
  );

  const chipY = layout.valueY + layout.valueH + layout.metaGap;
  const chipW = Math.floor((layout.rightW - 4) / 2);
  const chipH = 14;

  ink(28, 18, 38).box(layout.rightX, chipY, chipW, chipH);
  ink(28, 18, 38).box(layout.rightX + chipW + 4, chipY, chipW, chipH);
  ink(255, 208, 108).write(rawText === mappedText ? mappedText : `${rawText}->${mappedText}`, { x: layout.rightX + 2, y: chipY + 3 }, undefined, chipW - 4, true, MINI_FONT);
  ink(102, 255, 212).write(formatShift(qwertyShift), { x: layout.rightX + chipW + 6, y: chipY + 3 }, undefined, chipW - 8, true, MINI_FONT);

  const statusY = chipY + chipH + layout.metaGap;
  ink(20, 26, 28).box(layout.rightX, statusY, layout.rightW, layout.statusH);
  ink(statusTint[0], statusTint[1], statusTint[2], 88).box(layout.rightX, statusY, layout.rightW, 1);
  ink(...statusTint).write(statusText, { x: layout.rightX + 4, y: statusY + 4 }, undefined, layout.rightW - 8, true, MINI_FONT);

  const footerX = layout.pad + 8;
  const footerY = layout.footerY + 7;
  const footerW = w - layout.pad * 2 - 16;
  const footerH = layout.footerH - 14;
  const gap = 2;
  const keyW = Math.max(10, Math.floor((footerW - gap * (QWERTY_KEYS.length - 1)) / QWERTY_KEYS.length));
  const totalW = keyW * QWERTY_KEYS.length + gap * (QWERTY_KEYS.length - 1);
  const keyX0 = footerX + Math.round((footerW - totalW) / 2);

  ink(18, 18, 24).box(footerX, footerY, footerW, footerH);
  ink(255, 122, 186, 48).box(footerX, footerY, footerW, 1);
  ink(255, 212, 108).write(`QWERTY ${octaveWindow}`, { x: footerX + 4, y: footerY - 7 }, undefined, undefined, false, MINI_FONT);
  ink(114, 242, 224).write(`hits ${noteHits}`, { x: footerX + footerW - 48, y: footerY - 7 }, undefined, undefined, false, MINI_FONT);

  for (let i = 0; i < QWERTY_KEYS.length; i++) {
    const keyX = keyX0 + i * (keyW + gap);
    const black = BLACK_KEY_INDICES.has(i);
    const keyY = footerY + (black ? 3 : 0);
    const keyH = black ? footerH - 18 : footerH - 6;
    const hit = qIndex === i;
    const rgb = noteColor(qwertyMidiForIndex(i));
    const fill = hit
      ? [255, 112, 182]
      : black
        ? [Math.round(rgb[0] * 0.42), Math.round(rgb[1] * 0.42), Math.round(rgb[2] * 0.42)]
        : [Math.round(rgb[0] * 0.72), Math.round(rgb[1] * 0.72), Math.round(rgb[2] * 0.72)];

    ink(...fill).box(keyX, keyY, keyW, keyH);
    ink(255, hit ? 248 : 255, hit ? 252 : 255, hit ? 160 : 28).box(keyX, keyY, keyW, 1);
    ink(black ? 255 : 30, black ? 212 : 34, black ? 230 : 44).write(noteLabel(qwertyMidiForIndex(i)), { x: keyX + 1, y: keyY + 1 }, undefined, keyW - 2, true, MINI_FONT);
    ink(248, 244, 250).write(QWERTY_KEYS[i], { x: keyX + 1, y: keyY + keyH - 12 }, undefined, keyW - 2, true, FONT);

    if (hit) {
      ink(255, 246, 252).line(keyX, keyY + keyH, keyX + keyW, keyY + keyH);
    }
  }
}

function act({ needsPaint }) {
  needsPaint();
}

function leave() {}

function meta() {
  return {
    title: "Spreadnob",
    desc: "Spread any Ableton knob across your QWERTY keys.",
  };
}

export const nohud = true;
export { boot, sim, paint, act, leave, meta };
