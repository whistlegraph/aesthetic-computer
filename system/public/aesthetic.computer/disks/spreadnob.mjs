// Spreadnob, 2026.03.31
// AC-native UI for the Ableton spreadnob device.

import { getNoteColorWithOctave } from "../lib/note-colors.mjs";

const FONT = "YWFTProcessing-Regular";
const MINI_FONT = "MatrixChunky8";
const KEY_LABELS = ["A", "W", "S", "E", "D", "F", "T", "G", "Y", "H", "U", "J", "K", "O", "L", "P", ";"];
const PITCH_CLASSES = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"];
const DEFAULT_LOW = 60;
const DEFAULT_HIGH = 76;
const MAX_SPAN = KEY_LABELS.length - 1;

let active = false;
let target = "";
let value = null;
let currentNote = null;
let low = DEFAULT_LOW;
let high = DEFAULT_HIGH;
let flash = 0;
let noteHits = 0;

let uiKit = null;
let requestPaint = () => {};
let messageHandler = null;
let layoutKey = "";

let lowDownBtn = null;
let lowUpBtn = null;
let resetBtn = null;
let highDownBtn = null;
let highUpBtn = null;

function clamp(n, min, max) {
  return Math.max(min, Math.min(max, n));
}

function noteLabel(midi) {
  if (!Number.isFinite(midi)) return "--";
  const pitch = PITCH_CLASSES[((midi % 12) + 12) % 12].toUpperCase();
  const octave = Math.floor(midi / 12) - 1;
  return `${pitch}${octave}`;
}

function noteColor(midi) {
  if (!Number.isFinite(midi)) return [190, 190, 190];
  const pitch = PITCH_CLASSES[((midi % 12) + 12) % 12];
  const octave = Math.floor(midi / 12) - 1;
  return getNoteColorWithOctave(pitch, octave);
}

function isBlack(midi) {
  return [1, 3, 6, 8, 10].includes(((midi % 12) + 12) % 12);
}

function isDefaultRange() {
  return low === DEFAULT_LOW && high === DEFAULT_HIGH;
}

function rangeCount() {
  return clamp(high - low + 1, 1, KEY_LABELS.length);
}

function noteRangeState(note = currentNote) {
  if (!Number.isFinite(note)) return "none";
  if (note < low) return "below";
  if (note > high) return "above";
  return "inside";
}

function getLayout(screen) {
  const compact = screen.height <= 194;
  return compact
    ? {
        compact,
        inset: 4,
        headerH: 22,
        titleX: 9,
        titleY: 8,
        pillW: 40,
        pillH: 12,
        targetY: 31,
        subY: 45,
        statY: 54,
        statH: 15,
        modeY: 73,
        keyboardY: 83,
        keyboardH: 34,
        statusBaseY: 124,
        warningY: 124,
        footerLabelY: screen.height - 28,
        footerY: screen.height - 16,
        btnY: screen.height - 14,
        btnW: 14,
        btnH: 11,
        resetW: 50,
      }
    : {
        compact,
        inset: 6,
        headerH: 28,
        titleX: 13,
        titleY: 10,
        pillW: 50,
        pillH: 14,
        targetY: 42,
        subY: 58,
        statY: 67,
        statH: 17,
        modeY: 92,
        keyboardY: 97,
        keyboardH: 46,
        statusBaseY: 152,
        warningY: 152,
        footerLabelY: screen.height - 28,
        footerY: screen.height - 18,
        btnY: screen.height - 16,
        btnW: 16,
        btnH: 13,
        resetW: 56,
      };
}

function setRange(nextLow, nextHigh, { emit = false } = {}) {
  let resolvedLow = clamp(Math.round(nextLow), 0, 127);
  let resolvedHigh = clamp(Math.round(nextHigh), 0, 127);

  if (resolvedHigh < resolvedLow) resolvedHigh = resolvedLow;

  if (resolvedHigh - resolvedLow > MAX_SPAN) {
    if (resolvedLow !== low) {
      resolvedHigh = resolvedLow + MAX_SPAN;
    } else {
      resolvedLow = resolvedHigh - MAX_SPAN;
    }
  }

  low = clamp(resolvedLow, 0, 127);
  high = clamp(resolvedHigh, low, 127);

  if (emit && typeof window !== "undefined" && window.max?.outlet) {
    try {
      window.max.outlet("range", low, high);
    } catch {}
  }

  requestPaint();
}

function updateFromMessage(data = {}) {
  if (data.type === "spreadnob:active") {
    active = !!Number(data.active);
    console.log("[SPREADNOB-UI/ACTIVE]", active ? 1 : 0);
    requestPaint();
    return;
  }

  if (data.type === "spreadnob:target") {
    target = String(data.name || "").trim();
    console.log("[SPREADNOB-UI/TARGET]", target || "(none)");
    requestPaint();
    return;
  }

  if (data.type === "spreadnob:value") {
    const num = Number(data.value);
    value = Number.isFinite(num) ? num : null;
    requestPaint();
    return;
  }

  if (data.type === "spreadnob:range") {
    setRange(Number(data.low), Number(data.high), { emit: false });
    return;
  }

  if (data.type === "spreadnob:note") {
    const num = Number(data.note);
    currentNote = Number.isFinite(num) ? num : null;
    noteHits++;
    flash = active ? 1 : 0.45;
    console.log("[SPREADNOB-UI/NOTE]", currentNote, "range", `${low}..${high}`);
    requestPaint();
  }
}

function buildUi(screen) {
  if (!uiKit) return;

  const key = `${screen.width}x${screen.height}`;
  if (layoutKey === key) return;
  layoutKey = key;

  const layout = getLayout(screen);
  const footerY = layout.footerY;
  const btnY = layout.btnY;
  const sideX = 8;
  const btnW = layout.btnW;
  const btnH = layout.btnH;
  const chipW = 48;
  const rightX = screen.width - sideX - btnW;
  const resetW = layout.resetW;
  const resetX = Math.round((screen.width - resetW) / 2);

  lowDownBtn = new uiKit.Button(sideX, btnY, btnW, btnH);
  lowUpBtn = new uiKit.Button(sideX + btnW + 2, btnY, btnW, btnH);
  resetBtn = new uiKit.Button(resetX, btnY, resetW, btnH);
  highDownBtn = new uiKit.Button(rightX - btnW - 2, btnY, btnW, btnH);
  highUpBtn = new uiKit.Button(rightX, btnY, btnW, btnH);

  lowDownBtn.labelBox = { x: sideX, y: footerY - 11, w: chipW, h: 10 };
  highUpBtn.labelBox = { x: screen.width - sideX - chipW, y: footerY - 11, w: chipW, h: 10 };
}

function boot({ ui, screen, needsPaint }) {
  uiKit = ui;
  requestPaint = needsPaint;
  buildUi(screen);

  if (typeof window !== "undefined") {
    messageHandler = (event) => updateFromMessage(event?.data || {});
    window.addEventListener("message", messageHandler);

    window.acSpreadnobSetActive = (next) =>
      updateFromMessage({ type: "spreadnob:active", active: next });
    window.acSpreadnobSetTarget = (name) =>
      updateFromMessage({ type: "spreadnob:target", name });
    window.acSpreadnobSetValue = (next) =>
      updateFromMessage({ type: "spreadnob:value", value: next });
    window.acSpreadnobSetRange = (nextLow, nextHigh) =>
      updateFromMessage({ type: "spreadnob:range", low: nextLow, high: nextHigh });
    window.acSpreadnobNote = (note) =>
      updateFromMessage({ type: "spreadnob:note", note });
  }

  requestPaint();
}

function sim({ needsPaint }) {
  if (flash <= 0) return;
  flash *= 0.82;
  if (flash < 0.025) flash = 0;
  needsPaint();
}

function paintButton({ button, label, ink, box, fg = [245, 240, 250], bg = [52, 26, 41] }) {
  button?.paint((btn) => {
    ink(...bg).box(btn.box);
    ink(255, 110, 180, 80).box(btn.box.x, btn.box.y, btn.box.w, 1);
    ink(...fg).write(label, { x: btn.box.x + 5, y: btn.box.y + 1 }, undefined, undefined, false, FONT);
  });
}

function paint({ wipe, ink, screen, line }) {
  buildUi(screen);

  const w = screen.width;
  const h = screen.height;
  const layout = getLayout(screen);
  const targetName = target || "CLICK ABLETON KNOB";
  const activeFill = active ? [110, 255, 120] : [255, 90, 95];
  const shell = active ? [19, 33, 20] : [36, 17, 24];
  const border = active ? [60, 180, 90] : [170, 50, 85];
  const flashAlpha = Math.round(110 * flash);
  const keyboardY = layout.keyboardY;
  const keyboardX = 10;
  const keyboardW = w - 20;
  const keyboardH = layout.keyboardH;
  const count = rangeCount();
  const noteState = noteRangeState();
  const gap = 2;
  const keyW = Math.max(8, Math.floor((keyboardW - gap * (KEY_LABELS.length - 1)) / KEY_LABELS.length));
  const totalKeyW = keyW * KEY_LABELS.length + gap * (KEY_LABELS.length - 1);
  const keyStartX = Math.round((w - totalKeyW) / 2);

  wipe(8, 10, 12);
  ink(...shell).box(0, 0, w, h);
  ink(255, 80, 150, 28).box(0, 0, w, layout.headerH);
  ink(40, 120, 80, active ? 44 : 18).box(0, layout.headerH - 4, w, h - (layout.headerH - 4));

  if (flashAlpha > 0) {
    ink(active ? 110 : 255, active ? 255 : 105, active ? 150 : 145, flashAlpha).box(0, 0, w, h);
  }

  ink(...border).box(layout.inset, layout.inset, w - layout.inset * 2, h - layout.inset * 2);
  ink(255, 115, 180, 120).line(8, layout.headerH + 6, w - 8, layout.headerH + 6);
  ink(255, 115, 180, 60).line(8, layout.modeY + 8, w - 8, layout.modeY + 8);
  ink(255, 115, 180, 60).line(8, layout.statusBaseY - 3, w - 8, layout.statusBaseY - 3);

  ink(255, 120, 185).write("spreadnob", { x: layout.titleX, y: layout.titleY }, undefined, undefined, false, FONT);

  ink(...activeFill).box(w - layout.pillW - 10, layout.titleY, layout.pillW, layout.pillH);
  ink(10, 18, 12).write(
    active ? "ON" : "OFF",
    { x: w - layout.pillW + 2 - 10, y: layout.titleY + 1 },
    undefined,
    undefined,
    false,
    FONT,
  );

  ink(255, 190, 220).write(targetName, { x: 10, y: layout.targetY }, undefined, w - 20, true, FONT);
  ink(170, 145, 170).write(
    active ? "follows selected knob" : "device bypassed by ableton",
    { x: 10, y: layout.subY },
    undefined,
    undefined,
    false,
    MINI_FONT,
  );

  const statY = layout.statY;
  const statW = Math.floor((w - 34) / 3);
  const statXs = [13, 13 + statW + 4, 13 + (statW + 4) * 2];
  const valueText = value === null ? "--" : value.toFixed(3);
  const noteText = currentNote === null ? "--" : `${noteLabel(currentNote)} ${currentNote}`;
  const rangeText = `${low}..${high}`;

  for (const x of statXs) {
    ink(24, 18, 28).box(x, statY, statW, layout.statH);
    ink(255, 255, 255, 24).box(x, statY, statW, 1);
  }

  ink(150, 135, 150).write("value", { x: statXs[0] + 3, y: statY + 2 }, undefined, undefined, false, MINI_FONT);
  ink(255, 118, 180).write(valueText, { x: statXs[0] + 3, y: statY + 8 }, undefined, statW - 6, true, FONT);

  ink(150, 135, 150).write("note", { x: statXs[1] + 3, y: statY + 2 }, undefined, undefined, false, MINI_FONT);
  ink(130, 230, 255).write(noteText, { x: statXs[1] + 3, y: statY + 8 }, undefined, statW - 6, true, FONT);

  ink(150, 135, 150).write("range", { x: statXs[2] + 3, y: statY + 2 }, undefined, undefined, false, MINI_FONT);
  ink(210, 245, 160).write(rangeText, { x: statXs[2] + 3, y: statY + 8 }, undefined, statW - 6, true, FONT);

  ink(255, 135, 200).write(
    isDefaultRange() ? "ableton qwerty spread" : "custom key spread",
    { x: 10, y: layout.modeY },
    undefined,
    undefined,
    false,
    MINI_FONT,
  );

  ink(14, 14, 18).box(keyboardX, keyboardY, keyboardW, keyboardH);

  for (let i = 0; i < KEY_LABELS.length; i++) {
    const x = keyStartX + i * (keyW + gap);
    const inRange = i < count;
    const midi = low + i;
    const current = currentNote === midi;
    const edgeCurrent =
      (noteState === "below" && i === 0) ||
      (noteState === "above" && i === count - 1);
    const black = inRange && isBlack(midi);
    const baseY = keyboardY + (black ? 4 : 0);
    const keyH = black ? keyboardH - 14 : keyboardH;
    const rgb = inRange ? noteColor(midi) : [42, 42, 46];
    const fill = current || edgeCurrent
      ? [255, 105, 182]
      : black
        ? [rgb[0] * 0.45, rgb[1] * 0.45, rgb[2] * 0.45]
        : [rgb[0] * 0.7, rgb[1] * 0.7, rgb[2] * 0.7];

    ink(...fill).box(x, baseY, keyW, keyH);
    ink(
      255,
      current || edgeCurrent ? 240 : 255,
      current || edgeCurrent ? 245 : 255,
      current || edgeCurrent ? 150 : 24,
    ).box(x, baseY, keyW, 1);

    if (inRange) {
      ink(250, 248, 252).write(KEY_LABELS[i], { x: x + 2, y: keyboardY + keyboardH - 11 }, undefined, undefined, false, MINI_FONT);
      ink(black ? 255 : 25, black ? 190 : 34, black ? 220 : 42).write(String(midi), { x: x + 1, y: baseY + 1 }, undefined, undefined, false, MINI_FONT);
      if (current || edgeCurrent) {
        ink(255, 245, 250).line(x, baseY + keyH, x + keyW, baseY + keyH);
      }
    }
  }

  const statusY = noteState !== "inside" && currentNote !== null
    ? layout.statusBaseY + 14
    : layout.statusBaseY;

  if (noteState !== "inside" && currentNote !== null) {
    const warning = noteState === "below"
      ? `${currentNote} below visible range`
      : `${currentNote} above visible range`;
    ink(255, 105, 182).box(12, layout.warningY, w - 24, 12);
    ink(20, 10, 18).write(warning, { x: 15, y: layout.warningY + 2 }, undefined, w - 30, true, MINI_FONT);
  }

  ink(160, 145, 165).write(
    currentNote === null
      ? "press a note to see the map move"
      : noteState === "inside"
        ? `${noteLabel(currentNote)} hit ${noteHits}`
        : `${noteLabel(currentNote)} hit ${noteHits} · shift range if needed`,
    { x: 12, y: statusY },
    undefined,
    w - 24,
    true,
    MINI_FONT,
  );

  ink(110, 230, 170).write(`${low} ${noteLabel(low)}`, { x: 8, y: layout.footerLabelY }, undefined, undefined, false, MINI_FONT);
  ink(110, 230, 170).write(`${high} ${noteLabel(high)}`, { right: 8, y: layout.footerLabelY }, undefined, undefined, false, MINI_FONT);

  paintButton({ button: lowDownBtn, label: "-", ink });
  paintButton({ button: lowUpBtn, label: "+", ink });
  paintButton({
    button: resetBtn,
    label: isDefaultRange() ? "QWERTY" : "RESET",
    ink,
    fg: isDefaultRange() ? [230, 235, 240] : [220, 255, 180],
    bg: isDefaultRange() ? [40, 45, 50] : [34, 56, 30],
  });
  paintButton({ button: highDownBtn, label: "-", ink });
  paintButton({ button: highUpBtn, label: "+", ink });
}

function act({ event, screen, needsPaint }) {
  buildUi(screen);

  lowDownBtn?.act(event, () => setRange(low - 1, high, { emit: true }));
  lowUpBtn?.act(event, () => setRange(low + 1, high, { emit: true }));
  highDownBtn?.act(event, () => setRange(low, high - 1, { emit: true }));
  highUpBtn?.act(event, () => setRange(low, high + 1, { emit: true }));
  resetBtn?.act(event, () => setRange(DEFAULT_LOW, DEFAULT_HIGH, { emit: true }));

  if (event.is("keyboard:down:left")) setRange(low - 1, high, { emit: true });
  if (event.is("keyboard:down:right")) setRange(low, high + 1, { emit: true });
  if (event.is("keyboard:down:down")) setRange(low + 1, high, { emit: true });
  if (event.is("keyboard:down:up")) setRange(low, high - 1, { emit: true });
  if (event.is("keyboard:down:r")) setRange(DEFAULT_LOW, DEFAULT_HIGH, { emit: true });

  needsPaint();
}

function leave() {
  if (typeof window !== "undefined") {
    if (messageHandler) window.removeEventListener("message", messageHandler);
    delete window.acSpreadnobSetActive;
    delete window.acSpreadnobSetTarget;
    delete window.acSpreadnobSetValue;
    delete window.acSpreadnobSetRange;
    delete window.acSpreadnobNote;
  }
}

function meta() {
  return {
    title: "Spreadnob",
    desc: "AC-native control surface for the Ableton spreadnob device.",
  };
}

export const nohud = true;
export { boot, sim, paint, act, leave, meta };
