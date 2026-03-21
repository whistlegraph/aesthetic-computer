// kaos-pad-template
// Minimal multi-touch XY pad template.
// Hold each pointer down to play one sine voice.
// X = pitch, Y = volume.

const voices = new Map();
const maxPointers = 8;

const minHz = 80;
const maxHz = 1800;
const hudSafeTop = 22; // Reserve prompt HUD corner label row.
const hudSafeLeft = 74; // Keep local helper text out of HUD label zone.
const uiFont = "MatrixChunky8";

const palette = [
  [255, 96, 96],
  [96, 220, 255],
  [120, 255, 140],
  [255, 220, 96],
  [220, 120, 255],
  [255, 170, 120],
  [170, 255, 255],
  [255, 255, 160],
];

function clamp(value, low, high) {
  return Math.max(low, Math.min(high, value));
}

function lerp(a, b, t) {
  return a + (b - a) * t;
}

function mapXToFreq(x, width) {
  const w = Math.max(1, width - 1);
  const n = clamp((x ?? w / 2) / w, 0, 1);
  // Exponential map feels more musical than linear.
  return minHz * Math.pow(maxHz / minHz, n);
}

function mapYToVolume(y, top, height) {
  const yMin = top;
  const yMax = Math.max(top + 1, height - 1);
  const n = clamp(((y ?? yMin) - yMin) / (yMax - yMin), 0, 1);
  return 0.08 + (1 - n) * 0.82;
}

function getPointerXY(pointerIndex, e, pens, screen) {
  const pointer = pens?.(pointerIndex);
  const x = clamp(pointer?.x ?? e.x ?? screen.width / 2, 0, screen.width - 1);
  const y = clamp(pointer?.y ?? e.y ?? screen.height / 2, 0, screen.height - 1);
  return { x, y };
}

function startVoice(pointerIndex, xy, sound, screen) {
  const key = `pointer-${pointerIndex}`;
  const existing = voices.get(key);
  if (existing) return;

  const freq = mapXToFreq(xy.x, screen.width);
  const volume = mapYToVolume(xy.y, hudSafeTop, screen.height);
  const pan = clamp((xy.x / Math.max(1, screen.width - 1)) * 2 - 1, -1, 1);

  const synth = sound.synth({
    type: "sine",
    tone: freq,
    duration: "🔁",
    volume,
    pan,
    attack: 0.01,
    decay: 0.99,
  });

  voices.set(key, {
    pointerIndex,
    synth,
    x: xy.x,
    y: xy.y,
    freq,
    volume,
    targetFreq: freq,
    targetVolume: volume,
  });
}

function updateVoice(pointerIndex, xy, sound, screen) {
  const key = `pointer-${pointerIndex}`;
  const v = voices.get(key);
  if (!v) {
    startVoice(pointerIndex, xy, sound, screen);
    return;
  }

  v.x = xy.x;
  v.y = xy.y;
  v.targetFreq = mapXToFreq(xy.x, screen.width);
  v.targetVolume = mapYToVolume(xy.y, hudSafeTop, screen.height);
}

function stopVoice(pointerIndex, fade = 0.05) {
  const key = `pointer-${pointerIndex}`;
  const v = voices.get(key);
  if (!v) return;
  v.synth?.kill(fade);
  voices.delete(key);
}

function stopAllVoices(fade = 0.05) {
  for (const v of voices.values()) {
    v.synth?.kill(fade);
  }
  voices.clear();
}

function boot({ hud }) {
  hud.label("kaos");
}

function act({ event: e, sound, screen, pens }) {
  for (let i = 1; i <= maxPointers; i++) {
    if (e.is(`touch:${i}`)) {
      startVoice(i, getPointerXY(i, e, pens, screen), sound, screen);
    }

    if (e.is(`draw:${i}`)) {
      updateVoice(i, getPointerXY(i, e, pens, screen), sound, screen);
    }

    if (e.is(`lift:${i}`)) {
      stopVoice(i, 0.05);
    }
  }

  // Generic fallback for environments that emit un-indexed events.
  if (e.is("touch")) startVoice(1, getPointerXY(1, e, pens, screen), sound, screen);
  if (e.is("draw")) updateVoice(1, getPointerXY(1, e, pens, screen), sound, screen);
  if (e.is("lift")) stopVoice(1, 0.05);

  // Quick panic clear.
  if (e.is("keyboard:down:backspace")) stopAllVoices(0.02);
}

function sim() {
  for (const v of voices.values()) {
    v.freq = lerp(v.freq, v.targetFreq, 0.22);
    v.volume = lerp(v.volume, v.targetVolume, 0.22);
    v.synth?.update?.({
      tone: v.freq,
      volume: v.volume,
      duration: 0.03,
    });
  }
}

function paint({ wipe, ink, line, box, circle, write, screen }) {
  wipe(8, 10, 18);
  const padTop = hudSafeTop;
  const padBottom = screen.height - 1;

  // Pad grid.
  ink(24, 28, 40);
  for (let i = 1; i < 8; i++) {
    const x = (screen.width * i) / 8;
    const y = padTop + ((padBottom - padTop) * i) / 8;
    line(x, padTop, x, padBottom);
    line(0, y, screen.width - 1, y);
  }

  // Header text starts right of prompt HUD corner label.
  ink(230, 230, 240);
  write("multi sine pad", { x: hudSafeLeft, y: 4 }, undefined, undefined, false, uiFont);
  ink(160, 190, 220);
  write("hold pointers", { x: hudSafeLeft, y: 11 }, undefined, undefined, false, uiFont);
  write("x=pitch y=volume", { x: hudSafeLeft, y: 18 }, undefined, undefined, false, uiFont);

  let voiceCount = 0;
  for (const v of voices.values()) {
    voiceCount++;
    const color = palette[(v.pointerIndex - 1) % palette.length];
    const [r, g, b] = color;

    ink(r, g, b, 110);
    box(v.x - 12, v.y - 12, 24, 24);
    ink(r, g, b);
    circle(v.x, v.y, 8);

    const labelY = clamp(v.y - 8, padTop + 2, padBottom - 6);
    ink(245, 245, 255);
    write(
      `${v.pointerIndex}:${Math.round(v.freq)}hz`,
      { x: v.x + 10, y: labelY },
      undefined,
      undefined,
      false,
      uiFont
    );
  }

  ink(220, 240, 255);
  write(`${voiceCount} voices`, { x: 4, y: screen.height - 10 }, undefined, undefined, false, uiFont);
}

function leave() {
  stopAllVoices(0.04);
}

export { boot, act, sim, paint, leave };
