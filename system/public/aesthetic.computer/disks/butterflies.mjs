// butterflies
// A simple 1-bit bitmap instrument:
// - Hold touches to spawn voices.
// - Each voice has a reader that scans the bitmap.
// - Touch positions steer readers and write to the bitmap.
// - Voice pitch/volume are derived from what readers sample.

const voices = new Map();
const maxPointers = 8;

const hudSafeTop = 22;
const hudSafeLeft = 74;
const uiFont = "MatrixChunky8";

const mapWidth = 64;
const mapHeight = 48;
const bits = new Uint8Array(mapWidth * mapHeight);
const nextBits = new Uint8Array(mapWidth * mapHeight);

const palette = [
  [255, 120, 110],
  [110, 225, 255],
  [145, 255, 160],
  [255, 220, 120],
  [220, 140, 255],
  [255, 170, 130],
  [170, 255, 255],
  [255, 255, 180],
];

const noteTable = [
  55.0, 65.41, 73.42, 82.41,
  98.0, 110.0, 130.81, 146.83,
  164.81, 196.0, 220.0, 261.63,
  293.66, 329.63, 392.0, 440.0,
];

let simFrame = 0;

function clamp(value, low, high) {
  return Math.max(low, Math.min(high, value));
}

function wrap(value, size) {
  if (size <= 0) return 0;
  return ((value % size) + size) % size;
}

function shortestWrapDelta(from, to, size) {
  let d = to - from;
  const half = size * 0.5;
  if (d > half) d -= size;
  if (d < -half) d += size;
  return d;
}

function mapIndex(x, y) {
  return wrap(y, mapHeight) * mapWidth + wrap(x, mapWidth);
}

function getBit(x, y) {
  return bits[mapIndex(x, y)];
}

function setBit(x, y, bit) {
  bits[mapIndex(x, y)] = bit ? 1 : 0;
}

function countNeighbors(x, y) {
  let count = 0;
  for (let dy = -1; dy <= 1; dy++) {
    for (let dx = -1; dx <= 1; dx++) {
      if (dx === 0 && dy === 0) continue;
      count += getBit(x + dx, y + dy);
    }
  }
  return count;
}

function gridToScreen(gx, gy, screen) {
  const width = Math.max(1, screen.width - 1);
  const mapTop = Math.min(hudSafeTop, Math.max(0, screen.height - 1));
  const mapHeightPx = Math.max(1, screen.height - 1 - mapTop);
  const x = Math.floor((wrap(gx, mapWidth) / Math.max(1, mapWidth - 1)) * width);
  const y = mapTop + Math.floor((wrap(gy, mapHeight) / Math.max(1, mapHeight - 1)) * mapHeightPx);
  return { x, y };
}

function screenToGrid(x, y, screen) {
  const mapTop = Math.min(hudSafeTop, Math.max(0, screen.height - 1));
  const width = Math.max(1, screen.width - 1);
  const mapHeightPx = Math.max(1, screen.height - 1 - mapTop);
  const sx = clamp(Math.round(x ?? width * 0.5), 0, width);
  const sy = clamp(Math.round(y ?? (mapTop + mapHeightPx * 0.5)), mapTop, screen.height - 1);
  const gx = Math.floor((sx / width) * (mapWidth - 1));
  const gy = Math.floor(((sy - mapTop) / mapHeightPx) * (mapHeight - 1));
  return { gx, gy };
}

function clearMap() {
  bits.fill(0);
}

function seedButterflyBits() {
  clearMap();
  const cx = (mapWidth - 1) * 0.5;
  for (let y = 0; y < mapHeight; y++) {
    for (let x = 0; x < Math.ceil(mapWidth * 0.5); x++) {
      const nx = x / Math.max(1, mapWidth - 1);
      const ny = y / Math.max(1, mapHeight - 1);
      const wave =
        Math.sin(nx * 11.7) +
        Math.cos(ny * 8.9) +
        Math.sin((nx + ny) * 15.4) * 0.7;
      const wingBit = wave > 1.15 ? 1 : 0;
      const mirrorX = Math.round(cx + (cx - x));
      setBit(x, y, wingBit);
      setBit(mirrorX, y, wingBit);
    }
  }

  // Vertical body in the center.
  for (let y = 6; y < mapHeight - 6; y++) {
    setBit(Math.floor(cx), y, 1);
    setBit(Math.ceil(cx), y, 1);
  }
}

function stepAutomata() {
  for (let y = 0; y < mapHeight; y++) {
    for (let x = 0; x < mapWidth; x++) {
      const i = y * mapWidth + x;
      const alive = bits[i];
      const neighbors = countNeighbors(x, y);
      let next = alive;

      if (alive) {
        next = neighbors === 2 || neighbors === 3 ? 1 : 0;
      } else {
        next = neighbors === 3 ? 1 : 0;
      }

      // Sparse low-rate perturbation to keep texture moving.
      if ((simFrame & 15) === 0 && ((x * 13 + y * 7 + simFrame) & 127) === 0) {
        next = next ? 0 : 1;
      }

      nextBits[i] = next;
    }
  }

  bits.set(nextBits);
}

function writeTouchIntoMap(voice, screen) {
  const pointerGrid = screenToGrid(voice.pointerX, voice.pointerY, screen);
  const radius = 1 + (voice.pointerIndex % 2);
  const yNorm =
    (voice.pointerY - hudSafeTop) /
    Math.max(1, screen.height - 1 - hudSafeTop);
  const writeBit = yNorm < 0.58 ? 1 : 0; // Top paints 1s, lower area erases.

  for (let dy = -radius; dy <= radius; dy++) {
    for (let dx = -radius; dx <= radius; dx++) {
      if (dx * dx + dy * dy > radius * radius) continue;
      setBit(pointerGrid.gx + dx, pointerGrid.gy + dy, writeBit);
    }
  }
}

function rebalanceVoiceVolumes() {
  const count = voices.size;
  if (count <= 0) return;
  const base = clamp(0.56 / Math.sqrt(count), 0.14, 0.34);
  for (const voice of voices.values()) {
    voice.baseVolume = base;
  }
}

function createVoice(pointerIndex, x, y, sound, screen) {
  const key = `pointer-${pointerIndex}`;
  if (voices.has(key)) return;

  const safeX = clamp(Math.round(x ?? screen.width * 0.5), 0, Math.max(0, screen.width - 1));
  const safeY = clamp(Math.round(y ?? screen.height * 0.5), 0, Math.max(0, screen.height - 1));
  const grid = screenToGrid(safeX, safeY, screen);
  const synth = sound.synth({
    type: "square",
    tone: 220,
    duration: "🔁",
    volume: 0.01,
    attack: 0.01,
    decay: 0.98,
    pan: clamp((safeX / Math.max(1, screen.width - 1)) * 2 - 1, -1, 1),
  });

  voices.set(key, {
    pointerIndex,
    pointerX: safeX,
    pointerY: safeY,
    readerX: grid.gx,
    readerY: grid.gy,
    readerVX: 0,
    readerVY: 0,
    bit: 0,
    density: 0,
    frequency: 220,
    targetFrequency: 220,
    volume: 0.01,
    targetVolume: 0.12,
    baseVolume: 0.2,
    synth,
    trail: [],
  });

  rebalanceVoiceVolumes();
}

function updateVoicePointer(pointerIndex, x, y, sound, screen) {
  const key = `pointer-${pointerIndex}`;
  const voice = voices.get(key);
  if (!voice) {
    createVoice(pointerIndex, x, y, sound, screen);
    return;
  }
  voice.pointerX = clamp(Math.round(x ?? screen.width * 0.5), 0, Math.max(0, screen.width - 1));
  voice.pointerY = clamp(Math.round(y ?? screen.height * 0.5), 0, Math.max(0, screen.height - 1));
}

function stopVoice(pointerIndex, fade = 0.05) {
  const key = `pointer-${pointerIndex}`;
  const voice = voices.get(key);
  if (!voice) return;
  voice.synth?.kill(fade);
  voices.delete(key);
  rebalanceVoiceVolumes();
}

function stopAllVoices(fade = 0.05) {
  for (const voice of voices.values()) {
    voice.synth?.kill(fade);
  }
  voices.clear();
}

function updateReaderForVoice(voice, screen) {
  writeTouchIntoMap(voice, screen);

  const pointerGrid = screenToGrid(voice.pointerX, voice.pointerY, screen);
  const steerX = shortestWrapDelta(voice.readerX, pointerGrid.gx, mapWidth);
  const steerY = shortestWrapDelta(voice.readerY, pointerGrid.gy, mapHeight);

  const pointerXNorm = clamp(
    voice.pointerX / Math.max(1, screen.width - 1),
    0,
    1
  );
  const pointerYNorm = clamp(
    (voice.pointerY - hudSafeTop) / Math.max(1, screen.height - 1 - hudSafeTop),
    0,
    1
  );

  const driftX = (pointerXNorm - 0.5) * 0.32;
  const driftY = (pointerYNorm - 0.5) * 0.32;

  voice.readerVX = clamp(voice.readerVX * 0.85 + steerX * 0.03 + driftX * 0.12, -1.2, 1.2);
  voice.readerVY = clamp(voice.readerVY * 0.85 + steerY * 0.03 + driftY * 0.12, -1.2, 1.2);
  voice.readerX = wrap(voice.readerX + voice.readerVX, mapWidth);
  voice.readerY = wrap(voice.readerY + voice.readerVY, mapHeight);

  const rx = Math.floor(voice.readerX);
  const ry = Math.floor(voice.readerY);
  const bit = getBit(rx, ry);
  const density = countNeighbors(rx, ry);
  const squareBits =
    (getBit(rx, ry) << 0) |
    (getBit(rx + 1, ry) << 1) |
    (getBit(rx, ry + 1) << 2) |
    (getBit(rx + 1, ry + 1) << 3);

  // Reader writes forward into the map, so touch history affects the future.
  const aheadX = Math.floor(voice.readerX + Math.sign(voice.readerVX));
  const aheadY = Math.floor(voice.readerY + Math.sign(voice.readerVY));
  if (bit) {
    setBit(aheadX, aheadY, 1);
  } else if (density <= 2) {
    setBit(aheadX, aheadY, 0);
  }

  const noteIndex =
    (squareBits + density + Math.floor(voice.readerX * 0.5) + voice.pointerIndex * 2) %
    noteTable.length;
  const octave = 0.5 + Math.floor((ry / Math.max(1, mapHeight - 1)) * 3) * 0.5;
  const targetFrequency = clamp(noteTable[noteIndex] * octave, 55, 1760);
  const gate = bit ? 1 : 0.06;
  const densityGain = 0.25 + (density / 8) * 0.95;
  const targetVolume = clamp(voice.baseVolume * gate * densityGain, 0, 0.42);

  voice.bit = bit;
  voice.density = density;
  voice.targetFrequency = targetFrequency;
  voice.targetVolume = targetVolume;
  voice.frequency += (voice.targetFrequency - voice.frequency) * 0.24;
  voice.volume += (voice.targetVolume - voice.volume) * 0.26;

  const pan = clamp(pointerXNorm * 2 - 1, -1, 1);
  voice.synth?.update?.({
    tone: voice.frequency,
    volume: voice.volume,
    pan,
    duration: 0.03,
  });

  const readerScreen = gridToScreen(voice.readerX, voice.readerY, screen);
  voice.trail.push(readerScreen);
  if (voice.trail.length > 14) voice.trail.shift();
}

function boot({ hud }) {
  hud.label("butterflies");
  seedButterflyBits();
}

function act({ event: e, sound, screen, pens }) {
  let touchHandled = false;
  let drawHandled = false;
  let liftHandled = false;

  for (let i = 1; i <= maxPointers; i++) {
    if (e.is(`touch:${i}`)) {
      touchHandled = true;
      const pointer = pens?.(i);
      const x = pointer?.x ?? e.x ?? screen.width * 0.5;
      const y = pointer?.y ?? e.y ?? screen.height * 0.5;
      createVoice(i, x, y, sound, screen);
    }

    if (e.is(`draw:${i}`)) {
      drawHandled = true;
      const pointer = pens?.(i);
      const x = pointer?.x ?? e.x ?? screen.width * 0.5;
      const y = pointer?.y ?? e.y ?? screen.height * 0.5;
      updateVoicePointer(i, x, y, sound, screen);
    }

    if (e.is(`lift:${i}`)) {
      liftHandled = true;
      stopVoice(i, 0.06);
    }
  }

  if (!touchHandled && e.is("touch")) {
    createVoice(1, e.x, e.y, sound, screen);
  }
  if (!drawHandled && e.is("draw")) {
    updateVoicePointer(1, e.x, e.y, sound, screen);
  }
  if (!liftHandled && e.is("lift")) {
    stopVoice(1, 0.06);
  }

  if (e.is("keyboard:down:backspace")) {
    seedButterflyBits();
  }
}

function sim({ sound, screen }) {
  sound.speaker?.poll();
  simFrame++;

  // Keep automata simple and readable.
  if ((simFrame & 1) === 0) {
    stepAutomata();
  }

  for (const voice of voices.values()) {
    updateReaderForVoice(voice, screen);
  }
}

function paint({ wipe, ink, line, circle, write, screen }) {
  wipe(8, 10, 16);

  const mapTop = Math.min(hudSafeTop, Math.max(0, screen.height - 1));
  const width = Math.max(1, screen.width - 1);
  const mapHeightPx = Math.max(1, screen.height - 1 - mapTop);

  // Render 1-bit map as a grayscale bitmap.
  for (let y = 0; y < mapHeightPx; y++) {
    const gy = Math.floor((y / Math.max(1, mapHeightPx - 1)) * (mapHeight - 1));
    const py = mapTop + y;
    const rowOffset = py * screen.width * 4;
    for (let x = 0; x <= width; x++) {
      const gx = Math.floor((x / width) * (mapWidth - 1));
      const alive = bits[gy * mapWidth + gx];
      const shade = alive ? 228 : 16;
      const pixel = rowOffset + x * 4;
      screen.pixels[pixel] = shade;
      screen.pixels[pixel + 1] = shade;
      screen.pixels[pixel + 2] = shade;
      screen.pixels[pixel + 3] = 255;
    }
  }

  ink(42, 52, 66);
  for (let i = 1; i < 8; i++) {
    const x = Math.floor((screen.width * i) / 8);
    line(x, mapTop, x, screen.height - 1);
  }
  for (let i = 1; i < 6; i++) {
    const y = mapTop + Math.floor((mapHeightPx * i) / 6);
    line(0, y, screen.width - 1, y);
  }

  // HUD-safe helper text.
  ink(230, 236, 248);
  write("butterflies", { x: hudSafeLeft, y: 4 }, undefined, undefined, false, uiFont);
  ink(170, 196, 224);
  write("1-bit reader map", { x: hudSafeLeft, y: 11 }, undefined, undefined, false, uiFont);
  write("hold touches", { x: hudSafeLeft, y: 18 }, undefined, undefined, false, uiFont);

  // Draw active pointers and their reader overlays.
  for (const voice of voices.values()) {
    const color = palette[(voice.pointerIndex - 1) % palette.length];
    const [r, g, b] = color;
    const reader = gridToScreen(voice.readerX, voice.readerY, screen);

    ink(r, g, b, 110);
    line(voice.pointerX, voice.pointerY, reader.x, reader.y);
    ink(r, g, b, 90);
    for (let i = 1; i < voice.trail.length; i++) {
      const a = voice.trail[i - 1];
      const c = voice.trail[i];
      line(a.x, a.y, c.x, c.y);
    }

    ink(r, g, b, 70);
    circle(voice.pointerX, voice.pointerY, 11);
    ink(r, g, b);
    circle(voice.pointerX, voice.pointerY, 7);

    ink(255, 255, 255, 120);
    circle(reader.x, reader.y, 8);
    ink(r, g, b);
    circle(reader.x, reader.y, 4);
    line(reader.x - 9, reader.y, reader.x + 9, reader.y);
    line(reader.x, reader.y - 9, reader.x, reader.y + 9);

    const labelX = clamp(Math.round(voice.pointerX + 10), 2, Math.max(2, screen.width - 80));
    const labelY = clamp(Math.round(voice.pointerY - 10), mapTop, Math.max(mapTop, screen.height - 8));
    ink(245, 245, 255);
    write(
      `${voice.pointerIndex}:${voice.bit ? 1 : 0}/${voice.density}`,
      { x: labelX, y: labelY },
      undefined,
      undefined,
      false,
      uiFont
    );
  }

  ink(220, 232, 248);
  write(
    "top writes 1 / lower erases",
    { x: 2, y: Math.max(mapTop + 2, screen.height - 16) },
    undefined,
    undefined,
    false,
    uiFont
  );
  write(
    `${voices.size} voices`,
    { x: 2, y: Math.max(mapTop + 10, screen.height - 8) },
    undefined,
    undefined,
    false,
    uiFont
  );
}

function leave() {
  stopAllVoices(0.04);
}

function meta() {
  return {
    title: "butterflies",
    desc: "Multi-touch 1-bit bitmap reader instrument.",
  };
}

export { boot, act, sim, paint, leave, meta };
