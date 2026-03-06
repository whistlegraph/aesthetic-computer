// notepat-native.mjs — Traditional notepat for ac-native bare metal

let sounds = {};
let trail = {};
let frame = 0;
let octave = 4;
let wave = "sine";
let waveIndex = 0;
let quickMode = false;
const wavetypes = ["sine", "triangle", "sawtooth", "square", "noise"];

// Metronome state
let metronomeEnabled = false;
let metronomeBPM = 180;
let metronomeBeatCount = 0;
let metronomeFlash = 0;
let metronomeVisualPhase = 0;

// Echo (room) mix — controlled by trackpad Y
let echoMix = 0;
let trackpadActivity = 0;

// Debug: last key pressed
let lastKey = "";
let lastKeyTimer = 0;

// Chromatic note order per octave
const CHROMATIC = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"];

// Keyboard mapping matching notepat.mjs NOTE_TO_KEYBOARD_KEY
const NOTE_TO_KEY = {
  "c": "c", "c#": "v", "d": "d", "d#": "s", "e": "e", "f": "f",
  "f#": "w", "g": "g", "g#": "r", "a": "a", "a#": "q", "b": "b",
  "+c": "h", "+c#": "t", "+d": "i", "+d#": "y", "+e": "j", "+f": "k",
  "+f#": "u", "+g": "l", "+g#": "o", "+a": "m", "+a#": "p", "+b": "n",
  "++c": ";", "++c#": "'", "++d": "]",
  "-a#": "z", "-b": "x",
};

const KEY_TO_NOTE = {};
for (const [note, key] of Object.entries(NOTE_TO_KEY)) {
  KEY_TO_NOTE[key] = note;
}

function parseNote(name) {
  if (name.startsWith("++")) return [name.slice(2), 2];
  if (name.startsWith("+")) return [name.slice(1), 1];
  if (name.startsWith("-")) return [name.slice(1), -1];
  return [name, 0];
}

const LEFT_GRID = [
  ["c", "c#", "d", "d#"],
  ["e", "f", "f#", "g"],
  ["g#", "a", "a#", "b"],
];
const RIGHT_GRID = [
  ["+c", "+c#", "+d", "+d#"],
  ["+e", "+f", "+f#", "+g"],
  ["+g#", "+a", "+a#", "+b"],
];

const NOTE_COLORS = {
  c: [220, 30, 30], "c#": [60, 60, 60],
  d: [220, 130, 0], "d#": [60, 60, 60],
  e: [200, 180, 0],
  f: [30, 160, 30], "f#": [60, 60, 60],
  g: [30, 90, 220], "g#": [60, 60, 60],
  a: [110, 30, 180], "a#": [60, 60, 60],
  b: [160, 50, 220],
};

function noteToFreq(note, oct) {
  const idx = CHROMATIC.indexOf(note);
  if (idx < 0) return 440;
  return 440 * Math.pow(2, (oct - 4) + (idx - 9) / 12);
}

function noteColor(n) { return NOTE_COLORS[n] || [80, 80, 80]; }

function boot({ wipe }) { wipe(255); }

function act({ event: e, sound }) {
  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (!key) return;

    // Debug: show every key
    lastKey = key;
    lastKeyTimer = 120; // show for 2 seconds

    if (key === "shift") { quickMode = !quickMode; return; }
    if (key === "tab") {
      waveIndex = (waveIndex + 1) % wavetypes.length;
      wave = wavetypes[waveIndex]; return;
    }
    if (key === "space") {
      metronomeEnabled = !metronomeEnabled;
      if (metronomeEnabled) {
        metronomeBeatCount = Math.floor(Date.now() / (60000 / metronomeBPM));
      }
      return;
    }
    if (key >= "1" && key <= "9") { octave = parseInt(key); return; }
    if (key === "arrowup") { octave = Math.min(9, octave + 1); return; }
    if (key === "arrowdown") { octave = Math.max(1, octave - 1); return; }
    if (key === "arrowleft") {
      waveIndex = (waveIndex - 1 + wavetypes.length) % wavetypes.length;
      wave = wavetypes[waveIndex]; return;
    }
    if (key === "arrowright") {
      waveIndex = (waveIndex + 1) % wavetypes.length;
      wave = wavetypes[waveIndex]; return;
    }
    if (key === "-") {
      metronomeBPM = Math.max(20, metronomeBPM - 5);
      metronomeBeatCount = Math.floor(Date.now() / (60000 / metronomeBPM));
      return;
    }
    if (key === "=") {
      metronomeBPM = Math.min(300, metronomeBPM + 5);
      metronomeBeatCount = Math.floor(Date.now() / (60000 / metronomeBPM));
      return;
    }

    const noteName = KEY_TO_NOTE[key];
    if (noteName && !sounds[key]) {
      const [letter, offset] = parseNote(noteName);
      const noteOctave = octave + offset;
      const freq = noteToFreq(letter, noteOctave);
      const semitones = (noteOctave - 4) * 12 + CHROMATIC.indexOf(letter);
      const pan = Math.max(-0.8, Math.min(0.8, (semitones - 12) / 15));

      sounds[key] = sound.synth({
        type: wave, tone: freq,
        duration: Infinity,
        volume: 0.6, attack: quickMode ? 0.002 : 0.005,
        decay: 0.1, pan: pan,
      });
      trail[key] = { note: letter, octave: noteOctave, brightness: 1.0 };
    }
  }

  if (e.is("keyboard:up")) {
    const key = e.key?.toLowerCase();
    if (!key) return;
    if (sounds[key]) {
      sound.kill(sounds[key], quickMode ? 0.02 : 0.08);
      delete sounds[key];
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, sound, system, trackpad }) {
  frame++;
  const activeCount = Object.keys(sounds).length;
  const w = screen.width;
  const h = screen.height;
  const CH = 8; // char width at size 1

  // Trackpad Y controls echo mix
  if (trackpad && (trackpad.dy !== 0 || trackpad.dx !== 0)) {
    if (trackpad.dy !== 0) {
      echoMix = Math.max(0, Math.min(1, echoMix - trackpad.dy * 0.003));
      sound?.room?.setMix?.(echoMix);
    }
    trackpadActivity = Math.min(1, trackpadActivity + 0.3);
  }
  if (trackpadActivity > 0) trackpadActivity *= 0.93;

  // Metronome tick
  if (metronomeEnabled && metronomeBPM > 0) {
    const now = Date.now();
    const msPerBeat = 60000 / metronomeBPM;
    const beatNumber = Math.floor(now / msPerBeat);
    if (beatNumber !== metronomeBeatCount) {
      metronomeBeatCount = beatNumber;
      metronomeVisualPhase = 1.0;
      metronomeFlash = 1.0;
      const isDownbeat = (beatNumber % 4) === 0;
      sound.synth({
        type: "square", tone: isDownbeat ? 1200 : 800,
        duration: 0.03, volume: isDownbeat ? 0.4 : 0.25,
        attack: 0.001, decay: 0.02, pan: 0,
      });
    }
    if (metronomeVisualPhase > 0) metronomeVisualPhase = Math.max(0, metronomeVisualPhase - 0.08);
    if (metronomeFlash > 0) metronomeFlash = Math.max(0, metronomeFlash - 0.15);
  }

  // Decay debug timer
  if (lastKeyTimer > 0) lastKeyTimer--;

  wipe(255);

  // Metronome flash border
  if (metronomeFlash > 0 && metronomeEnabled) {
    const fa = Math.floor(metronomeFlash * 120);
    const fw = Math.max(2, Math.floor(5 * metronomeFlash));
    const db = (metronomeBeatCount % 4) === 0;
    if (db) ink(255, 100, 100, fa); else ink(180, 200, 255, fa);
    box(0, 0, w, fw, true);
    box(0, h - fw, w, fw, true);
    box(0, 0, fw, h, true);
    box(w - fw, 0, fw, h, true);
  }

  // === TOP BAR (row 1: title left, battery right) ===
  const row1Y = 1;
  const row2Y = 10;
  const topBarH = 19;

  ink(245, 245, 245);
  box(0, 0, w, topBarH, true);
  ink(200, 200, 200);
  line(0, topBarH - 1, w, topBarH - 1);

  // Row 1 left: title
  ink(0, 0, 0, 200);
  write("notepat", { x: 2, y: row1Y, size: 1 });

  // Row 1 right: battery
  const bat = system?.battery;
  const batPct = bat?.percent ?? -1;
  if (batPct >= 0) {
    // Battery icon
    const batW = 14, batH = 7;
    const batX = w - batW - 6, batY = row1Y;
    ink(140, 140, 140);
    box(batX, batY, batW, batH, "outline");
    box(batX + batW, batY + 2, 2, 3, true);
    const fillW = Math.max(0, Math.floor(batPct * (batW - 2) / 100));
    if (batPct <= 20) ink(220, 30, 30);
    else if (batPct <= 50) ink(200, 160, 0);
    else ink(50, 160, 50);
    if (fillW > 0) box(batX + 1, batY + 1, fillW, batH - 2, true);

    // Percentage left of icon
    const pctStr = batPct + "%";
    ink(100, 100, 100);
    write(pctStr, { x: batX - pctStr.length * CH - 3, y: row1Y, size: 1 });
  }

  // Row 2 left: status
  const statusStr = activeCount > 0
    ? activeCount + " note" + (activeCount > 1 ? "s" : "")
    : "ready";
  ink(activeCount > 0 ? 0 : 170, activeCount > 0 ? 0 : 170, activeCount > 0 ? 0 : 170);
  write(statusStr, { x: 2, y: row2Y, size: 1 });

  // Row 2 right: wave, octave, mode, bpm
  const metroInfo = metronomeEnabled ? " " + metronomeBPM + "bpm" : "";
  const info = wave + " o:" + octave + (quickMode ? " QK" : "") + metroInfo;
  ink(100, 100, 100);
  write(info, { x: w - info.length * CH - 2, y: row2Y, size: 1 });

  // Volume bar (row 2, centered-right area, between status and info)
  const sysVol = sound?.speaker?.systemVolume ?? 100;
  const volBarW = 24, volBarH = 3;
  const infoStartX = w - info.length * CH - 2;
  const statusEndX = 2 + statusStr.length * CH;
  const volBarX = Math.floor((statusEndX + infoStartX) / 2 - volBarW / 2);
  ink(220, 220, 225); box(volBarX, row2Y + 2, volBarW, volBarH, true);
  const fillV = Math.floor(sysVol * volBarW / 100);
  if (fillV > 0) { ink(80, 80, 80); box(volBarX, row2Y + 2, fillV, volBarH, true); }

  // Waveform (overlaid on top bar when playing)
  const wf = sound?.speaker?.waveforms?.left;
  if (wf && activeCount > 0) {
    const wfX = 2 + 8 * CH, wfEnd = infoStartX - 4;
    const wfW = wfEnd - wfX;
    if (wfW > 20) {
      ink(120, 120, 120, 100);
      for (let i = 1; i < 80; i++) {
        const x0 = wfX + Math.floor((i - 1) * wfW / 80);
        const x1 = wfX + Math.floor(i * wfW / 80);
        const y0 = Math.round(row1Y + 3 - (wf[i - 1] || 0) * 3);
        const y1 = Math.round(row1Y + 3 - (wf[i] || 0) * 3);
        line(x0, y0, x1, y1);
      }
    }
  }

  // === SPLIT GRID: 4x3 left + 4x3 right ===
  const gridTop = topBarH + 2;
  const gridBottom = h - 8; // leave room for echo bar
  const gridH = gridBottom - gridTop;
  const gap = 1;
  const centerGap = 6;
  const cols = 4, rows = 3;

  const availW = (w - centerGap - 4) / 2;
  const btnW = Math.floor((availW - (cols - 1) * gap) / cols);
  const btnH = Math.floor((gridH - (rows - 1) * gap) / rows);

  const leftX = Math.floor((w / 2 - centerGap / 2) - cols * (btnW + gap) + gap);
  const rightX = Math.floor(w / 2 + centerGap / 2);

  function drawGrid(grid, startX, octOffset) {
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        const noteName = grid[r][c];
        const [letter, off] = parseNote(noteName);
        const noteOctave = octave + off + octOffset;
        const key = NOTE_TO_KEY[noteName];
        const isActive = key && sounds[key] !== undefined;
        const trailInfo = key && trail[key];
        const sharp = letter.includes("#");
        const nc = noteColor(letter);

        const x = startX + c * (btnW + gap);
        const y = gridTop + r * (btnH + gap);

        if (isActive) {
          ink(nc[0], nc[1], nc[2]);
          box(x, y, btnW, btnH, true);
          ink(255, 255, 255);
        } else if (trailInfo && trailInfo.brightness > 0.05) {
          const b = trailInfo.brightness;
          ink(Math.floor(255 - (255 - nc[0]) * b * 0.4),
              Math.floor(255 - (255 - nc[1]) * b * 0.4),
              Math.floor(255 - (255 - nc[2]) * b * 0.4));
          box(x, y, btnW, btnH, true);
          ink(40, 40, 40);
        } else {
          if (sharp) ink(215, 215, 220); else ink(245, 245, 245);
          box(x, y, btnW, btnH, true);
          ink(210, 210, 210);
          box(x, y, btnW, btnH, "outline");
          ink(sharp ? 140 : 60, sharp ? 140 : 60, sharp ? 140 : 60);
        }

        const label = key ? key.toUpperCase() : "";
        write(label, { x: x + 2, y: y + 2, size: 1 });

        if (btnH > 16) {
          if (isActive) ink(255, 255, 255, 180);
          else ink(sharp ? 160 : 110, sharp ? 160 : 110, sharp ? 160 : 110);
          write(letter + noteOctave, { x: x + 2, y: y + btnH - 10, size: 1 });
        }
      }
    }
  }

  drawGrid(LEFT_GRID, leftX, 0);
  drawGrid(RIGHT_GRID, rightX, 0);

  // Echo mix bar (bottom)
  {
    const barH = 3;
    const barY = h - barH - 1;
    ink(230, 230, 235);
    box(0, barY, w, barH, true);
    const fillW = Math.floor(echoMix * w);
    if (fillW > 0) {
      const pulse = trackpadActivity > 0.05 ? Math.floor(trackpadActivity * 40) : 0;
      ink(80 + pulse, 120 + pulse, 220, 200);
      box(0, barY, fillW, barH, true);
    }
    if (echoMix > 0.01) {
      ink(80, 80, 140, 160);
      const echoStr = "echo:" + Math.round(echoMix * 100) + "%";
      write(echoStr, { x: w - echoStr.length * CH - 2, y: barY - 9, size: 1 });
    }
  }

  // Debug: show last key pressed (temporary, remove once keys work)
  if (lastKeyTimer > 0 && lastKey) {
    ink(255, 0, 0, Math.min(200, lastKeyTimer * 4));
    write("key:" + lastKey, { x: 2, y: h - 10, size: 1 });
  }

  // Fade trails
  Object.keys(trail).forEach((key) => {
    if (!sounds[key]) {
      trail[key].brightness *= 0.92;
      if (trail[key].brightness < 0.02) delete trail[key];
    }
  });
}

export { boot, act, paint };

