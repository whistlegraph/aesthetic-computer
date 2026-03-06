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

// Echo (room) mix — controlled by mouse slider under status bar
let echoMix = 0;
let echoDragging = false;

// Dark mode: auto based on LA time (7pm-7am)
// UEFI clock is UTC; LA is UTC-7 (PDT) or UTC-8 (PST)
// DST: second Sunday of March 2am → first Sunday of November 2am
function getLAOffset() {
  const now = new Date();
  const y = now.getUTCFullYear(), m = now.getUTCMonth();
  if (m > 2 && m < 10) return 7; // Apr-Oct: always PDT
  if (m < 2 || m > 10) return 8; // Jan-Feb, Dec: always PST
  // March (m=2): DST starts second Sunday at 2am UTC-8 (10am UTC)
  if (m === 2) {
    const d1 = new Date(Date.UTC(y, 2, 1)).getUTCDay(); // day-of-week of Mar 1
    const secondSun = 8 + (7 - d1) % 7; // date of second Sunday
    const dstStart = Date.UTC(y, 2, secondSun, 10); // 2am PST = 10am UTC
    return now.getTime() >= dstStart ? 7 : 8;
  }
  // November (m=10): DST ends first Sunday at 2am UTC-7 (9am UTC)
  const d1 = new Date(Date.UTC(y, 10, 1)).getUTCDay();
  const firstSun = 1 + (7 - d1) % 7;
  const dstEnd = Date.UTC(y, 10, firstSun, 9); // 2am PDT = 9am UTC
  return now.getTime() < dstEnd ? 7 : 8;
}
function getLAHour() {
  const now = new Date();
  return (now.getUTCHours() - getLAOffset() + 24) % 24;
}
function isDark() {
  const h = getLAHour();
  return h >= 19 || h < 7;
}
let dark = isDark();

// Background color — average of active notes, lerped
let bgColor = dark ? [20, 20, 25] : [255, 255, 255];
let bgTarget = dark ? [20, 20, 25] : [255, 255, 255];

// Debug: last key pressed
let lastKey = "";
let lastKeyTimer = 0;

// WiFi UI state
let wifiPanelOpen = false;
let wifiSelectedIdx = -1;
let wifiPassword = "";
let wifiPasswordMode = false;  // true = capturing keyboard for password
// Touch-note state (for clickable grid buttons)
let touchNotes = {};  // pointer id -> { key, note, octave }

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

// Hit-test a touch point against the note grid
function hitTestGrid(x, y, gi) {
  const grids = [
    { grid: LEFT_GRID, startX: gi.leftX, octOffset: 0 },
    { grid: RIGHT_GRID, startX: gi.rightX, octOffset: 0 },
  ];
  for (const { grid, startX, octOffset } of grids) {
    for (let r = 0; r < 3; r++) {
      for (let c = 0; c < 4; c++) {
        const bx = startX + c * (gi.btnW + gi.gap);
        const by = gi.gridTop + r * (gi.btnH + gi.gap);
        if (x >= bx && x < bx + gi.btnW && y >= by && y < by + gi.btnH) {
          const noteName = grid[r][c];
          const key = NOTE_TO_KEY[noteName];
          const [letter, off] = parseNote(noteName);
          return { key, letter, octave: octave + off + octOffset };
        }
      }
    }
  }
  return null;
}

function boot({ wipe }) { wipe(255); }

function act({ event: e, sound, wifi }) {
  // WiFi password input mode — capture keyboard
  if (wifiPasswordMode && e.is("keyboard:down")) {
    const key = e.key;
    if (key === "Escape") { wifiPasswordMode = false; wifiPassword = ""; return; }
    if (key === "Enter" || key === "Return") {
      if (wifi && wifiSelectedIdx >= 0) {
        const nets = wifi.networks || [];
        if (nets[wifiSelectedIdx]) {
          wifi.connect(nets[wifiSelectedIdx].ssid, wifiPassword);
        }
      }
      wifiPasswordMode = false;
      wifiPanelOpen = false;
      return;
    }
    if (key === "Backspace") { wifiPassword = wifiPassword.slice(0, -1); return; }
    if (key.length === 1) { wifiPassword += key; return; }
    return;
  }

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

      // Start at current pressure (or full for digital keys)
      const velocity = e.pressure > 0 ? e.pressure : 1.0;
      const vol = 0.15 + velocity * 0.55;
      const synth = sound.synth({
        type: wave, tone: freq,
        duration: Infinity,
        volume: vol, attack: quickMode ? 0.002 : 0.005,
        decay: 0.1, pan: pan,
      });
      sounds[key] = { synth, note: letter, octave: noteOctave };
      trail[key] = { note: letter, octave: noteOctave, brightness: velocity };
    }
  }

  if (e.is("keyboard:up")) {
    const key = e.key?.toLowerCase();
    if (!key) return;
    if (sounds[key]) {
      const s = sounds[key].synth || sounds[key];
      sound.kill(s, quickMode ? 0.02 : 0.08);
      delete sounds[key];
    }
  }

  // Touch interactions
  if (e.is("touch")) {
    const y = e.pointer?.y ?? e.y ?? 0;
    const x = e.pointer?.x ?? e.x ?? 0;
    const w = globalThis.__screenW || 320;
    const h = globalThis.__screenH || 200;
    const pid = e.pointer?.id ?? 0;

    // WiFi antenna icon: top-right corner (within status bar)
    if (y < 16 && x > w - 22) {
      wifiPanelOpen = !wifiPanelOpen;
      if (wifiPanelOpen && wifi) {
        wifi.scan();  // Always rescan when opening
      }
      return;
    }

    // WiFi panel clicks
    if (wifiPanelOpen) {
      const panelX = w - 140, panelY = 12, panelW = 138, panelH = 120;
      if (x >= panelX && x < panelX + panelW && y >= panelY && y < panelY + panelH) {
        const nets = wifi?.networks || [];
        const rowH = 12;
        const listY = panelY + 14;
        const clickedRow = Math.floor((y - listY) / rowH);
        if (clickedRow >= 0 && clickedRow < nets.length) {
          wifiSelectedIdx = clickedRow;
          if (nets[clickedRow].encrypted) {
            wifiPassword = "";
            wifiPasswordMode = true;
          } else {
            wifi?.connect(nets[clickedRow].ssid, "");
            wifiPanelOpen = false;
          }
        }
        return;
      }
      wifiPanelOpen = false;
      wifiPasswordMode = false;
      return;
    }

    // Echo slider zone (below settings row, y 26-36)
    if (y >= 26 && y < 36) {
      echoDragging = true;
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
      return;
    }

    // Grid button tap — check if touch lands on a note button
    const gridInfo = globalThis.__gridInfo;
    if (gridInfo) {
      const hitNote = hitTestGrid(x, y, gridInfo);
      if (hitNote && !sounds[hitNote.key]) {
        const freq = noteToFreq(hitNote.letter, hitNote.octave);
        const semitones = (hitNote.octave - 4) * 12 + CHROMATIC.indexOf(hitNote.letter);
        const pan = Math.max(-0.8, Math.min(0.8, (semitones - 12) / 15));
        const synth = sound.synth({
          type: wave, tone: freq, duration: Infinity,
          volume: 0.5, attack: 0.005, decay: 0.1, pan,
        });
        sounds[hitNote.key] = { synth, note: hitNote.letter, octave: hitNote.octave };
        trail[hitNote.key] = { note: hitNote.letter, octave: hitNote.octave, brightness: 1.0 };
        touchNotes[pid] = { key: hitNote.key };
        return;
      }
    }
  }
  if (e.is("draw")) {
    const y = e.pointer?.y ?? e.y ?? 0;
    const x = e.pointer?.x ?? e.x ?? 0;
    const w = globalThis.__screenW || 320;
    if (echoDragging) {
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
    }
  }
  if (e.is("lift")) {
    const pid = e.pointer?.id ?? 0;
    if (echoDragging) echoDragging = false;
    // Release touch-triggered note
    if (touchNotes[pid]) {
      const key = touchNotes[pid].key;
      if (sounds[key]) {
        const s = sounds[key].synth || sounds[key];
        sound.kill(s, 0.08);
        delete sounds[key];
      }
      delete touchNotes[pid];
    }
  }
}

function paint({ wipe, ink, box, line, write, screen, sound, system, trackpad, pressures, wifi }) {
  frame++;
  const activeCount = Object.keys(sounds).length;
  const w = screen.width;
  const h = screen.height;
  const CH = 6; // char width at size 1 (font_1 = 6x10)
  globalThis.__screenW = w; // expose for act()
  globalThis.__screenH = h;

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

  // Re-check dark mode every ~10 seconds
  if (frame % 600 === 0) dark = isDark();

  // Hour-based accent tint (subtle color shift through the day)
  const laH = getLAHour();
  // Map hour to hue: dawn=warm orange, midday=cool blue, sunset=pink, night=deep blue
  let tR, tG, tB;
  if (laH >= 5 && laH < 9) { tR = 80; tG = 40; tB = 10; }       // dawn: warm amber
  else if (laH >= 9 && laH < 14) { tR = 10; tG = 35; tB = 70; }  // midday: cool blue
  else if (laH >= 14 && laH < 18) { tR = 25; tG = 60; tB = 20; } // afternoon: green
  else if (laH >= 18 && laH < 21) { tR = 70; tG = 15; tB = 55; } // sunset: magenta
  else { tR = 20; tG = 15; tB = 65; }                              // night: deep indigo

  const f = Math.floor; // shorthand
  // Full saturation tint
  const BG = dark
    ? [f(10 + tR), f(10 + tG), f(12 + tB)]
    : [f(Math.min(255, 230 + tR / 3)), f(Math.min(255, 230 + tG / 3)), f(Math.min(255, 230 + tB / 3))];
  const FG = dark ? 220 : 0;
  const FG_DIM = dark ? 140 : 100;
  const FG_MUTED = dark ? 80 : 170;
  // Pink-tinted status bar
  const BAR_BG = dark
    ? [f(45 + tR / 3), f(25 + tG / 6), f(35 + tB / 4)]
    : [255, f(230 + tG / 8), f(235 + tB / 6)];
  const BAR_BORDER = dark ? [f(70 + tR / 3), f(40 + tG / 4), f(55 + tB / 3)] : [230, 190, 200];
  const PAD_NORMAL = dark
    ? [f(35 + tR / 3), f(35 + tG / 3), f(38 + tB / 3)]
    : [f(Math.min(255, 242 + tR / 8)), f(Math.min(255, 242 + tG / 8)), f(Math.min(255, 242 + tB / 8))];
  const PAD_SHARP = dark
    ? [f(25 + tR / 4), f(25 + tG / 4), f(28 + tB / 4)]
    : [f(Math.min(255, 212 + tR / 8)), f(Math.min(255, 212 + tG / 8)), f(Math.min(255, 215 + tB / 8))];
  const PAD_OUTLINE = dark ? [55 + f(tR / 4), 55 + f(tG / 4), 60 + f(tB / 4)] : [210, 210, 210];

  // Compute background color from active notes
  const activeKeys = Object.keys(sounds);
  if (activeKeys.length > 0) {
    let r = 0, g = 0, b = 0;
    for (const key of activeKeys) {
      const noteName = KEY_TO_NOTE[key];
      if (noteName) {
        const [letter] = parseNote(noteName);
        const nc = noteColor(letter);
        r += nc[0]; g += nc[1]; b += nc[2];
      }
    }
    const n = activeKeys.length;
    if (dark) {
      // Dark: full saturation blend toward note color
      bgTarget = [
        Math.floor(15 + (r / n) * 0.5),
        Math.floor(15 + (g / n) * 0.5),
        Math.floor(18 + (b / n) * 0.5),
      ];
    } else {
      // Light: stronger note color blend
      bgTarget = [
        Math.floor(255 - (255 - r / n) * 0.6),
        Math.floor(255 - (255 - g / n) * 0.6),
        Math.floor(255 - (255 - b / n) * 0.6),
      ];
    }
  } else {
    bgTarget = BG;
  }
  // Lerp toward target
  bgColor[0] += (bgTarget[0] - bgColor[0]) * 0.15;
  bgColor[1] += (bgTarget[1] - bgColor[1]) * 0.15;
  bgColor[2] += (bgTarget[2] - bgColor[2]) * 0.15;

  wipe(Math.round(bgColor[0]), Math.round(bgColor[1]), Math.round(bgColor[2]));

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

  // === STATUS BAR ===
  const topBarH = 14;
  const barY = 2;

  ink(BAR_BG[0], BAR_BG[1], BAR_BG[2]);
  box(0, 0, w, topBarH, true);
  ink(BAR_BORDER[0], BAR_BORDER[1], BAR_BORDER[2]);
  line(0, topBarH - 1, w, topBarH - 1);

  // Left: "notepat" in matrix font, ".com" in pink/accent color
  ink(FG, FG, FG, 200);
  write("notepat", { x: 2, y: barY, size: 1, font: "matrix" });
  const dotComX = 2 + 7 * 8; // 7 chars * 8px matrix width
  ink(dark ? 200 : 180, dark ? 100 : 60, dark ? 140 : 120);
  write(".com", { x: dotComX, y: barY, size: 1, font: "matrix" });
  const statusStr = activeCount > 0
    ? activeCount + " note" + (activeCount > 1 ? "s" : "")
    : "";
  if (statusStr) {
    ink(FG_DIM, FG_DIM, FG_DIM);
    write(statusStr, { x: dotComX + 4 * 8 + 4, y: barY, size: 1 });
  }

  // Center status bar: note count is enough, settings go below
  const centerX = Math.floor(w / 2);

  // Right section: wifi | battery | time | vol
  let rx = w - 2; // right edge cursor (builds right to left)

  // WiFi antenna icon (clickable)
  {
    const ax = w - 12, ay = 3;
    const wifiConnected = wifi?.connected;
    const wifiState = wifi?.state ?? 0;
    // Draw antenna: three arcs + base dot
    if (wifiConnected) ink(80, 200, 80); // green when connected
    else if (wifiState >= 2) ink(200, 200, 80); // yellow when scanning/connecting
    else ink(dark ? 80 : 160, dark ? 80 : 160, dark ? 90 : 170); // gray
    // Simple antenna icon: vertical line + signal bars
    line(ax + 5, ay + 1, ax + 5, ay + 7); // vertical
    line(ax + 3, ay + 5, ax + 5, ay + 3); // left arm
    line(ax + 7, ay + 5, ax + 5, ay + 3); // right arm
    if (wifiConnected || wifiState >= 2) {
      box(ax + 1, ay + 2, 2, 1, true); // left bar
      box(ax + 8, ay + 2, 2, 1, true); // right bar
    }
    rx -= 14;
  }

  // Battery icon + percentage
  const bat = system?.battery;
  const batPct = bat?.percent ?? -1;
  if (batPct >= 0) {
    const batW = 12, batH = 6;
    rx -= batW + 2;
    const batX = rx;
    ink(dark ? 90 : 150, dark ? 90 : 150, dark ? 90 : 150);
    box(batX, barY, batW, batH, "outline");
    box(batX + batW, barY + 2, 2, 2, true);
    const fillW = Math.max(0, Math.floor(batPct * (batW - 2) / 100));
    if (batPct <= 20) ink(220, 30, 30);
    else if (batPct <= 50) ink(200, 160, 0);
    else ink(50, 160, 50);
    if (fillW > 0) box(batX + 1, barY + 1, fillW, batH - 2, true);
    const pctStr = batPct + "%";
    rx -= pctStr.length * CH + 2;
    ink(FG_DIM, FG_DIM, FG_DIM);
    write(pctStr, { x: rx, y: barY, size: 1 });
    rx -= 4; // separator space
  }

  // Time (LA)
  {
    const now = new Date();
    const laMs = now.getTime() - getLAOffset() * 3600000;
    const laDate = new Date(laMs);
    const hh = laDate.getUTCHours();
    const mm = laDate.getUTCMinutes();
    const ss = laDate.getUTCSeconds();
    const ampm = hh >= 12 ? "p" : "a";
    const h12 = hh % 12 || 12;
    const timeStr = h12 + ":" + (mm < 10 ? "0" : "") + mm + ":" + (ss < 10 ? "0" : "") + ss + ampm;
    rx -= timeStr.length * CH;
    ink(FG_DIM, FG_DIM, FG_DIM);
    write(timeStr, { x: rx, y: barY, size: 1 });
    rx -= 4;
  }

  // Volume bar
  const sysVol = sound?.speaker?.systemVolume ?? 100;
  {
    const volW = 20, volH = 3;
    rx -= volW;
    ink(dark ? 45 : 220, dark ? 45 : 220, dark ? 50 : 225);
    box(rx, barY + 2, volW, volH, true);
    const fillV = Math.floor(sysVol * volW / 100);
    if (fillV > 0) { ink(dark ? 150 : 80, dark ? 150 : 80, dark ? 150 : 80); box(rx, barY + 2, fillV, volH, true); }
    rx -= 2;
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    rx -= 3 * CH;
    write("vol", { x: rx, y: barY, size: 1 });
  }

  // Brightness bar
  const sysBrt = system?.brightness ?? -1;
  if (sysBrt >= 0) {
    rx -= 4;
    const brtW = 16, brtH = 3;
    rx -= brtW;
    ink(dark ? 45 : 220, dark ? 45 : 220, dark ? 50 : 225);
    box(rx, barY + 2, brtW, brtH, true);
    const fillB = Math.floor(sysBrt * brtW / 100);
    if (fillB > 0) { ink(dark ? 180 : 60, dark ? 160 : 60, dark ? 80 : 30); box(rx, barY + 2, fillB, brtH, true); }
    rx -= 2;
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    rx -= 3 * CH;
    write("brt", { x: rx, y: barY, size: 1 });
  }

  // Waveform (between title and right section, overlaid when playing)
  const wf = sound?.speaker?.waveforms?.left;
  if (wf && activeCount > 0) {
    const wfX = dotComX + 4 * 8 + 4 + (statusStr ? (statusStr.length * CH + 4) : 0);
    const wfEnd = rx - 4;
    const wfW = wfEnd - wfX;
    if (wfW > 20) {
      ink(dark ? 80 : 140, dark ? 80 : 140, dark ? 80 : 140, 120);
      for (let i = 1; i < 60; i++) {
        const x0 = wfX + Math.floor((i - 1) * wfW / 60);
        const x1 = wfX + Math.floor(i * wfW / 60);
        const y0 = Math.round(barY + 3 - (wf[i - 1] || 0) * 3);
        const y1 = Math.round(barY + 3 - (wf[i] || 0) * 3);
        line(x0, y0, x1, y1);
      }
    }
  }

  // === SPLIT GRID: 4x3 left (bottom-left) + 4x3 right (bottom-right) ===
  const gap = 0;  // marginless — buttons touch for rollover clicking
  const cols = 4, rows = 3;
  const margin = 2; // minimal edge margin

  // Small buttons: ~30% of screen height, anchored to bottom corners
  const maxGridH = Math.floor(h * 0.30);
  const btnH = Math.floor(maxGridH / rows);
  const maxBtnW = Math.floor((w / 2 - margin * 2) / cols);
  const btnW = Math.min(maxBtnW, btnH * 2);

  const gridW = cols * btnW;
  const gridH = rows * btnH;
  const gridTop = h - gridH - margin - 8; // anchor near bottom, leave room for echo bar

  const leftX = margin;
  const rightX = w - gridW - margin;

  // Expose grid layout for touch hit-testing in act()
  globalThis.__gridInfo = { leftX, rightX, gridTop, btnW, btnH, gap };

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
          if (dark) {
            ink(Math.floor(nc[0] * b * 0.4),
                Math.floor(nc[1] * b * 0.4),
                Math.floor(nc[2] * b * 0.4));
          } else {
            ink(Math.floor(255 - (255 - nc[0]) * b * 0.4),
                Math.floor(255 - (255 - nc[1]) * b * 0.4),
                Math.floor(255 - (255 - nc[2]) * b * 0.4));
          }
          box(x, y, btnW, btnH, true);
          ink(dark ? 200 : 40, dark ? 200 : 40, dark ? 200 : 40);
        } else {
          // Idle: saturated note color
          if (dark) {
            ink(Math.floor(nc[0] * 0.35) + 20,
                Math.floor(nc[1] * 0.35) + 20,
                Math.floor(nc[2] * 0.35) + 22);
          } else {
            ink(Math.floor(255 - (255 - nc[0]) * 0.3),
                Math.floor(255 - (255 - nc[1]) * 0.3),
                Math.floor(255 - (255 - nc[2]) * 0.3));
          }
          box(x, y, btnW, btnH, true);
          if (dark) {
            ink(Math.floor(nc[0] * 0.5) + 30,
                Math.floor(nc[1] * 0.5) + 30,
                Math.floor(nc[2] * 0.5) + 33);
          } else {
            ink(Math.floor(255 - (255 - nc[0]) * 0.45),
                Math.floor(255 - (255 - nc[1]) * 0.45),
                Math.floor(255 - (255 - nc[2]) * 0.45));
          }
          box(x, y, btnW, btnH, "outline");
          const fg = dark ? (sharp ? 120 : 190) : (sharp ? 100 : 50);
          ink(fg, fg, fg);
        }

        const label = key ? key.toUpperCase() : "";
        write(label, { x: x + 2, y: y + 2, size: 1, font: "font_1" });

        if (btnH > 12) {
          if (isActive) ink(255, 255, 255, 180);
          else { const sl = dark ? (sharp ? 80 : 120) : (sharp ? 160 : 110); ink(sl, sl, sl); }
          write(letter + noteOctave, { x: x + 2, y: y + btnH - 12, size: 1, font: "font_1" });
        }

        // Pressure bar — fills from bottom of pad proportional to analog pressure
        if (isActive && pressures && key && pressures[key] !== undefined) {
          const p = pressures[key];
          const barH = Math.floor(p * (btnH - 2));
          if (barH > 0) {
            ink(255, 255, 255, 120);
            box(x + 1, y + btnH - 1 - barH, btnW - 2, barH, true);
          }
        }
      }
    }
  }

  drawGrid(LEFT_GRID, leftX, 0);
  drawGrid(RIGHT_GRID, rightX, 0);

  // === SETTINGS ROW + ACTIVE NOTE (under status bar) ===
  const settingsY = topBarH + 1;
  {
    // Settings: wave type, octave, quick, kHz — in matrix chunky font
    const metroInfo = metronomeEnabled ? " " + metronomeBPM + "bpm" : "";
    const sRate = sound?.speaker?.sampleRate;
    const khzStr = sRate ? " " + Math.round(sRate / 1000) + "kHz" : "";
    const settingsStr = wave + " o:" + octave + (quickMode ? " QK" : "") + metroInfo + khzStr;
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    write(settingsStr, { x: 2, y: settingsY, size: 1, font: "matrix" });

    // Active note name — red, matrix chunky, right side of settings row
    if (activeKeys.length > 0) {
      // Show all active note names
      let noteNames = [];
      for (const key of activeKeys) {
        const noteName = KEY_TO_NOTE[key];
        if (noteName) {
          const [letter, off] = parseNote(noteName);
          noteNames.push(letter + (octave + off));
        }
      }
      const noteStr = noteNames.join(" ");
      const noteX = w - noteStr.length * 8 - 2;
      ink(255, 60, 60);
      write(noteStr, { x: noteX, y: settingsY, size: 1, font: "matrix" });
    }
  }

  // Echo slider (below settings row, taller)
  {
    const sliderY = settingsY + 11;
    const sliderH = 10;
    ink(dark ? 25 : 235, dark ? 25 : 235, dark ? 28 : 238);
    box(0, sliderY, w, sliderH, true);
    const fillW = Math.floor(echoMix * w);
    if (fillW > 0) {
      ink(80, 120, 220, echoDragging ? 240 : 180);
      box(0, sliderY, fillW, sliderH, true);
    }
    // Handle knob
    if (echoMix > 0.005 || echoDragging) {
      const knobX = Math.max(1, Math.min(w - 3, Math.floor(echoMix * w)));
      ink(140, 180, 255, 220);
      box(knobX - 1, sliderY, 3, sliderH, true);
    }
    // Label centered vertically in slider
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const echoStr = "echo " + Math.round(echoMix * 100) + "%";
    write(echoStr, { x: 2, y: sliderY + 1, size: 1, font: "font_1" });
  }

  // WiFi panel overlay
  if (wifiPanelOpen && wifi) {
    const panelW = 138, panelH = 120;
    const panelX = w - panelW - 2, panelY = 12;

    // Background
    ink(dark ? 30 : 245, dark ? 30 : 245, dark ? 35 : 248, 240);
    box(panelX, panelY, panelW, panelH, true);
    ink(dark ? 60 : 200, dark ? 60 : 200, dark ? 70 : 210);
    box(panelX, panelY, panelW, panelH, "outline");

    // Title + interface name
    ink(FG, FG, FG);
    write("WiFi", { x: panelX + 3, y: panelY + 2, size: 1, font: "font_1" });

    // Status + interface info
    ink(FG_DIM, FG_DIM, FG_DIM);
    const wifiStatusStr = (wifi.status || "?") + (wifi.iface ? " [" + wifi.iface + "]" : "");
    write(wifiStatusStr, { x: panelX + 28, y: panelY + 2, size: 1, font: "font_1" });

    // Network list
    const nets = wifi.networks || [];
    const rowH = 12;
    const listY = panelY + 14;
    const maxRows = Math.min(nets.length, Math.floor((panelH - 16) / rowH));

    for (let i = 0; i < maxRows; i++) {
      const net = nets[i];
      const ry = listY + i * rowH;

      // Highlight selected
      if (i === wifiSelectedIdx) {
        ink(dark ? 50 : 220, dark ? 60 : 225, dark ? 80 : 240);
        box(panelX + 1, ry, panelW - 2, rowH, true);
      }

      // Signal bars (1-4 based on dBm)
      const bars = net.signal > -50 ? 4 : net.signal > -60 ? 3 : net.signal > -70 ? 2 : 1;
      for (let b = 0; b < 4; b++) {
        if (b < bars) ink(80, 200, 80);
        else ink(dark ? 40 : 220, dark ? 40 : 220, dark ? 45 : 225);
        box(panelX + 3 + b * 3, ry + 8 - (b + 1) * 2, 2, (b + 1) * 2, true);
      }

      // SSID name
      ink(FG, FG, FG);
      const ssidDisplay = net.ssid.length > 14 ? net.ssid.slice(0, 13) + "~" : net.ssid;
      write(ssidDisplay, { x: panelX + 16, y: ry + 1, size: 1, font: "font_1" });

      // Lock icon for encrypted
      if (net.encrypted) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("*", { x: panelW + panelX - 10, y: ry + 1, size: 1, font: "font_1" });
      }
    }

    // Password input (if in password mode)
    if (wifiPasswordMode) {
      const py = panelY + panelH - 14;
      ink(dark ? 20 : 250, dark ? 20 : 250, dark ? 25 : 255);
      box(panelX + 2, py, panelW - 4, 12, true);
      ink(dark ? 70 : 180, dark ? 70 : 180, dark ? 80 : 190);
      box(panelX + 2, py, panelW - 4, 12, "outline");
      ink(FG, FG, FG);
      const display = wifiPassword.length > 0 ? "*".repeat(wifiPassword.length) + "_" : "password_";
      write(display, { x: panelX + 4, y: py + 1, size: 1, font: "font_1" });
    }

    // Connected IP
    if (wifi.connected && wifi.ip) {
      ink(80, 200, 80);
      write(wifi.ip, { x: panelX + 3, y: panelY + panelH - 12, size: 1, font: "font_1" });
    }

    // Rescan timer
    if (frame % 300 === 0 && wifiPanelOpen) {
      wifi.scan();
    }
  }

  // Last key pressed indicator (bottom corner, fading)
  if (lastKeyTimer > 0 && lastKey) {
    ink(FG_MUTED, FG_MUTED, FG_MUTED, Math.min(150, lastKeyTimer * 3));
    write(lastKey, { x: 2, y: h - 10, size: 1 });
  }

  // Fade trails
  Object.keys(trail).forEach((key) => {
    if (!sounds[key]) {
      trail[key].brightness *= 0.92;
      if (trail[key].brightness < 0.02) delete trail[key];
    }
  });
}

function sim({ pressures, sound }) {
  // Continuously update synth volumes from analog key pressure
  if (!pressures) return;
  for (const key of Object.keys(sounds)) {
    const entry = sounds[key];
    if (!entry?.synth) continue;
    const p = pressures[key]; // 0.0-1.0 analog pressure, undefined if not analog
    if (p !== undefined) {
      const vol = p * 0.7; // Linear: 0 pressure = silence, full press = 0.7
      entry.synth.update({ volume: vol });
      if (trail[key]) trail[key].brightness = p;
    }
  }
}

export { boot, act, paint, sim };

