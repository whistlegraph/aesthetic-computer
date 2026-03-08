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

// Echo (room) mix — controlled by trackpad X / slider
let echoMix = 0;
let echoDragging = false;

// Pitch shift — controlled by trackpad Y / slider
let pitchShift = 0; // -1 to +1, 0 = no shift

// Trackpad FX control (\ toggles on/off)
let trackpadFX = true;

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
let dark = true; // always dark

// Background color — average of active notes, lerped
let bgColor = [0, 0, 0];
let bgTarget = dark ? [20, 20, 25] : [255, 255, 255];

// Cached sound API ref (for leave)
let soundAPI = null;

// Debug: last key pressed
let lastKey = "";
let lastKeyTimer = 0;

// WiFi UI state
let wifiPanelOpen = false;
let wifiSelectedIdx = -1;
let wifiPassword = "";
let wifiPasswordMode = false;  // true = fullscreen password entry
let shiftHeld = false;

// AC chat: latest message fetched after WiFi connects
let acMsg = null;            // { from, text } once loaded
let acMsgFetched = false;    // true once fetch triggered
let wifiWasConnected = false;

// Auto-connect: try "aesthetic.computer" hotspot when not connected
const AC_SSID = "aesthetic.computer";
const AC_PASS = "aesthetic.computer";
let autoConnectFrame = 0;    // counts frames; try every ~5s (300 frames)
let autoConnectBlink = 0;    // blink counter for antenna icon while polling

// Saved WiFi credentials — persisted to /mnt/wifi_creds.json (USB EFI partition)
const CREDS_PATH = "/mnt/wifi_creds.json";
let savedCreds = [];         // [{ssid, pass}, ...] loaded at boot

// US-QWERTY shift map for bare-metal text input (no OS layout handling)
const SHIFT_MAP = {
  "1":"!","2":"@","3":"#","4":"$","5":"%","6":"^","7":"&","8":"*","9":"(","0":")",
  "-":"_","=":"+","[":"{","]":"}",";":":","'":'"',",":"<",".":">","/":"?","\\":"|","`":"~",
};
let wifiCursorBlink = 0;       // cursor blink counter
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
  c: [255, 30, 30], "c#": [255, 80, 0],
  d: [255, 150, 0], "d#": [200, 200, 0],
  e: [230, 220, 0],
  f: [30, 200, 30], "f#": [0, 200, 180],
  g: [30, 100, 255], "g#": [80, 50, 255],
  a: [140, 30, 220], "a#": [200, 30, 150],
  b: [200, 50, 255],
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

function boot({ wipe, system }) {
  wipe(0);
  // Load saved credentials from USB EFI partition
  if (system?.readFile) {
    try {
      const raw = system.readFile(CREDS_PATH);
      if (raw) savedCreds = JSON.parse(raw);
    } catch (_) {}
  }
}

function act({ event: e, sound, wifi }) {
  soundAPI = sound;
  // Track shift state before any other handling
  if (e.is("keyboard:down") && e.key?.toLowerCase() === "shift") shiftHeld = true;
  if (e.is("keyboard:up") && e.key?.toLowerCase() === "shift") shiftHeld = false;

  // WiFi password input mode — fullscreen, capture all keyboard
  if (wifiPasswordMode && e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    lastKey = key; lastKeyTimer = 180; // debug: show raw key name on screen
    if (key === "escape") { wifiPasswordMode = false; wifiPassword = ""; wifiSelectedIdx = -1; return; }
    if (key === "enter") {
      if (wifi && wifiSelectedIdx >= 0) {
        const nets = wifi.networks || [];
        const net = nets[wifiSelectedIdx];
        if (net) {
          wifi.connect(net.ssid, wifiPassword);
          // Save credentials persistently (skip duplicates)
          const already = savedCreds.find((c) => c.ssid === net.ssid);
          if (!already) savedCreds.push({ ssid: net.ssid, pass: wifiPassword });
          else already.pass = wifiPassword;
        }
      }
      wifiPasswordMode = false;
      wifiPanelOpen = false;
      wifiSelectedIdx = -1;
      return;
    }
    if (key === "backspace") { wifiPassword = wifiPassword.slice(0, -1); return; }
    if (key.length === 1) {
      wifiPassword += shiftHeld ? (SHIFT_MAP[key] ?? key.toUpperCase()) : key;
      return;
    }
    return;
  }

  // WiFi password mode — block touch from reaching note grid
  if (wifiPasswordMode && (e.is("touch") || e.is("draw") || e.is("lift"))) {
    return;
  }

  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (!key) return;

    // Debug: show every key
    lastKey = key;
    lastKeyTimer = 120; // show for 2 seconds

    if (key === "escape" && wifiPanelOpen) { wifiPanelOpen = false; return; }
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
    if (key === "\\") {
      trackpadFX = !trackpadFX;
      sound.synth({
        type: "sine", tone: trackpadFX ? 880 : 440,
        duration: 0.06, volume: 0.2,
        attack: 0.002, decay: 0.05, pan: 0,
      });
      return;
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
      const playFreq = freq * Math.pow(2, pitchShift);
      const synth = sound.synth({
        type: wave, tone: playFreq,
        duration: Infinity,
        volume: vol, attack: quickMode ? 0.002 : 0.005,
        decay: 0.1, pan: pan,
      });
      sounds[key] = { synth, note: letter, octave: noteOctave, baseFreq: freq };
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

    // WiFi fullscreen network list clicks
    if (wifiPanelOpen && !wifiPasswordMode) {
      const nets = wifi?.networks || [];
      const rowH = 16;
      const listY = 44;
      const clickedRow = Math.floor((y - listY) / rowH);
      if (clickedRow >= 0 && clickedRow < nets.length) {
        wifiSelectedIdx = clickedRow;
        if (nets[clickedRow].encrypted) {
          wifiPassword = "";
          wifiPasswordMode = true;
        } else {
          const openNet = nets[clickedRow];
          wifi?.connect(openNet.ssid, "");
          // Save open network credential entry
          if (!savedCreds.find((c) => c.ssid === openNet.ssid))
            savedCreds.push({ ssid: openNet.ssid, pass: "" });
          wifiPanelOpen = false;
        }
      }
      return;
    }

    // Echo slider zone (below settings row, y 26-38)
    if (y >= 26 && y < 38) {
      echoDragging = true;
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
      return;
    }
    // Pitch slider zone (below echo slider, y 38-50)
    if (y >= 38 && y < 50) {
      pitchShift = Math.max(-1, Math.min(1, (x / w) * 2 - 1)); // map 0-w to -1..+1
      const factor = Math.pow(2, pitchShift);
      for (const k of Object.keys(sounds)) {
        const s = sounds[k];
        if (s && s.synth && s.baseFreq) s.synth.update({ tone: s.baseFreq * factor });
      }
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
        const playFreq = freq * Math.pow(2, pitchShift);
        const synth = sound.synth({
          type: wave, tone: playFreq, duration: Infinity,
          volume: 0.5, attack: 0.005, decay: 0.1, pan,
        });
        sounds[hitNote.key] = { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq };
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

  // Trackpad FX: X = echo, Y = pitch shift (when enabled via \)
  if (trackpadFX && trackpad) {
    if (trackpad.dx !== 0) {
      echoMix = Math.max(0, Math.min(1, echoMix + (trackpad.dx * 3) / w));
      sound?.room?.setMix?.(echoMix);
    }
    if (trackpad.dy !== 0) {
      pitchShift = Math.max(-1, Math.min(1, pitchShift - trackpad.dy / h));
      // Apply pitch shift to all active voices
      const factor = Math.pow(2, pitchShift); // ±1 octave
      for (const key of Object.keys(sounds)) {
        const s = sounds[key];
        if (s && s.synth && s.baseFreq) {
          s.synth.update({ tone: s.baseFreq * factor });
        }
      }
    }
  }

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
  // dark mode is always on

  // Clean dark theme — no time-of-day tinting
  const FG = 220;
  const FG_DIM = 140;
  const FG_MUTED = 80;
  const BAR_BG = [35, 20, 30];
  const BAR_BORDER = [55, 35, 45];
  const PAD_NORMAL = [28, 28, 30];
  const PAD_SHARP = [18, 18, 20];
  const PAD_OUTLINE = [50, 50, 55];

  // Compute background color from active notes — full color flash
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
    // Saturate: boost toward max channel, clamp to 255
    const avg = [r / n, g / n, b / n];
    const mx = Math.max(avg[0], avg[1], avg[2], 1);
    const sat = 255 / mx; // scale so brightest channel hits 255
    bgTarget = [
      Math.min(255, Math.floor(avg[0] * sat)),
      Math.min(255, Math.floor(avg[1] * sat)),
      Math.min(255, Math.floor(avg[2] * sat)),
    ];
    // Snap on instantly (blink)
    bgColor[0] = bgTarget[0];
    bgColor[1] = bgTarget[1];
    bgColor[2] = bgTarget[2];
  } else {
    bgTarget = [0, 0, 0];
    // Fade to black smoothly
    bgColor[0] += (bgTarget[0] - bgColor[0]) * 0.25;
    bgColor[1] += (bgTarget[1] - bgColor[1]) * 0.25;
    bgColor[2] += (bgTarget[2] - bgColor[2]) * 0.25;
  }

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

  // Left: "notepat.com" — notepat in fg, .com in accent
  ink(FG, FG, FG, 200);
  write("notepat", { x: 2, y: barY, size: 1, font: "matrix" });
  ink(dark ? 200 : 180, dark ? 100 : 60, dark ? 140 : 120);
  const dotComX = 2 + 7 * 4; // x position of ".com" (7 matrix chars * ~4px each)
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

  // TTS + WebSocket connect on WiFi connect transition
  if (wifi?.connected && !wifiWasConnected) {
    sound?.synth({ type: "sine", tone: 880, duration: 0.08, volume: 0.3, attack: 0.01, decay: 0.06 });
    sound?.speak("connected");
    system.ws?.connect("wss://chat-system.aesthetic.computer/");
    // Persist credentials to USB EFI partition
    if (system?.writeFile && savedCreds.length > 0) {
      system.writeFile(CREDS_PATH, JSON.stringify(savedCreds));
    }
  }
  wifiWasConnected = !!wifi?.connected;

  // Process incoming WebSocket chat messages (real-time)
  const wsMsgs = system.ws?.messages;
  if (wsMsgs?.length) {
    for (const raw of wsMsgs) {
      try {
        const msg = JSON.parse(raw);
        if (msg.type === "connected") {
          // Initial backlog — grab most recent message
          const content = JSON.parse(msg.content);
          const last = (content.messages || []).slice(-1)[0];
          if (last?.from && last?.text) acMsg = { from: last.from, text: last.text };
        } else if (msg.type === "message") {
          const m = JSON.parse(msg.content);
          if (m?.from && m?.text) acMsg = { from: m.from, text: m.text };
        }
      } catch (_) {}
    }
  }

  // Auto-connect to aesthetic.computer hotspot when not connected
  autoConnectFrame++;
  autoConnectBlink = (autoConnectBlink + 1) % 60;
  const notConnecting = wifi && !wifi.connected &&
    wifi.state !== 3 /* CONNECTING */ && wifi.state !== 4 /* CONNECTED */;
  if (notConnecting && autoConnectFrame % 300 === 0) {
    // Build candidate list: AC hotspot first, then any other saved creds
    const acCred = { ssid: AC_SSID, pass: AC_PASS };
    const others = savedCreds.filter((c) => c.ssid !== AC_SSID);
    const candidates = [acCred, ...others];
    const cred = candidates[Math.floor(autoConnectFrame / 300) % candidates.length];
    wifi.connect(cred.ssid, cred.pass);
  }

  // Right section: wifi | battery | time | vol
  let rx = w - 2; // right edge cursor (builds right to left)

  // WiFi antenna icon (clickable)
  {
    const ax = w - 12, ay = 3;
    const wifiConnected = wifi?.connected;
    const wifiState = wifi?.state ?? 0;
    const autoPollBlink = autoConnectBlink < 30; // on half the time
    // Draw antenna icon with appropriate color
    if (wifiConnected) {
      ink(80, 200, 80); // solid green when connected
    } else if (wifiState === 3 /* CONNECTING */) {
      ink(200, 200, 80); // yellow while connecting
    } else if (wifiState === 1 || wifiState === 2 /* SCANNING */) {
      ink(200, 200, 80); // yellow while scanning
    } else {
      // idle: blink to show auto-polling in progress
      const b = autoPollBlink ? (dark ? 140 : 200) : (dark ? 50 : 90);
      ink(b, b, dark ? b + 10 : b + 30);
    }
    // Simple antenna icon: vertical line + arms
    line(ax + 5, ay + 1, ax + 5, ay + 7); // vertical
    line(ax + 3, ay + 5, ax + 5, ay + 3); // left arm
    line(ax + 7, ay + 5, ax + 5, ay + 3); // right arm
    if (wifiConnected || wifiState >= 1) {
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

  // Echo slider (below settings row)
  {
    const sliderY = settingsY + 11;
    const sliderH = 12;
    ink(dark ? 25 : 235, dark ? 25 : 235, dark ? 28 : 238);
    box(0, sliderY, w, sliderH, true);
    const fillW = Math.floor(echoMix * w);
    if (fillW > 0) {
      ink(80, 120, 220, trackpadFX ? 240 : 180);
      box(0, sliderY, fillW, sliderH, true);
    }
    if (echoMix > 0.005) {
      const knobX = Math.max(1, Math.min(w - 3, Math.floor(echoMix * w)));
      ink(140, 180, 255, 220);
      box(knobX - 1, sliderY, 3, sliderH, true);
    }
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const echoStr = "echo " + Math.round(echoMix * 100) + "%";
    write(echoStr, { x: 2, y: sliderY + 1, size: 1, font: "font_1" });
    // Trackpad FX indicator
    if (trackpadFX) {
      ink(120, 220, 120);
      write("\\", { x: w - 8, y: sliderY + 1, size: 1, font: "font_1" });
    }
  }

  // Pitch shift slider (below echo slider)
  {
    const sliderY = settingsY + 23;
    const sliderH = 12;
    ink(dark ? 25 : 235, dark ? 25 : 235, dark ? 28 : 238);
    box(0, sliderY, w, sliderH, true);
    // Center line (pitch = 0)
    const centerX = Math.floor(w / 2);
    ink(dark ? 40 : 220, dark ? 40 : 220, dark ? 45 : 225);
    box(centerX, sliderY, 1, sliderH, true);
    // Fill from center
    const pitchX = Math.floor((pitchShift + 1) / 2 * w);
    if (Math.abs(pitchShift) > 0.005) {
      ink(200, 100, 160, trackpadFX ? 240 : 180);
      const fx = Math.min(centerX, pitchX);
      const fw = Math.abs(pitchX - centerX);
      box(fx, sliderY, fw, sliderH, true);
    }
    // Knob
    if (Math.abs(pitchShift) > 0.005) {
      ink(255, 140, 180, 220);
      box(Math.max(1, Math.min(w - 3, pitchX)) - 1, sliderY, 3, sliderH, true);
    }
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const cents = Math.round(pitchShift * 1200); // ±1200 cents = ±1 octave
    const pitchStr = "pitch " + (cents >= 0 ? "+" : "") + cents + "c";
    write(pitchStr, { x: 2, y: sliderY + 1, size: 1, font: "font_1" });
  }

  // WiFi fullscreen password entry
  if (wifiPasswordMode && wifi) {
    wifiCursorBlink++;
    const nets = wifi.networks || [];
    const net = nets[wifiSelectedIdx];
    const ssid = net ? net.ssid : "?";

    // Fullscreen dark overlay
    ink(dark ? 10 : 240, dark ? 10 : 240, dark ? 15 : 245, 250);
    box(0, 0, w, h, true);

    // Title: network name in matrix chunky (centered estimate: ~9px per char at scale 2)
    ink(FG, FG, FG);
    const titleX = Math.max(10, (w - ssid.length * 18) / 2);
    write(ssid, { x: titleX, y: h / 2 - 50, size: 2, font: "matrix" });

    // "enter password" label
    ink(FG_DIM, FG_DIM, FG_DIM);
    write("enter password:", { x: 20, y: h / 2 - 20, size: 1, font: "font_1" });

    // Password field — visible text with blinking cursor
    const cursor = (wifiCursorBlink % 60) < 35 ? "|" : "";
    const pwDisplay = wifiPassword + cursor;
    ink(FG, FG, FG);

    // Background box for password field
    ink(dark ? 25 : 235, dark ? 25 : 235, dark ? 30 : 240);
    box(18, h / 2 - 6, w - 36, 18, true);
    ink(dark ? 70 : 180, dark ? 70 : 180, dark ? 80 : 190);
    box(18, h / 2 - 6, w - 36, 18, "outline");

    // Password text (visible!)
    ink(FG, FG, FG);
    write(pwDisplay, { x: 22, y: h / 2 - 2, size: 1, font: "font_1" });

    // Instructions
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    write("Enter: connect    Esc: cancel", { x: 20, y: h / 2 + 22, size: 1, font: "font_1" });

    // Show connection status if connecting
    if (wifi.state === 3) { // WIFI_STATE_CONNECTING
      ink(200, 200, 80);
      write("connecting...", { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    } else if (wifi.state === 5) { // WIFI_STATE_FAILED
      ink(220, 80, 80);
      write("failed: " + (wifi.status || "?"), { x: 20, y: h / 2 + 40, size: 1, font: "font_1" });
    }
  }

  // WiFi fullscreen network chooser
  else if (wifiPanelOpen && wifi) {
    // Fullscreen dark overlay
    ink(dark ? 10 : 240, dark ? 10 : 240, dark ? 15 : 245, 250);
    box(0, 0, w, h, true);

    // Title
    ink(FG, FG, FG);
    write("WiFi Networks", { x: 20, y: 12, size: 2, font: "matrix" });

    // Status + interface info
    ink(FG_DIM, FG_DIM, FG_DIM);
    const wifiStatusStr = (wifi.status || "scanning...") + (wifi.iface ? " [" + wifi.iface + "]" : "");
    write(wifiStatusStr, { x: 20, y: 30, size: 1, font: "font_1" });

    // Network list (fullscreen, generous row height)
    const nets = wifi.networks || [];
    const rowH = 16;
    const listY = 44;
    const maxRows = Math.min(nets.length, Math.floor((h - listY - 20) / rowH));

    for (let i = 0; i < maxRows; i++) {
      const net = nets[i];
      const ry = listY + i * rowH;

      // Highlight selected
      if (i === wifiSelectedIdx) {
        ink(dark ? 40 : 220, dark ? 50 : 225, dark ? 70 : 240);
        box(10, ry, w - 20, rowH, true);
      }

      // Signal bars
      const bars = net.signal > -50 ? 4 : net.signal > -60 ? 3 : net.signal > -70 ? 2 : 1;
      for (let b = 0; b < 4; b++) {
        if (b < bars) ink(80, 200, 80);
        else ink(dark ? 40 : 220, dark ? 40 : 220, dark ? 45 : 225);
        box(16 + b * 4, ry + 10 - (b + 1) * 2, 3, (b + 1) * 2, true);
      }

      // SSID name
      ink(FG, FG, FG);
      const ssidDisplay = net.ssid.length > 28 ? net.ssid.slice(0, 27) + "~" : net.ssid;
      write(ssidDisplay, { x: 36, y: ry + 2, size: 1, font: "font_1" });

      // Lock icon for encrypted
      if (net.encrypted) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("*", { x: w - 20, y: ry + 2, size: 1, font: "font_1" });
      }
    }

    // Connected IP + latest AC message at bottom
    if (wifi.connected && wifi.ip) {
      ink(80, 200, 80);
      write("connected: " + wifi.ip, { x: 20, y: h - 26, size: 1, font: "font_1" });
      if (acMsg) {
        ink(FG_DIM, FG_DIM, FG_DIM);
        const preview = (acMsg.from + ": " + acMsg.text).slice(0, 52);
        write(preview, { x: 20, y: h - 16, size: 1, font: "font_1" });
      } else if (acMsgFetched) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("loading...", { x: 20, y: h - 16, size: 1, font: "font_1" });
      }
    }

    // Instructions
    ink(FG_MUTED, FG_MUTED, FG_MUTED);
    write("Esc: close", { x: w - 60, y: h - 26, size: 1, font: "font_1" });

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

  // HDMI output: blend active note colors → solid fill on secondary display
  if (system.hasHdmi) {
    const activeKeys = Object.keys(sounds);
    if (activeKeys.length > 0) {
      let tr = 0, tg = 0, tb = 0;
      for (const k of activeKeys) {
        const s = sounds[k];
        if (s && s.note) {
          const c = noteColor(s.note);
          tr += c[0]; tg += c[1]; tb += c[2];
        }
      }
      const n = activeKeys.length;
      system.hdmi(Math.round(tr / n), Math.round(tg / n), Math.round(tb / n));
    } else {
      system.hdmi(0, 0, 0);
    }
  }
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

function leave() {
  // Reset all FX before shutdown
  echoMix = 0;
  pitchShift = 0;
  trackpadFX = false;
  soundAPI?.room?.setMix?.(0);
  // Stop all playing sounds
  for (const key of Object.keys(sounds)) {
    const s = sounds[key];
    if (s && s.synth) s.synth.stop?.();
  }
  sounds = {};
}

export { boot, act, paint, sim, leave };

