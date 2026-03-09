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
let metronomeBPM = 120;
let metronomeBeatCount = 0;
let metronomeFlash = 0;
let metronomeVisualPhase = 0;
let metronomePendulumAngle = 0;  // swinging pendulum animation (-1..1)
let metronomePendulumDir = 1;

// Clock time sync
let clockOffset = 0;       // ms offset added to Date.now() for accurate time
let clockSynced = false;   // true once we have a server time sync
let clockSyncFrame = 0;    // frame counter for periodic resync
function syncedNow() { return Date.now() + clockOffset; }

// Echo (room) mix — controlled by trackpad X / slider
let echoMix = 0;
let echoDragging = false;
let pitchDragging = false;

// Pitch shift — controlled by trackpad Y / slider
let pitchShift = 0; // -1 to +1, 0 = no shift

// Trackpad FX control (\ toggles on/off)
let trackpadFX = false;

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


// OS image download URL (shown on "os" button tap)
const OS_URL = "releases.aesthetic.computer/os/native-notepat-latest.img.gz";
let osUrlVisible = false;

// WiFi UI state
let wifiPanelOpen = false;
let wifiSelectedIdx = -1;
let wifiPassword = "";
let wifiPasswordMode = false;  // true = fullscreen password entry
let shiftHeld = false;

// AC chat: latest message fetched after WiFi connects
let acMsg = null;            // { from, text } once loaded
let wsStatus = "";           // "connecting" | "connected" | "error" | ""
let wsConnectGrace = 0;      // frames to wait before declaring error (race-condition guard)
let wsReconnectTimer = 0;   // frames until next reconnect attempt
let chatMuted = false;       // mute TTS for incoming chat messages
let wifiWasConnected = false;
let lastBatPercent = -1;     // for battery change TTS

// Auto-connect: try "aesthetic.computer" hotspot when not connected
const AC_SSID = "aesthetic.computer";
const AC_PASS = "aesthetic.computer";
// Fallback networks to try if AC hotspot isn't available (cycled in order)
const FALLBACK_WIFI = [
  { ssid: "ATT2AWTpcr", pass: "t84q%7%g2h8u" },
];
let autoConnectFrame = 0;    // counts frames; try every ~5s (300 frames)
let autoConnectBlink = 0;    // blink counter for antenna icon while polling
let autoConnectTry = 0;      // increments each attempt; beep on 1st/2nd/3rd

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
// Hover / cursor position (updated from touch + draw events)
let hoverX = -1, hoverY = -1;

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

// Play a short identifying blip in the new wave type when switching
function playWaveSound(sound, waveType) {
  if (!sound?.synth) return;
  const tones = { sine: 660, triangle: 550, sawtooth: 440, square: 330, noise: 220 };
  sound.synth({
    type: waveType === "noise" ? "noise" : waveType,
    tone: tones[waveType] || 440,
    duration: 0.07, volume: 0.18,
    attack: 0.002, decay: 0.06, pan: 0,
  });
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

    if (key === "escape" && wifiPanelOpen) { wifiPanelOpen = false; return; }
    if (key === "shift") { quickMode = !quickMode; return; }
    if (key === "tab") {
      waveIndex = (waveIndex + 1) % wavetypes.length;
      wave = wavetypes[waveIndex];
      playWaveSound(sound, wave); return;
    }
    if (key === "space") {
      metronomeEnabled = !metronomeEnabled;
      if (metronomeEnabled) {
        metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
      }
      return;
    }
    if (key >= "1" && key <= "9") { octave = parseInt(key); return; }
    if (key === "arrowup") { octave = Math.min(9, octave + 1); return; }
    if (key === "arrowdown") { octave = Math.max(1, octave - 1); return; }
    if (key === "arrowleft") {
      waveIndex = (waveIndex - 1 + wavetypes.length) % wavetypes.length;
      wave = wavetypes[waveIndex];
      playWaveSound(sound, wave); return;
    }
    if (key === "arrowright") {
      waveIndex = (waveIndex + 1) % wavetypes.length;
      wave = wavetypes[waveIndex];
      playWaveSound(sound, wave); return;
    }
    if (key === "\\") {
      trackpadFX = !trackpadFX;
      sound.synth({
        type: "sine", tone: trackpadFX ? 880 : 440,
        duration: 0.06, volume: 0.2,
        attack: 0.002, decay: 0.05, pan: 0,
      });
      // FX on: bright chord; FX off: hollow low note
      if (trackpadFX) sound?.synth({ type: "triangle", tone: 1320, duration: 0.08, volume: 0.15, attack: 0.002, decay: 0.06 });
      return;
    }
    if (key === "-") {
      metronomeBPM = Math.max(20, metronomeBPM - 5);
      metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
      return;
    }
    if (key === "=") {
      metronomeBPM = Math.min(300, metronomeBPM + 5);
      metronomeBeatCount = Math.floor(syncedNow() / (60000 / metronomeBPM));
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
    hoverX = x; hoverY = y;

    // "os" button: top bar — hit zone set dynamically in paint()
    const ob = globalThis.__osBtn;
    if (ob && y < ob.h && x >= ob.x && x <= ob.x + ob.w) {
      osUrlVisible = !osUrlVisible;
      return;
    }

    // Mute button (chat TTS toggle) — position set dynamically in paint
    const mb = globalThis.__muteBtn;
    if (y < 16 && mb && x >= mb.x - 1 && x <= mb.x + mb.w) {
      chatMuted = !chatMuted;
      sound?.synth({ type: "sine", tone: chatMuted ? 330 : 660, duration: 0.05, volume: 0.15, attack: 0.002, decay: 0.04 });
      return;
    }

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

    // Echo slider zone (y 15-26)
    if (y >= 15 && y < 27) {
      echoDragging = true;
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
      return;
    }
    // Pitch slider zone (y 27-38)
    if (y >= 27 && y < 39) {
      pitchDragging = true;
      pitchShift = Math.max(-1, Math.min(1, (x / w) * 2 - 1));
      const factor = Math.pow(2, pitchShift);
      for (const k of Object.keys(sounds)) {
        const s = sounds[k];
        if (s && s.synth && s.baseFreq) s.synth.update({ tone: s.baseFreq * factor });
      }
      return;
    }
    // Wave type buttons (y 39-52)
    const wb = globalThis.__waveButtons;
    if (wb && y >= wb.y && y < wb.y + wb.h) {
      if (x >= wb.octX) {
        // Octave button — tap to increment
        octave = (octave % 6) + 1;
      } else {
        const idx = Math.min(wavetypes.length - 1, Math.floor(x / wb.btnW));
        waveIndex = idx;
        wave = wavetypes[idx];
        playWaveSound(sound, wave);
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
    const pid = e.pointer?.id ?? 0;
    hoverX = x; hoverY = y;
    if (echoDragging) {
      echoMix = Math.max(0, Math.min(1, x / w));
      sound?.room?.setMix?.(echoMix);
    }
    if (pitchDragging) {
      pitchShift = Math.max(-1, Math.min(1, (x / w) * 2 - 1));
      const factor = Math.pow(2, pitchShift);
      for (const k of Object.keys(sounds)) {
        const s = sounds[k];
        if (s && s.synth && s.baseFreq) s.synth.update({ tone: s.baseFreq * factor });
      }
    }
    // Grid rollover: dragging across note buttons triggers the new one
    const gridInfo = globalThis.__gridInfo;
    if (gridInfo && touchNotes[pid] !== undefined) {
      const hitNote = hitTestGrid(x, y, gridInfo);
      if (hitNote && hitNote.key && hitNote.key !== touchNotes[pid]?.key) {
        // Release current note
        const oldKey = touchNotes[pid].key;
        if (sounds[oldKey]) {
          sound.kill(sounds[oldKey].synth || sounds[oldKey], 0.02);
          delete sounds[oldKey];
        }
        // Trigger new note
        if (!sounds[hitNote.key]) {
          const freq = noteToFreq(hitNote.letter, hitNote.octave);
          const semitones = (hitNote.octave - 4) * 12 + CHROMATIC.indexOf(hitNote.letter);
          const pan = Math.max(-0.8, Math.min(0.8, (semitones - 12) / 15));
          const synth = sound.synth({
            type: wave, tone: freq * Math.pow(2, pitchShift),
            duration: Infinity, volume: 0.5, attack: 0.005, decay: 0.1, pan,
          });
          sounds[hitNote.key] = { synth, note: hitNote.letter, octave: hitNote.octave, baseFreq: freq };
          trail[hitNote.key] = { note: hitNote.letter, octave: hitNote.octave, brightness: 1.0 };
          touchNotes[pid] = { key: hitNote.key };
        }
      }
    }
  }
  if (e.is("lift")) {
    const pid = e.pointer?.id ?? 0;
    if (echoDragging) echoDragging = false;
    if (pitchDragging) pitchDragging = false;
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

  // Metronome tick (clock-synced)
  if (metronomeEnabled && metronomeBPM > 0) {
    const now = syncedNow();
    const msPerBeat = 60000 / metronomeBPM;
    const beatNumber = Math.floor(now / msPerBeat);
    // Pendulum: smooth sinusoidal swing synced to beat phase (2-beat period)
    const beatPhase = (now % (msPerBeat * 2)) / (msPerBeat * 2);
    metronomePendulumAngle = Math.sin(beatPhase * Math.PI * 2);
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
  // "os" button — clickable, shows download URL
  const osX = dotComX + 4 * 4 + 6;
  const osHovered = hoverX >= osX - 2 && hoverX <= osX + 14 && hoverY < topBarH;
  if (osHovered) { ink(255, 255, 255, 30); box(osX - 2, 0, 18, topBarH, true); }
  ink(dark ? (osHovered ? 120 : 80) : (osHovered ? 200 : 140),
      dark ? (osHovered ? 210 : 160) : (osHovered ? 150 : 100),
      dark ? (osHovered ? 120 : 80) : (osHovered ? 200 : 140));
  write("os", { x: osX, y: barY, size: 1, font: "matrix" });
  globalThis.__osBtn = { x: osX - 2, w: 14, h: topBarH };

  // WS status + latest chat + mute button — right of "os"
  let chatX = osX + 2 * 4 + 6;
  if (acMsg) {
    // Always show latest message if we have one — even after server drops
    const maxChatChars = Math.floor((w - chatX - 130) / 4);
    if (maxChatChars > 6) {
      const preview = (acMsg.from + ": " + acMsg.text).slice(0, maxChatChars);
      ink(dark ? 140 : 100, dark ? 180 : 130, dark ? 140 : 100);
      write(preview, { x: chatX, y: barY, size: 1, font: "matrix" });
      chatX += preview.length * 4 + 2;
    }
  } else if (wsStatus === "connecting") {
    ink(200, 200, 80);
    write("chat...", { x: chatX, y: barY, size: 1, font: "matrix" });
    chatX += 7 * 4 + 2;
  } else if (wsStatus === "error") {
    ink(220, 80, 80);
    write("chat?", { x: chatX, y: barY, size: 1, font: "matrix" });
    chatX += 5 * 4 + 2;
  } else if (wsStatus === "connected") {
    // Connected but no message yet
    ink(dark ? 60 : 180, dark ? 100 : 190, dark ? 60 : 180);
    write("chat", { x: chatX, y: barY, size: 1, font: "matrix" });
    chatX += 4 * 4 + 2;
  }
  // Metronome indicator (pendulum) in status bar — shown when enabled
  if (metronomeEnabled) {
    const metX = chatX;
    // BPM label
    ink(dark ? 160 : 100, dark ? 160 : 100, dark ? 170 : 110);
    const bpmLabel = metronomeBPM + "b";
    write(bpmLabel, { x: metX, y: barY, size: 1, font: "matrix" });
    chatX += bpmLabel.length * 4 + 3;
    // Pendulum: pivot at top-center of a small 10px zone, swinging bob
    const px = chatX + 5;
    const pvY = barY + 1;
    const armLen = 8;
    const angle = metronomePendulumAngle * 0.45; // radians max ≈ 26°
    const bobX = Math.round(px + Math.sin(angle) * armLen);
    const bobY = Math.round(pvY + Math.cos(angle) * armLen);
    // Flash on downbeat
    const db = (metronomeBeatCount % 4) === 0;
    const fa = Math.floor(metronomeVisualPhase * 255);
    if (fa > 0) {
      ink(db ? 255 : 180, db ? 100 : 180, db ? 100 : 255, fa);
    } else {
      ink(dark ? 120 : 150, dark ? 130 : 150, dark ? 140 : 160);
    }
    line(px, pvY, bobX, bobY);
    box(bobX - 1, bobY - 1, 3, 3, true);
    chatX += 14;
  }

  // Mute button: "M" when muted, "m" when live
  const muteX = chatX;
  const muteHovered = hoverX >= muteX - 1 && hoverX <= muteX + 8 && hoverY < topBarH;
  if (muteHovered) { ink(255, 255, 255, 30); box(muteX - 1, 0, 10, topBarH, true); }
  ink(chatMuted ? (dark ? 200 : 80) : (dark ? 80 : 180),
      chatMuted ? (dark ? 80 : 200) : (dark ? 80 : 180),
      chatMuted ? (dark ? 80 : 80) : (dark ? 80 : 180));
  write(chatMuted ? "M" : "m", { x: muteX, y: barY, size: 1, font: "matrix" });
  globalThis.__muteBtn = { x: muteX, w: 10 };

  // OS URL overlay (shown when "os" tapped)
  if (osUrlVisible) {
    ink(0, 0, 0, 210);
    box(0, topBarH, w, 14, true);
    ink(80, 200, 100);
    write(OS_URL, { x: 4, y: topBarH + 3, size: 1, font: "font_1" });
  }

  // Center status bar: note count is enough, settings go below
  const centerX = Math.floor(w / 2);

  // TTS + WebSocket connect on WiFi connect transition
  if (wifi?.connected && !wifiWasConnected) {
    // WiFi connect chord: perfect fifth (G5 + D6)
    sound?.synth({ type: "sine", tone: 784, duration: 0.18, volume: 0.22, attack: 0.005, decay: 0.15 });
    sound?.synth({ type: "sine", tone: 1175, duration: 0.18, volume: 0.16, attack: 0.005, decay: 0.15 });
    autoConnectTry = 0;
    wsStatus = "connecting";
    wsConnectGrace = 180; // ~3s grace before declaring error
    system.ws?.connect("wss://chat-system.aesthetic.computer/");
    if (system?.writeFile && savedCreds.length > 0) {
      system.writeFile(CREDS_PATH, JSON.stringify(savedCreds));
    }
    // Kick off clock sync fetch
    clockSynced = false;
    system.fetch?.("https://aesthetic.computer/api/clock");
    clockSyncFrame = 0;
  }
  wifiWasConnected = !!wifi?.connected;

  // Poll for clock sync result
  if (system.fetchResult) {
    const t1 = Date.now();
    try {
      const serverTime = new Date(system.fetchResult).getTime();
      if (!isNaN(serverTime)) {
        // Simple: treat fetchResult as server time at moment of receipt
        const newOffset = serverTime - t1;
        clockOffset += (newOffset - clockOffset) * 0.5;
        clockSynced = true;
      }
    } catch (e) { /* ignore parse errors */ }
  }
  // Periodic re-sync every ~10 min (at 60fps: 36000 frames)
  clockSyncFrame++;
  if (wifi?.connected && clockSyncFrame % 36000 === 0) {
    system.fetch?.("https://aesthetic.computer/api/clock");
  }

  // Track WS connection status + auto-reconnect when server drops
  if (wsConnectGrace > 0) wsConnectGrace--;
  if (wsReconnectTimer > 0) wsReconnectTimer--;
  if (system.ws?.connecting) { wsStatus = "connecting"; wsConnectGrace = 0; }
  else if (system.ws?.connected && wsStatus !== "connected") {
    wsStatus = "connected";
    wsConnectGrace = 0;
    // Chat connected: bright rising two-note ding
    sound?.synth({ type: "sine", tone: 1047, duration: 0.1, volume: 0.18, attack: 0.003, decay: 0.08 });
    sound?.synth({ type: "sine", tone: 1319, duration: 0.1, volume: 0.14, attack: 0.02, decay: 0.08 });
  } else if (!system.ws?.connected && !system.ws?.connecting && wsConnectGrace === 0) {
    if (wsStatus === "connecting") wsStatus = "error";
    // Auto-reconnect 10s after drop (server sends history then closes)
    if (wifi?.connected && wsReconnectTimer === 0 && wsStatus !== "connecting") {
      wsReconnectTimer = 600; // ~10s
      wsStatus = "connecting";
      wsConnectGrace = 180;
      system.ws?.connect("wss://chat-system.aesthetic.computer/");
    }
  }

  // Process incoming WebSocket chat messages (real-time)
  const wsMsgs = system.ws?.messages;
  if (wsMsgs?.length) {
    for (const raw of wsMsgs) {
      try {
        const msg = JSON.parse(raw);
        // content may be a string (needs parsing) or already an object
        const parseContent = (c) => (typeof c === "string" ? JSON.parse(c) : c);
        if (msg.type === "connected") {
          wsStatus = "connected"; // lock in even if server closes right after
          const content = parseContent(msg.content);
          const last = (content?.messages || []).slice(-1)[0];
          if (last?.from && last?.text) {
            acMsg = { from: last.from, text: last.text };
            if (!chatMuted) sound?.speak(last.from + ": " + last.text);
          }
        } else if (msg.type === "message") {
          const m = parseContent(msg.content);
          if (m?.from && m?.text) {
            acMsg = { from: m.from, text: m.text };
            if (!chatMuted) sound?.speak(m.from + ": " + m.text);
          }
        } else if (msg.from && msg.text) {
          acMsg = { from: msg.from, text: msg.text };
          if (!chatMuted) sound?.speak(msg.from + ": " + msg.text);
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
    // Build candidate list: AC hotspot first, then fallbacks, then user-saved creds
    const acCred = { ssid: AC_SSID, pass: AC_PASS };
    const others = savedCreds.filter((c) => c.ssid !== AC_SSID && !FALLBACK_WIFI.find((f) => f.ssid === c.ssid));
    const candidates = [acCred, ...FALLBACK_WIFI, ...others];
    const cred = candidates[Math.floor(autoConnectFrame / 300) % candidates.length];
    autoConnectTry++;
    // Soft beep on 1st, 2nd, 3rd attempt so the user knows it's trying
    if (autoConnectTry <= 3) {
      sound?.synth({ type: "sine", tone: 440 + (autoConnectTry - 1) * 110,
                     duration: 0.06, volume: 0.15, attack: 0.01, decay: 0.05 });
    }
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
    const wifiHovered = hoverX > w - 22 && hoverY < topBarH;
    if (wifiHovered) { ink(255, 255, 255, 30); box(w - 22, 0, 22, topBarH, true); }
    // Draw antenna icon with appropriate color
    if (wifiConnected) {
      ink(wifiHovered ? 120 : 80, 200, wifiHovered ? 120 : 80); // solid green when connected
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
  // TTS when battery percent changes
  if (batPct >= 0 && batPct !== lastBatPercent) {
    if (lastBatPercent >= 0) {
      // Battery change: low = descending minor third, high = ascending major third
      const charging = bat?.charging;
      const tone = charging ? 660 : (batPct < 20 ? 330 : 523);
      const tone2 = charging ? 880 : (batPct < 20 ? 277 : 659);
      sound?.synth({ type: "sine", tone, duration: 0.12, volume: 0.2, attack: 0.005, decay: 0.1 });
      sound?.synth({ type: "sine", tone: tone2, duration: 0.12, volume: 0.15, attack: 0.03, decay: 0.09 });
    }
    lastBatPercent = batPct;
  }
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

  // Time (LA) — uses syncedNow() for accuracy; shows green dot when synced
  {
    const now = new Date(syncedNow());
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
    // Green sync dot(s) after time when synced
    if (clockSynced) {
      ink(60, 220, 80);
      const dotX = rx + timeStr.length * CH + 1;
      box(dotX, barY + 3, 2, 2, true);
      if (Math.abs(clockOffset) < 500) box(dotX + 3, barY + 3, 2, 2, true); // 2 dots = very precise
    }
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

  // Fullscreen waveform behind everything (drawn here, before grids)
  const wf = sound?.speaker?.waveforms?.left;
  if (wf && activeCount > 0) {
    const wfTop = topBarH;
    const wfH = h - wfTop;
    ink(dark ? 60 : 160, dark ? 60 : 160, dark ? 60 : 160, 60);
    for (let i = 1; i < 120; i++) {
      const x0 = Math.floor((i - 1) * w / 120);
      const x1 = Math.floor(i * w / 120);
      const mid = wfTop + Math.floor(wfH / 2);
      const amp = Math.floor(wfH * 0.45);
      const y0 = mid - Math.round((wf[i - 1] || 0) * amp);
      const y1 = mid - Math.round((wf[i] || 0) * amp);
      line(x0, y0, x1, y1);
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
        const isHovered = hoverX >= x && hoverX < x + btnW && hoverY >= y && hoverY < y + btnH;

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
          if (isHovered) {
            ink(255, 255, 255, 30);
            box(x, y, btnW, btnH, true);
          }
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

  // === SLIDERS: echo and pitch (directly under status bar) ===
  const settingsY = topBarH + 1;

  // Echo slider
  {
    const sliderY = settingsY;
    const sliderH = 12;
    const echoHovered = hoverY >= sliderY && hoverY < sliderY + sliderH;
    ink(dark ? (echoHovered ? 40 : 25) : (echoHovered ? 220 : 235),
        dark ? (echoHovered ? 40 : 25) : (echoHovered ? 220 : 235),
        dark ? (echoHovered ? 45 : 28) : (echoHovered ? 225 : 238));
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
    write(echoStr, { x: 2, y: sliderY + 2, size: 1, font: "font_1" });
    if (trackpadFX) {
      ink(120, 220, 120);
      write("\\", { x: w - 8, y: sliderY + 2, size: 1, font: "font_1" });
    }
  }

  // Pitch shift slider
  {
    const sliderY = settingsY + 12;
    const sliderH = 12;
    const pitchHovered = hoverY >= sliderY && hoverY < sliderY + sliderH;
    ink(dark ? (pitchHovered ? 40 : 25) : (pitchHovered ? 220 : 235),
        dark ? (pitchHovered ? 40 : 25) : (pitchHovered ? 220 : 235),
        dark ? (pitchHovered ? 45 : 28) : (pitchHovered ? 225 : 238));
    box(0, sliderY, w, sliderH, true);
    const centerX = Math.floor(w / 2);
    ink(dark ? 40 : 220, dark ? 40 : 220, dark ? 45 : 225);
    box(centerX, sliderY, 1, sliderH, true);
    const pitchX = Math.floor((pitchShift + 1) / 2 * w);
    if (Math.abs(pitchShift) > 0.005) {
      ink(200, 100, 160, trackpadFX ? 240 : 180);
      const fx = Math.min(centerX, pitchX);
      const fw = Math.abs(pitchX - centerX);
      box(fx, sliderY, fw, sliderH, true);
    }
    if (Math.abs(pitchShift) > 0.005) {
      ink(255, 140, 180, 220);
      box(Math.max(1, Math.min(w - 3, pitchX)) - 1, sliderY, 3, sliderH, true);
    }
    ink(dark ? 90 : 160, dark ? 90 : 160, dark ? 100 : 170);
    const cents = Math.round(pitchShift * 1200);
    const pitchStr = "pitch " + (cents >= 0 ? "+" : "") + cents + "c";
    write(pitchStr, { x: 2, y: sliderY + 2, size: 1, font: "font_1" });
  }

  // === WAVE TYPE BUTTONS (below sliders, modular GUI) ===
  {
    const waveRowY = settingsY + 24;
    const waveRowH = 14;
    const waveLabels = ["sin", "tri", "saw", "sq", "ns"];
    const octBtnW = 22;                           // octave button on right
    const waveAreaW = w - octBtnW - 1;
    const btnW2 = Math.floor(waveAreaW / wavetypes.length);

    // Draw each wave button
    for (let i = 0; i < wavetypes.length; i++) {
      const bx = i * btnW2;
      const isActive = wave === wavetypes[i];
      const isHov = hoverX >= bx && hoverX < bx + btnW2 && hoverY >= waveRowY && hoverY < waveRowY + waveRowH;
      if (isActive) {
        ink(dark ? 55 : 200, dark ? 75 : 210, dark ? 110 : 230);
        box(bx, waveRowY, btnW2, waveRowH, true);
        ink(dark ? 200 : 30, dark ? 220 : 40, dark ? 255 : 60);
      } else if (isHov) {
        ink(dark ? 35 : 210, dark ? 35 : 210, dark ? 40 : 215);
        box(bx, waveRowY, btnW2, waveRowH, true);
        ink(dark ? 160 : 80, dark ? 160 : 80, dark ? 170 : 90);
      } else {
        ink(dark ? 20 : 225, dark ? 20 : 225, dark ? 25 : 230);
        box(bx, waveRowY, btnW2, waveRowH, true);
        ink(dark ? 100 : 140, dark ? 100 : 140, dark ? 110 : 150);
      }
      // Separator line between buttons
      if (i > 0) {
        ink(dark ? 40 : 190, dark ? 40 : 190, dark ? 45 : 195);
        box(bx, waveRowY, 1, waveRowH, true);
      }
      // Label
      const lx = bx + Math.floor((btnW2 - waveLabels[i].length * 6) / 2);
      write(waveLabels[i], { x: lx, y: waveRowY + 3, size: 1, font: "font_1" });
    }

    // Octave button (right side)
    const obx = w - octBtnW;
    const octHov = hoverX >= obx && hoverY >= waveRowY && hoverY < waveRowY + waveRowH;
    ink(dark ? (octHov ? 50 : 28) : (octHov ? 200 : 225),
        dark ? (octHov ? 50 : 28) : (octHov ? 200 : 225),
        dark ? (octHov ? 55 : 32) : (octHov ? 205 : 230));
    box(obx, waveRowY, octBtnW, waveRowH, true);
    ink(dark ? 40 : 190, dark ? 40 : 190, dark ? 45 : 195);
    box(obx, waveRowY, 1, waveRowH, true);
    ink(dark ? 140 : 100, dark ? 140 : 100, dark ? 150 : 110);
    write("o:" + octave, { x: obx + 3, y: waveRowY + 3, size: 1, font: "font_1" });

    // Expose hit zones for act()
    globalThis.__waveButtons = { y: waveRowY, h: waveRowH, btnW: btnW2, octX: obx, octW: octBtnW };

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
    write("enter password:", { x: 20, y: h / 2 - 24, size: 2, font: "font_1" });

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
      } else if (system.ws?.connecting) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("connecting to chat...", { x: 20, y: h - 16, size: 1, font: "font_1" });
      } else if (system.ws?.connected) {
        ink(FG_MUTED, FG_MUTED, FG_MUTED);
        write("loading chat...", { x: 20, y: h - 16, size: 1, font: "font_1" });
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

  // Passive chat message (main screen, bottom center — hidden by WiFi overlays)
  if (!wifiPanelOpen && !wifiPasswordMode && acMsg && wifi?.connected) {
    ink(FG_DIM, FG_DIM, FG_DIM, 180);
    const preview = (acMsg.from + ": " + acMsg.text).slice(0, 52);
    write(preview, { x: 4, y: h - 10, size: 1, font: "font_1" });
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

