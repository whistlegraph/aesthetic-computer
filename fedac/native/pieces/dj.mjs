// dj.mjs — DJ deck piece for AC Native
// Two decks with turntable interface, crossfader, auto-scan, hot-plug USB, TTS.
// Audio continues when jumping to notepat or other pieces.

const MUSIC_DIR = "/media";
const FALLBACK_DIRS = ["/mnt/samples", "/mnt", "/media"];
const AUDIO_EXTS = new Set(["mp3", "wav", "flac", "ogg", "aac", "m4a", "opus", "wma"]);

// State
let files = [];           // all discovered audio files [{path, name, size}]
let queue = [];           // upcoming tracks (indices into files[])
let queueIdx = 0;
let activeDeck = 0;       // 0 = A, 1 = B
let scratching = [false, false]; // per-deck scratch mode
let scratchPos = [0, 0];  // scratch position (virtual "angle")
let crossfader = 0.5;
let mounted = false;
let message = "";
let messageFrame = 0;
let frame = 0;
let lastUsbCheck = 0;
let usbConnected = false;
let T = __theme.update();
let view = "decks";       // "decks" | "browser" | "queue"
let browserPath = "";
let browserFiles = [];
let browserIdx = 0;
let browserScroll = 0;

function isAudioFile(name) {
  const dot = name.lastIndexOf(".");
  return dot >= 0 && AUDIO_EXTS.has(name.slice(dot + 1).toLowerCase());
}

// Recursively scan a directory for audio files
function scanDir(system, path, results, depth) {
  if (depth > 5) return; // safety limit
  const listing = system?.listDir?.(path);
  if (!listing) return;
  for (const f of listing) {
    const full = path + "/" + f.name;
    if (f.isDir && !f.name.startsWith(".")) {
      scanDir(system, full, results, depth + 1);
    } else if (isAudioFile(f.name)) {
      results.push({ path: full, name: f.name, size: f.size || 0 });
    }
  }
}

function autoScan(system, tts) {
  files = [];
  // Try music dir first, then fallbacks
  const dirs = mounted ? [MUSIC_DIR, ...FALLBACK_DIRS] : FALLBACK_DIRS;
  for (const dir of dirs) {
    scanDir(system, dir, files, 0);
  }
  // Sort by name
  files.sort((a, b) => a.name.localeCompare(b.name));

  // Build queue from all files
  queue = files.map((_, i) => i);
  queueIdx = 0;

  if (tts) {
    if (files.length > 0) {
      tts.speak(`Found ${files.length} tracks`);
    } else {
      tts.speak("No tracks found");
    }
  }
  showMsg(files.length > 0 ? `${files.length} tracks found` : "No tracks");
}

function showMsg(text) {
  message = text;
  messageFrame = frame;
}

function fmt(secs) {
  if (!secs || secs < 0) return "0:00";
  const m = Math.floor(secs / 60);
  const s = Math.floor(secs % 60);
  return `${m}:${s < 10 ? "0" : ""}${s}`;
}

// Load next track from queue into deck
function loadNext(sound, deck, tts) {
  if (files.length === 0) return;
  if (queueIdx >= queue.length) queueIdx = 0; // loop
  const f = files[queue[queueIdx]];
  if (f) {
    const ok = sound?.deck?.load(deck, f.path);
    if (ok) {
      showMsg(`${deck ? "B" : "A"}: ${f.name}`);
      if (tts) tts.speak(`Deck ${deck ? "B" : "A"}, ${f.name.replace(/\.[^.]+$/, "")}`);
      queueIdx++;
    }
  }
}

// Check USB hot-plug
function checkUsb(system, tts) {
  const nowMounted = system?.mountMusic?.() || false;
  if (nowMounted && !usbConnected) {
    // USB plugged in
    usbConnected = true;
    mounted = true;
    if (tts) tts.speak("USB connected, scanning tracks");
    autoScan(system, tts);
  } else if (!nowMounted && usbConnected) {
    // USB removed
    usbConnected = false;
    if (tts) tts.speak("USB removed");
    showMsg("USB removed");
  }
}

function boot({ system, sound, tts }) {
  mounted = system?.mountMusic?.() || false;
  usbConnected = mounted;
  autoScan(system, tts);

  // If decks already playing (returned from another piece), don't reset
  const decks = sound?.deck?.decks;
  if (decks?.[0]?.loaded || decks?.[1]?.loaded) {
    showMsg("Decks resumed");
    if (tts) tts.speak("Decks resumed");
  } else if (files.length > 0) {
    // Auto-load first two tracks
    loadNext(sound, 0, tts);
    loadNext(sound, 1, null); // silent for deck B
  }
}

function act({ event: e, sound, system, tts }) {
  if (!e.is("keyboard:down")) return;
  const dk = sound?.deck;
  const decks = dk?.decks || [{}, {}];

  // --- Global ---
  if (e.is("keyboard:down:escape")) { system?.jump?.("prompt"); return; }

  // Tab: switch active deck
  if (e.is("keyboard:down:tab")) {
    activeDeck = activeDeck === 0 ? 1 : 0;
    sound?.synth?.({ type: "sine", tone: activeDeck ? 880 : 660, duration: 0.04, volume: 0.06 });
    return;
  }

  // V: switch view (decks / browser / queue)
  if (e.is("keyboard:down:v")) {
    view = view === "decks" ? "browser" : view === "browser" ? "queue" : "decks";
    return;
  }

  // R: rescan tracks
  if (e.is("keyboard:down:r")) {
    autoScan(system, tts);
    return;
  }

  // --- Deck controls ---
  // Space: play/pause active deck
  if (e.is("keyboard:down:space")) {
    const d = decks[activeDeck];
    if (d?.loaded) {
      if (d.playing) { dk.pause(activeDeck); showMsg(`${activeDeck ? "B" : "A"} paused`); }
      else { dk.play(activeDeck); showMsg(`${activeDeck ? "B" : "A"} playing`); }
    }
    return;
  }

  // Q/W: play/pause deck A/B directly
  if (e.is("keyboard:down:q")) {
    const d = decks[0];
    if (d?.loaded) { if (d.playing) dk.pause(0); else dk.play(0); }
    return;
  }
  if (e.is("keyboard:down:w")) {
    const d = decks[1];
    if (d?.loaded) { if (d.playing) dk.pause(1); else dk.play(1); }
    return;
  }

  // N: load next track into active deck
  if (e.is("keyboard:down:n")) {
    loadNext(sound, activeDeck, tts);
    return;
  }

  // S: toggle scratch mode for active deck
  if (e.is("keyboard:down:s")) {
    scratching[activeDeck] = !scratching[activeDeck];
    showMsg(`Scratch ${scratching[activeDeck] ? "ON" : "OFF"} (${activeDeck ? "B" : "A"})`);
    return;
  }

  // --- Scratch / Seek ---
  if (e.is("keyboard:down:arrowleft")) {
    const d = decks[activeDeck];
    if (d?.loaded) {
      if (scratching[activeDeck]) {
        // Scratch: small backward jumps
        dk.seek(activeDeck, Math.max(0, d.position - 0.1));
        scratchPos[activeDeck] -= 5;
      } else {
        dk.seek(activeDeck, Math.max(0, d.position - 5));
      }
    }
    return;
  }
  if (e.is("keyboard:down:arrowright")) {
    const d = decks[activeDeck];
    if (d?.loaded) {
      if (scratching[activeDeck]) {
        dk.seek(activeDeck, Math.min(d.duration, d.position + 0.1));
        scratchPos[activeDeck] += 5;
      } else {
        dk.seek(activeDeck, Math.min(d.duration, d.position + 5));
      }
    }
    return;
  }

  // --- Crossfader ---
  if (e.is("keyboard:down:[")) {
    crossfader = Math.max(0, crossfader - 0.05);
    dk?.setCrossfader(crossfader);
    return;
  }
  if (e.is("keyboard:down:]")) {
    crossfader = Math.min(1, crossfader + 0.05);
    dk?.setCrossfader(crossfader);
    return;
  }

  // --- Speed / Pitch ---
  if (e.is("keyboard:down:z")) {
    const d = decks[activeDeck];
    if (d?.loaded) dk.setSpeed(activeDeck, Math.max(0.5, d.speed - 0.05));
    return;
  }
  if (e.is("keyboard:down:x")) {
    const d = decks[activeDeck];
    if (d?.loaded) dk.setSpeed(activeDeck, Math.min(2.0, d.speed + 0.05));
    return;
  }

  // --- Volume ---
  if (e.is("keyboard:down:-")) {
    const d = decks[activeDeck];
    if (d) dk.setVolume(activeDeck, Math.max(0, d.volume - 0.05));
    return;
  }
  if (e.is("keyboard:down:=")) {
    const d = decks[activeDeck];
    if (d) dk.setVolume(activeDeck, Math.min(1, d.volume + 0.05));
    return;
  }

  // --- Browser view navigation ---
  if (view === "browser") {
    if (e.is("keyboard:down:arrowdown")) {
      if (browserFiles.length > 0) browserIdx = Math.min(browserIdx + 1, browserFiles.length - 1);
      return;
    }
    if (e.is("keyboard:down:arrowup")) {
      if (browserFiles.length > 0) browserIdx = Math.max(browserIdx - 1, 0);
      return;
    }
    if (e.is("keyboard:down:enter")) {
      const sel = browserFiles[browserIdx];
      if (sel?.isDir) {
        browserPath = browserPath + "/" + sel.name;
        browseCurrent(system);
      } else if (sel) {
        const ok = dk?.load(activeDeck, browserPath + "/" + sel.name);
        if (ok) showMsg(`Loaded: ${sel.name}`);
      }
      return;
    }
    if (e.is("keyboard:down:backspace")) {
      browserPath = browserPath.replace(/\/[^/]*$/, "") || "/";
      browseCurrent(system);
      return;
    }
  }

  // --- Queue view ---
  if (view === "queue") {
    if (e.is("keyboard:down:arrowdown")) {
      if (queue.length > 0) queueIdx = Math.min(queueIdx + 1, queue.length - 1);
      return;
    }
    if (e.is("keyboard:down:arrowup")) {
      if (queue.length > 0) queueIdx = Math.max(queueIdx - 1, 0);
      return;
    }
  }
}

function browseCurrent(system) {
  const listing = system?.listDir?.(browserPath);
  browserFiles = (listing || [])
    .filter(f => f.isDir || isAudioFile(f.name))
    .sort((a, b) => {
      if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
      return a.name.localeCompare(b.name);
    });
  browserIdx = 0;
  browserScroll = 0;
}

function paint({ wipe, ink, box, line, write, circle, screen, sound }) {
  frame++;
  T = __theme.update();
  const w = screen.width, h = screen.height;
  const P = 4; // padding
  const F = "font_1";
  const CW = 6;
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const dk = sound?.deck;
  const decks = dk?.decks || [{}, {}];
  const cf = crossfader;

  // --- Turntable decks ---
  const deckW = Math.floor((w - P * 3) / 2);
  const deckH = Math.min(120, Math.floor(h * 0.35));

  const drawDeck = (d, idx, x0) => {
    const isActive = idx === activeDeck;
    const label = idx === 0 ? "A" : "B";

    // Border
    const dim = T.fgMute || 60;
    ink(isActive ? dim : dim - 30, isActive ? dim + 5 : dim - 28, isActive ? dim + 20 : dim - 20);
    box(x0 - 1, P - 1, deckW + 2, deckH + 2);
    ink(T.bg[0] + 4, T.bg[1] + 4, T.bg[2] + 8);
    box(x0, P, deckW, deckH);

    // Label
    const fg = T.fg || 220;
    ink(isActive ? fg : dim, isActive ? fg : dim - 20, isActive ? fg - 120 : dim - 10);
    write(label, { x: x0 + 2, y: P + 2, size: 1, font: "matrix" });

    if (!d.loaded) {
      ink(dim, dim, dim + 10);
      write("empty", { x: x0 + 14, y: P + 3, size: 1, font: F });
      write("N=load next", { x: x0 + 4, y: P + deckH - 12, size: 1, font: F });
      return;
    }

    // Turntable platter (circle)
    const cx = x0 + deckW - 30;
    const cy = P + 30;
    const r = 20;
    ink(25, 25, 40);
    circle(cx, cy, r, true);
    ink(isActive ? 60 : 35, isActive ? 60 : 35, isActive ? 80 : 50);
    circle(cx, cy, r, false);

    // Spinning indicator (rotates with position)
    const angle = (d.position || 0) * 3;
    const nx = cx + Math.cos(angle) * (r - 4);
    const ny = cy + Math.sin(angle) * (r - 4);
    ink(d.playing ? 100 : 60, d.playing ? 255 : 120, d.playing ? 100 : 60);
    circle(Math.floor(nx), Math.floor(ny), 2, true);

    // Scratch indicator
    if (scratching[idx]) {
      ink(255, 80, 80);
      write("SCR", { x: cx - 9, y: cy + r + 3, size: 1, font: F });
    }

    // Title
    const maxChars = Math.floor((deckW - 65) / CW);
    ink(fg, fg, fg + 20);
    const title = (d.title || "?").replace(/\.[^.]+$/, "");
    write(title.slice(0, maxChars), { x: x0 + 2, y: P + 14, size: 1, font: F });

    // Play state
    ink(d.playing ? 60 : 180, d.playing ? 220 : 180, d.playing ? 60 : 60);
    write(d.playing ? "PLAY" : "STOP", { x: x0 + 2, y: P + 26, size: 1, font: F });

    // Progress bar
    const barY = P + 38;
    const barW = deckW - 4;
    const progress = d.duration > 0 ? d.position / d.duration : 0;
    ink(25, 25, 40);
    box(x0 + 2, barY, barW, 4);
    ink(d.playing ? 60 : 40, d.playing ? 180 : 100, d.playing ? 60 : 40);
    box(x0 + 2, barY, Math.max(1, Math.floor(barW * progress)), 4);

    // Time
    ink(140, 140, 160);
    write(`${fmt(d.position)}/${fmt(d.duration)}`, { x: x0 + 2, y: barY + 7, size: 1, font: F });

    // Speed
    ink(100, 100, 120);
    const spd = `${(d.speed || 1).toFixed(2)}x`;
    write(spd, { x: x0 + deckW - spd.length * CW - 4, y: barY + 7, size: 1, font: F });

    // Volume bar
    const volY = barY + 20;
    ink(40, 40, 60);
    box(x0 + 2, volY, barW, 3);
    ink(100, 100, 200);
    box(x0 + 2, volY, Math.floor(barW * (d.volume || 1)), 3);
    ink(80, 80, 100);
    write("vol", { x: x0 + 2, y: volY + 5, size: 1, font: F });
  };

  drawDeck(decks[0], 0, P);
  drawDeck(decks[1], 1, P * 2 + deckW);

  // --- Crossfader ---
  const cfY = P + deckH + 6;
  const cfW = w - P * 2;
  ink(20, 20, 35);
  box(P, cfY, cfW, 5);
  const cfPos = Math.floor(cfW * cf);
  ink(255, 200, 60);
  box(P + cfPos - 3, cfY - 2, 7, 9);
  ink(80, 80, 100);
  write("A", { x: P, y: cfY + 7, size: 1, font: F });
  write("[ ]", { x: Math.floor(w / 2) - 9, y: cfY + 7, size: 1, font: F });
  write("B", { x: w - P - CW, y: cfY + 7, size: 1, font: F });

  // --- Lower section (view-dependent) ---
  const lowerY = cfY + 22;
  ink(25, 25, 40);
  line(0, lowerY - 2, w, lowerY - 2);

  if (view === "decks") {
    // Track queue preview
    ink(100, 100, 140);
    write("QUEUE", { x: P, y: lowerY, size: 1, font: F });
    ink(60, 60, 80);
    write(`${files.length} tracks`, { x: P + 42, y: lowerY, size: 1, font: F });

    const rowH = 11;
    const maxRows = Math.floor((h - lowerY - 24) / rowH);
    for (let i = 0; i < maxRows && queueIdx + i < queue.length; i++) {
      const fi = files[queue[queueIdx + i]];
      if (!fi) continue;
      const y = lowerY + 12 + i * rowH;
      const isCur = i === 0;
      if (isCur) {
        ink(20, 25, 35);
        box(0, y - 1, w, rowH);
      }
      ink(isCur ? 255 : 120, isCur ? 255 : 120, isCur ? 200 : 140);
      write(fi.name.replace(/\.[^.]+$/, "").slice(0, Math.floor(w / CW) - 2),
            { x: P, y, size: 1, font: F });
    }
  } else if (view === "browser") {
    ink(100, 100, 140);
    write("BROWSE: " + (browserPath || "/"), { x: P, y: lowerY, size: 1, font: F });

    const rowH = 11;
    const maxRows = Math.floor((h - lowerY - 24) / rowH);
    if (browserIdx < browserScroll) browserScroll = browserIdx;
    if (browserIdx >= browserScroll + maxRows) browserScroll = browserIdx - maxRows + 1;

    for (let i = 0; i < maxRows && i + browserScroll < browserFiles.length; i++) {
      const fi = browserFiles[i + browserScroll];
      const y = lowerY + 12 + i * rowH;
      const isSel = (i + browserScroll) === browserIdx;
      if (isSel) { ink(20, 25, 35); box(0, y - 1, w, rowH); }
      if (fi.isDir) {
        ink(isSel ? 255 : 120, isSel ? 200 : 100, isSel ? 80 : 60);
        write(`> ${fi.name}/`, { x: P, y, size: 1, font: F });
      } else {
        ink(isSel ? 255 : 160, isSel ? 255 : 160, isSel ? 255 : 180);
        write(fi.name.slice(0, Math.floor(w / CW) - 2), { x: P + CW, y, size: 1, font: F });
      }
    }
  } else if (view === "queue") {
    ink(100, 100, 140);
    write(`QUEUE (${queue.length} tracks)`, { x: P, y: lowerY, size: 1, font: F });

    const rowH = 11;
    const maxRows = Math.floor((h - lowerY - 24) / rowH);
    for (let i = 0; i < maxRows && i < queue.length; i++) {
      const fi = files[queue[i]];
      if (!fi) continue;
      const y = lowerY + 12 + i * rowH;
      const isCur = i === queueIdx;
      if (isCur) { ink(20, 25, 35); box(0, y - 1, w, rowH); }
      ink(isCur ? 255 : 120, isCur ? 200 : 120, isCur ? 80 : 140);
      write(`${i + 1}. ${fi.name.replace(/\.[^.]+$/, "").slice(0, Math.floor(w / CW) - 6)}`,
            { x: P, y, size: 1, font: F });
    }
  }

  // --- Status bar ---
  const sY = h - 11;
  ink(12, 12, 22);
  box(0, sY - 1, w, 12);
  ink(100, 100, 120);
  const dk_l = activeDeck === 0 ? "A" : "B";
  const help = `${dk_l} Spc:play N:next S:scr V:view [:xf Tab:deck Esc:exit`;
  write(help.slice(0, Math.floor(w / CW)), { x: P, y: sY, size: 1, font: F });

  // USB indicator
  ink(usbConnected ? 60 : 40, usbConnected ? 200 : 40, usbConnected ? 60 : 40);
  write(usbConnected ? "USB" : "---", { x: w - 22, y: sY, size: 1, font: F });

  // --- Message toast ---
  if (message && frame - messageFrame < 120) {
    const fade = Math.max(0, 255 - Math.floor((frame - messageFrame) * 2.5));
    ink(255, 220, 60, fade);
    write(message, { x: P, y: cfY - 10, size: 1, font: F });
  }
}

function sim({ system, tts, sound }) {
  // USB hot-plug check every 2 seconds
  if (frame - lastUsbCheck > 120) {
    lastUsbCheck = frame;
    checkUsb(system, tts);
  }

  // Auto-advance: when a deck finishes, load next from queue
  const decks = sound?.deck?.decks || [{}, {}];
  for (let i = 0; i < 2; i++) {
    const d = decks[i];
    if (d?.loaded && !d.playing && d.position >= d.duration - 0.1 && d.duration > 0) {
      loadNext(sound, i, tts);
      sound?.deck?.play(i);
    }
  }
}

function leave() {
  // Audio keeps playing — intentional!
}

export { boot, act, paint, sim, leave };
