// dj.mjs — DJ deck piece for AC Native
// Two decks with crossfader, file browser, and persistent audio playback.
// Audio continues when jumping to notepat or other pieces.

const MUSIC_DIR = "/media";
const FALLBACK_DIR = "/mnt/samples";
let files = [];         // current directory listing [{name, isDir, size}]
let currentPath = "";   // current browsing path
let selectedIdx = 0;
let scrollOffset = 0;
let activeDeck = 0;     // 0 = A, 1 = B
let mounted = false;
let message = "";
let messageFrame = 0;
let frame = 0;

// Audio file extensions
const AUDIO_EXTS = new Set(["mp3", "wav", "flac", "ogg", "aac", "m4a", "opus", "wma"]);

function isAudioFile(name) {
  const dot = name.lastIndexOf(".");
  if (dot < 0) return false;
  return AUDIO_EXTS.has(name.slice(dot + 1).toLowerCase());
}

function browseDir(system, path) {
  const listing = system?.listDir?.(path);
  if (!listing) {
    files = [];
    return;
  }
  // Sort: directories first, then audio files, alphabetically
  files = listing
    .filter(f => f.isDir || isAudioFile(f.name))
    .sort((a, b) => {
      if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
      return a.name.localeCompare(b.name);
    });
  currentPath = path;
  selectedIdx = 0;
  scrollOffset = 0;
}

function showMsg(text) {
  message = text;
  messageFrame = frame;
}

function formatTime(secs) {
  if (!secs || secs < 0) return "0:00";
  const m = Math.floor(secs / 60);
  const s = Math.floor(secs % 60);
  return `${m}:${s < 10 ? "0" : ""}${s}`;
}

function formatSize(bytes) {
  if (bytes < 1024) return bytes + "B";
  if (bytes < 1048576) return (bytes / 1024).toFixed(0) + "K";
  return (bytes / 1048576).toFixed(1) + "M";
}

function boot({ system, sound }) {
  // Try to mount music USB
  mounted = system?.mountMusic?.() || false;

  // Browse music dir or fallback
  if (mounted) {
    browseDir(system, MUSIC_DIR);
    showMsg("Music USB mounted");
  } else {
    browseDir(system, FALLBACK_DIR);
    showMsg("No USB found — browsing samples");
  }

  // If decks already playing (returned from another piece), don't reset
  const decks = sound?.deck?.decks;
  if (decks?.[0]?.loaded || decks?.[1]?.loaded) {
    showMsg("Decks resumed");
  }
}

function act({ event: e, sound, system }) {
  if (!e.is("keyboard:down")) return;

  // Exit (audio persists!)
  if (e.is("keyboard:down:escape")) {
    system?.jump?.("prompt");
    return;
  }

  // Deck selection
  if (e.is("keyboard:down:tab")) {
    activeDeck = activeDeck === 0 ? 1 : 0;
    sound?.synth?.({ type: "sine", tone: activeDeck ? 880 : 660, duration: 0.04, volume: 0.06 });
    return;
  }

  // Play/pause
  if (e.is("keyboard:down:space")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) {
      if (d.playing) {
        sound.deck.pause(activeDeck);
        showMsg(`Deck ${activeDeck ? "B" : "A"} paused`);
      } else {
        sound.deck.play(activeDeck);
        showMsg(`Deck ${activeDeck ? "B" : "A"} playing`);
      }
    }
    return;
  }

  // Load file into active deck
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    if (files.length === 0) return;
    const sel = files[selectedIdx];
    if (!sel) return;
    const fullPath = currentPath + "/" + sel.name;
    if (sel.isDir) {
      browseDir(system, fullPath);
      sound?.synth?.({ type: "sine", tone: 550, duration: 0.03, volume: 0.06 });
    } else {
      const ok = sound?.deck?.load(activeDeck, fullPath);
      if (ok) {
        showMsg(`Loaded -> Deck ${activeDeck ? "B" : "A"}: ${sel.name}`);
        sound?.synth?.({ type: "sine", tone: 880, duration: 0.06, volume: 0.08 });
      } else {
        showMsg(`Failed to load: ${sel.name}`);
        sound?.synth?.({ type: "square", tone: 200, duration: 0.1, volume: 0.08 });
      }
    }
    return;
  }

  // Navigate file browser
  if (e.is("keyboard:down:arrowdown")) {
    if (files.length > 0) selectedIdx = Math.min(selectedIdx + 1, files.length - 1);
    sound?.synth?.({ type: "sine", tone: 440, duration: 0.02, volume: 0.04 });
    return;
  }
  if (e.is("keyboard:down:arrowup")) {
    if (files.length > 0) selectedIdx = Math.max(selectedIdx - 1, 0);
    sound?.synth?.({ type: "sine", tone: 480, duration: 0.02, volume: 0.04 });
    return;
  }

  // Go up directory
  if (e.is("keyboard:down:backspace")) {
    const parent = currentPath.replace(/\/[^/]*$/, "") || "/";
    if (parent !== currentPath) {
      browseDir(system, parent);
      sound?.synth?.({ type: "sine", tone: 330, duration: 0.04, volume: 0.06 });
    }
    return;
  }

  // Seek
  if (e.is("keyboard:down:arrowleft")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.seek(activeDeck, Math.max(0, d.position - 5));
    return;
  }
  if (e.is("keyboard:down:arrowright")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.seek(activeDeck, Math.min(d.duration, d.position + 5));
    return;
  }

  // Crossfader
  if (e.is("keyboard:down:[")) {
    const cf = Math.max(0, (sound?.deck?.crossfaderValue || 0.5) - 0.05);
    sound?.deck?.setCrossfader(cf);
    return;
  }
  if (e.is("keyboard:down:]")) {
    const cf = Math.min(1, (sound?.deck?.crossfaderValue || 0.5) + 0.05);
    sound?.deck?.setCrossfader(cf);
    return;
  }

  // Volume
  if (e.is("keyboard:down:-")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d) sound.deck.setVolume(activeDeck, Math.max(0, d.volume - 0.05));
    return;
  }
  if (e.is("keyboard:down:=")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d) sound.deck.setVolume(activeDeck, Math.min(1, d.volume + 0.05));
    return;
  }

  // Speed
  if (e.is("keyboard:down:z")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.setSpeed(activeDeck, Math.max(0.5, d.speed - 0.05));
    return;
  }
  if (e.is("keyboard:down:x")) {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.setSpeed(activeDeck, Math.min(2.0, d.speed + 0.05));
    return;
  }

  // Quick play: Q = deck A, W = deck B
  if (e.is("keyboard:down:q")) {
    const d = sound?.deck?.decks?.[0];
    if (d?.loaded) { if (d.playing) sound.deck.pause(0); else sound.deck.play(0); }
    return;
  }
  if (e.is("keyboard:down:w")) {
    const d = sound?.deck?.decks?.[1];
    if (d?.loaded) { if (d.playing) sound.deck.pause(1); else sound.deck.play(1); }
    return;
  }
}

function paint({ wipe, ink, box, line, write, screen, sound }) {
  frame++;
  const w = screen.width, h = screen.height;
  const pad = 4;
  const F = "font_1";
  const CW = 6; // font_1 char width
  wipe(8, 8, 12);

  const decks = sound?.deck?.decks || [{}, {}];
  const cf = sound?.deck?.crossfaderValue ?? 0.5;
  const deckW = Math.floor((w - pad * 3) / 2);

  // --- Deck A (top-left) ---
  const drawDeck = (dk, d, x0) => {
    const isActive = d === activeDeck;
    const label = d === 0 ? "A" : "B";
    const maxChars = Math.floor((deckW - 4) / CW);

    // Header: label + status
    ink(isActive ? 255 : 100, isActive ? 255 : 80, isActive ? 80 : 60);
    write(label, { x: x0, y: pad, size: 1, font: "matrix" });

    if (!dk.loaded) {
      ink(60, 60, 80);
      write("--", { x: x0 + 12, y: pad, size: 1, font: F });
      return;
    }

    // Playing indicator
    if (dk.playing) {
      ink(60, 220, 60);
      write(">", { x: x0 + 12, y: pad, size: 1, font: F });
    } else {
      ink(180, 180, 60);
      write("=", { x: x0 + 12, y: pad, size: 1, font: F });
    }

    // Title (truncated)
    ink(220, 220, 240);
    write((dk.title || "?").slice(0, maxChars - 3), { x: x0 + 20, y: pad, size: 1, font: F });

    // Progress bar
    const barY = pad + 12;
    const barW = deckW - 4;
    const progress = dk.duration > 0 ? dk.position / dk.duration : 0;
    ink(30, 30, 45);
    box(x0, barY, barW, 4);
    ink(dk.playing ? 60 : 40, dk.playing ? 180 : 100, dk.playing ? 60 : 40);
    box(x0, barY, Math.max(1, Math.floor(barW * progress)), 4);

    // Time + speed
    ink(140, 140, 160);
    const timeTxt = `${formatTime(dk.position)}/${formatTime(dk.duration)}`;
    write(timeTxt, { x: x0, y: barY + 6, size: 1, font: F });
    ink(100, 100, 120);
    const spdTxt = `${dk.speed?.toFixed(2) || "1.00"}x`;
    write(spdTxt, { x: x0 + deckW - spdTxt.length * CW - 4, y: barY + 6, size: 1, font: F });
  };

  drawDeck(decks[0], 0, pad);
  drawDeck(decks[1], 1, pad * 2 + deckW);

  // --- Crossfader (horizontal bar below decks) ---
  const cfY = pad + 28;
  const cfW = w - pad * 2;
  ink(25, 25, 40);
  box(pad, cfY, cfW, 3);
  const cfPos = Math.floor(cfW * cf);
  ink(255, 200, 60);
  box(pad + cfPos - 2, cfY - 1, 5, 5);
  ink(50, 50, 70);
  write("A", { x: pad, y: cfY + 5, size: 1, font: F });
  write("B", { x: w - pad - CW, y: cfY + 5, size: 1, font: F });

  // --- Divider ---
  const divY = cfY + 14;
  ink(30, 30, 50);
  line(0, divY, w, divY);

  // --- File browser ---
  const rowH = 12;
  const headerY = divY + 2;
  ink(100, 100, 140);
  const pathMax = Math.floor(w / CW) - 2;
  const pathStr = currentPath.length > pathMax
    ? "..." + currentPath.slice(-pathMax + 3) : currentPath;
  write(pathStr, { x: pad, y: headerY, size: 1, font: F });

  // Scroll count
  if (files.length > 0) {
    ink(60, 60, 80);
    const si = `${selectedIdx + 1}/${files.length}`;
    write(si, { x: w - si.length * CW - pad, y: headerY, size: 1, font: F });
  }

  const listY = headerY + 12;
  const maxVisible = Math.floor((h - listY - 14) / rowH);

  if (selectedIdx < scrollOffset) scrollOffset = selectedIdx;
  if (selectedIdx >= scrollOffset + maxVisible) scrollOffset = selectedIdx - maxVisible + 1;

  if (files.length === 0) {
    ink(80, 80, 100);
    write("(empty)", { x: pad, y: listY, size: 1, font: F });
  }

  for (let i = 0; i < maxVisible && i + scrollOffset < files.length; i++) {
    const fi = files[i + scrollOffset];
    const y = listY + i * rowH;
    const isSel = (i + scrollOffset) === selectedIdx;

    if (isSel) {
      ink(25, 30, 45);
      box(0, y - 1, w, rowH);
    }

    const nameMax = Math.floor(w / CW) - 6;
    if (fi.isDir) {
      ink(isSel ? 255 : 120, isSel ? 200 : 120, isSel ? 80 : 80);
      write(`> ${fi.name.slice(0, nameMax)}/`, { x: pad, y: y, size: 1, font: F });
    } else {
      ink(isSel ? 255 : 160, isSel ? 255 : 160, isSel ? 255 : 180);
      write(fi.name.slice(0, nameMax), { x: pad + CW, y: y, size: 1, font: F });
      ink(80, 80, 100);
      const sz = formatSize(fi.size);
      write(sz, { x: w - sz.length * CW - pad, y: y, size: 1, font: F });
    }
  }

  // --- Status bar ---
  const sY = h - 11;
  ink(15, 15, 25);
  box(0, sY - 1, w, 12);
  ink(120, 120, 140);
  const dl = activeDeck === 0 ? "A" : "B";
  write(`${dl} Spc:play Tab:deck Esc:exit`, { x: pad, y: sY, size: 1, font: F });

  // --- Message ---
  if (message && frame - messageFrame < 120) {
    const fade = Math.max(0, 255 - Math.floor((frame - messageFrame) * 2.5));
    ink(255, 220, 60, fade);
    write(message, { x: pad, y: divY - 10, size: 1, font: F });
  }
}

function sim() {}

function leave() {
  // Audio keeps playing — this is intentional!
}

export { boot, act, paint, sim, leave };
