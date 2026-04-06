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
  const key = e.key;

  // Exit (audio persists!)
  if (key === "Escape") {
    system?.jump?.("prompt");
    return;
  }

  // Deck selection
  if (key === "Tab") {
    activeDeck = activeDeck === 0 ? 1 : 0;
    sound?.synth({ type: "sine", tone: activeDeck ? 880 : 660, duration: 0.04, volume: 0.06 });
    return;
  }

  // Play/pause
  if (key === " ") {
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
  if (key === "Enter") {
    if (files.length === 0) return;
    const sel = files[selectedIdx];
    if (!sel) return;
    const fullPath = currentPath + "/" + sel.name;
    if (sel.isDir) {
      browseDir(system, fullPath);
      sound?.synth({ type: "sine", tone: 550, duration: 0.03, volume: 0.06 });
    } else {
      const ok = sound?.deck?.load(activeDeck, fullPath);
      if (ok) {
        showMsg(`Loaded → Deck ${activeDeck ? "B" : "A"}: ${sel.name}`);
        sound?.synth({ type: "sine", tone: 880, duration: 0.06, volume: 0.08 });
      } else {
        showMsg(`Failed to load: ${sel.name}`);
        sound?.synth({ type: "noise", tone: 200, duration: 0.1, volume: 0.08 });
      }
    }
    return;
  }

  // Navigate file browser
  if (key === "ArrowDown") {
    if (files.length > 0) selectedIdx = Math.min(selectedIdx + 1, files.length - 1);
    sound?.synth({ type: "sine", tone: 440, duration: 0.02, volume: 0.04 });
    return;
  }
  if (key === "ArrowUp") {
    if (files.length > 0) selectedIdx = Math.max(selectedIdx - 1, 0);
    sound?.synth({ type: "sine", tone: 480, duration: 0.02, volume: 0.04 });
    return;
  }

  // Go up directory
  if (key === "Backspace") {
    const parent = currentPath.replace(/\/[^/]*$/, "") || "/";
    if (parent !== currentPath) {
      browseDir(system, parent);
      sound?.synth({ type: "sine", tone: 330, duration: 0.04, volume: 0.06 });
    }
    return;
  }

  // Seek
  if (key === "ArrowLeft") {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.seek(activeDeck, Math.max(0, d.position - 5));
    return;
  }
  if (key === "ArrowRight") {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.seek(activeDeck, Math.min(d.duration, d.position + 5));
    return;
  }

  // Crossfader
  if (key === "[") {
    const cf = Math.max(0, (sound?.deck?.crossfaderValue || 0.5) - 0.05);
    sound?.deck?.setCrossfader(cf);
    return;
  }
  if (key === "]") {
    const cf = Math.min(1, (sound?.deck?.crossfaderValue || 0.5) + 0.05);
    sound?.deck?.setCrossfader(cf);
    return;
  }

  // Volume
  if (key === "-") {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d) sound.deck.setVolume(activeDeck, Math.max(0, d.volume - 0.05));
    return;
  }
  if (key === "=") {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d) sound.deck.setVolume(activeDeck, Math.min(1, d.volume + 0.05));
    return;
  }

  // Speed
  if (key === "z") {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.setSpeed(activeDeck, Math.max(0.5, d.speed - 0.05));
    return;
  }
  if (key === "x") {
    const d = sound?.deck?.decks?.[activeDeck];
    if (d?.loaded) sound.deck.setSpeed(activeDeck, Math.min(2.0, d.speed + 0.05));
    return;
  }

  // Quick play: Q = deck A, W = deck B
  if (key === "q") {
    const d = sound?.deck?.decks?.[0];
    if (d?.loaded) { if (d.playing) sound.deck.pause(0); else sound.deck.play(0); }
    return;
  }
  if (key === "w") {
    const d = sound?.deck?.decks?.[1];
    if (d?.loaded) { if (d.playing) sound.deck.pause(1); else sound.deck.play(1); }
    return;
  }
}

function paint({ wipe, ink, box, line, write, screen, sound }) {
  frame++;
  const W = screen.width, H = screen.height;
  wipe(10, 10, 14); // dark background

  const decks = sound?.deck?.decks || [{}, {}];
  const cf = sound?.deck?.crossfaderValue ?? 0.5;
  const halfW = Math.floor(W / 2) - 2;

  // --- Draw two decks ---
  for (let d = 0; d < 2; d++) {
    const dk = decks[d];
    const x0 = d === 0 ? 0 : halfW + 4;
    const isActive = d === activeDeck;
    const headerY = 2;

    // Deck header
    ink(isActive ? 255 : 100, isActive ? 255 : 100, isActive ? 100 : 80);
    write(`DECK ${d === 0 ? "A" : "B"}`, [x0 + 2, headerY], 1);

    if (dk.loaded) {
      // Artist / Title
      ink(180, 180, 200);
      if (dk.artist) write(dk.artist.slice(0, 20), [x0 + 2, headerY + 12], 1);
      ink(255, 255, 255);
      write((dk.title || "???").slice(0, 22), [x0 + 2, headerY + 22], 1);

      // Progress bar
      const barY = headerY + 34;
      const barW = halfW - 8;
      const progress = dk.duration > 0 ? dk.position / dk.duration : 0;
      ink(40, 40, 50);
      box(x0 + 4, barY, barW, 6);
      ink(dk.playing ? 80 : 50, dk.playing ? 200 : 120, dk.playing ? 80 : 50);
      box(x0 + 4, barY, Math.floor(barW * progress), 6);

      // Time
      ink(160, 160, 180);
      write(`${formatTime(dk.position)} / ${formatTime(dk.duration)}`, [x0 + 2, barY + 10], 1);

      // Speed + Volume
      ink(120, 120, 150);
      write(`SPD ${dk.speed?.toFixed(2) || "1.00"}x`, [x0 + 2, barY + 22], 1);
      write(`VOL ${Math.round((dk.volume || 0) * 100)}%`, [x0 + halfW / 2, barY + 22], 1);

      // Playing indicator
      if (dk.playing) {
        ink(80, 255, 80);
        write(">>", [x0 + halfW - 16, headerY], 1);
      } else if (dk.finished) {
        ink(200, 100, 100);
        write("END", [x0 + halfW - 22, headerY], 1);
      } else {
        ink(200, 200, 100);
        write("||", [x0 + halfW - 16, headerY], 1);
      }
    } else {
      ink(80, 80, 100);
      write("No track", [x0 + 2, headerY + 22], 1);
    }
  }

  // --- Crossfader ---
  const cfX = halfW;
  const cfY = 2;
  ink(80, 80, 100);
  write("CF", [cfX - 4, cfY], 1);
  // Visual crossfader bar
  const cfBarY = cfY + 12;
  const cfBarH = 50;
  ink(40, 40, 50);
  box(cfX, cfBarY, 3, cfBarH);
  const cfPos = Math.floor(cfBarH * cf);
  ink(255, 200, 80);
  box(cfX - 1, cfBarY + cfPos - 1, 5, 3);

  // --- Divider ---
  ink(40, 40, 60);
  const divY = 72;
  line(0, divY, W, divY);

  // --- File browser ---
  const browserY = divY + 2;
  const lineH = 11;
  const maxVisible = Math.floor((H - browserY - 20) / lineH);

  // Ensure selected item is visible
  if (selectedIdx < scrollOffset) scrollOffset = selectedIdx;
  if (selectedIdx >= scrollOffset + maxVisible) scrollOffset = selectedIdx - maxVisible + 1;

  ink(120, 120, 160);
  write(`${currentPath}/`, [2, browserY], 1);

  if (files.length === 0) {
    ink(100, 100, 120);
    write("(empty)", [4, browserY + lineH], 1);
  }

  for (let i = 0; i < maxVisible && i + scrollOffset < files.length; i++) {
    const fi = files[i + scrollOffset];
    const y = browserY + lineH + i * lineH;
    const isSel = (i + scrollOffset) === selectedIdx;

    if (isSel) {
      ink(30, 30, 50);
      box(0, y - 1, W, lineH);
    }

    if (fi.isDir) {
      ink(isSel ? 255 : 140, isSel ? 220 : 140, isSel ? 100 : 100);
      write(`> ${fi.name}/`, [4, y], 1);
    } else {
      ink(isSel ? 255 : 180, isSel ? 255 : 180, isSel ? 255 : 200);
      const sizeStr = formatSize(fi.size);
      write(`  ${fi.name}`, [4, y], 1);
      ink(100, 100, 120);
      write(sizeStr, [W - sizeStr.length * 6 - 4, y], 1);
    }
  }

  // --- Status bar ---
  const statusY = H - 10;
  ink(20, 20, 30);
  box(0, statusY - 1, W, 12);
  ink(160, 160, 180);
  const deckLabel = activeDeck === 0 ? "A" : "B";
  write(`Deck:${deckLabel}  Q/W:play  Tab:switch  []:xfade  -/=:vol  z/x:spd  Esc:exit`, [2, statusY], 1);

  // --- Message overlay ---
  if (message && frame - messageFrame < 120) {
    const alpha = Math.max(0, 1 - (frame - messageFrame) / 120);
    ink(Math.floor(255 * alpha), Math.floor(220 * alpha), Math.floor(80 * alpha));
    write(message, [W / 2 - message.length * 3, divY - 10], 1);
  }
}

function sim() {}

function leave() {
  // Audio keeps playing — this is intentional!
}

export { boot, act, paint, sim, leave };
