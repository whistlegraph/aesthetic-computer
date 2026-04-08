// dj.mjs — Turntable piece for AC Native
// One big record, one track, live scratching with mouse/touchpad.
// Auto-scans USB for audio files. Simple, visual, interactive.

const AUDIO_EXTS = new Set(["mp3", "wav", "flac", "ogg", "aac", "m4a", "opus", "wma"]);
let T = __theme.update();

// State
let files = [];         // discovered audio [{path, name}]
let trackIdx = 0;       // current track index
let mounted = false;
let dragging = false;
let dragLastY = 0;
let angle = 0;          // visual rotation angle
let message = "";
let messageFrame = 0;
let frame = 0;
let lastUsbCheck = 0;
let usbConnected = false;
let spinSpeed = 0;      // current spin velocity (auto-decays)

function isAudio(name) {
  const dot = name.lastIndexOf(".");
  return dot >= 0 && AUDIO_EXTS.has(name.slice(dot + 1).toLowerCase());
}

function scanDir(system, path, results, depth) {
  if (depth > 4) return;
  const listing = system?.listDir?.(path);
  if (!listing) return;
  for (const f of listing) {
    const full = path + "/" + f.name;
    if (f.isDir && !f.name.startsWith(".")) scanDir(system, full, results, depth + 1);
    else if (isAudio(f.name)) results.push({ path: full, name: f.name });
  }
}

function scan(system, tts) {
  files = [];
  const dirs = ["/media", "/mnt/samples", "/mnt"];
  for (const d of dirs) scanDir(system, d, files, 0);
  files.sort((a, b) => a.name.localeCompare(b.name));
  if (tts) tts.speak(files.length > 0 ? `${files.length} tracks` : "no tracks");
  msg(files.length > 0 ? `${files.length} tracks` : "no tracks found");
}

function msg(t) { message = t; messageFrame = frame; }

function fmt(s) {
  if (!s || s < 0) return "0:00";
  return `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;
}

function loadTrack(sound, tts) {
  if (files.length === 0) return;
  if (trackIdx >= files.length) trackIdx = 0;
  const f = files[trackIdx];
  const ok = sound?.deck?.load(0, f.path);
  if (ok) {
    msg(f.name.replace(/\.[^.]+$/, ""));
    if (tts) tts.speak(f.name.replace(/\.[^.]+$/, ""));
    sound.deck.play(0);
    spinSpeed = 1;
  } else {
    msg("failed: " + f.name);
  }
}

function boot({ system, sound, tts }) {
  mounted = system?.mountMusic?.() || false;
  usbConnected = mounted;
  scan(system, null);
  const d = sound?.deck?.decks?.[0];
  if (d?.loaded) {
    msg("resumed");
    if (d.playing) spinSpeed = 1;
  } else if (files.length > 0) {
    loadTrack(sound, tts);
  }
}

function act({ event: e, sound, system, tts, screen }) {
  const dk = sound?.deck;
  const d = dk?.decks?.[0];
  const w = screen?.width || 320;
  const h = screen?.height || 240;

  // --- Mouse/touchpad scratch ---
  if (e.is("touch")) {
    dragging = true;
    dragLastY = e.y;
    spinSpeed = 0;
    if (d?.playing) dk.pause(0);
    return;
  }
  if (e.is("draw") && dragging) {
    if (d?.loaded) {
      const dy = e.y - dragLastY;
      const seekAmt = dy * 0.03; // drag sensitivity
      const pos = Math.max(0, Math.min(d.duration, d.position + seekAmt));
      dk.seek(0, pos);
      angle += dy * 0.02;
      dragLastY = e.y;
    }
    return;
  }
  if (e.is("lift")) {
    dragging = false;
    return;
  }

  if (!e.is("keyboard:down")) return;

  if (e.is("keyboard:down:escape")) { system?.jump?.("prompt"); return; }

  // Space: play/pause
  if (e.is("keyboard:down:space")) {
    if (d?.loaded) {
      if (d.playing) { dk.pause(0); spinSpeed = 0; msg("paused"); }
      else { dk.play(0); spinSpeed = 1; msg("playing"); }
    }
    return;
  }

  // N: next track
  if (e.is("keyboard:down:n")) {
    trackIdx++;
    loadTrack(sound, tts);
    return;
  }
  // P: prev track
  if (e.is("keyboard:down:p")) {
    trackIdx = Math.max(0, trackIdx - 1);
    loadTrack(sound, tts);
    return;
  }

  // R: rescan
  if (e.is("keyboard:down:r")) {
    mounted = system?.mountMusic?.() || false;
    scan(system, tts);
    if (files.length > 0) { trackIdx = 0; loadTrack(sound, tts); }
    return;
  }

  // Arrow keys: seek
  if (e.is("keyboard:down:arrowleft")) {
    if (d?.loaded) dk.seek(0, Math.max(0, d.position - 5));
    return;
  }
  if (e.is("keyboard:down:arrowright")) {
    if (d?.loaded) dk.seek(0, Math.min(d.duration, d.position + 5));
    return;
  }

  // Volume
  if (e.is("keyboard:down:-")) { if (d) dk.setVolume(0, Math.max(0, d.volume - 0.05)); return; }
  if (e.is("keyboard:down:=")) { if (d) dk.setVolume(0, Math.min(1, d.volume + 0.05)); return; }

  // Speed
  if (e.is("keyboard:down:z")) { if (d?.loaded) dk.setSpeed(0, Math.max(0.5, d.speed - 0.05)); return; }
  if (e.is("keyboard:down:x")) { if (d?.loaded) dk.setSpeed(0, Math.min(2.0, d.speed + 0.05)); return; }
}

function paint({ wipe, ink, box, line, write, circle, screen, sound }) {
  frame++;
  T = __theme.update();
  const w = screen.width, h = screen.height;
  const F = "font_1";
  const CW = 6;
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  const d = sound?.deck?.decks?.[0] || {};

  // --- Big turntable ---
  const cx = Math.floor(w / 2);
  const cy = Math.floor(h / 2) - 10;
  const r = Math.min(cx - 8, cy - 16);

  // Rotate angle
  if (d.playing && !dragging) {
    angle += (d.speed || 1) * 0.05;
    spinSpeed = d.speed || 1;
  }

  // Platter
  const fg = T.fg || 200;
  const dim = T.fgMute || 60;
  ink(T.bg[0] + 8, T.bg[1] + 8, T.bg[2] + 12);
  circle(cx, cy, r, true);

  // Grooves
  for (let g = Math.floor(r * 0.3); g < r; g += 2) {
    const brightness = dragging ? 10 : 6;
    ink(T.bg[0] + brightness, T.bg[1] + brightness, T.bg[2] + brightness + 4);
    circle(cx, cy, g, false);
  }

  // Rim
  ink(dragging ? 120 : dim, dragging ? 120 : dim, dragging ? 140 : dim + 15);
  circle(cx, cy, r, false);

  // Label (center area)
  const labelR = Math.floor(r * 0.25);
  ink(dragging ? 60 : 30, dragging ? 40 : 25, dragging ? 30 : 20);
  circle(cx, cy, labelR, true);
  ink(dragging ? 100 : 50, dragging ? 80 : 40, dragging ? 60 : 35);
  circle(cx, cy, labelR, false);

  // Center dot
  ink(fg, fg - 30, fg - 60);
  circle(cx, cy, 3, true);

  // Spinning needle line
  const needleLen = r - 6;
  const nx = cx + Math.cos(angle) * needleLen;
  const ny = cy + Math.sin(angle) * needleLen;
  ink(d.playing ? 80 : 40, d.playing ? 200 : 80, d.playing ? 80 : 40);
  line(cx, cy, Math.floor(nx), Math.floor(ny));
  // Needle tip
  ink(d.playing ? 120 : 60, d.playing ? 255 : 120, d.playing ? 120 : 60);
  circle(Math.floor(nx), Math.floor(ny), Math.max(2, Math.floor(r * 0.04)), true);

  // --- Track info ---
  const title = d.loaded ? (d.title || "?").replace(/\.[^.]+$/, "") : "no track";
  const maxC = Math.floor(w / CW) - 2;

  // Title at top
  ink(fg, fg, fg + 10);
  write(title.slice(0, maxC), { x: 4, y: 3, size: 1, font: F });

  // Progress bar
  const barY = h - 22;
  const barW = w - 8;
  const progress = d.duration > 0 ? d.position / d.duration : 0;
  ink(T.bg[0] + 15, T.bg[1] + 15, T.bg[2] + 20);
  box(4, barY, barW, 4);
  ink(d.playing ? 60 : 40, d.playing ? 180 : 100, d.playing ? 60 : 40);
  box(4, barY, Math.max(1, Math.floor(barW * progress)), 4);

  // Time + speed
  ink(dim + 20, dim + 20, dim + 30);
  write(`${fmt(d.position)} / ${fmt(d.duration)}`, { x: 4, y: barY + 7, size: 1, font: F });
  if (d.loaded) {
    const spd = `${(d.speed || 1).toFixed(2)}x`;
    write(spd, { x: w - spd.length * CW - 4, y: barY + 7, size: 1, font: F });
  }

  // Track counter
  if (files.length > 0) {
    ink(dim, dim, dim + 10);
    const tc = `${trackIdx + 1}/${files.length}`;
    write(tc, { x: w - tc.length * CW - 4, y: 3, size: 1, font: F });
  }

  // Status bar
  const sY = h - 10;
  ink(dim, dim, dim + 10);
  write("Spc:play N/P:track R:scan Esc:exit", { x: 4, y: sY, size: 1, font: F });

  // Drag state
  if (dragging) {
    ink(255, 100, 60);
    write("SCRATCH", { x: cx - 21, y: cy - 5, size: 1, font: F });
  }

  // Message toast
  if (message && frame - messageFrame < 120) {
    const fade = Math.max(0, 255 - Math.floor((frame - messageFrame) * 2.5));
    ink(255, 220, 60, fade);
    write(message.slice(0, maxC), { x: 4, y: 14, size: 1, font: F });
  }

  // USB indicator
  ink(usbConnected ? 60 : dim, usbConnected ? 180 : dim, usbConnected ? 60 : dim);
  write(usbConnected ? "USB" : "---", { x: w - 22, y: sY, size: 1, font: F });
}

function sim({ system, tts, sound }) {
  // USB hot-plug check every 3 seconds
  if (frame - lastUsbCheck > 180) {
    lastUsbCheck = frame;
    const nowMounted = system?.mountMusic?.() || false;
    if (nowMounted && !usbConnected) {
      usbConnected = true; mounted = true;
      if (tts) tts.speak("USB connected");
      scan(system, tts);
      if (files.length > 0) { trackIdx = 0; loadTrack(sound, tts); }
    } else if (!nowMounted && usbConnected) {
      usbConnected = false;
      if (tts) tts.speak("USB removed");
      msg("USB removed");
    }
  }
  // Auto-advance when track ends
  const d = sound?.deck?.decks?.[0];
  if (d?.loaded && !d.playing && d.position >= d.duration - 0.1 && d.duration > 0 && !dragging) {
    trackIdx++;
    loadTrack(sound, tts);
  }
}

function leave() {}

export { boot, act, paint, sim, leave };
