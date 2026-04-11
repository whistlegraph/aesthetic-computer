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
let dragLastX = 0;
let dragLastY = 0;
let angle = 0;          // visual rotation angle
let message = "";
let messageFrame = 0;
let frame = 0;
let lastUsbCheck = 0;
let usbConnected = false;
let spinSpeed = 0;      // record spin rate (0=stopped, 1=playing)
let scratchSpeed = 0;   // current scratch velocity (-2 to 2)
let wasPlaying = false;  // was playing before scratch started

// Button layout (computed in paint, used in act)
let buttons = [];       // [{x, y, w, h, id, label}]

function isAudio(name) {
  if (name.startsWith(".")) return false; // skip metadata (._xxx.m4a etc)
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

function say(sound, text) { sound?.speak?.(text); }

function scan(system, sound) {
  files = [];
  const dirs = ["/media", "/mnt/samples", "/mnt"];
  for (const d of dirs) scanDir(system, d, files, 0);
  files.sort((a, b) => a.name.localeCompare(b.name));
  say(sound, files.length > 0 ? `${files.length} tracks` : "no tracks");
  msg(files.length > 0 ? `${files.length} tracks` : "no tracks found");
}

function msg(t) { message = t; messageFrame = frame; }

function fmt(s) {
  if (!s || s < 0) return "0:00";
  return `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;
}

function loadTrack(sound) {
  if (files.length === 0) return;
  if (trackIdx >= files.length) trackIdx = 0;
  const f = files[trackIdx];
  const ok = sound?.deck?.load(0, f.path);
  if (ok) {
    msg(f.name.replace(/\.[^.]+$/, ""));
    say(sound, f.name.replace(/\.[^.]+$/, ""));
    sound.deck.play(0);
    spinSpeed = 1;
  } else {
    msg("failed: " + f.name);
  }
}

function boot({ system, sound }) {
  system?.mountMusic?.();
  mounted = !!system?.mountMusicMounted;
  usbConnected = mounted;
  scan(system, sound);
  const d = sound?.deck?.decks?.[0];
  if (d?.loaded) {
    msg("resumed");
    if (d.playing) spinSpeed = 1;
  } else if (files.length > 0) {
    loadTrack(sound);
  }
}

function act({ event: e, sound, system, screen }) {
  const dk = sound?.deck;
  const d = dk?.decks?.[0];
  const w = screen?.width || 320;
  const h = screen?.height || 240;

  // --- Button tap check (before scratch) ---
  if (e.is("touch")) {
    for (const b of buttons) {
      if (e.x >= b.x && e.x <= b.x + b.w && e.y >= b.y && e.y <= b.y + b.h) {
        if (b.id === "play") {
          if (d?.loaded) {
            if (d.playing) { dk.pause(0); spinSpeed = 0; msg("paused"); }
            else { dk.play(0); spinSpeed = 1; msg("playing"); }
          }
        } else if (b.id === "next") { trackIdx++; loadTrack(sound); }
        else if (b.id === "prev") { trackIdx = Math.max(0, trackIdx - 1); loadTrack(sound); }
        else if (b.id === "reset") { if (d?.loaded) { dk.setSpeed(0, 1); msg("1.00x"); } }
        else if (b.id === "scan") {
          system?.mountMusic?.();
          mounted = !!system?.mountMusicMounted;
          usbConnected = mounted;
          if (mounted) {
            scan(system, sound);
            if (files.length > 0) { trackIdx = 0; loadTrack(sound); }
          } else {
            msg(system?.mountMusicPending ? "checking usb" : "no usb");
          }
        }
        return;
      }
    }
  }

  // --- Mouse/touchpad scratch (radial around platter center) ---
  if (e.is("touch")) {
    dragging = true;
    dragLastX = e.x;
    dragLastY = e.y;
    wasPlaying = d?.playing || false;
    scratchSpeed = 0;
    return;
  }
  if (e.is("draw") && dragging) {
    if (d?.loaded) {
      const cx = (screen?.width || 320) / 2;
      const cy = (screen?.height || 240) / 2 - 10;
      // Compute angular change around platter center
      const prevAngle = Math.atan2(dragLastY - cy, dragLastX - cx);
      const curAngle = Math.atan2(e.y - cy, e.x - cx);
      let delta = curAngle - prevAngle;
      // Normalize to [-PI, PI]
      if (delta > Math.PI) delta -= Math.PI * 2;
      if (delta < -Math.PI) delta += Math.PI * 2;
      // Convert angular delta to speed: positive = forward, negative = reverse
      scratchSpeed = delta * 8; // sensitivity multiplier
      // Clamp
      if (scratchSpeed > 3) scratchSpeed = 3;
      if (scratchSpeed < -3) scratchSpeed = -3;
      dk.setSpeed(0, scratchSpeed);
      if (!d.playing) dk.play(0);
      angle += delta;
      dragLastX = e.x;
      dragLastY = e.y;
    }
    return;
  }
  if (e.is("lift")) {
    if (dragging && d?.loaded) {
      // Release: resume normal speed or stop
      if (wasPlaying) {
        dk.setSpeed(0, 1);
        dk.play(0);
      } else {
        dk.setSpeed(0, 1);
        dk.pause(0);
      }
      scratchSpeed = 0;
    }
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
    loadTrack(sound);
    return;
  }
  // P: prev track
  if (e.is("keyboard:down:p")) {
    trackIdx = Math.max(0, trackIdx - 1);
    loadTrack(sound);
    return;
  }

  // R: rescan
  if (e.is("keyboard:down:r")) {
    system?.mountMusic?.();
    mounted = !!system?.mountMusicMounted;
    usbConnected = mounted;
    if (mounted) {
      scan(system, sound);
      if (files.length > 0) { trackIdx = 0; loadTrack(sound); }
    } else {
      msg(system?.mountMusicPending ? "checking usb" : "no usb");
    }
    return;
  }

  // Arrow keys: speed/stretch (like a pitch fader)
  if (e.is("keyboard:down:arrowleft")) {
    if (d?.loaded) dk.setSpeed(0, Math.max(-2, (d.speed || 1) - 0.1));
    return;
  }
  if (e.is("keyboard:down:arrowright")) {
    if (d?.loaded) dk.setSpeed(0, Math.min(3, (d.speed || 1) + 0.1));
    return;
  }
  // Arrow up/down: seek
  if (e.is("keyboard:down:arrowup")) {
    if (d?.loaded) dk.seek(0, Math.min(d.duration, d.position + 10));
    return;
  }
  if (e.is("keyboard:down:arrowdown")) {
    if (d?.loaded) dk.seek(0, Math.max(0, d.position - 10));
    return;
  }

  // Volume
  if (e.is("keyboard:down:-")) { if (d) dk.setVolume(0, Math.max(0, d.volume - 0.05)); return; }
  if (e.is("keyboard:down:=")) { if (d) dk.setVolume(0, Math.min(1, d.volume + 0.05)); return; }

  // Z: reset speed to 1x
  if (e.is("keyboard:down:z")) { if (d?.loaded) dk.setSpeed(0, 1); msg("1.00x"); return; }
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

  // Rotate angle — follows playback speed (including negative for reverse scratch)
  if (d.playing && !dragging) {
    angle += (d.speed || 1) * 0.05;
  }

  // Theme-derived helpers
  const fg = T.fg || 200;
  const dim = T.fgMute || 60;
  const s = T.dark ? 1 : -1; // sign: +brightness for dark, -brightness for light

  // Platter
  ink(T.bg[0] + 8 * s, T.bg[1] + 8 * s, T.bg[2] + 12 * s);
  circle(cx, cy, r, true);

  // Grooves
  for (let g = Math.floor(r * 0.3); g < r; g += 2) {
    const b = dragging ? 10 : 6;
    ink(T.bg[0] + b * s, T.bg[1] + b * s, T.bg[2] + (b + 4) * s);
    circle(cx, cy, g, false);
  }

  // Rim
  ink(T.border[0], T.border[1], T.border[2]);
  circle(cx, cy, r, false);

  // Label (center area)
  const labelR = Math.floor(r * 0.25);
  const lf = T.dark ? 1 : -1;
  ink(T.bg[0] + 20 * lf, T.bg[1] + 15 * lf, T.bg[2] + 10 * lf);
  circle(cx, cy, labelR, true);
  ink(T.bg[0] + 40 * lf, T.bg[1] + 30 * lf, T.bg[2] + 25 * lf);
  circle(cx, cy, labelR, false);

  // Center dot
  ink(T.accent[0], T.accent[1], T.accent[2]);
  circle(cx, cy, 3, true);

  // Spinning needle line
  const needleLen = r - 6;
  const nx = cx + Math.cos(angle) * needleLen;
  const ny = cy + Math.sin(angle) * needleLen;
  const nOn = T.ok, nOff = [dim, dim, dim];
  const nc = d.playing ? nOn : nOff;
  ink(nc[0], nc[1], nc[2]);
  line(cx, cy, Math.floor(nx), Math.floor(ny));
  // Needle tip
  ink(d.playing ? nOn[0] + 40 : dim + 20, d.playing ? nOn[1] + 40 : dim + 20, d.playing ? nOn[2] + 40 : dim + 20);
  circle(Math.floor(nx), Math.floor(ny), Math.max(2, Math.floor(r * 0.04)), true);

  // --- Track info ---
  const title = d.loaded ? (d.title || "?").replace(/\.[^.]+$/, "") : "no track";
  const maxC = Math.floor(w / CW) - 2;

  // Title at top
  ink(fg, fg, fg);
  write(title.slice(0, maxC), { x: 4, y: 3, size: 1, font: F });

  // Track counter + USB (top right)
  if (files.length > 0) {
    ink(dim, dim, dim);
    const tc = `${trackIdx + 1}/${files.length}`;
    write(tc, { x: w - tc.length * CW - 4, y: 3, size: 1, font: F });
  }
  const uc = usbConnected ? T.ok : [dim, dim, dim];
  ink(uc[0], uc[1], uc[2]);
  write(usbConnected ? "USB" : "---", { x: w - 22, y: 14, size: 1, font: F });

  // --- Bottom panel (no overlaps) ---
  // Layout from bottom up:
  //   h-12: buttons row
  //   h-24: time / speed
  //   h-32: progress bar

  // Progress bar
  const barY = h - 34;
  const barW = w - 8;
  const progress = d.duration > 0 ? d.position / d.duration : 0;
  ink(T.bar[0], T.bar[1], T.bar[2]);
  box(4, barY, barW, 4);
  const pb = d.playing ? T.ok : [dim, dim + 20, dim];
  ink(pb[0], pb[1], pb[2]);
  box(4, barY, Math.max(1, Math.floor(barW * progress)), 4);

  // Time + speed
  ink(T.fgDim, T.fgDim, T.fgDim);
  write(`${fmt(d.position)} / ${fmt(d.duration)}`, { x: 4, y: barY + 7, size: 1, font: F });
  if (d.loaded) {
    const spd = `${(d.speed || 1).toFixed(2)}x`;
    write(spd, { x: w - spd.length * CW - 4, y: barY + 7, size: 1, font: F });
  }

  // --- Button row ---
  buttons = [];
  const btnY = h - 14;
  const btnH = 12;
  const btnDefs = [
    { id: "prev", label: "< Prev" },
    { id: "play", label: d.playing ? "Pause" : "Play" },
    { id: "next", label: "Next >" },
    { id: "reset", label: "1x" },
    { id: "scan", label: "Scan" },
  ];
  const gap = 3;
  const totalGap = gap * (btnDefs.length - 1);
  const btnW = Math.floor((w - 8 - totalGap) / btnDefs.length);
  for (let i = 0; i < btnDefs.length; i++) {
    const bx = 4 + i * (btnW + gap);
    const bd = btnDefs[i];
    buttons.push({ x: bx, y: btnY, w: btnW, h: btnH, id: bd.id, label: bd.label });
    // Button background
    ink(T.bar[0], T.bar[1], T.bar[2]);
    box(bx, btnY, btnW, btnH);
    // Button border
    ink(T.border[0], T.border[1], T.border[2]);
    box(bx, btnY, btnW, btnH, "outline");
    // Button label (centered)
    const lx = bx + Math.floor((btnW - bd.label.length * CW) / 2);
    const ly = btnY + 2;
    ink(fg, fg, fg);
    write(bd.label, { x: lx, y: ly, size: 1, font: F });
  }

  // Drag state
  if (dragging) {
    ink(T.accent[0], T.accent[1], T.accent[2]);
    write("SCRATCH", { x: cx - 21, y: cy - 5, size: 1, font: F });
  }

  // Message toast
  if (message && frame - messageFrame < 120) {
    const fade = Math.max(0, 255 - Math.floor((frame - messageFrame) * 2.5));
    ink(T.warn[0], T.warn[1], T.warn[2], fade);
    write(message.slice(0, maxC), { x: 4, y: 16, size: 1, font: F });
  }
}

function sim({ system, sound }) {
  // USB hot-plug check every 3 seconds while active, or 15 seconds while idle.
  const mountPending = !!system?.mountMusicPending;
  const nowMounted = !!system?.mountMusicMounted;
  const usbCheckFrames = usbConnected ? 180 : 900;
  if (!mountPending && frame - lastUsbCheck > usbCheckFrames) {
    lastUsbCheck = frame;
    system?.mountMusic?.();
  }
  if (nowMounted && !usbConnected) {
    usbConnected = true; mounted = true;
    say(sound, "USB DJ on");
    scan(system, null);
    if (files.length > 0) { trackIdx = 0; loadTrack(sound); }
  } else if (!nowMounted && usbConnected && !mountPending) {
    usbConnected = false;
    const dk = sound?.deck;
    const d = dk?.decks?.[0];
    if (d?.playing) { dk.pause(0); spinSpeed = 0; }
    files = [];
    say(sound, "USB DJ off");
    msg("USB removed");
  }
  // Auto-advance when track ends
  const d = sound?.deck?.decks?.[0];
  if (d?.loaded && !d.playing && d.position >= d.duration - 0.1 && d.duration > 0 && !dragging) {
    trackIdx++;
    loadTrack(sound);
  }
}

function leave() {}

export { boot, act, paint, sim, leave };
