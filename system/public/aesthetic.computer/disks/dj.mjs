// DJ, 2026.6.10
// One big record. Drop an audio file on the window (ac-electron) and
// scratch it. Ported from the AC Native turntable piece.
// Usage: dj [url|track-name] — or drag an mp3 into the app.

const STREAM_ID = "dj";
const CDN = "https://assets.aesthetic.computer";
const SECS_PER_ROT = 1.8; // 33⅓ rpm — one platter rotation of audio

// Palette
const BG = [16, 12, 24];
const PLATTER = [28, 24, 40];
const RIM = [80, 68, 110];
const BAR = [40, 32, 58];
const ACCENT = [0, 255, 200];
const OK = [0, 200, 160];
const WARN = [255, 180, 80];
const FG = 210;
const DIM = 80;

// Track + transport
let audioUrl = null,
  trackTitle = "";
let isPlaying = false,
  isLoading = false,
  hasStarted = false;
let currentTime = 0,
  duration = 0,
  bufferedTime = 0;
let pitch = 1; // resting speed (pitch fader)
let liveSpeed = 1; // last applied playback rate (display)
let volume = 0.8;
let peaks = null; // whole-track overview (0..1) or null while decoding

// Platter
let angle = -Math.PI / 2;
let frame = 0,
  message = "",
  messageFrame = 0;

// Scratch (radial drag around the platter). The stream backend can't run
// in reverse, so forward drags are audible (playbackRate) and backward
// drags scrub silently via seeks — vinyl-ish, within HTMLMediaElement law.
let scratching = false;
let scratchLast = { x: 0, y: 0 };
let wasPlayingBeforeScratch = false;
let scratchAudible = false; // engine currently running for the scratch
let scratchTime = 0; // predicted position while seeking backward

// Layout (computed in paint, hit-tested in act)
let buttons = [];
let strip = null; // waveform scrub strip box
let stripScrub = false;
let platterC = { x: 0, y: 0, r: 0 };

function msg(t) {
  message = t;
  messageFrame = frame;
}

function fmt(s) {
  if (!Number.isFinite(s) || s < 0) return "0:00";
  return `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;
}

function load(send, { url, name }) {
  if (hasStarted) send({ type: "stream:stop", content: { id: STREAM_ID } });
  audioUrl = url;
  trackTitle = (name || url.split("/").pop() || "track").replace(/\.[^.]+$/, "");
  isPlaying = false;
  hasStarted = false;
  currentTime = 0;
  duration = 0;
  bufferedTime = 0;
  pitch = 1;
  liveSpeed = 1;
  peaks = null;
  isLoading = true;
  send({ type: "stream:play", content: { id: STREAM_ID, url, volume } });
  send({ type: "stream:peaks", content: { id: STREAM_ID, url, count: 1024 } });
  msg(trackTitle);
}

function boot({ params, send, hud, system }) {
  hud.label();
  // ac-electron drag-drop takes priority — disk.mjs stashes the loopback
  // url on system.droppedFile and jumps here.
  const dropped = system?.droppedFile;
  if (dropped?.url) {
    load(send, dropped);
    if (system) system.droppedFile = null;
    return;
  }
  if (params[0]) {
    const key = params[0];
    const url = key.startsWith("http") ? key : `${CDN}/audio/${key}.mp3`;
    load(send, { url, name: key.split("/").pop() });
  }
}

function setSpeed(send, s) {
  liveSpeed = s;
  send({ type: "stream:speed", content: { id: STREAM_ID, speed: s } });
}

function togglePlay(send) {
  if (isLoading || !audioUrl) return;
  if (isPlaying) {
    send({ type: "stream:pause", content: { id: STREAM_ID } });
  } else if (hasStarted) {
    setSpeed(send, pitch);
    send({ type: "stream:resume", content: { id: STREAM_ID } });
  } else {
    isLoading = true;
    send({ type: "stream:play", content: { id: STREAM_ID, url: audioUrl, volume } });
  }
}

function nudgePitch(send, d) {
  pitch = Math.min(3, Math.max(0.25, pitch + d));
  if (isPlaying || scratching) setSpeed(send, pitch);
  else liveSpeed = pitch;
  msg(`${pitch.toFixed(2)}x`);
}

function act({ event: e, send, system }) {
  // A drop while dj is already running swaps the record.
  if (e.is("dropped:file") && system?.droppedFile?.url) {
    load(send, system.droppedFile);
    system.droppedFile = null;
    return;
  }

  if (e.is("touch")) {
    // Buttons
    for (const b of buttons) {
      if (e.x >= b.x && e.x <= b.x + b.w && e.y >= b.y && e.y <= b.y + b.h) {
        if (b.id === "play") togglePlay(send);
        else if (b.id === "slower") nudgePitch(send, -0.05);
        else if (b.id === "faster") nudgePitch(send, 0.05);
        else if (b.id === "reset") {
          pitch = 1;
          if (isPlaying) setSpeed(send, 1);
          else liveSpeed = 1;
          msg("1.00x");
        }
        return;
      }
    }
    // Waveform strip → needle drop
    if (strip && duration > 0 && e.x >= strip.x && e.x <= strip.x + strip.w &&
        e.y >= strip.y && e.y <= strip.y + strip.h) {
      stripScrub = true;
      const t = ((e.x - strip.x) / strip.w) * duration;
      send({ type: "stream:seek", content: { id: STREAM_ID, time: t } });
      return;
    }
    // Platter → start scratching
    const dx = e.x - platterC.x,
      dy = e.y - platterC.y;
    if (duration > 0 && Math.sqrt(dx * dx + dy * dy) <= platterC.r + 8) {
      scratching = true;
      scratchLast = { x: e.x, y: e.y };
      wasPlayingBeforeScratch = isPlaying;
      scratchAudible = isPlaying;
      scratchTime = currentTime;
    }
    return;
  }

  if (e.is("draw")) {
    if (stripScrub && strip && duration > 0) {
      const t = (Math.min(Math.max(e.x - strip.x, 0), strip.w) / strip.w) * duration;
      send({ type: "stream:seek", content: { id: STREAM_ID, time: t } });
      return;
    }
    if (scratching) {
      const prev = Math.atan2(scratchLast.y - platterC.y, scratchLast.x - platterC.x);
      const cur = Math.atan2(e.y - platterC.y, e.x - platterC.x);
      let delta = cur - prev;
      if (delta > Math.PI) delta -= Math.PI * 2;
      if (delta < -Math.PI) delta += Math.PI * 2;
      angle += delta;
      scratchLast = { x: e.x, y: e.y };
      const rate = delta * 8; // sensitivity — matches the native piece
      if (rate >= 0.0625) {
        // Forward: audible, pitch bends with speed.
        setSpeed(send, Math.min(4, rate));
        if (!scratchAudible) {
          send({ type: "stream:resume", content: { id: STREAM_ID } });
          scratchAudible = true;
        }
        scratchTime = currentTime;
      } else {
        // Held still or dragged backward: hold the record (silence) and
        // wind position by rotation.
        if (scratchAudible) {
          send({ type: "stream:pause", content: { id: STREAM_ID } });
          scratchAudible = false;
        }
        if (rate < 0) {
          scratchTime = Math.min(
            duration,
            Math.max(0, scratchTime + (delta / (Math.PI * 2)) * SECS_PER_ROT),
          );
          send({ type: "stream:seek", content: { id: STREAM_ID, time: scratchTime } });
        }
      }
      return;
    }
  }

  if (e.is("lift")) {
    if (scratching) {
      setSpeed(send, pitch);
      if (wasPlayingBeforeScratch) {
        send({ type: "stream:resume", content: { id: STREAM_ID } });
      } else if (scratchAudible) {
        send({ type: "stream:pause", content: { id: STREAM_ID } });
      }
      scratching = false;
      scratchAudible = false;
    }
    stripScrub = false;
    return;
  }

  if (e.is("keyboard:down:space")) togglePlay(send);
  if (e.is("keyboard:down:arrowleft")) nudgePitch(send, -0.05);
  if (e.is("keyboard:down:arrowright")) nudgePitch(send, 0.05);
  if (e.is("keyboard:down:arrowup") && duration > 0)
    send({ type: "stream:seek", content: { id: STREAM_ID, time: Math.min(duration, currentTime + 10) } });
  if (e.is("keyboard:down:arrowdown") && duration > 0)
    send({ type: "stream:seek", content: { id: STREAM_ID, time: Math.max(0, currentTime - 10) } });
  if (e.is("keyboard:down:-")) {
    volume = Math.max(0, volume - 0.05);
    send({ type: "stream:volume", content: { id: STREAM_ID, volume } });
    msg(`vol ${Math.round(volume * 100)}`);
  }
  if (e.is("keyboard:down:=")) {
    volume = Math.min(1, volume + 0.05);
    send({ type: "stream:volume", content: { id: STREAM_ID, volume } });
    msg(`vol ${Math.round(volume * 100)}`);
  }
  if (e.is("keyboard:down:z")) {
    pitch = 1;
    if (isPlaying) setSpeed(send, 1);
    else liveSpeed = 1;
    msg("1.00x");
  }
}

function sim({ send }) {
  if (!audioUrl) return;
  send({ type: "stream:time", content: { id: STREAM_ID } });
}

function paint({ wipe, ink, box, line, circle, write, screen }) {
  frame++;
  const w = screen.width,
    h = screen.height;
  const CW = 6; // font_1 advance
  wipe(...BG);

  // --- Platter ---
  const cx = Math.floor(w / 2);
  const cy = Math.floor(h / 2) - 10;
  const r = Math.min(cx - 8, cy - 16);
  platterC = { x: cx, y: cy, r };

  if (isPlaying && !scratching) angle += liveSpeed * 0.05;

  ink(...PLATTER);
  circle(cx, cy, r, true);
  for (let g = Math.floor(r * 0.3); g < r; g += 3) {
    const b = scratching ? 14 : 8;
    ink(PLATTER[0] + b, PLATTER[1] + b, PLATTER[2] + b + 4);
    circle(cx, cy, g);
  }
  ink(...RIM);
  circle(cx, cy, r);

  // Label
  const labelR = Math.floor(r * 0.25);
  ink(PLATTER[0] + 26, PLATTER[1] + 14, PLATTER[2] + 8);
  circle(cx, cy, labelR, true);
  ink(PLATTER[0] + 50, PLATTER[1] + 32, PLATTER[2] + 28);
  circle(cx, cy, labelR);
  ink(...ACCENT);
  circle(cx, cy, 3, true);

  // Needle
  const nx = cx + Math.cos(angle) * (r - 6);
  const ny = cy + Math.sin(angle) * (r - 6);
  const nc = isPlaying ? OK : [DIM, DIM, DIM];
  ink(...nc);
  line(cx, cy, Math.floor(nx), Math.floor(ny));
  ink(nc[0] + 40, nc[1] + 40, nc[2] + 40);
  circle(Math.floor(nx), Math.floor(ny), Math.max(2, Math.floor(r * 0.04)), true);

  // --- Track info ---
  const maxC = Math.floor(w / CW) - 2;
  if (audioUrl) {
    ink(FG, FG, FG).write(trackTitle.slice(0, maxC), { x: 4, y: 3 });
  } else {
    ink(FG, FG, FG).write("drop an audio file", { center: "x", screen, y: cy - 4 });
    ink(DIM, DIM, DIM).write("or: dj:https://url.mp3", { center: "x", screen, y: cy + 8 });
  }

  // --- Waveform strip + playhead ---
  const barY = h - 34;
  const barW = w - 8;
  strip = { x: 4, y: barY - 10, w: barW, h: 24 };
  const progress = duration > 0 ? currentTime / duration : 0;
  const playedX = 4 + Math.floor(barW * progress);
  if (peaks && peaks.length > 0) {
    const cyW = barY + 2,
      HH = 11;
    const pb = isPlaying ? OK : [DIM + 30, DIM + 30, DIM + 30];
    for (let x = 0; x < barW; x++) {
      const pk = peaks[Math.floor((x / barW) * peaks.length)] || 0;
      const colH = Math.max(1, Math.floor(pk * HH));
      if (4 + x < playedX) ink(...pb);
      else ink(...BAR);
      box(4 + x, cyW - colH, 1, colH * 2);
    }
    ink(...ACCENT);
    box(Math.min(4 + barW - 1, playedX), cyW - HH, 1, HH * 2);
  } else if (audioUrl) {
    // Peaks still decoding (or CORS-blocked) — thin progress bar.
    ink(...BAR);
    box(4, barY, barW, 4);
    if (duration > 0) {
      ink(...(isPlaying ? OK : [DIM, DIM + 20, DIM]));
      box(4, barY, Math.max(1, playedX - 4), 4);
    }
    if (bufferedTime > 0 && duration > 0) {
      ink(BAR[0] + 20, BAR[1] + 20, BAR[2] + 20);
      box(playedX, barY, Math.floor(barW * (bufferedTime / duration)) - (playedX - 4), 4);
    }
  }

  // Time + speed
  ink(DIM + 40, DIM + 40, DIM + 50).write(`${fmt(currentTime)} / ${fmt(duration)}`, { x: 4, y: barY + 7 });
  if (audioUrl) {
    const spd = `${liveSpeed.toFixed(2)}x`;
    write(spd, { x: w - spd.length * CW - 4, y: barY + 7 });
  }

  // --- Button row ---
  buttons = [];
  const btnY = h - 14,
    btnH = 12,
    gap = 3;
  const btnDefs = [
    { id: "slower", label: "-" },
    { id: "play", label: isLoading ? "..." : isPlaying ? "Pause" : "Play" },
    { id: "faster", label: "+" },
    { id: "reset", label: "1x" },
  ];
  const btnW = Math.floor((w - 8 - gap * (btnDefs.length - 1)) / btnDefs.length);
  for (let i = 0; i < btnDefs.length; i++) {
    const bx = 4 + i * (btnW + gap);
    const bd = btnDefs[i];
    buttons.push({ x: bx, y: btnY, w: btnW, h: btnH, id: bd.id });
    ink(...BAR);
    box(bx, btnY, btnW, btnH);
    ink(...RIM);
    box(bx, btnY, btnW, btnH, "outline");
    ink(FG, FG, FG).write(bd.label, {
      x: bx + Math.floor((btnW - bd.label.length * CW) / 2),
      y: btnY + 2,
    });
  }

  if (scratching) {
    ink(...ACCENT).write("SCRATCH", { x: cx - 21, y: cy - 5 });
  }

  // Message toast
  if (message && frame - messageFrame < 120) {
    const fade = Math.max(0, 255 - Math.floor((frame - messageFrame) * 2.5));
    ink(WARN[0], WARN[1], WARN[2], fade).write(message.slice(0, maxC), { x: 4, y: 16 });
  }
}

function receive({ type, content }) {
  if (content?.id !== STREAM_ID) return;
  if (type === "stream:playing") {
    isPlaying = true;
    isLoading = false;
    hasStarted = true;
  }
  if (type === "stream:paused") isPlaying = false;
  if (type === "stream:stopped") {
    isPlaying = false;
    hasStarted = false;
  }
  if (type === "stream:error") {
    isPlaying = false;
    isLoading = false;
    msg("stream error");
  }
  if (type === "stream:time-data") {
    currentTime = content.currentTime;
    duration = content.duration;
    bufferedTime = content.buffered;
    if (content.ended && hasStarted) isPlaying = false;
  }
  if (type === "stream:seeked") {
    currentTime = content.time;
    if (!scratching) scratchTime = content.time;
  }
  if (type === "stream:speed-data") liveSpeed = content.speed;
  if (type === "stream:peaks-data" && content.peaks?.length) peaks = content.peaks;
}

function leave({ send }) {
  send({ type: "stream:stop", content: { id: STREAM_ID } });
}

export { boot, paint, act, sim, receive, leave };
