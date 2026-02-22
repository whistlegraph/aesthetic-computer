// Audio, 2025.2.21
// Longform audio player with scrubbing, waveform, and subtitle display.

/* 📝 Notes
  Usage:
    audio jesper-per-english-nova   — English dub of Jesper Per (Laer Klokken ep.35)
    audio jesper-per-danish         — Original Danish episode
    audio https://example.com/f.mp3 — Arbitrary MP3 URL
*/

import { createScrubber } from "./common/scrub.mjs";

const STREAM_ID = "audio-player";

// Known tracks
const KNOWN_TRACKS = {
  "jesper-per-english-nova": {
    audio: "/assets/pruttipal/laer-klokken/dub-output/jesper-per-english-nova.mp3",
    subtitles: "/assets/pruttipal/laer-klokken/dub-output/jesper-per-english.json",
    subtitlesAlt: "/assets/pruttipal/laer-klokken/dub-output/jesper-per-danish.json",
    title: "Laer Klokken — Jesper Per (English)",
    langPrimary: "EN",
    langAlt: "DA",
  },
  "jesper-per-danish": {
    audio: "/assets/pruttipal/laer-klokken/jesper%20final.mp3",
    subtitles: "/assets/pruttipal/laer-klokken/dub-output/jesper-per-danish.json",
    subtitlesAlt: "/assets/pruttipal/laer-klokken/dub-output/jesper-per-english.json",
    title: "Laer Klokken — Jesper Per (Dansk)",
    langPrimary: "DA",
    langAlt: "EN",
  },
};

// State
let trackTitle = "Audio";
let audioUrl = null;
let segments = [];
let segmentsAlt = [];
let activeLang = "EN";
let altLang = null;
let activeSegment = null;

let isPlaying = false;
let isLoading = false;
let hasStarted = false;
let currentTime = 0;
let duration = 0;
let bufferedTime = 0;

let scrubber = createScrubber();
let frequencyData = [];
let bars = new Array(32).fill(0);

// UI
let playBtn = null;
let langBtn = null;
let scrubBox = null; // {x, y, w, h} for the scrub bar hit area

// Refs
let _send = null;

function boot({ params, send, ui, screen, hud, net: { preload } }) {
  _send = send;
  const param = params[0] || "jesper-per-english-nova";
  hud.label("audio");

  // Match exact name or shortest unique prefix (e.g. "jes" -> "jesper-per-english-nova")
  const track = KNOWN_TRACKS[param] || Object.values(KNOWN_TRACKS).find(
    (_, i) => Object.keys(KNOWN_TRACKS)[i].startsWith(param)
  );
  if (track) {
    audioUrl = track.audio;
    trackTitle = track.title;
    activeLang = track.langPrimary;
    altLang = track.langAlt;
    // Fetch subtitles via preload (works from worker context)
    preload(track.subtitles)
      .then((data) => { segments = data; })
      .catch(() => {});
    if (track.subtitlesAlt) {
      preload(track.subtitlesAlt)
        .then((data) => { segmentsAlt = data; })
        .catch(() => {});
    }
  } else if (param.startsWith("http")) {
    audioUrl = param;
    trackTitle = param.split("/").pop().replace(/\.[^.]+$/, "");
  } else {
    audioUrl = `/assets/${param}.mp3`;
    trackTitle = param;
  }

  // Buttons
  playBtn = new ui.TextButton("PLAY", { center: "x", screen, y: 0 });
  if (altLang) {
    langBtn = new ui.TextButton(altLang, { right: 6, top: 6, screen });
  }
}

function paint({ wipe, ink, screen, line }) {
  const floor = Math.floor;
  wipe(20, 15, 30);

  if (!audioUrl) {
    ink(150, 130, 170).write("audio track-name", { x: 6, y: 20 });
    ink(100, 90, 120).write("Known tracks:", { x: 6, y: 36 });
    let ty = 50;
    for (const name of Object.keys(KNOWN_TRACKS)) {
      ink(0, 200, 180).write(name, { x: 12, y: ty });
      ty += 12;
    }
    return;
  }

  const m = 6; // margin
  let y = 20; // Below HUD label

  // --- Title ---
  ink(200, 180, 220).write(trackTitle, { x: m, y });
  y += 14;

  // --- Language toggle button ---
  if (langBtn) {
    langBtn.paint(
      { ink },
      [[60, 50, 80], [150, 130, 170], [200, 180, 220]],
    );
  }

  // --- Frequency bars (live visualizer) ---
  const barAreaH = 48;
  const barAreaY = y;
  const barCount = bars.length;
  const gap = 1;
  const barW = floor((screen.width - m * 2 - (barCount - 1) * gap) / barCount);
  const barAreaX = m;

  for (let i = 0; i < barCount; i++) {
    const h = floor(bars[i] * barAreaH);
    if (h > 0) {
      const bx = barAreaX + i * (barW + gap);
      const by = barAreaY + barAreaH - h;
      ink(0, 200, 180, 180).box(bx, by, barW, h, "fill");
    }
  }
  y += barAreaH + 6;

  // --- Scrub bar ---
  const scrubH = 12;
  const scrubW = screen.width - m * 2;
  const scrubX = m;
  const scrubY = y;
  scrubBox = { x: scrubX, y: scrubY - 8, w: scrubW, h: scrubH + 16 }; // Bigger hit area

  // Background
  ink(40, 30, 55).box(scrubX, scrubY, scrubW, scrubH, "fill");

  // Buffered region
  if (duration > 0) {
    const bufW = floor((bufferedTime / duration) * scrubW);
    ink(60, 50, 80).box(scrubX, scrubY, bufW, scrubH, "fill");
  }

  // Playback position (needle)
  if (duration > 0) {
    const progress = scrubber.isScrubbing || scrubber.inertiaActive
      ? scrubber.needleProgress
      : currentTime / duration;
    const nx = scrubX + floor(progress * scrubW);
    ink(0, 255, 200).line(nx, scrubY, nx, scrubY + scrubH);

    // Target line (only when scrubbing)
    if (scrubber.isScrubbing) {
      const tx = scrubX + floor(scrubber.targetProgress * scrubW);
      ink(255, 255, 0, 150).line(tx, scrubY, tx, scrubY + scrubH);
    }
  }

  // Scrub bar outline
  ink(80, 70, 100).box(scrubX, scrubY, scrubW, scrubH, "outline");
  y += scrubH + 10;

  // --- Play/Pause button ---
  const btnLabel = isLoading ? "..." : isPlaying ? "PAUSE" : "PLAY";
  if (playBtn.txt !== btnLabel) {
    playBtn.txt = btnLabel;
    playBtn.reposition({ center: "x", screen, y });
  } else if (playBtn.btn.box.y !== y) {
    playBtn.reposition({ center: "x", screen, y });
  }

  const playScheme = isPlaying
    ? [[0, 80, 70], [0, 200, 180], 255]
    : [[50, 40, 80], [120, 100, 160], 255];
  playBtn.paint({ ink }, playScheme);
  y += playBtn.height + 4;

  // --- Time display ---
  const timeStr = duration > 0
    ? `${fmtTime(currentTime)} / ${fmtTime(duration)}`
    : "--:-- / --:--";
  ink(150, 130, 170).write(timeStr, { center: "x", screen, y });
  y += 16;

  // --- Subtitle ---
  if (activeSegment) {
    const subY = y + 8;
    const text = activeSegment.text.trim();
    const bounds = screen.width - m * 2 - 8;
    ink(255, 255, 240).write(text, { x: m + 4, y: subY }, undefined, bounds, true);
  }
}

function act({ event: e, screen, send, jump }) {
  _send = send;
  if (!audioUrl) return;

  // Play/pause button
  playBtn?.act(e, {
    push: () => {
      if (isLoading) return;
      if (isPlaying) {
        send({ type: "stream:pause", content: { id: STREAM_ID } });
      } else if (hasStarted) {
        send({ type: "stream:resume", content: { id: STREAM_ID } });
      } else {
        isLoading = true;
        send({ type: "stream:play", content: { id: STREAM_ID, url: audioUrl, volume: 0.8 } });
      }
    },
  });

  // Language toggle
  langBtn?.act(e, {
    push: () => {
      if (segmentsAlt.length === 0) return;
      const tmp = segments;
      segments = segmentsAlt;
      segmentsAlt = tmp;
      const tmpLang = activeLang;
      activeLang = altLang;
      altLang = tmpLang;
      langBtn.txt = altLang;
      langBtn.reposition({ right: 6, top: 6, screen });
      activeSegment = findSegment(segments, currentTime);
    },
  });

  // Scrub bar interaction
  if (scrubBox && e.is("touch")) {
    const { x, y } = e;
    if (x >= scrubBox.x && x <= scrubBox.x + scrubBox.w &&
        y >= scrubBox.y && y <= scrubBox.y + scrubBox.h && duration > 0) {
      // Tap-to-seek: jump directly to tapped position
      const progress = (x - scrubBox.x) / scrubBox.w;
      scrubber.start(e, progress, isPlaying);
      if (isPlaying) {
        send({ type: "stream:pause", content: { id: STREAM_ID } });
      }
    }
  }

  if (scrubber.isScrubbing && e.is("draw")) {
    scrubber.drag(e, screen.width);
  }

  if (scrubber.isScrubbing && e.is("lift")) {
    const result = scrubber.end();
    if (result.wasScrubbing) {
      // Seek to final position
      const seekTime = result.finalProgress * duration;
      send({ type: "stream:seek", content: { id: STREAM_ID, time: seekTime } });
      if (result.wasPlayingBefore) {
        send({ type: "stream:resume", content: { id: STREAM_ID } });
      }
    }
  }

  // Keyboard
  if (e.is("keyboard:down:space")) {
    if (isPlaying) {
      send({ type: "stream:pause", content: { id: STREAM_ID } });
    } else if (hasStarted) {
      send({ type: "stream:resume", content: { id: STREAM_ID } });
    } else {
      isLoading = true;
      send({ type: "stream:play", content: { id: STREAM_ID, url: audioUrl, volume: 0.8 } });
    }
  }

  if (e.is("keyboard:down:arrowleft") && duration > 0) {
    const t = Math.max(0, currentTime - 10);
    send({ type: "stream:seek", content: { id: STREAM_ID, time: t } });
  }

  if (e.is("keyboard:down:arrowright") && duration > 0) {
    const t = Math.min(duration, currentTime + 10);
    send({ type: "stream:seek", content: { id: STREAM_ID, time: t } });
  }
}

function sim({ send }) {
  _send = send;
  if (!audioUrl) return;

  // Poll time data
  send({ type: "stream:time", content: { id: STREAM_ID } });

  // Poll frequency data when playing
  if (isPlaying) {
    send({ type: "stream:frequencies", content: { id: STREAM_ID } });
  }

  // Run scrubber physics
  const simResult = scrubber.simulate();
  if (simResult && duration > 0) {
    const seekTime = simResult.needleProgress * duration;
    send({ type: "stream:seek", content: { id: STREAM_ID, time: seekTime } });
  }

  // Animate frequency bars (smooth decay)
  if (frequencyData.length > 0) {
    const step = Math.max(1, Math.floor(frequencyData.length / bars.length));
    for (let i = 0; i < bars.length; i++) {
      const idx = i * step;
      const val = (frequencyData[idx] || 0) / 255;
      bars[i] = bars[i] * 0.7 + val * 0.3; // Smooth
    }
  } else if (!isPlaying) {
    for (let i = 0; i < bars.length; i++) {
      bars[i] *= 0.92; // Decay when paused
    }
  }

  // Subtitle sync
  activeSegment = findSegment(segments, currentTime);
}

function receive({ type, content }) {
  if (content?.id !== STREAM_ID) return;

  if (type === "stream:playing") {
    isPlaying = true;
    isLoading = false;
    hasStarted = true;
  }
  if (type === "stream:paused") {
    isPlaying = false;
  }
  if (type === "stream:stopped") {
    isPlaying = false;
    hasStarted = false;
  }
  if (type === "stream:error") {
    isPlaying = false;
    isLoading = false;
    console.warn("Audio error:", content.error);
  }
  if (type === "stream:time-data") {
    if (!scrubber.isScrubbing && !scrubber.inertiaActive) {
      currentTime = content.currentTime;
    }
    duration = content.duration;
    bufferedTime = content.buffered;
    if (content.ended && hasStarted) {
      isPlaying = false;
    }
  }
  if (type === "stream:seeked") {
    currentTime = content.time;
  }
  if (type === "stream:frequencies-data") {
    frequencyData = content.data || [];
  }
}

function leave({ send }) {
  send({ type: "stream:stop", content: { id: STREAM_ID } });
  scrubber.reset();
  segments = [];
  segmentsAlt = [];
  activeSegment = null;
  isPlaying = false;
  hasStarted = false;
  currentTime = 0;
  duration = 0;
}

export { boot, paint, act, sim, receive, leave };

// --- Helpers ---

function fmtTime(sec) {
  if (!Number.isFinite(sec) || sec < 0) return "--:--";
  const m = Math.floor(sec / 60);
  const s = Math.floor(sec % 60);
  return `${m}:${String(s).padStart(2, "0")}`;
}

function findSegment(segs, time) {
  if (!segs.length) return null;
  let lo = 0, hi = segs.length - 1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (time < segs[mid].start) hi = mid - 1;
    else if (time > segs[mid].end) lo = mid + 1;
    else return segs[mid];
  }
  return null;
}
