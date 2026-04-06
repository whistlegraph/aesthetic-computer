// Play, 2025.4.05
// Simple audio player. Usage: play [track-name|url]

import { createScrubber } from "./common/scrub.mjs";

const STREAM_ID = "play";
const CDN = "https://assets.aesthetic.computer";

const KNOWN_TRACKS = {
  "sopra-reversed": {
    audio: `${CDN}/audio/Sopra%20il%20Silenzio%20-%2010336%20-%202026-02-10%20(reversed-trimmed).mp3`,
    title: "Sopra il Silenzio (reversed)",
  },
};

let trackTitle, audioUrl;
let isPlaying = false, isLoading = false, hasStarted = false;
let currentTime = 0, duration = 0, bufferedTime = 0;
let scrubber = createScrubber();
let bars = new Array(32).fill(0);
let frequencyData = [];
let playBtn, scrubBox;

function boot({ params, send, ui, screen, hud }) {
  hud.label("play");
  const param = params[0] || "sopra-reversed";

  const track = KNOWN_TRACKS[param];
  if (track) {
    audioUrl = track.audio;
    trackTitle = track.title;
  } else if (param.startsWith("http")) {
    audioUrl = param;
    trackTitle = param.split("/").pop().replace(/\.[^.]+$/, "");
  } else {
    audioUrl = `${CDN}/audio/${param}.mp3`;
    trackTitle = param;
  }

  playBtn = new ui.TextButton("PLAY", { center: "x", screen, y: 0 });
}

function paint({ wipe, ink, screen, line }) {
  const floor = Math.floor;
  wipe(20, 15, 30);

  const m = 6;
  let y = 20;

  // Title
  ink(200, 180, 220).write(trackTitle, { x: m, y });
  y += 14;

  // Frequency bars
  const barAreaH = 48, barCount = bars.length, gap = 1;
  const barW = floor((screen.width - m * 2 - (barCount - 1) * gap) / barCount);
  for (let i = 0; i < barCount; i++) {
    const h = floor(bars[i] * barAreaH);
    if (h > 0) {
      ink(0, 200, 180, 180).box(
        m + i * (barW + gap), y + barAreaH - h, barW, h, "fill",
      );
    }
  }
  y += barAreaH + 6;

  // Scrub bar
  const scrubH = 12, scrubW = screen.width - m * 2;
  scrubBox = { x: m, y: y - 8, w: scrubW, h: scrubH + 16 };

  ink(40, 30, 55).box(m, y, scrubW, scrubH, "fill");
  if (duration > 0) {
    ink(60, 50, 80).box(m, y, floor((bufferedTime / duration) * scrubW), scrubH, "fill");
    const progress = scrubber.isScrubbing || scrubber.inertiaActive
      ? scrubber.needleProgress
      : currentTime / duration;
    ink(0, 255, 200).line(m + floor(progress * scrubW), y, m + floor(progress * scrubW), y + scrubH);
  }
  ink(80, 70, 100).box(m, y, scrubW, scrubH, "outline");
  y += scrubH + 10;

  // Play/pause button
  const label = isLoading ? "..." : isPlaying ? "PAUSE" : "PLAY";
  if (playBtn.txt !== label) playBtn.txt = label;
  playBtn.reposition({ center: "x", screen, y });
  playBtn.paint({ ink }, isPlaying
    ? [[0, 80, 70], [0, 200, 180], 255]
    : [[50, 40, 80], [120, 100, 160], 255]);
  y += playBtn.height + 4;

  // Time
  const timeStr = duration > 0
    ? `${fmtTime(currentTime)} / ${fmtTime(duration)}`
    : "--:-- / --:--";
  ink(150, 130, 170).write(timeStr, { center: "x", screen, y });
}

function togglePlay(send) {
  if (isLoading) return;
  if (isPlaying) {
    send({ type: "stream:pause", content: { id: STREAM_ID } });
  } else if (hasStarted) {
    send({ type: "stream:resume", content: { id: STREAM_ID } });
  } else {
    isLoading = true;
    send({ type: "stream:play", content: { id: STREAM_ID, url: audioUrl, volume: 0.8 } });
  }
}

function act({ event: e, screen, send }) {
  playBtn?.act(e, { push: () => togglePlay(send) });

  if (scrubBox && e.is("touch") && duration > 0) {
    const { x, y } = e;
    if (x >= scrubBox.x && x <= scrubBox.x + scrubBox.w &&
        y >= scrubBox.y && y <= scrubBox.y + scrubBox.h) {
      scrubber.start(e, (x - scrubBox.x) / scrubBox.w, isPlaying);
      if (isPlaying) send({ type: "stream:pause", content: { id: STREAM_ID } });
    }
  }
  if (scrubber.isScrubbing && e.is("draw")) scrubber.drag(e, screen.width);
  if (scrubber.isScrubbing && e.is("lift")) {
    const result = scrubber.end();
    if (result.wasScrubbing) {
      send({ type: "stream:seek", content: { id: STREAM_ID, time: result.finalProgress * duration } });
      if (result.wasPlayingBefore) send({ type: "stream:resume", content: { id: STREAM_ID } });
    }
  }

  if (e.is("keyboard:down:space")) togglePlay(send);
  if (e.is("keyboard:down:arrowleft") && duration > 0)
    send({ type: "stream:seek", content: { id: STREAM_ID, time: Math.max(0, currentTime - 10) } });
  if (e.is("keyboard:down:arrowright") && duration > 0)
    send({ type: "stream:seek", content: { id: STREAM_ID, time: Math.min(duration, currentTime + 10) } });
}

function sim({ send }) {
  send({ type: "stream:time", content: { id: STREAM_ID } });
  if (isPlaying) send({ type: "stream:frequencies", content: { id: STREAM_ID } });

  const simResult = scrubber.simulate();
  if (simResult && duration > 0)
    send({ type: "stream:seek", content: { id: STREAM_ID, time: simResult.needleProgress * duration } });

  if (frequencyData.length > 0) {
    const step = Math.max(1, Math.floor(frequencyData.length / bars.length));
    for (let i = 0; i < bars.length; i++) {
      bars[i] = bars[i] * 0.7 + ((frequencyData[i * step] || 0) / 255) * 0.3;
    }
  } else if (!isPlaying) {
    for (let i = 0; i < bars.length; i++) bars[i] *= 0.92;
  }
}

function receive({ type, content }) {
  if (content?.id !== STREAM_ID) return;
  if (type === "stream:playing") { isPlaying = true; isLoading = false; hasStarted = true; }
  if (type === "stream:paused") isPlaying = false;
  if (type === "stream:stopped") { isPlaying = false; hasStarted = false; }
  if (type === "stream:error") { isPlaying = false; isLoading = false; }
  if (type === "stream:time-data") {
    if (!scrubber.isScrubbing && !scrubber.inertiaActive) currentTime = content.currentTime;
    duration = content.duration;
    bufferedTime = content.buffered;
    if (content.ended && hasStarted) isPlaying = false;
  }
  if (type === "stream:seeked") currentTime = content.time;
  if (type === "stream:frequencies-data") frequencyData = content.data || [];
}

function leave({ send }) {
  send({ type: "stream:stop", content: { id: STREAM_ID } });
  scrubber.reset();
  isPlaying = false; hasStarted = false; currentTime = 0; duration = 0;
}

export { boot, paint, act, sim, receive, leave };

function fmtTime(sec) {
  if (!Number.isFinite(sec) || sec < 0) return "--:--";
  return `${Math.floor(sec / 60)}:${String(Math.floor(sec % 60)).padStart(2, "0")}`;
}
