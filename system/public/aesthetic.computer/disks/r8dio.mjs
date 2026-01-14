// r8dio, 2024.12.04
// 📻 R8dio.dk live stream player
// Stream: https://s3.radio.co/s7cd1ffe2f/listen

/* #region 🏁 TODO
  - [x] Add visualizer via BIOS streaming API
  - [x] Show current program info
  - [x] Fix iOS Safari visualization (use time-domain fallback)
  - [x] Add QR code linking to prompt.ac/r8dio
  - [x] Add pressed state for play button
  + Done
#endregion */

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

const STREAM_URL = "https://s3.radio.co/s7cd1ffe2f/listen";
const STREAM_ID = "r8dio-stream";
const METADATA_URL = "https://public.radio.co/stations/s7cd1ffe2f/status";

let isPlaying = false;
let isLoading = false;
let loadError = null;
let volume = 0.5; // Start at 50%
let frequencyData = []; // For audio visualization
let waveformData = []; // Time-domain data (fallback for iOS)
let animPhase = 0;
let currentTrack = ""; // Current track/program title
let lastMetadataFetch = 0;
const METADATA_INTERVAL = 15000; // Fetch metadata every 15 seconds
let noAnalyserDataCount = 0; // Track consecutive empty data responses
const USE_WAVEFORM_THRESHOLD = 10; // Switch to waveform after this many empty responses

// Volume slider state
let isDraggingVolume = false;
let volSliderX = 10;
let volSliderY = 0; // Set in paint based on screen height
let volSliderW = 80;
let volSliderH = 12;

// 📻 Radio visualization bars
let bars = [];
const BAR_COUNT = 32;

// Store send function for use in other functions
let globalSend = null;

// QR Code for sharing
let qrCells = null;
const QR_URL = "https://prompt.ac/r8dio";

// Button pressed state
let isButtonPressed = false;

async function boot({ screen, net }) {
  // Initialize bars
  for (let i = 0; i < BAR_COUNT; i++) {
    bars.push({ height: 0, targetHeight: 0 });
  }
  
  // Generate QR code
  qrCells = qr(QR_URL).modules;
  
  // Fetch initial metadata
  fetchMetadata(net);
}

function paint({
  wipe,
  ink,
  screen,
  help,
  pen,
  text,
}) {
  // Dark background with slight purple tint (r8dio colors)
  wipe(25, 20, 35);
  
  const centerX = Math.floor(screen.width / 2);
  const centerY = Math.floor(screen.height / 2);
  
  // QR code layout - top right corner (bigger)
  const qrScale = Math.max(2, Math.floor(Math.min(screen.width, 80) / (qrCells?.length || 21)));
  const qrSize = (qrCells?.length || 21) * qrScale;
  const qrX = screen.width - qrSize - 6;
  const qrY = 6;
  const listenY = qrY + qrSize + 4;
  
  // Layout constants - centered play button
  const titleY = centerY - 70; // Title above visualizer
  const subtitleY = titleY + 18;
  const visualizerTopY = subtitleY + 14;
  const btnSize = 32;
  const btnY = centerY - btnSize / 2; // Centered vertically
  const volSliderYPos = btnY + btnSize + 8; // Volume right below button
  const visualizerBottomY = btnY - 12; // Visualizer ends above button
  
  // Status and track at bottom
  const statusY = screen.height - 28;
  const trackInfoY = screen.height - 14;
  
  // Draw visualizer bars (fills space between subtitle and button)
  const barWidth = Math.max(2, Math.floor((screen.width - 40) / BAR_COUNT));
  const barGap = 1;
  const totalWidth = (barWidth + barGap) * BAR_COUNT - barGap;
  const startX = Math.floor((screen.width - totalWidth) / 2);
  const maxBarHeight = Math.max(20, visualizerBottomY - visualizerTopY - 10);
  const barBaseY = visualizerBottomY;
  
  for (let i = 0; i < BAR_COUNT; i++) {
    const bar = bars[i];
    const x = startX + i * (barWidth + barGap);
    const height = Math.floor(bar.height * maxBarHeight);
    
    if (height > 0) {
      // Gradient color based on height - purple to pink to white
      const t = bar.height;
      const r = Math.floor(120 + t * 135);
      const g = Math.floor(60 + t * 100);
      const b = Math.floor(180 + t * 75);
      
      ink(r, g, b).box(x, barBaseY - height, barWidth, height);
      
      // Reflection below (subtle)
      ink(r, g, b, 50).box(x, barBaseY + 2, barWidth, Math.floor(height * 0.3));
    }
  }
  
  // Draw base line
  ink(80, 60, 100).line(startX - 10, barBaseY, startX + totalWidth + 10, barBaseY);
  
  // Title - "r8Dio" using unifont with color codes
  ink(255, 200, 220).write("\\pink\\r\\magenta\\8D\\pink\\io\\reset\\", { center: "x", y: titleY }, undefined, undefined, false, "unifont");
  
  // Subtitle
  ink(150, 120, 160).write("Danmarks snakke-radio", { center: "x", y: subtitleY }, undefined, undefined, false, "MatrixChunky8");
  
  // Play/Pause button - centered on screen like baktok
  const btnX = centerX - btnSize / 2;
  
  // Button background - with pressed state
  const isHovering = pen && 
    pen.x >= btnX && pen.x < btnX + btnSize &&
    pen.y >= btnY && pen.y < btnY + btnSize;
  
  // Pressed state: darker/inset look
  if (isButtonPressed) {
    ink([30, 25, 45]).box(btnX, btnY, btnSize, btnSize);
    ink([70, 50, 90]).box(btnX, btnY, btnSize, btnSize, "outline");
  } else {
    ink(isHovering ? [80, 60, 100] : [50, 40, 70]).box(btnX, btnY, btnSize, btnSize);
    ink(isHovering ? [140, 100, 160] : [100, 80, 120]).box(btnX, btnY, btnSize, btnSize, "outline");
  }
  
  // Play/Pause icon - slightly offset when pressed
  const pressOffset = isButtonPressed ? 1 : 0;
  const iconColor = isButtonPressed ? [150, 130, 170] : (isHovering ? [255, 220, 255] : [200, 180, 220]);
  const iconCenterY = btnY + btnSize / 2 + pressOffset;
  if (isPlaying) {
    // Pause icon (two bars)
    const barW = 5;
    const barH = 14;
    const gap = 6;
    ink(...iconColor).box(centerX - gap/2 - barW + pressOffset, iconCenterY - barH/2, barW, barH);
    ink(...iconColor).box(centerX + gap/2 + pressOffset, iconCenterY - barH/2, barW, barH);
  } else {
    // Play icon (triangle)
    const triX = centerX - 5 + pressOffset;
    const triY = iconCenterY - 7;
    ink(...iconColor).box(triX, triY, 3, 14);
    ink(...iconColor).box(triX + 3, triY + 2, 3, 10);
    ink(...iconColor).box(triX + 6, triY + 4, 3, 6);
    ink(...iconColor).box(triX + 9, triY + 6, 2, 2);
  }
  
  // Volume slider - centered, right below play button
  volSliderY = volSliderYPos;
  const volSliderCenterX = centerX - volSliderW / 2;
  const isHoveringVol = pen && 
    pen.x >= volSliderCenterX && pen.x < volSliderCenterX + volSliderW &&
    pen.y >= volSliderY - 4 && pen.y < volSliderY + volSliderH + 4;
  
  // Volume background track
  ink(50, 40, 70).box(volSliderCenterX, volSliderY, volSliderW, volSliderH);
  
  // Volume fill
  const fillColor = isDraggingVolume || isHoveringVol ? [180, 140, 220] : [150, 120, 180];
  ink(...fillColor).box(volSliderCenterX, volSliderY, Math.floor(volSliderW * volume), volSliderH);
  
  // Volume handle
  const handleX = volSliderCenterX + Math.floor(volSliderW * volume) - 2;
  const handleColor = isDraggingVolume || isHoveringVol ? [255, 220, 255] : [200, 180, 220];
  ink(...handleColor).box(handleX, volSliderY - 2, 4, volSliderH + 4);
  
  // Volume percentage (to the right of slider)
  const volPercent = Math.round(volume * 100);
  ink(100, 80, 120).write(`${volPercent}%`, { x: volSliderCenterX + volSliderW + 6, y: volSliderY + 2 }, undefined, undefined, false, "MatrixChunky8");
  
  // Status indicator (bottom area)
  let statusText, statusColor;
  if (loadError) {
    statusText = "Connection error";
    statusColor = [255, 100, 100];
  } else if (isLoading) {
    statusText = "Connecting" + ".".repeat((Math.floor(help.repeat / 20) % 4));
    statusColor = [255, 200, 100];
  } else if (isPlaying) {
    statusText = "● LIVE";
    statusColor = [100, 255, 150];
  } else {
    statusText = "Paused";
    statusColor = [150, 150, 150];
  }
  
  ink(...statusColor).write(statusText, { center: "x", y: statusY }, undefined, undefined, false, "MatrixChunky8");
  
  // Current track info (at bottom)
  if (currentTrack) {
    // Truncate if too long
    let displayTrack = currentTrack;
    const maxChars = Math.floor((screen.width - 20) / 5);
    if (displayTrack.length > maxChars) {
      displayTrack = displayTrack.substring(0, maxChars - 3) + "...";
    }
    ink(180, 150, 200).write(displayTrack, { center: "x", y: trackInfoY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // QR Code - top right corner
  if (qrCells) {
    for (let y = 0; y < qrCells.length; y++) {
      for (let x = 0; x < qrCells.length; x++) {
        const isBlack = qrCells[y][x];
        if (isBlack) {
          ink(200, 180, 220).box(qrX + x * qrScale, qrY + y * qrScale, qrScale, qrScale);
        } else {
          ink(40, 30, 55).box(qrX + x * qrScale, qrY + y * qrScale, qrScale, qrScale);
        }
      }
    }
    
    // QR outline
    ink(100, 80, 120).box(qrX - 1, qrY - 1, qrSize + 2, qrSize + 2, "outline");
    
    // "listen" text below QR (right-aligned)
    ink(150, 120, 160).write("listen", { x: qrX + qrSize / 2 - 12, y: listenY }, undefined, undefined, false, "MatrixChunky8");
  }
}

function sim({ num: { lerp }, send, net }) {
  animPhase += 0.05;
  
  // Request frequency data for visualization (and waveform as fallback for iOS)
  if (isPlaying && send) {
    send({ type: "stream:frequencies", content: { id: STREAM_ID } });
    // Also request waveform data as fallback for iOS Safari
    if (noAnalyserDataCount >= USE_WAVEFORM_THRESHOLD) {
      send({ type: "stream:waveform", content: { id: STREAM_ID } });
    }
  }
  
  // Periodically fetch metadata for current track info
  const now = Date.now();
  if (isPlaying && now - lastMetadataFetch > METADATA_INTERVAL) {
    fetchMetadata(net);
  }
  
  // Update bars
  for (let i = 0; i < BAR_COUNT; i++) {
    if (isPlaying && frequencyData.length > 0) {
      // Use real frequency data
      const dataIndex = Math.floor((i / BAR_COUNT) * frequencyData.length);
      bars[i].targetHeight = (frequencyData[dataIndex] || 0) / 255;
      noAnalyserDataCount = 0; // Reset counter when we get data
    } else if (isPlaying && waveformData.length > 0) {
      // Use waveform data as fallback (iOS Safari)
      const dataIndex = Math.floor((i / BAR_COUNT) * waveformData.length);
      // Waveform is centered at 128, convert to 0-1 amplitude
      const sample = waveformData[dataIndex] || 128;
      bars[i].targetHeight = Math.abs(sample - 128) / 128;
    } else if (isPlaying) {
      // Fake animation while playing without analyser data
      const wave = Math.sin(animPhase + i * 0.3) * 0.3 + 0.4;
      const noise = Math.random() * 0.2;
      bars[i].targetHeight = wave + noise;
    } else {
      bars[i].targetHeight = 0;
    }
    
    // Smooth interpolation
    bars[i].height = lerp(bars[i].height, bars[i].targetHeight, 0.15);
  }
}

function act({ event: e, jump, screen, num: { clamp }, send }) {
  // Store send function globally for use in other functions
  globalSend = send;
  
  const centerX = Math.floor(screen.width / 2);
  const centerY = Math.floor(screen.height / 2);
  
  // Play/Pause button - centered on screen (must match paint)
  const btnSize = 32;
  const btnX = centerX - btnSize / 2;
  const btnY = centerY - btnSize / 2; // Match paint layout
  
  // Volume slider - centered below button (must match paint)
  const volSliderCenterX = centerX - volSliderW / 2;
  const volSliderYPos = btnY + btnSize + 8;
  volSliderY = volSliderYPos;
  
  // Volume slider interaction
  const volHitY = volSliderY - 4;
  const volHitH = volSliderH + 8;
  
  // Start dragging volume or pressing button
  if (e.is("touch")) {
    if (e.x >= volSliderCenterX && e.x < volSliderCenterX + volSliderW &&
        e.y >= volHitY && e.y < volHitY + volHitH) {
      isDraggingVolume = true;
      updateVolumeFromX(e.x, volSliderCenterX, clamp, send);
    } else if (e.x >= btnX && e.x < btnX + btnSize &&
               e.y >= btnY && e.y < btnY + btnSize) {
      isButtonPressed = true;
    }
  }
  
  // Continue dragging volume
  if (e.is("draw") && isDraggingVolume) {
    updateVolumeFromX(e.x, volSliderCenterX, clamp, send);
  }
  
  // Stop dragging volume or release button
  if (e.is("lift")) {
    if (isDraggingVolume) {
      isDraggingVolume = false;
    } else if (isButtonPressed) {
      isButtonPressed = false;
      // Check for play/pause button click
      if (e.x >= btnX && e.x < btnX + btnSize &&
          e.y >= btnY && e.y < btnY + btnSize) {
        togglePlayback(send);
      }
    }
  }
  
  // Keyboard controls
  if (e.is("keyboard:down:space")) {
    togglePlayback(send);
  }
  
  if (e.is("keyboard:down:escape")) {
    stopPlayback(send);
    jump("prompt");
  }
}

function updateVolumeFromX(x, sliderX, clamp, send) {
  const newVol = clamp((x - sliderX) / volSliderW, 0, 1);
  volume = newVol;
  if (globalSend) {
    globalSend({ type: "stream:volume", content: { id: STREAM_ID, volume } });
  }
}

// Handle messages from BIOS
function receive({ type, content }) {
  if (type === "stream:playing") {
    if (content.id === STREAM_ID) {
      isPlaying = true;
      isLoading = false;
      loadError = null;
    }
  }
  
  if (type === "stream:paused") {
    if (content.id === STREAM_ID) {
      isPlaying = false;
    }
  }
  
  if (type === "stream:stopped") {
    if (content.id === STREAM_ID) {
      isPlaying = false;
      isLoading = false;
    }
  }
  
  if (type === "stream:error") {
    if (content.id === STREAM_ID) {
      isPlaying = false;
      isLoading = false;
      loadError = content.error;
    }
  }
  
  if (type === "stream:frequencies-data") {
    if (content.id === STREAM_ID) {
      const data = content.data || [];
      if (data.length > 0 && data.some(v => v > 0)) {
        frequencyData = data;
        noAnalyserDataCount = 0;
      } else {
        // Track consecutive empty responses (iOS Safari issue)
        noAnalyserDataCount++;
        frequencyData = [];
      }
    }
  }
  
  if (type === "stream:waveform-data") {
    if (content.id === STREAM_ID) {
      waveformData = content.data || [];
    }
  }
}

// Debounce to prevent rapid toggling
let lastToggleTime = 0;
const TOGGLE_DEBOUNCE = 300; // ms

function togglePlayback(send) {
  const now = Date.now();
  if (now - lastToggleTime < TOGGLE_DEBOUNCE) return;
  lastToggleTime = now;
  
  // Don't toggle while loading
  if (isLoading) return;
  
  if (isPlaying) {
    pausePlayback(send);
  } else {
    startPlayback(send);
  }
}

function startPlayback(send) {
  if (isLoading || isPlaying) return; // Prevent double starts
  
  isLoading = true;
  loadError = null;
  const s = send || globalSend;
  if (s) {
    s({ 
      type: "stream:play", 
      content: { 
        id: STREAM_ID, 
        url: STREAM_URL, 
        volume 
      } 
    });
  }
}

function pausePlayback(send) {
  const s = send || globalSend;
  if (s) {
    s({ type: "stream:pause", content: { id: STREAM_ID } });
  }
  // Don't set isPlaying = false here - wait for confirmation from BIOS
}

function stopPlayback(send) {
  const s = send || globalSend;
  if (s) {
    s({ type: "stream:stop", content: { id: STREAM_ID } });
  }
  isPlaying = false;
  isLoading = false;
}

// Fetch current track metadata from radio.co API
async function fetchMetadata(net) {
  lastMetadataFetch = Date.now();
  try {
    const response = await fetch(METADATA_URL);
    if (response.ok) {
      const data = await response.json();
      if (data.current_track && data.current_track.title) {
        currentTrack = data.current_track.title;
      }
    }
  } catch (err) {
    // Silently fail - metadata is optional
    console.log("📻 Could not fetch metadata:", err.message);
  }
}

function leave({ send }) {
  // Stop the stream when leaving the piece
  stopPlayback(send);
  frequencyData = [];
  waveformData = [];
  noAnalyserDataCount = 0;
  bars = [];
  for (let i = 0; i < BAR_COUNT; i++) {
    bars.push({ height: 0, targetHeight: 0 });
  }
}

function meta() {
  return {
    title: "r8Dio",
    desc: "Listen to R8dio.dk live - Danmarks snakke-radio",
  };
}

export { boot, paint, sim, act, receive, leave, meta };
