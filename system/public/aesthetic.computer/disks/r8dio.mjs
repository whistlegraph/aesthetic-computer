// r8dio, 2024.12.04
// 📻 R8dio.dk live stream player
// Stream: https://s3.radio.co/s7cd1ffe2f/listen

/* #region 🏁 TODO
  - [x] Add visualizer via BIOS streaming API
  - [x] Show current program info
  + Done
#endregion */

const STREAM_URL = "https://s3.radio.co/s7cd1ffe2f/listen";
const STREAM_ID = "r8dio-stream";
const METADATA_URL = "https://public.radio.co/stations/s7cd1ffe2f/status";

let isPlaying = false;
let isLoading = false;
let loadError = null;
let volume = 0.5; // Start at 50%
let frequencyData = []; // For audio visualization
let animPhase = 0;
let currentTrack = ""; // Current track/program title
let lastMetadataFetch = 0;
const METADATA_INTERVAL = 15000; // Fetch metadata every 15 seconds

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

async function boot({ screen, net }) {
  // Initialize bars
  for (let i = 0; i < BAR_COUNT; i++) {
    bars.push({ height: 0, targetHeight: 0 });
  }
  
  // Fetch initial metadata
  fetchMetadata(net);
}

function paint({
  wipe,
  ink,
  screen,
  help,
  pen,
}) {
  // Dark background with slight purple tint (r8dio colors)
  wipe(25, 20, 35);
  
  const centerX = Math.floor(screen.width / 2);
  const centerY = Math.floor(screen.height / 2);
  
  // Draw visualizer bars
  const barWidth = Math.max(2, Math.floor((screen.width - 40) / BAR_COUNT));
  const barGap = 1;
  const totalWidth = (barWidth + barGap) * BAR_COUNT - barGap;
  const startX = Math.floor((screen.width - totalWidth) / 2);
  const maxBarHeight = Math.floor(screen.height * 0.4);
  const barBaseY = centerY + 30;
  
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
  
  // Title - "r8Dio" with "8D" in magenta
  const titleY = 20;
  const titleParts = [
    { text: "r", color: [255, 200, 220] },
    { text: "8D", color: [255, 0, 255] }, // Magenta
    { text: "io", color: [255, 200, 220] },
  ];
  
  // Calculate total width for centering
  const charWidth = 8; // approximate
  const totalTitleWidth = "r8Dio".length * charWidth;
  let titleX = Math.floor((screen.width - totalTitleWidth) / 2);
  
  for (const part of titleParts) {
    ink(...part.color).write(part.text, { x: titleX, y: titleY });
    titleX += part.text.length * charWidth;
  }
  
  ink(150, 120, 160).write("Danmarks snakke-radio", { center: "x", y: 36 }, undefined, undefined, false, "MatrixChunky8");
  
  // Current track info (scrolling if too long)
  if (currentTrack) {
    const trackY = centerY + 75;
    // Truncate if too long
    let displayTrack = currentTrack;
    const maxChars = Math.floor((screen.width - 20) / 5); // Approximate for small font
    if (displayTrack.length > maxChars) {
      displayTrack = displayTrack.substring(0, maxChars - 3) + "...";
    }
    ink(180, 150, 200).write(displayTrack, { center: "x", y: trackY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // Status indicator
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
  
  ink(...statusColor).write(statusText, { center: "x", y: centerY + 60 }, undefined, undefined, false, "MatrixChunky8");
  
  // Play/Pause button area
  const btnSize = 40;
  const btnX = centerX - btnSize / 2;
  const btnY = screen.height - 60;
  
  // Button background
  const isHovering = pen && 
    pen.x >= btnX && pen.x < btnX + btnSize &&
    pen.y >= btnY && pen.y < btnY + btnSize;
  
  ink(isHovering ? [80, 60, 100] : [50, 40, 70]).box(btnX, btnY, btnSize, btnSize);
  ink(isHovering ? [140, 100, 160] : [100, 80, 120]).box(btnX, btnY, btnSize, btnSize, "outline");
  
  // Play/Pause icon
  const iconColor = isHovering ? [255, 220, 255] : [200, 180, 220];
  if (isPlaying) {
    // Pause icon (two bars)
    const barW = 6;
    const barH = 16;
    const gap = 6;
    ink(...iconColor).box(centerX - gap/2 - barW, btnY + 12, barW, barH);
    ink(...iconColor).box(centerX + gap/2, btnY + 12, barW, barH);
  } else {
    // Play icon (triangle)
    const triX = centerX - 6;
    const triY = btnY + 12;
    ink(...iconColor).box(triX, triY, 4, 16);
    ink(...iconColor).box(triX + 4, triY + 2, 4, 12);
    ink(...iconColor).box(triX + 8, triY + 4, 4, 8);
    ink(...iconColor).box(triX + 12, triY + 6, 2, 4);
  }
  
  // Volume slider (bottom left)
  volSliderY = screen.height - 18;
  const isHoveringVol = pen && 
    pen.x >= volSliderX && pen.x < volSliderX + volSliderW &&
    pen.y >= volSliderY - 4 && pen.y < volSliderY + volSliderH + 4;
  
  // Volume background track
  ink(50, 40, 70).box(volSliderX, volSliderY, volSliderW, volSliderH);
  
  // Volume fill
  const fillColor = isDraggingVolume || isHoveringVol ? [180, 140, 220] : [150, 120, 180];
  ink(...fillColor).box(volSliderX, volSliderY, Math.floor(volSliderW * volume), volSliderH);
  
  // Volume handle
  const handleX = volSliderX + Math.floor(volSliderW * volume) - 2;
  const handleColor = isDraggingVolume || isHoveringVol ? [255, 220, 255] : [200, 180, 220];
  ink(...handleColor).box(handleX, volSliderY - 2, 4, volSliderH + 4);
  
  // Volume percentage
  const volPercent = Math.round(volume * 100);
  ink(100, 80, 120).write(`${volPercent}%`, { x: volSliderX + volSliderW + 6, y: volSliderY + 2 }, undefined, undefined, false, "MatrixChunky8");
}

function sim({ num: { lerp }, send, net }) {
  animPhase += 0.05;
  
  // Request frequency data for visualization
  if (isPlaying && send) {
    send({ type: "stream:frequencies", content: { id: STREAM_ID } });
  }
  
  // Periodically fetch metadata for current track info
  const now = Date.now();
  if (isPlaying && now - lastMetadataFetch > METADATA_INTERVAL) {
    fetchMetadata(net);
  }
  
  // Update bars
  for (let i = 0; i < BAR_COUNT; i++) {
    if (isPlaying && frequencyData.length > 0) {
      // Use real audio data
      const dataIndex = Math.floor((i / BAR_COUNT) * frequencyData.length);
      bars[i].targetHeight = (frequencyData[dataIndex] || 0) / 255;
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
  
  // Play/Pause button click
  const btnSize = 40;
  const centerX = Math.floor(screen.width / 2);
  const btnX = centerX - btnSize / 2;
  const btnY = screen.height - 60;
  
  // Volume slider interaction
  const volHitY = volSliderY - 4;
  const volHitH = volSliderH + 8;
  
  // Start dragging volume
  if (e.is("touch")) {
    if (e.x >= volSliderX && e.x < volSliderX + volSliderW &&
        e.y >= volHitY && e.y < volHitY + volHitH) {
      isDraggingVolume = true;
      updateVolumeFromX(e.x, clamp, send);
    }
  }
  
  // Continue dragging volume
  if (e.is("draw") && isDraggingVolume) {
    updateVolumeFromX(e.x, clamp, send);
  }
  
  // Stop dragging volume
  if (e.is("lift")) {
    if (isDraggingVolume) {
      isDraggingVolume = false;
    } else {
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

function updateVolumeFromX(x, clamp, send) {
  const newVol = clamp((x - volSliderX) / volSliderW, 0, 1);
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
      frequencyData = content.data || [];
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
