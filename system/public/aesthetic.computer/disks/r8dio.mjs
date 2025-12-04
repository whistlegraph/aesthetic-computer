// r8dio, 2024.12.04
// 📻 R8dio.dk live stream player
// Stream: https://s3.radio.co/s7cd1ffe2f/listen

/* #region 🏁 TODO
  - [x] Add visualizer via BIOS streaming API
  - [x] Show current program info
  - [x] Fix iOS Safari visualization (use time-domain fallback)
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
  text,
}) {
  // Dark background with slight purple tint (r8dio colors)
  wipe(25, 20, 35);
  
  const centerX = Math.floor(screen.width / 2);
  const centerY = Math.floor(screen.height / 2);
  
  // Layout constants - work from top and bottom to avoid overlaps
  // unifont at scale 1 is 16px tall
  const titleY = 10;
  const subtitleY = 30; // After title (10 + 16 + 4 padding)
  const visualizerTopY = 46;
  const visualizerBottomY = screen.height - 90;
  const statusY = screen.height - 78;
  const trackInfoY = screen.height - 66;
  const btnY = screen.height - 52;
  const volSliderYPos = screen.height - 14;
  
  // Draw visualizer bars (in the middle section)
  const barWidth = Math.max(2, Math.floor((screen.width - 40) / BAR_COUNT));
  const barGap = 1;
  const totalWidth = (barWidth + barGap) * BAR_COUNT - barGap;
  const startX = Math.floor((screen.width - totalWidth) / 2);
  const maxBarHeight = Math.min(80, visualizerBottomY - visualizerTopY - 10);
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
  
  // Status indicator (above track info)
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
  
  // Current track info (below status, above play button)
  if (currentTrack) {
    // Truncate if too long
    let displayTrack = currentTrack;
    const maxChars = Math.floor((screen.width - 20) / 5); // Approximate for small font
    if (displayTrack.length > maxChars) {
      displayTrack = displayTrack.substring(0, maxChars - 3) + "...";
    }
    ink(180, 150, 200).write(displayTrack, { center: "x", y: trackInfoY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // Play/Pause button area (smaller, positioned better)
  const btnSize = 28;
  const btnX = centerX - btnSize / 2;
  
  // Button background
  const isHovering = pen && 
    pen.x >= btnX && pen.x < btnX + btnSize &&
    pen.y >= btnY && pen.y < btnY + btnSize;
  
  ink(isHovering ? [80, 60, 100] : [50, 40, 70]).box(btnX, btnY, btnSize, btnSize);
  ink(isHovering ? [140, 100, 160] : [100, 80, 120]).box(btnX, btnY, btnSize, btnSize, "outline");
  
  // Play/Pause icon (scaled for smaller button)
  const iconColor = isHovering ? [255, 220, 255] : [200, 180, 220];
  if (isPlaying) {
    // Pause icon (two bars)
    const barW = 4;
    const barH = 12;
    const gap = 5;
    ink(...iconColor).box(centerX - gap/2 - barW, btnY + 8, barW, barH);
    ink(...iconColor).box(centerX + gap/2, btnY + 8, barW, barH);
  } else {
    // Play icon (triangle)
    const triX = centerX - 4;
    const triY = btnY + 8;
    ink(...iconColor).box(triX, triY, 3, 12);
    ink(...iconColor).box(triX + 3, triY + 2, 3, 8);
    ink(...iconColor).box(triX + 6, triY + 3, 2, 6);
    ink(...iconColor).box(triX + 8, triY + 5, 2, 2);
  }
  
  // Volume slider (bottom left, below play button)
  volSliderY = volSliderYPos;
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
  
  // Play/Pause button click (must match paint function)
  const btnSize = 28;
  const centerX = Math.floor(screen.width / 2);
  const btnX = centerX - btnSize / 2;
  const btnY = screen.height - 52;
  
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
