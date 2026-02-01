// 📻 Radio - Shared utilities for radio stream players
// Used by: r8dio.mjs, kpbj.mjs

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

const BAR_COUNT = 32;
const USE_WAVEFORM_THRESHOLD = 10;
const TOGGLE_DEBOUNCE = 300;
const METADATA_INTERVAL = 15000;

// Create initial radio state
export function createRadioState(config) {
  return {
    // Config
    streamUrl: config.streamUrl,
    streamId: config.streamId,
    metadataUrl: config.metadataUrl || null,
    qrUrl: config.qrUrl,
    
    // Playback state
    isPlaying: false,
    isLoading: false,
    loadError: null,
    volume: 0.5,
    
    // Visualization
    frequencyData: [],
    waveformData: [],
    animPhase: 0,
    bars: [],
    noAnalyserDataCount: 0,
    
    // Metadata
    currentTrack: "",
    lastMetadataFetch: 0,
    
    // UI state
    isDraggingVolume: false,
    isButtonPressed: false,
    
    // QR code
    qrCells: null,
    
    // Debounce
    lastToggleTime: 0,
    
    // Global send reference
    globalSend: null,
  };
}

// Initialize visualizer bars
export function initBars(state) {
  state.bars = [];
  for (let i = 0; i < BAR_COUNT; i++) {
    state.bars.push({ height: 0, targetHeight: 0 });
  }
}

// Generate QR code
export function generateQRCode(state) {
  state.qrCells = qr(state.qrUrl).modules;
}

// Update visualizer bars each frame
export function updateBars(state, { lerp }) {
  state.animPhase += 0.05;
  
  for (let i = 0; i < BAR_COUNT; i++) {
    if (state.isPlaying && state.frequencyData.length > 0) {
      const dataIndex = Math.floor((i / BAR_COUNT) * state.frequencyData.length);
      state.bars[i].targetHeight = (state.frequencyData[dataIndex] || 0) / 255;
      state.noAnalyserDataCount = 0;
    } else if (state.isPlaying && state.waveformData.length > 0) {
      const dataIndex = Math.floor((i / BAR_COUNT) * state.waveformData.length);
      const sample = state.waveformData[dataIndex] || 128;
      state.bars[i].targetHeight = Math.abs(sample - 128) / 128;
    } else if (state.isPlaying) {
      const wave = Math.sin(state.animPhase + i * 0.3) * 0.3 + 0.4;
      const noise = Math.random() * 0.2;
      state.bars[i].targetHeight = wave + noise;
    } else {
      state.bars[i].targetHeight = 0;
    }
    state.bars[i].height = lerp(state.bars[i].height, state.bars[i].targetHeight, 0.15);
  }
}

// Request frequency/waveform data from BIOS
export function requestVisualizerData(state, send) {
  if (state.isPlaying && send) {
    send({ type: "stream:frequencies", content: { id: state.streamId } });
    if (state.noAnalyserDataCount >= USE_WAVEFORM_THRESHOLD) {
      send({ type: "stream:waveform", content: { id: state.streamId } });
    }
  }
}

// Fetch metadata from API (if configured)
export async function fetchMetadata(state, net) {
  if (!state.metadataUrl) return;
  
  state.lastMetadataFetch = Date.now();
  try {
    const response = await fetch(state.metadataUrl);
    if (response.ok) {
      const data = await response.json();
      // Handle radio.co format
      if (data.current_track && data.current_track.title) {
        state.currentTrack = data.current_track.title;
      }
      // Handle AzuraCast format
      else if (data.now_playing && data.now_playing.song) {
        state.currentTrack = data.now_playing.song.title || data.now_playing.song.text || "";
      }
    }
  } catch (err) {
    console.log("📻 Could not fetch metadata:", err.message);
  }
}

// Check if metadata should be fetched
export function shouldFetchMetadata(state) {
  return state.isPlaying && Date.now() - state.lastMetadataFetch > METADATA_INTERVAL;
}

// Toggle playback
export function togglePlayback(state, send) {
  const now = Date.now();
  if (now - state.lastToggleTime < TOGGLE_DEBOUNCE) return;
  state.lastToggleTime = now;
  
  if (state.isLoading) return;
  
  if (state.isPlaying) {
    pausePlayback(state, send);
  } else {
    startPlayback(state, send);
  }
}

// Start playback
export function startPlayback(state, send) {
  if (state.isLoading || state.isPlaying) return;
  
  state.isLoading = true;
  state.loadError = null;
  const s = send || state.globalSend;
  if (s) {
    s({
      type: "stream:play",
      content: {
        id: state.streamId,
        url: state.streamUrl,
        volume: state.volume,
      },
    });
  }
}

// Pause playback
export function pausePlayback(state, send) {
  const s = send || state.globalSend;
  if (s) {
    s({ type: "stream:pause", content: { id: state.streamId } });
  }
}

// Stop playback
export function stopPlayback(state, send) {
  const s = send || state.globalSend;
  if (s) {
    s({ type: "stream:stop", content: { id: state.streamId } });
  }
  state.isPlaying = false;
  state.isLoading = false;
}

// Update volume from slider position
export function updateVolume(state, x, sliderX, sliderW, clamp, send) {
  state.volume = clamp((x - sliderX) / sliderW, 0, 1);
  const s = send || state.globalSend;
  if (s) {
    s({ type: "stream:volume", content: { id: state.streamId, volume: state.volume } });
  }
}

// Handle messages from BIOS
export function handleStreamMessage(state, { type, content }) {
  if (type === "stream:playing" && content.id === state.streamId) {
    state.isPlaying = true;
    state.isLoading = false;
    state.loadError = null;
  }
  
  if (type === "stream:paused" && content.id === state.streamId) {
    state.isPlaying = false;
  }
  
  if (type === "stream:stopped" && content.id === state.streamId) {
    state.isPlaying = false;
    state.isLoading = false;
  }
  
  if (type === "stream:error" && content.id === state.streamId) {
    state.isPlaying = false;
    state.isLoading = false;
    state.loadError = content.error;
  }
  
  if (type === "stream:frequencies-data" && content.id === state.streamId) {
    const data = content.data || [];
    if (data.length > 0 && data.some((v) => v > 0)) {
      state.frequencyData = data;
      state.noAnalyserDataCount = 0;
    } else {
      state.noAnalyserDataCount++;
      state.frequencyData = [];
    }
  }
  
  if (type === "stream:waveform-data" && content.id === state.streamId) {
    state.waveformData = content.data || [];
  }
}

// Reset state when leaving piece
export function resetState(state, send) {
  stopPlayback(state, send);
  state.frequencyData = [];
  state.waveformData = [];
  state.noAnalyserDataCount = 0;
  initBars(state);
}

// ============================================
// Drawing utilities
// ============================================

// Calculate responsive layout - adapts to screen size
// Pass qrCells to get accurate QR sizing
export function calcLayout(screen, theme, qrCells = null) {
  const w = screen.width;
  const h = screen.height;
  const centerX = Math.floor(w / 2);
  const centerY = Math.floor(h / 2);
  
  // Determine if we're in portrait/small or landscape/wide mode
  const isWide = w > h * 1.2;
  const isSmall = w < 180 || h < 200;
  
  // QR code sizing - use actual QR module count (usually 21, 25, 29, etc.)
  const qrModules = qrCells ? qrCells.length : 21;
  const qrScale = isSmall ? 1 : 2;
  const qrSize = qrModules * qrScale;
  const qrPadding = isSmall ? 4 : 8;
  
  // Margins and padding
  const margin = isSmall ? 4 : 10;
  
  // Title area at top - centered with proper spacing
  const titleY = isSmall ? 8 : 14;
  const subtitleY = titleY + (isSmall ? 14 : 20); // More space between title and subtitle
  
  // QR code position - bottom right corner, fully inside screen
  const qrX = w - qrSize - qrPadding;
  const qrY = h - qrSize - qrPadding;
  
  // Button sizing - responsive
  const btnSize = isSmall ? 24 : 32;
  
  // Main content area (between subtitle and bottom)
  const contentTop = subtitleY + (isSmall ? 16 : 26);
  const contentBottom = h - (isSmall ? 12 : 20);
  const contentHeight = contentBottom - contentTop;
  
  // Layout: visualizer on top, then button, then volume, then status
  // Calculate from center outward for better balance
  const btnY = centerY - btnSize / 2;
  const btnX = centerX - btnSize / 2;
  
  // Volume slider below button (centered)
  const volSliderW = Math.min(100, w - 40);
  const volSliderH = isSmall ? 8 : 12;
  const volSliderX = centerX - volSliderW / 2;
  const volSliderY = btnY + btnSize + (isSmall ? 10 : 14);
  
  // Visualizer above button
  const visualizerBottomY = btnY - (isSmall ? 12 : 18);
  const visualizerTopY = contentTop;
  
  // Visualizer bars - centered, full width minus margins
  const barAreaWidth = w - margin * 2;
  const barWidth = Math.max(2, Math.floor(barAreaWidth / BAR_COUNT) - 1);
  const barGap = 1;
  const totalBarWidth = (barWidth + barGap) * BAR_COUNT - barGap;
  const barStartX = Math.floor((w - totalBarWidth) / 2);
  const maxBarHeight = Math.max(15, visualizerBottomY - visualizerTopY - 6);
  
  // Status below volume slider, left-aligned to avoid QR
  const statusY = volSliderY + volSliderH + (isSmall ? 8 : 14);
  const trackInfoY = statusY + (isSmall ? 10 : 14);
  
  return {
    centerX,
    centerY,
    qrScale,
    qrSize,
    qrX,
    qrY,
    titleY,
    subtitleY,
    btnSize,
    btnX,
    btnY,
    volSliderW,
    volSliderH,
    volSliderX,
    volSliderY,
    barWidth,
    barGap,
    barStartX,
    maxBarHeight,
    visualizerBottomY,
    statusY,
    trackInfoY,
    screenWidth: w,
    screenHeight: h,
    isSmall,
    isWide,
  };
}

// Draw visualizer bars
export function drawVisualizerBars(ctx, state, theme, layout) {
  const { ink, box, line } = ctx;
  const { barStartX, barWidth, barGap, maxBarHeight, visualizerBottomY } = layout;
  
  for (let i = 0; i < BAR_COUNT; i++) {
    const bar = state.bars[i];
    const x = barStartX + i * (barWidth + barGap);
    const height = Math.floor(bar.height * maxBarHeight);
    
    if (height > 0) {
      const t = bar.height;
      const color = theme.barColor(t);
      ink(color.r, color.g, color.b).box(x, visualizerBottomY - height, barWidth, height);
      ink(color.r, color.g, color.b, 50).box(x, visualizerBottomY + 2, barWidth, Math.floor(height * 0.3));
    }
  }
  
  // Base line
  const totalWidth = (barWidth + barGap) * BAR_COUNT - barGap;
  ink(...theme.baseLine).line(barStartX - 10, visualizerBottomY, barStartX + totalWidth + 10, visualizerBottomY);
}

// Draw play/pause button
export function drawPlayButton(ctx, state, theme, layout, pen, help) {
  const { ink, box } = ctx;
  const { btnX, btnY, btnSize, centerX } = layout;
  
  const isHovering =
    !state.isLoading &&
    pen &&
    pen.x >= btnX &&
    pen.x < btnX + btnSize &&
    pen.y >= btnY &&
    pen.y < btnY + btnSize;
  
  // Button background
  if (state.isButtonPressed) {
    ink(theme.buttonPressed).box(btnX, btnY, btnSize, btnSize);
    ink(theme.buttonPressedOutline).box(btnX, btnY, btnSize, btnSize, "outline");
  } else if (state.isLoading) {
    ink(theme.buttonBg).box(btnX, btnY, btnSize, btnSize);
    ink(theme.buttonOutline).box(btnX, btnY, btnSize, btnSize, "outline");
  } else {
    ink(isHovering ? theme.buttonHover : theme.buttonBg).box(btnX, btnY, btnSize, btnSize);
    ink(isHovering ? theme.buttonHoverOutline : theme.buttonOutline).box(btnX, btnY, btnSize, btnSize, "outline");
  }
  
  // Icon
  const pressOffset = state.isButtonPressed ? 1 : 0;
  const iconColor = state.isButtonPressed
    ? theme.iconPressed
    : isHovering
      ? theme.iconHover
      : theme.icon;
  const iconCenterY = btnY + btnSize / 2 + pressOffset;
  
  if (state.isLoading) {
    // Loading spinner
    const loadingPhase = (help.repeat / 10) % 8;
    for (let i = 0; i < 8; i++) {
      const angle = (i / 8) * Math.PI * 2 - Math.PI / 2;
      const radius = 8;
      const dotX = centerX + Math.cos(angle) * radius;
      const dotY = btnY + btnSize / 2 + Math.sin(angle) * radius;
      const brightness = ((i + Math.floor(loadingPhase)) % 8) / 8;
      const c = theme.spinnerColor(brightness);
      ink(c.r, c.g, c.b).box(dotX - 2, dotY - 2, 4, 4);
    }
  } else if (state.isPlaying) {
    // Pause icon
    const barW = 5;
    const barH = 14;
    const gap = 6;
    ink(...iconColor).box(centerX - gap / 2 - barW + pressOffset, iconCenterY - barH / 2, barW, barH);
    ink(...iconColor).box(centerX + gap / 2 + pressOffset, iconCenterY - barH / 2, barW, barH);
  } else {
    // Play icon
    const triX = centerX - 5 + pressOffset;
    const triY = iconCenterY - 7;
    ink(...iconColor).box(triX, triY, 3, 14);
    ink(...iconColor).box(triX + 3, triY + 2, 3, 10);
    ink(...iconColor).box(triX + 6, triY + 4, 3, 6);
    ink(...iconColor).box(triX + 9, triY + 6, 2, 2);
  }
}

// Draw volume slider
export function drawVolumeSlider(ctx, state, theme, layout, pen) {
  const { ink, box, write } = ctx;
  const { volSliderX, volSliderY, volSliderW, volSliderH } = layout;
  
  const isHoveringVol =
    !state.isLoading &&
    pen &&
    pen.x >= volSliderX &&
    pen.x < volSliderX + volSliderW &&
    pen.y >= volSliderY - 4 &&
    pen.y < volSliderY + volSliderH + 4;
  
  const alpha = state.isLoading ? 100 : 255;
  
  // Background track
  ink(...theme.volTrack, alpha).box(volSliderX, volSliderY, volSliderW, volSliderH);
  
  // Fill
  const fillColor = state.isDraggingVolume || isHoveringVol ? theme.volFillHover : theme.volFill;
  ink(...fillColor, alpha).box(volSliderX, volSliderY, Math.floor(volSliderW * state.volume), volSliderH);
  
  // Handle
  const handleX = volSliderX + Math.floor(volSliderW * state.volume) - 2;
  const handleColor = state.isDraggingVolume || isHoveringVol ? theme.volHandleHover : theme.volHandle;
  ink(...handleColor, alpha).box(handleX, volSliderY - 2, 4, volSliderH + 4);
  
  // Percentage
  const volPercent = Math.round(state.volume * 100);
  ink(...theme.volText, alpha).write(`${volPercent}%`, { x: volSliderX + volSliderW + 6, y: volSliderY + 2 }, undefined, undefined, false, "MatrixChunky8");
}

// Draw QR code - positioned in bottom right corner
export function drawQRCode(ctx, state, theme, layout) {
  const { ink, box } = ctx;
  const { qrX, qrY, qrScale, qrSize } = layout;
  
  if (!state.qrCells) return;
  
  // Draw QR modules
  for (let y = 0; y < state.qrCells.length; y++) {
    for (let x = 0; x < state.qrCells.length; x++) {
      const isBlack = state.qrCells[y][x];
      if (isBlack) {
        ink(...theme.qrFg).box(qrX + x * qrScale, qrY + y * qrScale, qrScale, qrScale);
      } else {
        ink(...theme.qrBg).box(qrX + x * qrScale, qrY + y * qrScale, qrScale, qrScale);
      }
    }
  }
  
  // Outline
  ink(...theme.qrOutline).box(qrX - 1, qrY - 1, qrSize + 2, qrSize + 2, "outline");
}

// Draw status text - centered
export function drawStatus(ctx, state, theme, layout, help) {
  const { ink, write } = ctx;
  const { statusY, trackInfoY, screenWidth, qrX, qrY, isSmall, centerX } = layout;
  
  let statusText, statusColor;
  if (state.loadError) {
    statusText = "Connection error";
    statusColor = theme.statusError;
  } else if (state.isLoading) {
    statusText = "Connecting" + ".".repeat(Math.floor(help.repeat / 20) % 4);
    statusColor = theme.statusLoading;
  } else if (state.isPlaying) {
    statusText = "● LIVE";
    statusColor = theme.statusLive;
  } else {
    statusText = "Paused";
    statusColor = theme.statusPaused;
  }
  
  // Only show status if it won't overlap with QR code
  if (statusY < qrY - 4) {
    ink(...statusColor).write(statusText, { center: "x", y: statusY }, undefined, undefined, false, "MatrixChunky8");
  
    // Track info - centered, truncate if needed
    if (state.currentTrack) {
      let displayTrack = state.currentTrack;
      const maxChars = Math.floor((screenWidth - 20) / 5);
      if (displayTrack.length > maxChars) {
        displayTrack = displayTrack.substring(0, maxChars - 3) + "...";
      }
      if (trackInfoY < qrY - 4) {
        ink(...theme.trackText).write(displayTrack, { center: "x", y: trackInfoY }, undefined, undefined, false, "MatrixChunky8");
      }
    }
  }
}

// Draw title and subtitle
export function drawTitle(ctx, theme, layout) {
  const { ink, write } = ctx;
  const { titleY, subtitleY } = layout;
  
  ink(...theme.title).write(theme.titleText, { center: "x", y: titleY }, undefined, undefined, false, "unifont");
  ink(...theme.subtitle).write(theme.subtitleText, { center: "x", y: subtitleY }, undefined, undefined, false, "MatrixChunky8");
}

// Handle button/volume interaction
export function handleInteraction(state, e, layout, clamp, send) {
  const { btnX, btnY, btnSize, volSliderX, volSliderY, volSliderW, volSliderH } = layout;
  const volHitY = volSliderY - 4;
  const volHitH = volSliderH + 8;
  
  if (e.is("touch")) {
    // Volume slider
    if (!state.isLoading && e.x >= volSliderX && e.x < volSliderX + volSliderW && e.y >= volHitY && e.y < volHitY + volHitH) {
      state.isDraggingVolume = true;
      updateVolume(state, e.x, volSliderX, volSliderW, clamp, send);
    } else if (!state.isLoading && e.x >= btnX && e.x < btnX + btnSize && e.y >= btnY && e.y < btnY + btnSize) {
      state.isButtonPressed = true;
    }
  }
  
  if (e.is("draw") && state.isDraggingVolume) {
    updateVolume(state, e.x, volSliderX, volSliderW, clamp, send);
  }
  
  if (e.is("lift")) {
    if (state.isDraggingVolume) {
      state.isDraggingVolume = false;
    } else if (state.isButtonPressed) {
      state.isButtonPressed = false;
      if (e.x >= btnX && e.x < btnX + btnSize && e.y >= btnY && e.y < btnY + btnSize) {
        togglePlayback(state, send);
      }
    }
  }
  
  if (e.is("keyboard:down:space")) {
    togglePlayback(state, send);
  }
}
