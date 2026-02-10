// kpbj, 2026.02.01
// 📻 KPBJ.FM live stream player - Shadow Hills Community Radio
// Stream: https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3

/* #region 🏁 TODO
  - [ ] Test on mobile/iOS
  + Done
#endregion */

import {
  createRadioState,
  initBars,
  generateQRCode,
  updateBars,
  requestVisualizerData,
  handleStreamMessage,
  resetState,
  calcLayout,
  drawVisualizerBars,
  drawPlayButton,
  drawVolumeSlider,
  drawQRCode,
  drawTitle,
  handleInteraction,
  stopPlayback,
} from "../lib/radio.mjs";

// KPBJ Configuration
const CONFIG = {
  streamUrl: "https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3",
  streamId: "kpbj-stream",
  metadataUrl: "https://kpbj.hasnoskills.com/api/nowplaying/kpbj_test_station",
  playoutNowUrl: "https://kpbj.fm/api/playout/now",
  playoutFallbackUrl: "https://kpbj.fm/api/playout/fallback",
  qrUrl: "https://prompt.ac/kpbj",
  qrLabel: "prompt.ac/kpbj",
  websiteUrl: "https://kpbj.fm",
  websiteLabel: "kpbj.fm",
};

// KPBJ-specific state for playout info
let playoutState = {
  currentShow: null,       // From /playout/now - null means nothing scheduled
  ephemera: null,          // From /playout/fallback - fun random text
  lastPlayoutFetch: 0,
  lastMetadataFetch: 0,
  ephemeraHovered: false,
  websiteHovered: false,
  qrLabelHovered: false,
  networkLoaded: false,    // Track if initial network fetch completed
};

// KPBJ Theme - Sun Valley mountain/nature aesthetic
const THEME = {
  // Background - deep mountain blue
  bg: [20, 30, 45],
  
  // Title
  title: [255, 200, 140],
  titleText: "\\yellow\\K\\orange\\P\\yellow\\B\\orange\\J\\reset\\",
  subtitle: [160, 140, 120],
  subtitleText: "Shadow Hills Community Radio",
  
  // Visualizer bar color gradient (sunrise over mountains)
  barColor: (t) => ({
    r: Math.floor(200 + t * 55),
    g: Math.floor(100 + t * 100),
    b: Math.floor(60 + t * 80),
  }),
  baseLine: [80, 70, 60],
  
  // Button
  buttonBg: [60, 50, 40],
  buttonHover: [80, 70, 55],
  buttonOutline: [120, 100, 80],
  buttonHoverOutline: [160, 130, 100],
  buttonPressed: [30, 25, 20],
  buttonPressedOutline: [70, 60, 50],
  
  // Icon
  icon: [200, 170, 130],
  iconHover: [255, 220, 170],
  iconPressed: [150, 130, 100],
  
  // Spinner
  spinnerColor: (brightness) => ({
    r: Math.floor(100 + brightness * 155),
    g: Math.floor(80 + brightness * 120),
    b: Math.floor(40 + brightness * 90),
  }),
  
  // Volume slider
  volTrack: [60, 50, 40],
  volFill: [180, 130, 80],
  volFillHover: [220, 160, 100],
  volHandle: [200, 170, 130],
  volHandleHover: [255, 220, 170],
  volText: [120, 100, 80],
  
  // QR code
  qrFg: [220, 180, 140],
  qrBg: [35, 45, 60],
  qrOutline: [100, 90, 70],
  qrLabel: [160, 140, 120],
  
  // Status
  statusError: [255, 100, 100],
  statusLoading: [255, 200, 100],
  statusLive: [100, 255, 150],
  statusPaused: [150, 150, 150],
  trackText: [180, 160, 130],
  
  // Background logo
  logoBg: [30, 40, 55],
};

// ASCII art logo for background
const LOGO_ART = [
  "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.",
  "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪",
  "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·",
  "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌",
  "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀",
];

// State
let state;
let layout;

// Fetch playout info from KPBJ API
async function fetchPlayoutInfo(forceRefresh = false) {
  const now = Date.now();
  // Fetch every 30 seconds, or immediately if forced
  if (!forceRefresh && now - playoutState.lastPlayoutFetch < 30000) return;
  playoutState.lastPlayoutFetch = now;
  
  try {
    // Fetch current show (null if nothing scheduled)
    const nowRes = await fetch(CONFIG.playoutNowUrl);
    if (nowRes.ok) {
      const data = await nowRes.json();
      playoutState.currentShow = data; // null means nothing scheduled
    } else {
      playoutState.currentShow = null;
    }
  } catch (err) {
    console.log("📻 Could not fetch playout/now:", err.message);
    playoutState.currentShow = null;
  }
  
  try {
    // Fetch ephemera/fallback
    const fallbackRes = await fetch(CONFIG.playoutFallbackUrl);
    if (fallbackRes.ok) {
      const data = await fallbackRes.json();
      playoutState.ephemera = data;
    }
  } catch (err) {
    console.log("📻 Could not fetch playout/fallback:", err.message);
  }
  
  playoutState.networkLoaded = true;
}

// Fetch metadata regardless of playback state
async function fetchKPBJMetadata() {
  const now = Date.now();
  // Fetch every 15 seconds
  if (now - playoutState.lastMetadataFetch < 15000) return;
  playoutState.lastMetadataFetch = now;
  
  if (!CONFIG.metadataUrl) return;
  
  try {
    const response = await fetch(CONFIG.metadataUrl);
    if (response.ok) {
      const data = await response.json();
      // Handle AzuraCast format
      if (data.now_playing && data.now_playing.song) {
        state.currentTrack = data.now_playing.song.title || data.now_playing.song.text || "";
      }
    }
  } catch (err) {
    console.log("📻 Could not fetch metadata:", err.message);
  }
}

async function boot({ screen, net }) {
  state = createRadioState(CONFIG);
  initBars(state);
  generateQRCode(state);
  
  // Reset playout state
  playoutState.currentShow = null;
  playoutState.ephemera = null;
  playoutState.lastPlayoutFetch = 0;
  playoutState.lastMetadataFetch = 0;
  playoutState.networkLoaded = false;
  
  // Fetch all network data immediately on boot (don't wait for playback)
  fetchKPBJMetadata();
  fetchPlayoutInfo();
}

function paint({ wipe, ink, screen, pen, help, box, line, write, jump }) {
  // Background
  wipe(...THEME.bg);
  
  // Draw ASCII logo in background (centered, subtle)
  drawBackgroundLogo({ ink, write }, screen);
  
  // Calculate layout (pass qrCells for accurate sizing)
  layout = calcLayout(screen, THEME, state.qrCells);
  
  // Create drawing context
  const ctx = { ink, box, line, write };
  
  // Draw components
  drawVisualizerBars(ctx, state, THEME, layout);
  drawTitle(ctx, THEME, layout);
  drawPlayButton(ctx, state, THEME, layout, pen, help);
  drawVolumeSlider(ctx, state, THEME, layout, pen);
  drawQRCode(ctx, state, THEME, layout);
  
  // Draw KPBJ-specific elements (QR label, website, track info, ephemera)
  drawKPBJElements(ctx, screen, pen, layout, help);
}

function sim({ num: { lerp }, send, net }) {
  state.globalSend = send;
  
  // Request visualizer data (only when playing)
  requestVisualizerData(state, send);
  
  // Update bars
  updateBars(state, { lerp });
  
  // Fetch metadata periodically (always, not just when playing)
  fetchKPBJMetadata();
  
  // Fetch playout info periodically
  fetchPlayoutInfo();
}

// Draw ASCII art logo in background
function drawBackgroundLogo(ctx, screen) {
  const { ink, write } = ctx;
  const isSmall = screen.width < 180 || screen.height < 200;
  const isTiny = screen.width < 120 || screen.height < 150;
  
  // Skip logo on very small screens
  if (isTiny) return;
  
  const lineHeight = isSmall ? 8 : 10;
  const logoHeight = LOGO_ART.length * lineHeight;
  
  // Center the logo vertically, offset up a bit
  const startY = Math.floor((screen.height - logoHeight) / 2) - (isSmall ? 10 : 20);
  
  // Draw each line centered
  ink(...THEME.logoBg);
  for (let i = 0; i < LOGO_ART.length; i++) {
    write(LOGO_ART[i], { center: "x", y: startY + i * lineHeight }, undefined, undefined, false, "MatrixChunky8");
  }
}

// Calculate responsive layout for KPBJ-specific elements
function calcKPBJLayout(screen, layout) {
  const { isSmall, qrY, qrX, qrSize, statusY, volSliderY, volSliderH } = layout;
  const isTiny = screen.width < 120 || screen.height < 150;
  
  // QR label position (below QR code)
  const qrLabelY = qrY + qrSize + (isTiny ? 2 : 4);
  const qrLabelX = qrX;
  
  // Website link position (bottom-left)
  const websiteLinkY = screen.height - (isTiny ? 6 : isSmall ? 8 : 12);
  const websiteLinkX = isTiny ? 2 : isSmall ? 4 : 8;
  
  // Status position (below volume slider)
  const statusTextY = volSliderY + volSliderH + (isTiny ? 6 : isSmall ? 8 : 12);
  
  // Track info position (below status)
  const trackInfoY = statusTextY + (isTiny ? 10 : isSmall ? 12 : 14);
  
  // Ephemera position (above website link)
  const ephemeraY = websiteLinkY - (isTiny ? 10 : isSmall ? 12 : 16);
  const ephemeraX = websiteLinkX;
  
  // Show name position (above ephemera if there's room)
  const showNameY = ephemeraY - (isTiny ? 10 : isSmall ? 12 : 14);
  
  // Max width for left side text (don't overlap QR code)
  const maxTextWidth = qrX - ephemeraX - (isSmall ? 4 : 8);
  
  return {
    qrLabelY,
    qrLabelX,
    websiteLinkY,
    websiteLinkX,
    statusTextY,
    trackInfoY,
    ephemeraY,
    ephemeraX,
    showNameY,
    maxTextWidth,
    isTiny,
  };
}

// Draw all KPBJ-specific UI elements
function drawKPBJElements(ctx, screen, pen, layout, help) {
  const { ink, write, box } = ctx;
  const kpbjLayout = calcKPBJLayout(screen, layout);
  const { 
    qrLabelY, qrLabelX, websiteLinkY, websiteLinkX, 
    statusTextY, trackInfoY, ephemeraY, ephemeraX, 
    showNameY, maxTextWidth, isTiny 
  } = kpbjLayout;
  const { isSmall, qrY, qrX, qrSize, centerX } = layout;
  
  // 1. QR Label (clickable - opens prompt.ac/kpbj)
  const qrLabelText = state.qrLabel || "prompt.ac/kpbj";
  const qrLabelWidth = qrLabelText.length * 5;
  const qrLabelHeight = 8;
  const qrLabelRightX = qrX + qrSize - qrLabelWidth;
  const isQrLabelHovered = pen && 
    pen.x >= qrLabelRightX && pen.x < qrLabelRightX + qrLabelWidth &&
    pen.y >= qrLabelY && pen.y < qrLabelY + qrLabelHeight;
  playoutState.qrLabelHovered = isQrLabelHovered;
  const qrLabelColor = isQrLabelHovered ? [255, 220, 170] : THEME.qrLabel;
  ink(...qrLabelColor).write(qrLabelText, { x: qrLabelRightX, y: qrLabelY }, undefined, undefined, false, "MatrixChunky8");
  
  // 2. Website link (clickable button - opens kpbj.fm)
  const websiteText = state.websiteLabel || "kpbj.fm";
  const websiteWidth = websiteText.length * 5;
  const websiteHeight = 8;
  const isWebsiteHovered = pen && 
    pen.x >= websiteLinkX && pen.x < websiteLinkX + websiteWidth &&
    pen.y >= websiteLinkY && pen.y < websiteLinkY + websiteHeight;
  playoutState.websiteHovered = isWebsiteHovered;
  const websiteColor = isWebsiteHovered ? [255, 220, 170] : THEME.qrLabel;
  ink(...websiteColor).write(websiteText, { x: websiteLinkX, y: websiteLinkY }, undefined, undefined, false, "MatrixChunky8");
  
  // 3. Status (● LIVE / Paused / Connecting...)
  let statusText, statusColor;
  if (state.loadError) {
    statusText = "Connection error";
    statusColor = THEME.statusError;
  } else if (state.isLoading) {
    statusText = "Connecting" + ".".repeat(Math.floor(help.repeat / 20) % 4);
    statusColor = THEME.statusLoading;
  } else if (state.isPlaying) {
    statusText = "● LIVE";
    statusColor = THEME.statusLive;
  } else {
    statusText = "Paused";
    statusColor = THEME.statusPaused;
  }
  
  // Only show status if it won't overlap with QR code
  if (statusTextY < qrY - 4) {
    ink(...statusColor).write(statusText, { center: "x", y: statusTextY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // 4. Current track info (from AzuraCast metadata)
  if (state.currentTrack && trackInfoY < qrY - 4) {
    let displayTrack = state.currentTrack;
    const maxChars = Math.floor((screen.width - 20) / 5);
    if (displayTrack.length > maxChars) {
      displayTrack = displayTrack.substring(0, maxChars - 3) + "...";
    }
    ink(...THEME.trackText).write(displayTrack, { center: "x", y: trackInfoY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // 5. Current show name (from playout/now - only if scheduled)
  if (playoutState.currentShow && showNameY > trackInfoY + 12 && showNameY < qrY - 4) {
    const showName = playoutState.currentShow.name || playoutState.currentShow.title || "On Air";
    const maxChars = Math.floor(maxTextWidth / 5);
    const truncatedShow = showName.length > maxChars ? showName.substring(0, maxChars - 1) + "…" : showName;
    ink(180, 160, 130).write(truncatedShow, { x: ephemeraX, y: showNameY }, undefined, undefined, false, "MatrixChunky8");
  }
  
  // 6. Ephemera (clickable button - fetches new random ephemera)
  if (ephemeraY < qrY - 4 && ephemeraY > trackInfoY + 8) {
    if (playoutState.ephemera) {
      const ephemeraText = playoutState.ephemera.text || playoutState.ephemera.name || playoutState.ephemera;
      const displayText = typeof ephemeraText === 'string' ? ephemeraText : "✨";
      
      // Truncate based on available width
      const maxChars = Math.floor(maxTextWidth / 5) - 3; // -3 for "✨ "
      const truncated = displayText.length > maxChars ? displayText.substring(0, Math.max(1, maxChars - 1)) + "…" : displayText;
      
      // Check hover state
      const fullText = "✨ " + truncated;
      const textWidth = fullText.length * 5;
      const textHeight = 8;
      const isHovered = pen && 
        pen.x >= ephemeraX && pen.x < ephemeraX + textWidth &&
        pen.y >= ephemeraY && pen.y < ephemeraY + textHeight;
      
      playoutState.ephemeraHovered = isHovered;
      
      // Draw with hover effect
      const color = isHovered ? [255, 220, 170] : [140, 120, 100];
      ink(...color).write(fullText, { x: ephemeraX, y: ephemeraY }, undefined, undefined, false, "MatrixChunky8");
    } else if (!playoutState.networkLoaded) {
      // Show loading indicator while fetching
      ink(100, 90, 80).write("...", { x: ephemeraX, y: ephemeraY }, undefined, undefined, false, "MatrixChunky8");
    }
  }
}

function act({ event: e, jump, screen, num: { clamp }, send, net }) {
  state.globalSend = send;
  
  // Recalculate layout for interaction
  layout = calcLayout(screen, THEME, state.qrCells);
  
  // Handle interactions
  handleInteraction(state, e, layout, clamp, send);
  
  // Handle button clicks on lift
  if (e.is("lift")) {
    // Ephemera button click - fetch new ephemera
    if (playoutState.ephemeraHovered) {
      fetchPlayoutInfo(true);
    }
    
    // Website button click - open kpbj.fm
    if (playoutState.websiteHovered && state.websiteUrl) {
      send({ type: "open-url", content: { url: state.websiteUrl } });
    }
    
    // QR label click - open prompt.ac/kpbj
    if (playoutState.qrLabelHovered && state.qrUrl) {
      send({ type: "open-url", content: { url: state.qrUrl } });
    }
  }
  
  // Escape to exit
  if (e.is("keyboard:down:escape")) {
    stopPlayback(state, send);
    jump("prompt");
  }
}

function receive({ type, content }) {
  handleStreamMessage(state, { type, content });
}

function leave({ send }) {
  resetState(state, send);
  // Reset playout state
  playoutState.currentShow = null;
  playoutState.ephemera = null;
  playoutState.lastPlayoutFetch = 0;
  playoutState.lastMetadataFetch = 0;
  playoutState.networkLoaded = false;
}

function meta() {
  return {
    title: "KPBJ",
    desc: "Listen to KPBJ.FM - Shadow Hills Community Radio",
  };
}

export { boot, paint, sim, act, receive, leave, meta };
