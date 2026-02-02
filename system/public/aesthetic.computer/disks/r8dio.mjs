// r8dio, 2024.12.04
// 📻 R8dio.dk live stream player
// Stream: https://s3.radio.co/s7cd1ffe2f/listen

/* #region 🏁 TODO
  - [x] Add visualizer via BIOS streaming API
  - [x] Show current program info
  - [x] Fix iOS Safari visualization (use time-domain fallback)
  - [x] Add QR code linking to prompt.ac/r8dio
  - [x] Add pressed state for play button
  - [x] Refactor to use shared radio.mjs lib
  + Done
#endregion */

import {
  createRadioState,
  initBars,
  generateQRCode,
  updateBars,
  requestVisualizerData,
  fetchMetadata,
  shouldFetchMetadata,
  handleStreamMessage,
  resetState,
  calcLayout,
  drawVisualizerBars,
  drawPlayButton,
  drawVolumeSlider,
  drawQRCode,
  drawWebsiteLink,
  drawStatus,
  drawTitle,
  handleInteraction,
  togglePlayback,
  stopPlayback,
} from "../lib/radio.mjs";

// R8dio Configuration
const CONFIG = {
  streamUrl: "https://s3.radio.co/s7cd1ffe2f/listen",
  streamId: "r8dio-stream",
  metadataUrl: "https://public.radio.co/stations/s7cd1ffe2f/status",
  qrUrl: "https://prompt.ac/r8dio",
  qrLabel: "prompt.ac/r8dio",
  websiteUrl: "https://r8dio.dk",
  websiteLabel: "r8dio.dk",
};

// R8dio Theme - Purple/pink Danish aesthetic
const THEME = {
  // Background - dark purple tint
  bg: [25, 20, 35],
  
  // Title
  title: [255, 200, 220],
  titleText: "\\pink\\r\\magenta\\8D\\pink\\io\\reset\\",
  subtitle: [150, 120, 160],
  subtitleText: "Danmarks snakke-radio",
  
  // Visualizer bar color gradient (purple to pink to white)
  barColor: (t) => ({
    r: Math.floor(120 + t * 135),
    g: Math.floor(60 + t * 100),
    b: Math.floor(180 + t * 75),
  }),
  baseLine: [80, 60, 100],
  
  // Button
  buttonBg: [50, 40, 70],
  buttonHover: [80, 60, 100],
  buttonOutline: [100, 80, 120],
  buttonHoverOutline: [140, 100, 160],
  buttonPressed: [30, 25, 45],
  buttonPressedOutline: [70, 50, 90],
  
  // Icon
  icon: [200, 180, 220],
  iconHover: [255, 220, 255],
  iconPressed: [150, 130, 170],
  
  // Spinner
  spinnerColor: (brightness) => ({
    r: Math.floor(100 + brightness * 155),
    g: Math.floor(80 + brightness * 140),
    b: Math.floor(160 + brightness * 95),
  }),
  
  // Volume slider
  volTrack: [50, 40, 70],
  volFill: [150, 120, 180],
  volFillHover: [180, 140, 220],
  volHandle: [200, 180, 220],
  volHandleHover: [255, 220, 255],
  volText: [100, 80, 120],
  
  // QR code
  qrFg: [200, 180, 220],
  qrBg: [40, 30, 55],
  qrOutline: [100, 80, 120],
  qrLabel: [150, 120, 160],
  
  // Status
  statusError: [255, 100, 100],
  statusLoading: [255, 200, 100],
  statusLive: [100, 255, 150],
  statusPaused: [150, 150, 150],
  trackText: [180, 150, 200],
};

// State
let state;
let layout;

async function boot({ screen, net }) {
  state = createRadioState(CONFIG);
  initBars(state);
  generateQRCode(state);
  
  // Fetch initial metadata
  fetchMetadata(state, net);
}

function paint({ wipe, ink, screen, pen, help, box, line, write }) {
  // Background
  wipe(...THEME.bg);
  
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
  drawWebsiteLink(ctx, state, THEME, layout);
  drawStatus(ctx, state, THEME, layout, help);
}

function sim({ num: { lerp }, send, net }) {
  state.globalSend = send;
  
  // Request visualizer data
  requestVisualizerData(state, send);
  
  // Update bars
  updateBars(state, { lerp });
  
  // Fetch metadata periodically
  if (shouldFetchMetadata(state)) {
    fetchMetadata(state, net);
  }
}

function act({ event: e, jump, screen, num: { clamp }, send }) {
  state.globalSend = send;
  
  // Recalculate layout for interaction
  layout = calcLayout(screen, THEME, state.qrCells);
  
  // Handle interactions
  handleInteraction(state, e, layout, clamp, send);
  
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
}

function meta() {
  return {
    title: "r8Dio",
    desc: "Listen to R8dio.dk live - Danmarks snakke-radio",
  };
}

export { boot, paint, sim, act, receive, leave, meta };

