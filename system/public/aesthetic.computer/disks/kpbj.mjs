// kpbj, 2026.02.01
// 📻 KPBJ.FM live stream player - Sun Valley Community Radio
// Stream: https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3

/* #region 🏁 TODO
  - [ ] Test on mobile/iOS
  - [ ] Add metadata fetching if AzuraCast API is available
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
  drawStatus,
  drawTitle,
  handleInteraction,
  togglePlayback,
  stopPlayback,
} from "../lib/radio.mjs";

// KPBJ Configuration
const CONFIG = {
  streamUrl: "https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3",
  streamId: "kpbj-stream",
  metadataUrl: null, // Could be: "https://kpbj.hasnoskills.com/api/nowplaying/kpbj_test_station"
  qrUrl: "https://prompt.ac/kpbj",
};

// KPBJ Theme - Sun Valley mountain/nature aesthetic
const THEME = {
  // Background - deep mountain blue
  bg: [20, 30, 45],
  
  // Title
  title: [255, 200, 140],
  titleText: "\\yellow\\K\\orange\\P\\yellow\\B\\orange\\J\\reset\\",
  subtitle: [160, 140, 120],
  subtitleText: "Sun Valley Community Radio",
  
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
    title: "KPBJ",
    desc: "Listen to KPBJ.FM - Sun Valley Community Radio",
  };
}

export { boot, paint, sim, act, receive, leave, meta };
