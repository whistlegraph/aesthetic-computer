// keep, 2024.12.15
// Preserve a KidLisp piece as a KEEP on Tezos.
// A "keep" stores your code, artwork, and interaction forever on the blockchain.
// Usage: `keep piece-name` or `keep $piece-name`

import { tokenize, KidLisp } from "../lib/kidlisp.mjs";
import {
  KEEPS_STAGING,
  DEFAULT_NETWORK,
  getNetwork,
  getObjktUrl,
  getTzktTokenUrl,
  getTzktApi,
} from "../lib/keeps/constants.mjs";
import {
  checkIfMinted,
  fetchTokenInfo,
  findTokenByName,
} from "../lib/keeps/tzkt-client.mjs";

const { min, max, floor, sin, cos, PI, abs } = Math;

// Theme scheme for light/dark mode
export const scheme = {
  dark: {
    bg: [25, 28, 35],
    bgShimmer: [20, 35, 40],
    bgPanel: [30, 35, 45],
    bgCarousel: [25, 30, 40],
    bgProgress: [30, 35, 45],
    text: [220, 225, 235],
    textDim: [100, 110, 130],
    textMuted: [140, 150, 170],
    accent: [100, 200, 255],
    positive: [100, 220, 150],
    warning: [255, 200, 100],
    error: [255, 100, 120],
    gold: [255, 200, 100],
    stripe: [40, 45, 55],
    divider: [50, 55, 65],
    arrow: [100, 110, 130],
    // Button color schemes
    btnLink: {
      normal: { bg: [30, 60, 70], outline: [100, 180, 220], outlineAlpha: 150, text: [100, 180, 220] },
      hover: { bg: [50, 90, 110], outline: [180, 240, 255], outlineAlpha: 200, text: [180, 240, 255] },
      disabled: { bg: [25, 35, 40], outline: [60, 80, 90], outlineAlpha: 100, text: [60, 80, 90] }
    },
    btnObjkt: {
      normal: { bg: [40, 80, 70], outline: [100, 200, 160], outlineAlpha: 150, text: [255, 255, 255] },
      hover: { bg: [60, 120, 100], outline: [150, 255, 200], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [35, 50, 45], outline: [70, 130, 100], outlineAlpha: 100, text: [140, 160, 150] }
    },
    btnRebake: {
      normal: { bg: [80, 60, 40], outline: [255, 180, 100], outlineAlpha: 150, text: [255, 200, 140] },
      hover: { bg: [120, 90, 60], outline: [255, 220, 150], outlineAlpha: 200, text: [255, 240, 200] },
      disabled: { bg: [50, 45, 40], outline: [120, 100, 80], outlineAlpha: 100, text: [140, 120, 100] }
    },
    btnSync: {
      normal: { bg: [50, 30, 80], outline: [140, 100, 200], outlineAlpha: 150, text: [200, 150, 255] },
      hover: { bg: [70, 50, 120], outline: [180, 140, 255], outlineAlpha: 200, text: [230, 200, 255] },
      disabled: { bg: [30, 25, 40], outline: [80, 60, 100], outlineAlpha: 100, text: [100, 80, 120] }
    },
    btnWallet: {
      normal: { bg: [40, 40, 55], outline: [100, 100, 140], outlineAlpha: 120, text: [140, 140, 180] },
      hover: { bg: [55, 55, 75], outline: [140, 140, 200], outlineAlpha: 180, text: [180, 180, 220] },
      disabled: { bg: [30, 30, 40], outline: [60, 60, 80], outlineAlpha: 80, text: [80, 80, 100] }
    },
    btnContract: {
      normal: { bg: [35, 45, 55], outline: [100, 140, 180], outlineAlpha: 120, text: [140, 180, 220] },
      hover: { bg: [45, 60, 75], outline: [140, 180, 220], outlineAlpha: 180, text: [180, 220, 255] },
      disabled: { bg: [25, 30, 35], outline: [60, 80, 100], outlineAlpha: 80, text: [80, 100, 120] }
    },
    btnTx: {
      normal: { bg: [40, 70, 60], outline: [100, 200, 150], outlineAlpha: 150, text: [150, 255, 200] },
      hover: { bg: [50, 90, 75], outline: [150, 255, 200], outlineAlpha: 200, text: [200, 255, 230] },
      disabled: { bg: [30, 40, 35], outline: [60, 100, 80], outlineAlpha: 100, text: [80, 120, 100] }
    },
    btnPreview: {
      normalBase: { bg: [25, 55, 50], outline: [80, 210, 190], outlineAlpha: 180, text: [100, 220, 200] },
      hover: { bg: [40, 80, 70], outline: [130, 255, 230], outlineAlpha: 220, text: [150, 255, 240] },
      disabled: { bg: [20, 35, 32], outline: [50, 120, 100], outlineAlpha: 100, text: [50, 120, 100] }
    },
    btnConfirm: {
      normalBase: { bg: [40, 75, 55], outline: [110, 220, 150], outlineAlpha: 230, text: [160, 255, 200] },
      hover: { bg: [55, 105, 80], outline: [170, 255, 210], outlineAlpha: 255, text: [230, 255, 245] },
      disabled: { bg: [30, 40, 35], outline: [60, 100, 80], outlineAlpha: 100, text: [80, 120, 100] }
    },
    btnLogin: {
      normalBase: { bg: [50, 55, 80], outline: [130, 150, 220], outlineAlpha: 200, text: [170, 190, 255] },
      hover: { bg: [65, 75, 110], outline: [175, 195, 255], outlineAlpha: 240, text: [210, 220, 255] },
      disabled: { bg: [30, 35, 45], outline: [70, 80, 100], outlineAlpha: 100, text: [90, 100, 120] }
    },
    btnCached: {
      normal: { bg: [50, 40, 30], outline: [160, 120, 80], outlineAlpha: 120, text: [160, 120, 80] },
      hover: { bg: [70, 55, 40], outline: [200, 160, 100], outlineAlpha: 180, text: [200, 160, 100] },
    },
    btnNet: {
      normal: { bg: [35, 50, 60], text: [100, 140, 160] },
      hover: { bg: [50, 70, 80], text: [160, 200, 220] },
    },
    btnView: {
      normal: { bg: [40, 80, 60], outline: [100, 220, 150], outlineAlpha: 150, text: [255, 255, 255] },
      hover: { bg: [60, 120, 90], outline: [150, 255, 200], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [35, 50, 45], outline: [70, 130, 100], outlineAlpha: 100, text: [140, 160, 150] }
    },
    btnToll: {
      normal: { bg: [60, 120, 80], text: [200, 255, 220] },
      hover: { bg: [80, 150, 100], text: [255, 255, 255] },
    },
    btnStaging: {
      normal: { bg: [50, 100, 70], outline: [100, 180, 140], text: [180, 255, 200] },
      hover: { bg: [80, 150, 100], outline: [150, 255, 200], text: [255, 255, 255] },
    },
    btnRetry: {
      normal: { bg: [100, 50, 50], outline: [255, 120, 120], outlineAlpha: 150, text: [255, 255, 255] },
      hover: { bg: [140, 70, 70], outline: [255, 160, 160], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [60, 40, 40], outline: [120, 80, 80], outlineAlpha: 100, text: [140, 120, 120] }
    },
    btnCancel: {
      normal: { bg: [80, 45, 45], outline: [220, 110, 110], outlineAlpha: 140, text: [255, 230, 230] },
      hover: { bg: [110, 60, 60], outline: [255, 150, 150], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [50, 35, 35], outline: [120, 80, 80], outlineAlpha: 100, text: [140, 120, 120] }
    },
  },
  light: {
    bg: [245, 243, 238],
    bgShimmer: [235, 240, 235],
    bgPanel: [235, 238, 242],
    bgCarousel: [230, 233, 240],
    bgProgress: [220, 225, 235],
    text: [30, 35, 45],
    textDim: [120, 130, 150],
    textMuted: [120, 130, 150],
    accent: [0, 120, 180],
    positive: [30, 160, 80],
    warning: [180, 120, 20],
    error: [200, 60, 80],
    gold: [180, 130, 30],
    stripe: [220, 225, 235],
    divider: [200, 205, 215],
    arrow: [140, 150, 170],
    // Button color schemes (light mode)
    btnLink: {
      normal: { bg: [220, 235, 240], outline: [40, 120, 160], outlineAlpha: 180, text: [40, 120, 160] },
      hover: { bg: [200, 225, 235], outline: [20, 100, 140], outlineAlpha: 220, text: [20, 100, 140] },
      disabled: { bg: [235, 240, 242], outline: [150, 170, 180], outlineAlpha: 120, text: [150, 170, 180] }
    },
    btnObjkt: {
      normal: { bg: [210, 235, 225], outline: [60, 150, 110], outlineAlpha: 180, text: [40, 100, 70] },
      hover: { bg: [195, 225, 210], outline: [40, 130, 90], outlineAlpha: 220, text: [30, 80, 50] },
      disabled: { bg: [230, 240, 235], outline: [130, 170, 150], outlineAlpha: 120, text: [130, 150, 140] }
    },
    btnRebake: {
      normal: { bg: [245, 235, 215], outline: [160, 120, 40], outlineAlpha: 180, text: [160, 120, 40] },
      hover: { bg: [240, 225, 195], outline: [140, 100, 20], outlineAlpha: 220, text: [140, 100, 20] },
      disabled: { bg: [242, 238, 230], outline: [180, 170, 150], outlineAlpha: 120, text: [180, 170, 150] }
    },
    btnSync: {
      normal: { bg: [235, 225, 245], outline: [100, 70, 150], outlineAlpha: 180, text: [100, 70, 150] },
      hover: { bg: [225, 212, 240], outline: [80, 50, 130], outlineAlpha: 220, text: [80, 50, 130] },
      disabled: { bg: [240, 238, 245], outline: [160, 150, 175], outlineAlpha: 120, text: [160, 150, 175] }
    },
    btnWallet: {
      normal: { bg: [235, 230, 245], outline: [100, 80, 140], outlineAlpha: 180, text: [100, 80, 140] },
      hover: { bg: [225, 218, 240], outline: [80, 60, 120], outlineAlpha: 220, text: [80, 60, 120] },
    },
    btnContract: {
      normal: { bg: [225, 235, 245], outline: [70, 110, 140], outlineAlpha: 150, text: [50, 90, 130] },
      hover: { bg: [210, 225, 240], outline: [50, 90, 120], outlineAlpha: 200, text: [30, 70, 110] },
      disabled: { bg: [245, 250, 252], outline: [150, 170, 190], outlineAlpha: 100, text: [150, 170, 190] }
    },
    btnTx: {
      normal: { bg: [220, 245, 235], outline: [40, 150, 100], outlineAlpha: 180, text: [30, 130, 80] },
      hover: { bg: [200, 235, 220], outline: [30, 130, 80], outlineAlpha: 220, text: [20, 110, 60] },
      disabled: { bg: [245, 252, 248], outline: [170, 200, 185], outlineAlpha: 100, text: [170, 200, 185] }
    },
    btnPreview: {
      normalBase: { bg: [220, 245, 240], outline: [60, 180, 160], outlineAlpha: 200, text: [40, 150, 130] },
      hover: { bg: [200, 235, 228], outline: [40, 200, 180], outlineAlpha: 240, text: [30, 170, 150] },
      disabled: { bg: [245, 252, 250], outline: [170, 210, 200], outlineAlpha: 120, text: [170, 210, 200] }
    },
    btnConfirm: {
      normalBase: { bg: [215, 245, 225], outline: [50, 180, 100], outlineAlpha: 230, text: [30, 150, 70] },
      hover: { bg: [195, 240, 210], outline: [40, 200, 110], outlineAlpha: 255, text: [20, 170, 80] },
      disabled: { bg: [242, 250, 245], outline: [160, 200, 175], outlineAlpha: 120, text: [160, 200, 175] }
    },
    btnLogin: {
      normalBase: { bg: [230, 232, 250], outline: [100, 110, 180], outlineAlpha: 220, text: [60, 70, 150] },
      hover: { bg: [218, 222, 245], outline: [80, 90, 160], outlineAlpha: 250, text: [40, 50, 130] },
      disabled: { bg: [248, 250, 255], outline: [170, 175, 200], outlineAlpha: 120, text: [170, 175, 200] }
    },
    btnCached: {
      normal: { bg: [245, 238, 225], outline: [140, 100, 50], outlineAlpha: 150, text: [140, 100, 50] },
      hover: { bg: [240, 230, 210], outline: [120, 80, 30], outlineAlpha: 200, text: [120, 80, 30] },
    },
    btnNet: {
      normal: { bg: [228, 235, 242], text: [70, 110, 130] },
      hover: { bg: [215, 225, 235], text: [50, 90, 110] },
    },
    btnView: {
      normal: { bg: [220, 245, 235], outline: [50, 180, 110], outlineAlpha: 180, text: [30, 120, 70] },
      hover: { bg: [200, 240, 220], outline: [40, 200, 130], outlineAlpha: 220, text: [20, 100, 50] },
      disabled: { bg: [242, 252, 248], outline: [160, 210, 180], outlineAlpha: 120, text: [160, 210, 180] }
    },
    btnToll: {
      normal: { bg: [215, 240, 225], text: [30, 100, 50] },
      hover: { bg: [200, 235, 210], text: [20, 80, 40] },
    },
    btnStaging: {
      normal: { bg: [210, 240, 220], outline: [60, 140, 90], text: [40, 100, 60] },
      hover: { bg: [190, 235, 205], outline: [40, 120, 70], text: [30, 80, 50] },
    },
    btnRetry: {
      normal: { bg: [245, 225, 225], outline: [180, 80, 80], outlineAlpha: 180, text: [140, 50, 50] },
      hover: { bg: [240, 210, 210], outline: [160, 60, 60], outlineAlpha: 220, text: [120, 40, 40] },
      disabled: { bg: [240, 235, 235], outline: [180, 160, 160], outlineAlpha: 120, text: [160, 140, 140] }
    },
    btnCancel: {
      normal: { bg: [240, 215, 215], outline: [180, 90, 90], outlineAlpha: 180, text: [130, 50, 50] },
      hover: { bg: [235, 200, 200], outline: [160, 70, 70], outlineAlpha: 220, text: [110, 40, 40] },
      disabled: { bg: [240, 235, 235], outline: [180, 160, 160], outlineAlpha: 120, text: [160, 140, 140] }
    },
  },
};

// Phase colors for dark/light modes
const PHASE_COLORS_DARK = {
  wallet: { stripe: [60, 40, 80], label: [180, 140, 220], detail: [140, 100, 180] },
  validate: { stripe: [50, 50, 70], label: [150, 150, 200], detail: [110, 110, 160] },
  analyze: { stripe: [50, 50, 70], label: [150, 150, 200], detail: [110, 110, 160] },
  thumbnail: { stripe: [70, 50, 40], label: [220, 160, 120], detail: [180, 120, 80] },
  bundle: { stripe: [70, 50, 40], label: [220, 160, 120], detail: [180, 120, 80] },
  ipfs: { stripe: [40, 60, 60], label: [120, 200, 200], detail: [80, 160, 160] },
  metadata: { stripe: [40, 60, 60], label: [120, 200, 200], detail: [80, 160, 160] },
  review: { stripe: [60, 60, 40], label: [220, 220, 140], detail: [180, 180, 100] },
  sign: { stripe: [50, 50, 35], label: [255, 255, 180], detail: [255, 255, 200] },
  complete: { stripe: [40, 70, 50], label: [140, 255, 180], detail: [100, 200, 140] },
};

const PHASE_COLORS_LIGHT = {
  wallet: { stripe: [230, 220, 240], label: [100, 60, 140], detail: [130, 90, 170] },
  validate: { stripe: [225, 225, 240], label: [70, 70, 130], detail: [100, 100, 160] },
  analyze: { stripe: [225, 225, 240], label: [70, 70, 130], detail: [100, 100, 160] },
  thumbnail: { stripe: [240, 230, 220], label: [160, 100, 40], detail: [180, 120, 60] },
  bundle: { stripe: [240, 230, 220], label: [160, 100, 40], detail: [180, 120, 60] },
  ipfs: { stripe: [220, 235, 235], label: [40, 130, 130], detail: [60, 150, 150] },
  metadata: { stripe: [220, 235, 235], label: [40, 130, 130], detail: [60, 150, 150] },
  review: { stripe: [235, 235, 215], label: [140, 140, 40], detail: [160, 160, 60] },
  sign: { stripe: [235, 235, 210], label: [160, 160, 40], detail: [180, 180, 60] },
  complete: { stripe: [220, 240, 225], label: [40, 160, 80], detail: [60, 180, 100] },
};

// Active palette and phase colors - set dynamically based on $.dark
let pal = scheme.dark;
let PHASE_COLORS = PHASE_COLORS_DARK;

// Get config from shared constants
const NETWORK = DEFAULT_NETWORK;
const KEEPS_CONTRACT = getNetwork(NETWORK).contract;

// 👻 Pac-Man Ghost Sprite (14x14, classic arcade bitmap)
const GHOST_SPRITE = [
  0b00000111110000, // row 0:      ████
  0b00011111111000, // row 1:    ██████
  0b00111111111100, // row 2:   ████████
  0b01111111111110, // row 3:  ██████████
  0b01111111111110, // row 4:  ██████████
  0b11111111111111, // row 5: ████████████
  0b11111111111111, // row 6: ████████████
  0b11111111111111, // row 7: ████████████
  0b11111111111111, // row 8: ████████████
  0b11111111111111, // row 9: ████████████
  0b11111111111111, // row 10: ████████████
  0b11011101110111, // row 11: ██ ███ ███ ██
  0b10001100110001, // row 12: █  ██  ██  █
  0b00000000000000, // row 13: (wavy gap)
];

function drawGhost(ink, box, x, y, bodyColor = [255, 180, 50], size = 1) {
  const w = 14, h = 14;
  ink(...bodyColor);
  for (let row = 0; row < h; row++) {
    for (let col = 0; col < w; col++) {
      if ((GHOST_SPRITE[row] >> (w - 1 - col)) & 1) {
        box(x + col * size, y + row * size, size, size);
      }
    }
  }
  // White sclera
  ink(255, 255, 255);
  box(x + 3 * size, y + 4 * size, 2 * size, 3 * size);
  box(x + 8 * size, y + 4 * size, 2 * size, 3 * size);
  // Blue pupils
  ink(50, 80, 200);
  box(x + 4 * size, y + 5 * size, size, 2 * size);
  box(x + 9 * size, y + 5 * size, size, 2 * size);
}

let piece;
let walletAddress = null;
let preparedData = null;
let txHash = null;
let tokenId = null;
let userHandle = null;
let userSub = null; // Current user's auth0 sub ID
let sourceCode = null;
let pieceAuthor = null; // Author handle (e.g. "@jeffrey")
let pieceAuthorSub = null; // Author's auth0 sub ID
let pieceCreatedAt = null; // When piece was created
let pieceSourceLength = null; // Character count
let pieceHits = null; // Number of times piece was accessed
let pieceSourceDisplay = null; // Source code for display (single line)

let rotation = 0;
let startTime = null;
let particles = []; // Vegas-style particles

// Ownership state
let isAuthor = null; // true if current user is the author, false if not, null if unknown
let loadingPieceInfo = false; // True while fetching piece info

// Already minted state
let alreadyMinted = null; // { tokenId, owner, artifactUri, thumbnailUri, metadataUri, mintedAt, name }
let loadingExisting = false;
let waitingConfirmation = false; // True when piece is ready to mint but awaiting user confirmation
let rebaking = false; // True when regenerating bundle for already-minted piece
let rebakeResult = null; // Result from rebake: { artifactUri, thumbnailUri }
let rebakeProgress = null; // Progress message during rebake
let originalOnChainUris = null; // Original URIs from chain before rebake { artifactUri, thumbnailUri }
let pendingRebake = null; // Pending rebake from DB (not yet updated on chain)
let cachedMedia = null; // Last generated bundle from DB { artifactUri, thumbnailUri, createdAt, sourceHash }
let updatingChain = false; // True when updating on-chain metadata
let updateChainResult = null; // Result from chain update
let updateChainProgress = null; // Progress message during chain update
let mintAbortController = null; // AbortController for cancelling mint preparation
let mintCancelled = false; // True when user cancelled during preparation
let thumbnailBitmap = null; // Loaded thumbnail image (single frame or current frame)
let thumbnailFrames = null; // Array of frames for animated WebP { frames, width, height, loopCount }
let thumbnailFrameIndex = 0; // Current animation frame
let thumbnailLastFrameTime = 0; // Time of last frame change
let piecePreviewBitmap = null; // Preview thumbnail from oven for unminted pieces
let piecePreviewFrames = null; // Animated preview frames
let piecePreviewFrameIndex = 0; // Current preview animation frame
let piecePreviewLastFrameTime = 0; // Preview frame timing
let piecePreviewLoading = false; // True while loading preview from oven
let kidlispSource = null; // Source code for syntax highlighting
let tickerOffset = 0; // For scrolling ticker

// Analysis state
let analysisData = null; // Formal Lisp analysis: { lines, expressions, depth, density, vocabulary, size, structure, varCount, funcCount, deps, behavior flags, topForms }
let onChainAnalyzerVersion = null; // Version from on-chain attributes

// Carousel animation state
let carouselTargetIndex = 0; // Target step index
let carouselCurrentX = 0; // Animated X position for smooth sliding
let lastActiveIndex = -1; // Track when active step changes

// Timeline - the heart of the UX
let timeline = [];

function resetTimeline() {
  timeline = [
    { id: "wallet", label: "Connect Wallet", status: "pending", detail: null, time: null, startedAt: null, duration: 500 },
    { id: "validate", label: "Validate Piece", status: "pending", detail: null, time: null, startedAt: null, duration: 2000 },
    { id: "analyze", label: "Analyze Source", status: "pending", detail: null, time: null, startedAt: null, duration: 1500 },
    { id: "thumbnail", label: "Bake Thumbnail", status: "pending", detail: null, time: null, startedAt: null, duration: 8000 },
    { id: "bundle", label: "Pack HTML Bundle", status: "pending", detail: null, time: null, startedAt: null, duration: 3000 },
    { id: "ipfs", label: "Pin to IPFS", status: "pending", detail: null, time: null, startedAt: null, duration: 5000 },
    { id: "metadata", label: "Build Metadata", status: "pending", detail: null, time: null, startedAt: null, duration: 2000 },
    { id: "review", label: "Pay Keep Toll", status: "pending", detail: null, time: null, startedAt: null, duration: null },
    { id: "sign", label: "Sign Transaction", status: "pending", detail: null, time: null, startedAt: null, duration: 30000 },
    { id: "complete", label: "Keep Complete!", status: "pending", detail: null, time: null, startedAt: null, duration: 500 },
  ];
}

function getElapsedTime() {
  const elapsed = (Date.now() - startTime) / 1000;
  return elapsed < 10 ? elapsed.toFixed(1) + "s" : floor(elapsed) + "s";
}

function setStep(id, status, detail = null) {
  const item = timeline.find(t => t.id === id);
  if (item) {
    // Track when step becomes active
    if (status === "active" && item.status !== "active") {
      item.startedAt = Date.now();
    }
    item.status = status;
    if (detail !== null) item.detail = detail;
    // Always record time when status changes
    item.time = getElapsedTime();
    // Console logging for debugging
    const icon = status === "done" ? "✓" : status === "error" ? "✗" : status === "active" ? "►" : "○";
    console.log(`🪙 KEEP [${item.time}] ${icon} ${item.label}${detail ? " — " + detail : ""}`);
  }
  _needsPaint?.();
}

function getActiveStep() {
  return timeline.find(t => t.status === "active");
}

function hasError() {
  return timeline.some(t => t.status === "error");
}

let btn;
let htmlBtn, thumbBtn, metaBtn, docsBtn, networkBtn, rebakeBtn, updateChainBtn;
let oldHtmlBtn, oldThumbBtn; // Buttons for original on-chain URIs
let walletBtn; // Navigate to wallet piece
let txBtn; // Transaction hash button after sync
let contractBtn; // Link to contract on TzKT
let loginBtn; // Login button for non-authors
let previewBtn; // Preview piece button
let cancelBtn; // Cancel preparation button
let _api, _net, _jump, _store, _needsPaint, _send, _ui, _screen, _preload, _preloadAnimatedWebp;

function boot({ api, net, hud, params, store, cursor, jump, needsPaint, ui, screen, send, user }) {
  cursor("native");
  _api = api;
  _net = net;
  _jump = jump;
  _store = store;
  _needsPaint = needsPaint;
  _send = send;
  _ui = ui;
  _screen = screen;
  _preload = net.preload;
  _preloadAnimatedWebp = net.preloadAnimatedWebp;

  hud.labelBack();
  resetTimeline();
  startTime = Date.now();
  particles = [];

  btn = new ui.TextButton("...", { center: "xy", screen });
  btn.btn.stickyScrubbing = true;

  htmlBtn = new ui.TextButton("HTML", { x: 0, y: 0, screen });
  thumbBtn = new ui.TextButton("THUMB", { x: 0, y: 0, screen });
  metaBtn = new ui.TextButton("META", { x: 0, y: 0, screen });
  docsBtn = new ui.TextButton("DOCS", { x: 0, y: 0, screen });
  networkBtn = new ui.TextButton("GHOSTNET", { x: 0, y: 0, screen });
  rebakeBtn = new ui.TextButton("REBAKE", { x: 0, y: 0, screen });
  updateChainBtn = new ui.TextButton("UPDATE", { x: 0, y: 0, screen });
  oldHtmlBtn = new ui.TextButton("OLD HTML", { x: 0, y: 0, screen });
  oldThumbBtn = new ui.TextButton("OLD THUMB", { x: 0, y: 0, screen });
  walletBtn = new ui.TextButton("Wallet", { x: 0, y: 0, screen });
  txBtn = new ui.TextButton("TX", { x: 0, y: 0, screen });
  contractBtn = new ui.TextButton("Contract", { x: 0, y: 0, screen });
  loginBtn = new ui.TextButton("Login", { x: 0, y: 0, screen });
  previewBtn = new ui.TextButton("Preview", { x: 0, y: 0, screen });
  cancelBtn = new ui.TextButton("Cancel", { x: 0, y: 0, screen });
  htmlBtn.btn.stickyScrubbing = true;
  thumbBtn.btn.stickyScrubbing = true;
  metaBtn.btn.stickyScrubbing = true;
  docsBtn.btn.stickyScrubbing = true;
  networkBtn.btn.stickyScrubbing = true;
  rebakeBtn.btn.stickyScrubbing = true;
  updateChainBtn.btn.stickyScrubbing = true;
  oldHtmlBtn.btn.stickyScrubbing = true;
  oldThumbBtn.btn.stickyScrubbing = true;
  walletBtn.btn.stickyScrubbing = true;
  txBtn.btn.stickyScrubbing = true;
  contractBtn.btn.stickyScrubbing = true;
  loginBtn.btn.stickyScrubbing = true;
  previewBtn.btn.stickyScrubbing = true;
  cancelBtn.btn.stickyScrubbing = true;

  let rawPiece = params[0] || store["keep:piece"];
  piece = rawPiece?.startsWith("$") ? rawPiece.slice(1) : rawPiece;

  if (!piece) {
    setStep("validate", "error", "No piece specified! Usage: keep $piece");
    return;
  }

  // Get current user's handle and sub from the passed user object
  _net?.getHandle?.().then(h => { userHandle = h; _needsPaint?.(); }).catch(() => {});

  // Use the user object directly if available (much more reliable than token decoding)
  if (user?.sub) {
    userSub = user.sub;
    console.log("🪙 KEEP: User sub from boot:", userSub);
    checkOwnership();
  } else {
    console.log("🪙 KEEP: No user logged in (user object not available)");
  }

  // Fetch piece info (author, hits, source) then check if already minted
  fetchPieceInfo();
  
  // Load preview thumbnail from oven (small animated webp)
  loadPiecePreview();
}

// Fetch piece info from database (author, hits, source)
async function fetchPieceInfo() {
  console.log("🪙 KEEP: Fetching piece info for $" + piece);
  loadingPieceInfo = true;
  _needsPaint?.();

  try {
    const response = await fetch(`/api/store-kidlisp?code=${piece}`);
    if (response.ok) {
      const data = await response.json();

      // Store source for display (single line)
      if (data.source) {
        pieceSourceDisplay = data.source.replace(/\n+/g, " ").replace(/\s+/g, " ").trim();
        pieceSourceLength = data.source.length;
      }

      // Store hits
      pieceHits = data.hits || 0;

      // Store when created
      if (data.when) {
        pieceCreatedAt = new Date(data.when);
      }

      // Store author sub ID
      pieceAuthorSub = data.user || null;

      // Fetch author handle if we have a sub
      if (pieceAuthorSub) {
        try {
          const handleRes = await fetch(`/user?from=${encodeURIComponent(pieceAuthorSub)}&withHandle=true`);
          if (handleRes.ok) {
            const handleData = await handleRes.json();
            if (handleData.handle) {
              pieceAuthor = "@" + handleData.handle;
            }
          }
        } catch (e) {
          console.warn("🪙 KEEP: Could not fetch author handle:", e);
        }
      }

      console.log("🪙 KEEP: Piece info:", { pieceAuthor, pieceHits, pieceAuthorSub, hasSource: !!pieceSourceDisplay });

      // Check ownership now that we have piece info
      checkOwnership();
    } else if (response.status === 404) {
      console.log("🪙 KEEP: Piece not found in database");
      pieceAuthor = null;
      pieceAuthorSub = null;
      isAuthor = null;
    }
  } catch (e) {
    console.error("🪙 KEEP: Error fetching piece info:", e);
  }

  loadingPieceInfo = false;
  _needsPaint?.();

  // Now check if already minted
  checkIfAlreadyMinted();
}

// Load preview thumbnail from oven (for unminted pieces)
async function loadPiecePreview() {
  if (!piece) return;
  
  console.log("🪙 KEEP: Loading preview thumbnail for $" + piece);
  piecePreviewLoading = true;
  _needsPaint?.();
  
  try {
    // Use oven to get a small animated WebP preview (100x100 matches other AC usages)
    const ovenUrl = `https://oven.aesthetic.computer/grab/webp/100/100/$${piece}?duration=2000&fps=8&quality=80&density=1`;
    
    console.log("🪙 KEEP: Preview URL:", ovenUrl);
    
    const result = await _preloadAnimatedWebp?.(ovenUrl);
    
    console.log("🪙 KEEP: Preview loaded:", {
      width: result?.width,
      height: result?.height,
      frameCount: result?.frameCount,
      hasFrames: !!result?.frames?.length
    });
    
    if (result.frameCount > 1) {
      // Animated preview
      piecePreviewFrames = {
        width: result.width,
        height: result.height,
        frameCount: result.frameCount,
        loopCount: result.loopCount,
        frames: result.frames
      };
      piecePreviewFrameIndex = 0;
      piecePreviewLastFrameTime = typeof performance !== 'undefined' ? performance.now() : Date.now();
      piecePreviewBitmap = {
        width: result.width,
        height: result.height,
        pixels: result.frames[0].pixels
      };
    } else {
      // Static preview
      piecePreviewBitmap = {
        width: result.width,
        height: result.height,
        pixels: result.frames[0].pixels
      };
      piecePreviewFrames = null;
    }
    piecePreviewLoading = false;
    _needsPaint?.();
  } catch (e) {
    piecePreviewLoading = false;
    _needsPaint?.();
    // Silent - preview is optional
  }
}

// Check if current user is the author
function checkOwnership() {
  if (!pieceAuthorSub) {
    // No author recorded - could be anonymous or very old piece
    isAuthor = null;
    console.log("🪙 KEEP: No author recorded for this piece");
  } else if (!userSub) {
    // User not logged in YET - but don't set to false, keep as null until we know
    // (They might still be loading their auth token)
    // Only show "not logged in" if we've given auth time to load
    console.log("🪙 KEEP: User sub not yet loaded, keeping isAuthor as null");
    // Don't change isAuthor - leave it as null until we have userSub
  } else {
    // Compare user sub with author sub
    isAuthor = userSub === pieceAuthorSub;
    console.log("🪙 KEEP: Ownership check:", isAuthor ? "YOU are the author" : "You are NOT the author");
    console.log("🪙 KEEP: userSub:", userSub, "pieceAuthorSub:", pieceAuthorSub);
  }
  _needsPaint?.();
}

// Check TzKT for existing mint
async function checkIfAlreadyMinted() {
  console.log("🪙 KEEP: Checking if $" + piece + " is already minted...");
  loadingExisting = true;
  _needsPaint?.();

  try {
    const result = await checkIfMinted(piece, NETWORK);

    if (result) {
      console.log("🪙 KEEP: Already minted as token #" + result.tokenId);
      // Fetch token metadata from TzKT
      await fetchExistingTokenInfo(result.tokenId);
      return;
    }

    // Not minted - show confirmation button
    console.log("🪙 KEEP: Not yet minted, waiting for confirmation...");
    loadingExisting = false;
    waitingConfirmation = true;
    _needsPaint?.();

  } catch (e) {
    console.error("🪙 KEEP: Error checking mint status:", e);
    loadingExisting = false;
    waitingConfirmation = true; // Still show confirmation on error
    _needsPaint?.();
  }
}

// Fetch existing token info from TzKT
async function fetchExistingTokenInfo(existingTokenId) {
  try {
    const tokenInfo = await fetchTokenInfo(existingTokenId, NETWORK);

    alreadyMinted = {
      tokenId: existingTokenId,
      owner: tokenInfo.owner,
      name: tokenInfo.name || `$${piece}`,
      description: tokenInfo.description,
      artifactUri: tokenInfo.artifactUri,
      thumbnailUri: tokenInfo.thumbnailUri,
      creators: tokenInfo.creators,
      mintedAt: tokenInfo.mintedAt,
      network: NETWORK,
      objktUrl: tokenInfo.objktUrl,
      tzktUrl: tokenInfo.tzktUrl,
    };

    // Extract analyzer version from on-chain attributes
    const attrs = tokenInfo.attributes || [];
    const analyzerAttr = attrs.find(a => a.name === "Analyzer Version");
    onChainAnalyzerVersion = analyzerAttr?.value || null;
    console.log("🪙 KEEP: On-chain analyzer version:", onChainAnalyzerVersion);

    loadingExisting = false;
    console.log("🪙 KEEP: Loaded existing token info:", alreadyMinted);
    _needsPaint?.();

    // Load thumbnail image from IPFS
    if (alreadyMinted.thumbnailUri) {
      loadThumbnail(alreadyMinted.thumbnailUri);
    }

    // Load KidLisp source code from database
    loadKidlispSource();

  } catch (e) {
    console.error("🪙 KEEP: Error fetching token info:", e);
    // Still show basic info
    alreadyMinted = {
      tokenId: existingTokenId,
      owner: null,
      name: `$${piece}`,
      network: NETWORK,
      objktUrl: getObjktUrl(existingTokenId, NETWORK),
      tzktUrl: getTzktTokenUrl(existingTokenId, NETWORK),
    };
    loadingExisting = false;
    _needsPaint?.();
  }
}

// Load thumbnail from IPFS URI or fallback to local preview
async function loadThumbnail(ipfsUri) {
  try {
    let url;

    // Convert IPFS URI to gateway URL - use our own gateway which is faster
    if (ipfsUri && ipfsUri.startsWith("ipfs://")) {
      url = `https://ipfs.aesthetic.computer/ipfs/${ipfsUri.slice(7)}`;
    } else if (ipfsUri && ipfsUri.startsWith("https://ipfs")) {
      url = ipfsUri; // Already a gateway URL
    } else {
      // Fallback to local preview service
      url = `/api/grab?piece=$${piece}&width=256&height=256`;
    }

    console.log("🪙 KEEP: Loading animated thumbnail from:", url);

    // Request animated WebP decode from bios (runs in main thread with window access)
    const result = await _preloadAnimatedWebp?.(url);

    console.log("🪙 KEEP: Received animated WebP:", result.frameCount, "frames,", result.width, "x", result.height);

    if (result.frameCount > 1) {
      // Animated WebP - store frames for animation
      thumbnailFrames = {
        width: result.width,
        height: result.height,
        frameCount: result.frameCount,
        loopCount: result.loopCount,
        frames: result.frames
      };
      thumbnailFrameIndex = 0;
      thumbnailLastFrameTime = typeof performance !== 'undefined' ? performance.now() : Date.now();

      // Set first frame as current bitmap
      const firstFrame = result.frames[0];
      thumbnailBitmap = {
        width: result.width,
        height: result.height,
        pixels: firstFrame.pixels
      };
      console.log("🪙 KEEP: Animated thumbnail ready -", result.frameCount, "frames");
    } else {
      // Single frame WebP
      thumbnailBitmap = {
        width: result.width,
        height: result.height,
        pixels: result.frames[0].pixels
      };
      thumbnailFrames = null;
      console.log("🪙 KEEP: Static thumbnail loaded:", thumbnailBitmap.width, "x", thumbnailBitmap.height);
    }

    _needsPaint?.();
  } catch (e) {
    console.warn("🪙 KEEP: Animated thumbnail load failed, trying standard preload:", e.message);

    // Fallback to standard image preload (won't animate but will display)
    try {
      let url;
      if (ipfsUri && ipfsUri.startsWith("ipfs://")) {
        url = `https://ipfs.aesthetic.computer/ipfs/${ipfsUri.slice(7)}`;
      } else {
        url = ipfsUri;
      }
      const result = await _preload?.({ path: url, extension: "webp" });
      thumbnailBitmap = result?.img;
      thumbnailFrames = null;
      console.log("🪙 KEEP: Fallback thumbnail loaded:", thumbnailBitmap?.width, "x", thumbnailBitmap?.height);
    } catch (e2) {
      console.error("🪙 KEEP: All thumbnail sources failed:", e2.message);
    }
    _needsPaint?.();
  }
}

// Load KidLisp source from database
async function loadKidlispSource() {
  try {
    const response = await fetch(`/api/store-kidlisp?code=${piece}`);
    if (response.ok) {
      const data = await response.json();
      if (data.source) {
        // Sanitize for ticker display - replace newlines with ", "
        kidlispSource = data.source.replace(/\n+/g, ", ").replace(/,\s*,/g, ",");
        console.log("🪙 KEEP: Loaded KidLisp source:", kidlispSource.slice(0, 100) + "...");

        // Run local analysis on source (use original with newlines)
        analyzeSource(data.source);
      }

      // Store cached media info (last generated bundle)
      if (data.ipfsMedia) {
        cachedMedia = {
          artifactUri: data.ipfsMedia.artifactUri,
          thumbnailUri: data.ipfsMedia.thumbnailUri,
          createdAt: data.ipfsMedia.createdAt,
          sourceHash: data.ipfsMedia.sourceHash,
          depCount: data.ipfsMedia.depCount,
          packDate: data.ipfsMedia.packDate,
        };
        console.log("🪙 KEEP: Cached media:", cachedMedia);
      }

      // Check for pending rebake (bundle regenerated but not updated on chain)
      if (data.pendingRebake) {
        pendingRebake = data.pendingRebake;
        console.log("🪙 KEEP: Found pending rebake:", pendingRebake);
        // Auto-populate rebakeResult so Update Chain button appears
        rebakeResult = {
          artifactUri: pendingRebake.artifactUri,
          thumbnailUri: pendingRebake.thumbnailUri,
        };
        // Store original on-chain URIs for comparison
        if (alreadyMinted && !originalOnChainUris) {
          originalOnChainUris = {
            artifactUri: alreadyMinted.artifactUri,
            thumbnailUri: alreadyMinted.thumbnailUri,
          };
        }
      }
      _needsPaint?.();
    }
  } catch (e) {
    console.error("🪙 KEEP: Error loading KidLisp source:", e);
  }
}

// Formal local analysis of KidLisp source (matches backend analyzer v2.0.0)
function analyzeSource(source) {
  if (!source) return;

  const lines = source.split('\n').filter(l => l.trim() && !l.trim().startsWith(';'));
  const lineCount = lines.length;

  // S-expression analysis
  let depth = 0, maxDepth = 0, sexpCount = 0;
  for (const c of source) {
    if (c === '(') { depth++; sexpCount++; maxDepth = Math.max(maxDepth, depth); }
    else if (c === ')') depth = Math.max(0, depth - 1);
  }
  const density = Math.round((sexpCount / Math.max(lineCount, 1)) * 10) / 10;

  // Form analysis
  const funcCalls = source.match(/\([a-zA-Z][a-zA-Z0-9_-]*/g) || [];
  const uniqueFuncs = [...new Set(funcCalls.map(f => f.slice(1)))];

  // Behavior detection
  const behavior = {
    interactive: /\(\s*(tap|draw)\s/.test(source),
    drawable: /\(\s*draw\s/.test(source),
    animated: /\(\s*(wiggle|spin|smoothspin|zoom|pan)\s/.test(source) || /\d+\.?\d*s/.test(source),
    looping: /\d+\.?\d*s\.\.\.?/.test(source),
    timed: /\d+\.?\d*s/.test(source),
    hasAudio: /\(\s*(speaker|melody|overtone|mic|amplitude)\s/.test(source),
    hasRandomness: /\(\s*(random|noise)\s/.test(source),
    hasConditionals: /\(\s*(if|>|<|=|\?)\s/.test(source),
    hasIteration: /\(\s*(repeat|bunch|range)\s/.test(source),
    hasEffects: /\(\s*(blur|contrast|mask|steal)\s/.test(source),
    hasNetwork: /\(\s*net\s/.test(source),
    hasRainbow: /rainbow/.test(source),
    hasGradient: /fade:/.test(source),
  };
  behavior.isPure = !behavior.hasAudio && !behavior.hasNetwork;

  // Count definitions
  const varCount = (source.match(/\(\s*def\s+[a-zA-Z]/g) || []).length;
  const funcCount = (source.match(/\(\s*later\s+[a-zA-Z]/g) || []).length;

  // Dependencies
  const deps = [...new Set((source.match(/\$([a-zA-Z][a-zA-Z0-9]*)/g) || []).map(d => d.slice(1)))];

  // Size category (formal)
  let size;
  if (lineCount <= 1) size = 'Atom';
  else if (lineCount <= 3) size = 'Molecule';
  else if (lineCount <= 8) size = 'Cell';
  else if (lineCount <= 20) size = 'Organism';
  else if (lineCount <= 50) size = 'Colony';
  else size = 'Ecosystem';

  // Structure category
  let structure;
  if (maxDepth <= 2) structure = 'Flat';
  else if (maxDepth <= 4) structure = 'Nested';
  else if (maxDepth <= 6) structure = 'Deep';
  else structure = 'Recursive';

  analysisData = {
    // Structural
    lines: lineCount,
    expressions: sexpCount,
    depth: maxDepth,
    density,
    vocabulary: uniqueFuncs.length,
    size,
    structure,
    // Definitions
    varCount,
    funcCount,
    // Dependencies
    deps,
    isComposite: deps.length > 0,
    // Behavior flags
    ...behavior,
    // Top forms
    topForms: uniqueFuncs.slice(0, 5),
  };

  // Generate natural language description
  const descParts = [];

  // Size description
  const sizeDesc = {
    'Atom': 'A minimal one-liner',
    'Molecule': 'A compact sketch',
    'Cell': 'A small piece',
    'Organism': 'A developed work',
    'Colony': 'An elaborate composition',
    'Ecosystem': 'A complex system'
  }[size] || 'A piece';
  descParts.push(sizeDesc);

  // Behavior adjectives
  const adjectives = [];
  if (behavior.interactive) adjectives.push('interactive');
  if (behavior.animated && behavior.looping) adjectives.push('looping');
  else if (behavior.animated) adjectives.push('animated');
  if (behavior.hasRandomness) adjectives.push('generative');
  if (behavior.hasAudio) adjectives.push('sonic');
  if (behavior.isPure && !behavior.interactive && !behavior.animated) adjectives.push('static');

  if (adjectives.length > 0) {
    descParts[0] = sizeDesc.replace('A ', 'A ' + adjectives.slice(0, 2).join(', ') + ' ').replace('An ', 'An ' + adjectives.slice(0, 2).join(', ') + ' ');
  }

  // What it does
  const actions = [];
  if (behavior.drawable) actions.push('draws on touch');
  else if (behavior.interactive) actions.push('responds to touch');
  if (behavior.hasIteration) actions.push('repeats patterns');
  if (behavior.hasConditionals) actions.push('makes choices');

  // Visual features
  const visuals = [];
  if (behavior.hasRainbow) visuals.push('rainbow colors');
  if (behavior.hasGradient) visuals.push('color gradients');
  if (behavior.hasEffects) visuals.push('visual effects');

  // Build second part
  const features = [...actions, ...visuals];
  if (features.length > 0) {
    descParts.push('with ' + features.slice(0, 2).join(' and '));
  }

  // Dependencies
  if (deps.length > 0) {
    descParts.push('embedding $' + deps.join(', $'));
  }

  // Forms used (pick interesting ones)
  const interestingForms = uniqueFuncs.filter(f =>
    ['wiggle', 'spin', 'rainbow', 'repeat', 'tap', 'draw', 'melody', 'speaker', 'random', 'noise', 'blur'].includes(f)
  );
  if (interestingForms.length > 0 && descParts.length < 3) {
    descParts.push('using ' + interestingForms.slice(0, 3).join(', '));
  }

  analysisData.description = descParts.join(' ') + '.';

  console.log("🪙 KEEP: Formal analysis:", analysisData);
}

// Parse color name/string to RGB (like prompt.mjs)
function parseColorName(colorName) {
  if (!colorName) return { r: 200, g: 200, b: 200 };

  // Handle RGB format colors (like "192,192,192")
  if (colorName.includes(',')) {
    const parts = colorName.split(',').map(p => parseInt(p.trim()));
    return { r: parts[0] || 200, g: parts[1] || 200, b: parts[2] || 200 };
  }

  // Handle named colors
  const colorMap = {
    'cyan': { r: 64, g: 224, b: 208 },
    'teal': { r: 64, g: 224, b: 208 },
    'lime': { r: 50, g: 205, b: 50 },
    'green': { r: 34, g: 139, b: 34 },
    'mediumseagreen': { r: 60, g: 179, b: 113 },
    'yellow': { r: 255, g: 255, b: 0 },
    'orange': { r: 255, g: 165, b: 0 },
    'purple': { r: 128, g: 0, b: 128 },
    'magenta': { r: 255, g: 0, b: 255 },
    'red': { r: 255, g: 0, b: 0 },
    'gray': { r: 128, g: 128, b: 128 },
    'grey': { r: 128, g: 128, b: 128 },
    'white': { r: 255, g: 255, b: 255 },
    'pink': { r: 255, g: 182, b: 193 },
    'blue': { r: 70, g: 130, b: 180 },
    'navy': { r: 0, g: 0, b: 128 },
    'gold': { r: 255, g: 215, b: 0 },
    'silver': { r: 192, g: 192, b: 192 },
  };

  return colorMap[colorName.toLowerCase()] || { r: 200, g: 200, b: 200 };
}

// Build a color-coded string for write() using \r,g,b\text syntax
// This lets the write() function handle natural spacing
function buildColoredSourceString(source) {
  if (!source) return "";

  try {
    const tokens = tokenize(source);
    let result = "";

    // Create KidLisp instance for proper color mapping
    const tempKidlisp = new KidLisp();
    tempKidlisp.syntaxHighlightSource = source;
    tempKidlisp.isEditMode = true;

    // Track position in original source to preserve spacing
    let sourceIdx = 0;

    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];

      // Find this token in the source to get any preceding whitespace
      const tokenPos = source.indexOf(token, sourceIdx);
      if (tokenPos > sourceIdx) {
        // Add any whitespace between tokens (use gray color)
        const ws = source.slice(sourceIdx, tokenPos);
        result += `\\128,128,128\\${ws}`;
      }
      sourceIdx = tokenPos + token.length;

      // Handle fade: expressions specially
      if (token.startsWith('fade:')) {
        const coloredFadeString = tempKidlisp.colorFadeExpression(token);
        // The colorFadeExpression returns \colorname\text format - convert to RGB
        const segments = coloredFadeString.split('\\').filter(s => s);
        for (let j = 0; j < segments.length; j += 2) {
          const segmentColor = segments[j];
          const segmentText = segments[j + 1] || '';
          const rgb = parseColorName(segmentColor);
          result += `\\${rgb.r},${rgb.g},${rgb.b}\\${segmentText}`;
        }
      } else {
        // Get color from KidLisp instance
        const colorName = tempKidlisp.getTokenColor(token, tokens, i);
        const rgb = parseColorName(colorName);

        // Special override for @handles
        if (token.startsWith("@")) {
          result += `\\100,255,200\\${token}`; // Bright teal
        } else {
          result += `\\${rgb.r},${rgb.g},${rgb.b}\\${token}`;
        }
      }
    }

    return result;
  } catch (e) {
    console.error("🪙 KEEP: Color string build error:", e);
    return source; // Return plain source on error
  }
}

// Legacy function for compatibility - returns token array
function syntaxHighlightKidlisp(source) {
  if (!source) return [];

  try {
    const tokens = tokenize(source);
    const highlighted = [];

    // Create KidLisp instance for proper color mapping (like prompt.mjs)
    const tempKidlisp = new KidLisp();
    tempKidlisp.syntaxHighlightSource = source;
    tempKidlisp.isEditMode = true; // Enable edit mode to prevent transparent text

    for (let i = 0; i < tokens.length; i++) {
      const token = tokens[i];

      // Handle fade: expressions specially (like prompt.mjs)
      if (token.startsWith('fade:')) {
        const coloredFadeString = tempKidlisp.colorFadeExpression(token);
        // Parse the colored fade string format: \color1\text1\color2\text2...
        const segments = coloredFadeString.split('\\').filter(s => s);

        for (let j = 0; j < segments.length; j += 2) {
          const segmentColor = segments[j];
          const segmentText = segments[j + 1] || '';
          const rgb = parseColorName(segmentColor);
          highlighted.push({ token: segmentText, color: [rgb.r, rgb.g, rgb.b] });
        }
      } else {
        // Get color from KidLisp instance
        const colorName = tempKidlisp.getTokenColor(token, tokens, i);
        const rgb = parseColorName(colorName);

        // Special override for @handles
        if (token.startsWith("@")) {
          highlighted.push({ token, color: [100, 255, 200] }); // Bright teal
        } else {
          highlighted.push({ token, color: [rgb.r, rgb.g, rgb.b] });
        }
      }
    }

    return highlighted;
  } catch (e) {
    console.error("🪙 KEEP: Syntax highlight error:", e);
    return [{ token: source, color: [200, 200, 200] }];
  }
}

async function runProcess(forceRegenerate = false) {
  console.log("🪙 KEEP: Starting mint process for $" + piece + (forceRegenerate ? " (force regenerate)" : ""));
  console.log("🪙 KEEP: Network:", NETWORK, "Contract:", KEEPS_CONTRACT);
  console.log("🪙 KEEP: Staging mode:", KEEPS_STAGING);

  // === STEP 1: Connect Wallet ===
  // Reset timer when process actually starts (not counting initial checks)
  startTime = Date.now();
  mintCancelled = false;
  
  // Track timing for each stage
  const stageTimes = {};
  
  // Create abort controller for cancellation
  mintAbortController = new AbortController();
  const { signal } = mintAbortController;

  // Start getting auth token in parallel (don't await yet)
  const tokenPromise = _net?.getToken?.();

  setStep("wallet", "active", "Checking wallet...");
  try {
    walletAddress = await _api.tezos.address();
    if (!walletAddress) {
      setStep("wallet", "active", "Opening Temple Wallet...");
      walletAddress = await _api.tezos.connect(NETWORK);
    }
    if (!walletAddress) {
      setStep("wallet", "error", "Connection cancelled");
      return;
    }
    // Show truncated address with network
    const shortAddr = walletAddress.slice(0, 6) + ".." + walletAddress.slice(-4);
    const netLabel = NETWORK === "mainnet" ? "Mainnet" : "Ghostnet";
    setStep("wallet", "done", `${shortAddr} on ${netLabel}`);
  } catch (e) {
    setStep("wallet", "error", e.message);
    return;
  }

  // === STEPS 2-7: Server Preparation ===
  setStep("validate", "active", `Validating $${piece}...`);

  // Get current screen dimensions for thumbnail aspect ratio
  const screenWidth = _screen?.width || 128;
  const screenHeight = _screen?.height || 128;

  try {
    // Await the token that was fetching in parallel
    const token = await tokenPromise;
    const response = await fetch("/api/keep-mint", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        ...(token ? { "Authorization": `Bearer ${token}` } : {}),
      },
      body: JSON.stringify({
        piece,
        walletAddress,
        network: NETWORK,
        screenWidth,
        screenHeight,
        regenerate: forceRegenerate,
      }),
      signal, // Enable cancellation
    });

    if (!response.ok) {
      setStep("validate", "error", `Server error ${response.status}`);
      return;
    }

    // Process events with staggered delays so user sees each step
    const delay = (ms) => new Promise(r => setTimeout(r, ms));
    const STEP_DELAY = 350; // ms between each visual step
    let firstEvent = true;
    let lastEventType = null;
    let lastEventData = null;

    const handleEvent = async (eventType, eventData) => {
      lastEventType = eventType;
      lastEventData = eventData;

      if (eventType === "progress" && eventData?.stage) {
        const { stage, message, source, author } = eventData;
        const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
        console.log(`🪙 KEEP SSE [${elapsed}s]: ${stage} →`, message || '(no message)', eventData);
        
        // Track when each stage starts
        if (!stageTimes[stage]) {
          stageTimes[stage] = { start: Date.now() };
        }

        // No delay on first event - start immediately
        if (!firstEvent) await delay(STEP_DELAY);
        firstEvent = false;

        // Check if message indicates cached/reused content
        const isCached = message?.includes("cached") || message?.includes("Cached");

        // Handle each stage - server may send in any order when using cache
        if (stage === "validate") {
          // Only mark done on final validation message
          if (message?.includes("passed") || message?.includes("✓")) {
            setStep("validate", "done", `$${piece} verified`);
          } else if (message?.includes("Authenticated")) {
            setStep("validate", "active", message);
          } else {
            setStep("validate", "active", message || "Validating...");
          }
        } else if (stage === "details") {
          // Capture piece details from server
          // Sanitize source: replace newlines with ", " for single-line ticker display
          if (eventData.source) sourceCode = eventData.source.replace(/\n+/g, ", ").replace(/,\s*,/g, ",").trim();
          if (eventData.author) pieceAuthor = eventData.author;
          if (eventData.createdAt) pieceCreatedAt = eventData.createdAt;
          if (eventData.sourceLength) pieceSourceLength = eventData.sourceLength;
          console.log("🪙 KEEP: Piece details received", { pieceAuthor, pieceCreatedAt, pieceSourceLength });
        } else if (stage === "analyze") {
          // Sanitize source: replace newlines with ", " for single-line ticker display
          if (source) sourceCode = source.replace(/\n+/g, ", ").replace(/,\s*,/g, ",").trim();
          if (author) pieceAuthor = author;
          // Mark done if we have complexity info or it's done
          if (message?.includes("complexity") || message?.includes("✓")) {
            const authorInfo = pieceAuthor ? `by @${pieceAuthor}` : "Analyzed";
            setStep("analyze", "done", authorInfo);
          } else {
            setStep("analyze", "active", "Analyzing...");
          }
        } else if (stage === "thumbnail") {
          const detail = isCached ? "Cached" : message || "WebP ready";
          setStep("thumbnail", "done", detail);
        } else if (stage === "bundle") {
          const detail = isCached ? "Cached" : message || "HTML packed";
          setStep("bundle", "done", detail);
        } else if (stage === "ipfs") {
          // Track IPFS progress more granularly
          if (stageTimes.ipfs) {
            stageTimes.ipfs.lastUpdate = Date.now();
          }
          const detail = isCached ? "Cached" : message || "Pinned";
          // Only mark done if message indicates completion
          if (message?.includes("Pinned") || message?.includes("Cached") || message?.includes("ipfs://")) {
            setStep("ipfs", "done", detail);
            console.log(`🪙 KEEP: IPFS complete in ${stageTimes.ipfs ? ((Date.now() - stageTimes.ipfs.start) / 1000).toFixed(1) : '?'}s`);
          } else {
            setStep("ipfs", "active", detail);
          }
        } else if (stage === "metadata") {
          // Mark done if uploaded
          if (message?.includes("uploaded") || message?.includes("✓")) {
            setStep("metadata", "done", "FA2 ready");
          } else {
            setStep("metadata", "active", "Building...");
          }
        } else if (stage === "ready") {
          // Mark all intermediate steps done if not already
          for (const stepId of ["validate", "analyze", "thumbnail", "bundle", "ipfs", "metadata"]) {
            const step = timeline.find(t => t.id === stepId);
            if (step && step.status !== "done") {
              setStep(stepId, "done", step.detail || "Done");
            }
          }
          await delay(STEP_DELAY);
          setStep("review", "active", null); // Button speaks for itself
        }
      }

      if (eventType === "prepared" && eventData) {
        const totalElapsed = ((Date.now() - startTime) / 1000).toFixed(1);
        console.log(`🪙 KEEP: Preparation complete in ${totalElapsed}s`, eventData);
        console.log(`🪙 KEEP: Stage times:`, Object.entries(stageTimes).map(([k, v]) => 
          `${k}: ${v.lastUpdate ? ((v.lastUpdate - v.start) / 1000).toFixed(1) : '?'}s`
        ).join(', '));
        preparedData = eventData;
        await delay(STEP_DELAY);
        // Update timeline label with actual fee from contract
        const reviewStep = timeline.find(t => t.id === "review");
        if (reviewStep && preparedData.mintFee) {
          reviewStep.label = `Pay ${preparedData.mintFee} ꜩ Toll`;
        }
        setStep("review", "active", null); // No detail text - button speaks for itself
        // Load thumbnail for preview during minting
        if (preparedData.thumbnailUri) {
          loadThumbnail(preparedData.thumbnailUri);
        }
      } else if (eventType === "error" && eventData) {
        const totalElapsed = ((Date.now() - startTime) / 1000).toFixed(1);
        console.error(`🪙 KEEP ERROR [${totalElapsed}s]:`, eventData.error || eventData.message);
        const active = getActiveStep();
        if (active) setStep(active.id, "error", eventData.error || eventData.message);
      }
    };

    if (response.body?.getReader) {
      const reader = response.body.getReader();
      const decoder = new TextDecoder();
      let buffer = "";

      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        buffer += decoder.decode(value, { stream: true });
        const chunks = buffer.split("\n\n");
        buffer = chunks.pop() || "";

        for (const chunk of chunks) {
          const lines = chunk.split("\n");
          let eventType = null;
          let eventData = null;
          for (const line of lines) {
            if (line.startsWith("event: ")) eventType = line.slice(7);
            else if (line.startsWith("data: ")) {
              try { eventData = JSON.parse(line.slice(6)); } catch {}
            }
          }
          if (eventType) {
            await handleEvent(eventType, eventData);
            if (eventType === "error") return;
          }
        }
      }

      if (buffer.trim()) {
        const lines = buffer.split("\n");
        let eventType = null;
        let eventData = null;
        for (const line of lines) {
          if (line.startsWith("event: ")) eventType = line.slice(7);
          else if (line.startsWith("data: ")) {
            try { eventData = JSON.parse(line.slice(6)); } catch {}
          }
        }
        if (eventType) await handleEvent(eventType, eventData);
      }
    } else {
      const text = await response.text();
      const events = text.split("\n\n").filter(e => e.trim());
      for (const event of events) {
        const lines = event.split("\n");
        let eventType = null, eventData = null;

        for (const line of lines) {
          if (line.startsWith("event: ")) eventType = line.slice(7);
          else if (line.startsWith("data: ")) {
            try { eventData = JSON.parse(line.slice(6)); } catch {}
          }
        }

        if (eventType) {
          await handleEvent(eventType, eventData);
          if (eventType === "error") return;
        }
      }
    }

    if (!preparedData) {
      const lastStage = lastEventData?.stage || lastEventType || "unknown";
      const tail = lastEventData?.message ? ` (last: ${lastEventData.message})` : "";
      if (!forceRegenerate && !mintCancelled) {
        setStep("validate", "active", `Retrying with regenerate… (${lastStage})`);
        await delay(350);
        return await runProcess(true);
      }
      setStep("validate", "error", `Server did not return prepared data (${lastStage})${tail}`);
    }

  } catch (e) {
    // Handle cancellation gracefully
    if (e.name === "AbortError" || mintCancelled) {
      const active = getActiveStep();
      if (active) setStep(active.id, "error", "Cancelled");
      console.log("🪙 KEEP: Mint preparation cancelled by user");
    } else {
      const active = getActiveStep();
      if (active) setStep(active.id, "error", e.message);
    }
  } finally {
    mintAbortController = null;
  }
}

function cancelMintPreparation() {
  if (mintAbortController) {
    mintCancelled = true;
    mintAbortController.abort();
    mintAbortController = null;
    console.log("🪙 KEEP: User requested cancellation");
    _needsPaint?.();
  }
}

function isPreparationInProgress() {
  // True if we're between wallet connect and review step (server preparation phase)
  const walletStep = timeline.find(t => t.id === "wallet");
  const reviewStep = timeline.find(t => t.id === "review");
  const completeStep = timeline.find(t => t.id === "complete");
  
  // In progress if wallet is done and review is not yet active/done, and not already complete or errored
  return walletStep?.status === "done" && 
         reviewStep?.status !== "active" && 
         reviewStep?.status !== "done" &&
         completeStep?.status !== "done" &&
         !hasError() &&
         mintAbortController !== null;
}

async function signAndMint() {
  setStep("review", "done", "Toll paid");
  setStep("sign", "active", "Check your wallet...");

  try {
    txHash = await _api.tezos.call(
      preparedData.contractAddress,
      preparedData.entrypoint,
      preparedData.michelsonParams.value,
      preparedData.mintFee
    );

    setStep("sign", "done", `TX: ${txHash}`);
    setStep("complete", "active", "Confirming on-chain...");

    // Wait and fetch token ID with retries (indexer may take a moment)
    const tokenName = `$${piece}`;
    const mintNetwork = preparedData.network || NETWORK;

    for (let attempt = 0; attempt < 5 && !tokenId; attempt++) {
      await new Promise(r => setTimeout(r, 3000));
      try {
        tokenId = await findTokenByName(tokenName, mintNetwork, preparedData.contractAddress);
        if (tokenId) {
          console.log(`🪙 KEEP: Found token #${tokenId} for ${tokenName} on attempt ${attempt + 1}`);
        }
      } catch (e) {
        console.warn(`🪙 KEEP: Token fetch attempt ${attempt + 1} failed:`, e.message);
      }
      if (!tokenId && attempt < 4) {
        setStep("complete", "active", `Waiting for indexer... (${attempt + 2}/5)`);
      }
    }

    const tokenInfo = tokenId ? `Token #${tokenId}` : "Kept!";
    setStep("complete", "done", tokenInfo);

    // Record the mint in MongoDB so it shows up as "kept"
    try {
      const confirmRes = await fetch("/api/keep-confirm", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          "Authorization": `Bearer ${await _api.authorize()}`,
        },
        body: JSON.stringify({
          piece,
          tokenId,
          txHash,
          walletAddress,
          network: preparedData.network,
          contractAddress: preparedData.contractAddress,
          artifactUri: preparedData.artifactUri,
          thumbnailUri: preparedData.thumbnailUri,
          metadataUri: preparedData.metadataUri,
        }),
      });
      if (confirmRes.ok) {
        console.log("🪙 KEEP: Recorded mint in database");
      } else {
        console.warn("🪙 KEEP: Failed to record mint:", await confirmRes.text());
      }
    } catch (e) {
      console.warn("🪙 KEEP: Error recording mint:", e.message);
    }

  } catch (e) {
    setStep("sign", "error", e.message || "Transaction rejected");
  }
}

function openUrl(url) {
  if (url.startsWith("ipfs://")) url = `https://ipfs.io/ipfs/${url.slice(7)}`;
  _jump?.(`out:${url}`);
}

// Calculate progress through timeline (0 = start, 1 = complete)
function getProgress() {
  const doneCount = timeline.filter(t => t.status === "done").length;
  return doneCount / timeline.length;
}

// Spawn a particle at position
function spawnParticle(x, y, color) {
  particles.push({
    x, y,
    vx: (Math.random() - 0.5) * 2,
    vy: -Math.random() * 2 - 1,
    color,
    life: 60 + Math.random() * 40,
    size: 1 + Math.random() * 2,
  });
}

function sim() {
  rotation += 0.03;

  // Update particles
  for (let i = particles.length - 1; i >= 0; i--) {
    const p = particles[i];
    p.x += p.vx;
    p.y += p.vy;
    p.vy += 0.05; // gravity
    p.life--;
    if (p.life <= 0) particles.splice(i, 1);
  }

  // Spawn particles based on progress
  const progress = getProgress();
  if (progress > 0 && Math.random() < 0.15 * progress) {
    const colors = progress > 0.8
      ? [pal.positive, [150, 255, 200], [200, 255, 100]] // Green success
      : progress > 0.5
        ? [pal.gold, [255, 180, 80], [255, 220, 150]] // Yellow/orange
        : [[200, 100, 255], [255, 100, 200], [180, 80, 220]]; // Purple/pink
    spawnParticle(
      Math.random() * (_screen?.width || 200),
      (_screen?.height || 150) + 10,
      colors[floor(Math.random() * colors.length)]
    );
  }

  // Scroll ticker for already minted view
  if (alreadyMinted && kidlispSource) {
    tickerOffset += 0.5;
  }

  // Carousel animation - smoothly slide to current step
  const activeIdx = timeline.findIndex(t => t.status === "active");
  if (activeIdx >= 0 && activeIdx !== lastActiveIndex) {
    carouselTargetIndex = activeIdx;
    lastActiveIndex = activeIdx;
  }
  // Smooth interpolation toward target
  const targetX = carouselTargetIndex * 100; // 100 units per step
  carouselCurrentX += (targetX - carouselCurrentX) * 0.08; // Ease toward target

  // Animate thumbnail frames if we have an animated WebP
  if (thumbnailFrames && thumbnailFrames.frameCount > 1) {
    const now = typeof performance !== 'undefined' ? performance.now() : Date.now();
    const currentFrame = thumbnailFrames.frames[thumbnailFrameIndex];
    const frameDuration = currentFrame?.duration || 100; // Default 100ms per frame

    if (now - thumbnailLastFrameTime >= frameDuration) {
      thumbnailFrameIndex = (thumbnailFrameIndex + 1) % thumbnailFrames.frameCount;
      thumbnailLastFrameTime = now;

      // Update the bitmap with the new frame
      // Note: pixels is already a Uint8ClampedArray transferred from bios
      const newFrame = thumbnailFrames.frames[thumbnailFrameIndex];
      thumbnailBitmap = {
        width: thumbnailFrames.width,
        height: thumbnailFrames.height,
        pixels: newFrame.pixels
      };
    }
  }

  // Animate piece preview frames (for confirmation view)
  if (piecePreviewFrames && piecePreviewFrames.frameCount > 1) {
    const now = typeof performance !== 'undefined' ? performance.now() : Date.now();
    const currentFrame = piecePreviewFrames.frames[piecePreviewFrameIndex];
    const frameDuration = currentFrame?.duration || 100;

    if (now - piecePreviewLastFrameTime >= frameDuration) {
      piecePreviewFrameIndex = (piecePreviewFrameIndex + 1) % piecePreviewFrames.frameCount;
      piecePreviewLastFrameTime = now;

      const newFrame = piecePreviewFrames.frames[piecePreviewFrameIndex];
      piecePreviewBitmap = {
        width: piecePreviewFrames.width,
        height: piecePreviewFrames.height,
        pixels: newFrame.pixels
      };
    }
  }

  // Always animate for already minted view, confirmation view, or when minting is active
  return alreadyMinted || waitingConfirmation || (!hasError() && timeline.some(t => t.status === "active"));
}

// MatrixChunky8 button constants (matching products.mjs style)
const MC8_CHAR_WIDTH = 4;
const MC8_CHAR_HEIGHT = 8;
const MC8_PAD_LEFT = 5;
const MC8_PAD_RIGHT = 5; // Balanced with left padding for better appearance
const MC8_PAD_TOP = 4;
const MC8_PAD_BOTTOM = 2;

// Calculate button dimensions for MatrixChunky8 text
function mc8ButtonSize(text) {
  return {
    w: text.length * MC8_CHAR_WIDTH + MC8_PAD_LEFT + MC8_PAD_RIGHT,
    h: MC8_CHAR_HEIGHT + MC8_PAD_TOP + MC8_PAD_BOTTOM
  };
}

// Large button constants (6x10 font)
const LG_CHAR_WIDTH = 6;
const LG_CHAR_HEIGHT = 10;
const LG_PAD_X = 12;
const LG_PAD_Y = 8;

// Calculate button dimensions for large text
function lgButtonSize(text) {
  return {
    w: text.length * LG_CHAR_WIDTH + LG_PAD_X * 2,
    h: LG_CHAR_HEIGHT + LG_PAD_Y * 2
  };
}

// Paint a large button with proper padding
function paintLgBtn(x, y, text, $, scheme, isHover = false, isDisabled = false) {
  const { w, h } = lgButtonSize(text);
  const s = isDisabled ? scheme.disabled : (isHover ? scheme.hover : scheme.normal);

  // Background
  $.ink(s.bg[0], s.bg[1], s.bg[2]).box(x, y, w, h);

  // Outline (thicker, 2px effect)
  $.ink(s.outline[0], s.outline[1], s.outline[2], s.outlineAlpha || 180)
    .line(x, y, x + w, y)                    // Top
    .line(x, y + 1, x + w, y + 1)            // Top inner
    .line(x, y + h - 1, x + w, y + h - 1)    // Bottom inner
    .line(x, y + h, x + w, y + h)            // Bottom
    .line(x, y, x, y + h)                    // Left
    .line(x + 1, y, x + 1, y + h)            // Left inner
    .line(x + w - 1, y, x + w - 1, y + h)    // Right inner
    .line(x + w, y, x + w, y + h);           // Right

  // Text (centered)
  $.ink(s.text[0], s.text[1], s.text[2]).write(text, { x: x + LG_PAD_X, y: y + LG_PAD_Y });

  return { x, y, w, h };
}

// Paint a MatrixChunky8 button with proper padding (like products.mjs)
// scheme format: [[bg r,g,b], [outline r,g,b], [text r,g,b]]
function paintMC8Btn(x, y, text, $, scheme, isHover = false, isDisabled = false) {
  const { w, h } = mc8ButtonSize(text);
  const s = isDisabled ? scheme.disabled : (isHover ? scheme.hover : scheme.normal);

  // Background
  $.ink(s.bg[0], s.bg[1], s.bg[2]).box(x, y, w, h);

  // Outline (draw all 4 edges like products.mjs)
  $.ink(s.outline[0], s.outline[1], s.outline[2], s.outlineAlpha || 150)
    .line(x, y, x + w, y)                    // Top
    .line(x, y + h, x + w, y + h)            // Bottom
    .line(x, y, x, y + h)                    // Left
    .line(x + w, y, x + w, y + h);           // Right

  // Text
  $.ink(s.text[0], s.text[1], s.text[2]).write(text, { x: x + MC8_PAD_LEFT, y: y + MC8_PAD_TOP }, undefined, undefined, false, "MatrixChunky8");

  return { x, y, w, h };
}

// Phase colors for different parts of the process

function paint($) {
  const { wipe, ink, box, screen, paste } = $;
  // Set theme palette based on dark mode
  pal = $.dark ? scheme.dark : scheme.light;
  PHASE_COLORS = $.dark ? PHASE_COLORS_DARK : PHASE_COLORS_LIGHT;
  pal = $.dark ? scheme.dark : scheme.light;

  const w = screen.width, h = screen.height;

  // === ALREADY MINTED VIEW ===
  if (alreadyMinted) {
    // Dark teal background with shimmer
    const pulse = sin(rotation * 1.5) * 5;
    wipe(pal.bgShimmer[0] + pulse, pal.bgShimmer[1] + pulse, pal.bgShimmer[2] + pulse);

    // Gold shimmer border
    for (let i = 0; i < w; i += 3) {
      const shimmer = sin(rotation * 2 + i * 0.2) * 0.5 + 0.5;
      const alpha = floor(shimmer * 80);
      ink(255, 200, 100, alpha).box(i, 0, 2, 1);
      ink(255, 200, 100, alpha).box(i, h - 1, 2, 1);
    }

    const margin = 6;
    let y = 4;

    // Header with network/staging badge
    const baseNetLabel = (alreadyMinted.network || NETWORK).toUpperCase();
    const netLabel = KEEPS_STAGING ? "STAGING V4" : baseNetLabel;
    const isMainnet = baseNetLabel === "MAINNET";
    const netColor = KEEPS_STAGING ? [255, 180, 100] : (isMainnet ? [100, 220, 100] : [220, 180, 100]);

    ink(255, 220, 100).write("KEPT", { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    y += 10;
    // Network badge
    ink(netColor[0], netColor[1], netColor[2]).write(`${netLabel} #${alreadyMinted.tokenId}`, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    y += 10;
    ink(100, 220, 180).write(`$${piece}`, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    y += 10;

    // Ownership indicator - show if connected wallet owns the token
    const isTokenOwner = walletAddress && alreadyMinted.owner && walletAddress === alreadyMinted.owner;
    if (isTokenOwner) {
      ink(100, 255, 150).write("★ You own this", { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 10;
    } else if (alreadyMinted.owner) {
      const shortOwner = alreadyMinted.owner.slice(0, 8) + "..";
      ink(150, 150, 170).write(`Owner: ${shortOwner}`, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 10;
    }
    y += 2;

    // Thumbnail display (smaller)
    if (thumbnailBitmap) {
      const thumbSize = min(48, w - 20, h - 100);
      const thumbX = floor((w - thumbSize) / 2);
      paste(thumbnailBitmap, thumbX, y, { scale: thumbSize / (thumbnailBitmap.width || 256) });
      y += thumbSize + 4;
    } else if (alreadyMinted.thumbnailUri) {
      ink(40, 55, 60).box(floor((w - 40) / 2), y, 40, 30);
      ink(80, 100, 110).write("...", { x: w/2, y: y + 10, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 34;
    }

    // === SYNC STATUS SECTION ===
    // Determine sync state by comparing on-chain URIs with latest generated
    const latestMedia = pendingRebake || cachedMedia;
    const onChainArtifact = alreadyMinted.artifactUri;
    const onChainThumb = alreadyMinted.thumbnailUri;
    const latestArtifact = latestMedia?.artifactUri;
    const latestThumb = latestMedia?.thumbnailUri;

    const artifactMatches = onChainArtifact === latestArtifact;
    const thumbMatches = onChainThumb === latestThumb;
    const isSynced = latestArtifact && artifactMatches && thumbMatches;
    const hasPending = pendingRebake && (!artifactMatches || !thumbMatches);
    // If no cached bundle but on-chain exists, assume synced (legacy mint)
    const isLegacySynced = !latestMedia && onChainArtifact;

    // Sync status indicator
    ink(35, 50, 55, 200).box(0, y, w, 12);
    if (isSynced || isLegacySynced) {
      ink(100, 220, 150).write("[ok] Synced", { x: w/2, y: y + 2, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    } else if (hasPending) {
      ink(255, 180, 100).write("!! Pending update", { x: w/2, y: y + 2, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    } else if (latestMedia) {
      ink(255, 200, 100).write("≠ Out of sync", { x: w/2, y: y + 2, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    } else {
      ink(180, 180, 120).write(">> Not bundled", { x: w/2, y: y + 2, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    }
    y += 14;

    // === CURRENT VERSION SECTION ===
    // Show rebaked URIs if available, otherwise show on-chain URIs
    const currentArtifact = rebakeResult?.artifactUri || pendingRebake?.artifactUri || alreadyMinted.artifactUri;
    const currentThumb = rebakeResult?.thumbnailUri || pendingRebake?.thumbnailUri || alreadyMinted.thumbnailUri;
    const hasRebaked = (rebakeResult || pendingRebake) && (currentArtifact !== alreadyMinted.artifactUri);

    // Format date for header
    let versionDateStr = "";
    if (hasRebaked) {
      const rebakedDate = (rebakeResult || pendingRebake)?.packDate || (rebakeResult || pendingRebake)?.createdAt;
      if (rebakedDate) {
        const d = new Date(rebakedDate);
        if (!isNaN(d.getTime())) {
          versionDateStr = ` ${d.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "2-digit" })}`;
        }
      }
      ink(200, 160, 100).write("REBAKED" + versionDateStr, { x: margin, y }, undefined, undefined, false, "MatrixChunky8");
    } else if (alreadyMinted.mintedAt) {
      const d = new Date(alreadyMinted.mintedAt);
      versionDateStr = ` since ${d.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "2-digit" })}`;
      ink(120, 180, 200).write("ON-CHAIN" + versionDateStr, { x: margin, y }, undefined, undefined, false, "MatrixChunky8");
    }
    y += 10;

    const linkScheme = hasRebaked ? pal.btnCached : pal.btnLink;

    let linkX = margin;
    if (currentArtifact) {
      const htmlSize = mc8ButtonSize("HTML");
      htmlBtn.btn.box.x = linkX;
      htmlBtn.btn.box.y = y;
      htmlBtn.btn.box.w = htmlSize.w;
      htmlBtn.btn.box.h = htmlSize.h;
      paintMC8Btn(linkX, y, "HTML", { ink, line: ink }, linkScheme, htmlBtn.btn.down || htmlBtn.btn.over);
      linkX += htmlSize.w + 4;
    }
    if (currentThumb) {
      const thumbBtnSize = mc8ButtonSize("THUMB");
      thumbBtn.btn.box.x = linkX;
      thumbBtn.btn.box.y = y;
      thumbBtn.btn.box.w = thumbBtnSize.w;
      thumbBtn.btn.box.h = thumbBtnSize.h;
      paintMC8Btn(linkX, y, "THUMB", { ink, line: ink }, linkScheme, thumbBtn.btn.down || thumbBtn.btn.over);
      linkX += thumbBtnSize.w + 4;
    }
    // META button - link to TzKT metadata view
    const metaBtnSize = mc8ButtonSize("META");
    metaBtn.btn.box.x = linkX;
    metaBtn.btn.box.y = y;
    metaBtn.btn.box.w = metaBtnSize.w;
    metaBtn.btn.box.h = metaBtnSize.h;
    paintMC8Btn(linkX, y, "META", { ink, line: ink }, pal.btnLink, metaBtn.btn.down || metaBtn.btn.over);
    linkX += metaBtnSize.w + 4;
    // DOCS button - link to kidlisp.com/keeps documentation
    const docsBtnSize = mc8ButtonSize("DOCS");
    docsBtn.btn.box.x = linkX;
    docsBtn.btn.box.y = y;
    docsBtn.btn.box.w = docsBtnSize.w;
    docsBtn.btn.box.h = docsBtnSize.h;
    paintMC8Btn(linkX, y, "DOCS", { ink, line: ink }, pal.btnLink, docsBtn.btn.down || docsBtn.btn.over);
    linkX += docsBtnSize.w + 4;
    // Show short CID
    if (currentArtifact) {
      const cid = currentArtifact.replace("ipfs://", "").split("/")[0];
      const cidColor = hasRebaked ? [120, 100, 80] : [80, 120, 140];
      ink(...cidColor).write(cid.slice(0, 8) + "...", { x: linkX + 2, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
    }
    y += mc8ButtonSize("X").h + 4;

    // === OLD ON-CHAIN VERSION (if rebaked and different) ===
    if (hasRebaked) {
      ink(100, 140, 160).write("OLD ON-CHAIN", { x: margin, y }, undefined, undefined, false, "MatrixChunky8");
      y += 10;

      linkX = margin;
      if (alreadyMinted.artifactUri) {
        const oldHtmlSize = mc8ButtonSize("HTML");
        oldHtmlBtn.btn.box.x = linkX;
        oldHtmlBtn.btn.box.y = y;
        oldHtmlBtn.btn.box.w = oldHtmlSize.w;
        oldHtmlBtn.btn.box.h = oldHtmlSize.h;
        paintMC8Btn(linkX, y, "HTML", { ink, line: ink }, pal.btnLink, oldHtmlBtn.btn.down || oldHtmlBtn.btn.over);
        linkX += oldHtmlSize.w + 4;
      }
      if (alreadyMinted.thumbnailUri) {
        const oldThumbSize = mc8ButtonSize("THUMB");
        oldThumbBtn.btn.box.x = linkX;
        oldThumbBtn.btn.box.y = y;
        oldThumbBtn.btn.box.w = oldThumbSize.w;
        oldThumbBtn.btn.box.h = oldThumbSize.h;
        paintMC8Btn(linkX, y, "THUMB", { ink, line: ink }, pal.btnLink, oldThumbBtn.btn.down || oldThumbBtn.btn.over);
        linkX += oldThumbSize.w + 4;
      }
      // Show short CID
      if (alreadyMinted.artifactUri) {
        const cid = alreadyMinted.artifactUri.replace("ipfs://", "").split("/")[0];
        ink(80, 120, 140).write(cid.slice(0, 8) + "...", { x: linkX + 2, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
      }
      y += mc8ButtonSize("X").h + 4;
    }

    // === ANALYSIS SECTION (Full Details) ===
    if (analysisData) {
      const isOutdated = onChainAnalyzerVersion && onChainAnalyzerVersion !== "2.0.0";
      const baseHeight = 72;
      const sectionHeight = isOutdated ? baseHeight + 12 : baseHeight;

      ink(25, 35, 45, 220).box(0, y, w, sectionHeight);

      // Row 1: Description ticker
      if (analysisData.description) {
        const desc = analysisData.description;
        const descLen = desc.length * 4;
        const gap = 60;
        const repeatWidth = descLen + gap;
        let startX = -(tickerOffset * 0.5 % repeatWidth);
        for (let repeat = 0; repeat < 3; repeat++) {
          const x = startX + repeat * repeatWidth;
          if (x < w && x + descLen > 0) {
            ink(180, 200, 220).write(desc, { x, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
          }
        }
      }

      // Row 2: Structure info
      const r2Y = y + 14;
      ink(120, 160, 180).write(`${analysisData.lines} lines`, { x: margin, y: r2Y }, undefined, undefined, false, "MatrixChunky8");
      ink(100, 140, 160).write(`${analysisData.expressions} expr`, { x: margin + 44, y: r2Y }, undefined, undefined, false, "MatrixChunky8");
      ink(90, 130, 150).write(`depth ${analysisData.depth}`, { x: margin + 88, y: r2Y }, undefined, undefined, false, "MatrixChunky8");
      // Size/Structure on right
      const sizeColor = { Atom: [100, 255, 200], Molecule: [150, 220, 180], Cell: [180, 200, 160], Organism: [200, 180, 140], Colony: [220, 150, 120], Ecosystem: [255, 120, 100] }[analysisData.size] || [150, 150, 150];
      ink(sizeColor[0], sizeColor[1], sizeColor[2]).write(analysisData.size, { x: w - margin, y: r2Y, right: true }, undefined, undefined, false, "MatrixChunky8");

      // Row 3: Behavior tags
      const r3Y = y + 25;
      let tagX = margin;
      if (analysisData.interactive) { ink(100, 255, 150).write("●touch", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 36; }
      if (analysisData.drawable) { ink(100, 220, 180).write("●draw", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 32; }
      if (analysisData.animated) { ink(100, 200, 255).write("●motion", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 40; }
      if (analysisData.looping) { ink(100, 255, 255).write("●loops", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 36; }
      if (analysisData.hasAudio) { ink(255, 100, 200).write("●sound", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 36; }
      if (analysisData.hasRandomness) { ink(255, 220, 100).write("●random", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 44; }
      if (analysisData.hasNetwork) { ink(255, 100, 100).write("●network", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); tagX += 48; }
      if (analysisData.isPure && tagX === margin) { ink(200, 200, 200).write("●static", { x: tagX, y: r3Y }, undefined, undefined, false, "MatrixChunky8"); }

      // Row 4: Logic and definitions
      const r4Y = y + 36;
      let logicX = margin;
      if (analysisData.hasConditionals) { ink(255, 180, 100).write("if/else", { x: logicX, y: r4Y }, undefined, undefined, false, "MatrixChunky8"); logicX += 40; }
      if (analysisData.hasIteration) { ink(255, 160, 80).write("repeat", { x: logicX, y: r4Y }, undefined, undefined, false, "MatrixChunky8"); logicX += 40; }
      if (analysisData.varCount > 0) { ink(180, 140, 220).write(`${analysisData.varCount} var`, { x: logicX, y: r4Y }, undefined, undefined, false, "MatrixChunky8"); logicX += 32; }
      if (analysisData.funcCount > 0) { ink(160, 120, 200).write(`${analysisData.funcCount} fn`, { x: logicX, y: r4Y }, undefined, undefined, false, "MatrixChunky8"); logicX += 28; }
      // Colors on right
      let colorX = w - margin;
      if (analysisData.hasRainbow) {
        ink(255, 100, 100).write("r", { x: colorX - 24, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
        ink(255, 200, 100).write("a", { x: colorX - 20, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
        ink(255, 255, 100).write("i", { x: colorX - 16, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
        ink(100, 255, 100).write("n", { x: colorX - 12, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
        ink(100, 200, 255).write("b", { x: colorX - 8, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
        ink(200, 100, 255).write("o", { x: colorX - 4, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
        ink(255, 100, 200).write("w", { x: colorX, y: r4Y }, undefined, undefined, false, "MatrixChunky8");
      } else if (analysisData.hasGradient) {
        ink(150, 150, 200).write("gradient", { x: colorX, y: r4Y, right: true }, undefined, undefined, false, "MatrixChunky8");
      }

      // Row 5: Dependencies and top forms
      const r5Y = y + 47;
      if (analysisData.isComposite && analysisData.deps?.length > 0) {
        ink(200, 180, 140).write("embeds " + analysisData.deps.map(d => '$' + d).join(' '), { x: margin, y: r5Y }, undefined, undefined, false, "MatrixChunky8");
      } else if (analysisData.topForms?.length > 0) {
        ink(100, 120, 140).write("uses: ", { x: margin, y: r5Y }, undefined, undefined, false, "MatrixChunky8");
        let formX = margin + 28;
        analysisData.topForms.slice(0, 4).forEach((f, i) => {
          const hue = (i * 50) % 360;
          const r = Math.round(140 + 40 * Math.cos(hue * Math.PI / 180));
          const g = Math.round(140 + 40 * Math.cos((hue - 120) * Math.PI / 180));
          const b = Math.round(140 + 40 * Math.cos((hue - 240) * Math.PI / 180));
          ink(r, g, b).write(f, { x: formX, y: r5Y }, undefined, undefined, false, "MatrixChunky8");
          formX += f.length * 4 + 6;
        });
      }
      // Vocabulary count on right
      ink(120, 140, 160).write(`${analysisData.vocabulary} forms`, { x: w - margin, y: r5Y, right: true }, undefined, undefined, false, "MatrixChunky8");

      // Row 6: Outdated warning
      if (isOutdated) {
        ink(255, 150, 100, 200).write(`!! Analyzer v${onChainAnalyzerVersion} outdated - Resync to update`, { x: margin, y: y + 60 }, undefined, undefined, false, "MatrixChunky8");
      }

      y += sectionHeight + 4;
    }

    // Syntax-highlighted source code ticker
    if (kidlispSource) {
      ink(25, 35, 40, 200).box(0, y, w, 14);
      const coloredSource = buildColoredSourceString(kidlispSource);
      const sourceLen = kidlispSource.length * 4;
      const gap = 40;
      const repeatWidth = sourceLen + gap;
      let startX = -(tickerOffset % repeatWidth);
      for (let repeat = 0; repeat < 3; repeat++) {
        const x = startX + repeat * repeatWidth;
        if (x < w && x + sourceLen > 0) {
          ink(200, 200, 200).write(coloredSource, { x, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
        }
      }
      y += 16;
    }

    // === ACTION BUTTONS ===
    y += 2;

    // View on objkt
    const objktScheme = pal.btnObjkt;
    const objktSize = mc8ButtonSize("objkt");
    const rebakeSize = mc8ButtonSize("Rebake");
    // Button text changes based on sync state - also check if analyzer is outdated
    const analyzerOutdated = onChainAnalyzerVersion && onChainAnalyzerVersion !== "2.0.0";
    const hasPendingSync = !isSynced && !isLegacySynced || rebakeResult || analyzerOutdated;
    const syncBtnText = hasPendingSync ? "Resync" : "Synced";
    const updateSize = mc8ButtonSize(syncBtnText);

    // Calculate button layout - always show all 3 buttons
    const totalWidth = objktSize.w + 4 + rebakeSize.w + 4 + updateSize.w;
    let btnX = floor((w - totalWidth) / 2);

    // objkt button
    btn.btn.box.x = btnX;
    btn.btn.box.y = y;
    btn.btn.box.w = objktSize.w;
    btn.btn.box.h = objktSize.h;
    paintMC8Btn(btnX, y, "objkt", { ink, line: ink }, objktScheme, btn.btn.down || btn.btn.over);
    btnX += objktSize.w + 4;

    // Rebake button
    const rebakeScheme = pal.btnRebake;
    rebakeBtn.btn.box.x = btnX;
    rebakeBtn.btn.box.y = y;
    rebakeBtn.btn.box.w = rebakeSize.w;
    rebakeBtn.btn.box.h = rebakeSize.h;
    paintMC8Btn(btnX, y, rebaking ? "..." : "Rebake", { ink, line: ink }, rebaking ? { normal: rebakeScheme.disabled, hover: rebakeScheme.disabled, disabled: rebakeScheme.disabled } : rebakeScheme, rebakeBtn.btn.down || rebakeBtn.btn.over);
    btnX += rebakeSize.w + 4;

    // Sync button - always visible, grayed out when synced
    const canSync = hasPending || rebakeResult || !isSynced && !isLegacySynced || analyzerOutdated;
    const syncScheme = pal.btnSync;
    const syncDisabled = updatingChain || (isSynced || isLegacySynced) && !rebakeResult && !analyzerOutdated;
    updateChainBtn.btn.box.x = btnX;
    updateChainBtn.btn.box.y = y;
    updateChainBtn.btn.box.w = updateSize.w;
    updateChainBtn.btn.box.h = updateSize.h;
    paintMC8Btn(btnX, y, updatingChain ? "..." : syncBtnText, { ink, line: ink }, syncDisabled ? { normal: syncScheme.disabled, hover: syncScheme.disabled, disabled: syncScheme.disabled } : syncScheme, updateChainBtn.btn.down || updateChainBtn.btn.over);
    y += objktSize.h + 4;

    // Wallet shortcut - bottom left corner
    const walletScheme = pal.btnWallet;
    const walletSize = mc8ButtonSize("wallet");
    walletBtn.btn.box.x = margin;
    walletBtn.btn.box.y = h - walletSize.h - 4;
    walletBtn.btn.box.w = walletSize.w;
    walletBtn.btn.box.h = walletSize.h;
    paintMC8Btn(margin, h - walletSize.h - 4, "wallet", { ink, line: ink }, walletScheme, walletBtn.btn.down || walletBtn.btn.over);

    // Contract button - bottom right corner
    const contractScheme = pal.btnContract;
    const shortContract = KEEPS_CONTRACT.slice(0, 8) + "..";
    const contractSize = mc8ButtonSize(shortContract);
    contractBtn.btn.box.x = w - margin - contractSize.w;
    contractBtn.btn.box.y = h - contractSize.h - 4;
    contractBtn.btn.box.w = contractSize.w;
    contractBtn.btn.box.h = contractSize.h;
    paintMC8Btn(w - margin - contractSize.w, h - contractSize.h - 4, shortContract, { ink, line: ink }, contractScheme, contractBtn.btn.down || contractBtn.btn.over);

    // Show streaming progress
    if (rebaking && rebakeProgress) {
      ink(200, 180, 120).write(rebakeProgress, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 10;
    }
    if (updatingChain && updateChainProgress) {
      ink(150, 150, 200).write(updateChainProgress, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 10;
    }

    // Success messages with transaction link
    if (updateChainResult) {
      ink(100, 255, 150).write("[ok] Synced!", { x: margin, y }, undefined, undefined, false, "MatrixChunky8");

      // TX button to view transaction on TzKT
      if (updateChainResult.opHash) {
        const txScheme = pal.btnTx;
        const shortHash = updateChainResult.opHash.slice(0, 8) + "..";
        const txSize = mc8ButtonSize(shortHash);
        const txX = margin + 52;
        txBtn.btn.box.x = txX;
        txBtn.btn.box.y = y;
        txBtn.btn.box.w = txSize.w;
        txBtn.btn.box.h = txSize.h;
        paintMC8Btn(txX, y, shortHash, { ink, line: ink }, txScheme, txBtn.btn.down || txBtn.btn.over);
      }
      y += 14;
    }

    return;
  }

  // === LOADING VIEW ===
  if (loadingExisting || loadingPieceInfo) {
    wipe(pal.bg[0], pal.bg[1], pal.bg[2]);
    const msg = loadingPieceInfo ? "Loading piece..." : "Checking chain...";
    ink(100, 150, 180).write(msg, { x: w/2, y: h/2, center: "xy" }, undefined, undefined, false, "MatrixChunky8");
    return;
  }

  // === CONFIRMATION VIEW ===
  if (waitingConfirmation) {
    // Dark gradient background
    const pulse = sin(rotation * 1.2) * 3;
    wipe(pal.bg[0] + pulse, pal.bg[1] + pulse, pal.bg[2] + pulse);

    // Subtle animated corner accents
    const accentAlpha = floor(60 + sin(rotation * 2) * 20);
    ink(255, 200, 100, accentAlpha).box(0, 0, 20, 2);
    ink(255, 200, 100, accentAlpha).box(0, 0, 2, 20);
    ink(255, 200, 100, accentAlpha).box(w - 20, 0, 20, 2);
    ink(255, 200, 100, accentAlpha).box(w - 2, 0, 2, 20);
    ink(255, 200, 100, accentAlpha).box(0, h - 2, 20, 2);
    ink(255, 200, 100, accentAlpha).box(0, h - 20, 2, 20);
    ink(255, 200, 100, accentAlpha).box(w - 20, h - 2, 20, 2);
    ink(255, 200, 100, accentAlpha).box(w - 2, h - 20, 2, 20);

    // Check if anonymous (no author recorded and not loading)
    const isAnonymous = !pieceAuthorSub && !loadingPieceInfo;

    // Responsive spacing based on screen size
    const tiny = h < 140 || w < 140;
    const compact = h < 200 || w < 200;
    const spacious = h > 280 && w > 280;
    const gap = tiny ? 4 : (compact ? 6 : (spacious ? 16 : 10));
    const smallGap = tiny ? 2 : (compact ? 4 : (spacious ? 10 : 6));
    const tinyGap = tiny ? 2 : (compact ? 3 : (spacious ? 6 : 4));

    const hasPreview = piecePreviewBitmap || piecePreviewLoading;
    
    // Calculate estimated content height for vertical layout
    const basePreviewSize = tiny ? 32 : (compact ? 40 : (spacious ? 56 : 48));
    let estimatedH = 12 + tinyGap; // title + gap (tight)
    if (hasPreview) estimatedH += basePreviewSize + tinyGap;
    estimatedH += 14 + tinyGap; // piece name button
    estimatedH += 10 + tinyGap; // source preview
    estimatedH += 10 + tinyGap; // author info
    estimatedH += 10 + tinyGap + 24; // ownership + button (minimum)
    
    // Calculate how much overflow we have
    const verticalMargin = 8;
    const availableH = h - verticalMargin;
    const contentWouldOverflow = estimatedH > availableH;
    const overflowAmount = estimatedH - availableH;
    
    // Use horizontal layout if:
    // 1. Wide screens always (>300px) with preview
    // 2. Moderate screens (>180px) when content would overflow by more than 20px
    // 3. Small screens (>140px) when severely overflowing (>40px)
    const useHorizontalLayout = hasPreview && (
      (w > 300) ||
      (w > 180 && overflowAmount > 20) ||
      (w > 140 && overflowAmount > 40)
    );
    
    // Adjust what to show based on available space - be more aggressive about hiding
    const severeSpace = h < 100 || overflowAmount > 30;
    const tightSpace = h < 130 || overflowAmount > 15;
    const showSourcePreview = pieceSourceDisplay && !tiny && !severeSpace;
    const showInfo = !tiny && !severeSpace;
    const showDate = !compact && !tightSpace;
    const showProse = !compact && !tightSpace;
    const showNetwork = !tiny && !severeSpace;
    const showContract = KEEPS_STAGING ? true : (!tiny && !tightSpace); // Always show contract in staging mode
    
    // Preview thumbnail size - responsive to layout mode and space constraints
    const previewThumbSize = useHorizontalLayout 
      ? min(72, max(40, floor(h * 0.45))) // Horizontal: fit to height
      : (tiny ? 32 : (compact ? 40 : (spacious ? 56 : 48))); // Vertical: based on screen size, smaller than before

    // === HORIZONTAL LAYOUT (side-by-side) ===
    if (useHorizontalLayout) {
      const previewAreaW = previewThumbSize + 16; // preview + padding
      const contentAreaX = previewAreaW;
      const contentAreaW = w - previewAreaW - 8;
      
      // Draw preview on left side (vertically centered)
      const previewX = 8;
      const previewY = floor((h - previewThumbSize) / 2);
      const borderGlow = floor(30 + sin(rotation * 2) * 15);
      ink(255, 200, 100, borderGlow).box(previewX - 2, previewY - 2, previewThumbSize + 4, previewThumbSize + 4);
      
      if (piecePreviewBitmap) {
        const scale = previewThumbSize / (piecePreviewBitmap.width || 100);
        paste(piecePreviewBitmap, previewX, previewY, { scale });
      } else if (piecePreviewLoading) {
        ink(40, 50, 60).box(previewX, previewY, previewThumbSize, previewThumbSize);
        const dots = ".".repeat((floor(rotation * 3) % 3) + 1);
        ink(100, 120, 140).write(dots, { x: previewX + previewThumbSize / 2, y: previewY + previewThumbSize / 2 - 4, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      }
      
      // Content on right side - calculate vertical centering
      const cx = contentAreaX + contentAreaW / 2;
      let contentH = 12 + tinyGap + 14 + tinyGap; // title + piece button
      if (showSourcePreview) contentH += 10 + tinyGap;
      if (showInfo) contentH += 10 + smallGap;
      contentH += 10 + tinyGap + 26; // ownership + button (minimum)
      
      let cy = max(6, floor((h - contentH) / 2));
      
      // Title
      const glowAlpha = floor(50 + sin(rotation * 2.5) * 30);
      if (isAnonymous) {
        ink(200, 120, 100, glowAlpha).write("ANONYMOUS", { x: cx, y: cy - 1, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        ink(200, 120, 100).write("ANONYMOUS", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      } else {
        ink(255, 220, 100, glowAlpha).write("KEEP", { x: cx, y: cy - 1, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        ink(255, 220, 100).write("KEEP", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      }
      cy += 12 + tinyGap;
      
      // Piece name button
      const btnPulsePreview = 1 + sin(rotation * 2) * 0.08;
      const base = pal.btnPreview.normalBase;
      const previewScheme = {
        normal: {
          bg: [floor(base.bg[0] * btnPulsePreview), floor(base.bg[1] * btnPulsePreview), floor(base.bg[2] * btnPulsePreview)],
          outline: [floor(base.outline[0] * btnPulsePreview), floor(base.outline[1] * btnPulsePreview), floor(base.outline[2] * btnPulsePreview)],
          outlineAlpha: base.outlineAlpha,
          text: base.text
        },
        hover: pal.btnPreview.hover,
        disabled: pal.btnPreview.disabled
      };
      const previewText = `$${piece}`;
      const previewSize = mc8ButtonSize(previewText);
      const btnX = floor(cx - previewSize.w / 2);
      previewBtn.btn.box.x = btnX;
      previewBtn.btn.box.y = cy;
      previewBtn.btn.box.w = previewSize.w;
      previewBtn.btn.box.h = previewSize.h;
      paintMC8Btn(btnX, cy, previewText, { ink, line: ink }, previewScheme, previewBtn.btn.down || previewBtn.btn.over);
      cy += 14 + tinyGap;
      
      // Source preview (scrolling) - only if space
      if (showSourcePreview && cy + 40 < h) {
        const maxSourceLen = floor(contentAreaW / 4) - 2;
        let displaySource = pieceSourceDisplay;
        if (displaySource.length > maxSourceLen) {
          const scrollOffset = floor(rotation * 3) % (displaySource.length + 10);
          const paddedSource = displaySource + "          " + displaySource;
          displaySource = paddedSource.substring(scrollOffset, scrollOffset + maxSourceLen);
        }
        const coloredSource = buildColoredSourceString(displaySource);
        ink(200, 200, 200, 140).write(coloredSource, { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + tinyGap;
      }
      
      // Author info - compact version
      if (showInfo && cy + 30 < h) {
        const authorStr = pieceAuthor || "anon";
        const hitsStr = pieceHits !== null ? `${pieceHits}▶` : "";
        const infoStr = hitsStr ? `${authorStr} · ${hitsStr}` : authorStr;
        ink(100, 105, 115).write(infoStr, { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + smallGap;
      }
      
      // Action area for horizontal layout - condensed
      if (isAuthor === true) {
        ink(100, 200, 130).write("✓ Your code", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + tinyGap;
        
        // Keep button
        const btnPulse = sin(rotation * 2.5) * 0.15 + 1;
        const confirmBase = pal.btnConfirm.normalBase;
        const confirmScheme = {
          normal: { bg: [floor(confirmBase.bg[0] * btnPulse), floor(confirmBase.bg[1] * btnPulse), floor(confirmBase.bg[2] * btnPulse)], outline: [floor(confirmBase.outline[0] * btnPulse), floor(confirmBase.outline[1] * btnPulse), floor(confirmBase.outline[2] * btnPulse)], outlineAlpha: confirmBase.outlineAlpha, text: confirmBase.text },
          hover: pal.btnConfirm.hover,
          disabled: pal.btnConfirm.disabled
        };
        const confirmText = "Keep";
        const confirmSize = lgButtonSize(confirmText);
        const confirmX = floor(cx - confirmSize.w / 2);
        btn.btn.box.x = confirmX;
        btn.btn.box.y = cy;
        btn.btn.box.w = confirmSize.w;
        btn.btn.box.h = confirmSize.h;
        paintLgBtn(confirmX, cy, confirmText, { ink, line: ink }, confirmScheme, btn.btn.down || btn.btn.over);
        cy += confirmSize.h + tinyGap;

        // Contract button showing version and address (clickable)
        if (cy + 12 < h && KEEPS_STAGING) {
          const contractShort = KEEPS_CONTRACT.slice(0, 10) + "..";
          const contractLabel = `v4: ${contractShort}`;
          const contractScheme = pal.btnContract;
          const contractSize = mc8ButtonSize(contractLabel);
          const contractX = floor(cx - contractSize.w / 2);
          contractBtn.btn.box.x = contractX;
          contractBtn.btn.box.y = cy;
          contractBtn.btn.box.w = contractSize.w;
          contractBtn.btn.box.h = contractSize.h;
          paintMC8Btn(contractX, cy, contractLabel, { ink, line: ink }, contractScheme, contractBtn.btn.down || contractBtn.btn.over);
        } else if (cy + 12 < h) {
          // Non-staging: just show network name
          const netLabel = NETWORK.toUpperCase();
          const netColor = [100, 220, 100];
          ink(netColor[0], netColor[1], netColor[2], 180).write(netLabel, { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
          contractBtn.btn.box.w = 0;
        } else {
          contractBtn.btn.box.w = 0;
        }
      } else if (pieceAuthorSub && !userSub) {
        ink(210, 160, 110).write("Login to keep", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + tinyGap;
        const loginPulse = sin(rotation * 2) * 0.1 + 1;
        const loginBase = pal.btnLogin.normalBase;
        const loginScheme = {
          normal: { bg: [floor(loginBase.bg[0] * loginPulse), floor(loginBase.bg[1] * loginPulse), floor(loginBase.bg[2] * loginPulse)], outline: [floor(loginBase.outline[0] * loginPulse), floor(loginBase.outline[1] * loginPulse), floor(loginBase.outline[2] * loginPulse)], outlineAlpha: loginBase.outlineAlpha, text: loginBase.text },
          hover: pal.btnLogin.hover,
          disabled: pal.btnLogin.disabled
        };
        const loginText = "Login";
        const loginSize = lgButtonSize(loginText);
        const loginX = floor(cx - loginSize.w / 2);
        loginBtn.btn.box.x = loginX;
        loginBtn.btn.box.y = cy;
        loginBtn.btn.box.w = loginSize.w;
        loginBtn.btn.box.h = loginSize.h;
        paintLgBtn(loginX, cy, loginText, { ink, line: ink }, loginScheme, loginBtn.btn.down || loginBtn.btn.over);
      } else if (isAuthor === false) {
        ink(210, 110, 110).write("✗ Not yours", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + tinyGap;
        ink(130, 130, 145).write(pieceAuthor || "unknown", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      } else if (isAnonymous) {
        ink(210, 130, 110).write("Anonymous", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + tinyGap;
        ink(150, 140, 130).write("Can't keep", { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      } else {
        const dots = ".".repeat((floor(rotation * 3) % 3) + 1);
        ink(180, 175, 150).write(`Loading${dots}`, { x: cx, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      }
      return;
    }

    // === VERTICAL (NARROW) LAYOUT ===
    
    // Use tighter spacing when space is constrained
    const vGap = tightSpace ? tinyGap : smallGap;
    const vGapLarge = severeSpace ? tinyGap : (tightSpace ? smallGap : gap);
    
    // Recalculate total height with actual values
    let totalH = 12 + vGap; // title + gap
    if (hasPreview) totalH += previewThumbSize + vGap;
    totalH += 14 + vGap; // piece name button + gap
    if (showSourcePreview) totalH += 10 + tinyGap;
    if (showInfo) totalH += 10 + vGapLarge;
    if (isAnonymous) {
      totalH += 14 + tinyGap + 9;
    } else if (isAuthor === true) {
      totalH += 10 + vGap; // ownership
      if (showProse) totalH += 10 + vGap;
      totalH += 24; // keep button (slightly smaller)
      if (showNetwork) totalH += 10 + vGap;
      if (showContract) totalH += 12 + vGap;
    } else if (pieceAuthorSub && !userSub) {
      totalH += 10 + vGap + 24;
    } else if (isAuthor === false) {
      totalH += 10 + vGap + 12 + tinyGap + 10;
    } else {
      totalH += 10 + vGapLarge + 16;
    }

    let cy = floor((h - totalH) / 2);
    if (cy < 2) cy = 2; // Allow very tight top margin

    // Title with glow effect - show ANONYMOUS in red/orange if anonymous piece
    const glowAlpha = floor(50 + sin(rotation * 2.5) * 30);
    if (isAnonymous) {
      ink(200, 120, 100, glowAlpha).write("ANONYMOUS", { x: w/2, y: cy - 1, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      ink(200, 120, 100).write("ANONYMOUS", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    } else {
      ink(255, 220, 100, glowAlpha).write("KEEP", { x: w/2, y: cy - 1, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      ink(255, 220, 100).write("KEEP", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    }
    cy += 12 + vGap;
    
    // Preview thumbnail from oven (animated webp) or loading placeholder
    if (piecePreviewBitmap) {
      const thumbX = floor((w - previewThumbSize) / 2);
      const scale = previewThumbSize / (piecePreviewBitmap.width || 100);
      // Draw a subtle border/glow around thumbnail
      const borderGlow = floor(30 + sin(rotation * 2) * 15);
      ink(255, 200, 100, borderGlow).box(thumbX - 2, cy - 2, previewThumbSize + 4, previewThumbSize + 4);
      paste(piecePreviewBitmap, thumbX, cy, { scale });
      cy += previewThumbSize + vGap;
    } else if (piecePreviewLoading) {
      // Loading placeholder box with animated dots
      const thumbX = floor((w - previewThumbSize) / 2);
      const borderGlow = floor(30 + sin(rotation * 2) * 15);
      ink(255, 200, 100, borderGlow).box(thumbX - 2, cy - 2, previewThumbSize + 4, previewThumbSize + 4);
      ink(40, 50, 60).box(thumbX, cy, previewThumbSize, previewThumbSize);
      const dots = ".".repeat((floor(rotation * 3) % 3) + 1);
      ink(100, 120, 140).write(dots, { x: w/2, y: cy + previewThumbSize / 2 - 4, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += previewThumbSize + vGap;
    }

    // Piece name as clickable button to preview - with subtle pulse
    const btnPulsePreview = 1 + sin(rotation * 2) * 0.08;
    const base = pal.btnPreview.normalBase;
    const previewScheme = {
      normal: {
        bg: [floor(base.bg[0] * btnPulsePreview), floor(base.bg[1] * btnPulsePreview), floor(base.bg[2] * btnPulsePreview)],
        outline: [floor(base.outline[0] * btnPulsePreview), floor(base.outline[1] * btnPulsePreview), floor(base.outline[2] * btnPulsePreview)],
        outlineAlpha: base.outlineAlpha,
        text: base.text
      },
      hover: pal.btnPreview.hover,
      disabled: pal.btnPreview.disabled
    };
    const previewText = `$${piece}`;
    const previewSize = mc8ButtonSize(previewText);
    const previewX = floor((w - previewSize.w) / 2);
    previewBtn.btn.box.x = previewX;
    previewBtn.btn.box.y = cy;
    previewBtn.btn.box.w = previewSize.w;
    previewBtn.btn.box.h = previewSize.h;
    paintMC8Btn(previewX, cy, previewText, { ink, line: ink }, previewScheme, previewBtn.btn.down || previewBtn.btn.over);
    cy += 14 + vGap;

    // Source code preview with syntax highlighting (scrolling ticker if long)
    if (showSourcePreview) {
      const maxSourceLen = floor(w / 6) - 4; // Responsive to screen width
      let displaySource = pieceSourceDisplay;
      if (displaySource.length > maxSourceLen) {
        // Scroll the source
        const scrollOffset = floor(rotation * 3) % (displaySource.length + 10);
        const paddedSource = displaySource + "          " + displaySource;
        displaySource = paddedSource.substring(scrollOffset, scrollOffset + maxSourceLen);
      }

      // Use proper KidLisp syntax highlighting via buildColoredSourceString
      const coloredSource = buildColoredSourceString(displaySource);
      ink(200, 200, 200, 140).write(coloredSource, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += 10 + tinyGap;
    }

    // Author, hits, and date info
    const authorStr = pieceAuthor || "anonymous";
    const hitsStr = pieceHits !== null ? `${pieceHits} hits` : "";
    // Format date as "Dec 26, 2025" or similar
    let dateStr = "";
    if (pieceCreatedAt) {
      const d = pieceCreatedAt instanceof Date ? pieceCreatedAt : new Date(pieceCreatedAt);
      if (!isNaN(d.getTime())) {
        const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
        dateStr = `${months[d.getMonth()]} ${d.getDate()}, ${d.getFullYear()}`;
      }
    }
    // Build info string with all available parts
    let infoParts = [`by ${authorStr}`];
    if (dateStr && showDate) infoParts.push(dateStr);
    if (hitsStr) infoParts.push(hitsStr);
    const infoStr = infoParts.join(" · ");
    if (showInfo) {
      ink(100, 105, 115).write(infoStr, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += 10 + vGapLarge;
    }

    // Ownership status and action area
    if (isAuthor === true) {
      // User IS the author - show keep button
      const checkmark = "✓";
      const ownershipMsg = userHandle ? `${checkmark} Logged in as @${userHandle}` : `${checkmark} You wrote this code`;
      ink(100, 200, 130).write(ownershipMsg, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += 10 + vGap;

      if (showProse) {
        // "Keep It?" prompt in yellow with glow - only when we have space
        const keepItGlow = floor(30 + sin(rotation * 3) * 15);
        ink(255, 220, 100, keepItGlow).write("Keep It?", { x: w/2, y: cy - 1, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        ink(255, 220, 100).write("Keep It?", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 12 + vGap;
      }

      // Main action button (prominent, glowing)
      const btnPulse = sin(rotation * 2.5) * 0.15 + 1;
      const confirmBase = pal.btnConfirm.normalBase;
      const confirmScheme = {
        normal: { bg: [floor(confirmBase.bg[0] * btnPulse), floor(confirmBase.bg[1] * btnPulse), floor(confirmBase.bg[2] * btnPulse)], outline: [floor(confirmBase.outline[0] * btnPulse), floor(confirmBase.outline[1] * btnPulse), floor(confirmBase.outline[2] * btnPulse)], outlineAlpha: confirmBase.outlineAlpha, text: confirmBase.text },
        hover: pal.btnConfirm.hover,
        disabled: pal.btnConfirm.disabled
      };
      const confirmText = tightSpace ? "Keep" : "Keep It";
      const confirmSize = lgButtonSize(confirmText);
      const confirmX = floor((w - confirmSize.w) / 2);
      btn.btn.box.x = confirmX;
      btn.btn.box.y = cy;
      btn.btn.box.w = confirmSize.w;
      btn.btn.box.h = confirmSize.h;
      paintLgBtn(confirmX, cy, confirmText, { ink, line: ink }, confirmScheme, btn.btn.down);
      cy += confirmSize.h + vGap;

      // Network badge
      if (showNetwork) {
        const netLabel = KEEPS_STAGING ? "STAGING V4" : NETWORK.toUpperCase();
        const isMainnet = NETWORK === "mainnet";
        const netColor = KEEPS_STAGING ? [255, 180, 100] : (isMainnet ? [100, 220, 100] : [220, 180, 100]);
        ink(netColor[0], netColor[1], netColor[2], 200).write(netLabel, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + vGap;
      }

      // Contract button (clickable link to TzKT)
      if (showContract) {
        const contractScheme = pal.btnContract;
        const contractShort = KEEPS_CONTRACT.slice(0, 10) + "..";
        const contractLabel = KEEPS_STAGING ? `v4: ${contractShort}` : contractShort;
        const contractSize = mc8ButtonSize(contractLabel);
        const contractX = floor((w - contractSize.w) / 2);
        contractBtn.btn.box.x = contractX;
        contractBtn.btn.box.y = cy;
        contractBtn.btn.box.w = contractSize.w;
        contractBtn.btn.box.h = contractSize.h;
        paintMC8Btn(contractX, cy, contractLabel, { ink, line: ink }, contractScheme, contractBtn.btn.down || contractBtn.btn.over);
        cy += 12 + vGapLarge;
      } else {
        contractBtn.btn.box.x = 0;
        contractBtn.btn.box.y = 0;
        contractBtn.btn.box.w = 0;
        contractBtn.btn.box.h = 0;
      }

    } else if (pieceAuthorSub && !userSub) {
      // Piece has an author, but user is NOT logged in yet
      if (tightSpace) {
        ink(210, 160, 110).write("Login to keep", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += vGapLarge;
      } else {
        ink(210, 160, 110).write("You can only keep KidLisp", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 9 + tinyGap;
        ink(210, 160, 110).write("that you authored.", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += vGapLarge;

        ink(150, 150, 165).write("Login to verify ownership", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + vGap;
      }

      // Login button with pulse
      const loginPulse = sin(rotation * 2) * 0.1 + 1;
      const loginBase = pal.btnLogin.normalBase;
      const loginScheme = {
        normal: { bg: [floor(loginBase.bg[0] * loginPulse), floor(loginBase.bg[1] * loginPulse), floor(loginBase.bg[2] * loginPulse)], outline: [floor(loginBase.outline[0] * loginPulse), floor(loginBase.outline[1] * loginPulse), floor(loginBase.outline[2] * loginPulse)], outlineAlpha: loginBase.outlineAlpha, text: loginBase.text },
        hover: pal.btnLogin.hover,
        disabled: pal.btnLogin.disabled
      };
      const loginText = "Login";
      const loginSize = lgButtonSize(loginText);
      const loginX = floor((w - loginSize.w) / 2);
      loginBtn.btn.box.x = loginX;
      loginBtn.btn.box.y = cy;
      loginBtn.btn.box.w = loginSize.w;
      loginBtn.btn.box.h = loginSize.h;
      paintLgBtn(loginX, cy, loginText, { ink, line: ink }, loginScheme, loginBtn.btn.down);

    } else if (isAuthor === false) {
      // User IS logged in but NOT the author
      const loggedInMsg = userHandle ? `Logged in as @${userHandle}` : "Logged in";
      ink(110, 150, 195).write(loggedInMsg, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += 10 + vGap;

      ink(210, 110, 110).write("✗ Not your code", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += 12 + tinyGap;

      if (tightSpace) {
        ink(180, 155, 145).write("Only the author can keep", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + vGap;
      } else {
        ink(180, 155, 145).write("You can only keep KidLisp", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 9 + tinyGap;
        ink(180, 155, 145).write("that you authored.", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 10 + vGap;
      }

      ink(130, 130, 145).write(`Author: ${pieceAuthor || "unknown"}`, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");

    } else if (isAnonymous) {
      // Anonymous piece - can't be kept
      ink(210, 130, 110).write("Anonymous piece", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      cy += 12 + tinyGap;

      // Explanation - condensed for tight space
      if (!severeSpace) {
        ink(150, 140, 130).write("Anonymous pieces cannot", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
        cy += 9 + tinyGap;
        ink(150, 140, 130).write("be kept at this time.", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      } else {
        ink(150, 140, 130).write("Cannot be kept", { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      }
    } else {
      // Still loading - show animated dots
      const dots = ".".repeat((floor(rotation * 3) % 3) + 1);
      ink(180, 175, 150).write(`Loading${dots}`, { x: w/2, y: cy, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    }

    return;
  }

  // === MINTING VIEW (original) ===

  // Animated background color based on progress
  const progress = getProgress();
  const isComplete = timeline.find(t => t.id === "complete")?.status === "done";
  const isError = hasError();

  // Animated background based on progress and theme
  let bgR, bgG, bgB;
  if (isError) {
    // Error: reddish tint
    bgR = $.dark ? 45 : 240; bgG = $.dark ? 20 : 220; bgB = $.dark ? 25 : 225;
  } else if (isComplete) {
    // Complete: greenish shimmer
    const pulse = sin(rotation * 2) * 8;
    bgR = $.dark ? (20 + pulse) : (220 + pulse);
    bgG = $.dark ? (45 + pulse) : (240 + pulse);
    bgB = $.dark ? (30 + pulse) : (225 + pulse);
  } else {
    // Progress gradient - blend from pal.bg toward accent
    const base = pal.bg;
    if (progress < 0.5) {
      const t = progress * 2;
      bgR = floor(base[0] + t * ($.dark ? 25 : -20));
      bgG = floor(base[1] + t * ($.dark ? 20 : -15));
      bgB = floor(base[2] + t * ($.dark ? -5 : -10));
    } else {
      const t = (progress - 0.5) * 2;
      bgR = floor(($.dark ? 60 : 225) - t * ($.dark ? 35 : 10));
      bgG = floor(($.dark ? 45 : 228) + t * ($.dark ? 10 : 5));
      bgB = floor(($.dark ? 25 : 228) + t * ($.dark ? 10 : 5));
    }
  }
  wipe(bgR, bgG, bgB);

  // Draw particles
  for (const p of particles) {
    const alpha = floor((p.life / 100) * 180);
    ink(p.color[0], p.color[1], p.color[2], alpha).box(floor(p.x), floor(p.y), floor(p.size), floor(p.size));
  }

  // Border shimmer
  if (progress > 0.3 || isComplete) {
    const shimmerIntensity = isComplete ? 1 : (progress - 0.3) / 0.7;
    for (let i = 0; i < w; i += 4) {
      const shimmer = sin(rotation * 3 + i * 0.15) * 0.5 + 0.5;
      const alpha = floor(shimmer * 50 * shimmerIntensity);
      const color = isComplete ? pal.positive : pal.gold;
      ink(color[0], color[1], color[2], alpha).box(i, 0, 2, 1);
      ink(color[0], color[1], color[2], alpha).box(i, h - 1, 2, 1);
    }
  }

  const margin = 6;
  const HUD_RESERVED = 21; // Space reserved for HUD label (matches notepat.mjs TOP_BAR_BOTTOM)
  let y = HUD_RESERVED;

  // === Current Task & Progress Bar (below HUD) ===
  const activeStep = getActiveStep();
  const doneCount = timeline.filter(t => t.status === "done").length;
  const totalSteps = timeline.length;

  // Progress bar - color-coded by phase segments
  const barY = y;
  const barH = 4;
  ink(pal.bgProgress[0], pal.bgProgress[1], pal.bgProgress[2]).box(0, barY, w, barH);

  // Draw each segment with its phase color
  for (let i = 0; i < totalSteps; i++) {
    const step = timeline[i];
    const phase = PHASE_COLORS[step.id] || PHASE_COLORS.validate;
    const segmentStart = floor(w * (i / totalSteps));
    const segmentEnd = floor(w * ((i + 1) / totalSteps));
    const segmentW = segmentEnd - segmentStart;

    if (step.status === "done") {
      // Done: use phase label color at full brightness
      ink(phase.label[0], phase.label[1], phase.label[2], 200).box(segmentStart, barY, segmentW, barH);
    } else if (step.status === "active") {
      // Active: pulsing phase color
      const pulse = sin(rotation * 4) * 0.3 + 0.7;
      ink(phase.label[0], phase.label[1], phase.label[2], floor(200 * pulse)).box(segmentStart, barY, segmentW, barH);
    } else if (step.status === "error") {
      // Error: red segment
      ink(255, 100, 100, 200).box(segmentStart, barY, segmentW, barH);
    }
    // Segment divider
    ink(pal.divider[0], pal.divider[1], pal.divider[2]).box(segmentEnd - 1, barY, 1, barH);
  }
  y += barH + 4;

  // === STEP CAROUSEL (prev | CURRENT | next) ===
  const carouselH = 20;
  const carouselY = y;
  ink(pal.bgCarousel[0], pal.bgCarousel[1], pal.bgCarousel[2]).box(0, carouselY, w, carouselH); // Dark bg for carousel

  // Calculate which steps to show based on animated position
  const stepSpacing = 100; // Virtual spacing between steps
  const centerX = w / 2;

  // Draw each step label, positioned based on carousel animation
  for (let i = 0; i < totalSteps; i++) {
    const step = timeline[i];
    const phase = PHASE_COLORS[step.id] || PHASE_COLORS.validate;

    // Calculate X position relative to carousel scroll
    const stepVirtualX = i * stepSpacing;
    const offsetFromCurrent = stepVirtualX - carouselCurrentX;
    const screenX = centerX + offsetFromCurrent * 0.8; // Scale down for tighter spacing

    // Skip if way off screen
    if (screenX < -80 || screenX > w + 80) continue;

    // Calculate opacity based on distance from center
    const distFromCenter = abs(offsetFromCurrent);
    const opacity = max(0, 1 - distFromCenter / 150);

    // Determine color and style
    let labelColor, labelAlpha;
    if (step.status === "active") {
      labelColor = phase.label;
      labelAlpha = 255;
    } else if (step.status === "done") {
      labelColor = phase.label.map(c => floor(c * 0.6));
      labelAlpha = floor(180 * opacity);
    } else if (step.status === "error") {
      labelColor = pal.error;
      labelAlpha = floor(200 * opacity);
    } else {
      labelColor = pal.textMuted;
      labelAlpha = floor(120 * opacity);
    }

    // Draw step label
    const label = step.status === "active" ? step.label.toUpperCase() : step.label;
    const labelW = label.length * (step.status === "active" ? 6 : 4);
    const textX = screenX - labelW / 2;

    if (step.status === "active") {
      // Active step: larger, brighter, with glow
      const pulse = sin(rotation * 3) * 0.15 + 0.85;
      ink(labelColor[0], labelColor[1], labelColor[2], floor(255 * pulse)).write(label, { x: textX, y: carouselY + 5 });
    } else {
      // Other steps: smaller MatrixChunky8 font
      ink(labelColor[0], labelColor[1], labelColor[2], labelAlpha).write(label, { x: textX, y: carouselY + 6 }, undefined, undefined, false, "MatrixChunky8");
    }
  }

  // Arrows indicating more steps
  if (carouselTargetIndex > 0) {
    ink(pal.arrow[0], pal.arrow[1], pal.arrow[2], 150).write("<", { x: 4, y: carouselY + 6 }, undefined, undefined, false, "MatrixChunky8");
  }
  if (carouselTargetIndex < totalSteps - 1) {
    ink(pal.arrow[0], pal.arrow[1], pal.arrow[2], 150).write(">", { x: w - 8, y: carouselY + 6 }, undefined, undefined, false, "MatrixChunky8");
  }

  y += carouselH + 2;

  // Step counter (small, centered)
  const stepText = `${doneCount}/${totalSteps}`;
  ink(pal.arrow[0], pal.arrow[1], pal.arrow[2]).write(stepText, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");

  // Cancel button during preparation
  if (isPreparationInProgress()) {
    const cancelScheme = pal.btnCancel;
    const cancelSize = mc8ButtonSize("Cancel");
    const cancelX = w - margin - cancelSize.w;
    const cancelY = y - 2;
    cancelBtn.btn.box.x = cancelX;
    cancelBtn.btn.box.y = cancelY;
    cancelBtn.btn.box.w = cancelSize.w;
    cancelBtn.btn.box.h = cancelSize.h;
    paintMC8Btn(cancelX, cancelY, "Cancel", { ink, line: ink }, cancelScheme, cancelBtn.btn.down || cancelBtn.btn.over);
  }
  y += 12;

  // Timeline with striped sections
  let shownPending = false;

  for (const item of timeline) {
    const isActive = item.status === "active";
    const isDone = item.status === "done";
    const isItemError = item.status === "error";
    const isPending = item.status === "pending";

    if (isPending) {
      if (shownPending) continue;
      shownPending = true;
    }

    const phase = PHASE_COLORS[item.id] || PHASE_COLORS.validate;
    const stripeH = item.detail ? 20 : 12;

    // Draw stripe background
    if (isDone || isActive || isItemError) {
      let stripeColor;
      if (isItemError) stripeColor = [70, 30, 35];
      else if (isActive) stripeColor = phase.stripe.map(c => c + 15);
      else stripeColor = phase.stripe;
      ink(stripeColor[0], stripeColor[1], stripeColor[2], isDone ? 60 : 100).box(0, y, w, stripeH);

      // Progress bar for active items
      if (isActive && item.startedAt && item.duration) {
        const elapsed = Date.now() - item.startedAt;
        const progress = min(1, elapsed / item.duration);
        const barW = floor((w - 4) * progress);
        // Glowing progress bar
        const glow = sin(rotation * 4) * 20 + 30;
        ink(phase.label[0], phase.label[1], phase.label[2], 40 + glow).box(2, y + stripeH - 3, barW, 2);
      }
    }

    // Label
    let labelColor;
    if (isPending) labelColor = pal.textMuted;
    else if (isItemError) labelColor = [255, 120, 120];
    else if (isActive) labelColor = phase.label;
    else labelColor = phase.label.map(c => floor(c * 0.7));

    ink(...labelColor).write(item.label, { x: margin, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");

    // Time (right aligned)
    if (item.time && (isDone || isActive || isItemError)) {
      const timeColor = isItemError ? [180, 80, 80] : (isActive ? ($.dark ? [200, 200, 140] : [120, 120, 40]) : pal.textDim);
      ink(...timeColor).write(item.time, { x: w - margin - 20, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
    }

    // Detail line (with better contrast) - skip for review step when active (has custom button UI)
    if (item.detail && !isPending && !(item.id === "review" && isActive && preparedData)) {
      let detailColor;
      if (isItemError) detailColor = [255, 120, 120];
      else if (isActive) detailColor = $.dark ? [255, 255, 100] : [140, 140, 20]; // Theme-aware yellow
      else detailColor = pal.text; // Use theme text color for done

      const maxLen = floor((w - 20) / 4);
      const truncated = item.detail.length > maxLen ? item.detail.slice(0, maxLen - 2) + ".." : item.detail;
      // Theme-aware background strip for better readability
      const detailBg = $.dark ? [0, 0, 0, 220] : [50, 50, 40, 220];
      ink(detailBg[0], detailBg[1], detailBg[2], detailBg[3]).box(margin - 2, y + 10, truncated.length * 4 + 4, 10);
      ink(...detailColor).write(truncated, { x: margin, y: y + 11 }, undefined, undefined, false, "MatrixChunky8");
    }

    y += stripeH + 2; // Gap between stripes

    // === INLINE SOURCE MARQUEE (after analyze row) ===
    if (item.id === "analyze" && (isDone || isActive) && sourceCode) {
      const marqueeH = 12;
      const marqueeBg = $.dark ? [35, 40, 50] : [60, 65, 75];
      ink(marqueeBg[0], marqueeBg[1], marqueeBg[2]).box(0, y, w, marqueeH);

      // Build colored string with natural spacing preserved
      const coloredSource = buildColoredSourceString(sourceCode);
      const sourceLen = sourceCode.length * 4; // Approximate width

      // Only scroll if content is wider than screen
      const needsScroll = sourceLen > w - margin * 2;
      const gap = 40;
      const repeatWidth = sourceLen + gap;
      let startX = needsScroll ? (margin - (tickerOffset % repeatWidth)) : margin;

      // Draw colored source using write() for natural spacing
      const repeats = needsScroll ? 2 : 1;
      for (let repeat = 0; repeat < repeats; repeat++) {
        const tx = startX + repeat * repeatWidth;
        if (tx < w && tx + sourceLen > 0) {
          ink(200, 200, 200).write(coloredSource, { x: tx, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
        }
      }

      // Author/stats on right
      if (pieceAuthor || pieceSourceLength) {
        const statsStr = pieceAuthor ? `@${pieceAuthor}` : (pieceSourceLength ? `${pieceSourceLength}c` : "");
        const statsW = statsStr.length * 4;
        const statsBg = $.dark ? [20, 25, 35] : [50, 55, 65];
        ink(statsBg[0], statsBg[1], statsBg[2]).box(w - statsW - margin - 4, y, statsW + 8, marqueeH); // bg to cover ticker
        // Bright teal for @author handles
        const authorColor = pieceAuthor ? [100, 255, 200] : [100, 130, 150];
        ink(authorColor[0], authorColor[1], authorColor[2]).write(statsStr, { x: w - margin - statsW, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
      }

      y += marqueeH + 2;
    }

    // === INLINE THUMBNAIL PREVIEW (only during review step) ===
    if (item.id === "review" && isActive && thumbnailBitmap) {
      const thumbH = 40;
      const thumbSize = min(36, w - 20);
      ink(pal.bgProgress[0], pal.bgProgress[1], pal.bgProgress[2]).box(0, y, w, thumbH);
      const thumbX = margin;
      paste(thumbnailBitmap, thumbX, y + 2, { scale: thumbSize / (thumbnailBitmap.width || 256) });

      // Show cache info or IPFS hash + REBAKE button
      let infoLabel = "";
      const isCached = preparedData?.usedCachedMedia && preparedData.cacheGeneratedAt;
      if (isCached) {
        const date = new Date(preparedData.cacheGeneratedAt);
        infoLabel = `Baked ${date.toLocaleDateString("en-US", { month: "short", day: "numeric" })}`;
      } else if (preparedData?.thumbnailUri) {
        // Show full IPFS hash
        const hash = preparedData.thumbnailUri.replace("ipfs://", "");
        infoLabel = hash;
      }
      const infoX = thumbX + thumbSize + 8;
      if (infoLabel) {
        // Brighter text for better visibility
        ink(140, 220, 180).write(infoLabel, { x: infoX, y: y + 8 }, undefined, undefined, false, "MatrixChunky8");
      }
      // REBAKE button if using cached media
      if (isCached) {
        const rebakeScheme = pal.btnRebake;
        const rebakeSize = mc8ButtonSize("REBAKE");
        const rebakeX = infoX;
        const rebakeY = y + 20;
        rebakeBtn.btn.box.x = rebakeX;
        rebakeBtn.btn.box.y = rebakeY;
        rebakeBtn.btn.box.w = rebakeSize.w;
        rebakeBtn.btn.box.h = rebakeSize.h;
        paintMC8Btn(rebakeX, rebakeY, "REBAKE", { ink, line: ink }, rebakeScheme, rebakeBtn.btn.down || rebakeBtn.btn.over);
      }
      y += thumbH + 2;
    }

    // IPFS links after metadata (inline text buttons)
    if (item.id === "metadata" && isDone && preparedData) {
      let linkX = margin;
      const linkScheme = pal.btnLink;

      if (preparedData.artifactUri) {
        const htmlSize = mc8ButtonSize("HTML");
        htmlBtn.btn.box.x = linkX;
        htmlBtn.btn.box.y = y;
        htmlBtn.btn.box.w = htmlSize.w;
        htmlBtn.btn.box.h = htmlSize.h;
        paintMC8Btn(linkX, y, "HTML", { ink, line: ink }, linkScheme, htmlBtn.btn.down || htmlBtn.btn.over);
        linkX += htmlSize.w + 4;
      }
      if (preparedData.thumbnailUri) {
        const thumbSize = mc8ButtonSize("THUMB");
        thumbBtn.btn.box.x = linkX;
        thumbBtn.btn.box.y = y;
        thumbBtn.btn.box.w = thumbSize.w;
        thumbBtn.btn.box.h = thumbSize.h;
        paintMC8Btn(linkX, y, "THUMB", { ink, line: ink }, linkScheme, thumbBtn.btn.down || thumbBtn.btn.over);
        linkX += thumbSize.w + 4;
      }
      if (preparedData.metadataUri) {
        const metaSize = mc8ButtonSize("META");
        metaBtn.btn.box.x = linkX;
        metaBtn.btn.box.y = y;
        metaBtn.btn.box.w = metaSize.w;
        metaBtn.btn.box.h = metaSize.h;
        paintMC8Btn(linkX, y, "META", { ink, line: ink }, linkScheme, metaBtn.btn.down || metaBtn.btn.over);
      }
      y += mc8ButtonSize("X").h + 4;
    }

    // === INLINE KEEP TOLL BUTTON (in review row) ===
    if (item.id === "review" && isActive && preparedData) {
      // Skip the normal detail rendering for review - button is the UI
      const tollH = 24; // Bigger button
      ink(40, 60, 45).box(0, y, w, tollH); // Darker green-tinted bg

      // Pay toll button centered (mixed font for ꜩ)
      const tollText = `Pay ${preparedData.mintFee ?? 0}`;
      const tezSymbol = "ꜩ";
      const tollSuffix = " Toll";
      const textW = tollText.length * 6; // default font
      const symbolW = 8;
      const suffixW = tollSuffix.length * 6; // default font
      const totalTollW = textW + symbolW + suffixW + 16; // padding
      const tollX = floor((w - totalTollW) / 2);

      // Button background
      const tollScheme = (btn.btn.down || btn.btn.over) ? pal.btnToll.hover : pal.btnToll.normal;
      ink(tollScheme.bg[0], tollScheme.bg[1], tollScheme.bg[2]).box(tollX, y + 3, totalTollW, tollH - 6);
      ink(100, 200, 140).box(tollX, y + 3, totalTollW, 1).box(tollX, y + tollH - 4, totalTollW, 1); // outline

      // Update button hitbox
      btn.btn.box.x = tollX;
      btn.btn.box.y = y + 3;
      btn.btn.box.w = totalTollW;
      btn.btn.box.h = tollH - 6;
      btn.txt = tollText + tezSymbol + tollSuffix;

      // Draw text in default font (bigger): "Pay 5" + ꜩ + " Toll"
      let textX = tollX + 8;
      ink(tollScheme.text[0], tollScheme.text[1], tollScheme.text[2]).write(tollText, { x: textX, y: y + 7 });
      textX += textW + 1;
      ink(tollScheme.text[0], tollScheme.text[1], tollScheme.text[2]).write(tezSymbol, { x: textX, y: y + 4 }, undefined, undefined, false, "unifont");
      textX += symbolW;
      ink(tollScheme.text[0], tollScheme.text[1], tollScheme.text[2]).write(tollSuffix, { x: textX, y: y + 7 });

      y += tollH + 2;

      // "to keep on mainnet staging" with clickable staging button showing contract version and address
      if (KEEPS_STAGING) {
        const netName = preparedData.network || NETWORK || "mainnet";
        const prefix = "to keep on " + netName + " ";
        const contractShort = KEEPS_CONTRACT.slice(0, 10) + "..";
        const stagingLabel = `v4: ${contractShort}`;
        // MatrixChunky8 is ~4px per char
        const prefixW = prefix.length * 4;
        const stagingW = stagingLabel.length * 4 + 8; // padding
        const totalInfoW = prefixW + stagingW;
        const infoX = floor((w - totalInfoW) / 2);
        const infoY = y;

        // Draw prefix text in MatrixChunky8 (theme-aware)
        const prefixColor = $.dark ? [200, 220, 210] : [60, 80, 70];
        ink(prefixColor[0], prefixColor[1], prefixColor[2]).write(prefix, { x: infoX, y: infoY + 2 }, undefined, undefined, false, "MatrixChunky8");

        // Staging button with proper UI styling
        const stagingBtnX = infoX + prefixW;
        btn.staging = btn.staging || { box: { x: 0, y: 0, w: 0, h: 0 }, down: false };
        btn.staging.box = { x: stagingBtnX, y: infoY, w: stagingW, h: 12 };

        const stagingScheme = (btn.staging.down || btn.staging.over) ? pal.btnStaging.hover : pal.btnStaging.normal;

        // Button background
        ink(stagingScheme.bg[0], stagingScheme.bg[1], stagingScheme.bg[2]).box(stagingBtnX, infoY, stagingW, 12);
        // Outline
        ink(stagingScheme.outline[0], stagingScheme.outline[1], stagingScheme.outline[2]).box(stagingBtnX, infoY, stagingW, 1).box(stagingBtnX, infoY + 11, stagingW, 1);
        // Text
        ink(stagingScheme.text[0], stagingScheme.text[1], stagingScheme.text[2]).write(stagingLabel, { x: stagingBtnX + 4, y: infoY + 2 }, undefined, undefined, false, "MatrixChunky8");

        y += 14;
      }
    }
  }

  // === Network badge in TOP RIGHT (when review active) ===
  const reviewStep = timeline.find(t => t.id === "review");
  if (reviewStep?.status === "active" && preparedData) {
    const baseNet = (preparedData.network || "mainnet").toUpperCase();
    const netLabel = KEEPS_STAGING && baseNet === "MAINNET" ? "MAINNET (STAGING V4)" : baseNet;
    const isGhostnet = baseNet === "GHOSTNET";
    const ghostW = isGhostnet ? 16 : 0; // Space for ghost icon
    const netW = netLabel.length * 4 + 8 + ghostW;
    const netX = w - margin - netW;
    const netY = 4; // Top of screen
    const netScheme = networkBtn.btn.down ? pal.btnNet.hover : pal.btnNet.normal;
    ink(netScheme.bg[0], netScheme.bg[1], netScheme.bg[2]).box(netX, netY, netW, 14);

    if (isGhostnet) {
      // Draw ghost icon first
      drawGhost(ink, box, netX + 2, netY, [255, 180, 50], 1);
      ink(netScheme.text[0], netScheme.text[1], netScheme.text[2]).write(netLabel, { x: netX + ghostW + 2, y: netY + 3 }, undefined, undefined, false, "MatrixChunky8");
    } else {
      ink(netScheme.text[0], netScheme.text[1], netScheme.text[2]).write(netLabel, { x: netX + 4, y: netY + 2 }, undefined, undefined, false, "MatrixChunky8");
    }
    networkBtn.btn.box.x = netX;
    networkBtn.btn.box.y = netY;
    networkBtn.btn.box.w = netW;
    networkBtn.btn.box.h = 14;
  }
  const completeStep = timeline.find(t => t.id === "complete");

  if (completeStep?.status === "done") {
    const viewScheme = pal.btnView;
    const viewSize = mc8ButtonSize("View on objkt");
    const viewX = floor((w - viewSize.w) / 2);
    btn.btn.box.x = viewX;
    btn.btn.box.y = y;
    btn.btn.box.w = viewSize.w;
    btn.btn.box.h = viewSize.h;
    btn.txt = "View on objkt";
    paintMC8Btn(viewX, y, "View on objkt", { ink, line: ink }, viewScheme, btn.btn.down || btn.btn.over);

  } else if (isError) {
    const retryScheme = pal.btnRetry;
    const retrySize = mc8ButtonSize("Retry");
    const retryX = floor((w - retrySize.w) / 2);
    btn.btn.box.x = retryX;
    btn.btn.box.y = y;
    btn.btn.box.w = retrySize.w;
    btn.btn.box.h = retrySize.h;
    btn.txt = "Retry";
    paintMC8Btn(retryX, y, "Retry", { ink, line: ink }, retryScheme, btn.btn.down || btn.btn.over);
  }
}

function act({ event: e, screen }) {
  if (e.is("reframed")) _needsPaint?.();

  // Define hover callbacks once for all buttons (using correct callback names)
  const hoverCb = { hover: () => _needsPaint?.(), leave: () => _needsPaint?.() };

  // ═══════════════════════════════════════════════════════════════════════
  // STATE 1: CONFIRMATION DIALOG
  // ═══════════════════════════════════════════════════════════════════════
  if (waitingConfirmation) {
    // Only enable keep button if user is author - anonymous pieces cannot be kept
    const isAnonymousClick = !pieceAuthorSub && !loadingPieceInfo;
    const canKeep = isAuthor === true && !isAnonymousClick;

    if (canKeep) {
      btn.btn.act(e, { ...hoverCb, push: () => {
        console.log("🪙 KEEP: User confirmed, starting mint process...");
        waitingConfirmation = false;
        resetTimeline();
        startTime = Date.now();
        runProcess();
      }});
    }

    // Preview button - jump to the piece to preview it
    previewBtn.btn.act(e, { ...hoverCb, push: () => {
      console.log("🪙 KEEP: Jumping to preview piece $" + piece);
      _jump?.(`$${piece}`);
    }});

    // Contract button - link to TzKT
    const tzktContractUrl = `https://${NETWORK}.tzkt.io/${KEEPS_CONTRACT}`;
    contractBtn.btn.act(e, { ...hoverCb, push: () => openUrl(tzktContractUrl) });

    // Login button - trigger auth0 login (show when piece has author but user not logged in)
    if (pieceAuthorSub && !userSub) {
      loginBtn.btn.act(e, { ...hoverCb, push: () => {
        console.log("🪙 KEEP: User clicked login, redirecting to login...");
        _api?.login?.(); // Triggers full auth0 login flow via redirect
      }});
    }

    return; // Early return - only process confirmation buttons
  }

  // ═══════════════════════════════════════════════════════════════════════
  // STATE 2: PREPARATION IN PROGRESS
  // ═══════════════════════════════════════════════════════════════════════
  if (isPreparationInProgress()) {
    cancelBtn.btn.act(e, { ...hoverCb, push: () => {
      cancelMintPreparation();
      _jump?.("prompt");
    }});
    return; // Early return - only process cancel button during preparation
  }

  const reviewStep = timeline.find(t => t.id === "review");
  const completeStep = timeline.find(t => t.id === "complete");

  // ═══════════════════════════════════════════════════════════════════════
  // STATE 3: REVIEW & SIGN (preparation complete, ready to mint)
  // ═══════════════════════════════════════════════════════════════════════
  if (reviewStep?.status === "active" && preparedData) {
    // Asset link buttons
    if (preparedData.artifactUri) htmlBtn.btn.act(e, { ...hoverCb, push: () => openUrl(preparedData.artifactUri) });
    if (preparedData.thumbnailUri) thumbBtn.btn.act(e, { ...hoverCb, push: () => openUrl(preparedData.thumbnailUri) });
    if (preparedData.metadataUri) metaBtn.btn.act(e, { ...hoverCb, push: () => openUrl(preparedData.metadataUri) });

    // REBAKE button - regenerate media without resetting timeline
    if (preparedData.usedCachedMedia && !rebaking) {
      rebakeBtn.btn.act(e, { ...hoverCb, push: async () => {
        console.log("🪙 KEEP: Rebaking media (in-place)...");
        rebaking = true;
        rebakeProgress = "Regenerating...";
        _needsPaint?.();

        try {
          const token = await _net?.getToken?.();
          const response = await fetch("/api/keep-mint", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
              ...(token ? { "Authorization": `Bearer ${token}` } : {}),
            },
            body: JSON.stringify({
              piece: "$" + piece,
              mode: "prepare",
              regenerate: true,
              screenWidth: _screen?.width || 128,
              screenHeight: _screen?.height || 128,
              walletAddress, // Send for on-chain owner verification
            }),
          });

          // Parse SSE stream
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = "";

          while (true) {
            const { done, value } = await reader.read();
            if (done) break;

            buffer += decoder.decode(value, { stream: true });
            const lines = buffer.split("\n");
            buffer = lines.pop() || "";

            let eventType = null;
            for (const line of lines) {
              if (line.startsWith("event: ")) {
                eventType = line.slice(7);
              } else if (line.startsWith("data: ") && eventType) {
                try {
                  const eventData = JSON.parse(line.slice(6));
                  console.log("🪙 REBAKE:", eventType, eventData);

                  if (eventType === "progress") {
                    rebakeProgress = eventData.message || (typeof eventData === 'string' ? eventData : JSON.stringify(eventData));
                    _needsPaint?.();
                  } else if (eventType === "ready" && eventData) {
                    // Update preparedData with new URIs
                    preparedData.artifactUri = eventData.artifactUri;
                    preparedData.thumbnailUri = eventData.thumbnailUri;
                    preparedData.metadataUri = eventData.metadataUri;
                    preparedData.usedCachedMedia = false; // Now using fresh media
                    thumbnailBitmap = null; // Clear cached thumbnail so it reloads
                    rebakeProgress = "✓ Regenerated!";
                    console.log("🪙 REBAKE complete:", eventData.artifactUri);
                    // Reload the thumbnail with the new IPFS URI (fire and forget)
                    if (eventData.thumbnailUri) {
                      loadThumbnail(eventData.thumbnailUri).catch(e => 
                        console.warn("🪙 REBAKE: Thumbnail reload failed:", e.message)
                      );
                    }
                  } else if (eventType === "error") {
                    rebakeProgress = "✗ " + (eventData.error || "Failed");
                    console.error("🪙 REBAKE error:", eventData);
                  }
                } catch (e) {
                  // Ignore JSON parse errors
                }
                eventType = null;
              }
            }
          }
        } catch (err) {
          console.error("🪙 REBAKE failed:", err);
          rebakeProgress = "✗ " + err.message;
        } finally {
          rebaking = false;
          _needsPaint?.();
        }
      }});
    }

    // Network button links to contract collection on objkt
    const contractAddress = preparedData.contractAddress || KEEPS_CONTRACT;
    const networkPrefix = preparedData.network === "mainnet" ? "" : "ghostnet.";
    networkBtn.btn.act(e, { ...hoverCb, push: () => openUrl(`https://${networkPrefix}objkt.com/collection/${contractAddress}`) });

    // Staging contract link
    if (KEEPS_STAGING && btn.staging) {
      const tzktStagingUrl = `https://${NETWORK}.tzkt.io/${KEEPS_CONTRACT}`;
      // Proper button handler with hitbox checking and hover support
      if (e.is("move")) {
        const containsNow = btn.staging.box.x <= e.x && e.x < btn.staging.box.x + btn.staging.box.w &&
                           btn.staging.box.y <= e.y && e.y < btn.staging.box.y + btn.staging.box.h;
        if (containsNow && !btn.staging.over && !btn.staging.down) {
          btn.staging.over = true;
          _needsPaint?.();
        } else if (!containsNow && btn.staging.over && !btn.staging.down) {
          btn.staging.over = false;
          _needsPaint?.();
        }
      }
      if (e.is("touch") && e.button < 1) {
        const containsNow = btn.staging.box.x <= e.x && e.x < btn.staging.box.x + btn.staging.box.w &&
                           btn.staging.box.y <= e.y && e.y < btn.staging.box.y + btn.staging.box.h;
        if (containsNow) {
          btn.staging.down = true;
          btn.staging.over = true;
          _needsPaint?.();
        }
      }
      if (e.is("lift") && btn.staging.down) {
        const containsNow = btn.staging.box.x <= e.x && e.x < btn.staging.box.x + btn.staging.box.w &&
                           btn.staging.box.y <= e.y && e.y < btn.staging.box.y + btn.staging.box.h;
        btn.staging.down = false;
        btn.staging.over = false;
        if (containsNow) {
          openUrl(tzktStagingUrl);
        }
        _needsPaint?.();
      }
    }

    // Main action button - sign and mint transaction
    btn.btn.act(e, { ...hoverCb, push: () => signAndMint() });

    return; // Early return - only process review buttons
  }

  // ═══════════════════════════════════════════════════════════════════════
  // STATE 4: MINT COMPLETE
  // ═══════════════════════════════════════════════════════════════════════
  if (completeStep?.status === "done") {
    // Main button - view on objkt.com
    btn.btn.act(e, { ...hoverCb, push: () => {
      const networkPrefix = preparedData?.network === "mainnet" ? "" : "ghostnet.";
      const url = tokenId
        ? `https://${networkPrefix}objkt.com/tokens/${preparedData.contractAddress}/${tokenId}`
        : `https://${networkPrefix}objkt.com/collections/${preparedData.contractAddress}`;
      openUrl(url);
    }});

    return; // Early return - only process completion buttons
  }

  // ═══════════════════════════════════════════════════════════════════════
  // STATE 5: ERROR (preparation or minting failed)
  // ═══════════════════════════════════════════════════════════════════════
  if (hasError()) {
    // Retry button
    btn.btn.act(e, { ...hoverCb, push: () => {
      resetTimeline();
      startTime = Date.now();
      runProcess();
    }});

    return; // Early return - only process retry button on error
  }

  // ═══════════════════════════════════════════════════════════════════════
  // STATE 6: ALREADY MINTED (viewing existing Keep)
  // ═══════════════════════════════════════════════════════════════════════
  if (alreadyMinted) {
    // Use rebaked/pending URIs if available, otherwise use on-chain URIs
    const currentArtifact = rebakeResult?.artifactUri || pendingRebake?.artifactUri || alreadyMinted.artifactUri;
    const currentThumb = rebakeResult?.thumbnailUri || pendingRebake?.thumbnailUri || alreadyMinted.thumbnailUri;
    const hasRebaked = (rebakeResult || pendingRebake) && (currentArtifact !== alreadyMinted.artifactUri);

    if (currentArtifact) htmlBtn.btn.act(e, { ...hoverCb, push: () => openUrl(currentArtifact) });
    if (currentThumb) thumbBtn.btn.act(e, { ...hoverCb, push: () => openUrl(currentThumb) });
    // META button - open TzKT token metadata view
    metaBtn.btn.act(e, { ...hoverCb, push: () => openUrl(alreadyMinted.tzktUrl) });
    // DOCS button - open kidlisp.com/keeps documentation
    docsBtn.btn.act(e, { ...hoverCb, push: () => openUrl("https://kidlisp.com/keeps") });
    btn.btn.act(e, { ...hoverCb, push: () => openUrl(alreadyMinted.objktUrl) });
    walletBtn.btn.act(e, { ...hoverCb, push: () => _jump("wallet") });

    // Contract button - open contract on TzKT
    const tzktContractUrl = `https://${NETWORK}.tzkt.io/${KEEPS_CONTRACT}`;
    contractBtn.btn.act(e, { ...hoverCb, push: () => openUrl(tzktContractUrl) });

    // TX button - open transaction on TzKT
    if (updateChainResult?.opHash) {
      const tzktTxUrl = `https://${NETWORK}.tzkt.io/${updateChainResult.opHash}`;
      txBtn.btn.act(e, { ...hoverCb, push: () => openUrl(tzktTxUrl) });
    }

    // Old on-chain URI buttons (only shown if rebaked version is different)
    // Show old on-chain buttons if there's a rebake with different URIs
    if (hasRebaked) {
      if (alreadyMinted.artifactUri) {
        oldHtmlBtn.btn.act(e, { ...hoverCb, push: () => openUrl(alreadyMinted.artifactUri) });
      }
      if (alreadyMinted.thumbnailUri) {
        oldThumbBtn.btn.act(e, { ...hoverCb, push: () => openUrl(alreadyMinted.thumbnailUri) });
      }
    }

    // Rebake button - regenerate bundle and thumbnail
    // Always attach handler, but it checks rebaking state internally
    rebakeBtn.btn.act(e, {
      ...hoverCb,
      push: async () => {
        // Prevent double-clicks while rebaking
        if (rebaking) {
          console.log("🪙 KEEP: Rebake already in progress, ignoring click");
          return;
        }

        console.log("🪙 KEEP: Starting rebake for already-minted piece $" + piece);

        // Ensure wallet is connected (needed for on-chain ownership verification)
        if (!walletAddress) {
          walletAddress = await _api.tezos.address();
          if (!walletAddress) {
            walletAddress = await _api.tezos.connect(NETWORK);
          }
          console.log("🪙 KEEP: Connected wallet for rebake:", walletAddress);
        }

        // Preserve original on-chain URIs before we overwrite them
        if (!originalOnChainUris && alreadyMinted.artifactUri) {
          originalOnChainUris = {
            artifactUri: alreadyMinted.artifactUri,
            thumbnailUri: alreadyMinted.thumbnailUri,
          };
          console.log("🪙 KEEP: Preserved original URIs:", originalOnChainUris);
        }

        rebaking = true;
        rebakeResult = null;
        rebakeProgress = "Starting...";
        _needsPaint?.();

        try {
          // Get auth token the same way as main process
          const token = await _net?.getToken?.();

          const response = await fetch("/api/keep-mint", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
              ...(token ? { "Authorization": `Bearer ${token}` } : {}),
            },
            body: JSON.stringify({
              piece: "$" + piece,
              mode: "prepare",
              regenerate: true,
              screenWidth: _screen?.width || 128,
              screenHeight: _screen?.height || 128,
              walletAddress, // Send for on-chain owner verification
            }),
          });

          // Parse SSE stream in real-time
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = "";

          while (true) {
            const { done, value } = await reader.read();
            if (done) break;

            buffer += decoder.decode(value, { stream: true });
            const lines = buffer.split("\n");
            buffer = lines.pop() || "";

            let eventType = null;
            for (const line of lines) {
              if (line.startsWith("event: ")) {
                eventType = line.slice(7);
              } else if (line.startsWith("data: ") && eventType) {
                try {
                  const eventData = JSON.parse(line.slice(6));
                  console.log("🪙 REBAKE:", eventType, eventData);

                  if (eventType === "progress") {
                    rebakeProgress = eventData.message || (typeof eventData === 'string' ? eventData : JSON.stringify(eventData));
                    _needsPaint?.();
                  } else if (eventType === "ready" && eventData) {
                    rebakeResult = {
                      artifactUri: eventData.artifactUri,
                      thumbnailUri: eventData.thumbnailUri,
                      metadataUri: eventData.metadataUri,
                      createdAt: eventData.createdAt,
                      packDate: eventData.packDate,
                    };
                    // Update pendingRebake (but NOT alreadyMinted - that stays as on-chain state)
                    pendingRebake = {
                      artifactUri: eventData.artifactUri,
                      thumbnailUri: eventData.thumbnailUri,
                      createdAt: eventData.createdAt,
                      packDate: eventData.packDate,
                    };
                    rebakeProgress = "✓ Bundle regenerated!";
                    _needsPaint?.();
                    console.log("🪙 REBAKE complete! New artifact:", rebakeResult.artifactUri, "Date:", eventData.createdAt || eventData.packDate);
                    // Clear success message after 3 seconds
                    setTimeout(() => {
                      if (rebakeProgress === "✓ Bundle regenerated!") {
                        rebakeProgress = null;
                        _needsPaint?.();
                      }
                    }, 3000);
                    // Reload the thumbnail with the new IPFS URI (fire and forget)
                    if (eventData.thumbnailUri) {
                      loadThumbnail(eventData.thumbnailUri).catch(e =>
                        console.warn("🪙 REBAKE: Thumbnail reload failed:", e.message)
                      );
                    }
                  } else if (eventType === "error") {
                    rebakeProgress = "✗ " + (eventData.error || "Failed");
                    console.error("🪙 REBAKE error:", eventData);
                  }
                } catch (e) {
                  // Ignore JSON parse errors
                }
                eventType = null;
              }
            }
          }
        } catch (err) {
          console.error("🪙 REBAKE failed:", err);
          rebakeProgress = "✗ " + err.message;
        } finally {
          rebaking = false;
          _needsPaint?.();
        }
      }
    });

    // Update Chain button - push new metadata on-chain (requires wallet)
    // Can sync if: rebakeResult exists, or pendingRebake exists, or we just want to force resync
    if (!updatingChain) {
      updateChainBtn.btn.act(e, { ...hoverCb, push: async () => {
        // Determine which URIs to sync - prefer rebake result, then pending, then current on-chain
        console.log("🪙 UPDATE CHAIN: Checking URIs...");
        console.log("  rebakeResult:", rebakeResult);
        console.log("  pendingRebake:", pendingRebake);
        console.log("  alreadyMinted:", alreadyMinted);
        
        const syncArtifact = rebakeResult?.artifactUri || pendingRebake?.artifactUri || alreadyMinted?.artifactUri;
        const syncThumb = rebakeResult?.thumbnailUri || pendingRebake?.thumbnailUri || alreadyMinted?.thumbnailUri;

        if (!syncArtifact) {
          console.error("🪙 UPDATE CHAIN: No artifact URI to sync - alreadyMinted.artifactUri:", alreadyMinted?.artifactUri);
          return;
        }

        if (alreadyMinted?.tokenId == null) {
          console.error("🪙 UPDATE CHAIN: No tokenId available");
          return;
        }

        console.log("🪙 UPDATE CHAIN: Starting on-chain update for token #" + alreadyMinted.tokenId);
        updatingChain = true;
        updateChainResult = null;
        updateChainProgress = null;
        rebakeProgress = null; // Clear rebake progress when starting chain update
        _needsPaint?.();

        try {
          // Connect wallet if not connected
          let walletAddress = await _api.tezos.address();
          if (!walletAddress) {
            walletAddress = await _api.tezos.connect(NETWORK);
          }
          if (!walletAddress) {
            console.error("🪙 UPDATE CHAIN: Wallet connection cancelled");
            updatingChain = false;
            _needsPaint?.();
            return;
          }

          // Call the update-metadata endpoint (streaming SSE)
          // V3 contract allows owner/creator to edit - use prepare mode for client-side signing
          const token = await _net?.getToken?.();
          const requestBody = {
            piece: "$" + piece,
            tokenId: alreadyMinted?.tokenId,
            artifactUri: syncArtifact,
            thumbnailUri: syncThumb,
            walletAddress,
            mode: "prepare", // Client-side signing preserves artist attribution on objkt
          };
          console.log("🪙 UPDATE CHAIN: Sending request:", requestBody);
          
          const response = await fetch("/api/keep-update", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
              ...(token ? { "Authorization": `Bearer ${token}` } : {}),
            },
            body: JSON.stringify(requestBody),
          });

          // Parse SSE stream
          const reader = response.body.getReader();
          const decoder = new TextDecoder();
          let buffer = "";
          let preparedParams = null;

          while (true) {
            const { done, value } = await reader.read();
            if (done) break;

            buffer += decoder.decode(value, { stream: true });
            const lines = buffer.split("\n");
            buffer = lines.pop() || "";

            let eventType = null;
            for (const line of lines) {
              if (line.startsWith("event: ")) {
                eventType = line.slice(7);
              } else if (line.startsWith("data: ") && eventType) {
                try {
                  const data = JSON.parse(line.slice(6));

                  if (eventType === "progress") {
                    updateChainProgress = data.message;
                    console.log("🪙 UPDATE CHAIN:", data.message);
                    _needsPaint?.();
                  } else if (eventType === "prepared") {
                    // Server has prepared the params - now we sign with user's wallet
                    preparedParams = data;
                    console.log("🪙 UPDATE CHAIN: Params prepared, signing with wallet...");
                    updateChainProgress = "Sign in your wallet...";
                    _needsPaint?.();
                  } else if (eventType === "complete") {
                    updateChainResult = data;
                    // Update alreadyMinted to reflect new on-chain state
                    alreadyMinted.artifactUri = data.artifactUri;
                    alreadyMinted.thumbnailUri = data.thumbnailUri;
                    // Update cachedMedia to match (they're now synced)
                    if (cachedMedia) {
                      cachedMedia.artifactUri = data.artifactUri;
                      cachedMedia.thumbnailUri = data.thumbnailUri;
                    }
                    // Clear pending state - chain is now updated
                    pendingRebake = null;
                    rebakeResult = null;
                    originalOnChainUris = null;
                    console.log("🪙 UPDATE CHAIN complete! Op hash:", data.opHash);
                  } else if (eventType === "error") {
                    console.error("🪙 UPDATE CHAIN error:", data.error);
                    updateChainProgress = "✗ " + data.error;
                  }
                } catch (e) {
                  // Ignore JSON parse errors
                }
                eventType = null;
              }
            }
          }

          // If we got prepared params, sign and broadcast with user's wallet
          if (preparedParams) {
            console.log("🪙 UPDATE CHAIN: Calling contract with wallet...", preparedParams);
            try {
              // Use the same call signature as signAndMint: (address, entrypoint, params.value, amount)
              const opHash = await _api.tezos.call(
                preparedParams.contractAddress,
                preparedParams.entrypoint,
                preparedParams.michelsonParams.value, // Just the value, not the full {entrypoint, value}
                0 // No tez transfer for edit_metadata
              );
              
              console.log("🪙 UPDATE CHAIN: Transaction submitted:", opHash);
              updateChainProgress = "Confirming...";
              _needsPaint?.();
              
              // Wait a moment for confirmation
              await new Promise(r => setTimeout(r, 3000));
              
              // Update local state
              updateChainResult = {
                success: true,
                opHash,
                artifactUri: preparedParams.artifactUri,
                thumbnailUri: preparedParams.thumbnailUri,
              };
              alreadyMinted.artifactUri = preparedParams.artifactUri;
              alreadyMinted.thumbnailUri = preparedParams.thumbnailUri;
              if (cachedMedia) {
                cachedMedia.artifactUri = preparedParams.artifactUri;
                cachedMedia.thumbnailUri = preparedParams.thumbnailUri;
              }
              pendingRebake = null;
              rebakeResult = null;
              originalOnChainUris = null;
              console.log("🪙 UPDATE CHAIN complete! Op hash:", opHash);
              updateChainProgress = "✓ Updated on-chain!";
            } catch (walletErr) {
              console.error("🪙 UPDATE CHAIN wallet error:", walletErr);
              updateChainProgress = "✗ " + (walletErr.message || "Wallet error");
            }
          }
        } catch (err) {
          console.error("🪙 UPDATE CHAIN failed:", err);
          updateChainProgress = "✗ " + err.message;
        } finally {
          updatingChain = false;
          _needsPaint?.();
        }
      }});
    }

    return; // Early return - only process already-minted view buttons
  }

  // ═══════════════════════════════════════════════════════════════════════
  // KEYBOARD SHORTCUTS (available in all states)
  // ═══════════════════════════════════════════════════════════════════════

  // Enter key - trigger primary action based on current state
  if (e.is("keyboard:down:enter")) {
    if (alreadyMinted) {
      openUrl(alreadyMinted.objktUrl);
    } else if (reviewStep?.status === "active" && preparedData) {
      signAndMint();
    } else if (hasError()) {
      resetTimeline();
      startTime = Date.now();
      runProcess();
    }
    return;
  }

  // Escape key - cancel/exit current operation
  if (e.is("keyboard:down:escape")) {
    // If preparation is in progress, cancel it before jumping away
    if (isPreparationInProgress()) {
      cancelMintPreparation();
    }
    _jump?.("prompt");
    return;
  }
}

export { boot, paint, sim, act };
