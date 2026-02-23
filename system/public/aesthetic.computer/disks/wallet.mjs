// Wallet, 2024.12.09
// Animated Tezos wallet display with live blockchain data.

/* #region 📚 README
  A live wallet display showing real-time blockchain activity.
  All info on one dynamic screen with block progress visualization.

  When not connected, shows an address input to manually enter a wallet address.
#endregion */

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

// Current Keeps contract (mainnet staging v4)
const KEEPS_CONTRACT = "KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W";
const KEEPS_NETWORK = "mainnet";
// TODO: Set to false when switching to production mainnet contract
const KEEPS_STAGING = true;

// State
let walletState = null;
let tezPrice = null;
let priceHistory = []; // For price graph (last 24 values)
let transactions = [];
let headBlock = null;
let lastBlockTime = null;
let blockProgress = 0;
let nftCount = 0; // Number of NFTs owned
let userKidlisps = []; // User's KidLisp pieces
let ownedKeeps = []; // Keeps tokens owned by wallet on current contract
let userSub = null; // Current user's sub for KidLisp queries
let userHandle = null; // Current user's AC handle

// Refresh timers
let lastPriceFetch = 0;
let lastBalanceFetch = 0;
let lastBlockFetch = 0;
let lastTxFetch = 0;
let lastKidlispFetch = 0;
const PRICE_REFRESH = 30000;
const BALANCE_REFRESH = 30000;
const BLOCK_REFRESH = 4000;
const TX_REFRESH = 60000;
const BLOCK_TIME = 8000; // Tezos block time ~8 seconds
const KIDLISP_REFRESH = 120000; // 2 minutes

// Animation
let frameCount = 0;

// News ticker
let tickerX = 0;
let tickerText = "";

// Falling data streams
let dataStreams = [];

// UI
let disconnectBtn;
let connectTempleBtn;
let keepsLoginBtn; // Login button for keeps section
let keepsSignupBtn; // Signup button for keeps section
let keepButtons = {}; // Map of code -> ui.TextButtonSmall for keeping unkept KidLisps
let stagingLinkBtn; // Link to staging contract on objkt
let ownedKeepBtns = {}; // Map of tokenId -> ui.TextButtonSmall for objkt links
let connectError = null;
let connecting = false; // Connection in progress
let marqueeOffset = 0; // For scrolling source preview
let selectedKidlisp = null; // Currently selected KidLisp for keeping
let keepsScroll = 0;
let keepsScrollMax = 0;
let keepsSectionRect = null;
let keepThumbs = new Map(); // key -> { bitmap, frames, frameIndex, lastFrameTime, loading }
let keepThumbQueue = [];
let activeThumbLoads = 0;
const MAX_THUMB_LOADS = 2;
let _preloadAnimatedWebp = null;
let _preload = null;

// Theme scheme - Tezos blue/cyan
export const scheme = {
  dark: {
    bg: [8, 12, 24],
    primary: [0, 180, 255],
    primaryDim: [0, 100, 150],
    primaryBright: [100, 220, 255],
    secondary: [180, 100, 255],
    text: [200, 210, 230],
    textDim: [80, 100, 130],
    positive: [50, 220, 100],
    negative: [255, 80, 100],
    block: [255, 180, 50],
    progressBg: [30, 40, 60],
    progressFill: [0, 200, 255],
    streamColors: [
      [0, 180, 255, 40],
      [180, 100, 255, 30],
      [255, 180, 50, 25],
      [50, 220, 100, 30],
    ],
  },
  light: {
    bg: [245, 243, 235],
    primary: [0, 120, 180],
    primaryDim: [60, 140, 180],
    primaryBright: [0, 160, 220],
    secondary: [140, 60, 200],
    text: [30, 35, 50],
    textDim: [100, 110, 130],
    positive: [30, 160, 70],
    negative: [200, 50, 70],
    block: [220, 140, 30],
    progressBg: [210, 215, 225],
    progressFill: [0, 140, 200],
    streamColors: [
      [0, 120, 180, 25],
      [140, 60, 200, 20],
      [220, 140, 30, 18],
      [30, 160, 70, 22],
    ],
  },
};

// Active colors - set dynamically based on $.dark
let colors = scheme.dark;

const TEZ_Y_ADJUST = -2;

// Section layout constants
const SECTION_PAD = 6;
const SECTION_MARGIN = 8;
const SECTION_GAP = 8;

// Relative time helper (e.g., "2h ago", "3d ago")
function relativeTime(date) {
  if (!date) return "";
  const now = Date.now();
  const diff = now - date.getTime();
  const mins = Math.floor(diff / 60000);
  const hours = Math.floor(diff / 3600000);
  const days = Math.floor(diff / 86400000);
  if (mins < 1) return "now";
  if (mins < 60) return `${mins}m ago`;
  if (hours < 24) return `${hours}h ago`;
  if (days < 30) return `${days}d ago`;
  return `${Math.floor(days / 30)}mo ago`;
}

// 👻 Pac-Man Ghost Sprite (14x14, classic arcade bitmap)
// Each row is a binary pattern where 1 = body pixel
// Eyes are drawn separately with white sclera + blue pupil
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
  0b11011101110111, // row 11: ██ ███ ███ ██ (wavy bottom)
  0b10001100110001, // row 12: █  ██  ██  █
  0b00000000000000, // row 13: (gap for wavy effect)
];

// Draw a Pac-Man ghost at x, y with given body color
// Size is the pixel scale (1 = 14px tall, 2 = 28px tall, etc.)
function drawGhost(ink, box, x, y, bodyColor = [255, 180, 50], size = 1, eyeOffset = { x: 0, y: 0 }) {
  const w = 14, h = 14;

  // Draw body from sprite
  ink(...bodyColor);
  for (let row = 0; row < h; row++) {
    for (let col = 0; col < w; col++) {
      if ((GHOST_SPRITE[row] >> (w - 1 - col)) & 1) {
        box(x + col * size, y + row * size, size, size);
      }
    }
  }

  // Eyes: white sclera (2x3 pixels each, at cols 3-4 and 8-9, rows 4-6)
  ink(255, 255, 255);
  // Left eye sclera
  box(x + 3 * size, y + 4 * size, 2 * size, 3 * size);
  // Right eye sclera
  box(x + 8 * size, y + 4 * size, 2 * size, 3 * size);

  // Pupils: blue (1x2 pixels, bottom-right of each eye)
  ink(50, 80, 200);
  // Left pupil
  box(x + (4 + eyeOffset.x) * size, y + (5 + eyeOffset.y) * size, size, 2 * size);
  // Right pupil
  box(x + (9 + eyeOffset.x) * size, y + (5 + eyeOffset.y) * size, size, 2 * size);
}

// Get API base URL based on network
function getApiBase(network) {
  return network === "mainnet"
    ? "https://api.tzkt.io"
    : "https://api.ghostnet.tzkt.io";
}

// KidLisp syntax highlighting colors
const syntaxColors = {
  paren: [100, 120, 160],      // Parentheses - dim blue
  keyword: [180, 100, 255],    // Keywords like def, repeat, later - purple
  builtin: [100, 200, 255],    // Builtins like ink, box, wipe - cyan
  number: [255, 200, 100],     // Numbers - gold
  string: [100, 255, 150],     // Strings - green
  comment: [80, 90, 100],      // Comments - dim
  variable: [255, 150, 200],   // $variables - pink
  default: [180, 190, 200],    // Default text
};

// Color word mappings - these get colored with their actual color!
const colorWords = {
  red: [255, 80, 80],
  green: [80, 255, 80],
  blue: [80, 80, 255],
  yellow: [255, 255, 80],
  orange: [255, 165, 80],
  purple: [180, 80, 255],
  pink: [255, 150, 200],
  cyan: [80, 255, 255],
  magenta: [255, 80, 255],
  white: [255, 255, 255],
  black: [100, 100, 100],
  gray: [160, 160, 160],
  grey: [160, 160, 160],
  brown: [180, 120, 80],
  lime: [180, 255, 80],
  navy: [80, 80, 180],
  teal: [80, 180, 180],
  maroon: [180, 80, 80],
  olive: [180, 180, 80],
  aqua: [80, 255, 255],
  silver: [200, 200, 200],
  gold: [255, 215, 80],
  violet: [200, 80, 255],
  indigo: [100, 80, 200],
  coral: [255, 127, 80],
  salmon: [250, 128, 114],
  tan: [210, 180, 140],
  crimson: [220, 20, 60],
  turquoise: [64, 224, 208],
};

// KidLisp keywords and builtins for highlighting
const kidlispKeywords = new Set(['def', 'later', 'repeat', 'if', 'when', 'else', 'and', 'or', 'not', 'let', 'set', 'do', 'fn', 'loop', 'bunch', 'range']);
const kidlispBuiltins = new Set(['ink', 'wipe', 'box', 'line', 'write', 'draw', 'tap', 'wiggle', 'spin', 'smoothspin', 'zoom', 'pan', 'blur', 'contrast', 'rainbow', 'frame', 'width', 'height', 'now', 'noise', 'random', 'sin', 'cos', 'abs', 'floor', 'ceil', 'round', 'min', 'max', 'clamp', 'map', 'fade', 'speaker', 'melody', 'overtone', 'mic', 'amplitude']);

// Tokenize and color KidLisp source for display
function tokenizeKidlisp(source) {
  const tokens = [];
  let i = 0;
  while (i < source.length) {
    const c = source[i];

    // Parentheses
    if (c === '(' || c === ')') {
      tokens.push({ text: c, color: syntaxColors.paren });
      i++;
    }
    // Numbers (including decimals and timing like 0.5s)
    else if (/[0-9]/.test(c) || (c === '-' && /[0-9]/.test(source[i + 1]))) {
      let num = '';
      while (i < source.length && /[0-9.\-]/.test(source[i])) {
        num += source[i++];
      }
      // Include trailing 's' for timing
      if (source[i] === 's') num += source[i++];
      tokens.push({ text: num, color: syntaxColors.number });
    }
    // Variables ($name)
    else if (c === '$') {
      let v = '$';
      i++;
      while (i < source.length && /[a-zA-Z0-9_-]/.test(source[i])) {
        v += source[i++];
      }
      tokens.push({ text: v, color: syntaxColors.variable });
    }
    // Strings
    else if (c === '"') {
      let s = '"';
      i++;
      while (i < source.length && source[i] !== '"') {
        s += source[i++];
      }
      if (i < source.length) s += source[i++];
      tokens.push({ text: s, color: syntaxColors.string });
    }
    // Comments
    else if (c === ';') {
      let comment = '';
      while (i < source.length && source[i] !== '\n') {
        comment += source[i++];
      }
      tokens.push({ text: comment, color: syntaxColors.comment });
    }
    // Words (keywords, builtins, identifiers, color names)
    else if (/[a-zA-Z_]/.test(c)) {
      let word = '';
      while (i < source.length && /[a-zA-Z0-9_\-:!?]/.test(source[i])) {
        word += source[i++];
      }
      const lower = word.toLowerCase();
      // Check for color words first - they get their actual color!
      if (colorWords[lower]) {
        tokens.push({ text: word, color: colorWords[lower] });
      } else if (kidlispKeywords.has(lower)) {
        tokens.push({ text: word, color: syntaxColors.keyword });
      } else if (kidlispBuiltins.has(lower)) {
        tokens.push({ text: word, color: syntaxColors.builtin });
      } else {
        tokens.push({ text: word, color: syntaxColors.default });
      }
    }
    // Whitespace and other
    else {
      tokens.push({ text: c, color: syntaxColors.default });
      i++;
    }
  }
  return tokens;
}

function toIpfsUrl(uri) {
  if (!uri) return null;
  if (uri.startsWith("ipfs://")) return `https://ipfs.aesthetic.computer/ipfs/${uri.slice(7)}`;
  if (uri.startsWith("https://ipfs")) return uri;
  return uri;
}

function getKeepThumbUrl(keep) {
  const uri = keep.thumbnailUri || keep.displayUri || keep.artifactUri;
  const ipfsUrl = toIpfsUrl(uri);
  if (ipfsUrl) return ipfsUrl;
  if (keep.code) {
    const code = keep.code.startsWith("$") ? keep.code.slice(1) : keep.code;
    return `https://oven.aesthetic.computer/grab/webp/64/64/$${code}?duration=2000&fps=6&quality=70&density=1`;
  }
  return null;
}

function queueKeepThumb(keep) {
  const key = keep.tokenId || keep.code;
  if (!key || keepThumbs.has(key)) return;
  if (!_preloadAnimatedWebp && !_preload) return;
  const url = getKeepThumbUrl(keep);
  if (!url) return;
  keepThumbs.set(key, { loading: true });
  keepThumbQueue.push({ key, url });
}

async function loadKeepThumb({ key, url }) {
  try {
    const result = await _preloadAnimatedWebp?.(url);
    if (result?.frameCount > 1) {
      const firstFrame = result.frames[0];
      keepThumbs.set(key, {
        bitmap: { width: result.width, height: result.height, pixels: firstFrame.pixels },
        frames: result.frames,
        frameIndex: 0,
        lastFrameTime: performance.now(),
      });
      return;
    }
    if (result?.frames?.[0]) {
      keepThumbs.set(key, {
        bitmap: { width: result.width, height: result.height, pixels: result.frames[0].pixels },
        frames: null,
      });
      return;
    }
  } catch (e) {
    // fallback below
  }

  try {
    const imgResult = await _preload?.({ path: url, extension: "webp" });
    if (imgResult?.img) {
      keepThumbs.set(key, { bitmap: imgResult.img, frames: null });
    } else {
      keepThumbs.delete(key);
    }
  } catch (e) {
    keepThumbs.delete(key);
  }
}

async function boot({ wallet, wipe, hud, ui, screen, user, handle, net }) {
  wipe(colors.bg);
  hud.label("wallet");

  _preloadAnimatedWebp = net?.preloadAnimatedWebp || null;
  _preload = net?.preload || null;

  wallet.sync();
  walletState = wallet.get();

  // Log wallet state on entering wallet piece
  if (walletState?.connected) {
    const addr = walletState.address;
    const shortAddr = addr ? `${addr.slice(0, 8)}...${addr.slice(-4)}` : "?";
    const domain = walletState.domain;
    const displayName = domain || shortAddr;
    const bal = walletState.balance != null ? `ꜩ${walletState.balance.toFixed(2)}` : "";
    const net = KEEPS_STAGING ? "staging" : (walletState.network || KEEPS_NETWORK);
    console.log(`💼 ${displayName} ${bal} [${net}]`);
  }

  // Get logged-in user sub and handle
  userSub = user?.sub || null;
  userHandle = typeof handle === "function" ? handle() : user?.handle || null;

  connectError = null;
  connecting = false;

  initDataStreams(screen);

  disconnectBtn = new ui.TextButton("Disconnect", {
    bottom: 6,
    right: 6,
    screen
  });

  connectTempleBtn = new ui.TextButton("Connect Wallet", {
    center: "x",
    y: -30,
    screen
  });

  // Login & signup buttons for keeps section (shown when wallet connected but not logged in)
  keepsLoginBtn = new ui.TextButton("Log in", { center: "xy", screen });
  keepsLoginBtn.stickyScrubbing = true;
  keepsSignupBtn = new ui.TextButton("I'm new", { center: "xy", screen });
  keepsSignupBtn.stickyScrubbing = true;

  // Fire off data fetches in background (non-blocking)
  // UI will render immediately and update as data arrives
  Promise.all([
    fetchTezPrice(walletState?.network),
    fetchHeadBlock(walletState?.network),
    walletState?.address ? fetchTransactions(walletState.address, walletState.network) : Promise.resolve(),
    walletState?.address ? fetchNFTCount(walletState.address, walletState.network) : Promise.resolve(),
    walletState?.address ? fetchOwnedKeeps(walletState.address, walletState.network) : Promise.resolve(),
    userSub ? fetchUserKidlisps(userSub) : Promise.resolve(),
  ]).catch(() => {}); // Silent
}

function initDataStreams(screen) {
  dataStreams = [];
  const numStreams = Math.floor(screen.width / 14);
  for (let i = 0; i < numStreams; i++) {
    dataStreams.push(createStream(i * 14, screen.height));
  }
}

function createStream(x, screenHeight) {
  const colorIdx = Math.floor(Math.random() * colors.streamColors.length);
  return {
    x,
    y: -Math.random() * screenHeight,
    speed: 0.3 + Math.random() * 1.2,
    chars: generateStreamChars(),
    color: colors.streamColors[colorIdx],
  };
}

function generateStreamChars() {
  const chars = [];
  const len = 6 + Math.floor(Math.random() * 10);
  for (let i = 0; i < len; i++) {
    chars.push('ꜩ0123456789ABCDEF'[Math.floor(Math.random() * 17)]);
  }
  return chars;
}

async function fetchTezPrice(network = "mainnet") {
  try {
    const apiBase = getApiBase(network);
    const res = await fetch(`${apiBase}/v1/quotes/last`);
    if (res.ok) {
      const data = await res.json();
      tezPrice = { usd: data.usd, eur: data.eur, btc: data.btc };
      // Track price history for mini graph (keep last 24 values)
      priceHistory.push(data.usd);
      if (priceHistory.length > 24) priceHistory.shift();
    }
  } catch (e) {}
}

async function fetchHeadBlock(network = "mainnet") {
  try {
    const apiBase = getApiBase(network);
    const res = await fetch(`${apiBase}/v1/head`);
    if (res.ok) {
      const newHead = await res.json();
      if (!headBlock || newHead.level !== headBlock.level) {
        lastBlockTime = Date.now();
        blockProgress = 0;
      }
      headBlock = newHead;
    }
  } catch (e) {}
}

async function fetchTransactions(address, network = "mainnet") {
  try {
    const apiBase = network === "mainnet" ? "https://api.tzkt.io" : "https://api.ghostnet.tzkt.io";
    const res = await fetch(`${apiBase}/v1/accounts/${address}/operations?limit=5&type=transaction`);
    if (res.ok) {
      const data = await res.json();
      transactions = data.map(tx => ({
        timestamp: new Date(tx.timestamp),
        amount: tx.amount / 1000000,
        type: tx.sender?.address === address ? 'sent' : 'received',
        target: tx.target?.address,
        sender: tx.sender?.address,
        hasInternals: tx.hasInternals || false, // Contract/NFT operations
      }));
    }
  } catch (e) {}
}

async function fetchNFTCount(address, network = "mainnet") {
  try {
    const apiBase = network === "mainnet" ? "https://api.tzkt.io" : "https://api.ghostnet.tzkt.io";
    const res = await fetch(`${apiBase}/v1/tokens/balances?account=${address}&balance.gt=0&limit=1000`);
    if (res.ok) {
      const data = await res.json();
      nftCount = data.length;
    }
  } catch (e) {}
}

// Fetch Keeps tokens owned by this wallet on the current contract
async function fetchOwnedKeeps(address, network = "mainnet") {
  if (!address) return;

  try {
    const apiBase = network === "mainnet" ? "https://api.tzkt.io" : "https://api.ghostnet.tzkt.io";
    // Query tokens owned by this address on the Keeps contract (include balance=0 for burned)
    const res = await fetch(`${apiBase}/v1/tokens/balances?account=${address}&token.contract=${KEEPS_CONTRACT}`);
    if (res.ok) {
      const data = await res.json();
      // Silently loaded keeps tokens

      // Also fetch all tokens on the contract to get mint dates
      const tokensRes = await fetch(`${apiBase}/v1/tokens?contract=${KEEPS_CONTRACT}&limit=100`);
      const allTokens = tokensRes.ok ? await tokensRes.json() : [];
      const tokenMintDates = {};
      for (const t of allTokens) {
        tokenMintDates[t.tokenId] = t.firstTime; // When token was first minted
      }

      // Fetch burn transactions for this address on this contract
      const burnRes = await fetch(`${apiBase}/v1/tokens/transfers?from=${address}&token.contract=${KEEPS_CONTRACT}&limit=100`);
      const burnTransfers = burnRes.ok ? await burnRes.json() : [];
      const burnInfo = {}; // tokenId -> { burnedBy, burnedAt }
      for (const tx of burnTransfers) {
        // A burn is a transfer to null address (burn address) or balance going to 0
        if (tx.to?.address === null || tx.to?.address === "tz1burnburnburnburnburnburnburjAYjjX" || !tx.to) {
          burnInfo[tx.token.tokenId] = {
            burnedBy: tx.from?.alias || tx.from?.address?.slice(0, 8) + "..." || "unknown",
            burnedAt: tx.timestamp ? new Date(tx.timestamp) : null,
          };
        }
      }

      // TzKT returns token metadata directly in the response
      ownedKeeps = data.map((tb) => {
        const tokenId = tb.token.tokenId;
        const meta = tb.token.metadata || {};
        const name = meta.name || `#${tokenId}`;
        const balance = parseInt(tb.balance) || 0;
        const mintedAt = tokenMintDates[tokenId] || tb.token.firstTime;
        const burned = balance === 0;
        const burn = burnInfo[tokenId] || {};

        return {
          tokenId,
          name,
          balance,
          burned,
          burnedBy: burned ? burn.burnedBy : null,
          burnedAt: burned ? burn.burnedAt : null,
          mintedAt: mintedAt ? new Date(mintedAt) : null,
          lastActivity: burned ? burn.burnedAt : (mintedAt ? new Date(mintedAt) : null),
          contract: KEEPS_CONTRACT,
          objktUrl: `https://objkt.com/asset/${KEEPS_CONTRACT}/${tokenId}`,
          thumbnailUri: meta.thumbnailUri || meta.displayUri || meta.artifactUri || null,
          artifactUri: meta.artifactUri || null,
        };
      });

      // Keeps loaded silently
    }
  } catch (e) {
    // Silent
  }
}

async function fetchUserKidlisps(userSub) {
  if (!userSub) return;

  // Loading pieces...

  try {
    // Fetch recent KidLisp pieces and filter by user
    const res = await fetch(`/api/store-kidlisp?recent=true&limit=200`);
    if (res.ok) {
      const data = await res.json();
      // Filter pieces by this user's sub (stored in user field)
      const allUserPieces = (data.recent || []).filter(p => p.user === userSub);

      // Filter pieces by kept status
      const keptPieces = allUserPieces.filter(p => p.kept);

      userKidlisps = allUserPieces
        .slice(0, 20) // Show more pieces
        .map(p => {
          // Only mark as "kept" if it's on the current contract
          const isOnCurrentContract = p.kept?.contractAddress === KEEPS_CONTRACT;
          return {
            code: p.code,
            source: p.source || '',
            preview: p.source ? p.source.replace(/\n/g, ' ').slice(0, 60) : p.code,
            kept: isOnCurrentContract ? { ...p.kept, keptBy: p.kept.keptBy || null } : null,
            oldKept: !isOnCurrentContract && p.kept ? p.kept : null, // Track old keeps for reference
            when: p.when ? new Date(p.when) : null,
            hits: p.hits || 0,
            marqueeX: 0, // Individual marquee offset
          };
        });

      // Pieces loaded
    }
  } catch (e) {
    // Fetch error (silent)
  }
}

function sim({ wallet, jump, screen }) {
  frameCount++;

  const newState = wallet.get();

  // If we just got connected, clear error
  if (newState?.connected && !walletState?.connected) {
    connectError = null;
    connecting = false;
  }

  walletState = newState;

  const now = Date.now();

  // Update block progress
  if (lastBlockTime) {
    blockProgress = Math.min(1, (now - lastBlockTime) / BLOCK_TIME);
  }

  // Data refresh
  if (now - lastPriceFetch > PRICE_REFRESH) {
    fetchTezPrice(walletState?.network);
    lastPriceFetch = now;
  }

  if (now - lastBlockFetch > BLOCK_REFRESH) {
    fetchHeadBlock(walletState?.network);
    lastBlockFetch = now;
  }

  if (walletState?.connected) {
    if (now - lastBalanceFetch > BALANCE_REFRESH) {
      wallet.refreshBalance();
      lastBalanceFetch = now;
    }
    if (now - lastTxFetch > TX_REFRESH && walletState.address) {
      fetchTransactions(walletState.address, walletState.network);
      lastTxFetch = now;
    }
  }

  // KidLisp refresh (uses userSub, not wallet connection)
  if (now - lastKidlispFetch > KIDLISP_REFRESH && userSub) {
    fetchUserKidlisps(userSub);
    lastKidlispFetch = now;
  }

  // Animate marquee for KidLisp source previews (slower, using source length)
  if (frameCount % 4 === 0) {
    for (const piece of userKidlisps) {
      const rawSource = (piece.source || piece.preview || '').replace(/\s+/g, ' ').trim();
      const separator = '  ◆  ';
      const totalLen = (rawSource + separator).length;
      if (totalLen > 10) {
        piece.marqueeX = (piece.marqueeX || 0) + 1;
        // Loop seamlessly when we've scrolled past one full cycle
        const contentWidth = totalLen * 4;
        if (piece.marqueeX >= contentWidth) {
          piece.marqueeX = 0; // Seamless loop back
        }
      }
    }
  }

  // Animate keep thumbnails (animated WebP frames)
  const nowPerf = performance.now();
  for (const thumb of keepThumbs.values()) {
    if (!thumb?.frames || thumb.frames.length <= 1) continue;
    const currentFrame = thumb.frames[thumb.frameIndex || 0];
    const frameDuration = currentFrame?.duration || 100;
    if (!thumb.lastFrameTime || nowPerf - thumb.lastFrameTime >= frameDuration) {
      const nextIndex = ((thumb.frameIndex || 0) + 1) % thumb.frames.length;
      const nextFrame = thumb.frames[nextIndex];
      thumb.frameIndex = nextIndex;
      thumb.lastFrameTime = nowPerf;
      thumb.bitmap = {
        width: thumb.bitmap?.width || nextFrame.width || 1,
        height: thumb.bitmap?.height || nextFrame.height || 1,
        pixels: nextFrame.pixels,
      };
    }
  }

  // Process thumbnail preload queue
  while (activeThumbLoads < MAX_THUMB_LOADS && keepThumbQueue.length > 0) {
    const job = keepThumbQueue.shift();
    activeThumbLoads += 1;
    loadKeepThumb(job).finally(() => {
      activeThumbLoads -= 1;
    });
  }

  // Animate news ticker (smooth, slower)
  if (frameCount % 4 === 0) {
    tickerX -= 1;
  }

  // Build ticker text - Tezos financial news only
  const tickerParts = [];
  if (tezPrice?.usd) {
    const change = priceHistory.length > 1 ?
      ((tezPrice.usd - priceHistory[0]) / priceHistory[0] * 100).toFixed(2) : 0;
    const arrow = change >= 0 ? "▲" : "▼";
    tickerParts.push(`XTZ $${tezPrice.usd.toFixed(3)} ${arrow}${Math.abs(change)}%`);
  }
  if (headBlock) {
    tickerParts.push(`BLOCK #${headBlock.level.toLocaleString()}`);
    // Add block time info
    if (lastBlockTime) {
      const secAgo = Math.floor((Date.now() - lastBlockTime) / 1000);
      tickerParts.push(`${secAgo}s ago`);
    }
  }
  tickerText = tickerParts.join("  ◆  ");
  const tickerWidth = tickerText.length * 6;
  if (tickerX < -tickerWidth) {
    tickerX = screen.width;
  }

  // Animate streams
  for (const stream of dataStreams) {
    stream.y += stream.speed;
    if (stream.y > screen.height + stream.chars.length * 10) {
      stream.y = -stream.chars.length * 10 - Math.random() * 50;
      stream.chars = generateStreamChars();
    }
  }
}

// HUD constants - matching notepat.mjs
const TOP_BAR_BOTTOM = 21; // Standard HUD height
const HUD_LABEL_WIDTH = 44; // "wallet" label width approx

function paint($) {
  // Set theme colors based on dark mode
  colors = $.dark ? scheme.dark : scheme.light;

  const { wipe, ink, screen, line, box, ui, write, paste } = $;

  const w = screen.width;
  const h = screen.height;
  const m = 6;

  wipe(colors.bg);

  // Data streams background (dimmer)
  for (const stream of dataStreams) {
    const [r, g, b, a] = stream.color;
    for (let i = 0; i < stream.chars.length; i++) {
      const cy = Math.floor(stream.y - i * 10);
      if (cy < 0 || cy > h) continue;
      const fade = Math.max(6, (a - i * 3) * 0.6);
      ink(r, g, b, fade).write(stream.chars[i], { x: stream.x, y: cy });
    }
  }

  // Subtle grid (lighter)
  ink(colors.primary[0], colors.primary[1], colors.primary[2], 8);
  for (let x = 0; x < w; x += 24) line(x, 0, x, h);
  for (let y = 0; y < h; y += 24) line(0, y, w, y);

  // Content starts below HUD bar
  let y = TOP_BAR_BOTTOM + 4;

  // === COMPACT TOP ROW: Network indicator only ===
  if (walletState?.connected) {
    const netColor = walletState.network === "mainnet" ? colors.positive : colors.block;
    const isGhostnet = walletState.network !== "mainnet";

    if (isGhostnet) {
      // Draw ghost and label on left
      const eyeShiftX = Math.max(0, Math.min(1, Math.round((Math.sin(frameCount / 12) + 1) / 2)));
      const eyeShiftY = Math.max(0, Math.min(1, Math.round((Math.cos(frameCount / 16) + 1) / 2)));
      drawGhost(ink, box, m, y, netColor, 1, { x: eyeShiftX, y: eyeShiftY });
      ink(...netColor).write("GHOSTNET", { x: m + 16, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
    } else {
      ink(...netColor).write(walletState.network?.toUpperCase() || "MAINNET", { x: m, y });
    }
  }
  y += 16;

  if (walletState?.connected) {
    // === YOUR BALANCE section ===
    const balBarH = 50;
    const balBarTop = y;
    const headerH = 14;

    // Background with left/right borders
    ink(0, 30, 50, 150).box(SECTION_MARGIN, balBarTop, w - SECTION_MARGIN * 2, balBarH, "fill");
    ink(0, 80, 120, 100).box(SECTION_MARGIN, balBarTop, w - SECTION_MARGIN * 2, balBarH, "outline");

    const innerM = SECTION_MARGIN + SECTION_PAD;
    const headerY = balBarTop + SECTION_PAD;
    const rightInner = w - SECTION_MARGIN - SECTION_PAD;

    // Section title (left)
    ink(...colors.primary).write("YOUR BALANCE", { x: innerM, y: headerY });

    // Balance line: ꜩ X.XXXX (large unifont) - top right, same line as title
    const bal = walletState.balance !== null ? walletState.balance.toFixed(4) : "...";
    const balTextWidth = bal.length * 8 + 14; // unifont char width + ꜩ symbol
    const balStartX = rightInner - balTextWidth;
    ink(...colors.primary).write("ꜩ", { x: balStartX, y: headerY + TEZ_Y_ADJUST }, undefined, undefined, false, "unifont");
    ink(...colors.primaryBright).write(bal, { x: balStartX + 14, y: headerY }, undefined, undefined, false, "unifont");

    // Left side: identity (domain / handle / address stacked)
    const contentY = headerY + headerH;
    let idY = contentY;
    if (walletState.domain) {
      ink(...colors.secondary).write(walletState.domain, { x: innerM, y: idY }, undefined, undefined, false, "MatrixChunky8");
      idY += 9;
    }
    if (userHandle) {
      const displayHandle = userHandle.startsWith("@") ? userHandle : `@${userHandle}`;
      ink(...colors.primaryBright).write(displayHandle, { x: innerM, y: idY }, undefined, undefined, false, "MatrixChunky8");
      idY += 9;
    }
    if (walletState.address) {
      const addr = walletState.address;
      const shortAddr = `${addr.slice(0,6)}...${addr.slice(-4)}`;
      ink(...colors.textDim).write(shortAddr, { x: innerM, y: idY }, undefined, undefined, false, "MatrixChunky8");
    }

    // Right side: USD value + XTZ price (below balance)
    // USD value of wallet
    if (walletState.balance !== null && tezPrice?.usd) {
      const usd = (walletState.balance * tezPrice.usd).toFixed(2);
      const usdText = `$${usd} USD`;
      const usdWidth = usdText.length * 6; // default font ~6px
      ink(...colors.text).write(usdText, { x: rightInner - usdWidth, y: contentY });
    }

    // 1 XTZ price (below USD)
    if (tezPrice?.usd) {
      const tezText = `1 tez = $${tezPrice.usd.toFixed(2)}`;
      const tezWidth = tezText.length * 4; // MatrixChunky8 ~4px
      ink(...colors.textDim).write(tezText, { x: rightInner - tezWidth, y: contentY + 12 }, undefined, undefined, false, "MatrixChunky8");
    }

    y = balBarTop + balBarH + SECTION_GAP;

    // === KIDLISP PIECES (two columns: Kept / Unkept) ===
    // Show login/signup buttons if wallet connected but not logged in
    if (!userSub && walletState?.connected) {
      y += 12;

      // Position buttons side by side like prompt.mjs
      keepsLoginBtn.reposition({ center: "x", y, screen });
      keepsSignupBtn.reposition({ center: "x", y, screen });
      const offset = 5;
      keepsSignupBtn.btn.box.x += keepsSignupBtn.btn.box.w / 2 + offset;
      keepsLoginBtn.btn.box.x -= keepsLoginBtn.btn.box.w / 2 + offset;

      // Paint with prompt.mjs style colors (dark mode)
      keepsLoginBtn.paint({ ink, box, write }, [[0, 0, 64], 255, 255, [0, 0, 64]]);
      keepsSignupBtn.paint({ ink, box, write }, [[0, 64, 0], 255, 255, [0, 64, 0]]);

      y += 28;
      // Subtitle below buttons
      ink(...colors.textDim).write("to see your keeps", { x: w/2, y, center: "x" });
      y += 16;
    } else if (userKidlisps.length > 0 || ownedKeeps.length > 0) {
      // Build unified keeps list: authored keeps + collected (owned but not authored)
      const isKeptByUser = p =>
        p.kept &&
        (p.kept.keptBy === userSub ||
          (walletState?.address && p.kept.walletAddress === walletState.address) ||
          (!p.kept.keptBy && !p.kept.walletAddress));
      const keptPieces = userKidlisps.filter(isKeptByUser);
      const unkeptPieces = userKidlisps.filter(p => !isKeptByUser(p));

      // Build unified keeps: merge keptPieces with ownership info from ownedKeeps
      const unifiedKeeps = keptPieces.map(p => {
        const owned = ownedKeeps.find(o => o.tokenId === p.kept?.tokenId);
        const tid = p.kept?.tokenId;
        const burned = owned?.burned || false;
        return {
          code: p.code,
          tokenId: tid,
          owned: !!owned && !burned,
          burned,
          burnedBy: owned?.burnedBy || null,
          burnedAt: owned?.burnedAt || null,
          mintedAt: owned?.mintedAt || (p.kept?.mintedAt ? new Date(p.kept.mintedAt) : null),
          lastActivity: owned?.lastActivity || owned?.mintedAt || (p.kept?.mintedAt ? new Date(p.kept.mintedAt) : null),
          balance: owned?.balance || 0,
          objktUrl: owned?.objktUrl || (tid ? `https://objkt.com/tokens/${KEEPS_CONTRACT}/${tid}` : `https://objkt.com/collection/${KEEPS_CONTRACT}`),
          authored: true,
          source: p.source || '',
          hits: p.hits || 0,
          thumbnailUri: p.kept?.thumbnailUri || owned?.thumbnailUri || null,
          artifactUri: owned?.artifactUri || null,
        };
      });

      // Add collected tokens (owned but not authored by user)
      const authoredTokenIds = new Set(keptPieces.map(p => p.kept?.tokenId).filter(Boolean));
      for (const owned of ownedKeeps) {
        if (!authoredTokenIds.has(owned.tokenId)) {
          unifiedKeeps.push({
            code: owned.name?.replace('$', '') || `token${owned.tokenId}`,
            tokenId: owned.tokenId,
            owned: !owned.burned,
            burned: owned.burned,
            burnedBy: owned.burnedBy,
            burnedAt: owned.burnedAt,
            mintedAt: owned.mintedAt,
            lastActivity: owned.lastActivity || owned.mintedAt,
            balance: owned.balance,
            objktUrl: owned.objktUrl,
            authored: false,
            collected: true,
            hits: null,
            source: '',
            thumbnailUri: owned.thumbnailUri || null,
            artifactUri: owned.artifactUri || null,
          });
        }
      }

      // Sort by recent activity (most recent first)
      unifiedKeeps.sort((a, b) => {
        const aTime = a.lastActivity?.getTime() || 0;
        const bTime = b.lastActivity?.getTime() || 0;
        return bTime - aTime;
      });

      const rowH = 28;
      const headerH = 14;
      const innerPad = SECTION_PAD;
      const fullW = w - SECTION_MARGIN * 2;

      // === YOUR KIDLISP KEEPS section (unified) ===
      if (unifiedKeeps.length > 0) {
        const maxKeepsRows = Math.min(10, Math.max(4, Math.floor((h - y - 60) / rowH)));
        const visibleKeepsRows = Math.min(maxKeepsRows, unifiedKeeps.length);
        const keepsH = innerPad + headerH + visibleKeepsRows * rowH + innerPad;

        // Background with left/right borders
        ink(0, 40, 30, 180).box(SECTION_MARGIN, y, fullW, keepsH, "fill");
        ink(0, 100, 60, 100).box(SECTION_MARGIN, y, fullW, keepsH, "outline");

        const innerX = SECTION_MARGIN + innerPad;
        const headerY = y + innerPad;

        // Header
        ink(...colors.positive).write(KEEPS_STAGING ? "YOUR KIDLISP KEEPS on" : "YOUR KIDLISP KEEPS", { x: innerX, y: headerY });

        // Staging button
        if (KEEPS_STAGING) {
          const stagingX = innerX + 132; // After "YOUR KIDLISP KEEPS on "
          if (!stagingLinkBtn) {
            stagingLinkBtn = new ui.TextButtonSmall("STAGING V3 ", { x: stagingX, y: headerY - 1 });
          } else {
            stagingLinkBtn.reposition({ x: stagingX, y: headerY - 1 });
          }
          stagingLinkBtn.paint({ ink, box, write },
            [[80, 60, 0], [140, 100, 0], [255, 200, 100], [80, 60, 0]],
            [[100, 80, 0], [180, 140, 0], [255, 255, 200], [100, 80, 0]]
          );
        }

        const keepsStartY = headerY + headerH;
        const activeOwnedIds = new Set();

        keepsScrollMax = Math.max(0, (unifiedKeeps.length - visibleKeepsRows) * rowH);
        if (keepsScroll > keepsScrollMax) keepsScroll = keepsScrollMax;
        if (keepsScroll < 0) keepsScroll = 0;

        keepsSectionRect = {
          x: SECTION_MARGIN,
          y,
          w: fullW,
          h: keepsH,
          innerX,
          innerY: keepsStartY,
          innerH: visibleKeepsRows * rowH,
        };

        const startIdx = Math.floor(keepsScroll / rowH);
        const offsetY = -(keepsScroll % rowH);

        for (let i = 0; i < visibleKeepsRows; i++) {
          const keep = unifiedKeeps[startIdx + i];
          if (!keep) continue;
          const rowY = keepsStartY + i * rowH + offsetY;
          const textY = rowY + 4;
          const subY = rowY + 16;
          const thumbSize = 20;
          const thumbX = innerX;
          const thumbY = rowY + 4;

          activeOwnedIds.add(keep.tokenId);
          queueKeepThumb(keep);

          // Thumbnail
          const thumbKey = keep.tokenId || keep.code;
          const thumb = thumbKey ? keepThumbs.get(thumbKey) : null;
          if (thumb?.bitmap) {
            const scale = thumbSize / (thumb.bitmap.width || thumbSize);
            paste(thumb.bitmap, thumbX, thumbY, { scale });
          } else {
            ink(20, 30, 35).box(thumbX, thumbY, thumbSize, thumbSize);
            ink(60, 80, 90).box(thumbX, thumbY, thumbSize, thumbSize, "outline");
          }

          const textX = innerX + thumbSize + 6;

          // Token ID
          ink(...colors.primaryBright).write(`#${keep.tokenId}`, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");

          // Code name (color indicates status)
          const isBurned = keep.burned;
          const nameColor = isBurned ? colors.negative : (keep.owned ? colors.positive : colors.textDim);
          ink(...nameColor).write(`$${keep.code}`, { x: textX + 16, y: textY }, undefined, undefined, false, "MatrixChunky8");

          // Source preview + hits
          const btnRightEdge = w - SECTION_MARGIN - innerPad;
          const objktBtnX = btnRightEdge - 42;
          const sourceMaxW = Math.max(40, objktBtnX - textX - 4);
          const sourceMaxChars = Math.max(0, Math.floor(sourceMaxW / 4));
          const rawSource = (keep.source || '').replace(/\s+/g, ' ').trim();
          const sourceText = rawSource && sourceMaxChars > 2 ? rawSource.slice(0, sourceMaxChars - 1) : '';
          if (sourceText) {
            ink(...colors.textDim).write(sourceText, { x: textX, y: subY }, undefined, undefined, false, "MatrixChunky8");
          }

          if (keep.hits !== null && keep.hits !== undefined) {
            const hitsText = `${keep.hits} hits`;
            ink(...colors.textDim).write(hitsText, { x: objktBtnX - 6, y: subY, right: true }, undefined, undefined, false, "MatrixChunky8");
          }

          // OBJKT.com button (flush right)
          const btnY = rowY + 6;
          if (!ownedKeepBtns[keep.tokenId]) {
            ownedKeepBtns[keep.tokenId] = new ui.TextButtonSmall("OBJKT.com", { x: objktBtnX, y: btnY });
          } else {
            ownedKeepBtns[keep.tokenId].reposition({ x: objktBtnX, y: btnY });
          }
          ownedKeepBtns[keep.tokenId].paint({ ink, box, write },
            isBurned ? [[60, 30, 30], [100, 50, 50], [180, 100, 100], [60, 30, 30]] : [[0, 40, 60], [0, 100, 140], [100, 200, 255], [0, 40, 60]],
            isBurned ? [[80, 40, 40], [140, 70, 70], [220, 150, 150], [80, 40, 40]] : [[0, 60, 80], [0, 140, 180], [200, 255, 255], [0, 60, 80]]
          );
        }

        // Scrollbar
        if (keepsScrollMax > 0) {
          const trackX = SECTION_MARGIN + fullW - 3;
          const trackY = keepsStartY;
          const trackH = visibleKeepsRows * rowH;
          const thumbH = Math.max(12, Math.floor(trackH * (visibleKeepsRows / unifiedKeeps.length)));
          const thumbY = trackY + Math.floor((trackH - thumbH) * (keepsScroll / keepsScrollMax));
          ink(0, 80, 60, 120).box(trackX, trackY, 2, trackH);
          ink(0, 140, 90, 200).box(trackX, thumbY, 2, thumbH);
        }

        // Clean up buttons
        for (const tokenId of Object.keys(ownedKeepBtns)) {
          if (!activeOwnedIds.has(parseInt(tokenId))) {
            delete ownedKeepBtns[tokenId];
          }
        }

        y += keepsH + SECTION_GAP;
      } else {
        keepsSectionRect = null;
      }

      // === YOUR KIDLISP section (unkept pieces) ===
      if (unkeptPieces.length > 0) {
        const kidlispRowH = 26;
        const maxRows = Math.max(3, Math.floor((h - y - 30) / kidlispRowH));
        const visibleRows = Math.min(maxRows, unkeptPieces.length);
        const kidlispH = innerPad + headerH + visibleRows * kidlispRowH + innerPad;

        // Background with left/right borders
        ink(40, 20, 60, 180).box(SECTION_MARGIN, y, fullW, kidlispH, "fill");
        ink(100, 50, 120, 100).box(SECTION_MARGIN, y, fullW, kidlispH, "outline");

        const innerX = SECTION_MARGIN + innerPad;
        const headerY = y + innerPad;

        // Header
        ink(...colors.secondary).write("YOUR KIDLISP", { x: innerX, y: headerY });
        const kidlispStartY = headerY + headerH;

        const sortedUnkept = [...unkeptPieces].sort((a, b) => (b.hits || 0) - (a.hits || 0));

        // Draw unkept pieces with KEEP buttons and syntax-highlighted source ticker
        const activeKeepCodes = new Set();
        for (let i = 0; i < visibleRows; i++) {
          const piece = sortedUnkept[i];
          if (!piece) continue;
          const rowY = kidlispStartY + i * kidlispRowH;
          const textY = rowY + 4;
          const subY = rowY + 16;
          activeKeepCodes.add(piece.code);

          const thumbSize = 18;
          const thumbX = innerX;
          const thumbY = rowY + 4;
          queueKeepThumb(piece);

          const thumbKey = piece.code;
          const thumb = thumbKey ? keepThumbs.get(thumbKey) : null;
          if (thumb?.bitmap) {
            const scale = thumbSize / (thumb.bitmap.width || thumbSize);
            paste(thumb.bitmap, thumbX, thumbY, { scale });
          } else {
            ink(20, 30, 35).box(thumbX, thumbY, thumbSize, thumbSize);
            ink(60, 80, 90).box(thumbX, thumbY, thumbSize, thumbSize, "outline");
          }

          const textX = innerX + thumbSize + 6;

          // Code name
          const codeW = (piece.code.length + 1) * 4 + 4;
          ink(...colors.secondary).write(`$${piece.code}`, { x: textX, y: textY }, undefined, undefined, false, "MatrixChunky8");

          // Timestamp (relative time)
          const timeStr = piece.when ? relativeTime(piece.when) : "";
          if (timeStr) {
            ink(...colors.textDim).write(timeStr, { x: textX + codeW, y: textY }, undefined, undefined, false, "MatrixChunky8");
          }

          // Source preview + hits
          const btnRightEdge = w - SECTION_MARGIN - innerPad;
          const btnX = btnRightEdge - 22;
          const sourceMaxW = Math.max(40, btnX - textX - 4);
          const sourceMaxChars = Math.max(0, Math.floor(sourceMaxW / 4));
          const rawSource = (piece.source || piece.preview || '').replace(/\s+/g, ' ').trim();
          const sourceText = rawSource && sourceMaxChars > 2 ? rawSource.slice(0, sourceMaxChars - 1) : '';
          if (sourceText) {
            ink(...colors.textDim).write(sourceText, { x: textX, y: subY }, undefined, undefined, false, "MatrixChunky8");
          }

          if (piece.hits !== null && piece.hits !== undefined) {
            const hitsText = `${piece.hits} hits`;
            ink(...colors.textDim).write(hitsText, { x: btnX - 6, y: subY, right: true }, undefined, undefined, false, "MatrixChunky8");
          }

          // Keep button (flush right)
          const btnY = rowY + 5;
          if (!keepButtons[piece.code]) {
            keepButtons[piece.code] = new ui.TextButtonSmall("Keep", { x: btnX, y: btnY });
          } else {
            keepButtons[piece.code].reposition({ x: btnX, y: btnY });
          }
          keepButtons[piece.code].paint({ ink, box, write });
        }

        // Clean up buttons
        for (const code of Object.keys(keepButtons)) {
          if (!activeKeepCodes.has(code)) {
            delete keepButtons[code];
          }
        }

        y += kidlispH + SECTION_GAP;
      }
    }

    // Disconnect button (moved up, above ticker)
    disconnectBtn?.reposition({ bottom: 18, right: 6, screen });
    disconnectBtn?.paint($,
      [[50, 30, 40], [120, 70, 90], [180, 150, 170], [50, 30, 40]],
      [[80, 50, 60], [180, 100, 120], [255, 255, 255], [80, 50, 60]]
    );

    // === BOTTOM TICKER ===
    if (tickerText) {
      const tickerY = h - 10;
      ink(0, 20, 40, 180).box(0, tickerY - 2, w, 12, "fill");
      ink(...colors.primaryBright, 180).write(tickerText, { x: Math.floor(tickerX), y: tickerY });
    }
  } else {
    // Not connected - show connection UI (centered vertically)
    // Content height: ~50 (tez symbol) + 30 (title) + 10 (handle) + 40 (button) = ~130
    const contentH = userHandle ? 100 : 90;
    const cy = h / 2 - contentH / 2 + 20;

    // Title
    ink(...colors.primary).write("ꜩ", { x: w/2, y: cy - 30 + TEZ_Y_ADJUST, center: "x" }, undefined, undefined, false, "unifont");
    ink(...colors.text).write("CONNECT WALLET", { x: w/2, y: cy, center: "x" });
    if (userHandle) {
      const displayHandle = userHandle.startsWith("@") ? userHandle : `@${userHandle}`;
      ink(...colors.primaryBright).write(displayHandle, { x: w/2, y: cy + 14, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    }

    const btnOffsetY = userHandle ? 35 : 25;

    // Connect Wallet button — Beacon handles wallet selection
    connectTempleBtn?.reposition({ center: "x", y: cy + btnOffsetY, screen });
    const connectBtnColors = $.dark
      ? [[[0, 60, 100], [0, 140, 200], [200, 230, 255], [0, 60, 100]],
         [[0, 80, 130], [0, 180, 255], [255, 255, 255], [0, 80, 130]]]
      : [[[210, 230, 245], [0, 100, 160], [0, 70, 120], [210, 230, 245]],
         [[225, 240, 250], [0, 130, 190], [0, 90, 150], [225, 240, 250]]];
    connectTempleBtn?.paint($, ...connectBtnColors);

    // Show connecting state or error
    if (connecting) {
      ink(...colors.primaryBright).write("Connecting...", { x: w/2, y: cy + btnOffsetY + 40, center: "x" });
    } else if (connectError) {
      ink(...colors.negative).write(connectError, { x: w/2, y: cy + btnOffsetY + 40, center: "x" });
    }
  }

}

function getTimeAgo(date) {
  const s = Math.floor((Date.now() - date.getTime()) / 1000);
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  if (m < 60) return `${m}m`;
  const hr = Math.floor(m / 60);
  if (hr < 24) return `${hr}h`;
  return `${Math.floor(hr / 24)}d`;
}

function act({ event: e, wallet, jump, screen, net }) {
  if (e.is("reframed")) {
    disconnectBtn?.reposition({ bottom: 6, right: 6, screen });
    connectTempleBtn?.reposition({ center: "x", y: screen.height / 2 - 60 + 25, screen });
    initDataStreams(screen);
  }

  // Scroll keeps list (mouse wheel or drag)
  if (keepsSectionRect) {
    const inKeeps = e.x >= keepsSectionRect.x && e.x <= keepsSectionRect.x + keepsSectionRect.w &&
      e.y >= keepsSectionRect.y && e.y <= keepsSectionRect.y + keepsSectionRect.h;

    if (inKeeps && e.is("scroll")) {
      keepsScroll -= e.y;
      if (keepsScroll < 0) keepsScroll = 0;
      if (keepsScroll > keepsScrollMax) keepsScroll = keepsScrollMax;
      return;
    }

    if (inKeeps && e.is("draw:1")) {
      keepsScroll += e.delta.y;
      if (keepsScroll < 0) keepsScroll = 0;
      if (keepsScroll > keepsScrollMax) keepsScroll = keepsScrollMax;
      return;
    }
  }


  if (e.is("keyboard:down:arrowdown")) {
    if (keepsSectionRect) keepsScroll = Math.min(keepsScrollMax, keepsScroll + 20);
  } else if (e.is("keyboard:down:arrowup")) {
    if (keepsSectionRect) keepsScroll = Math.max(0, keepsScroll - 20);
  }

  // Handle wallet connect button when not connected
  if (!walletState?.connected && !connecting) {
    connectTempleBtn?.btn.act(e, {
      push: async () => {
        connecting = true;
        connectError = null;
        try {
          await wallet.connect({ network: KEEPS_NETWORK });
        } catch (err) {
          connectError = err.message || "Connection failed";
          connecting = false;
        }
      }
    });
  }

  if (walletState?.connected) {
    disconnectBtn?.btn.act(e, {
      push: () => {
        wallet.disconnect();
      }
    });

    // Login/signup buttons for keeps (when not logged in)
    if (!userSub) {
      keepsLoginBtn?.btn.act(e, {
        push: () => {
          net.login();
        }
      });
      keepsSignupBtn?.btn.act(e, {
        push: () => {
          net.signup();
        }
      });
    }

    // Handle KEEP button clicks (TextButtonSmall has .btn property)
    for (const [code, btn] of Object.entries(keepButtons)) {
      btn.btn.act(e, {
        push: () => {
          // Keeping...
          jump(`keep~$${code}`);
        }
      });
    }

    // Handle staging contract link click
    if (KEEPS_STAGING && stagingLinkBtn) {
      stagingLinkBtn.btn.act(e, {
        push: () => {
          const objktUrl = `https://objkt.com/collections/${KEEPS_CONTRACT}`;
          // Opening objkt...
          net.web(objktUrl, true);
        }
      });
    }

    // Handle owned keeps objkt link clicks
    for (const [tokenId, btn] of Object.entries(ownedKeepBtns)) {
      btn.btn.act(e, {
        push: () => {
          const objktUrl = `https://objkt.com/tokens/${KEEPS_CONTRACT}/${tokenId}`;
          // Opening token...
          net.web(objktUrl, true);
        }
      });
    }
  }

  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

function meta() {
  return {
    title: "Wallet",
    desc: "Live Tezos wallet with blockchain data.",
  };
}

export { boot, sim, paint, act, meta };
