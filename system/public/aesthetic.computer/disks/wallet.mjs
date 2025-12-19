// Wallet, 2024.12.09
// Animated Tezos wallet display with live blockchain data.

/* #region 📚 README 
  A live wallet display showing real-time blockchain activity.
  All info on one dynamic screen with block progress visualization.
  
  When not connected, shows an address input to manually enter a wallet address.
#endregion */

import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

// State
let walletState = null;
let tezPrice = null;
let priceHistory = []; // For price graph (last 24 values)
let transactions = [];
let headBlock = null;
let lastBlockTime = null;
let blockProgress = 0;
let showQR = false; // Toggle QR code display
let qrCells = null; // Cached QR code cells
let pairingUri = null; // Beacon pairing URI for mobile connection
let pairingError = null; // Error if pairing fails
let nftCount = 0; // Number of NFTs owned
let userKidlisps = []; // User's KidLisp pieces
let userKeeps = []; // User's Keeps from contract
let userSub = null; // Current user's sub for KidLisp queries

// Refresh timers
let lastPriceFetch = 0;
let lastBalanceFetch = 0;
let lastBlockFetch = 0;
let lastTxFetch = 0;
let lastKidlispFetch = 0;
let lastKeepsFetch = 0;
const PRICE_REFRESH = 30000;
const BALANCE_REFRESH = 30000;
const BLOCK_REFRESH = 4000;
const TX_REFRESH = 60000;
const BLOCK_TIME = 8000; // Tezos block time ~8 seconds
const KIDLISP_REFRESH = 120000; // 2 minutes
const KEEPS_REFRESH = 60000; // 1 minute

// Animation
let frameCount = 0;

// Falling data streams
let dataStreams = [];

// UI
let disconnectBtn;
let connectTempleBtn;
let connectMobileBtn;
let connectExtensionBtn;
let keepButtons = {}; // Map of code -> ui.TextButton for keeping unkept KidLisps
let showConnectOptions = false; // Show extension vs mobile choice
let connectError = null;
let connecting = false; // Connection in progress
let marqueeOffset = 0; // For scrolling source preview
let selectedKidlisp = null; // Currently selected KidLisp for keeping

// Color scheme - Tezos blue/cyan
const colors = {
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
};

const TEZ_Y_ADJUST = -2;

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
function drawGhost(ink, box, x, y, bodyColor = [255, 180, 50], size = 1) {
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
  box(x + 4 * size, y + 5 * size, size, 2 * size);
  // Right pupil
  box(x + 9 * size, y + 5 * size, size, 2 * size);
}

// Get API base URL based on network
function getApiBase(network) {
  return network === "mainnet" 
    ? "https://api.tzkt.io" 
    : "https://api.ghostnet.tzkt.io";
}

async function boot({ wallet, wipe, hud, ui, screen, user }) {
  wipe(colors.bg);
  hud.label("wallet");
  
  wallet.sync();
  walletState = wallet.get();
  
  // Get logged-in user sub for KidLisp queries
  userSub = user?.sub || null;
  
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
  
  // Sub-buttons for connect options
  connectMobileBtn = new ui.TextButton("📱 Mobile App", {
    center: "x", 
    y: 30,
    screen
  });
  
  connectExtensionBtn = new ui.TextButton("🔌 Browser Extension", {
    center: "x",
    y: 0,
    screen
  });
  
  await Promise.all([
    fetchTezPrice(walletState?.network),
    fetchHeadBlock(walletState?.network),
    walletState?.address ? fetchTransactions(walletState.address, walletState.network) : Promise.resolve(),
    walletState?.address ? fetchNFTCount(walletState.address, walletState.network) : Promise.resolve(),
    walletState?.address ? fetchUserKeeps(walletState.address, walletState.network) : Promise.resolve(),
    userSub ? fetchUserKidlisps(userSub) : Promise.resolve(),
  ]);
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

async function fetchTezPrice(network = "ghostnet") {
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

async function fetchHeadBlock(network = "ghostnet") {
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

async function fetchTransactions(address, network = "ghostnet") {
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

async function fetchNFTCount(address, network = "ghostnet") {
  try {
    const apiBase = network === "mainnet" ? "https://api.tzkt.io" : "https://api.ghostnet.tzkt.io";
    const res = await fetch(`${apiBase}/v1/tokens/balances?account=${address}&balance.gt=0&limit=1000`);
    if (res.ok) {
      const data = await res.json();
      nftCount = data.length;
    }
  } catch (e) {}
}

async function fetchUserKidlisps(userSub) {
  if (!userSub) return;
  try {
    // Fetch recent KidLisp pieces and filter by user
    const res = await fetch(`/api/store-kidlisp?recent=true&limit=200`);
    if (res.ok) {
      const data = await res.json();
      // Filter pieces by this user's sub (stored in user field)
      userKidlisps = (data.recent || [])
        .filter(p => p.user === userSub)
        .slice(0, 12) // Show more pieces
        .map(p => ({
          code: p.code,
          source: p.source || '',
          preview: p.source ? p.source.replace(/\n/g, ' ').slice(0, 60) : p.code,
          kept: p.kept || null,
          when: p.when ? new Date(p.when) : null,
          hits: p.hits || 0,
          marqueeX: 0, // Individual marquee offset
        }));
    }
  } catch (e) {
    console.log("Failed to fetch KidLisps:", e);
  }
}

async function fetchUserKeeps(address, network = "ghostnet") {
  if (!address) return;
  try {
    // Keeps contract address - from keep-mint.mjs
    const contractAddress = network === "mainnet" 
      ? "KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc" // Mainnet (future)
      : "KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc"; // Ghostnet current
    const apiBase = network === "mainnet" ? "https://api.tzkt.io" : "https://api.ghostnet.tzkt.io";
    
    // Fetch tokens owned by this address from the Keeps contract
    const res = await fetch(`${apiBase}/v1/tokens/balances?account=${address}&token.contract=${contractAddress}&balance.gt=0&limit=100`);
    if (res.ok) {
      const data = await res.json();
      userKeeps = data.map(item => ({
        tokenId: item.token.tokenId,
        metadata: item.token.metadata,
        name: item.token.metadata?.name || `Keep #${item.token.tokenId}`,
      }));
    }
  } catch (e) {
    console.log("Failed to fetch Keeps:", e);
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
    if (now - lastKeepsFetch > KEEPS_REFRESH && walletState.address) {
      fetchUserKeeps(walletState.address, walletState.network);
      lastKeepsFetch = now;
    }
  }
  
  // KidLisp refresh (uses userSub, not wallet connection)
  if (now - lastKidlispFetch > KIDLISP_REFRESH && userSub) {
    fetchUserKidlisps(userSub);
    lastKidlispFetch = now;
  }
  
  // Animate marquee for KidLisp source previews
  if (frameCount % 3 === 0) {
    for (const piece of userKidlisps) {
      if (piece.preview && piece.preview.length > 20) {
        piece.marqueeX = (piece.marqueeX || 0) + 1;
        if (piece.marqueeX > piece.preview.length * 6) {
          piece.marqueeX = -80; // Reset with gap
        }
      }
    }
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

function paint($) {
  const { wipe, ink, screen, line, box } = $;
  
  const w = screen.width;
  const h = screen.height;
  const m = 6;
  const top = 16;
  
  wipe(colors.bg);
  
  // Data streams background
  for (const stream of dataStreams) {
    const [r, g, b, a] = stream.color;
    for (let i = 0; i < stream.chars.length; i++) {
      const cy = Math.floor(stream.y - i * 10);
      if (cy < 0 || cy > h) continue;
      const fade = Math.max(10, a - i * 3);
      ink(r, g, b, fade).write(stream.chars[i], { x: stream.x, y: cy });
    }
  }
  
  // Subtle grid
  ink(colors.primary[0], colors.primary[1], colors.primary[2], 12);
  for (let x = 0; x < w; x += 24) line(x, 0, x, h);
  for (let y = 0; y < h; y += 24) line(0, y, w, y);
  
  let y = top;
  
  // === NETWORK (left) & PRICE (right, with mini graph) - TOP ROW ===
  if (walletState?.connected) {
    const netColor = walletState.network === "mainnet" ? colors.positive : colors.block;
    const isGhostnet = walletState.network !== "mainnet";
    
    if (isGhostnet) {
      // Draw Pac-Man ghost icon before "GHOSTNET" text
      drawGhost(ink, box, m, y - 2, netColor, 1);
      ink(...netColor).write("GHOSTNET", { x: m + 16, y });
    } else {
      ink(...netColor).write(walletState.network?.toUpperCase() || "MAINNET", { x: m, y });
    }
  }
  
  if (tezPrice?.usd) {
    // Mini price graph (24 values, ~40px wide)
    if (priceHistory.length > 1) {
      const graphW = 40;
      const graphH = 8;
      const graphX = w - m - graphW - 55;
      const graphY = y - 1;
      
      const minP = Math.min(...priceHistory);
      const maxP = Math.max(...priceHistory);
      const range = maxP - minP || 1;
      
      // Trend color
      const trendUp = priceHistory[priceHistory.length - 1] >= priceHistory[0];
      const graphColor = trendUp ? colors.positive : colors.negative;
      
      for (let i = 1; i < priceHistory.length; i++) {
        const x1 = graphX + ((i - 1) / (priceHistory.length - 1)) * graphW;
        const x2 = graphX + (i / (priceHistory.length - 1)) * graphW;
        const y1 = graphY + graphH - ((priceHistory[i - 1] - minP) / range) * graphH;
        const y2 = graphY + graphH - ((priceHistory[i] - minP) / range) * graphH;
        ink(...graphColor, 180).line(Math.floor(x1), Math.floor(y1), Math.floor(x2), Math.floor(y2));
      }
    }
    
    // Price text (right aligned) - move ꜩ up 3 more pixels
    const priceText = `$${tezPrice.usd.toFixed(2)}`;
    const priceX = w - m - (priceText.length * 6) - 2;
    ink(...colors.primary).write("ꜩ", { x: priceX - 12, y: y + TEZ_Y_ADJUST - 4 }, undefined, undefined, false, "unifont");
    ink(...colors.text).write(priceText, { x: priceX, y });
  }
  y += 14;
  
  // Divider
  ink(...colors.primaryDim, 60).line(m, y, w - m, y);
  y += 6;
  
  if (walletState?.connected) {
    // === IDENTITY SECTION ===
    if (walletState.domain) {
      ink(...colors.secondary).write(walletState.domain, { x: m, y });
      y += 12;
    }
    
    // Address (compact)
    if (walletState.address) {
      const addr = walletState.address;
      const shortAddr = w < 200 ? `${addr.slice(0,8)}...${addr.slice(-6)}` : addr;
      ink(...colors.textDim).write(shortAddr, { x: m, y }, undefined, undefined, false, "MatrixChunky8");
      y += 10;
    }
    
    y += 4;
    
    // === BALANCE (centered) ===
    ink(...colors.textDim).write("BALANCE", { x: w/2, y, center: "x" });
    y += 12;
    
    const bal = walletState.balance !== null ? walletState.balance.toFixed(4) : "...";
    // Center the balance properly using unifont metrics
    ink(...colors.primary).write("ꜩ", { x: w/2, y: y + TEZ_Y_ADJUST, center: "x" }, undefined, undefined, false, "unifont");
    y += 14;
    ink(...colors.primaryBright).write(bal, { x: w/2, y, center: "x" }, undefined, undefined, false, "unifont");
    y += 16; // Extra spacing before USD
    
    // USD value centered below (moved down 2px more)
    if (walletState.balance !== null && tezPrice?.usd) {
      const usd = (walletState.balance * tezPrice.usd).toFixed(2);
      ink(...colors.textDim).write(`($${usd} USD)`, { x: w/2, y, center: "x" });
    }
    y += 14;
    
    // NFT count
    if (nftCount > 0) {
      ink(...colors.textDim).write(`${nftCount} NFT${nftCount !== 1 ? 'S' : ''}`, { x: w/2, y, center: "x" });
      y += 10;
    }
    y += 4;
    
    // === RECENT TRANSACTIONS (compact) ===
    if (transactions.length > 0) {
      ink(...colors.textDim).write("RECENT", { x: m, y });
      y += 10;
      
      const maxTx = Math.min(3, transactions.length);
      for (let i = 0; i < maxTx; i++) {
        const tx = transactions[i];
        const isSent = tx.type === 'sent';
        const color = isSent ? colors.negative : colors.positive;
        const sign = isSent ? '-' : '+';
        const ago = getTimeAgo(tx.timestamp);
        
        // Show transaction type indicator
        const txType = tx.amount === 0 ? 'NFT' : 'XTZ';
        
        ink(...colors.textDim).write(ago, { x: m, y });
        ink(...color).write(`${sign}${tx.amount.toFixed(2)}`, { x: m + 24, y });
        ink(...colors.textDim).write(txType, { x: m + 70, y });
        y += 10;
      }
      y += 6;
    }
    
    // === KIDLISP PIECES (two columns: Kept / Unkept) ===
    if (userKidlisps.length > 0) {
      const keptPieces = userKidlisps.filter(p => p.kept);
      const unkeptPieces = userKidlisps.filter(p => !p.kept);
      const colW = Math.floor((w - m * 3) / 2);
      const leftX = m;
      const rightX = m + colW + m;
      
      // Column headers
      ink(...colors.positive).write(`KEPT (${keptPieces.length})`, { x: leftX, y });
      ink(...colors.textDim).write(`UNKEPT (${unkeptPieces.length})`, { x: rightX, y });
      y += 12;
      
      const maxRows = 4;
      const rowH = 18;
      
      // Draw kept pieces (left column)
      for (let i = 0; i < Math.min(maxRows, keptPieces.length); i++) {
        const piece = keptPieces[i];
        const rowY = y + i * rowH;
        
        // Token ID badge
        if (piece.kept?.tokenId) {
          ink(...colors.primaryBright).write(`#${piece.kept.tokenId}`, { x: leftX, y: rowY });
        }
        
        // Code name
        ink(...colors.positive).write(`$${piece.code}`, { x: leftX + 24, y: rowY }, undefined, undefined, false, "MatrixChunky8");
        
        // Scrolling source preview
        const previewY = rowY + 9;
        const scrollX = piece.marqueeX || 0;
        const previewText = piece.preview || '';
        // Simple scroll display
        const startChar = Math.floor(scrollX / 5) % (previewText.length + 10);
        for (let c = 0; c < 12; c++) {
          const charIdx = (startChar + c) % previewText.length;
          if (charIdx < previewText.length) {
            ink(...colors.textDim).write(previewText[charIdx] || '', { x: leftX + c * 5, y: previewY }, undefined, undefined, false, "MatrixChunky8");
          }
        }
      }
      
      // Draw unkept pieces (right column) with KEEP buttons
      const activeKeepCodes = new Set();
      for (let i = 0; i < Math.min(maxRows, unkeptPieces.length); i++) {
        const piece = unkeptPieces[i];
        const rowY = y + i * rowH;
        activeKeepCodes.add(piece.code);
        
        // Code name
        ink(...colors.secondary).write(`$${piece.code}`, { x: rightX, y: rowY }, undefined, undefined, false, "MatrixChunky8");
        
        // Scrolling source preview
        const previewY = rowY + 9;
        const scrollX = piece.marqueeX || 0;
        const previewText = piece.preview || '';
        const startChar = Math.floor(scrollX / 5) % (previewText.length + 10);
        for (let c = 0; c < 10; c++) {
          const charIdx = (startChar + c) % previewText.length;
          if (charIdx < previewText.length) {
            ink(...colors.textDim).write(previewText[charIdx] || '', { x: rightX + c * 5, y: previewY }, undefined, undefined, false, "MatrixChunky8");
          }
        }
        
        // Create or reposition KEEP button
        const btnX = rightX + colW - 30;
        const btnY = rowY - 2;
        if (!keepButtons[piece.code]) {
          keepButtons[piece.code] = new ui.TextButton("KEEP", { x: btnX, y: btnY });
        } else {
          keepButtons[piece.code].reposition({ x: btnX, y: btnY });
        }
        
        // Paint KEEP button with hover/down states
        const btn = keepButtons[piece.code];
        btn.paint(
          { ink, box, write },
          [colors.primary[0], colors.primary[1], colors.primary[2], 180], // normal: fill, outline, text, alpha
          [colors.primaryBright[0], colors.primaryBright[1], colors.primaryBright[2], 255], // hover/down
          [colors.textDim[0], colors.textDim[1], colors.textDim[2], 100] // disabled
        );
      }
      
      
      // Clean up buttons for pieces no longer visible
      for (const code of Object.keys(keepButtons)) {
        if (!activeKeepCodes.has(code)) {
          delete keepButtons[code];
        }
      }
      
      y += maxRows * rowH + 4;
      
      // Show overflow counts
      if (keptPieces.length > maxRows || unkeptPieces.length > maxRows) {
        if (keptPieces.length > maxRows) {
          ink(...colors.textDim).write(`+${keptPieces.length - maxRows} more`, { x: leftX, y });
        }
        if (unkeptPieces.length > maxRows) {
          ink(...colors.textDim).write(`+${unkeptPieces.length - maxRows} more`, { x: rightX, y });
        }
        y += 10;
      }
      y += 6;
    }
    
    // === KEEPS (from contract) ===
    if (userKeeps.length > 0) {
      ink(...colors.textDim).write("KEEPS", { x: m, y });
      ink(...colors.primaryBright).write(`(${userKeeps.length})`, { x: m + 36, y });
      y += 10;
      
      const maxKeeps = Math.min(3, userKeeps.length);
      for (let i = 0; i < maxKeeps; i++) {
        const keep = userKeeps[i];
        // Parse the name - it's usually the $code from metadata
        let displayName = keep.name;
        if (!displayName.startsWith('$') && keep.metadata?.symbol) {
          displayName = `$${keep.metadata.symbol}`;
        }
        displayName = displayName.length > 16 ? displayName.slice(0, 14) + '...' : displayName;
        ink(...colors.primaryBright).write(displayName, { x: m, y }, undefined, undefined, false, "MatrixChunky8");
        // Show token ID on the right
        ink(...colors.textDim).write(`#${keep.tokenId}`, { x: m + 90, y });
        y += 10;
      }
      if (userKeeps.length > 3) {
        ink(...colors.textDim).write(`+${userKeeps.length - 3} more`, { x: m, y });
        y += 10;
      }
    }
    
    // === BLOCKCHAIN STATUS BAR (at bottom) ===
    const statusY = h - 28;
    const barH = 4;
    const barW = w - m * 2;
    
    // Block progress bar
    ink(...colors.progressBg).box(m, statusY, barW, barH);
    ink(...colors.progressFill).box(m, statusY, Math.floor(barW * blockProgress), barH);
    
    // Status row below progress bar
    const infoY = statusY + barH + 2;
    
    // Left: Block level
    if (headBlock) {
      ink(...colors.block).write(`BLK ${headBlock.level}`, { x: m, y: infoY });
    }
    
    // Center: Time
    const timeStr = new Date().toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
    ink(...colors.textDim).write(timeStr, { x: w/2, y: infoY, center: "x" });
    
    // Right: Cycle
    if (headBlock) {
      const cycleText = `CYC ${headBlock.cycle}`;
      const cycleX = w - m - (cycleText.length * 6);
      ink(...colors.textDim).write(cycleText, { x: cycleX, y: infoY });
    }
    
    // Disconnect button (bottom right, above status bar)
    disconnectBtn?.reposition({ bottom: 32, right: 6, screen });
    disconnectBtn?.paint($, 
      [[50, 30, 40], [120, 70, 90], [180, 150, 170], [50, 30, 40]],
      [[80, 50, 60], [180, 100, 120], [255, 255, 255], [80, 50, 60]]
    );
  } else {
    // Not connected - show connection UI
    const cy = h / 2 - 30;
    
    // Title
    ink(...colors.primary).write("ꜩ", { x: w/2, y: cy - 50 + TEZ_Y_ADJUST, center: "x" }, undefined, undefined, false, "unifont");
    ink(...colors.text).write("CONNECT WALLET", { x: w/2, y: cy - 20, center: "x" });
    
    if (showConnectOptions) {
      // Show two options: Browser Extension and Mobile App
      connectExtensionBtn?.reposition({ center: "x", y: cy + 5, screen });
      connectExtensionBtn?.paint($,
        [[0, 60, 100], [0, 140, 200], [200, 230, 255], [0, 60, 100]],
        [[0, 80, 130], [0, 180, 255], [255, 255, 255], [0, 80, 130]]
      );
      
      connectMobileBtn?.reposition({ center: "x", y: cy + 35, screen });
      connectMobileBtn?.paint($,
        [[60, 40, 80], [120, 80, 160], [200, 160, 255], [60, 40, 80]],
        [[80, 60, 100], [160, 120, 200], [255, 255, 255], [80, 60, 100]]
      );
      
      ink(...colors.textDim).write("Choose connection method", { x: w/2, y: cy + 70, center: "x" });
    } else {
      // Connect Wallet button
      connectTempleBtn?.reposition({ center: "x", y: cy + 15, screen });
      connectTempleBtn?.paint($,
        [[0, 60, 100], [0, 140, 200], [200, 230, 255], [0, 60, 100]],
        [[0, 80, 130], [0, 180, 255], [255, 255, 255], [0, 80, 130]]
      );
    }
    
    // Show connecting state or error
    if (connecting) {
      ink(...colors.primaryBright).write("Connecting...", { x: w/2, y: cy + 90, center: "x" });
    } else if (connectError) {
      ink(...colors.negative).write(connectError, { x: w/2, y: cy + 90, center: "x" });
    }
    
    // Hint
    ink(...colors.textDim).write("ESC to go back", { x: w/2, y: cy + 110, center: "x" });
    
    // Chain info at bottom
    if (headBlock) {
      ink(...colors.block).write(`BLOCK ${headBlock.level}`, { x: w/2, y: h - 40, center: "x" });
      ink(...colors.textDim).write(`CYCLE ${headBlock.cycle}`, { x: w/2, y: h - 28, center: "x" });
    }
    
    if (tezPrice?.usd) {
      ink(...colors.primary).write("ꜩ", { x: w/2 - 30, y: h - 12 + TEZ_Y_ADJUST }, undefined, undefined, false, "unifont");
      ink(...colors.text).write(`$${tezPrice.usd.toFixed(4)}`, { x: w/2 - 18, y: h - 12 });
    }
  }
  
  // === QR CODE OVERLAY (drawn last to overlay everything) ===
  if (showQR) {
    // Dark overlay
    ink(0, 0, 0, 220).box(0, 0, w, h);
    
    if (pairingError) {
      // Show error
      ink(...colors.negative).write("QR Generation Failed", { x: w/2, y: h/2 - 20, center: "x" });
      ink(...colors.textDim).write(pairingError, { x: w/2, y: h/2, center: "x" });
      ink(...colors.textDim).write("Tap to close", { x: w/2, y: h/2 + 30, center: "x" });
    } else if (!qrCells) {
      // Loading state
      ink(...colors.primaryBright).write("Generating QR...", { x: w/2, y: h/2, center: "x" });
    } else {
      // QR code centered
      const qrSize = Math.min(w, h) - 80;
      const cellSize = Math.floor(qrSize / qrCells.length);
      const qrW = cellSize * qrCells.length;
      const qrX = Math.floor((w - qrW) / 2);
      const qrY = Math.floor((h - qrW) / 2) - 30;
      
      // White background
      ink(255, 255, 255).box(qrX - 8, qrY - 8, qrW + 16, qrW + 16);
      
      // QR cells
      ink(0, 0, 0);
      for (let row = 0; row < qrCells.length; row++) {
        for (let col = 0; col < qrCells[row].length; col++) {
          if (qrCells[row][col]) {
            box(qrX + col * cellSize, qrY + row * cellSize, cellSize, cellSize);
          }
        }
      }
      
      // Labels
      ink(...colors.primaryBright).write("SCAN WITH TEMPLE APP", { x: w/2, y: qrY + qrW + 16, center: "x" });
      ink(...colors.textDim).write("Open Temple → Settings → Pair", { x: w/2, y: qrY + qrW + 30, center: "x" });
      ink(...colors.textDim).write("Tap anywhere to close", { x: w/2, y: qrY + qrW + 50, center: "x" });
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

function act({ event: e, wallet, jump, screen }) {
  if (e.is("reframed")) {
    disconnectBtn?.reposition({ bottom: 6, right: 6, screen });
    connectTempleBtn?.reposition({ center: "x", y: screen.height / 2 - 60 + 25, screen });
    connectExtensionBtn?.reposition({ center: "x", y: screen.height / 2 - 60 + 35, screen });
    connectMobileBtn?.reposition({ center: "x", y: screen.height / 2 - 60 + 65, screen });
    initDataStreams(screen);
  }
  
  // Close QR overlay on any tap when showing
  if (showQR && (e.is("touch") || e.is("keyboard:down:escape") || e.is("keyboard:down:enter"))) {
    showQR = false;
    return;
  }
  
  // Close connect options on ESC
  if (showConnectOptions && e.is("keyboard:down:escape")) {
    showConnectOptions = false;
    return;
  }
  
  // Handle wallet connect buttons when not connected
  if (!walletState?.connected && !connecting) {
    if (showConnectOptions) {
      // Extension button
      connectExtensionBtn?.btn.act(e, {
        push: async () => {
          showConnectOptions = false;
          connecting = true;
          connectError = null;
          try {
            await wallet.connect({ network: "ghostnet", walletType: "temple" });
          } catch (err) {
            connectError = err.message || "Connection failed";
            connecting = false;
          }
        }
      });
      
      // Mobile app button - show our own QR code for P2P pairing
      connectMobileBtn?.btn.act(e, {
        push: () => {
          showConnectOptions = false;
          showQR = true;
          qrCells = null;
          pairingError = null;
          // Request pairing code from bios
          wallet.getPairingUri("ghostnet", (response) => {
            console.log("🔷 Mobile pairing response:", response);
            if (response.result === "success" && response.pairingInfo) {
              try {
                // pairingInfo is now the bs58check encoded string
                const code = typeof response.pairingInfo === "string" 
                  ? response.pairingInfo 
                  : JSON.stringify(response.pairingInfo);
                qrCells = qr(code).modules;
                console.log("🔷 QR generated for mobile pairing, code length:", code.length);
              } catch (err) {
                pairingError = "QR generation failed";
                console.error("🔷 QR error:", err);
              }
            } else {
              pairingError = response.error || "Failed to generate pairing";
            }
          });
        }
      });
    } else {
      // Main Connect Wallet button - show options
      connectTempleBtn?.btn.act(e, {
        push: () => {
          showConnectOptions = true;
          connectError = null;
        }
      });
    }
  }
  
  if (walletState?.connected) {
    disconnectBtn?.btn.act(e, {
      push: () => {
        wallet.disconnect();
        showQR = false;
      }
    });
    
    // Handle KEEP button clicks using ui.TextButton
    for (const [code, btn] of Object.entries(keepButtons)) {
      btn.btn.act(e, {
        push: () => {
          console.log("🔷 Keep button clicked for:", code);
          jump(`keep~$${code}`);
        }
      });
    }
  }
  
  if (e.is("keyboard:down:escape")) {
    if (showQR) {
      showQR = false;
    } else {
      jump("prompt");
    }
  }
}

function meta() {
  return {
    title: "Wallet",
    desc: "Live Tezos wallet with blockchain data.",
  };
}

export { boot, sim, paint, act, meta };
