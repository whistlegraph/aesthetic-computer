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
let transactions = [];
let headBlock = null;
let lastBlockTime = null;
let blockProgress = 0;
let showQR = false; // Toggle QR code display
let qrCells = null; // Cached QR code cells
let pairingUri = null; // Beacon pairing URI for mobile connection
let pairingError = null; // Error if pairing fails

// Refresh timers
let lastPriceFetch = 0;
let lastBalanceFetch = 0;
let lastBlockFetch = 0;
let lastTxFetch = 0;
const PRICE_REFRESH = 30000;
const BALANCE_REFRESH = 30000;
const BLOCK_REFRESH = 4000;
const TX_REFRESH = 60000;
const BLOCK_TIME = 8000; // Tezos block time ~8 seconds

// Animation
let frameCount = 0;

// Falling data streams
let dataStreams = [];

// UI
let disconnectBtn;
let connectTempleBtn;
let connectMobileBtn;
let connectExtensionBtn;
let showConnectOptions = false; // Show extension vs mobile choice
// let connectKukaiBtn; // Disabled - Beacon SDK has bundling issues
let signTestBtn;
let showQRBtn;
let connectError = null;
let connecting = false; // Connection in progress
let signResult = null; // Last sign result
let signing = false; // Signing in progress
let signFeedback = null; // Feedback message for signing
let signFeedbackTime = 0; // When feedback was set (for auto-clear)

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

// Get API base URL based on network
function getApiBase(network) {
  return network === "mainnet" 
    ? "https://api.tzkt.io" 
    : "https://api.ghostnet.tzkt.io";
}

async function boot({ wallet, wipe, hud, ui, screen }) {
  wipe(colors.bg);
  hud.label("wallet");
  
  wallet.sync();
  walletState = wallet.get();
  
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
  
  signTestBtn = new ui.TextButton("Test Sign", {
    bottom: 6,
    left: 6,
    screen
  });
  
  showQRBtn = new ui.TextButton("QR", {
    bottom: 6,
    left: 80,
    screen
  });
  
  await Promise.all([
    fetchTezPrice(walletState?.network),
    fetchHeadBlock(walletState?.network),
    walletState?.address ? fetchTransactions(walletState.address, walletState.network) : Promise.resolve(),
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
      }));
    }
  } catch (e) {}
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
  
  // Auto-clear sign feedback after 5 seconds
  if (signFeedback && !signing && now - signFeedbackTime > 5000) {
    signFeedback = null;
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
  
  // === BLOCK PROGRESS BAR (top) ===
  const barH = 6;
  const barW = w - m * 2;
  ink(...colors.progressBg).box(m, y, barW, barH);
  ink(...colors.progressFill).box(m, y, Math.floor(barW * blockProgress), barH);
  
  // Block info next to progress (right-aligned)
  if (headBlock) {
    const blockText = `BLOCK ${headBlock.level}`;
    const blockTextWidth = blockText.length * 6; // Approximate width
    ink(...colors.block).write(blockText, { x: w - m - blockTextWidth, y: y - 1 });
  }
  y += barH + 8;
  
  // === NETWORK & PRICE (header row) ===
  if (walletState?.connected) {
    const netColor = walletState.network === "mainnet" ? colors.positive : colors.block;
    ink(...netColor).write(walletState.network?.toUpperCase() || "GHOSTNET", { x: m, y });
  }
  
  if (tezPrice?.usd) {
    ink(...colors.primary).write("ꜩ", { x: w - m - 70, y: y + TEZ_Y_ADJUST }, undefined, undefined, false, "unifont");
    ink(...colors.text).write(`$${tezPrice.usd.toFixed(4)}`, { x: w - m - 58, y });
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
    
    // === BALANCE ===
    ink(...colors.textDim).write("BALANCE", { x: m, y });
    y += 10;
    
    const bal = walletState.balance !== null ? walletState.balance.toFixed(4) : "...";
    ink(...colors.primary).write("ꜩ", { x: m, y: y + TEZ_Y_ADJUST }, undefined, undefined, false, "unifont");
    ink(...colors.primaryBright).write(bal, { x: m + 12, y }, undefined, undefined, false, "unifont");
    
    // USD value inline
    if (walletState.balance !== null && tezPrice?.usd) {
      const usd = (walletState.balance * tezPrice.usd).toFixed(2);
      ink(...colors.textDim).write(`($${usd})`, { x: m + 80, y });
    }
    y += 18;
    
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
        
        ink(...colors.textDim).write(ago, { x: m, y });
        ink(...color).write(`${sign}${tx.amount.toFixed(2)}`, { x: m + 30, y });
        y += 10;
      }
    }
    
    // === CHAIN STATS (bottom area, above buttons) ===
    y = h - 70;
    ink(...colors.textDim).write("CHAIN", { x: m, y });
    y += 10;
    
    if (headBlock) {
      ink(...colors.text).write(`Cycle ${headBlock.cycle}`, { x: m, y });
      
      // Show sync status
      const syncText = headBlock.synced ? "SYNCED" : "SYNCING...";
      const syncColor = headBlock.synced ? colors.positive : colors.block;
      ink(...syncColor).write(syncText, { x: m + 70, y });
    }
    
    // Disconnect button (bottom right)
    disconnectBtn?.paint($, 
      [[50, 30, 40], [120, 70, 90], [180, 150, 170], [50, 30, 40]],
      [[80, 50, 60], [180, 100, 120], [255, 255, 255], [80, 50, 60]]
    );
    
    // Sign Test button (bottom left) - only when connected
    signTestBtn?.reposition({ bottom: 6, left: 6, screen });
    signTestBtn?.paint($,
      signing ? [[60, 60, 40], [140, 140, 80], [220, 220, 160], [60, 60, 40]] : [[40, 50, 80], [80, 120, 180], [160, 200, 255], [40, 50, 80]],
      [[60, 80, 120], [120, 160, 220], [255, 255, 255], [60, 80, 120]]
    );
    
    // Sign feedback (above sign button)
    if (signFeedback) {
      const isSuccess = signFeedback.startsWith("✓");
      const isError = signFeedback.startsWith("✗");
      const feedbackColor = isSuccess ? colors.positive : isError ? colors.negative : colors.primaryBright;
      ink(...feedbackColor).write(signFeedback, { x: m, y: h - 32 });
    }
    
    // QR button (next to sign)
    showQRBtn?.reposition({ bottom: 6, left: 80, screen });
    showQRBtn?.paint($,
      [[60, 40, 80], [120, 80, 160], [200, 160, 255], [60, 40, 80]],
      [[80, 60, 100], [160, 120, 200], [255, 255, 255], [80, 60, 100]]
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
  
  // Time (bottom center when connected to avoid buttons, bottom left otherwise)
  const timeStr = new Date().toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' });
  if (walletState?.connected) {
    ink(...colors.textDim).write(timeStr, { x: w/2, y: h - 10, center: "x" });
  } else {
    ink(...colors.textDim).write(timeStr, { x: m, y: h - 10 });
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
    signTestBtn?.reposition({ bottom: 6, left: 6, screen });
    showQRBtn?.reposition({ bottom: 6, left: 80, screen });
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
    
    // Sign test button
    signTestBtn?.btn.act(e, {
      push: () => {
        if (signing) return; // Prevent double-click
        console.log("🔷 Testing sign...");
        signing = true;
        signFeedback = "Signing...";
        signFeedbackTime = Date.now();
        wallet.sign("Hello from Aesthetic Computer! 🎨", (response) => {
          signing = false;
          if (response.result === "success") {
            // Show truncated signature
            const sig = response.signature;
            signFeedback = `✓ ${sig.slice(0, 12)}...${sig.slice(-8)}`;
          } else {
            signFeedback = `✗ ${response.error || "Failed"}`;
          }
          signFeedbackTime = Date.now();
        });
      }
    });
    
    // QR button - show pairing QR for mobile wallet
    showQRBtn?.btn.act(e, {
      push: () => {
        showQR = true;
        qrCells = null;
        pairingError = null;
        // Request pairing URI from bios
        wallet.getPairingUri("ghostnet", (response) => {
          console.log("🔷 Pairing response:", response);
          if (response.result === "success" && response.pairingInfo) {
            // Generate QR from pairing URI
            try {
              const uri = typeof response.pairingInfo === "string" 
                ? response.pairingInfo 
                : JSON.stringify(response.pairingInfo);
              qrCells = qr(uri).modules;
              console.log("🔷 QR generated for pairing");
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
