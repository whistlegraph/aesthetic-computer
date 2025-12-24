// keep, 2024.12.15
// Mint a piece as a KEEP NFT on Tezos with live timeline feedback.
// Usage: `keep piece-name` or `keep $piece-name`

import { tokenize, KidLisp } from "../lib/kidlisp.mjs";

const { min, max, floor, sin, cos, PI, abs } = Math;

// Keeps contract (mainnet staging)
const KEEPS_CONTRACT = "KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM";
const NETWORK = "mainnet";
// TODO: Set to false when switching to production mainnet contract
const KEEPS_STAGING = true;

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
let sourceCode = null;
let pieceAuthor = null;
let pieceCreatedAt = null; // When piece was created
let pieceSourceLength = null; // Character count

let rotation = 0;
let startTime = null;
let particles = []; // Vegas-style particles

// Already minted state
let alreadyMinted = null; // { tokenId, owner, artifactUri, thumbnailUri, metadataUri, mintedAt, name }
let loadingExisting = false;
let rebaking = false; // True when regenerating bundle for already-minted piece
let rebakeResult = null; // Result from rebake: { artifactUri, thumbnailUri }
let thumbnailBitmap = null; // Loaded thumbnail image (single frame or current frame)
let thumbnailFrames = null; // Array of frames for animated WebP { frames, width, height, loopCount }
let thumbnailFrameIndex = 0; // Current animation frame
let thumbnailLastFrameTime = 0; // Time of last frame change
let kidlispSource = null; // Source code for syntax highlighting
let tickerOffset = 0; // For scrolling ticker

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
    { id: "complete", label: "Mint Complete!", status: "pending", detail: null, time: null, startedAt: null, duration: 500 },
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
let htmlBtn, thumbBtn, metaBtn, networkBtn, rebakeBtn;
let _api, _net, _jump, _store, _needsPaint, _send, _ui, _screen, _preload, _preloadAnimatedWebp;

function boot({ api, net, hud, params, store, cursor, jump, needsPaint, ui, screen, send }) {
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
  networkBtn = new ui.TextButton("GHOSTNET", { x: 0, y: 0, screen });
  rebakeBtn = new ui.TextButton("REBAKE", { x: 0, y: 0, screen });
  htmlBtn.btn.stickyScrubbing = true;
  thumbBtn.btn.stickyScrubbing = true;
  metaBtn.btn.stickyScrubbing = true;
  networkBtn.btn.stickyScrubbing = true;
  rebakeBtn.btn.stickyScrubbing = true;
  
  let rawPiece = params[0] || store["keep:piece"];
  piece = rawPiece?.startsWith("$") ? rawPiece.slice(1) : rawPiece;
  
  if (!piece) {
    setStep("validate", "error", "No piece specified! Usage: keep $piece");
    return;
  }
  
  _net?.getHandle?.().then(h => { userHandle = h; _needsPaint?.(); }).catch(() => {});
  
  // First check if already minted
  checkIfAlreadyMinted();
}

// Convert piece name to hex bytes for TzKT lookup
function stringToBytes(str) {
  return Array.from(new TextEncoder().encode(str))
    .map(b => b.toString(16).padStart(2, '0'))
    .join('');
}

// Check TzKT for existing mint
async function checkIfAlreadyMinted() {
  console.log("🪙 KEEP: Checking if $" + piece + " is already minted...");
  loadingExisting = true;
  _needsPaint?.();
  
  try {
    const keyBytes = stringToBytes(piece);
    const url = `https://api.${NETWORK}.tzkt.io/v1/contracts/${KEEPS_CONTRACT}/bigmaps/content_hashes/keys/${keyBytes}`;
    
    const response = await fetch(url);
    if (response.status === 200) {
      const data = await response.json();
      if (data.active) {
        const existingTokenId = data.value;
        console.log("🪙 KEEP: Already minted as token #" + existingTokenId);
        
        // Fetch token metadata from TzKT
        await fetchExistingTokenInfo(existingTokenId);
        return;
      }
    }
    
    // Not minted - proceed with normal flow
    console.log("🪙 KEEP: Not yet minted, starting mint process...");
    loadingExisting = false;
    runProcess();
    
  } catch (e) {
    console.error("🪙 KEEP: Error checking mint status:", e);
    loadingExisting = false;
    runProcess(); // Proceed anyway
  }
}

// Fetch existing token info from TzKT
async function fetchExistingTokenInfo(existingTokenId) {
  try {
    // Get token metadata
    const metaUrl = `https://api.${NETWORK}.tzkt.io/v1/tokens?contract=${KEEPS_CONTRACT}&tokenId=${existingTokenId}`;
    const metaRes = await fetch(metaUrl);
    
    let tokenData = {};
    if (metaRes.ok) {
      const tokens = await metaRes.json();
      if (tokens.length > 0) {
        tokenData = tokens[0];
      }
    }
    
    // Get owner from ledger
    const ledgerUrl = `https://api.${NETWORK}.tzkt.io/v1/contracts/${KEEPS_CONTRACT}/bigmaps/ledger/keys/${existingTokenId}`;
    const ledgerRes = await fetch(ledgerUrl);
    let ownerAddress = null;
    if (ledgerRes.ok) {
      const ledgerData = await ledgerRes.json();
      if (ledgerData.active) {
        ownerAddress = ledgerData.value;
      }
    }
    
    // Parse IPFS URIs from metadata (TzKT may use different field names)
    const meta = tokenData.metadata || {};
    
    // TzKT might store URIs as thumbnailUri, thumbnail_uri, or displayUri
    const thumbnailUri = meta.thumbnailUri || meta.thumbnail_uri || meta.displayUri || meta.display_uri;
    const artifactUri = meta.artifactUri || meta.artifact_uri;
    
    alreadyMinted = {
      tokenId: existingTokenId,
      owner: ownerAddress,
      name: meta.name || `$${piece}`,
      description: meta.description,
      artifactUri: artifactUri,
      thumbnailUri: thumbnailUri,
      creators: meta.creators,
      mintedAt: tokenData.firstTime || tokenData.lastTime,
      network: NETWORK, // ghostnet or mainnet
      objktUrl: `https://${NETWORK === "mainnet" ? "" : "ghostnet."}objkt.com/asset/${KEEPS_CONTRACT}/${existingTokenId}`,
      tzktUrl: `https://${NETWORK}.tzkt.io/${KEEPS_CONTRACT}/tokens/${existingTokenId}`,
    };
    
    loadingExisting = false;
    console.log("🪙 KEEP: Loaded existing token info:", alreadyMinted);
    console.log("🪙 KEEP: Raw metadata from TzKT:", meta);
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
      network: NETWORK, // ghostnet or mainnet
      objktUrl: `https://${NETWORK === "mainnet" ? "" : "ghostnet."}objkt.com/asset/${KEEPS_CONTRACT}/${existingTokenId}`,
      tzktUrl: `https://${NETWORK}.tzkt.io/${KEEPS_CONTRACT}/tokens/${existingTokenId}`,
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
      thumbnailLastFrameTime = performance.now();
      
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
        kidlispSource = data.source;
        console.log("🪙 KEEP: Loaded KidLisp source:", kidlispSource.slice(0, 100) + "...");
        _needsPaint?.();
      }
    }
  } catch (e) {
    console.error("🪙 KEEP: Error loading KidLisp source:", e);
  }
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
  
  // === STEP 1: Connect Wallet ===
  // Reset timer when process actually starts (not counting initial checks)
  startTime = Date.now();
  
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
    });
    
    if (!response.ok) {
      setStep("validate", "error", `Server error ${response.status}`);
      return;
    }
    
    const text = await response.text();
    const events = text.split('\n\n').filter(e => e.trim());
    
    // Process events with staggered delays so user sees each step
    const delay = (ms) => new Promise(r => setTimeout(r, ms));
    const STEP_DELAY = 350; // ms between each visual step
    let firstEvent = true;
    
    for (const event of events) {
      const lines = event.split('\n');
      let eventType = null, eventData = null;
      
      for (const line of lines) {
        if (line.startsWith('event: ')) eventType = line.slice(7);
        else if (line.startsWith('data: ')) {
          try { eventData = JSON.parse(line.slice(6)); } catch {}
        }
      }
      
      if (eventType === "progress" && eventData?.stage) {
        const { stage, message, source, author } = eventData;
        console.log(`🪙 KEEP SSE: ${stage}`, eventData);
        
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
          if (eventData.source) sourceCode = eventData.source;
          if (eventData.author) pieceAuthor = eventData.author;
          if (eventData.createdAt) pieceCreatedAt = eventData.createdAt;
          if (eventData.sourceLength) pieceSourceLength = eventData.sourceLength;
          console.log("🪙 KEEP: Piece details received", { pieceAuthor, pieceCreatedAt, pieceSourceLength });
        } else if (stage === "analyze") {
          if (source) sourceCode = source;
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
          const detail = isCached ? "Cached" : message || "Pinned";
          setStep("ipfs", "done", detail);
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
        preparedData = eventData;
        await delay(STEP_DELAY);
        setStep("review", "active", null); // No detail text - button speaks for itself
        // Load thumbnail for preview during minting
        if (preparedData.thumbnailUri) {
          loadThumbnail(preparedData.thumbnailUri);
        }
      } else if (eventType === "error" && eventData) {
        const active = getActiveStep();
        if (active) setStep(active.id, "error", eventData.error || eventData.message);
        return;
      }
    }
    
    if (!preparedData) {
      setStep("validate", "error", "Server did not return prepared data");
    }
    
  } catch (e) {
    const active = getActiveStep();
    if (active) setStep(active.id, "error", e.message);
  }
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
    const networkPrefix = preparedData.network === "mainnet" ? "" : "ghostnet.";
    const apiBase = `https://api.${networkPrefix}tzkt.io`;
    
    for (let attempt = 0; attempt < 5 && !tokenId; attempt++) {
      await new Promise(r => setTimeout(r, 3000));
      try {
        const res = await fetch(`${apiBase}/v1/tokens?contract=${preparedData.contractAddress}&sort.desc=id&limit=1`);
        if (res.ok) {
          const tokens = await res.json();
          if (tokens.length > 0) {
            tokenId = tokens[0].tokenId;
            console.log(`🪙 KEEP: Found token #${tokenId} on attempt ${attempt + 1}`);
          }
        }
      } catch (e) {
        console.warn(`🪙 KEEP: Token fetch attempt ${attempt + 1} failed:`, e.message);
      }
      if (!tokenId && attempt < 4) {
        setStep("complete", "active", `Waiting for indexer... (${attempt + 2}/5)`);
      }
    }
    
    const tokenInfo = tokenId ? `Token #${tokenId}` : "Minted!";
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
      ? [[100, 255, 150], [150, 255, 200], [200, 255, 100]] // Green success
      : progress > 0.5 
        ? [[255, 200, 100], [255, 180, 80], [255, 220, 150]] // Yellow/orange
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
    const now = performance.now();
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
  
  // Always animate for already minted view, or when minting is active
  return alreadyMinted || (!hasError() && timeline.some(t => t.status === "active"));
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
const PHASE_COLORS = {
  wallet: { stripe: [60, 40, 80], label: [180, 140, 220], detail: [140, 100, 180] },      // Purple - auth
  validate: { stripe: [50, 50, 70], label: [150, 150, 200], detail: [110, 110, 160] },    // Blue-gray - verify
  analyze: { stripe: [50, 50, 70], label: [150, 150, 200], detail: [110, 110, 160] },     // Blue-gray - verify
  thumbnail: { stripe: [70, 50, 40], label: [220, 160, 120], detail: [180, 120, 80] },    // Orange - generate
  bundle: { stripe: [70, 50, 40], label: [220, 160, 120], detail: [180, 120, 80] },       // Orange - generate
  ipfs: { stripe: [40, 60, 60], label: [120, 200, 200], detail: [80, 160, 160] },         // Cyan - upload
  metadata: { stripe: [40, 60, 60], label: [120, 200, 200], detail: [80, 160, 160] },     // Cyan - upload
  review: { stripe: [60, 60, 40], label: [220, 220, 140], detail: [180, 180, 100] },      // Yellow - confirm
  sign: { stripe: [60, 60, 40], label: [220, 220, 140], detail: [180, 180, 100] },        // Yellow - confirm
  complete: { stripe: [40, 70, 50], label: [140, 255, 180], detail: [100, 200, 140] },    // Green - done
};

function paint({ wipe, ink, box, screen, paste }) {
  const w = screen.width, h = screen.height;
  
  // === ALREADY MINTED VIEW ===
  if (alreadyMinted) {
    // Dark teal background with shimmer
    const pulse = sin(rotation * 1.5) * 5;
    wipe(20 + pulse, 35 + pulse, 40 + pulse);
    
    // Gold shimmer border
    for (let i = 0; i < w; i += 3) {
      const shimmer = sin(rotation * 2 + i * 0.2) * 0.5 + 0.5;
      const alpha = floor(shimmer * 80);
      ink(255, 200, 100, alpha).box(i, 0, 2, 1);
      ink(255, 200, 100, alpha).box(i, h - 1, 2, 1);
    }
    
    const margin = 6;
    let y = 4;
    
    // Header with network badge
    const netLabel = (alreadyMinted.network || NETWORK).toUpperCase();
    const isMainnet = netLabel === "MAINNET";
    const netColor = isMainnet ? [100, 220, 100] : [220, 180, 100]; // green for mainnet, amber for testnet
    
    ink(255, 220, 100).write("ALREADY KEPT", { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    y += 10;
    // Network badge
    ink(netColor[0], netColor[1], netColor[2]).write(`on ${netLabel}`, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    y += 10;
    ink(100, 220, 180).write(`$${piece}`, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
    y += 14;
    
    // Thumbnail display
    if (thumbnailBitmap) {
      const thumbSize = min(64, w - 20, h - 100);
      const thumbX = floor((w - thumbSize) / 2);
      paste(thumbnailBitmap, thumbX, y, { scale: thumbSize / (thumbnailBitmap.width || 256) });
      y += thumbSize + 6;
    } else if (alreadyMinted.thumbnailUri) {
      // Show placeholder while loading
      ink(40, 55, 60).box(floor((w - 50) / 2), y, 50, 40);
      ink(80, 100, 110).write("Loading...", { x: w/2, y: y + 15, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 46;
    } else {
      // No thumbnail available - show a simple placeholder
      ink(40, 55, 60).box(floor((w - 50) / 2), y, 50, 40);
      ink(60, 80, 85).write(`$${piece}`, { x: w/2, y: y + 15, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 46;
    }
    
    // Token info stripe
    ink(40, 55, 60, 180).box(0, y, w, 22);
    ink(180, 220, 200).write(`Token #${alreadyMinted.tokenId}`, { x: margin, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
    
    // Owner (truncated address)
    if (alreadyMinted.owner) {
      const ownerShort = alreadyMinted.owner.slice(0, 8) + "..." + alreadyMinted.owner.slice(-4);
      ink(120, 150, 140).write(ownerShort, { x: margin, y: y + 12 }, undefined, undefined, false, "MatrixChunky8");
    }
    y += 26;
    
    // Syntax-highlighted source code ticker
    if (kidlispSource) {
      // Ticker background
      ink(25, 35, 40, 200).box(0, y, w, 14);
      
      // Build colored string with natural spacing
      const coloredSource = buildColoredSourceString(kidlispSource);
      const sourceLen = kidlispSource.length * 4;
      
      // Add gap between repeats
      const gap = 40;
      const repeatWidth = sourceLen + gap;
      
      // Calculate starting x position (scrolling)
      let startX = -(tickerOffset % repeatWidth);
      
      // Draw colored source using write() for natural spacing
      for (let repeat = 0; repeat < 3; repeat++) {
        const x = startX + repeat * repeatWidth;
        if (x < w && x + sourceLen > 0) {
          ink(200, 200, 200).write(coloredSource, { x, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
        }
      }
      y += 18;
    }
    
    // IPFS link buttons - styled like products.mjs MatrixChunky8 buttons
    y += 4;
    let linkX = margin;
    const linkScheme = {
      normal: { bg: [30, 60, 70], outline: [100, 180, 220], outlineAlpha: 150, text: [100, 180, 220] },
      hover: { bg: [50, 90, 110], outline: [180, 240, 255], outlineAlpha: 200, text: [180, 240, 255] },
      disabled: { bg: [25, 35, 40], outline: [60, 80, 90], outlineAlpha: 100, text: [60, 80, 90] }
    };
    
    if (alreadyMinted.artifactUri) {
      const htmlSize = mc8ButtonSize("HTML");
      htmlBtn.btn.box.x = linkX;
      htmlBtn.btn.box.y = y;
      htmlBtn.btn.box.w = htmlSize.w;
      htmlBtn.btn.box.h = htmlSize.h;
      paintMC8Btn(linkX, y, "HTML", { ink, line: ink }, linkScheme, htmlBtn.btn.down);
      linkX += htmlSize.w + 4;
    }
    if (alreadyMinted.thumbnailUri) {
      const thumbSize = mc8ButtonSize("THUMB");
      thumbBtn.btn.box.x = linkX;
      thumbBtn.btn.box.y = y;
      thumbBtn.btn.box.w = thumbSize.w;
      thumbBtn.btn.box.h = thumbSize.h;
      paintMC8Btn(linkX, y, "THUMB", { ink, line: ink }, linkScheme, thumbBtn.btn.down);
      linkX += thumbSize.w + 4;
    }
    y += mc8ButtonSize("X").h + 6;
    
    // Rebake status display
    if (rebaking) {
      ink(255, 200, 100).write("Regenerating bundle...", { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 12;
    } else if (rebakeResult) {
      ink(100, 255, 150).write("✓ Bundle regenerated!", { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 10;
      // Show new IPFS hash (shortened)
      const hash = rebakeResult.artifactUri?.replace("ipfs://", "").slice(0, 20) + "...";
      ink(180, 180, 180).write("New: " + hash, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
      y += 14;
    }
    
    // Action buttons
    // View on objkt button - styled with proper MatrixChunky8 padding
    const objktScheme = {
      normal: { bg: [40, 80, 70], outline: [100, 200, 160], outlineAlpha: 150, text: [255, 255, 255] },
      hover: { bg: [60, 120, 100], outline: [150, 255, 200], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [35, 50, 45], outline: [70, 130, 100], outlineAlpha: 100, text: [140, 160, 150] }
    };
    const objktSize = mc8ButtonSize("View on objkt");
    const objktX = floor((w - objktSize.w) / 2);
    btn.btn.box.x = objktX;
    btn.btn.box.y = y;
    btn.btn.box.w = objktSize.w;
    btn.btn.box.h = objktSize.h;
    paintMC8Btn(objktX, y, "View on objkt", { ink, line: ink }, objktScheme, btn.btn.down);
    y += objktSize.h + 4;
    
    // Rebake button - regenerate bundle without re-minting
    const rebakeScheme = {
      normal: { bg: [80, 50, 30], outline: [200, 140, 80], outlineAlpha: 150, text: [255, 200, 100] },
      hover: { bg: [120, 70, 40], outline: [255, 180, 100], outlineAlpha: 200, text: [255, 220, 150] },
      disabled: { bg: [40, 30, 25], outline: [100, 80, 60], outlineAlpha: 100, text: [120, 100, 80] }
    };
    const rebakingScheme = {
      normal: { bg: [40, 30, 25], outline: [100, 80, 60], outlineAlpha: 100, text: [120, 100, 80] },
      hover: { bg: [40, 30, 25], outline: [100, 80, 60], outlineAlpha: 100, text: [120, 100, 80] },
      disabled: { bg: [40, 30, 25], outline: [100, 80, 60], outlineAlpha: 100, text: [120, 100, 80] }
    };
    const rebakeSize = mc8ButtonSize("Rebake Bundle");
    const rebakeX = floor((w - rebakeSize.w) / 2);
    rebakeBtn.btn.box.x = rebakeX;
    rebakeBtn.btn.box.y = y;
    rebakeBtn.btn.box.w = rebakeSize.w;
    rebakeBtn.btn.box.h = rebakeSize.h;
    paintMC8Btn(rebakeX, y, rebaking ? "Rebaking..." : "Rebake Bundle", { ink, line: ink }, rebaking ? rebakingScheme : rebakeScheme, rebakeBtn.btn.down);
    
    return;
  }
  
  // === LOADING VIEW ===
  if (loadingExisting) {
    wipe(30, 35, 45);
    ink(100, 150, 180).write("Checking chain...", { x: w/2, y: h/2, center: "xy" }, undefined, undefined, false, "MatrixChunky8");
    return;
  }
  
  // === MINTING VIEW (original) ===
  
  // Animated background color based on progress
  const progress = getProgress();
  const isComplete = timeline.find(t => t.id === "complete")?.status === "done";
  const isError = hasError();
  
  let bgR, bgG, bgB;
  if (isError) {
    bgR = 45; bgG = 20; bgB = 25;
  } else if (isComplete) {
    const pulse = sin(rotation * 2) * 8;
    bgR = 20 + pulse; bgG = 45 + pulse; bgB = 30 + pulse;
  } else {
    if (progress < 0.5) {
      const t = progress * 2;
      bgR = floor(35 + t * 25); bgG = floor(25 + t * 20); bgB = floor(40 - t * 15);
    } else {
      const t = (progress - 0.5) * 2;
      bgR = floor(60 - t * 35); bgG = floor(45 + t * 10); bgB = floor(25 + t * 10);
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
      const color = isComplete ? [100, 255, 150] : [255, 200, 100];
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
  ink(30, 35, 45).box(0, barY, w, barH);
  
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
    ink(50, 55, 65).box(segmentEnd - 1, barY, 1, barH);
  }
  y += barH + 4;
  
  // === STEP CAROUSEL (prev | CURRENT | next) ===
  const carouselH = 20;
  const carouselY = y;
  ink(25, 30, 40).box(0, carouselY, w, carouselH); // Dark bg for carousel
  
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
      labelColor = [255, 100, 100];
      labelAlpha = floor(200 * opacity);
    } else {
      labelColor = [80, 90, 110];
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
    ink(100, 110, 130, 150).write("<", { x: 4, y: carouselY + 6 }, undefined, undefined, false, "MatrixChunky8");
  }
  if (carouselTargetIndex < totalSteps - 1) {
    ink(100, 110, 130, 150).write(">", { x: w - 8, y: carouselY + 6 }, undefined, undefined, false, "MatrixChunky8");
  }
  
  y += carouselH + 2;
  
  // Step counter (small, centered)
  const stepText = `${doneCount}/${totalSteps}`;
  ink(100, 110, 130).write(stepText, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
  y += 10;

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
    if (isPending) labelColor = [45, 50, 60];
    else if (isItemError) labelColor = [255, 120, 120];
    else if (isActive) labelColor = phase.label;
    else labelColor = phase.label.map(c => floor(c * 0.7));
    
    ink(...labelColor).write(item.label, { x: margin, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
    
    // Time (right aligned)
    if (item.time && (isDone || isActive || isItemError)) {
      const timeColor = isItemError ? [180, 80, 80] : (isActive ? [200, 200, 140] : [80, 90, 100]);
      ink(...timeColor).write(item.time, { x: w - margin - 20, y: y + 2 }, undefined, undefined, false, "MatrixChunky8");
    }
    
    // Detail line (with better contrast) - skip for review step when active (has custom button UI)
    if (item.detail && !isPending && !(item.id === "review" && isActive && preparedData)) {
      let detailColor;
      if (isItemError) detailColor = [255, 120, 120];
      else if (isActive) detailColor = [255, 255, 180]; // Bright white-yellow for active
      else detailColor = [200, 210, 220]; // Light gray for done
      
      const maxLen = floor((w - 20) / 4);
      const truncated = item.detail.length > maxLen ? item.detail.slice(0, maxLen - 2) + ".." : item.detail;
      // Darker background strip for better readability
      ink(0, 0, 0, 180).box(margin - 2, y + 10, truncated.length * 4 + 4, 10);
      ink(...detailColor).write(truncated, { x: margin, y: y + 11 }, undefined, undefined, false, "MatrixChunky8");
    }
    
    y += stripeH + 2; // Gap between stripes
    
    // === INLINE SOURCE MARQUEE (after analyze row) ===
    if (item.id === "analyze" && (isDone || isActive) && sourceCode) {
      const marqueeH = 12;
      ink(35, 40, 50).box(0, y, w, marqueeH);
      
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
        ink(20, 25, 35).box(w - statsW - margin - 4, y, statsW + 8, marqueeH); // bg to cover ticker
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
      ink(30, 35, 45).box(0, y, w, thumbH);
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
        const rebakeScheme = {
          normal: { bg: [80, 60, 40], outline: [255, 180, 100], outlineAlpha: 150, text: [255, 200, 140] },
          hover: { bg: [120, 90, 60], outline: [255, 220, 150], outlineAlpha: 200, text: [255, 240, 200] },
          disabled: { bg: [50, 45, 40], outline: [120, 100, 80], outlineAlpha: 100, text: [140, 120, 100] }
        };
        const rebakeSize = mc8ButtonSize("REBAKE");
        const rebakeX = infoX;
        const rebakeY = y + 20;
        rebakeBtn.btn.box.x = rebakeX;
        rebakeBtn.btn.box.y = rebakeY;
        rebakeBtn.btn.box.w = rebakeSize.w;
        rebakeBtn.btn.box.h = rebakeSize.h;
        paintMC8Btn(rebakeX, rebakeY, "REBAKE", { ink, line: ink }, rebakeScheme, rebakeBtn.btn.down);
      }
      y += thumbH + 2;
    }
    
    // IPFS links after metadata (inline text buttons)
    if (item.id === "metadata" && isDone && preparedData) {
      let linkX = margin;
      const linkScheme = {
        normal: { bg: [30, 60, 70], outline: [100, 180, 220], outlineAlpha: 150, text: [100, 180, 220] },
        hover: { bg: [50, 90, 110], outline: [180, 240, 255], outlineAlpha: 200, text: [180, 240, 255] },
        disabled: { bg: [25, 35, 40], outline: [60, 80, 90], outlineAlpha: 100, text: [60, 80, 90] }
      };
      
      if (preparedData.artifactUri) {
        const htmlSize = mc8ButtonSize("HTML");
        htmlBtn.btn.box.x = linkX;
        htmlBtn.btn.box.y = y;
        htmlBtn.btn.box.w = htmlSize.w;
        htmlBtn.btn.box.h = htmlSize.h;
        paintMC8Btn(linkX, y, "HTML", { ink, line: ink }, linkScheme, htmlBtn.btn.down);
        linkX += htmlSize.w + 4;
      }
      if (preparedData.thumbnailUri) {
        const thumbSize = mc8ButtonSize("THUMB");
        thumbBtn.btn.box.x = linkX;
        thumbBtn.btn.box.y = y;
        thumbBtn.btn.box.w = thumbSize.w;
        thumbBtn.btn.box.h = thumbSize.h;
        paintMC8Btn(linkX, y, "THUMB", { ink, line: ink }, linkScheme, thumbBtn.btn.down);
        linkX += thumbSize.w + 4;
      }
      if (preparedData.metadataUri) {
        const metaSize = mc8ButtonSize("META");
        metaBtn.btn.box.x = linkX;
        metaBtn.btn.box.y = y;
        metaBtn.btn.box.w = metaSize.w;
        metaBtn.btn.box.h = metaSize.h;
        paintMC8Btn(linkX, y, "META", { ink, line: ink }, linkScheme, metaBtn.btn.down);
      }
      y += mc8ButtonSize("X").h + 4;
    }
    
    // === INLINE KEEP TOLL BUTTON (in review row) ===
    if (item.id === "review" && isActive && preparedData) {
      // Skip the normal detail rendering for review - button is the UI
      const tollH = 24; // Bigger button
      ink(40, 60, 45).box(0, y, w, tollH); // Darker green-tinted bg
      
      // Pay toll button centered (mixed font for ꜩ)
      const tollText = `Pay ${preparedData.mintFee || 5}`;
      const tezSymbol = "ꜩ";
      const tollSuffix = " Toll";
      const textW = tollText.length * 6; // default font
      const symbolW = 8;
      const suffixW = tollSuffix.length * 6; // default font
      const totalTollW = textW + symbolW + suffixW + 16; // padding
      const tollX = floor((w - totalTollW) / 2);
      
      // Button background
      const tollScheme = btn.btn.down ? { bg: [80, 150, 100], text: [255, 255, 255] } : { bg: [60, 120, 80], text: [200, 255, 220] };
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
    }
  }
  
  // === Network badge in TOP RIGHT (when review active) ===
  const reviewStep = timeline.find(t => t.id === "review");
  if (reviewStep?.status === "active" && preparedData) {
    const baseNet = (preparedData.network || "mainnet").toUpperCase();
    const netLabel = KEEPS_STAGING && baseNet === "MAINNET" ? "MAINNET (STAGING)" : baseNet;
    const isGhostnet = baseNet === "GHOSTNET";
    const ghostW = isGhostnet ? 16 : 0; // Space for ghost icon
    const netW = netLabel.length * 4 + 8 + ghostW;
    const netX = w - margin - netW;
    const netY = 4; // Top of screen
    const netScheme = networkBtn.btn.down ? { bg: [50, 70, 80], text: [160, 200, 220] } : { bg: [35, 50, 60], text: [100, 140, 160] };
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
    const viewScheme = {
      normal: { bg: [40, 80, 60], outline: [100, 220, 150], outlineAlpha: 150, text: [255, 255, 255] },
      hover: { bg: [60, 120, 90], outline: [150, 255, 200], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [35, 50, 45], outline: [70, 130, 100], outlineAlpha: 100, text: [140, 160, 150] }
    };
    const viewSize = mc8ButtonSize("View on objkt");
    const viewX = floor((w - viewSize.w) / 2);
    btn.btn.box.x = viewX;
    btn.btn.box.y = y;
    btn.btn.box.w = viewSize.w;
    btn.btn.box.h = viewSize.h;
    btn.txt = "View on objkt";
    paintMC8Btn(viewX, y, "View on objkt", { ink, line: ink }, viewScheme, btn.btn.down);
    
  } else if (isError) {
    const retryScheme = {
      normal: { bg: [100, 50, 50], outline: [255, 120, 120], outlineAlpha: 150, text: [255, 255, 255] },
      hover: { bg: [140, 70, 70], outline: [255, 160, 160], outlineAlpha: 200, text: [255, 255, 255] },
      disabled: { bg: [60, 40, 40], outline: [120, 80, 80], outlineAlpha: 100, text: [140, 120, 120] }
    };
    const retrySize = mc8ButtonSize("Retry");
    const retryX = floor((w - retrySize.w) / 2);
    btn.btn.box.x = retryX;
    btn.btn.box.y = y;
    btn.btn.box.w = retrySize.w;
    btn.btn.box.h = retrySize.h;
    btn.txt = "Retry";
    paintMC8Btn(retryX, y, "Retry", { ink, line: ink }, retryScheme, btn.btn.down);
  }
}

function act({ event: e, screen }) {
  if (e.is("reframed")) _needsPaint?.();
  
  const reviewStep = timeline.find(t => t.id === "review");
  const completeStep = timeline.find(t => t.id === "complete");
  
  // Asset link buttons + network button + rebake
  if (reviewStep?.status === "active" && preparedData) {
    if (preparedData.artifactUri) htmlBtn.btn.act(e, { push: () => openUrl(preparedData.artifactUri) });
    if (preparedData.thumbnailUri) thumbBtn.btn.act(e, { push: () => openUrl(preparedData.thumbnailUri) });
    if (preparedData.metadataUri) metaBtn.btn.act(e, { push: () => openUrl(preparedData.metadataUri) });
    
    // REBAKE button - regenerate media from scratch
    if (preparedData.usedCachedMedia) {
      rebakeBtn.btn.act(e, { push: () => {
        console.log("🪙 KEEP: Rebaking media...");
        resetTimeline();
        startTime = Date.now();
        preparedData = null;
        thumbnailBitmap = null;
        thumbnailFrames = null;
        runProcess(true); // Pass true to force regeneration
      }});
    }
    
    // Network button links to contract collection on objkt
    const contractAddress = preparedData.contractAddress || KEEPS_CONTRACT;
    const networkPrefix = preparedData.network === "mainnet" ? "" : "ghostnet.";
    networkBtn.btn.act(e, { push: () => openUrl(`https://${networkPrefix}objkt.com/collection/${contractAddress}`) });
    
    btn.btn.act(e, { push: () => signAndMint() });
  }
  
  if (completeStep?.status === "done") {
    btn.btn.act(e, { push: () => {
      const networkPrefix = preparedData?.network === "mainnet" ? "" : "ghostnet.";
      const url = tokenId 
        ? `https://${networkPrefix}objkt.com/tokens/${preparedData.contractAddress}/${tokenId}`
        : `https://${networkPrefix}objkt.com/collections/${preparedData.contractAddress}`;
      openUrl(url);
    }});
  }
  
  if (hasError()) {
    btn.btn.act(e, { push: () => {
      resetTimeline();
      startTime = Date.now();
      runProcess();
    }});
  }
  
  // Already minted view interactions
  if (alreadyMinted) {
    if (alreadyMinted.artifactUri) htmlBtn.btn.act(e, { push: () => openUrl(alreadyMinted.artifactUri) });
    if (alreadyMinted.thumbnailUri) thumbBtn.btn.act(e, { push: () => openUrl(alreadyMinted.thumbnailUri) });
    btn.btn.act(e, { push: () => openUrl(alreadyMinted.objktUrl) });
    
    // Rebake button - regenerate bundle and thumbnail
    if (!rebaking) {
      rebakeBtn.btn.act(e, { push: async () => {
        console.log("🪙 KEEP: Starting rebake for already-minted piece $" + piece);
        rebaking = true;
        rebakeResult = null;
        _needsPaint?.();
        
        try {
          const response = await fetch("/api/keep-mint", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
              "Authorization": `Bearer ${_store["login-token"]}`,
            },
            body: JSON.stringify({
              piece: "$" + piece,
              mode: "prepare",
              regenerate: true,
              screenWidth: _screen?.width || 128,
              screenHeight: _screen?.height || 128,
            }),
          });
          
          // Read full response and parse SSE events
          const text = await response.text();
          const events = text.split('\n\n').filter(e => e.trim());
          
          for (const event of events) {
            const lines = event.split('\n');
            let eventType = null, eventData = null;
            
            for (const line of lines) {
              if (line.startsWith('event: ')) eventType = line.slice(7);
              else if (line.startsWith('data: ')) {
                try { eventData = JSON.parse(line.slice(6)); } catch {}
              }
            }
            
            console.log("🪙 REBAKE:", eventType, eventData);
            
            if (eventType === "ready" && eventData) {
              rebakeResult = {
                artifactUri: eventData.artifactUri,
                thumbnailUri: eventData.thumbnailUri,
                metadataUri: eventData.metadataUri,
              };
              // Update alreadyMinted with new URIs for display
              alreadyMinted.artifactUri = eventData.artifactUri;
              alreadyMinted.thumbnailUri = eventData.thumbnailUri;
              console.log("🪙 REBAKE complete! New artifact:", rebakeResult.artifactUri);
            } else if (eventType === "error") {
              console.error("🪙 REBAKE error:", eventData);
            }
          }
        } catch (err) {
          console.error("🪙 REBAKE failed:", err);
        } finally {
          rebaking = false;
          _needsPaint?.();
        }
      }});
    }
  }
  
  if (e.is("keyboard:down:enter")) {
    if (alreadyMinted) openUrl(alreadyMinted.objktUrl);
    else if (reviewStep?.status === "active" && preparedData) signAndMint();
    else if (hasError()) { resetTimeline(); startTime = Date.now(); runProcess(); }
  }
  
  if (e.is("keyboard:down:escape")) _jump?.("prompt");
}

export { boot, paint, sim, act };
