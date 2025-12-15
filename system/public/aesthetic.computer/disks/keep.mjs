// keep, 2024.12.15
// Mint a piece as a KEEP NFT on Tezos with live timeline feedback.
// Usage: `keep piece-name` or `keep $piece-name`

import { tokenize } from "../lib/kidlisp.mjs";

const { min, max, floor, sin, cos, PI, abs } = Math;

// Keeps contract address on ghostnet
const KEEPS_CONTRACT = "KT1NeytR5BHDfGBjG9ZuLkPd7nmufmH1icVc";
const NETWORK = "ghostnet";

let piece;
let walletAddress = null;
let preparedData = null;
let txHash = null;
let tokenId = null;
let userHandle = null;
let sourceCode = null;
let pieceAuthor = null;

let rotation = 0;
let startTime = null;
let particles = []; // Vegas-style particles

// Already minted state
let alreadyMinted = null; // { tokenId, owner, artifactUri, thumbnailUri, metadataUri, mintedAt, name }
let loadingExisting = false;
let thumbnailBitmap = null; // Loaded thumbnail image (single frame or current frame)
let thumbnailFrames = null; // Array of frames for animated WebP { frames, width, height, loopCount }
let thumbnailFrameIndex = 0; // Current animation frame
let thumbnailLastFrameTime = 0; // Time of last frame change
let kidlispSource = null; // Source code for syntax highlighting
let tickerOffset = 0; // For scrolling ticker

// Timeline - the heart of the UX
let timeline = [];

function resetTimeline() {
  timeline = [
    { id: "wallet", label: "Connect Wallet", status: "pending", detail: null, time: null, startedAt: null, duration: 500 },
    { id: "validate", label: "Validate Piece", status: "pending", detail: null, time: null, startedAt: null, duration: 2000 },
    { id: "analyze", label: "Analyze Source", status: "pending", detail: null, time: null, startedAt: null, duration: 1500 },
    { id: "thumbnail", label: "Generate Preview", status: "pending", detail: null, time: null, startedAt: null, duration: 8000 },
    { id: "bundle", label: "Bundle Assets", status: "pending", detail: null, time: null, startedAt: null, duration: 3000 },
    { id: "ipfs", label: "Upload to IPFS", status: "pending", detail: null, time: null, startedAt: null, duration: 5000 },
    { id: "metadata", label: "Create Metadata", status: "pending", detail: null, time: null, startedAt: null, duration: 2000 },
    { id: "review", label: "Review & Confirm", status: "pending", detail: null, time: null, startedAt: null, duration: null },
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
let htmlBtn, thumbBtn, metaBtn, networkBtn;
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
  htmlBtn.btn.stickyScrubbing = true;
  thumbBtn.btn.stickyScrubbing = true;
  metaBtn.btn.stickyScrubbing = true;
  networkBtn.btn.stickyScrubbing = true;
  
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

// Syntax highlight KidLisp source code
function syntaxHighlightKidlisp(source) {
  if (!source) return [];
  
  try {
    const tokens = tokenize(source);
    const highlighted = [];
    
    // Color mapping for different token types
    const isFunction = (t) => ["wipe", "ink", "line", "box", "circle", "write", "paste", 
      "stamp", "point", "poly", "embed", "def", "if", "cond", "repeat", "later", "once",
      "lambda", "let", "do", "+", "-", "*", "/", "%", "=", ">", "<", "sin", "cos", "tan",
      "floor", "ceil", "round", "random", "abs", "sqrt", "min", "max", "now", "frame",
      "width", "height", "tap", "draw", "bake", "jump", "wiggle", "rainbow", "zoom"].includes(t);
    
    const isNumber = (t) => /^-?\d+(\.\d+)?$/.test(t);
    const isString = (t) => (t.startsWith('"') && t.endsWith('"')) || (t.startsWith("'") && t.endsWith("'"));
    const isColor = (t) => ["red", "green", "blue", "yellow", "orange", "purple", "pink",
      "cyan", "magenta", "white", "black", "gray", "lime", "teal", "navy", "maroon"].includes(t);
    const isTiming = (t) => /^\d*\.?\d+s\.?\.?\.?!?$/.test(t);
    
    for (const token of tokens) {
      let color;
      if (token === "(" || token === ")") {
        color = [150, 150, 150]; // Gray parens
      } else if (isFunction(token)) {
        color = [100, 220, 255]; // Cyan for functions
      } else if (isNumber(token)) {
        color = [255, 200, 100]; // Orange for numbers
      } else if (isString(token)) {
        color = [150, 255, 150]; // Green for strings
      } else if (isColor(token)) {
        color = [255, 150, 255]; // Pink for color names
      } else if (isTiming(token)) {
        color = [255, 255, 100]; // Yellow for timing
      } else if (token.startsWith("$")) {
        color = [100, 255, 180]; // Teal for $codes
      } else {
        color = [200, 200, 220]; // Light gray for variables
      }
      highlighted.push({ token, color });
    }
    
    return highlighted;
  } catch (e) {
    console.error("🪙 KEEP: Syntax highlight error:", e);
    return [{ token: source, color: [200, 200, 200] }];
  }
}

async function runProcess() {
  console.log("🪙 KEEP: Starting mint process for $" + piece);
  
  // === STEP 1: Connect Wallet ===
  setStep("wallet", "active", "Checking wallet...");
  try {
    walletAddress = await _api.tezos.address();
    if (!walletAddress) {
      setStep("wallet", "active", "Opening Temple Wallet...");
      walletAddress = await _api.tezos.connect("ghostnet");
    }
    if (!walletAddress) {
      setStep("wallet", "error", "Connection cancelled");
      return;
    }
    setStep("wallet", "done", walletAddress);
  } catch (e) {
    setStep("wallet", "error", e.message);
    return;
  }
  
  // === STEPS 2-7: Server Preparation ===
  setStep("validate", "active", `Validating $${piece}...`);
  
  try {
    const token = await _net?.getToken?.();
    const response = await fetch("/api/keep-mint", {
      method: "POST",
      headers: { 
        "Content-Type": "application/json",
        ...(token ? { "Authorization": `Bearer ${token}` } : {}),
      },
      body: JSON.stringify({ piece, walletAddress, network: "ghostnet" }),
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
        
        if (stage === "validate") {
          setStep("validate", "done", `$${piece} exists`);
          await delay(STEP_DELAY);
          setStep("analyze", "active", "Reading source...");
        } else if (stage === "analyze") {
          if (source) sourceCode = source;
          if (author) pieceAuthor = author;
          const authorInfo = pieceAuthor ? `by @${pieceAuthor}` : "Source analyzed";
          setStep("analyze", "done", authorInfo);
          await delay(STEP_DELAY);
          setStep("thumbnail", "active", "Rendering preview...");
        } else if (stage === "thumbnail") {
          setStep("thumbnail", "done", "256x256 PNG");
          await delay(STEP_DELAY);
          setStep("bundle", "active", "Bundling HTML...");
        } else if (stage === "bundle") {
          setStep("bundle", "done", "Standalone HTML");
          await delay(STEP_DELAY);
          setStep("ipfs", "active", "Pinning to IPFS...");
        } else if (stage === "ipfs") {
          setStep("ipfs", "done", "3 files pinned");
          await delay(STEP_DELAY);
          setStep("metadata", "active", "Building metadata...");
        } else if (stage === "metadata") {
          setStep("metadata", "done", "FA2 metadata ready");
        } else if (stage === "ready") {
          await delay(STEP_DELAY);
          setStep("review", "active", "Review & confirm");
        }
      }
      
      if (eventType === "prepared" && eventData) {
        preparedData = eventData;
        await delay(STEP_DELAY);
        setStep("review", "active", `${preparedData.mintFee} XTZ · Review below`);
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
  setStep("review", "done", "Confirmed");
  setStep("sign", "active", "Check your wallet...");
  
  try {
    txHash = await _api.tezos.call(
      preparedData.contractAddress,
      preparedData.entrypoint,
      preparedData.michelsonParams.value,
      preparedData.mintFee
    );
    
    setStep("sign", "done", `TX: ${txHash.slice(0, 8)}...`);
    setStep("complete", "active", "Confirming on-chain...");
    
    // Wait and fetch token ID
    await new Promise(r => setTimeout(r, 3000));
    
    const networkPrefix = preparedData.network === "mainnet" ? "" : "ghostnet.";
    try {
      const res = await fetch(`https://api.${networkPrefix}tzkt.io/v1/tokens?contract=${preparedData.contractAddress}&sort.desc=id&limit=1`);
      if (res.ok) {
        const tokens = await res.json();
        if (tokens.length > 0) tokenId = tokens[0].tokenId;
      }
    } catch {}
    
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
      const highlighted = syntaxHighlightKidlisp(kidlispSource);
      
      // Ticker background
      ink(25, 35, 40, 200).box(0, y, w, 14);
      
      // Calculate total width and draw ticker
      let totalWidth = 0;
      for (const { token } of highlighted) {
        totalWidth += token.length * 4 + 2; // Approximate char width
      }
      
      // Add gap between repeats
      const gap = 40;
      const repeatWidth = totalWidth + gap;
      
      // Calculate starting x position (scrolling)
      let startX = -(tickerOffset % repeatWidth);
      
      // Draw tokens (repeat for seamless scroll)
      for (let repeat = 0; repeat < 3; repeat++) {
        let x = startX + repeat * repeatWidth;
        for (const { token, color } of highlighted) {
          if (x > w) break;
          if (x + token.length * 4 > 0) {
            ink(color[0], color[1], color[2]).write(token, { x, y: y + 3 }, undefined, undefined, false, "MatrixChunky8");
          }
          x += token.length * 4 + 2;
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
  let y = 4;
  
  // Header
  ink(100, 220, 180).write(`KEEP $${piece}`, { x: w/2, y, center: "x" }, undefined, undefined, false, "MatrixChunky8");
  y += 14;
  
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
    
    // Detail line
    if (item.detail && !isPending) {
      let detailColor;
      if (isItemError) detailColor = [200, 90, 90];
      else if (isActive) detailColor = phase.detail.map(c => c + 30);
      else detailColor = phase.detail;
      
      const maxLen = floor((w - 20) / 4);
      const truncated = item.detail.length > maxLen ? item.detail.slice(0, maxLen - 2) + ".." : item.detail;
      ink(...detailColor).write(truncated, { x: margin, y: y + 11 }, undefined, undefined, false, "MatrixChunky8");
    }
    
    y += stripeH + 2; // Gap between stripes
    
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
  }
  
  // === Bottom section ===
  const reviewStep = timeline.find(t => t.id === "review");
  const completeStep = timeline.find(t => t.id === "complete");
  
  y += 4;
  
  if (reviewStep?.status === "active" && preparedData) {
    // Network label (small, right-aligned)
    const netLabel = (preparedData.network || "ghostnet").toUpperCase();
    const netScheme = {
      normal: { bg: [35, 50, 60], outline: [100, 140, 160], outlineAlpha: 150, text: [100, 140, 160] },
      hover: { bg: [50, 75, 90], outline: [160, 220, 255], outlineAlpha: 200, text: [160, 220, 255] }
    };
    const netSize = mc8ButtonSize(netLabel);
    const netX = w - margin - netSize.w;
    networkBtn.btn.box.x = netX;
    networkBtn.btn.box.y = y;
    networkBtn.btn.box.w = netSize.w;
    networkBtn.btn.box.h = netSize.h;
    networkBtn.txt = netLabel;
    paintMC8Btn(netX, y, netLabel, { ink, line: ink }, netScheme, networkBtn.btn.down);
    y += netSize.h + 8;
    
    // KEEP button with price (large, centered)
    const keepLabel = `KEEP ꜩ${preparedData.mintFee || 5}`;
    const keepScheme = {
      normal: { bg: [70, 40, 120], outline: [180, 130, 255], outlineAlpha: 200, text: [255, 255, 255] },
      hover: { bg: [100, 60, 160], outline: [220, 180, 255], outlineAlpha: 255, text: [255, 255, 255] },
      disabled: { bg: [50, 40, 60], outline: [100, 80, 120], outlineAlpha: 100, text: [120, 120, 140] }
    };
    const keepSize = lgButtonSize(keepLabel);
    const keepX = floor((w - keepSize.w) / 2);
    btn.btn.box.x = keepX;
    btn.btn.box.y = y;
    btn.btn.box.w = keepSize.w;
    btn.btn.box.h = keepSize.h;
    btn.txt = keepLabel;
    paintLgBtn(keepX, y, keepLabel, { ink, line: ink }, keepScheme, btn.btn.down);
    
  } else if (completeStep?.status === "done") {
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
  
  // Asset link buttons + network button
  if (reviewStep?.status === "active" && preparedData) {
    if (preparedData.artifactUri) htmlBtn.btn.act(e, { push: () => openUrl(preparedData.artifactUri) });
    if (preparedData.thumbnailUri) thumbBtn.btn.act(e, { push: () => openUrl(preparedData.thumbnailUri) });
    if (preparedData.metadataUri) metaBtn.btn.act(e, { push: () => openUrl(preparedData.metadataUri) });
    
    // Network button links to contract on tzkt
    const contractAddress = preparedData.contractAddress || KEEPS_CONTRACT;
    const networkPrefix = preparedData.network === "mainnet" ? "" : "ghostnet.";
    networkBtn.btn.act(e, { push: () => openUrl(`https://${networkPrefix}tzkt.io/${contractAddress}`) });
    
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
  }
  
  if (e.is("keyboard:down:enter")) {
    if (alreadyMinted) openUrl(alreadyMinted.objktUrl);
    else if (reviewStep?.status === "active" && preparedData) signAndMint();
    else if (hasError()) { resetTimeline(); startTime = Date.now(); runProcess(); }
  }
  
  if (e.is("keyboard:down:escape")) _jump?.("prompt");
}

export { boot, paint, sim, act };
