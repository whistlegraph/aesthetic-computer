// keep-mint.mjs - Streaming NFT minting endpoint for KidLisp Keeps
// 
// POST /api/keep-mint - Mint a piece as an NFT (streaming SSE response)
// Requires authentication and piece ownership
//
// Optimized flow:
// 1. Validate auth/ownership/not-minted
// 2. START thumbnail generation in parallel (oven)
// 3. Analyze source for traits
// 4. Generate bundle (bundle-html)
// 5. Upload bundle to IPFS
// 6. AWAIT thumbnail
// 7. Upload metadata JSON to IPFS
// 8. Mint on Tezos

import { authorize, handleFor, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { analyzeKidLisp } from "../../backend/kidlisp-analyzer.mjs";
import { stream } from "@netlify/functions";
import { TezosToolkit } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";

const dev = process.env.CONTEXT === "dev";

// Allow self-signed certs in dev mode
if (dev) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// Configuration - Mainnet v5 RC staging contract
const CONTRACT_ADDRESS = process.env.TEZOS_KEEPS_CONTRACT || "KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido";
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const OVEN_FALLBACK_URL = "https://oven.aesthetic.computer"; // Always available fallback
const RPC_URL = NETWORK === "mainnet" 
  ? "https://mainnet.ecadinfra.com"
  : "https://ghostnet.ecadinfra.com";

// IPFS Gateway configuration
// When USE_GATEWAY_URLS is true, metadata will use full HTTPS URLs instead of ipfs:// URIs
// This ensures compatibility with platforms that use slow/unreliable public gateways
const USE_GATEWAY_URLS = process.env.USE_IPFS_GATEWAY_URLS === "true";
const IPFS_GATEWAY = process.env.IPFS_GATEWAY || "https://ipfs.aesthetic.computer";

// Helper to format IPFS URI - returns gateway URL or ipfs:// based on config
function formatIpfsUri(hash) {
  if (USE_GATEWAY_URLS) {
    return `${IPFS_GATEWAY}/ipfs/${hash}`;
  }
  return `ipfs://${hash}`;
}

// Cache credentials in memory for warm function invocations
let cachedPinataCredentials = null;
let cachedTezosCredentials = null;

async function getPinataCredentials() {
  if (cachedPinataCredentials) return cachedPinataCredentials;
  
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "pinata" });
  
  if (!secrets) {
    throw new Error("Pinata credentials not found in database");
  }
  
  cachedPinataCredentials = {
    apiKey: secrets.apiKey,
    apiSecret: secrets.apiSecret,
    jwt: secrets.jwt,
  };
  
  return cachedPinataCredentials;
}

async function getTezosCredentials() {
  if (cachedTezosCredentials) return cachedTezosCredentials;
  
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "tezos-kidlisp" });
  
  if (!secrets) {
    throw new Error("Tezos KidLisp credentials not found in database");
  }
  
  cachedTezosCredentials = {
    address: secrets.address,
    publicKey: secrets.publicKey,
    privateKey: secrets.privateKey,
    network: secrets.network,
  };
  
  return cachedTezosCredentials;
}

// Convert string to hex bytes (for Tezos)
function stringToBytes(str) {
  return Buffer.from(str, "utf8").toString("hex");
}

// Get the appropriate TzKT API base URL
function getTzktApiBase() {
  return NETWORK === "mainnet" ? "https://api.tzkt.io" : `https://api.${NETWORK}.tzkt.io`;
}

// Check if a piece name is already minted via TzKT
async function checkMintStatus(pieceName) {
  const keyBytes = stringToBytes(pieceName);
  const url = `${getTzktApiBase()}/v1/contracts/${CONTRACT_ADDRESS}/bigmaps/content_hashes/keys/${keyBytes}`;
  
  try {
    const response = await fetch(url);
    if (response.status === 200) {
      const data = await response.json();
      if (data.active) {
        const tokenId = data.value;
        
        // Fetch full token metadata for thumbnail
        let thumbnailUri = null;
        let name = null;
        let minter = null;
        try {
          const tokenUrl = `${getTzktApiBase()}/v1/tokens?contract=${CONTRACT_ADDRESS}&tokenId=${tokenId}`;
          const tokenResponse = await fetch(tokenUrl);
          if (tokenResponse.ok) {
            const tokens = await tokenResponse.json();
            if (tokens[0]?.metadata) {
              thumbnailUri = tokens[0].metadata.thumbnailUri;
              name = tokens[0].metadata.name;
              minter = tokens[0].metadata.minter;
            }
          }
        } catch (e) {
          console.warn(`Failed to fetch token metadata: ${e.message}`);
        }
        
        return { 
          minted: true, 
          tokenId,
          name,
          minter,
          thumbnailUri,
          objktUrl: `https://${NETWORK === "mainnet" ? "" : "ghostnet."}objkt.com/asset/${CONTRACT_ADDRESS}/${tokenId}`,
        };
      }
    }
    return { minted: false };
  } catch (error) {
    return { minted: false, error: error.message };
  }
}

// Simple hash for source code to detect changes
function hashSource(source) {
  let hash = 0;
  for (let i = 0; i < source.length; i++) {
    const char = source.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32bit integer
  }
  return hash.toString(16);
}

// Check if cached IPFS media is valid (source hasn't changed)
function isCachedMediaValid(piece) {
  if (!piece.ipfsMedia) return false;
  const { artifactUri, thumbnailUri, sourceHash, createdAt } = piece.ipfsMedia;
  if (!artifactUri || !thumbnailUri) return false;
  // Check if source code has changed
  const currentHash = hashSource(piece.source || "");
  if (currentHash !== sourceHash) return false;
  // Cache is valid for 30 days max (runtime may have changed)
  const maxAge = 30 * 24 * 60 * 60 * 1000;
  if (createdAt && Date.now() - new Date(createdAt).getTime() > maxAge) return false;
  return true;
}

// Upload file to IPFS via Pinata (with optional progress callback for SSE updates)
async function uploadToIPFS(content, filename, mimeType = "text/html", onProgress = null) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const formData = new FormData();
  const blob = new Blob([content], { type: mimeType });
  formData.append("file", blob, filename);
  formData.append("pinataMetadata", JSON.stringify({ name: filename }));
  // Don't wrap with directory - we want direct access to the HTML file
  formData.append("pinataOptions", JSON.stringify({ wrapWithDirectory: false }));

  // Add timeout to prevent hanging on slow Pinata responses
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 60000); // 60 second timeout
  
  const sizeKB = Math.round(content.length / 1024);
  console.log(`🪙 KEEP: Uploading ${filename} to IPFS (${sizeKB}KB)...`);
  const startTime = Date.now();
  
  // Send periodic progress updates while waiting
  let progressInterval = null;
  if (onProgress) {
    let dots = 0;
    progressInterval = setInterval(async () => {
      const elapsed = ((Date.now() - startTime) / 1000).toFixed(0);
      dots = (dots + 1) % 4;
      const dotStr = '.'.repeat(dots + 1);
      await onProgress(`Pinning ${sizeKB}KB${dotStr} (${elapsed}s)`);
    }, 3000); // Update every 3 seconds
  }

  let response;
  try {
    response = await fetch("https://api.pinata.cloud/pinning/pinFileToIPFS", {
      method: "POST",
      headers: {
        pinata_api_key: apiKey,
        pinata_secret_api_key: apiSecret,
      },
      body: formData,
      signal: controller.signal,
    });
  } catch (err) {
    clearTimeout(timeout);
    if (progressInterval) clearInterval(progressInterval);
    const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
    console.error(`🪙 KEEP: IPFS upload failed after ${elapsed}s:`, err.message);
    if (err.name === "AbortError") {
      throw new Error(`IPFS upload timed out after 60s. Pinata may be slow. Please try again.`);
    }
    throw err;
  }
  clearTimeout(timeout);
  if (progressInterval) clearInterval(progressInterval);
  
  const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`🪙 KEEP: IPFS upload response in ${elapsed}s, status: ${response.status}`);

  if (!response.ok) {
    const errorText = await response.text().catch(() => 'Unknown error');
    console.error(`🪙 KEEP: IPFS upload error response:`, errorText);
    throw new Error(`IPFS upload failed: ${response.status} - ${errorText.slice(0, 100)}`);
  }

  const result = await response.json();
  console.log(`🪙 KEEP: IPFS pin successful: ${result.IpfsHash}`);
  return formatIpfsUri(result.IpfsHash);
}

// Upload JSON metadata to IPFS
async function uploadJsonToIPFS(data, name) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  // Add timeout to prevent hanging on slow Pinata responses
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 30000); // 30 second timeout
  
  console.log(`🪙 KEEP: Uploading metadata ${name} to IPFS...`);
  const startTime = Date.now();

  let response;
  try {
    response = await fetch("https://api.pinata.cloud/pinning/pinJSONToIPFS", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        pinata_api_key: apiKey,
        pinata_secret_api_key: apiSecret,
      },
      body: JSON.stringify({
        pinataContent: data,
        pinataMetadata: { name },
      }),
      signal: controller.signal,
    });
  } catch (err) {
    clearTimeout(timeout);
    const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
    console.error(`🪙 KEEP: Metadata upload failed after ${elapsed}s:`, err.message);
    if (err.name === "AbortError") {
      throw new Error(`Metadata upload timed out after 30s. Pinata may be slow. Please try again.`);
    }
    throw err;
  }
  clearTimeout(timeout);
  
  const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  console.log(`🪙 KEEP: Metadata upload response in ${elapsed}s, status: ${response.status}`);

  if (!response.ok) {
    throw new Error(`Metadata upload failed: ${response.status}`);
  }

  const result = await response.json();
  return formatIpfsUri(result.IpfsHash);
}

// SSE helper
function sse(eventType, data) {
  return `event: ${eventType}\ndata: ${JSON.stringify(data)}\n\n`;
}

export const handler = stream(async (event, context) => {
  const headers = {
    "Content-Type": "text/event-stream",
    "Cache-Control": "no-cache",
    "Connection": "keep-alive",
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
  };

  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers, body: "" };
  }

  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ error: "Method not allowed" }),
    };
  }

  // Create readable stream for SSE
  const { readable, writable } = new TransformStream();
  const writer = writable.getWriter();
  const encoder = new TextEncoder();

  const send = async (eventType, data) => {
    await writer.write(encoder.encode(sse(eventType, data)));
  };

  // Track if stream is closed to prevent double-close
  let streamClosed = false;
  const processStartTime = Date.now();
  const stageTimes = {};
  
  const logStage = (stage, message) => {
    const elapsed = ((Date.now() - processStartTime) / 1000).toFixed(1);
    if (!stageTimes[stage]) stageTimes[stage] = { start: Date.now() };
    stageTimes[stage].last = Date.now();
    console.log(`🪙 KEEP [${elapsed}s] ${stage}: ${message}`);
  };
  
  const closeStream = async () => {
    if (!streamClosed) {
      streamClosed = true;
      await writer.close();
    }
  };

  // Process minting
  (async () => {
    try {
      // Parse body
      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        await send("error", { error: "Invalid JSON body" });
        return; // finally will close
      }

      const pieceName = body.piece?.replace(/^\$/, "");
      const mode = body.mode || "prepare"; // "prepare" (default) or "mint" (server-side)
      const regenerate = body.regenerate === true; // Force regenerate IPFS media
      const walletAddress = body.walletAddress; // For on-chain owner verification
      
      if (!pieceName) {
        await send("error", { error: "Missing 'piece' in request body" });
        return; // finally will close
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 1: VALIDATE
      // ═══════════════════════════════════════════════════════════════════
      logStage('validate', `Starting validation for $${pieceName}`);
      await send("progress", { stage: "validate", message: `Validating $${pieceName}...` });

      // Get piece from database first (needed for ownership checks)
      const database = await connect();
      const collection = database.db.collection("kidlisp");
      const piece = await collection.findOne({ code: pieceName });

      if (!piece) {
        await database.disconnect();
        await send("error", { error: `Piece '$${pieceName}' not found` });
        return; // finally will close
      }

      // Check if this is a regenerate request for an existing token
      const mintStatus = await checkMintStatus(pieceName);
      const isRebake = mintStatus.minted && regenerate;

      // For rebake: check if wallet is on-chain token owner (allows update without AC login)
      let isOnChainOwner = false;
      if (isRebake && walletAddress && mintStatus.tokenId != null) {
        try {
          const tokenResponse = await fetch(
            `https://api.tzkt.io/v1/tokens/balances?token.contract=${CONTRACT_ADDRESS}&token.tokenId=${mintStatus.tokenId}&balance.gt=0`
          );
          const balances = await tokenResponse.json();
          const ownerBalance = balances.find(b => b.account?.address === walletAddress);
          isOnChainOwner = !!ownerBalance;
          console.log(`🪙 KEEP-MINT: Wallet ${walletAddress} is on-chain owner: ${isOnChainOwner}`);
        } catch (e) {
          console.warn("🪙 KEEP-MINT: Could not verify on-chain ownership:", e.message);
        }
      }

      // Check AC auth - required for new mints, optional for rebake if wallet is token owner
      const user = await authorize(event.headers);
      
      // For new mints, always require AC login
      if (!isRebake && !user) {
        await database.disconnect();
        await send("error", { error: "Please log in first" });
        return; // finally will close
      }

      // For rebake without AC login, must be on-chain owner
      if (isRebake && !user && !isOnChainOwner) {
        await database.disconnect();
        await send("error", { error: "Please log in or connect a wallet that owns this token" });
        return; // finally will close
      }

      let userHandle = null;
      let isAdmin = false;
      
      if (user) {
        userHandle = await handleFor(user.sub);
        if (!userHandle && !isRebake) {
          await database.disconnect();
          await send("error", { error: "You need an @handle first. Enter 'handle' to claim one." });
          return; // finally will close
        }
        await send("progress", { stage: "validate", message: userHandle ? `Authenticated as @${userHandle}` : "Authenticated" });
        isAdmin = await hasAdmin(user);
      } else if (isOnChainOwner) {
        await send("progress", { stage: "validate", message: `Token owner: ${walletAddress.slice(0, 8)}...` });
      }

      // Check ownership - anonymous pieces cannot be kept
      if (!piece.user) {
        await database.disconnect();
        await send("error", { error: "Anonymous pieces cannot be kept. Log in and save it first." });
        return; // finally will close
      }
      
      // For new mints: require AC piece ownership
      // For rebake: allow AC piece owner OR on-chain token owner
      if (!isRebake && user && !isAdmin && piece.user !== user.sub) {
        const ownerHandle = await handleFor(piece.user);
        await database.disconnect();
        await send("error", { error: `This piece belongs to @${ownerHandle || "someone else"}` });
        return; // finally will close
      }
      
      if (isRebake && !isAdmin && !isOnChainOwner && (!user || piece.user !== user.sub)) {
        const ownerHandle = await handleFor(piece.user);
        await database.disconnect();
        await send("error", { error: `Only the piece owner or token holder can rebake` });
        return; // finally will close
      }

      // Check not already kept (unless regenerating bundle for existing token)
      if (mintStatus.minted && !regenerate) {
        await database.disconnect();
        await send("error", { 
          error: "Already kept", 
          tokenId: mintStatus.tokenId,
          name: mintStatus.name,
          minter: mintStatus.minter,
          thumbnailUri: mintStatus.thumbnailUri,
          objktUrl: mintStatus.objktUrl 
        });
        return; // finally will close
      }
      
      // Rebake mode: regenerate bundle for already-minted piece
      if (isRebake) {
        await send("progress", { stage: "validate", message: `Rebaking bundle for token #${mintStatus.tokenId}...` });
      }

      await send("progress", { stage: "validate", message: "Validation passed ✓" });
      
      // Get user's Tezos wallet address for creator attribution
      // RULE: The minting wallet MUST match the author's linked Tezos address
      const usersCollection = database.db.collection("users");
      const userDoc = user ? await usersCollection.findOne({ _id: user.sub }) : null;
      const linkedWalletAddress = userDoc?.tezos?.address;
      let creatorWalletAddress;
      
      // Skip wallet validation for rebake mode (not minting, just regenerating bundle)
      if (!isRebake) {
        // Verify author has a linked Tezos wallet
        if (!linkedWalletAddress) {
          await database.disconnect();
          await send("error", { 
            error: "Connect your Tezos wallet first",
            details: "Go to wallet.ac to link your Tezos address before keeping pieces."
          });
          return;
        }
        
        if (mode === "prepare") {
          // Client-side minting - wallet address must be provided AND match linked wallet
          creatorWalletAddress = body.walletAddress;
          if (!creatorWalletAddress || !creatorWalletAddress.startsWith('tz')) {
            await database.disconnect();
            await send("error", { error: "Valid Tezos wallet address required for minting" });
            return;
          }
          
          // ENFORCE: Minting wallet must match author's linked wallet
          if (creatorWalletAddress !== linkedWalletAddress) {
            await database.disconnect();
            await send("error", { 
              error: "Wallet mismatch",
              details: `You must mint from your linked wallet (${linkedWalletAddress.slice(0, 8)}...${linkedWalletAddress.slice(-6)}). Connect the correct wallet and try again.`,
              linkedWallet: linkedWalletAddress,
              providedWallet: creatorWalletAddress
            });
            return;
          }
        } else {
          // Server-side minting - use linked wallet address (admin testing only)
          creatorWalletAddress = linkedWalletAddress;
        }
      } else {
        // Rebake mode - use linked wallet for metadata (or fallback)
        creatorWalletAddress = linkedWalletAddress || "tz1burnburnburnburnburnburnburjAYjjX";
      }
      
      // Send piece details for client display
      const ownerHandle = await handleFor(piece.user);
      const pieceSourceHash = hashSource(piece.source || "");
      await send("progress", { 
        stage: "details", 
        piece: pieceName,
        source: piece.source,
        author: ownerHandle || userHandle,
        createdAt: piece.createdAt || piece.when,
        updatedAt: piece.updatedAt,
        sourceLength: piece.source?.length || 0,
      });
      
      // Note: Keep database connection open - we need it later for status update
      
      // Check for cached IPFS media (reuse if source hasn't changed)
      let useCachedMedia = !regenerate && isCachedMediaValid(piece);
      let artifactUri, thumbnailUri, metadataUri;
      
      if (useCachedMedia) {
        artifactUri = piece.ipfsMedia.artifactUri;
        thumbnailUri = piece.ipfsMedia.thumbnailUri;
        await send("progress", { stage: "ipfs", message: "Using cached IPFS media ✓" });
        console.log(`🪙 KEEP: Reusing cached IPFS media for $${pieceName}`);
      }
      
      // Get Pinata credentials early for thumbnail generation
      const pinataCredentials = await getPinataCredentials();

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 2: START THUMBNAIL IN PARALLEL (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      let thumbnailPromise = null;

      // Fixed 256x256 thumbnail for consistent display across platforms
      const thumbW = 256;
      const thumbH = 256;

      // Thumbnail generation timeout - must complete within this time
      const THUMBNAIL_TIMEOUT_MS = 45000; // 45 seconds (allows 30s piece load + 15s capture/encode/upload)

      if (!useCachedMedia) {
        logStage('thumbnail', `Starting ${thumbW * 2}x${thumbH * 2} WebP generation`);
        await send("progress", { stage: "thumbnail", message: `Baking ${thumbW * 2}x${thumbH * 2} WebP...` });

        // Helper to try oven thumbnail generation with timeout
        const tryOvenThumbnail = async (ovenUrl, timeoutMs = 45000) => {
        // Create abort controller for fetch timeout
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), timeoutMs);

        try {
          const res = await fetch(`${ovenUrl}/grab-ipfs`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              piece: `$${pieceName}`,
              format: "webp",
              width: thumbW,
              height: thumbH,
              density: 2,
              duration: 8000,
              fps: 10,
              playbackFps: 20,
              quality: 70,
              // Source-aware cache key lets oven reuse only when source + constraints match.
              cacheKey: `src-${pieceSourceHash}`,
              skipCache: false,
              pinataKey: pinataCredentials.apiKey,
              pinataSecret: pinataCredentials.apiSecret,
            }),
            signal: controller.signal,
          });
          clearTimeout(timeoutId);
          if (!res.ok) throw new Error(`Thumbnail failed: ${res.status}`);
          return res.json();
        } catch (err) {
          clearTimeout(timeoutId);
          if (err.name === 'AbortError') {
            throw new Error(`Oven request timed out after ${timeoutMs/1000}s`);
          }
          throw err;
        }
      };

      // Try local oven first, then fallback to production - with overall timeout
      const thumbnailGenerationPromise = !useCachedMedia ? (async () => {
        try {
          const result = await tryOvenThumbnail(OVEN_URL, THUMBNAIL_TIMEOUT_MS);
          console.log(`🪙 KEEP: Oven response:`, JSON.stringify(result));
          if (!result?.ipfsUri && result?.error) {
            throw new Error(result.error);
          }
          return result;
        } catch (localErr) {
          console.warn(`🪙 KEEP: Oven failed (${OVEN_URL}):`, localErr.message);
          if (OVEN_URL !== OVEN_FALLBACK_URL) {
            console.log(`🪙 KEEP: Trying fallback oven: ${OVEN_FALLBACK_URL}`);
            try {
              const fallbackResult = await tryOvenThumbnail(OVEN_FALLBACK_URL, THUMBNAIL_TIMEOUT_MS);
              console.log(`🪙 KEEP: Fallback oven response:`, JSON.stringify(fallbackResult));
              if (!fallbackResult?.ipfsUri && fallbackResult?.error) {
                throw new Error(fallbackResult.error);
              }
              return fallbackResult;
            } catch (fallbackErr) {
              console.warn(`🪙 KEEP: Fallback oven failed:`, fallbackErr.message);
              return { error: fallbackErr.message };
            }
          }
          return { error: localErr.message };
        }
      })() : Promise.resolve({ ipfsUri: thumbnailUri });

      // Wrap with timeout promise to ensure we don't hang indefinitely
      thumbnailPromise = Promise.race([
        thumbnailGenerationPromise,
        new Promise((_, reject) =>
          setTimeout(() => reject(new Error(`Thumbnail generation timed out after ${THUMBNAIL_TIMEOUT_MS/1000}s`)), THUMBNAIL_TIMEOUT_MS)
        )
      ]);
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 3: ANALYZE SOURCE
      // ═══════════════════════════════════════════════════════════════════
      await send("progress", { stage: "analyze", message: "Analyzing source code..." });
      
      const analysis = analyzeKidLisp(piece.source);

      await send("progress", {
        stage: "analyze",
        message: `${analysis.chars} chars ✓`
      });

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 4: GENERATE BUNDLE (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      let bundleHtml, bundleFilename, authorHandle, userCode, packDate, depCount;
      
      if (!useCachedMedia) {
        logStage('bundle', 'Generating HTML bundle');
        await send("progress", { stage: "bundle", message: "Packing HTML bundle..." });
        
        const bundleUrl = dev
          ? `https://localhost:8888/api/bundle-html?code=${pieceName}&format=json&noboxart=1`
          : `https://oven.aesthetic.computer/bundle-html?code=${pieceName}&format=json&noboxart=1`;
        
        // Add timeout to prevent function from hanging if bundle-html is slow
        // Bundle generation can take 15-30s on cold starts due to SWC minification
        const bundleController = new AbortController();
        const bundleTimeout = setTimeout(() => bundleController.abort(), 40000); // 40 second timeout
        
        console.log(`🪙 KEEP: Fetching bundle from ${bundleUrl}...`);
        await send("progress", { stage: "bundle", message: "Requesting bundle from API..." });
        const bundleStartTime = Date.now();

        let bundleResponse;
        try {
          bundleResponse = await fetch(bundleUrl, { signal: bundleController.signal });
        } catch (fetchErr) {
          clearTimeout(bundleTimeout);
          const elapsed = ((Date.now() - bundleStartTime) / 1000).toFixed(1);
          console.error(`🪙 KEEP: Bundle fetch failed after ${elapsed}s:`, fetchErr.message);
          await send("progress", { stage: "bundle", message: `Fetch error after ${elapsed}s: ${fetchErr.message}` });
          if (fetchErr.name === "AbortError") {
            throw new Error("Bundle generation timed out (>40s). The server may be cold-starting. Please try again in a moment.");
          }
          throw fetchErr;
        }
        clearTimeout(bundleTimeout);

        const bundleElapsed = ((Date.now() - bundleStartTime) / 1000).toFixed(1);
        console.log(`🪙 KEEP: Bundle HTTP response received in ${bundleElapsed}s, status: ${bundleResponse.status}`);
        await send("progress", { stage: "bundle", message: `Response received (${bundleElapsed}s, status ${bundleResponse.status})` });

        if (!bundleResponse.ok) {
          const errorText = await bundleResponse.text();
          console.error(`🪙 KEEP: Bundle generation failed with status ${bundleResponse.status}:`, errorText.slice(0, 200));
          throw new Error(`Bundle generation failed: ${bundleResponse.status}`);
        }

        console.log(`🪙 KEEP: Parsing bundle JSON response...`);
        const bundleData = await bundleResponse.json();
        console.log(`🪙 KEEP: Bundle JSON parsed successfully, has error: ${!!bundleData.error}`);

        if (bundleData.error) {
          console.error(`🪙 KEEP: Bundle API returned error:`, bundleData.error);
          throw new Error(`Bundle error: ${bundleData.error}`);
        }

        console.log(`🪙 KEEP: Decoding bundle content...`);
        bundleHtml = Buffer.from(bundleData.content, "base64").toString("utf8");
        bundleFilename = bundleData.filename || `$${pieceName}.lisp.html`;
        authorHandle = bundleData.authorHandle || `@${userHandle}`;
        userCode = bundleData.userCode;
        packDate = bundleData.packDate;
        depCount = bundleData.depCount || 0;

        // Calculate bundle size
        const bundleSize = Math.round(bundleHtml.length / 1024);
        console.log(`🪙 KEEP: Bundle ready: ${bundleSize}KB with ${depCount} deps`);
        await send("progress", { stage: "bundle", message: `Packed ${bundleSize}KB · ${depCount} deps` });
      } else {
        // Use cached values
        authorHandle = piece.ipfsMedia.authorHandle || `@${userHandle}`;
        userCode = piece.ipfsMedia.userCode;
        packDate = piece.ipfsMedia.packDate;
        depCount = piece.ipfsMedia.depCount || 0;
        // Show cache date
        const cacheDate = piece.ipfsMedia.createdAt ? new Date(piece.ipfsMedia.createdAt).toLocaleDateString("en-US", { month: "short", day: "numeric" }) : "cached";
        await send("progress", { stage: "bundle", message: `Using cached · ${cacheDate}` });
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 5: UPLOAD BUNDLE TO IPFS (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      if (!useCachedMedia) {
        logStage('ipfs', `Uploading bundle to Pinata (${Math.round(bundleHtml.length / 1024)}KB)`);
        await send("progress", { stage: "ipfs", message: "Connecting to Pinata..." });
        
        const ipfsStartTime = Date.now();
        console.log(`🪙 KEEP: Starting IPFS bundle upload...`);
        
        // Pass progress callback for periodic updates during upload
        const onIPFSProgress = async (msg) => {
          await send("progress", { stage: "ipfs", message: msg });
        };
        
        artifactUri = await uploadToIPFS(
          bundleHtml, 
          bundleFilename,
          "text/html",
          onIPFSProgress
        );
        
        const ipfsElapsed = ((Date.now() - ipfsStartTime) / 1000).toFixed(1);
        logStage('ipfs', `Upload complete in ${ipfsElapsed}s`);
        
        // Show IPFS hash with timing
        const hash = artifactUri.replace("ipfs://", "");
        await send("progress", { stage: "ipfs", message: `Pinned in ${ipfsElapsed}s` });
      } else {
        // Show cached IPFS hash (full)
        const hash = artifactUri.replace("ipfs://", "");
        await send("progress", { stage: "ipfs", message: `Cached ${hash}` });
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 6: AWAIT THUMBNAIL (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      if (!useCachedMedia) {
        await send("progress", { stage: "thumbnail", message: `Awaiting ${thumbW}x${thumbH}@2x WebP...` });
        
        const thumbResult = await thumbnailPromise;
        
        if (!thumbResult?.ipfsUri) {
          const errorMsg = thumbResult?.error || "unknown error";
          console.error(`🪙 KEEP: Thumbnail failed:`, errorMsg);
          throw new Error(`Thumbnail generation failed - ${errorMsg}`);
        }
        
        thumbnailUri = thumbResult.ipfsUri;
        const thumbHash = thumbnailUri.replace("ipfs://", "");
        
        await send("progress", { 
          stage: "thumbnail", 
          message: `Baked ${thumbHash.slice(0, 12)}..`
        });
        
        // Cache the IPFS media in MongoDB for future use
        // First, preserve old media in history for cleanup tracking
        const updateOps = { 
          $set: { 
            ipfsMedia: {
              artifactUri,
              thumbnailUri,
              sourceHash: pieceSourceHash,
              authorHandle,
              userCode,
              packDate,
              depCount,
              createdAt: new Date(),
            }
          }
        };
        
        // If there was previous media, push it to history (for cleanup tracking)
        if (piece.ipfsMedia?.artifactUri || piece.ipfsMedia?.thumbnailUri) {
          updateOps.$push = {
            mediaHistory: {
              $each: [{
                artifactUri: piece.ipfsMedia.artifactUri,
                thumbnailUri: piece.ipfsMedia.thumbnailUri,
                sourceHash: piece.ipfsMedia.sourceHash,
                createdAt: piece.ipfsMedia.createdAt,
                archivedAt: new Date(),
                reason: isRebake ? 'rebake' : 'mint',
              }],
              $slice: -20, // Keep last 20 entries max
            }
          };
          console.log(`🪙 KEEP: Archived previous media to history`);
        }
        
        await collection.updateOne({ code: pieceName }, updateOps);
        console.log(`🪙 KEEP: Cached IPFS media for $${pieceName}`);
        
        // REBAKE MODE: Return early with new URIs (don't proceed to minting)
        if (isRebake) {
          // Store pending rebake info so it persists across page refreshes
          await collection.updateOne(
            { code: pieceName },
            { 
              $set: { 
                pendingRebake: {
                  artifactUri,
                  thumbnailUri,
                  createdAt: new Date(),
                  sourceHash: pieceSourceHash,
                }
              }
            }
          );
          console.log(`🪙 KEEP: Stored pending rebake for $${pieceName}`);
          
          const rebakeCreatedAt = new Date().toISOString();
          await send("progress", { stage: "ready", message: "Bundle regenerated!" });
          await send("ready", {
            success: true,
            rebake: true,
            piece: pieceName,
            tokenId: mintStatus.tokenId,
            artifactUri,
            thumbnailUri,
            objktUrl: mintStatus.objktUrl,
            createdAt: rebakeCreatedAt,
            packDate,
          });
          await database.disconnect();
          return;
        }
      } else {
        const thumbHash = thumbnailUri.replace("ipfs://", "");
        await send("progress", { stage: "thumbnail", message: `Cached ${thumbHash.slice(0, 12)}..` });
      }
      
      // REBAKE MODE with cached media: Still return the cached URIs
      // (pendingRebake was already stored when the bundle was first generated)
      if (isRebake) {
        await send("progress", { stage: "ready", message: "Bundle info retrieved!" });
        await send("ready", {
          success: true,
          rebake: true,
          piece: pieceName,
          tokenId: mintStatus.tokenId,
          artifactUri,
          thumbnailUri,
          objktUrl: mintStatus.objktUrl,
          createdAt: piece.pendingRebake?.createdAt ? new Date(piece.pendingRebake.createdAt).toISOString() : new Date().toISOString(),
          packDate,
          fromCache: true,
        });
        await database.disconnect();
        return;
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 7: BUILD & UPLOAD METADATA
      // ═══════════════════════════════════════════════════════════════════
      await send("progress", { stage: "metadata", message: "Building metadata..." });

      const tokenName = `$${pieceName}`;
      
      // Description is the raw KidLisp source code (newlines preserved)
      const description = piece.source || "A KidLisp piece preserved on Tezos";

      // Build tags
      const tags = [`$${pieceName}`, "KidLisp", "Aesthetic.Computer"];
      if (authorHandle) tags.push(`@${authorHandle.replace(/^@/, "")}`);
      if (userCode) tags.push(userCode);

      // Use analyzer traits + add author/packed info as attributes
      const attributes = [
        ...analysis.traits,
        ...(authorHandle && authorHandle !== "@anon" ? [{ name: "Author Handle", value: `@${authorHandle.replace(/^@/, "")}` }] : []),
        ...(userCode ? [{ name: "Author Code", value: userCode }] : []),
        ...(depCount > 0 ? [{ name: "Dependencies", value: String(depCount) }] : []),
        ...(packDate ? [{ name: "Packed", value: packDate }] : []),
      ];

      // Creator identity for metadata
      // objkt.com uses firstMinter for artist attribution
      // creators array contains just the wallet address for on-chain attribution
      const creatorsArray = [creatorWalletAddress];
      
      // v4: 10% royalty to creator on secondary sales
      const royalties = {
        decimals: 4,
        shares: {
          [creatorWalletAddress]: "1000"  // 10% = 1000/10000 basis points
        }
      };

      const metadataJson = {
        name: tokenName,
        description,
        artifactUri,
        displayUri: artifactUri,
        thumbnailUri,
        decimals: 0,
        symbol: pieceName,
        isBooleanAmount: true,
        shouldPreferSymbol: false,
        minter: `@${(authorHandle || "anon").replace(/^@/, "")}`,
        creators: creatorsArray,
        royalties,  // v4: Add royalty configuration
        rights: "© All rights reserved",
        mintingTool: "https://aesthetic.computer",
        formats: [{
          uri: artifactUri,
          mimeType: "text/html",
          dimensions: { value: "responsive", unit: "viewport" },
        }],
        tags,
        attributes,
      };

      metadataUri = await uploadJsonToIPFS(
        metadataJson,
        `$${pieceName}-metadata.json`
      );
      
      await send("progress", { stage: "metadata", message: "Metadata uploaded ✓" });

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 8: PREPARE OR MINT
      // ═══════════════════════════════════════════════════════════════════
      
      // Minimal on-chain metadata — full metadata lives in IPFS via metadata_uri
      // description = raw KidLisp source (the actual art, preserved on-chain)
      const onChainMetadata = {
        name: stringToBytes(tokenName),
        symbol: stringToBytes(pieceName),
        description: stringToBytes(piece.source || ""),
        artifactUri: stringToBytes(artifactUri),
        displayUri: stringToBytes(artifactUri),
        thumbnailUri: stringToBytes(thumbnailUri),
        decimals: stringToBytes("0"),
        creators: stringToBytes(JSON.stringify(creatorsArray)),
        royalties: stringToBytes(JSON.stringify(royalties)),
        content_hash: stringToBytes(pieceName),
        metadata_uri: stringToBytes(metadataUri),
      };

      // PREPARE MODE: Return data for client-side wallet minting
      if (mode === "prepare") {
        const totalElapsed = ((Date.now() - processStartTime) / 1000).toFixed(1);
        console.log(`🪙 KEEP: Preparation complete in ${totalElapsed}s`);
        console.log(`🪙 KEEP: Stage timing summary:`, Object.entries(stageTimes).map(([k, v]) => 
          `${k}: ${((v.last - v.start) / 1000).toFixed(1)}s`
        ).join(', '));
        
        await send("progress", { stage: "ready", message: "Ready for wallet signature..." });
        
        // Use Taquito to generate the Michelson parameters for the contract call
        // This ensures proper encoding that Beacon can use
        const tezos = new TezosToolkit(RPC_URL);
        const contract = await tezos.contract.at(CONTRACT_ADDRESS);
        
        // Read the keep_fee from contract storage
        const storage = await contract.storage();
        const keepFeeMutez = storage.keep_fee?.toNumber?.() ?? 0;
        const keepFeeXtz = keepFeeMutez / 1_000_000;
        
        // Use the wallet address validated earlier
        const ownerAddress = creatorWalletAddress;
        
        const transferParams = contract.methodsObject.keep({
          name: onChainMetadata.name,
          symbol: onChainMetadata.symbol,
          description: onChainMetadata.description,
          artifactUri: onChainMetadata.artifactUri,
          displayUri: onChainMetadata.displayUri,
          thumbnailUri: onChainMetadata.thumbnailUri,
          decimals: onChainMetadata.decimals,
          creators: onChainMetadata.creators,
          royalties: onChainMetadata.royalties,
          content_hash: onChainMetadata.content_hash,
          metadata_uri: onChainMetadata.metadata_uri,
          owner: ownerAddress,
        }).toTransferParams();
        
        await send("prepared", {
          success: true,
          piece: pieceName,
          contractAddress: CONTRACT_ADDRESS,
          network: NETWORK,
          mintFee: keepFeeXtz, // Read from contract storage
          // Send the Michelson-encoded parameters for Beacon
          michelsonParams: transferParams.parameter,
          entrypoint: "keep",
          artifactUri,
          thumbnailUri,
          metadataUri,
          packDate, // Bundle pack date (for display after rebake)
          rpcUrl: RPC_URL,
          usedCachedMedia: useCachedMedia, // Tell client if we reused IPFS pins
          cacheGeneratedAt: useCachedMedia ? piece.ipfsMedia?.createdAt : null, // When cache was generated
        });
        return;
      }

      // MINT MODE: Server-side minting (for testing/admin)
      await send("progress", { stage: "mint", message: "Minting on Tezos..." });

      // Get Tezos credentials from MongoDB
      const tezosCredentials = await getTezosCredentials();
      
      const tezos = new TezosToolkit(RPC_URL);
      tezos.setProvider({ signer: new InMemorySigner(tezosCredentials.privateKey) });
      const adminAddress = await tezos.signer.publicKeyHash();

      // Use destination address from profile (already fetched) or fall back to admin
      const destinationAddress = creatorWalletAddress || adminAddress;
      
      if (destinationAddress !== adminAddress) {
        await send("progress", { 
          stage: "mint", 
          message: `Minting to ${destinationAddress.slice(0, 8)}...${destinationAddress.slice(-6)}` 
        });
      }

      const contract = await tezos.contract.at(CONTRACT_ADDRESS);
        
        // Read the keep_fee from contract storage
        const storage = await contract.storage();
        const keepFeeMutez = storage.keep_fee?.toNumber?.() ?? 0;
        const keepFeeXtz = keepFeeMutez / 1_000_000;
      
      const op = await contract.methodsObject.keep({
        name: onChainMetadata.name,
        symbol: onChainMetadata.symbol,
        description: onChainMetadata.description,
        artifactUri: onChainMetadata.artifactUri,
        displayUri: onChainMetadata.displayUri,
        thumbnailUri: onChainMetadata.thumbnailUri,
        decimals: onChainMetadata.decimals,
        creators: onChainMetadata.creators,
        royalties: onChainMetadata.royalties,
        content_hash: onChainMetadata.content_hash,
        metadata_uri: onChainMetadata.metadata_uri,
        owner: destinationAddress,
      }).send();

      await send("progress", { stage: "mint", message: `Tx submitted: ${op.hash.slice(0, 12)}...` });
      
      // Wait for confirmation
      await op.confirmation(1);

      // Get token ID from contract storage (next_token_id - 1)
      // This is O(1) and scales to millions of tokens
      const updatedStorage = await contract.storage();
      const tokenId = updatedStorage.next_token_id.toNumber() - 1;

      const objktUrl = `https://${NETWORK === "mainnet" ? "" : "ghostnet."}objkt.com/asset/${CONTRACT_ADDRESS}/${tokenId}`;

      // Update MongoDB to mark piece as minted
      // Use contract-keyed storage: tezos.contracts[CONTRACT_ADDRESS]
      const piecesCollection = database.db.collection("kidlisp");
      await piecesCollection.updateOne(
        { user: user.sub, code: pieceName },
        { 
          $set: { 
            [`tezos.contracts.${CONTRACT_ADDRESS}`]: {
              minted: true,
              tokenId: tokenId,
              txHash: op.hash,
              network: NETWORK,
              mintedAt: new Date(),
              owner: destinationAddress,
              minter: creatorWalletAddress,
              artifactUri: artifactUri,
              thumbnailUri: thumbnailUri,
              metadataUri: metadataUri,
            }
          }
        }
      );

      await send("complete", {
        success: true,
        piece: pieceName,
        tokenId,
        txHash: op.hash,
        artifactUri,
        thumbnailUri,
        objktUrl,
        explorerUrl: `https://${NETWORK}.tzkt.io/${op.hash}`,
      });

    } catch (error) {
      console.error("Keep mint error:", error);
      try {
        await send("error", { error: error.message || "Minting failed" });
      } catch (e) {
        // Stream may already be closed
      }
    } finally {
      await closeStream();
    }
  })();

  return { statusCode: 200, headers, body: readable };
});
