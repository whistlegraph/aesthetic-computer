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
import { analyzeKidLisp, ANALYZER_VERSION } from "../../backend/kidlisp-analyzer.mjs";
import { stream } from "@netlify/functions";
import { TezosToolkit } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";

const dev = process.env.CONTEXT === "dev";

// Allow self-signed certs in dev mode
if (dev) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// Configuration - Mainnet staging contract by default
const CONTRACT_ADDRESS = process.env.TEZOS_KEEPS_CONTRACT || "KT1EcsqR69BHekYF5mDQquxrvNg5HhPFx6NM";
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const OVEN_FALLBACK_URL = "https://oven.aesthetic.computer"; // Always available fallback
const RPC_URL = NETWORK === "mainnet" 
  ? "https://mainnet.ecadinfra.com"
  : "https://ghostnet.ecadinfra.com";

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

// Upload file to IPFS via Pinata
async function uploadToIPFS(content, filename, mimeType = "text/html") {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const formData = new FormData();
  const blob = new Blob([content], { type: mimeType });
  formData.append("file", blob, filename);
  formData.append("pinataMetadata", JSON.stringify({ name: filename }));
  // Don't wrap with directory - we want direct access to the HTML file
  formData.append("pinataOptions", JSON.stringify({ wrapWithDirectory: false }));

  const response = await fetch("https://api.pinata.cloud/pinning/pinFileToIPFS", {
    method: "POST",
    headers: {
      pinata_api_key: apiKey,
      pinata_secret_api_key: apiSecret,
    },
    body: formData,
  });

  if (!response.ok) {
    throw new Error(`IPFS upload failed: ${response.status}`);
  }

  const result = await response.json();
  return `ipfs://${result.IpfsHash}`;
}

// Upload JSON metadata to IPFS
async function uploadJsonToIPFS(data, name) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const response = await fetch("https://api.pinata.cloud/pinning/pinJSONToIPFS", {
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
  });

  if (!response.ok) {
    throw new Error(`Metadata upload failed: ${response.status}`);
  }

  const result = await response.json();
  return `ipfs://${result.IpfsHash}`;
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
      
      if (!pieceName) {
        await send("error", { error: "Missing 'piece' in request body" });
        return; // finally will close
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 1: VALIDATE
      // ═══════════════════════════════════════════════════════════════════
      await send("progress", { stage: "validate", message: `Validating $${pieceName}...` });

      // Check auth
      const user = await authorize(event.headers);
      if (!user) {
        await send("error", { error: "Please log in first" });
        return; // finally will close
      }

      const userHandle = await handleFor(user.sub);
      if (!userHandle) {
        await send("error", { error: "You need an @handle first. Enter 'handle' to claim one." });
        return; // finally will close
      }

      await send("progress", { stage: "validate", message: `Authenticated as @${userHandle}` });

      const isAdmin = await hasAdmin(user);

      // Get piece from database
      const database = await connect();
      const collection = database.db.collection("kidlisp");
      const piece = await collection.findOne({ code: pieceName });

      if (!piece) {
        await database.disconnect();
        await send("error", { error: `Piece '$${pieceName}' not found` });
        return; // finally will close
      }

      // Check ownership - anonymous pieces cannot be kept
      if (!piece.user) {
        await database.disconnect();
        await send("error", { error: "Anonymous pieces cannot be kept. Log in and save it first." });
        return; // finally will close
      }
      
      if (!isAdmin && piece.user !== user.sub) {
        const ownerHandle = await handleFor(piece.user);
        await database.disconnect();
        await send("error", { error: `This piece belongs to @${ownerHandle || "someone else"}` });
        return; // finally will close
      }

      // Check not already kept (unless regenerating bundle for existing token)
      const mintStatus = await checkMintStatus(pieceName);
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
      const isRebake = mintStatus.minted && regenerate;
      if (isRebake) {
        await send("progress", { stage: "validate", message: `Rebaking bundle for token #${mintStatus.tokenId}...` });
      }

      await send("progress", { stage: "validate", message: "Validation passed ✓" });
      
      // Get user's Tezos wallet address for creator attribution
      // RULE: The minting wallet MUST match the author's linked Tezos address
      const usersCollection = database.db.collection("users");
      const userDoc = await usersCollection.findOne({ _id: user.sub });
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
      
      // Calculate thumbnail dimensions based on screen aspect ratio
      // Max 128px on longest side at 2x density = 256px actual
      const screenW = body.screenWidth || 128;
      const screenH = body.screenHeight || 128;
      const maxSize = 128; // Base size (will be 2x for density)
      const aspect = screenW / screenH;
      let thumbW, thumbH;
      if (aspect >= 1) {
        // Landscape or square
        thumbW = maxSize;
        thumbH = Math.round(maxSize / aspect);
      } else {
        // Portrait
        thumbH = maxSize;
        thumbW = Math.round(maxSize * aspect);
      }
      
      if (!useCachedMedia) {
        await send("progress", { stage: "thumbnail", message: `Baking ${thumbW * 2}x${thumbH * 2} WebP...` });
        
        // Helper to try oven thumbnail generation
        const tryOvenThumbnail = async (ovenUrl) => {
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
            pinataKey: pinataCredentials.apiKey,
            pinataSecret: pinataCredentials.apiSecret,
          }),
        });
        if (!res.ok) throw new Error(`Thumbnail failed: ${res.status}`);
        return res.json();
      };
      
      // Try local oven first, then fallback to production
      thumbnailPromise = !useCachedMedia ? (async () => {
        try {
          return await tryOvenThumbnail(OVEN_URL);
        } catch (localErr) {
          console.warn(`Local oven failed (${OVEN_URL}):`, localErr.message);
          if (OVEN_URL !== OVEN_FALLBACK_URL) {
            console.log(`Trying fallback oven: ${OVEN_FALLBACK_URL}`);
            try {
              return await tryOvenThumbnail(OVEN_FALLBACK_URL);
            } catch (fallbackErr) {
              console.warn(`Fallback oven failed:`, fallbackErr.message);
              return null;
            }
          }
          return null;
        }
      })() : Promise.resolve({ ipfsUri: thumbnailUri });
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 3: ANALYZE SOURCE
      // ═══════════════════════════════════════════════════════════════════
      await send("progress", { stage: "analyze", message: "Analyzing source code..." });
      
      const analysis = analyzeKidLisp(piece.source);
      
      await send("progress", { 
        stage: "analyze", 
        message: `${analysis.lineCount} lines, ${analysis.complexity.tier} complexity ✓`
      });

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 4: GENERATE BUNDLE (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      let bundleHtml, bundleFilename, authorHandle, userCode, packDate, depCount;
      
      if (!useCachedMedia) {
        await send("progress", { stage: "bundle", message: "Packing HTML bundle..." });
        
        const bundleUrl = dev 
          ? `https://localhost:8888/api/bundle-html?code=${pieceName}&format=json`
          : `https://aesthetic.computer/api/bundle-html?code=${pieceName}&format=json`;
        
        const bundleResponse = await fetch(bundleUrl);
        if (!bundleResponse.ok) {
          throw new Error(`Bundle generation failed: ${bundleResponse.status}`);
        }
        
        const bundleData = await bundleResponse.json();
        if (bundleData.error) {
          throw new Error(`Bundle error: ${bundleData.error}`);
        }
        
        bundleHtml = Buffer.from(bundleData.content, "base64").toString("utf8");
        bundleFilename = bundleData.filename || `$${pieceName}.lisp.html`;
        authorHandle = bundleData.authorHandle || `@${userHandle}`;
        userCode = bundleData.userCode;
        packDate = bundleData.packDate;
        depCount = bundleData.depCount || 0;
        
        // Calculate bundle size
        const bundleSize = Math.round(bundleHtml.length / 1024);
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
        await send("progress", { stage: "ipfs", message: "Pinning bundle..." });
        
        artifactUri = await uploadToIPFS(
          bundleHtml, 
          bundleFilename,
          "text/html"
        );
        
        // Show full IPFS hash
        const hash = artifactUri.replace("ipfs://", "");
        await send("progress", { stage: "ipfs", message: `Pinned ${hash}` });
      } else {
        // Show cached IPFS hash (full)
        const hash = artifactUri.replace("ipfs://", "");
        await send("progress", { stage: "ipfs", message: `Cached ${hash}` });
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 6: AWAIT THUMBNAIL (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      if (!useCachedMedia) {
        await send("progress", { stage: "thumbnail", message: "Baking 256x256 WebP..." });
        
        const thumbResult = await thumbnailPromise;
        
        if (!thumbResult?.ipfsUri) {
          throw new Error("Thumbnail generation failed - oven unavailable");
        }
        
        thumbnailUri = thumbResult.ipfsUri;
        const thumbHash = thumbnailUri.replace("ipfs://", "");
        
        await send("progress", { 
          stage: "thumbnail", 
          message: `Baked ${thumbHash.slice(0, 12)}..`
        });
        
        // Cache the IPFS media in MongoDB for future use
        await collection.updateOne(
          { code: pieceName },
          { 
            $set: { 
              ipfsMedia: {
                artifactUri,
                thumbnailUri,
                sourceHash: hashSource(piece.source || ""),
                authorHandle,
                userCode,
                packDate,
                depCount,
                createdAt: new Date(),
              }
            }
          }
        );
        console.log(`🪙 KEEP: Cached IPFS media for $${pieceName}`);
        
        // REBAKE MODE: Return early with new URIs (don't proceed to minting)
        if (isRebake) {
          await send("progress", { stage: "ready", message: "Bundle regenerated!" });
          await send("ready", {
            success: true,
            rebake: true,
            piece: pieceName,
            tokenId: mintStatus.tokenId,
            artifactUri,
            thumbnailUri,
            objktUrl: mintStatus.objktUrl,
          });
          await database.disconnect();
          return;
        }
      } else {
        const thumbHash = thumbnailUri.replace("ipfs://", "");
        await send("progress", { stage: "thumbnail", message: `Cached ${thumbHash.slice(0, 12)}..` });
      }
      
      // REBAKE MODE with cached media: Still return the cached URIs
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
        });
        await database.disconnect();
        return;
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 7: BUILD & UPLOAD METADATA
      // ═══════════════════════════════════════════════════════════════════
      await send("progress", { stage: "metadata", message: "Building metadata..." });

      const tokenName = `$${pieceName}`;
      
      // Description is ONLY the KidLisp source code (clean and simple)
      const description = piece.source || `A KidLisp piece preserved on Tezos`;

      // Build tags
      const tags = [`$${pieceName}`, "KidLisp", "Aesthetic.Computer", "interactive"];
      if (userCode) tags.push(userCode);

      // Use analyzer traits + add author/packed info as attributes
      const attributes = [
        ...analysis.traits,
        ...(authorHandle && authorHandle !== "@anon" ? [{ name: "Author", value: authorHandle }] : []),
        ...(userCode ? [{ name: "User Code", value: userCode }] : []),
        ...(depCount > 0 ? [{ name: "Dependencies", value: String(depCount) }] : []),
        ...(packDate ? [{ name: "Packed", value: packDate }] : []),
        { name: "Analyzer Version", value: ANALYZER_VERSION },
      ];

      // Creator identity for metadata
      // objkt.com uses firstMinter for artist attribution
      // creators array contains just the wallet address for on-chain attribution
      const creatorsArray = [creatorWalletAddress];
      
      const metadataJson = {
        name: tokenName,
        description,
        artifactUri,
        displayUri: artifactUri,
        thumbnailUri,
        decimals: 0,
        symbol: "KEEP",
        isBooleanAmount: true,
        shouldPreferSymbol: false,
        minter: authorHandle,
        creators: creatorsArray,
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
      
      // Build on-chain metadata (shared between prepare and mint modes)
      const onChainMetadata = {
        name: stringToBytes(tokenName),
        description: stringToBytes(description),
        artifactUri: stringToBytes(artifactUri),
        displayUri: stringToBytes(artifactUri),
        thumbnailUri: stringToBytes(thumbnailUri),
        decimals: stringToBytes("0"),
        symbol: stringToBytes("KEEP"),
        isBooleanAmount: stringToBytes("true"),
        shouldPreferSymbol: stringToBytes("false"),
        formats: stringToBytes(JSON.stringify(metadataJson.formats)),
        tags: stringToBytes(JSON.stringify(tags)),
        attributes: stringToBytes(JSON.stringify(attributes)),
        creators: stringToBytes(JSON.stringify(creatorsArray)),
        rights: stringToBytes("© All rights reserved"),
        content_type: stringToBytes("KidLisp"),
        content_hash: stringToBytes(pieceName),
        metadata_uri: stringToBytes(metadataUri),
      };

      // PREPARE MODE: Return data for client-side wallet minting
      if (mode === "prepare") {
        await send("progress", { stage: "ready", message: "Ready for wallet signature..." });
        
        // Use Taquito to generate the Michelson parameters for the contract call
        // This ensures proper encoding that Beacon can use
        const tezos = new TezosToolkit(RPC_URL);
        const contract = await tezos.contract.at(CONTRACT_ADDRESS);
        
        // Use the wallet address validated earlier
        const ownerAddress = creatorWalletAddress;
        
        const transferParams = contract.methodsObject.keep({
          artifactUri: onChainMetadata.artifactUri,
          attributes: onChainMetadata.attributes,
          content_hash: onChainMetadata.content_hash,
          content_type: onChainMetadata.content_type,
          creators: onChainMetadata.creators,
          decimals: onChainMetadata.decimals,
          description: onChainMetadata.description,
          displayUri: onChainMetadata.displayUri,
          formats: onChainMetadata.formats,
          isBooleanAmount: onChainMetadata.isBooleanAmount,
          metadata_uri: onChainMetadata.metadata_uri,
          name: onChainMetadata.name,
          owner: ownerAddress,
          rights: onChainMetadata.rights,
          shouldPreferSymbol: onChainMetadata.shouldPreferSymbol,
          symbol: onChainMetadata.symbol,
          tags: onChainMetadata.tags,
          thumbnailUri: onChainMetadata.thumbnailUri,
        }).toTransferParams();
        
        await send("prepared", {
          success: true,
          piece: pieceName,
          contractAddress: CONTRACT_ADDRESS,
          network: NETWORK,
          mintFee: 0, // Contract keep_fee is 0 (free minting)
          // Send the Michelson-encoded parameters for Beacon
          michelsonParams: transferParams.parameter,
          entrypoint: "keep",
          artifactUri,
          thumbnailUri,
          metadataUri,
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
      
      const op = await contract.methodsObject.keep({
        artifactUri: onChainMetadata.artifactUri,
        attributes: onChainMetadata.attributes,
        content_hash: onChainMetadata.content_hash,
        content_type: onChainMetadata.content_type,
        creators: onChainMetadata.creators,
        decimals: onChainMetadata.decimals,
        description: onChainMetadata.description,
        displayUri: onChainMetadata.displayUri,
        formats: onChainMetadata.formats,
        isBooleanAmount: onChainMetadata.isBooleanAmount,
        metadata_uri: onChainMetadata.metadata_uri,
        name: onChainMetadata.name,
        owner: destinationAddress,
        rights: onChainMetadata.rights,
        shouldPreferSymbol: onChainMetadata.shouldPreferSymbol,
        symbol: onChainMetadata.symbol,
        tags: onChainMetadata.tags,
        thumbnailUri: onChainMetadata.thumbnailUri,
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
      const piecesCollection = database.db.collection("kidlisp");
      await piecesCollection.updateOne(
        { user: user.sub, code: pieceName },
        { 
          $set: { 
            "tezos.minted": true,
            "tezos.tokenId": tokenId,
            "tezos.txHash": op.hash,
            "tezos.contract": CONTRACT_ADDRESS,
            "tezos.network": NETWORK,
            "tezos.mintedAt": new Date(),
            "tezos.owner": destinationAddress,
            "tezos.artifactUri": artifactUri,
            "tezos.thumbnailUri": thumbnailUri,
            "tezos.metadataUri": metadataUri,
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
