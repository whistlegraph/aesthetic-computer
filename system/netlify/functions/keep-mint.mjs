// keep-mint.mjs - Streaming NFT minting endpoint for KidLisp Keeps
// 
// POST /api/keep-mint - Mint a piece as an NFT (streaming SSE response)
// Requires authentication and piece ownership
//
// Optimized flow:
// 1. Validate auth/ownership/not-minted
// 2. START thumbnail generation in parallel (oven)
// 3. Analyze source for character count
// 4. Generate bundle (bundle-html)
// 5. Upload bundle to IPFS
// 6. AWAIT thumbnail
// 7. Upload metadata JSON to IPFS
// 8. Mint on Tezos

import { authorize, handleFor, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { analyzeKidLisp } from "../../backend/kidlisp-analyzer.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";
import { stream } from "@netlify/functions";
import { TezosToolkit } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";
import { packDataBytes } from "@taquito/michel-codec";

const dev = process.env.CONTEXT === "dev";

// Allow self-signed certs in dev mode
if (dev) {
  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";
}

// Configuration
let CONTRACT_ADDRESS = LEGACY_KEEPS_CONTRACT;
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const OVEN_FALLBACK_URL = "https://oven.aesthetic.computer"; // Always available fallback
const RPC_URL = NETWORK === "mainnet" 
  ? "https://mainnet.ecadinfra.com"
  : "https://ghostnet.ecadinfra.com";

function envInt(name, fallback) {
  const parsed = Number.parseInt(process.env[name] || "", 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

// Keep this function under provider execution ceilings so SSE can emit terminal events.
const KEEP_MINT_MAX_PREP_MS = envInt("KEEP_MINT_MAX_PREP_MS", dev ? 55000 : 24000);
const KEEP_MINT_PROVIDER_CEILING_MS = envInt("KEEP_MINT_PROVIDER_CEILING_MS", dev ? 55000 : 24000);
const KEEP_MINT_EFFECTIVE_PREP_MS = Math.min(KEEP_MINT_MAX_PREP_MS, KEEP_MINT_PROVIDER_CEILING_MS);
const KEEP_MINT_STREAM_GUARD_MS = envInt("KEEP_MINT_STREAM_GUARD_MS", 1500);
const KEEP_MINT_THUMBNAIL_TIMEOUT_MS = envInt("KEEP_MINT_THUMBNAIL_TIMEOUT_MS", dev ? 45000 : 22000);
const KEEP_MINT_FORCE_FRESH_THUMBNAIL_AWAIT_MS = envInt("KEEP_MINT_FORCE_FRESH_THUMBNAIL_AWAIT_MS", dev ? 25000 : 9000);
const KEEP_MINT_PERMIT_TTL_MS = envInt("KEEP_MINT_PERMIT_TTL_MS", 1_200_000); // 20 minutes
const KEEP_MINT_SIGNER_CACHE_TTL_MS = envInt("KEEP_MINT_SIGNER_CACHE_TTL_MS", 30000); // 30 seconds

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
let cachedTezosCredentialsExpiresAt = 0;

const KEEP_PERMIT_PAYLOAD_TYPE = {
  prim: "pair",
  args: [
    { prim: "address", annots: ["%contract"] },
    {
      prim: "pair",
      args: [
        { prim: "address", annots: ["%owner"] },
        {
          prim: "pair",
          args: [
            { prim: "bytes", annots: ["%content_hash"] },
            { prim: "timestamp", annots: ["%permit_deadline"] },
          ],
        },
      ],
    },
  ],
};

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
  if (cachedTezosCredentials && Date.now() < cachedTezosCredentialsExpiresAt) {
    return cachedTezosCredentials;
  }
  
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "tezos-kidlisp" });
  
  if (!secrets) {
    throw new Error("Tezos KidLisp credentials not found in database");
  }

  const signerPrivateKey = secrets.keepPermitSignerPrivateKey || secrets.keepPermitPrivateKey || secrets.privateKey;
  const signerPublicKey = secrets.keepPermitSignerPublicKey || secrets.keepPermitPublicKey || secrets.publicKey;
  const signerAddress = secrets.keepPermitSignerAddress || secrets.keepPermitAddress || secrets.address;

  if (!signerPrivateKey || !signerPublicKey || !signerAddress) {
    throw new Error("Tezos KidLisp signer credentials are incomplete in secrets.tezos-kidlisp");
  }
  
  cachedTezosCredentials = {
    address: signerAddress,
    publicKey: signerPublicKey,
    privateKey: signerPrivateKey,
    network: secrets.network,
    treasuryAddress: secrets.treasuryAddress || null,
  };
  cachedTezosCredentialsExpiresAt = Date.now() + KEEP_MINT_SIGNER_CACHE_TTL_MS;
  
  return cachedTezosCredentials;
}

// Convert string to hex bytes (for Tezos)
function stringToBytes(str) {
  return Buffer.from(str, "utf8").toString("hex");
}

async function buildKeepPermit({
  privateKey,
  contractAddress,
  ownerAddress,
  contentHashBytes,
  permitDeadline = null,
}) {
  if (!privateKey || !contractAddress || !ownerAddress || !contentHashBytes) {
    throw new Error("Missing required keep permit fields");
  }

  const deadlineIso = permitDeadline || new Date(Date.now() + KEEP_MINT_PERMIT_TTL_MS).toISOString();
  const payloadData = {
    prim: "Pair",
    args: [
      { string: contractAddress },
      {
        prim: "Pair",
        args: [
          { string: ownerAddress },
          {
            prim: "Pair",
            args: [
              { bytes: contentHashBytes },
              { string: deadlineIso },
            ],
          },
        ],
      },
    ],
  };

  const packed = packDataBytes(payloadData, KEEP_PERMIT_PAYLOAD_TYPE).bytes;
  const signer = new InMemorySigner(privateKey);
  const signature = await signer.sign(packed);

  return {
    permit_deadline: deadlineIso,
    keep_permit: signature.prefixSig,
  };
}

function normalizeRoyaltyBps(value, fallback = 1000) {
  const raw = value?.toNumber?.() ?? value;
  const parsed = Number(raw);
  if (!Number.isFinite(parsed)) return fallback;
  const bounded = Math.max(0, Math.min(2500, Math.trunc(parsed)));
  return bounded;
}

function buildRoyalties(creatorAddress, artistBps, platformBps, platformAddress) {
  const shares = { [creatorAddress]: String(artistBps) };
  if (platformBps > 0 && platformAddress) {
    shares[platformAddress] = String(platformBps);
  }
  return { decimals: 4, shares };
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
          objktUrl: `https://${NETWORK === "mainnet" ? "" : "ghostnet."}objkt.com/tokens/${CONTRACT_ADDRESS}/${tokenId}`,
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

function isTezosAddress(value) {
  return typeof value === "string" && /^tz[1-3][1-9A-HJ-NP-Za-km-z]{33}$/.test(value.trim());
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
async function uploadToIPFS(content, filename, mimeType = "text/html", onProgress = null, timeoutMs = 60000) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const formData = new FormData();
  const blob = new Blob([content], { type: mimeType });
  formData.append("file", blob, filename);
  formData.append("pinataMetadata", JSON.stringify({ name: filename }));
  // Don't wrap with directory - we want direct access to the HTML file
  formData.append("pinataOptions", JSON.stringify({ wrapWithDirectory: false }));

  // Add timeout to prevent hanging on slow Pinata responses
  const controller = new AbortController();
  const safeTimeoutMs = Math.max(3000, timeoutMs);
  const timeout = setTimeout(() => controller.abort(), safeTimeoutMs);
  
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
      throw new Error(`IPFS upload timed out after ${Math.round(safeTimeoutMs / 1000)}s. Pinata may be slow. Please try again.`);
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
async function uploadJsonToIPFS(data, name, timeoutMs = 30000) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  // Add timeout to prevent hanging on slow Pinata responses
  const controller = new AbortController();
  const safeTimeoutMs = Math.max(3000, timeoutMs);
  const timeout = setTimeout(() => controller.abort(), safeTimeoutMs);
  
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
      throw new Error(`Metadata upload timed out after ${Math.round(safeTimeoutMs / 1000)}s. Pinata may be slow. Please try again.`);
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

  const getRemainingPrepMs = () => KEEP_MINT_EFFECTIVE_PREP_MS - (Date.now() - processStartTime);
  const getStageBudgetMs = (maxMs) => {
    const remaining = getRemainingPrepMs() - KEEP_MINT_STREAM_GUARD_MS;
    if (remaining <= 0) return 0;
    return Math.max(0, Math.min(maxMs, remaining));
  };
  
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

      let pieceName = body.piece?.replace(/^\$/, "").trim();
      const mode = body.mode || "prepare"; // "prepare" (default) or "simulate"
      const regenerate = body.regenerate === true; // Force regenerate IPFS media
      const walletAddress = body.walletAddress; // For on-chain owner verification

      // Simulator mode can run without a real piece, useful for launch dry-runs.
      if (!pieceName && mode === "simulate") {
        pieceName = "sim-v9";
      }
      
      if (!pieceName) {
        await send("error", { error: "Missing 'piece' in request body" });
        return; // finally will close
      }

      CONTRACT_ADDRESS = await getKeepsContractAddress({
        network: NETWORK,
        fallback: LEGACY_KEEPS_CONTRACT,
      });

      // ═══════════════════════════════════════════════════════════════════
      // SIMULATOR MODE: Build metadata + contract call scaffold without minting
      // ═══════════════════════════════════════════════════════════════════
      if (mode === "simulate") {
        let database = null;
        try {
          const providedSource = typeof body.source === "string" ? body.source.trim() : "";
          const normalizedHandleRaw = typeof body.handle === "string" ? body.handle.trim() : "";
          const providedUserCodeRaw = typeof body.userCode === "string" ? body.userCode.trim() : "";
          const simulatorUserCode = providedUserCodeRaw || null;
          const authorHandle = normalizedHandleRaw
            ? (normalizedHandleRaw.startsWith("@") ? normalizedHandleRaw : `@${normalizedHandleRaw}`)
            : "@simulator";

          const walletValid = isTezosAddress(walletAddress);
          const creatorWalletAddress = walletValid
            ? walletAddress.trim()
            : "tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC";

          const packDate = typeof body.packedOn === "string" && body.packedOn.trim()
            ? body.packedOn.trim()
            : new Date().toISOString().slice(0, 10);

          await send("progress", { stage: "validate", message: `Simulator mode for $${pieceName}` });

          let source = providedSource;
          let sourceOrigin = providedSource ? "request" : "fallback";
          let dbPieceMeta = null;

          if (!source) {
            database = await connect();
            const collection = database.db.collection("kidlisp");
            const piece = await collection.findOne({ code: pieceName });
            if (piece?.source) {
              source = piece.source;
              sourceOrigin = "database";
              dbPieceMeta = {
                authorSub: piece.user || null,
                createdAt: piece.createdAt || piece.when || null,
              };
              await send("progress", { stage: "validate", message: `Loaded source for $${pieceName} from kidlisp cache` });
            }
          }

          if (!source) {
            source = `(wipe)\n(color 255 105 180)\n(write "$${pieceName}")\n(circle 64 64 28)\n`;
            await send("progress", { stage: "validate", message: "No source found; using built-in simulator fallback source" });
          }

          await send("progress", { stage: "validate", message: "Validation passed ✓" });
          await send("progress", {
            stage: "details",
            piece: pieceName,
            source,
            author: authorHandle,
            sourceLength: source.length,
            sourceOrigin,
          });

          await send("progress", { stage: "analyze", message: "Analyzing source code..." });
          const analysis = analyzeKidLisp(source);
          await send("progress", { stage: "analyze", message: `${analysis.chars || source.length} chars ✓` });

          await send("progress", { stage: "bundle", message: "Simulating bundle scaffold..." });
          const sourceHash = hashSource(source);
          const sourceHashSafe = sourceHash.replace(/-/g, "x");
          const cidSeed = Buffer.from(`${pieceName}:${sourceHashSafe}`, "utf8").toString("hex").slice(0, 40).padEnd(40, "0");
          const artifactCid = `bafybeisim${cidSeed}`;
          const thumbCid = `bafybeisimthumb${cidSeed.slice(0, 35)}`;
          const metaCid = `bafybeisimmeta${cidSeed.slice(0, 36)}`;
          const artifactUri = `ipfs://${artifactCid}/$${pieceName}.html`;
          const thumbnailUri = `ipfs://${thumbCid}/$${pieceName}.webp`;
          const metadataUri = `ipfs://${metaCid}/$${pieceName}-metadata.json`;

          await send("progress", { stage: "bundle", message: `Scaffolded ${Math.max(1, Math.round(source.length / 1024))}KB bundle` });
          await send("progress", { stage: "ipfs", message: "Simulator mode: skipping Pinata writes" });
          await send("progress", { stage: "thumbnail", message: "Simulator mode: skipping oven render" });
          await send("progress", { stage: "metadata", message: "Building metadata payload..." });

          const tokenName = `$${pieceName}`;
          const description = source || "A KidLisp piece preserved on Tezos";
          const tags = ["KidLisp"];
          const attributes = [
            { name: "Characters", value: String(analysis?.chars || source.length) },
          ];

          let artistBps = 900;
          let platformBps = 100;
          try {
            const tezos = new TezosToolkit(RPC_URL);
            const contract = await tezos.contract.at(CONTRACT_ADDRESS);
            const storage = await contract.storage();
            if (storage.artist_royalty_bps !== undefined) {
              artistBps = normalizeRoyaltyBps(storage.artist_royalty_bps, 900);
              platformBps = normalizeRoyaltyBps(storage.platform_royalty_bps, 100);
            } else {
              // Legacy contract — split default_royalty_bps as all-artist
              artistBps = normalizeRoyaltyBps(storage.default_royalty_bps, 1000);
              platformBps = 0;
            }
          } catch (royaltyErr) {
            console.warn(`🪙 KEEP: Unable to read royalty bps, using defaults (${royaltyErr?.message || "unknown error"})`);
          }

          const creatorsArray = [creatorWalletAddress];
          const simCredentials = await getTezosCredentials();
          const royalties = buildRoyalties(creatorWalletAddress, artistBps, platformBps, simCredentials.treasuryAddress);

          const metadataJson = {
            name: tokenName,
            description,
            artifactUri,
            displayUri: artifactUri,
            thumbnailUri,
            ...(simulatorUserCode ? { permauser: simulatorUserCode } : {}),
            decimals: 0,
            symbol: pieceName,
            isBooleanAmount: true,
            shouldPreferSymbol: false,
            minter: `@${(authorHandle || "anon").replace(/^@/, "")}`,
            creators: creatorsArray,
            royalties,
            rights: "© All rights reserved",
            mintingTool: "https://kidlisp.com",
            formats: [{
              uri: artifactUri,
              mimeType: "text/html",
              dimensions: { value: "responsive", unit: "viewport" },
            }],
            tags,
            attributes,
          };

          const onChainMetadata = {
            name: stringToBytes(tokenName),
            symbol: stringToBytes(pieceName),
            description: stringToBytes(source || ""),
            artifactUri: stringToBytes(artifactUri),
            displayUri: stringToBytes(artifactUri),
            thumbnailUri: stringToBytes(thumbnailUri),
            decimals: stringToBytes("0"),
            creators: stringToBytes(JSON.stringify(creatorsArray)),
            royalties: stringToBytes(JSON.stringify(royalties)),
            content_hash: stringToBytes(pieceName),
            metadata_uri: stringToBytes(metadataUri),
          };

          await send("progress", { stage: "metadata", message: "Metadata payload ready ✓" });
          await send("progress", { stage: "contract", message: "Building contract call scaffold..." });

          let michelsonParams = null;
          let keepFeeXtz = null;
          let scaffoldError = null;
          let contractCallPreview = null;
          let keepPermit = null;
          let keepPermitError = null;

          try {
            try {
              const tezosCredentials = await getTezosCredentials();
              // Simulation never emits a production-usable permit. We sign an
              // intentionally mismatched payload so clients can still scaffold
              // Michelson params without exposing valid authorization.
              const simulationOnlyHash = `${onChainMetadata.content_hash}00`;
              keepPermit = await buildKeepPermit({
                privateKey: tezosCredentials.privateKey,
                contractAddress: CONTRACT_ADDRESS,
                ownerAddress: creatorWalletAddress,
                contentHashBytes: simulationOnlyHash,
              });
            } catch (permitErr) {
              keepPermitError = permitErr?.message || "Keep permit unavailable";
            }

            const tezos = new TezosToolkit(RPC_URL);
            const contract = await tezos.contract.at(CONTRACT_ADDRESS);
            const storage = await contract.storage();
            const keepFeeMutez = storage.keep_fee?.toNumber?.() ?? 0;
            keepFeeXtz = keepFeeMutez / 1_000_000;

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
              ...(keepPermit || {}),
            }).toTransferParams();

            michelsonParams = transferParams.parameter || null;
            contractCallPreview = {
              destination: transferParams.to || CONTRACT_ADDRESS,
              amountMutez: transferParams.amount?.toString?.() || String(transferParams.amount ?? 0),
              entrypoint: transferParams.parameter?.entrypoint || "keep",
            };

            await send("progress", { stage: "contract", message: `Scaffold ready • keep_fee=${keepFeeXtz} XTZ` });
          } catch (err) {
            scaffoldError = err?.message || "Failed to build contract scaffold";
            await send("progress", { stage: "contract", message: `Scaffold warning: ${scaffoldError}` });
          }

          await send("progress", { stage: "ready", message: "Simulation complete ✓" });
          await send("simulated", {
            success: true,
            mode: "simulate",
            piece: pieceName,
            contractAddress: CONTRACT_ADDRESS,
            network: NETWORK,
            rpcUrl: RPC_URL,
            mintFee: keepFeeXtz,
            entrypoint: "keep",
            artifactUri,
            thumbnailUri,
            metadataUri,
            packDate,
            metadataJson,
            onChainMetadata,
            michelsonParams,
            contractCallPreview,
            simulation: {
              sourceOrigin,
              sourceLength: source.length,
              sourceHash,
              walletFallbackUsed: !walletValid,
              scaffoldOk: !!michelsonParams,
              scaffoldError,
              keepPermitSigned: !!keepPermit,
              keepPermitError,
              keepPermitUsableOnChain: false,
              dbPieceMeta,
            },
            keepPermit,
          });
          return;
        } finally {
          if (database) {
            try {
              await database.disconnect();
            } catch {}
          }
        }
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
        
        // Wallet address must be provided AND match linked wallet
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
      const forceFreshMedia = Boolean(regenerate);

      // Fixed 256x256 thumbnail for consistent display across platforms
      const thumbW = 256;
      const thumbH = 256;
      // Rebake can use a shorter thumbnail capture profile for faster turnaround.
      const thumbDurationMs = forceFreshMedia ? 6000 : 8000;
      const thumbCaptureFps = forceFreshMedia ? 8 : 10;
      const thumbPlaybackFps = forceFreshMedia ? 16 : 20;

      // Thumbnail generation timeout (kept under stream execution budget in production)
      const THUMBNAIL_TIMEOUT_MS = KEEP_MINT_THUMBNAIL_TIMEOUT_MS;

      if (!useCachedMedia) {
        const initialThumbBudgetMs = getStageBudgetMs(THUMBNAIL_TIMEOUT_MS);
        if (initialThumbBudgetMs < 2500) {
          throw new Error("Preparation time budget exhausted before thumbnail bake started");
        }

        logStage('thumbnail', `Starting ${thumbW * 2}x${thumbH * 2} WebP generation (budget ${Math.ceil(initialThumbBudgetMs / 1000)}s)`);
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
              duration: thumbDurationMs,
              fps: thumbCaptureFps,
              playbackFps: thumbPlaybackFps,
              quality: 70,
              source: "keep",
              keepId: isRebake ? mintStatus.tokenId : null,
              // Rebake requests must bypass stale oven caches.
              cacheKey: forceFreshMedia
                ? `rebake-${pieceSourceHash}-${Date.now()}`
                : `src-${pieceSourceHash}`,
              skipCache: forceFreshMedia,
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

      // Try local oven first, then fallback to production.
      const thumbnailGenerationPromise = !useCachedMedia ? (async () => {
        try {
          const firstAttemptBudgetMs = getStageBudgetMs(THUMBNAIL_TIMEOUT_MS);
          if (firstAttemptBudgetMs < 2500) {
            throw new Error("Not enough time left to start thumbnail bake");
          }
          const result = await tryOvenThumbnail(OVEN_URL, firstAttemptBudgetMs);
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
              const fallbackBudgetMs = getStageBudgetMs(THUMBNAIL_TIMEOUT_MS);
              if (fallbackBudgetMs < 2500) {
                throw new Error("Not enough time left for fallback oven attempt");
              }
              const fallbackResult = await tryOvenThumbnail(OVEN_FALLBACK_URL, fallbackBudgetMs);
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
      thumbnailPromise = thumbnailGenerationPromise;
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 3: ANALYZE SOURCE
      // ═══════════════════════════════════════════════════════════════════
      let analysis = null;
      if (isRebake) {
        await send("progress", { stage: "analyze", message: "Skipping analysis for rebake" });
      } else {
        await send("progress", { stage: "analyze", message: "Analyzing source code..." });
        analysis = analyzeKidLisp(piece.source);
        await send("progress", {
          stage: "analyze",
          message: `${analysis.chars} chars ✓`
        });
      }

      // ═══════════════════════════════════════════════════════════════════
      // STAGE 4: GENERATE BUNDLE (skip if cached)
      // ═══════════════════════════════════════════════════════════════════
      let bundleHtml, bundleFilename, authorHandle, userCode, packDate, depCount;
      
      if (!useCachedMedia) {
        logStage('bundle', 'Generating HTML bundle');
        await send("progress", { stage: "bundle", message: "Packing HTML bundle..." });
        
        let bundleUrl = dev
          ? `https://localhost:8888/api/bundle-html?code=${pieceName}&format=json&noboxart=1`
          : `https://oven.aesthetic.computer/bundle-html?code=${pieceName}&format=json&noboxart=1`;
        if (forceFreshMedia) {
          bundleUrl += `&rebake=1&nocache=1&sourceHash=${encodeURIComponent(pieceSourceHash)}&ts=${Date.now()}`;
        }
        
        // Add timeout to prevent function from hanging if bundle-html is slow
        // Bundle generation can take 15-30s on cold starts due to SWC minification.
        // Bound by remaining stream budget so we can still emit a terminal SSE event.
        const bundleBudgetMs = getStageBudgetMs(40000);
        if (bundleBudgetMs < 2500) {
          throw new Error("Preparation time budget exhausted before bundle generation");
        }
        const bundleController = new AbortController();
        const bundleTimeout = setTimeout(() => bundleController.abort(), bundleBudgetMs);
        
        console.log(`🪙 KEEP: Fetching bundle from ${bundleUrl}...`);
        await send("progress", { stage: "bundle", message: "Requesting bundle from API..." });
        const bundleStartTime = Date.now();

        let bundleResponse;
        try {
          bundleResponse = await fetch(bundleUrl, {
            signal: bundleController.signal,
            cache: "no-store",
            headers: forceFreshMedia ? { "Cache-Control": "no-cache" } : undefined,
          });
        } catch (fetchErr) {
          clearTimeout(bundleTimeout);
          const elapsed = ((Date.now() - bundleStartTime) / 1000).toFixed(1);
          console.error(`🪙 KEEP: Bundle fetch failed after ${elapsed}s:`, fetchErr.message);
          await send("progress", { stage: "bundle", message: `Fetch error after ${elapsed}s: ${fetchErr.message}` });
          if (fetchErr.name === "AbortError") {
            throw new Error(`Bundle generation timed out after ${Math.round(bundleBudgetMs / 1000)}s. The server may be cold-starting. Please try again in a moment.`);
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
        userCode = bundleData.userCode || userDoc?.code || null;
        packDate = bundleData.packDate;
        depCount = bundleData.depCount || 0;

        // Calculate bundle size
        const bundleSize = Math.round(bundleHtml.length / 1024);
        console.log(`🪙 KEEP: Bundle ready: ${bundleSize}KB with ${depCount} deps`);
        await send("progress", { stage: "bundle", message: `Packed ${bundleSize}KB · ${depCount} deps` });
      } else {
        // Use cached values
        authorHandle = piece.ipfsMedia.authorHandle || `@${userHandle}`;
        userCode = piece.ipfsMedia.userCode || userDoc?.code || null;
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
        const ipfsBudgetMs = getStageBudgetMs(60000);
        if (ipfsBudgetMs < 2500) {
          throw new Error("Preparation time budget exhausted before IPFS upload");
        }
        
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
          onIPFSProgress,
          ipfsBudgetMs
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
        let thumbnailAwaitBudgetMs = getStageBudgetMs(THUMBNAIL_TIMEOUT_MS);
        if (forceFreshMedia) {
          thumbnailAwaitBudgetMs = Math.min(thumbnailAwaitBudgetMs, KEEP_MINT_FORCE_FRESH_THUMBNAIL_AWAIT_MS);
        }
        const preexistingThumbnailUri = piece?.ipfsMedia?.thumbnailUri || mintStatus?.thumbnailUri || null;
        if (thumbnailAwaitBudgetMs < 2500) {
          if (forceFreshMedia && preexistingThumbnailUri) {
            thumbnailUri = preexistingThumbnailUri;
            console.warn("🪙 KEEP: Low prep budget before thumbnail wait; reusing previous thumbnail");
            await send("progress", {
              stage: "thumbnail",
              message: "Low time budget, reusing previous thumbnail",
            });
          } else {
            throw new Error("Preparation time budget exhausted while waiting for thumbnail");
          }
        } else {
        await send("progress", {
          stage: "thumbnail",
          message: `Awaiting ${thumbW}x${thumbH}@2x WebP... (${Math.round(thumbnailAwaitBudgetMs / 1000)}s budget)`
        });

        // Send heartbeat messages to keep the SSE connection alive while
        // the oven generates the thumbnail.
        const heartbeat = setInterval(async () => {
          try {
            const elapsed = ((Date.now() - processStartTime) / 1000).toFixed(0);
            await send("progress", { stage: "thumbnail", message: `Still baking… (${elapsed}s)` });
          } catch {}
        }, 10000);

        let thumbResult;
        let thumbnailWaitError = null;
        try {
          thumbResult = await Promise.race([
            thumbnailPromise,
            new Promise((_, reject) => {
              setTimeout(() => {
                reject(new Error(`Thumbnail generation timed out after ${Math.round(thumbnailAwaitBudgetMs / 1000)}s`));
              }, thumbnailAwaitBudgetMs);
            }),
          ]);
        } catch (thumbErr) {
          thumbnailWaitError = thumbErr;
        } finally {
          clearInterval(heartbeat);
        }

        if (!thumbResult?.ipfsUri) {
          const fallbackThumbnailUri = forceFreshMedia
            ? preexistingThumbnailUri
            : null;

          if (fallbackThumbnailUri) {
            thumbnailUri = fallbackThumbnailUri;
            const fallbackReason = thumbnailWaitError?.message || thumbResult?.error || "unknown";
            console.warn(`🪙 KEEP: Thumbnail bake fallback for regenerate path: ${fallbackReason}`);
            await send("progress", {
              stage: "thumbnail",
              message: "Bake timeout, reusing previous thumbnail",
            });
          } else {
            const errorMsg = thumbnailWaitError?.message || thumbResult?.error || "unknown error";
            console.error(`🪙 KEEP: Thumbnail failed:`, errorMsg);
            throw new Error(`Thumbnail generation failed - ${errorMsg}`);
          }
        } else {
          thumbnailUri = thumbResult.ipfsUri;
          const thumbHash = thumbnailUri.replace("ipfs://", "");

          await send("progress", {
            stage: "thumbnail",
            message: `Baked ${thumbHash.slice(0, 12)}..`
          });
        }
        }

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

      // v9 metadata policy: single canonical tag only
      const tags = ["KidLisp"];

      const attributes = [
        { name: "Characters", value: String(analysis?.chars || (piece.source || "").length) },
      ];

      // Creator identity for metadata
      // objkt.com uses firstMinter for artist attribution
      // creators array contains just the wallet address for on-chain attribution
      const creatorsArray = [creatorWalletAddress];
      
      let artistBps = 900;
      let platformBps = 100;
      try {
        const readTezos = new TezosToolkit(RPC_URL);
        const readContract = await readTezos.contract.at(CONTRACT_ADDRESS);
        const readStorage = await readContract.storage();
        if (readStorage.artist_royalty_bps !== undefined) {
          artistBps = normalizeRoyaltyBps(readStorage.artist_royalty_bps, 900);
          platformBps = normalizeRoyaltyBps(readStorage.platform_royalty_bps, 100);
        } else {
          // Legacy contract — split default_royalty_bps as all-artist
          artistBps = normalizeRoyaltyBps(readStorage.default_royalty_bps, 1000);
          platformBps = 0;
        }
      } catch (royaltyErr) {
        console.warn(`🪙 KEEP: Unable to read royalty bps, using defaults (${royaltyErr?.message || "unknown error"})`);
      }

      // Royalty split read from on-chain storage at mint time.
      const mintCredentials = await getTezosCredentials();
      const royalties = buildRoyalties(creatorWalletAddress, artistBps, platformBps, mintCredentials.treasuryAddress);

      const metadataJson = {
        name: tokenName,
        description,
        artifactUri,
        displayUri: artifactUri,
        thumbnailUri,
        ...(userCode ? { permauser: userCode } : {}),
        decimals: 0,
        symbol: pieceName,
        isBooleanAmount: true,
        shouldPreferSymbol: false,
        minter: `@${(authorHandle || "anon").replace(/^@/, "")}`,
        creators: creatorsArray,
        royalties,  // v4: Add royalty configuration
        rights: "© All rights reserved",
        mintingTool: "https://kidlisp.com",
        formats: [{
          uri: artifactUri,
          mimeType: "text/html",
          dimensions: { value: "responsive", unit: "viewport" },
        }],
        tags,
        attributes,
      };

      const metadataBudgetMs = getStageBudgetMs(30000);
      if (metadataBudgetMs < 2500) {
        throw new Error("Preparation time budget exhausted before metadata upload");
      }

      metadataUri = await uploadJsonToIPFS(
        metadataJson,
        `$${pieceName}-metadata.json`,
        metadataBudgetMs
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
        const tezosCredentials = await getTezosCredentials();
        const keepPermit = await buildKeepPermit({
          privateKey: tezosCredentials.privateKey,
          contractAddress: CONTRACT_ADDRESS,
          ownerAddress,
          contentHashBytes: onChainMetadata.content_hash,
        });
        
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
          permit_deadline: keepPermit.permit_deadline,
          keep_permit: keepPermit.keep_permit,
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
          keepPermitDeadline: keepPermit.permit_deadline,
          usedCachedMedia: useCachedMedia, // Tell client if we reused IPFS pins
          cacheGeneratedAt: useCachedMedia ? piece.ipfsMedia?.createdAt : null, // When cache was generated
        });
        return;
      }

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
