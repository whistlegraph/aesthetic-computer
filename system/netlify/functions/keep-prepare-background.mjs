// keep-prepare-background.mjs — Netlify Background Function for keep pipeline.
//
// Runs the heavy preparation work (oven bake, IPFS upload, metadata).
// Background functions return 202 immediately and run for up to 15 minutes.
// All progress is written to MongoDB via the keep-job model.
//
// Invoked by keep-prepare.mjs after job creation and validation.

import { connect } from "../../backend/database.mjs";
import { analyzeKidLisp } from "../../backend/kidlisp-analyzer.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";
import { handleFor } from "../../backend/authorization.mjs";
import {
  updateJobStage,
  setJobResult,
  markJobReady,
  markJobFailed,
  getJobById,
} from "../../backend/keep-job.mjs";
import { TezosToolkit } from "@taquito/taquito";
import { InMemorySigner } from "@taquito/signer";
import { packDataBytes } from "@taquito/michel-codec";
import { getPkhfromPk } from "@taquito/utils";
import { createHash } from "crypto";

const dev = process.env.CONTEXT === "dev";
if (dev) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

// ─── Configuration ───────────────────────────────────────────────────────────
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const OVEN_FALLBACK_URL = "https://oven.aesthetic.computer";
const RPC_URL = NETWORK === "mainnet"
  ? "https://mainnet.ecadinfra.com"
  : "https://ghostnet.ecadinfra.com";

function envInt(name, fallback) {
  const parsed = Number.parseInt(process.env[name] || "", 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

const KEEP_MINT_THUMBNAIL_TIMEOUT_MS = envInt("KEEP_MINT_THUMBNAIL_TIMEOUT_MS", 150000);
const KEEP_MINT_PERMIT_TTL_MS = envInt("KEEP_MINT_PERMIT_TTL_MS", 1_200_000);
const KEEP_MINT_SIGNER_CACHE_TTL_MS = envInt("KEEP_MINT_SIGNER_CACHE_TTL_MS", 30000);
const KEEP_MINT_SECURITY_SCAN_LIMIT = envInt("KEEP_MINT_SECURITY_SCAN_LIMIT", 30);
const KEEP_MINT_MIN_EXPECTED_FEE_MUTEZ = envInt("KEEP_MINT_MIN_EXPECTED_FEE_MUTEZ", 1);
const KEEP_MINT_BLOCK_ON_ALERT = process.env.KEEP_MINT_BLOCK_ON_ALERT === "true";
const KEEP_MINT_STRICT_PREFLIGHT = process.env.KEEP_MINT_STRICT_PREFLIGHT !== "false";

const DEFAULT_MAINNET_CONTRACT = "KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB";
const DEFAULT_MAINNET_ADMIN = "tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC";
const DEFAULT_MAINNET_CODE_HASH = 1692834636;
const DEFAULT_MAINNET_TYPE_HASH = 399679480;

const KEEP_MINT_EXPECTED_CONTRACT =
  process.env.KEEP_MINT_EXPECTED_CONTRACT?.trim()
  || (NETWORK === "mainnet" ? DEFAULT_MAINNET_CONTRACT : null);
const KEEP_MINT_EXPECTED_ADMIN =
  process.env.KEEP_MINT_EXPECTED_ADMIN?.trim()
  || (NETWORK === "mainnet" ? DEFAULT_MAINNET_ADMIN : null);
const KEEP_MINT_EXPECTED_PERMIT_SIGNER =
  process.env.KEEP_MINT_EXPECTED_PERMIT_SIGNER?.trim()
  || KEEP_MINT_EXPECTED_ADMIN;
const KEEP_MINT_EXPECTED_CODE_HASH = Number.parseInt(
  process.env.KEEP_MINT_EXPECTED_CODE_HASH || `${NETWORK === "mainnet" ? DEFAULT_MAINNET_CODE_HASH : ""}`, 10
);
const KEEP_MINT_EXPECTED_TYPE_HASH = Number.parseInt(
  process.env.KEEP_MINT_EXPECTED_TYPE_HASH || `${NETWORK === "mainnet" ? DEFAULT_MAINNET_TYPE_HASH : ""}`, 10
);

const USE_GATEWAY_URLS = process.env.USE_IPFS_GATEWAY_URLS === "true";
const IPFS_GATEWAY = process.env.IPFS_GATEWAY || "https://ipfs.aesthetic.computer";

const ADMIN_ENTRYPOINTS = new Set([
  "set_administrator", "set_contract_metadata", "lock_contract_metadata",
  "set_keep_fee", "set_treasury", "set_royalty_split",
  "pause", "unpause", "withdraw_fees",
]);

// ─── Credential caches ──────────────────────────────────────────────────────
let cachedPinataCredentials = null;
let cachedTezosCredentials = null;
let cachedTezosCredentialsExpiresAt = 0;

// ─── Helpers ─────────────────────────────────────────────────────────────────
function formatIpfsUri(hash) {
  return USE_GATEWAY_URLS ? `${IPFS_GATEWAY}/ipfs/${hash}` : `ipfs://${hash}`;
}

function stringToBytes(str) {
  return Buffer.from(str, "utf8").toString("hex");
}

function hashSource(source) {
  return createHash("sha256").update(source || "").digest("hex").slice(0, 16);
}

function isTezosAddress(addr) {
  return typeof addr === "string" && /^tz[123][a-zA-Z0-9]{33}$/.test(addr);
}

function normalizeAddress(value) {
  return typeof value === "string" ? value.trim() : null;
}

function maybeInt(value, fallback = null) {
  const parsed = Number.parseInt(`${value ?? ""}`, 10);
  return Number.isFinite(parsed) ? parsed : fallback;
}

function getTzktApiBase() {
  return NETWORK === "mainnet" ? "https://api.tzkt.io" : `https://api.${NETWORK}.tzkt.io`;
}

function normalizeRoyaltyBps(value, fallback = 1000) {
  const raw = value?.toNumber?.() ?? value;
  const parsed = Number(raw);
  if (!Number.isFinite(parsed)) return fallback;
  return Math.max(0, Math.min(2500, Math.trunc(parsed)));
}

function extractRoyaltyPolicy(storage) {
  let artistBps = 900, platformBps = 100;
  if (storage?.artist_royalty_bps !== undefined) {
    artistBps = normalizeRoyaltyBps(storage.artist_royalty_bps, 900);
    platformBps = normalizeRoyaltyBps(storage.platform_royalty_bps, 100);
  } else {
    artistBps = normalizeRoyaltyBps(storage?.default_royalty_bps, 1000);
    platformBps = 0;
  }
  const treasuryAddress = normalizeAddress(storage?.treasury_address);
  if (platformBps > 0 && !treasuryAddress) {
    throw new Error("Contract royalty split requires treasury_address but storage value is empty");
  }
  return { artistBps, platformBps, treasuryAddress };
}

function buildRoyalties(creatorAddress, artistBps, platformBps, platformAddress) {
  if (!isTezosAddress(creatorAddress)) {
    throw new Error(`Invalid creator address for royalties: ${creatorAddress || "missing"}`);
  }
  const shares = { [creatorAddress]: String(artistBps) };
  if (platformBps > 0 && platformAddress) {
    shares[platformAddress] = String(platformBps);
  }
  return { decimals: 4, shares };
}

const KEEP_PERMIT_PAYLOAD_TYPE = {
  prim: "pair",
  args: [
    { prim: "address", annots: ["%contract"] },
    { prim: "pair", args: [
      { prim: "address", annots: ["%owner"] },
      { prim: "pair", args: [
        { prim: "bytes", annots: ["%content_hash"] },
        { prim: "timestamp", annots: ["%permit_deadline"] },
      ]},
    ]},
  ],
};

async function getPinataCredentials() {
  if (cachedPinataCredentials) return cachedPinataCredentials;
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "pinata" });
  if (!secrets) throw new Error("Pinata credentials not found");
  cachedPinataCredentials = { apiKey: secrets.apiKey, apiSecret: secrets.apiSecret, jwt: secrets.jwt };
  return cachedPinataCredentials;
}

async function getTezosCredentials() {
  if (cachedTezosCredentials && Date.now() < cachedTezosCredentialsExpiresAt) return cachedTezosCredentials;
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "tezos-kidlisp" });
  if (!secrets) throw new Error("Tezos credentials not found");
  const signerPrivateKey = secrets.keepPermitSignerPrivateKey || secrets.keepPermitPrivateKey || secrets.privateKey;
  const signerPublicKey = secrets.keepPermitSignerPublicKey || secrets.keepPermitPublicKey || secrets.publicKey;
  const signerAddress = secrets.keepPermitSignerAddress || secrets.keepPermitAddress || secrets.address;
  const derivedSignerAddress = signerPublicKey ? getPkhfromPk(signerPublicKey) : null;
  if (!signerPrivateKey || !signerPublicKey || !signerAddress) throw new Error("Tezos signer credentials incomplete");
  if (derivedSignerAddress && derivedSignerAddress !== signerAddress) {
    throw new Error(`Tezos signer mismatch: ${derivedSignerAddress} vs ${signerAddress}`);
  }
  cachedTezosCredentials = {
    address: signerAddress, derivedAddress: derivedSignerAddress,
    publicKey: signerPublicKey, privateKey: signerPrivateKey, network: secrets.network,
  };
  cachedTezosCredentialsExpiresAt = Date.now() + KEEP_MINT_SIGNER_CACHE_TTL_MS;
  return cachedTezosCredentials;
}

async function buildKeepPermit({ privateKey, contractAddress, ownerAddress, contentHashBytes, permitDeadline = null }) {
  const deadlineIso = permitDeadline || new Date(Date.now() + KEEP_MINT_PERMIT_TTL_MS).toISOString();
  const payloadData = {
    prim: "Pair",
    args: [
      { string: contractAddress },
      { prim: "Pair", args: [
        { string: ownerAddress },
        { prim: "Pair", args: [
          { bytes: contentHashBytes },
          { string: deadlineIso },
        ]},
      ]},
    ],
  };
  const packed = packDataBytes(payloadData, KEEP_PERMIT_PAYLOAD_TYPE).bytes;
  const signer = new InMemorySigner(privateKey);
  const signature = await signer.sign(packed);
  return { permit_deadline: deadlineIso, keep_permit: signature.prefixSig };
}

async function runContractPreflight({ contractAddress, creatorWalletAddress, includeRecentAlerts = true }) {
  const tezos = new TezosToolkit(RPC_URL);
  const contract = await tezos.contract.at(contractAddress);
  const storage = await contract.storage();
  const violations = [];

  if (KEEP_MINT_EXPECTED_CONTRACT && contractAddress !== KEEP_MINT_EXPECTED_CONTRACT) {
    violations.push(`Contract address mismatch`);
  }
  const administrator = normalizeAddress(storage?.administrator);
  if (KEEP_MINT_EXPECTED_ADMIN && administrator !== KEEP_MINT_EXPECTED_ADMIN) {
    violations.push(`Administrator mismatch`);
  }

  let indexerSnapshot = null;
  try {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 8000);
    const res = await fetch(`${getTzktApiBase()}/v1/contracts/${contractAddress}`, { signal: controller.signal });
    clearTimeout(timeout);
    if (res.ok) {
      const data = await res.json();
      indexerSnapshot = { codeHash: maybeInt(data?.codeHash), typeHash: maybeInt(data?.typeHash) };
    }
  } catch {}

  if (Number.isFinite(KEEP_MINT_EXPECTED_CODE_HASH) && indexerSnapshot?.codeHash !== KEEP_MINT_EXPECTED_CODE_HASH) {
    violations.push(`Code hash mismatch`);
  }
  if (Number.isFinite(KEEP_MINT_EXPECTED_TYPE_HASH) && indexerSnapshot?.typeHash !== KEEP_MINT_EXPECTED_TYPE_HASH) {
    violations.push(`Type hash mismatch`);
  }

  const royaltyPolicy = extractRoyaltyPolicy(storage);
  const royalties = buildRoyalties(creatorWalletAddress, royaltyPolicy.artistBps, royaltyPolicy.platformBps, royaltyPolicy.treasuryAddress);
  const keepFeeMutez = storage?.keep_fee?.toNumber?.() ?? storage?.keep_fee ?? 0;
  const keepFeeXtz = keepFeeMutez / 1_000_000;

  if (KEEP_MINT_MIN_EXPECTED_FEE_MUTEZ && keepFeeMutez < KEEP_MINT_MIN_EXPECTED_FEE_MUTEZ) {
    violations.push(`Keep fee too low: ${keepFeeMutez} mutez`);
  }

  const tezosCredentials = await getTezosCredentials();
  const signerAddress = tezosCredentials.address;
  const signerPublicKeyAddress = tezosCredentials.derivedAddress;
  if (KEEP_MINT_EXPECTED_PERMIT_SIGNER && signerAddress !== KEEP_MINT_EXPECTED_PERMIT_SIGNER) {
    violations.push(`Signer mismatch`);
  }

  let alerts = [];
  if (includeRecentAlerts) {
    try {
      const safeLimit = Math.max(1, Math.min(KEEP_MINT_SECURITY_SCAN_LIMIT, 100));
      const res = await fetch(
        `${getTzktApiBase()}/v1/operations/transactions?target=${contractAddress}&status=applied&limit=${safeLimit}&sort.desc=level`
      );
      if (res.ok) {
        const operations = await res.json();
        for (const op of operations) {
          const entrypoint = op?.parameter?.entrypoint || "";
          const sender = normalizeAddress(op?.sender?.address);
          if (ADMIN_ENTRYPOINTS.has(entrypoint)) {
            const unexpectedAdmin = KEEP_MINT_EXPECTED_ADMIN && sender !== KEEP_MINT_EXPECTED_ADMIN;
            alerts.push({
              message: `${entrypoint} by ${sender?.slice(0, 8)}...`,
              severity: unexpectedAdmin ? "critical" : "info",
              entrypoint, sender, hash: op?.hash, timestamp: op?.timestamp,
            });
          }
        }
      }
    } catch {}
  }

  if (KEEP_MINT_BLOCK_ON_ALERT && alerts.some(a => a.severity === "critical")) {
    violations.push("Critical security alert detected");
  }

  return {
    contract, storage, violations, alerts, signerAddress, signerPublicKeyAddress,
    keepFeeMutez, keepFeeXtz, royaltyPolicy, royalties, indexerSnapshot,
  };
}

async function checkMintStatus(pieceName, contractAddress) {
  try {
    const res = await fetch(`${getTzktApiBase()}/v1/tokens?contract=${contractAddress}&metadata.symbol=${pieceName}&limit=1`);
    if (!res.ok) return { minted: false };
    const tokens = await res.json();
    if (tokens.length > 0) {
      const token = tokens[0];
      return {
        minted: true, tokenId: token.tokenId,
        name: token.metadata?.name,
        objktUrl: `https://objkt.com/tokens/${contractAddress}/${token.tokenId}`,
      };
    }
  } catch {}
  return { minted: false };
}

function isCachedMediaValid(piece) {
  return piece?.ipfsMedia?.artifactUri && piece?.ipfsMedia?.thumbnailUri
    && piece?.ipfsMedia?.sourceHash === hashSource(piece.source);
}

// ─── IPFS Upload ─────────────────────────────────────────────────────────────
async function uploadToIPFS(content, filename, mimeType, timeoutMs = 90000) {
  const pinata = await getPinataCredentials();
  const formData = new FormData();
  formData.append("file", new Blob([content], { type: mimeType }), filename);
  formData.append("pinataMetadata", JSON.stringify({ name: filename }));
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), Math.max(3000, timeoutMs));
  try {
    const res = await fetch("https://api.pinata.cloud/pinning/pinFileToIPFS", {
      method: "POST",
      headers: { pinata_api_key: pinata.apiKey, pinata_secret_api_key: pinata.apiSecret },
      body: formData,
      signal: controller.signal,
    });
    clearTimeout(timeout);
    if (!res.ok) throw new Error(`IPFS upload failed: ${res.status}`);
    const result = await res.json();
    return formatIpfsUri(result.IpfsHash);
  } catch (err) {
    clearTimeout(timeout);
    if (err.name === "AbortError") throw new Error(`IPFS upload timed out after ${Math.round(timeoutMs / 1000)}s`);
    throw err;
  }
}

async function uploadJsonToIPFS(json, name, timeoutMs = 30000) {
  const pinata = await getPinataCredentials();
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), Math.max(3000, timeoutMs));
  try {
    const res = await fetch("https://api.pinata.cloud/pinning/pinJSONToIPFS", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        pinata_api_key: pinata.apiKey,
        pinata_secret_api_key: pinata.apiSecret,
      },
      body: JSON.stringify({ pinataContent: json, pinataMetadata: { name } }),
      signal: controller.signal,
    });
    clearTimeout(timeout);
    if (!res.ok) throw new Error(`Metadata upload failed: ${res.status}`);
    const result = await res.json();
    return formatIpfsUri(result.IpfsHash);
  } catch (err) {
    clearTimeout(timeout);
    if (err.name === "AbortError") throw new Error(`Metadata upload timed out after ${Math.round(timeoutMs / 1000)}s`);
    throw err;
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
// PIPELINE — runs in background, writes progress to MongoDB
// ═══════════════════════════════════════════════════════════════════════════════
async function runPipeline({ jobId, pieceName, isRebake, regenerate, creatorWalletAddress, userHandle, walletAddress }) {
  const startTime = Date.now();
  const log = (stage, msg) => console.log(`🪙 KEEP-BG [${((Date.now() - startTime) / 1000).toFixed(1)}s] ${stage}: ${msg}`);

  const CONTRACT_ADDRESS = await getKeepsContractAddress({ network: NETWORK, fallback: LEGACY_KEEPS_CONTRACT });

  const database = await connect();
  const col = database.db.collection("kidlisp");
  const piece = await col.findOne({ code: pieceName });

  if (!piece) {
    await markJobFailed(jobId, `Piece '$${pieceName}' not found`, "validate");
    return;
  }

  // ── Analyze ────────────────────────────────────────────────────────
  await updateJobStage(jobId, "analyze", "Analyzing source...");
  const analysis = isRebake ? null : analyzeKidLisp(piece.source);
  const charCount = analysis?.chars || piece.source?.length || 0;
  await updateJobStage(jobId, "analyze", `${charCount} chars`);
  log("analyze", `${charCount} chars`);

  const pieceSourceHash = hashSource(piece.source || "");
  const forceFresh = Boolean(regenerate);

  // Check cached media
  const useCachedMedia = !regenerate && isCachedMediaValid(piece);
  let artifactUri, thumbnailUri, metadataUri;

  if (useCachedMedia) {
    artifactUri = piece.ipfsMedia.artifactUri;
    thumbnailUri = piece.ipfsMedia.thumbnailUri;
    await setJobResult(jobId, { artifactUri, thumbnailUri });
    await updateJobStage(jobId, "ipfs", "Using cached IPFS media");
    log("cache", "Reusing cached");
  }

  const pinataCredentials = await getPinataCredentials();

  // ── Thumbnail (async) ──────────────────────────────────────────────
  let thumbnailPromise;
  if (!useCachedMedia) {
    await updateJobStage(jobId, "thumbnail", "Baking thumbnail...");
    log("thumbnail", "Starting oven grab");

    thumbnailPromise = (async () => {
      const tryOven = async (ovenUrl, timeoutMs) => {
        const controller = new AbortController();
        const tid = setTimeout(() => controller.abort(), timeoutMs);
        try {
          const res = await fetch(`${ovenUrl}/grab-ipfs`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              piece: `$${pieceName}`, format: "webp",
              width: 256, height: 256, density: 2,
              duration: forceFresh ? 4000 : 5000, fps: 8, playbackFps: 16, quality: 70,
              source: "keep",
              author: userHandle ? `@${userHandle}` : null,
              cacheKey: forceFresh ? `rebake-${pieceSourceHash}-${Date.now()}` : `src-${pieceSourceHash}`,
              skipCache: forceFresh,
              pinataKey: pinataCredentials.apiKey,
              pinataSecret: pinataCredentials.apiSecret,
            }),
            signal: controller.signal,
          });
          clearTimeout(tid);
          if (!res.ok) throw new Error(`Oven ${res.status}`);
          return res.json();
        } catch (err) {
          clearTimeout(tid);
          if (err.name === "AbortError") throw new Error(`Oven timed out after ${timeoutMs / 1000}s`);
          throw err;
        }
      };

      try {
        const result = await tryOven(OVEN_URL, KEEP_MINT_THUMBNAIL_TIMEOUT_MS);
        if (result?.ipfsUri) return result;
        if (result?.error) throw new Error(result.error);
        return result;
      } catch (err) {
        log("thumbnail", `Primary oven failed: ${err.message}`);
        if (OVEN_URL !== OVEN_FALLBACK_URL) {
          try {
            const fb = await tryOven(OVEN_FALLBACK_URL, KEEP_MINT_THUMBNAIL_TIMEOUT_MS);
            if (fb?.ipfsUri) return fb;
            if (fb?.error) throw new Error(fb.error);
            return fb;
          } catch (fbErr) {
            return { error: fbErr.message };
          }
        }
        return { error: err.message };
      }
    })();
  } else {
    thumbnailPromise = Promise.resolve({ ipfsUri: thumbnailUri });
  }

  // ── Bundle ─────────────────────────────────────────────────────────
  let bundleHtml, bundleFilename, bundleAuthorHandle, userCode, packDate, depCount;
  if (!useCachedMedia) {
    await updateJobStage(jobId, "bundle", "Packing HTML bundle...");
    const bundleCode = `$${pieceName}`;
    let bundleUrl = dev
      ? `https://localhost:8888/api/pack-html?code=${encodeURIComponent(bundleCode)}&format=json&keeplabel=1`
      : `https://oven.aesthetic.computer/pack-html?code=${encodeURIComponent(bundleCode)}&format=json&keeplabel=1`;
    if (forceFresh) bundleUrl += `&rebake=1&nocache=1&sourceHash=${encodeURIComponent(pieceSourceHash)}&ts=${Date.now()}`;

    const bundleController = new AbortController();
    const bundleTimeout = setTimeout(() => bundleController.abort(), 60000);
    log("bundle", `Fetching ${bundleUrl}`);

    const bundleRes = await fetch(bundleUrl, {
      signal: bundleController.signal, cache: "no-store",
      headers: forceFresh ? { "Cache-Control": "no-cache" } : undefined,
    });
    clearTimeout(bundleTimeout);
    if (!bundleRes.ok) throw new Error(`Bundle generation failed: ${bundleRes.status}`);

    const bundleData = await bundleRes.json();
    bundleHtml = Buffer.from(bundleData.content || bundleData.html, "base64").toString("utf8");
    bundleFilename = bundleData.filename || `$${pieceName}.lisp.html`;
    bundleAuthorHandle = bundleData.authorHandle || userHandle;
    userCode = bundleData.userCode;
    packDate = bundleData.packDate;
    depCount = bundleData.depCount || 0;

    const bundleKB = Math.round(bundleHtml.length / 1024);
    await updateJobStage(jobId, "bundle", `Packed ${bundleKB}KB · ${depCount} deps`);
    log("bundle", `${bundleKB}KB`);
  }

  // ── IPFS upload (parallel with thumbnail await) ────────────────────
  let ipfsUploadPromise;
  if (!useCachedMedia) {
    await updateJobStage(jobId, "ipfs", "Uploading to IPFS...");
    ipfsUploadPromise = uploadToIPFS(bundleHtml, bundleFilename, "text/html", 90000).then(uri => {
      log("ipfs", `Pinned: ${uri}`);
      return uri;
    });
  }

  // ── Await thumbnail ────────────────────────────────────────────────
  if (!useCachedMedia) {
    const thumbResult = await thumbnailPromise;
    if (thumbResult?.ipfsUri) {
      thumbnailUri = thumbResult.ipfsUri;
      await setJobResult(jobId, { thumbnailUri });
      await updateJobStage(jobId, "thumbnail", "Thumbnail baked");
      log("thumbnail", `Done: ${thumbnailUri}`);
    } else {
      const preexisting = piece.ipfsMedia?.thumbnailUri;
      if (preexisting && forceFresh) {
        thumbnailUri = preexisting;
        await setJobResult(jobId, { thumbnailUri });
        log("thumbnail", `Reusing previous: ${thumbnailUri}`);
      } else {
        throw new Error(`Thumbnail failed: ${thumbResult?.error || "unknown"}`);
      }
    }
  }

  // ── Await IPFS upload ──────────────────────────────────────────────
  if (!useCachedMedia) {
    artifactUri = await ipfsUploadPromise;
    await setJobResult(jobId, { artifactUri });
    await updateJobStage(jobId, "ipfs", "Pinned to IPFS");
    log("ipfs", `Artifact: ${artifactUri}`);
  }

  // ── Cache media in MongoDB ─────────────────────────────────────────
  if (!useCachedMedia) {
    const updateOps = {
      $set: {
        ipfsMedia: {
          artifactUri, thumbnailUri, sourceHash: pieceSourceHash,
          authorHandle: bundleAuthorHandle, userCode, packDate, depCount,
          createdAt: new Date(),
        },
      },
    };
    if (piece.ipfsMedia?.artifactUri || piece.ipfsMedia?.thumbnailUri) {
      updateOps.$push = {
        mediaHistory: {
          $each: [{
            artifactUri: piece.ipfsMedia.artifactUri,
            thumbnailUri: piece.ipfsMedia.thumbnailUri,
            sourceHash: piece.ipfsMedia.sourceHash,
            createdAt: piece.ipfsMedia.createdAt,
            archivedAt: new Date(),
            reason: isRebake ? "rebake" : "mint",
          }],
          $slice: -20,
        },
      };
    }
    await col.updateOne({ code: pieceName }, updateOps);
  }

  // ── Rebake early exit ──────────────────────────────────────────────
  if (isRebake) {
    const mintStatus = await checkMintStatus(pieceName, CONTRACT_ADDRESS);
    await markJobReady(jobId, {
      rebake: true, piece: pieceName, artifactUri, thumbnailUri,
      tokenId: mintStatus.tokenId, objktUrl: mintStatus.objktUrl, packDate,
    });
    log("ready", "Rebake complete");
    return;
  }

  // ── Metadata ───────────────────────────────────────────────────────
  await updateJobStage(jobId, "metadata", "Building metadata...");
  const tokenName = `$${pieceName}`;
  const creatorsArray = [creatorWalletAddress];
  const tags = ["KidLisp"];
  const attributes = [{ name: "Characters", value: String(charCount) }];

  // Preflight
  await updateJobStage(jobId, "security", "Running contract preflight...");
  const preflight = await runContractPreflight({
    contractAddress: CONTRACT_ADDRESS, creatorWalletAddress, includeRecentAlerts: true,
  });

  if (preflight.violations.length > 0 && KEEP_MINT_STRICT_PREFLIGHT) {
    throw new Error(`Security preflight failed: ${preflight.violations.join("; ")}`);
  }

  const keepFeeXtz = preflight.keepFeeXtz;
  const royalties = preflight.royalties;

  const metadataJson = {
    name: tokenName,
    description: piece.source || "A KidLisp piece preserved on Tezos",
    artifactUri, displayUri: artifactUri, thumbnailUri,
    decimals: 0, symbol: pieceName,
    creators: creatorsArray, royalties, tags, attributes,
    formats: [{ uri: artifactUri, mimeType: "text/html", dimensions: { value: "responsive", unit: "viewport" } }],
  };

  metadataUri = await uploadJsonToIPFS(metadataJson, `$${pieceName}-metadata.json`, 30000);
  await setJobResult(jobId, { metadataUri });
  await updateJobStage(jobId, "metadata", "Metadata uploaded");
  log("metadata", `URI: ${metadataUri}`);

  // ── Build permit + transfer params ─────────────────────────────────
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

  const tezosCredentials = await getTezosCredentials();
  const keepPermit = await buildKeepPermit({
    privateKey: tezosCredentials.privateKey,
    contractAddress: CONTRACT_ADDRESS,
    ownerAddress: creatorWalletAddress,
    contentHashBytes: onChainMetadata.content_hash,
  });

  const contract = preflight.contract;
  const transferParams = contract.methodsObject.keep({
    ...onChainMetadata,
    permit_deadline: keepPermit.permit_deadline,
    keep_permit: keepPermit.keep_permit,
  }).toTransferParams();

  // ── Mark ready ─────────────────────────────────────────────────────
  await markJobReady(jobId, {
    success: true, piece: pieceName,
    contractAddress: CONTRACT_ADDRESS, network: NETWORK,
    mintFee: keepFeeXtz,
    michelsonParams: transferParams.parameter,
    entrypoint: "keep",
    artifactUri, thumbnailUri, metadataUri, packDate,
    rpcUrl: RPC_URL,
    keepPermitDeadline: keepPermit.permit_deadline,
    usedCachedMedia: useCachedMedia,
    security: {
      alerts: preflight.alerts,
      observed: {
        contract: CONTRACT_ADDRESS,
        administrator: normalizeAddress(preflight.storage?.administrator),
        signer: preflight.signerAddress,
        keepFeeMutez: preflight.keepFeeMutez,
        keepFeeXtz: preflight.keepFeeXtz,
        royalty: preflight.royaltyPolicy,
        codeHash: preflight.indexerSnapshot?.codeHash ?? null,
        typeHash: preflight.indexerSnapshot?.typeHash ?? null,
      },
    },
  });

  const totalElapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  log("ready", `Complete in ${totalElapsed}s`);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Handler (Netlify Background Function — returns 202, runs up to 15 min)
// ═══════════════════════════════════════════════════════════════════════════════
export const handler = async (event) => {
  if (event.httpMethod !== "POST") return { statusCode: 405 };

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return { statusCode: 400 };
  }

  const { jobId, pieceName, isRebake, regenerate, creatorWalletAddress, userHandle, walletAddress } = body;
  if (!jobId || !pieceName) {
    console.error("🪙 KEEP-BG: Missing jobId or pieceName");
    return { statusCode: 400 };
  }

  console.log(`🪙 KEEP-BG: Starting pipeline for $${pieceName} (job ${jobId})`);

  try {
    await runPipeline({ jobId, pieceName, isRebake, regenerate, creatorWalletAddress, userHandle, walletAddress });
  } catch (err) {
    console.error(`🪙 KEEP-BG: Pipeline failed:`, err.message);
    await markJobFailed(jobId, err.message, "unknown").catch(() => {});
  }

  // Background functions don't return a meaningful response
  return { statusCode: 200 };
};
