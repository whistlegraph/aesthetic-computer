// keep-prepare-background.mjs — Netlify Background Function for keep pipeline.
//
// Runs the heavy preparation work (oven bake, IPFS upload, metadata).
// Background functions return 202 immediately and run for up to 15 minutes.
// All progress is written to MongoDB via the keep-job model.
//
// Invoked by keep-prepare.mjs after job creation and validation.

import { connect } from "../../backend/database.mjs";
import { loadKidlispPiece } from "../../backend/kidlisp-read.mjs";
import { analyzeKidLisp } from "../../backend/kidlisp-analyzer.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";
import { handleFor } from "../../backend/authorization.mjs";
import {
  mirrorIpfsMedia,
  mirrorPendingRebake,
} from "../../backend/kidlisp-dual-write.mjs";
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
  ? "https://rpc.tzkt.io/mainnet"
  : "https://rpc.ghostnet.teztnets.com";

const VERSION_BY_PROFILE = {
  v11: "11.0.0",
  v10: "10.0.0",
  v9: "9.0.0",
  v8: "8.0.0",
  v7: "7.0.0",
  v6: "6.0.0",
  v5: "5.0.0",
  v5rc: "5.0.0-rc",
  v4: "4.0.0",
};

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

function normalizeContractProfile(value) {
  if (typeof value !== "string") return null;
  const trimmed = value.trim().toLowerCase();
  return trimmed || null;
}

function normalizeContractVersion(value) {
  if (typeof value !== "string") return null;
  const trimmed = value.trim();
  return trimmed || null;
}

function pickString(value, network) {
  if (typeof value === "string" && value.trim()) return value.trim();
  if (!value || typeof value !== "object" || Array.isArray(value)) return null;
  const candidate = value[network] || value.mainnet || value.current || value.default;
  return typeof candidate === "string" && candidate.trim() ? candidate.trim() : null;
}

function resolveProfile(secretDoc, network) {
  const raw =
    pickString(secretDoc?.currentKeepsProfile, network) ||
    pickString(secretDoc?.keepsProfile, network) ||
    pickString(secretDoc?.keeps_profile, network) ||
    pickString(secretDoc?.contractProfile, network) ||
    pickString(secretDoc?.keeps?.profile, network);
  return normalizeContractProfile(raw) || "v11";
}

function resolveVersion(secretDoc, profile, network) {
  const explicit =
    pickString(secretDoc?.currentKeepsVersion, network) ||
    pickString(secretDoc?.keepsVersion, network) ||
    pickString(secretDoc?.keeps_version, network) ||
    pickString(secretDoc?.keeps?.version, network);
  return normalizeContractVersion(explicit) || VERSION_BY_PROFILE[profile] || null;
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

// ─── IPFS Upload (self-hosted Kubo node on lith + oven seeder + public pin) ──
const IPFS_API = process.env.IPFS_API_URL || "http://localhost:5001";
const IPFS_SEEDER_URL = process.env.IPFS_SEEDER_URL || "http://137.184.237.166:5001";

// Public pinning service (IPFS Pinning Service API spec — Filebase, web3.storage,
// Estuary, etc.). Vendor-neutral so provider can be swapped by env var alone.
// Pinning the CID we already own (no re-upload) gives objkt's indexer / public
// gateways a well-peered place to fetch from while our node stays primary.
const IPFS_PINNING_SERVICE_URL = process.env.IPFS_PINNING_SERVICE_URL || "";
const IPFS_PINNING_SERVICE_TOKEN = process.env.IPFS_PINNING_SERVICE_TOKEN || "";

// Seed content to the oven IPFS node (fire-and-forget for faster gateway propagation)
function seedToSecondaryNode(hash) {
  fetch(`${IPFS_SEEDER_URL}/api/v0/pin/add?arg=${hash}`, { method: "POST", signal: AbortSignal.timeout(120000) })
    .then(r => r.ok ? console.log(`🌱 Seeded ${hash.slice(0, 12)}... to oven`) : null)
    .catch(() => {}); // Best-effort, don't block pipeline
}

// Pin existing CID on a public IPFS pinning service (fire-and-forget).
// Skips silently when credentials aren't configured.
function pinToPublicService(hash, name) {
  if (!IPFS_PINNING_SERVICE_URL || !IPFS_PINNING_SERVICE_TOKEN) return;
  fetch(`${IPFS_PINNING_SERVICE_URL}/pins`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${IPFS_PINNING_SERVICE_TOKEN}`,
    },
    body: JSON.stringify({ cid: hash, ...(name ? { name } : {}) }),
    signal: AbortSignal.timeout(30000),
  })
    .then(r => r.ok ? console.log(`📌 Pinned ${hash.slice(0, 12)}... to public service`) : r.text().then(t => console.warn(`📌 Public pin ${r.status}: ${t.slice(0, 200)}`)))
    .catch(() => {}); // Best-effort, don't block pipeline
}

// Warm public IPFS gateways so they DHT-fetch the CID from us and cache it.
// Once cached on e.g. ipfs.io, there are multiple providers for downstream
// indexers (objkt, tzkt) to find, which dramatically cuts the lag between
// edit_metadata and rebaked thumbnails appearing in their UIs. Fire-and-forget;
// Range header keeps our egress tiny while the gateway fetches the full CID.
const PUBLIC_GATEWAYS = [
  "https://ipfs.io",
  "https://gateway.ipfs.io",
  "https://dweb.link",
  "https://nftstorage.link",
];
function warmPublicGateways(hash) {
  for (const gw of PUBLIC_GATEWAYS) {
    fetch(`${gw}/ipfs/${hash}`, {
      method: "GET",
      headers: { Range: "bytes=0-0" },
      redirect: "follow",
      signal: AbortSignal.timeout(20000),
    }).catch(() => {}); // Best-effort, don't block pipeline
  }
  console.log(`🔥 Warming ${hash.slice(0, 12)}... on ${PUBLIC_GATEWAYS.length} public gateways`);
}

async function uploadToIPFS(content, filename, mimeType, timeoutMs = 90000) {
  const formData = new FormData();
  formData.append("file", new Blob([content], { type: mimeType }), filename);
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), Math.max(3000, timeoutMs));
  try {
    const res = await fetch(`${IPFS_API}/api/v0/add?pin=true`, {
      method: "POST",
      body: formData,
      signal: controller.signal,
    });
    clearTimeout(timeout);
    if (!res.ok) throw new Error(`IPFS upload failed: ${res.status}`);
    const result = await res.json();
    seedToSecondaryNode(result.Hash);
    pinToPublicService(result.Hash, filename);
    warmPublicGateways(result.Hash);
    return formatIpfsUri(result.Hash);
  } catch (err) {
    clearTimeout(timeout);
    if (err.name === "AbortError") throw new Error(`IPFS upload timed out after ${Math.round(timeoutMs / 1000)}s`);
    throw err;
  }
}

async function uploadJsonToIPFS(json, name, timeoutMs = 30000) {
  const content = JSON.stringify(json);
  const formData = new FormData();
  formData.append("file", new Blob([content], { type: "application/json" }), name);
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), Math.max(3000, timeoutMs));
  try {
    const res = await fetch(`${IPFS_API}/api/v0/add?pin=true`, {
      method: "POST",
      body: formData,
      signal: controller.signal,
    });
    clearTimeout(timeout);
    if (!res.ok) throw new Error(`Metadata upload failed: ${res.status}`);
    const result = await res.json();
    seedToSecondaryNode(result.Hash);
    pinToPublicService(result.Hash, name);
    warmPublicGateways(result.Hash);
    return formatIpfsUri(result.Hash);
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

  // ── Oven progress poller — forwards grab-status to MongoDB ─────────
  let ovenProgressTimer = null;
  function startOvenProgressPoller(ovenUrl, stage) {
    if (ovenProgressTimer) clearInterval(ovenProgressTimer);
    ovenProgressTimer = setInterval(async () => {
      try {
        const res = await fetch(`${ovenUrl}/grab-status`, { signal: AbortSignal.timeout(3000) });
        if (!res.ok) return;
        const status = await res.json();
        const progress = status.progress;
        if (progress?.stage && progress.piece?.includes(pieceName)) {
          const detail = progress.stageDetail || progress.stage;
          const frame = progress.currentFrame != null && progress.totalFrames
            ? ` (${progress.currentFrame}/${progress.totalFrames})`
            : "";
          const pct = progress.percent != null ? ` ${progress.percent}%` : "";
          await updateJobStage(jobId, stage, `${detail}${frame}${pct}`);
        }
      } catch {}
    }, 1000);
  }
  function stopOvenProgressPoller() {
    if (ovenProgressTimer) { clearInterval(ovenProgressTimer); ovenProgressTimer = null; }
  }

  const database = await connect();
  try {
  const secretDoc = await database.db.collection("secrets").findOne({ _id: "tezos-kidlisp" });
  let contractProfile = resolveProfile(secretDoc, NETWORK);
  let contractVersion = resolveVersion(secretDoc, contractProfile, NETWORK);
  if (normalizeAddress(CONTRACT_ADDRESS)?.toLowerCase() === LEGACY_KEEPS_CONTRACT.toLowerCase()) {
    contractProfile = "legacy";
    contractVersion = VERSION_BY_PROFILE.v9 || null;
  }
  if (!contractVersion && contractProfile) {
    contractVersion = VERSION_BY_PROFILE[contractProfile] || null;
  }
  const col = database.db.collection("kidlisp");
  // Datomic-aware read so new pieces (which only live in Datomic when
  // KIDLISP_DATOMIC=on) are visible to the bake pipeline.
  const piece = await loadKidlispPiece(database, pieceName);

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

  // Identity fields to seed when an upsert below INSERTS a Mongo mirror row for
  // a piece that, under KIDLISP_DATOMIC, lives only in Datomic. Without these
  // the new row has no `hash`, lands as { hash: null }, and collides with the
  // partial-but-formerly-plain kidlisp_hash_unique index. The canonical `hash`
  // is sha256(source.trim()) — matching store-kidlisp — NOT the sliced
  // hashSource() used for ipfsMedia.sourceHash.
  const identityOnInsert = {};
  if (typeof piece.source === "string" && piece.source.length > 0) {
    identityOnInsert.source = piece.source;
    identityOnInsert.hash = createHash("sha256")
      .update(piece.source.trim())
      .digest("hex");
    identityOnInsert.when = new Date();
  }
  const withInsertIdentity = (ops) =>
    Object.keys(identityOnInsert).length > 0
      ? { ...ops, $setOnInsert: identityOnInsert }
      : ops;

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

  // ── Thumbnail (async) ──────────────────────────────────────────────
  let thumbnailPromise;
  if (!useCachedMedia) {
    await updateJobStage(jobId, "thumbnail", "Baking thumbnail...");
    log("thumbnail", "Starting oven grab");
    startOvenProgressPoller(OVEN_URL, "thumbnail");

    thumbnailPromise = (async () => {
      // Existing CID we're trying to replace. Pinata content-addresses, so
      // a silent oven dud (uniform-color black frames re-encoded as a 528-
      // byte still) always pins to the same CID — looks like a successful
      // bake but freezes the thumbnail forever. Detect and retry.
      const previousThumbnailUri = piece.ipfsMedia?.thumbnailUri || null;

      const tryOven = async (ovenUrl, timeoutMs) => {
        const controller = new AbortController();
        const tid = setTimeout(() => controller.abort(), timeoutMs);
        try {
          // Use /grab to get raw image, then pin to local IPFS
          const res = await fetch(`${ovenUrl}/grab`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              piece: `$${pieceName}`, format: "webp",
              width: 256, height: 256, density: 2,
              // Longer + higher-quality so per-frame bytes clear objkt's
              // "probably-a-still" heuristic (was flattening low-entropy WebPs
              // to static JPEG; $sum at 6.7KB/frame → animated GIF, $pie at
              // 1.3KB/frame → JPEG). Aiming for ≥5KB/frame average.
              duration: forceFresh ? 6000 : 7000, fps: 10,
              quality: 88,
              cacheKey: forceFresh ? `rebake-${pieceSourceHash}-${Date.now()}` : `src-${pieceSourceHash}`,
              skipCache: forceFresh,
            }),
            signal: controller.signal,
          });
          clearTimeout(tid);
          if (!res.ok) {
            const body = await res.text().catch(() => "");
            throw new Error(`Oven ${res.status}${body ? `: ${body.slice(0, 200)}` : ""}`);
          }
          const buffer = Buffer.from(await res.arrayBuffer());
          const thumbFilename = `${pieceName}-thumbnail.webp-${pieceSourceHash.slice(0, 16)}`;
          const ipfsUri = await uploadToIPFS(buffer, thumbFilename, "image/webp");
          return { ipfsUri, size: buffer.length };
        } catch (err) {
          clearTimeout(tid);
          if (err.name === "AbortError") throw new Error(`Oven timed out after ${timeoutMs / 1000}s`);
          throw err;
        }
      };

      // forceFresh callers (rebake / regenerate) get one extra retry to
      // shake out cold-puppeteer warmup flakes; non-fresh callers keep
      // the legacy single-attempt + fallback-host behavior.
      const maxAttempts = forceFresh ? 3 : 1;
      let lastErr;
      for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
          const result = await tryOven(OVEN_URL, KEEP_MINT_THUMBNAIL_TIMEOUT_MS);
          // Silent dedup detection: if the CID matches what's already on
          // the piece despite forceFresh, oven returned a uniform-color
          // dud (post-fix it should throw, but older builds may still
          // dedup). Treat as a soft failure and retry.
          if (
            forceFresh &&
            previousThumbnailUri &&
            result.ipfsUri === previousThumbnailUri &&
            attempt < maxAttempts
          ) {
            log("thumbnail", `Attempt ${attempt}/${maxAttempts}: CID matches previous (${result.ipfsUri}); retrying`);
            await new Promise(r => setTimeout(r, 2000));
            continue;
          }
          if (attempt > 1) log("thumbnail", `Succeeded on attempt ${attempt}/${maxAttempts}`);
          return result;
        } catch (err) {
          lastErr = err;
          log("thumbnail", `Attempt ${attempt}/${maxAttempts} failed: ${err.message}`);
          if (attempt < maxAttempts) {
            await new Promise(r => setTimeout(r, 2000));
          }
        }
      }

      // All primary attempts exhausted — fall back to secondary host once.
      if (OVEN_URL !== OVEN_FALLBACK_URL) {
        try {
          return await tryOven(OVEN_FALLBACK_URL, KEEP_MINT_THUMBNAIL_TIMEOUT_MS);
        } catch (fbErr) {
          return { error: fbErr.message };
        }
      }
      return { error: lastErr?.message || "thumbnail bake failed" };
    })();
  } else {
    thumbnailPromise = Promise.resolve({ ipfsUri: thumbnailUri });
  }

  // ── Bundle ─────────────────────────────────────────────────────────
  let bundleHtml, bundleFilename, bundleAuthorHandle, userCode, packDate, depCount;
  if (!useCachedMedia) {
    await updateJobStage(jobId, "bundle", "Connecting to oven for HTML bundle...");
    const bundleCode = `$${pieceName}`;
    let bundleUrl = dev
      ? `https://localhost:8888/api/pack-html?code=${encodeURIComponent(bundleCode)}&format=json`
      : `https://oven.aesthetic.computer/pack-html?code=${encodeURIComponent(bundleCode)}&format=json`;
    if (forceFresh) bundleUrl += `&rebake=1&nocache=1&sourceHash=${encodeURIComponent(pieceSourceHash)}&ts=${Date.now()}`;

    const bundleController = new AbortController();
    const bundleTimeout = setTimeout(() => bundleController.abort(), 60000);
    log("bundle", `Fetching ${bundleUrl}`);

    const bundleStart = Date.now();
    const bundleRes = await fetch(bundleUrl, {
      signal: bundleController.signal, cache: "no-store",
      headers: forceFresh ? { "Cache-Control": "no-cache" } : undefined,
    });
    clearTimeout(bundleTimeout);
    if (!bundleRes.ok) throw new Error(`Bundle generation failed: ${bundleRes.status}`);

    await updateJobStage(jobId, "bundle", "Decoding HTML bundle...");
    const bundleData = await bundleRes.json();
    bundleHtml = Buffer.from(bundleData.content || bundleData.html, "base64").toString("utf8");
    bundleFilename = bundleData.filename || `$${pieceName}.lisp.html`;
    bundleAuthorHandle = bundleData.authorHandle || userHandle;
    userCode = bundleData.userCode;
    packDate = bundleData.packDate;
    depCount = bundleData.depCount || 0;

    const bundleKB = Math.round(bundleHtml.length / 1024);
    const bundleElapsed = ((Date.now() - bundleStart) / 1000).toFixed(1);
    await updateJobStage(jobId, "bundle", `Packed ${bundleKB}KB · ${depCount} deps · ${bundleElapsed}s`);
    log("bundle", `${bundleKB}KB in ${bundleElapsed}s`);
  }

  // ── IPFS upload (parallel with thumbnail await) ────────────────────
  let ipfsUploadPromise;
  if (!useCachedMedia) {
    const bundleSizeKB = Math.round((bundleHtml?.length || 0) / 1024);
    await updateJobStage(jobId, "ipfs", `Uploading ${bundleSizeKB}KB to IPFS...`);
    log("ipfs", `Starting upload: ${bundleSizeKB}KB ${bundleFilename}`);
    const ipfsStart = Date.now();
    ipfsUploadPromise = uploadToIPFS(bundleHtml, bundleFilename, "text/html", 90000).then(uri => {
      const elapsed = ((Date.now() - ipfsStart) / 1000).toFixed(1);
      log("ipfs", `Pinned in ${elapsed}s: ${uri}`);
      return uri;
    });
  }

  // ── Await thumbnail ────────────────────────────────────────────────
  if (!useCachedMedia) {
    const thumbResult = await thumbnailPromise;
    stopOvenProgressPoller();
    if (thumbResult?.ipfsUri) {
      thumbnailUri = thumbResult.ipfsUri;
      await setJobResult(jobId, { thumbnailUri, thumbnailFallback: null });
      await updateJobStage(jobId, "thumbnail", "Thumbnail baked");
      log("thumbnail", `Done: ${thumbnailUri}`);
    } else {
      const errMsg = thumbResult?.error || "oven returned no ipfsUri";
      const preexisting = piece.ipfsMedia?.thumbnailUri;
      if (preexisting) {
        thumbnailUri = preexisting;
        // Surface the silent fallback so the track log + regenerated-artifact
        // panel can show the user that the new thumbnail DIDN'T land and they
        // are still looking at the previous bake. Without this the bake
        // failure was invisible — new artifactUri on-chain but stale thumb.
        await setJobResult(jobId, { thumbnailUri, thumbnailFallback: { kind: "preexisting", reason: errMsg } });
        await updateJobStage(jobId, "thumbnail", `⚠️ Bake failed (${errMsg}); reused previous thumbnail`);
        log("thumbnail", `⚠️ Reusing previous ${thumbnailUri} (reason: ${errMsg})`);
      } else {
        // Fallback to artifact handled below once artifactUri resolves.
        await updateJobStage(jobId, "thumbnail", `⚠️ Bake failed (${errMsg}); will reuse artifact`);
        log("thumbnail", `Oven failed (${errMsg}), artifact fallback pending`);
      }
    }
  }

  // ── Await IPFS upload ──────────────────────────────────────────────
  if (!useCachedMedia) {
    artifactUri = await ipfsUploadPromise;
    await setJobResult(jobId, { artifactUri });
    await updateJobStage(jobId, "ipfs", "Pinned to IPFS");
    log("ipfs", `Artifact: ${artifactUri}`);

    // If thumbnail failed AND no previous thumbnail exists, fall back to artifact URI
    if (!thumbnailUri) {
      thumbnailUri = artifactUri;
      await setJobResult(jobId, { thumbnailUri, thumbnailFallback: { kind: "artifact", reason: "no previous thumbnail to reuse" } });
      log("thumbnail", `⚠️ Fallback to artifact: ${thumbnailUri}`);
    }
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
    // upsert: pieces that live only in Datomic don't have a Mongo row yet.
    // Write one on first bake so ipfsMedia / mediaHistory / kept can land
    // and subsequent keep operations (rebake, confirm, sync) work.
    await col.updateOne({ code: pieceName }, withInsertIdentity(updateOps), { upsert: true });
    await mirrorIpfsMedia(pieceName, updateOps.$set.ipfsMedia);
  }

  // ── Rebake early exit ──────────────────────────────────────────────
  if (isRebake) {
    const rebakePayload = {
      artifactUri,
      thumbnailUri,
      metadataUri: null,
      createdAt: new Date(),
      sourceHash: pieceSourceHash,
      network: NETWORK,
      contractAddress: CONTRACT_ADDRESS,
      contractProfile: contractProfile || null,
      contractVersion: contractVersion || null,
      packDate: packDate || null,
    };
    await col.updateOne(
      { code: pieceName },
      withInsertIdentity({ $set: { pendingRebake: rebakePayload } }),
      { upsert: true }
    );
    await mirrorPendingRebake(pieceName, rebakePayload);
    const mintStatus = await checkMintStatus(pieceName, CONTRACT_ADDRESS);
    await markJobReady(jobId, {
      rebake: true, piece: pieceName, artifactUri, thumbnailUri,
      tokenId: mintStatus.tokenId, objktUrl: mintStatus.objktUrl, packDate,
      network: NETWORK,
      contractAddress: CONTRACT_ADDRESS,
      contractProfile: contractProfile || null,
      contractVersion: contractVersion || null,
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
    contractProfile: contractProfile || null,
    contractVersion: contractVersion || null,
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
  } finally {
    stopOvenProgressPoller();
    await database.disconnect().catch(() => {});
  }
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
