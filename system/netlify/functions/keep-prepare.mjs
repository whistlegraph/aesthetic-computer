// keep-prepare.mjs — Job-based keep preparation endpoint.
//
// POST /api/keep-prepare
//   Body: { piece, walletAddress, network, regenerate?, force? }
//   Auth: Bearer token (required for new mints, optional for rebake if on-chain owner)
//   Returns: { jobId, status, stage, ... }
//
// Validates auth & ownership, creates a keep-job in MongoDB, then invokes
// keep-prepare-background to run the heavy pipeline (oven, IPFS, metadata).
// Client polls GET /api/keep-status?jobId=xxx for real-time progress.

import { authorize, handleFor, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";
import {
  upsertJob,
  formatJobForClient,
  getJob,
} from "../../backend/keep-job.mjs";

const dev = process.env.CONTEXT === "dev";
if (dev) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
let CONTRACT_ADDRESS = LEGACY_KEEPS_CONTRACT;

function isTezosAddress(addr) {
  return typeof addr === "string" && /^tz[123][a-zA-Z0-9]{33}$/.test(addr);
}

function getTzktApiBase() {
  return NETWORK === "mainnet" ? "https://api.tzkt.io" : `https://api.${NETWORK}.tzkt.io`;
}

async function checkMintStatus(pieceName) {
  try {
    const res = await fetch(`${getTzktApiBase()}/v1/tokens?contract=${CONTRACT_ADDRESS}&metadata.symbol=${pieceName}&limit=1`);
    if (!res.ok) return { minted: false };
    const tokens = await res.json();
    if (tokens.length > 0) {
      const token = tokens[0];
      return {
        minted: true, tokenId: token.tokenId,
        name: token.metadata?.name,
        objktUrl: `https://objkt.com/tokens/${CONTRACT_ADDRESS}/${token.tokenId}`,
      };
    }
  } catch {}
  return { minted: false };
}

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Content-Type": "application/json",
};

function jsonResponse(statusCode, body) {
  return { statusCode, headers: CORS_HEADERS, body: JSON.stringify(body) };
}

// ═══════════════════════════════════════════════════════════════════════════════
// Handler — validates, creates job, invokes background pipeline
// ═══════════════════════════════════════════════════════════════════════════════
export const handler = async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers: CORS_HEADERS, body: "" };
  }

  if (event.httpMethod !== "POST") {
    return jsonResponse(405, { error: "Method not allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return jsonResponse(400, { error: "Invalid JSON body" });
  }

  const pieceName = body.piece?.replace(/^\$/, "").trim();
  const walletAddress = body.walletAddress;
  const regenerate = body.regenerate === true;

  if (!pieceName) return jsonResponse(400, { error: "Missing 'piece'" });
  if (!walletAddress || !isTezosAddress(walletAddress)) {
    return jsonResponse(400, { error: "Valid Tezos wallet address required" });
  }

  // ── Resolve contract address ───────────────────────────────────────
  CONTRACT_ADDRESS = await getKeepsContractAddress({ network: NETWORK, fallback: LEGACY_KEEPS_CONTRACT });

  // ── Auth ────────────────────────────────────────────────────────────
  const user = await authorize(event.headers);

  // ── Load piece from DB ─────────────────────────────────────────────
  const database = await connect();
  const collection = database.db.collection("kidlisp");
  const piece = await collection.findOne({ code: pieceName });
  if (!piece) return jsonResponse(404, { error: `Piece '$${pieceName}' not found` });

  // ── Check mint status ──────────────────────────────────────────────
  const mintStatus = await checkMintStatus(pieceName);
  const isRebake = mintStatus.minted && regenerate;

  // ── Verify on-chain ownership (for rebake without AC login) ────────
  let isOnChainOwner = false;
  if (isRebake && walletAddress && mintStatus.tokenId != null) {
    try {
      const tokenRes = await fetch(
        `https://api.tzkt.io/v1/tokens/balances?token.contract=${CONTRACT_ADDRESS}&token.tokenId=${mintStatus.tokenId}&balance.gt=0`
      );
      const balances = await tokenRes.json();
      isOnChainOwner = !!balances.find(b => b.account?.address === walletAddress);
    } catch {}
  }

  // ── Auth checks ────────────────────────────────────────────────────
  if (!isRebake && !user) return jsonResponse(401, { error: "Please log in first" });
  if (isRebake && !user && !isOnChainOwner) return jsonResponse(401, { error: "Login or connect token-owning wallet" });

  let userHandle = null;
  let isAdmin = false;
  if (user) {
    userHandle = await handleFor(user.sub);
    if (!userHandle && !isRebake) return jsonResponse(400, { error: "You need an @handle first" });
    isAdmin = await hasAdmin(user);
  }

  if (!piece.user) return jsonResponse(400, { error: "Anonymous pieces cannot be kept" });
  if (!isRebake && user && !isAdmin && piece.user !== user.sub) {
    return jsonResponse(403, { error: "This piece belongs to someone else" });
  }
  if (isRebake && !isAdmin && !isOnChainOwner && (!user || piece.user !== user.sub)) {
    return jsonResponse(403, { error: "Only the piece owner or token holder can rebake" });
  }

  // Already minted (and not regenerating)
  if (mintStatus.minted && !regenerate) {
    return jsonResponse(200, {
      alreadyMinted: true,
      tokenId: mintStatus.tokenId,
      objktUrl: mintStatus.objktUrl,
    });
  }

  // ── Wallet validation (new mints) ──────────────────────────────────
  let creatorWalletAddress;
  if (!isRebake) {
    const usersCol = database.db.collection("users");
    const userDoc = user ? await usersCol.findOne({ _id: user.sub }) : null;
    const linkedWallet = userDoc?.tezos?.address;
    if (!linkedWallet) return jsonResponse(400, { error: "Connect your Tezos wallet first (wallet.ac)" });
    if (walletAddress !== linkedWallet) {
      return jsonResponse(400, { error: `Wallet mismatch — mint from ${linkedWallet.slice(0, 8)}...` });
    }
    creatorWalletAddress = walletAddress;
  } else {
    const usersCol = database.db.collection("users");
    const userDoc = user ? await usersCol.findOne({ _id: user.sub }) : null;
    creatorWalletAddress = userDoc?.tezos?.address || "tz1burnburnburnburnburnburnburjAYjjX";
  }

  // ── Check for existing active job ──────────────────────────────────
  const existingJob = await getJob(pieceName, walletAddress);
  if (existingJob && existingJob.status === "preparing" && !body.force) {
    const ageMs = Date.now() - new Date(existingJob.updatedAt).getTime();
    if (ageMs < 5 * 60 * 1000) {
      // Job still in progress — return it
      return jsonResponse(200, { resuming: true, ...formatJobForClient(existingJob) });
    }
  }

  // ── Create job ─────────────────────────────────────────────────────
  const job = await upsertJob({
    piece: pieceName, wallet: walletAddress,
    user: user?.sub, handle: userHandle,
    isRebake, regenerate,
  });

  // ── Invoke background function ─────────────────────────────────────
  const siteUrl = process.env.URL || process.env.DEPLOY_URL || (dev ? "http://localhost:8888" : "https://aesthetic.computer");
  const bgPayload = {
    jobId: job._id.toString(),
    pieceName, isRebake, regenerate,
    creatorWalletAddress, userHandle, walletAddress,
  };

  // Fire-and-forget: background function returns 202 immediately
  fetch(`${siteUrl}/.netlify/functions/keep-prepare-background`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(bgPayload),
  }).catch(err => {
    console.error(`🪙 KEEP: Failed to invoke background function:`, err.message);
  });

  // ── Return immediately ─────────────────────────────────────────────
  return jsonResponse(200, formatJobForClient(job));
};
