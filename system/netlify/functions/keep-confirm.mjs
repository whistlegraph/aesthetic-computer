// keep-confirm.mjs - Confirm a client-side mint and update MongoDB
// 
// POST /api/keep-confirm - Record a successful client-side wallet mint
// This is called after the user signs the transaction with their wallet
// to update the kidlisp record with mint information.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";

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

function normalizeTokenId(value) {
  if (value === null || typeof value === "undefined" || value === "") return null;
  const parsed = Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) return null;
  return parsed;
}

function normalizeNetwork(value) {
  const normalized = String(value || "mainnet").trim().toLowerCase();
  return normalized === "ghostnet" ? "ghostnet" : "mainnet";
}

function normalizeAddress(value) {
  return typeof value === "string" ? value.trim().toLowerCase() : "";
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

async function resolveContractIdentity({
  db,
  network,
  contractAddress,
  requestedProfile,
  requestedVersion,
}) {
  let contractProfile = normalizeContractProfile(requestedProfile);
  let contractVersion = normalizeContractVersion(requestedVersion);
  const normalizedContract = normalizeAddress(contractAddress);

  if (!contractProfile || !contractVersion) {
    try {
      const secretDoc = await db.collection("secrets").findOne({ _id: "tezos-kidlisp" });
      const activeContract = await getKeepsContractAddress({
        db,
        network,
        fallback: LEGACY_KEEPS_CONTRACT,
      });
      const activeProfile = resolveProfile(secretDoc, network);
      const activeVersion = resolveVersion(secretDoc, activeProfile, network);
      const isActiveContract =
        normalizedContract &&
        normalizeAddress(activeContract) === normalizedContract;

      if (!contractProfile && (!normalizedContract || isActiveContract)) {
        contractProfile = activeProfile;
      }
      if (!contractVersion && (!normalizedContract || isActiveContract)) {
        contractVersion = activeVersion;
      }
    } catch (error) {
      console.warn("⚠️ keep-confirm: contract identity fallback failed:", error?.message || error);
    }
  }

  if (!contractProfile && normalizedContract === normalizeAddress(LEGACY_KEEPS_CONTRACT)) {
    contractProfile = "legacy";
  }
  if (!contractVersion && contractProfile) {
    contractVersion = VERSION_BY_PROFILE[contractProfile] || null;
  }

  return { contractProfile, contractVersion };
}

function tzktApiBase(network) {
  return network === "mainnet" ? "https://api.tzkt.io" : `https://api.${network}.tzkt.io`;
}

function stringToHex(value) {
  return Buffer.from(String(value || ""), "utf8").toString("hex");
}

function delay(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function fetchOperationState({ txHash, network }) {
  const base = tzktApiBase(network);
  const url = `${base}/v1/operations/transactions/${txHash}`;

  try {
    const response = await fetch(url);
    if (!response.ok) return { known: false, applied: false, failed: false };
    const txs = await response.json();
    if (!Array.isArray(txs) || txs.length === 0) return { known: false, applied: false, failed: false };
    const statuses = txs.map((tx) => tx?.status).filter(Boolean);
    const failed = statuses.some((s) => s === "failed" || s === "backtracked");
    const applied = statuses.some((s) => s === "applied");
    return { known: true, applied, failed };
  } catch (error) {
    return { known: false, applied: false, failed: false };
  }
}

async function resolveTokenIdFromChain({ piece, contractAddress, network, retries = 20, delayMs = 1500 }) {
  const keyBytes = stringToHex(piece);
  const base = tzktApiBase(network);
  const url = `${base}/v1/contracts/${contractAddress}/bigmaps/content_hashes/keys/${keyBytes}`;

  for (let i = 0; i < retries; i++) {
    try {
      const response = await fetch(url);
      if (response.ok) {
        const data = await response.json();
        const tokenId = normalizeTokenId(data?.value);
        if (data?.active && tokenId !== null) {
          return tokenId;
        }
      }
    } catch (_) {
      // Retry on transient API/indexing errors
    }
    if (i < retries - 1) {
      await delay(delayMs);
    }
  }

  return null;
}

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  let database;
  try {
    database = await connect();
  } catch (connectError) {
    console.error("❌ MongoDB connection failed:", connectError.message);
    return respond(503, { error: "Database temporarily unavailable" });
  }

  try {
    // Verify user is authenticated
    const user = await authorize(event.headers);
    if (!user) {
      await database.disconnect();
      return respond(401, { error: "Authentication required" });
    }

    // Parse body
    const body = JSON.parse(event.body || "{}");
    const {
      piece,
      tokenId,
      txHash,
      walletAddress,
      network,
      contractAddress,
      contractProfile,
      contractVersion,
      artifactUri,
      thumbnailUri,
      metadataUri,
    } = body;

    if (!piece || !txHash) {
      await database.disconnect();
      return respond(400, { error: "Missing piece or txHash" });
    }

    // Clean piece name (remove $ prefix if present)
    const cleanPiece = piece.replace(/^\$/, "");
    const normalizedNetwork = normalizeNetwork(network);
    const effectiveContractAddress = typeof contractAddress === "string" && contractAddress.trim()
      ? contractAddress.trim()
      : null;
    const normalizedTxHash = String(txHash || "").trim();
    const tokenIdFromRequest = normalizeTokenId(tokenId);
    const contractIdentity = await resolveContractIdentity({
      db: database.db,
      network: normalizedNetwork,
      contractAddress: effectiveContractAddress,
      requestedProfile: contractProfile,
      requestedVersion: contractVersion,
    });

    // Find the kidlisp record
    const collection = database.db.collection("kidlisp");
    const record = await collection.findOne({ code: cleanPiece });

    if (!record) {
      await database.disconnect();
      return respond(404, { error: `Piece '$${cleanPiece}' not found` });
    }

    // Verify ownership (user must own the piece or be admin)
    if (record.user && record.user !== user.sub) {
      console.warn(`❌ User ${user.sub} tried to confirm mint for piece owned by ${record.user}`);
      await database.disconnect();
      return respond(403, { error: "Not authorized to confirm this mint" });
    }

    // Check operation status when indexer already sees it
    const operationState = await fetchOperationState({
      txHash: normalizedTxHash,
      network: normalizedNetwork,
    });
    if (operationState.failed) {
      await database.disconnect();
      return respond(400, { error: "Mint transaction failed on-chain", txHash: normalizedTxHash });
    }

    // Resolve token id from request first, then from on-chain content_hashes bigmap.
    let resolvedTokenId = tokenIdFromRequest;
    if (resolvedTokenId === null && effectiveContractAddress) {
      resolvedTokenId = await resolveTokenIdFromChain({
        piece: cleanPiece,
        contractAddress: effectiveContractAddress,
        network: normalizedNetwork,
      });
    }

    const now = new Date();
    const setOps = {};
    const unsetOps = {};

    if (effectiveContractAddress) {
      const basePath = `tezos.contracts.${effectiveContractAddress}`;
      setOps[`${basePath}.txHash`] = normalizedTxHash;
      setOps[`${basePath}.network`] = normalizedNetwork;
      setOps[`${basePath}.owner`] = walletAddress || null;
      setOps[`${basePath}.walletAddress`] = walletAddress || null;
      setOps[`${basePath}.artifactUri`] = artifactUri || null;
      setOps[`${basePath}.thumbnailUri`] = thumbnailUri || null;
      setOps[`${basePath}.metadataUri`] = metadataUri || null;
      setOps[`${basePath}.lastConfirmAt`] = now;
      setOps[`${basePath}.minted`] = resolvedTokenId !== null;
      setOps[`${basePath}.pending`] = resolvedTokenId === null;
      setOps[`${basePath}.contractProfile`] = contractIdentity.contractProfile || null;
      setOps[`${basePath}.contractVersion`] = contractIdentity.contractVersion || null;
      if (resolvedTokenId !== null) {
        setOps[`${basePath}.tokenId`] = resolvedTokenId;
        setOps[`${basePath}.mintedAt`] = now;
      }
    }

    if (resolvedTokenId !== null) {
      setOps.kept = {
        tokenId: resolvedTokenId,
        txHash: normalizedTxHash,
        walletAddress: walletAddress || null,
        network: normalizedNetwork,
        contractAddress: effectiveContractAddress,
        contractProfile: contractIdentity.contractProfile || null,
        contractVersion: contractIdentity.contractVersion || null,
        artifactUri: artifactUri || null,
        thumbnailUri: thumbnailUri || null,
        metadataUri: metadataUri || null,
        keptAt: now,
        keptBy: user.sub,
      };
    } else {
      setOps.pendingKeep = {
        txHash: normalizedTxHash,
        walletAddress: walletAddress || null,
        network: normalizedNetwork,
        contractAddress: effectiveContractAddress,
        contractProfile: contractIdentity.contractProfile || null,
        contractVersion: contractIdentity.contractVersion || null,
        artifactUri: artifactUri || null,
        thumbnailUri: thumbnailUri || null,
        metadataUri: metadataUri || null,
        requestedAt: now,
        requestedBy: user.sub,
      };
      const existingTokenId = normalizeTokenId(record?.kept?.tokenId);
      if (record?.kept && existingTokenId === null) {
        unsetOps.kept = "";
      }
    }

    if (resolvedTokenId !== null) {
      unsetOps.pendingKeep = "";
    }

    const updateResult = await collection.updateOne(
      { code: cleanPiece },
      {
        $set: setOps,
        ...(Object.keys(unsetOps).length > 0 ? { $unset: unsetOps } : {}),
      }
    );

    if (updateResult.modifiedCount === 0 && updateResult.matchedCount === 0) {
      console.warn(`❌ Failed to update piece ${cleanPiece}`);
      await database.disconnect();
      return respond(500, { error: "Failed to record mint" });
    }

    console.log(`✅ Recorded keep for $${cleanPiece} - Token #${resolvedTokenId ?? "pending"} on ${normalizedNetwork}`);
    console.log(`   TX: ${normalizedTxHash}`);
    console.log(`   Wallet: ${walletAddress || "unknown"}`);

    await database.disconnect();

    return respond(200, {
      success: true,
      piece: cleanPiece,
      tokenId: resolvedTokenId,
      network: normalizedNetwork,
      contractProfile: contractIdentity.contractProfile || null,
      contractVersion: contractIdentity.contractVersion || null,
      pending: resolvedTokenId === null,
      message: resolvedTokenId === null
        ? `Mint submitted on ${normalizedNetwork.toUpperCase()} (awaiting index confirmation)`
        : `Recorded as kept on ${normalizedNetwork.toUpperCase()}`,
    });

  } catch (error) {
    console.error("❌ Keep confirm error:", error);
    if (database) await database.disconnect();
    return respond(500, { error: error.message || "Failed to confirm mint" });
  }
}
