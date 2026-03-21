// keep-update-confirm.mjs - Confirm a client-side metadata update and update MongoDB
// 
// POST /api/keep-update-confirm - Record a successful client-side wallet metadata update
// This is called after the user signs the edit_metadata transaction with their wallet
// to update the kidlisp record with the new URIs.

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getKeepsContractAddress, LEGACY_KEEPS_CONTRACT } from "../../backend/tezos-keeps-contract.mjs";

// Configuration
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";

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

function normalizeTokenId(value) {
  const parsed = Number.parseInt(`${value ?? ""}`, 10);
  return Number.isInteger(parsed) && parsed >= 0 ? parsed : null;
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
      console.warn("⚠️ keep-update-confirm: contract identity fallback failed:", error?.message || error);
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
      artifactUri,
      thumbnailUri,
      metadataUri,
      contractAddress,
      contractProfile,
      contractVersion,
    } = body;

    if (!piece || !txHash) {
      await database.disconnect();
      return respond(400, { error: "Missing piece or txHash" });
    }

    // Clean piece name (remove $ prefix if present)
    const cleanPiece = piece.replace(/^\$/, "");
    const defaultContract = await getKeepsContractAddress({
      db: database.db,
      network: NETWORK,
      fallback: LEGACY_KEEPS_CONTRACT,
    });
    const effectiveContract = contractAddress || defaultContract;
    const contractIdentity = await resolveContractIdentity({
      db: database.db,
      network: NETWORK,
      contractAddress: effectiveContract,
      requestedProfile: contractProfile,
      requestedVersion: contractVersion,
    });
    const normalizedTokenId = normalizeTokenId(tokenId);

    // Find the kidlisp record
    const collection = database.db.collection("kidlisp");
    const record = await collection.findOne({ code: cleanPiece });

    if (!record) {
      await database.disconnect();
      return respond(404, { error: `Piece '$${cleanPiece}' not found` });
    }

    // Verify ownership (user must own the piece or be admin)
    const isAdmin = await hasAdmin(user);
    if (!isAdmin && record.user && record.user !== user.sub) {
      console.warn(`❌ User ${user.sub} tried to confirm update for piece owned by ${record.user}`);
      await database.disconnect();
      return respond(403, { error: "Not authorized to confirm this update" });
    }

    // Update the record - move pending URIs to actual URIs and record the update
    const updateResult = await collection.updateOne(
      { code: cleanPiece },
      {
        $set: {
          // Update contract-specific data
          [`tezos.contracts.${effectiveContract}.artifactUri`]: artifactUri,
          [`tezos.contracts.${effectiveContract}.thumbnailUri`]: thumbnailUri,
          [`tezos.contracts.${effectiveContract}.metadataUri`]: metadataUri,
          [`tezos.contracts.${effectiveContract}.lastUpdatedAt`]: new Date(),
          [`tezos.contracts.${effectiveContract}.lastUpdateTxHash`]: txHash,
          [`tezos.contracts.${effectiveContract}.contractProfile`]: contractIdentity.contractProfile || null,
          [`tezos.contracts.${effectiveContract}.contractVersion`]: contractIdentity.contractVersion || null,
          kept: {
            ...(record?.kept && typeof record.kept === "object" ? record.kept : {}),
            ...(normalizedTokenId !== null ? { tokenId: normalizedTokenId } : {}),
            txHash: txHash || record?.kept?.txHash || null,
            walletAddress: record?.kept?.walletAddress || null,
            network: NETWORK,
            contractAddress: effectiveContract,
            contractProfile: contractIdentity.contractProfile || null,
            contractVersion: contractIdentity.contractVersion || null,
            artifactUri: artifactUri || null,
            thumbnailUri: thumbnailUri || null,
            metadataUri: metadataUri || null,
            keptAt: record?.kept?.keptAt || new Date(),
            keptBy: record?.kept?.keptBy || user.sub,
          },
        },
        $unset: {
          // Clear pending state
          pendingRebake: "",
          [`tezos.contracts.${effectiveContract}.pendingMetadataUri`]: "",
          [`tezos.contracts.${effectiveContract}.pendingArtifactUri`]: "",
          [`tezos.contracts.${effectiveContract}.pendingThumbnailUri`]: "",
        }
      }
    );

    if (updateResult.modifiedCount === 0 && updateResult.matchedCount === 0) {
      console.warn(`❌ Failed to update piece ${cleanPiece}`);
      await database.disconnect();
      return respond(500, { error: "Failed to record update" });
    }

    console.log(`✅ Confirmed metadata update for $${cleanPiece} (token #${tokenId || "?"}): ${txHash}`);

    await database.disconnect();
    return respond(200, {
      success: true,
      piece: cleanPiece,
      tokenId,
      txHash,
      artifactUri,
      thumbnailUri,
      metadataUri,
      contractProfile: contractIdentity.contractProfile || null,
      contractVersion: contractIdentity.contractVersion || null,
    });

  } catch (err) {
    console.error("❌ keep-update-confirm error:", err);
    if (database) await database.disconnect();
    return respond(500, { error: err.message });
  }
}
