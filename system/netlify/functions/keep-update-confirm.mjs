// keep-update-confirm.mjs - Confirm a client-side metadata update and update MongoDB
// 
// POST /api/keep-update-confirm - Record a successful client-side wallet metadata update
// This is called after the user signs the edit_metadata transaction with their wallet
// to update the kidlisp record with the new URIs.

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

// Configuration - Mainnet v5 RC contract by default
const CONTRACT_ADDRESS = process.env.TEZOS_KEEPS_CONTRACT || "KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido";

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
    } = body;

    if (!piece || !txHash) {
      await database.disconnect();
      return respond(400, { error: "Missing piece or txHash" });
    }

    // Clean piece name (remove $ prefix if present)
    const cleanPiece = piece.replace(/^\$/, "");
    const effectiveContract = contractAddress || CONTRACT_ADDRESS;

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
    });

  } catch (err) {
    console.error("❌ keep-update-confirm error:", err);
    if (database) await database.disconnect();
    return respond(500, { error: err.message });
  }
}
