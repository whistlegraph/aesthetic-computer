// keep-confirm.mjs - Confirm a client-side mint and update MongoDB
// 
// POST /api/keep-confirm - Record a successful client-side wallet mint
// This is called after the user signs the transaction with their wallet
// to update the kidlisp record with mint information.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

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

    // Update the record with kept status
    const updateResult = await collection.updateOne(
      { code: cleanPiece },
      {
        $set: {
          kept: {
            tokenId: tokenId || null,
            txHash: txHash,
            walletAddress: walletAddress || null,
            network: network || "ghostnet",
            contractAddress: contractAddress || null,
            artifactUri: artifactUri || null,
            thumbnailUri: thumbnailUri || null,
            metadataUri: metadataUri || null,
            keptAt: new Date(),
            keptBy: user.sub,
          },
        },
      }
    );

    if (updateResult.modifiedCount === 0 && updateResult.matchedCount === 0) {
      console.warn(`❌ Failed to update piece ${cleanPiece}`);
      await database.disconnect();
      return respond(500, { error: "Failed to record mint" });
    }

    console.log(`✅ Recorded keep for $${cleanPiece} - Token #${tokenId || "pending"} on ${network || "ghostnet"}`);
    console.log(`   TX: ${txHash}`);
    console.log(`   Wallet: ${walletAddress || "unknown"}`);

    await database.disconnect();

    return respond(200, {
      success: true,
      piece: cleanPiece,
      tokenId: tokenId || null,
      network: network || "ghostnet",
      message: `Recorded as kept on ${(network || "ghostnet").toUpperCase()}`,
    });

  } catch (error) {
    console.error("❌ Keep confirm error:", error);
    if (database) await database.disconnect();
    return respond(500, { error: error.message || "Failed to confirm mint" });
  }
}
