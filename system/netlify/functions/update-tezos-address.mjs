/**
 * update-tezos-address - Update user's Tezos wallet address in profile
 * Called when user connects their wallet via the AC interface or keeps.html
 */

import { connect } from "../../backend/database.mjs";
import { authorize } from "../../backend/authorization.mjs";

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Access-Control-Allow-Methods": "POST, OPTIONS",
  "Content-Type": "application/json",
};

function jsonResponse(statusCode, body) {
  return { statusCode, headers: CORS_HEADERS, body: JSON.stringify(body) };
}

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers: CORS_HEADERS, body: "" };
  }

  if (event.httpMethod !== "POST") {
    return jsonResponse(405, { error: "Method Not Allowed" });
  }

  try {
    // Verify authentication
    const authHeader = event.headers.authorization || event.headers.Authorization;
    if (!authHeader) {
      return jsonResponse(401, { error: "Missing authorization header" });
    }

    const userInfo = await authorize({ authorization: authHeader });
    if (!userInfo || !userInfo.sub) {
      return jsonResponse(401, { error: "Invalid token" });
    }

    const userId = userInfo.sub;

    // Parse request body
    const { address, network } = JSON.parse(event.body || "{}");
    if (!address || !network) {
      return jsonResponse(400, { error: "Missing address or network" });
    }

    // Validate Tezos address format (tz1, tz2, tz3, or KT1)
    if (!address.match(/^(tz1|tz2|tz3|KT1)[1-9A-HJ-NP-Za-km-z]{33}$/)) {
      return jsonResponse(400, { error: "Invalid Tezos address format" });
    }

    // Update user record
    const { db } = await connect();
    const result = await db.collection("users").updateOne(
      { _id: userId },
      {
        $set: {
          "tezos.address": address,
          "tezos.network": network,
          "tezos.connectedAt": new Date(),
        },
      }
    );

    if (result.matchedCount === 0) {
      console.warn(`⚠️ update-tezos-address: No user doc with _id=${userId}`);
      return jsonResponse(404, { error: "User not found" });
    }

    console.log(`✅ Updated Tezos address for user ${userId}: ${address} (${network})`);

    return jsonResponse(200, { success: true, address, network });
  } catch (err) {
    console.error("Error updating Tezos address:", err);
    return jsonResponse(500, { error: err.message });
  }
}
