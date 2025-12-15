/**
 * update-tezos-address - Update user's Tezos wallet address in profile
 * Called when user connects their wallet via the AC interface
 */

import { connect } from "../../backend/database.mjs";
import { authorize } from "../../backend/authorization.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return { statusCode: 405, body: "Method Not Allowed" };
  }

  try {
    // Verify authentication
    const authHeader = event.headers.authorization || event.headers.Authorization;
    if (!authHeader) {
      return {
        statusCode: 401,
        body: JSON.stringify({ error: "Missing authorization header" }),
      };
    }

    const userInfo = await authorize({ authorization: authHeader });
    if (!userInfo || !userInfo.sub) {
      return {
        statusCode: 401,
        body: JSON.stringify({ error: "Invalid token" }),
      };
    }

    const userId = userInfo.sub;

    // Parse request body
    const { address, network } = JSON.parse(event.body);
    if (!address || !network) {
      return {
        statusCode: 400,
        body: JSON.stringify({ error: "Missing address or network" }),
      };
    }

    // Validate Tezos address format (tz1, tz2, tz3, or KT1)
    if (!address.match(/^(tz1|tz2|tz3|KT1)[1-9A-HJ-NP-Za-km-z]{33}$/)) {
      return {
        statusCode: 400,
        body: JSON.stringify({ error: "Invalid Tezos address format" }),
      };
    }

    // Update user record
    const { db } = await connect();
    const result = await db.collection("users").updateOne(
      { sub: userId },
      {
        $set: {
          "tezos.address": address,
          "tezos.network": network,
          "tezos.connectedAt": new Date(),
        },
      }
    );

    if (result.matchedCount === 0) {
      return {
        statusCode: 404,
        body: JSON.stringify({ error: "User not found" }),
      };
    }

    console.log(`✅ Updated Tezos address for user ${userId}: ${address} (${network})`);

    return {
      statusCode: 200,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        success: true,
        address,
        network,
      }),
    };
  } catch (err) {
    console.error("Error updating Tezos address:", err);
    return {
      statusCode: 500,
      body: JSON.stringify({ error: err.message }),
    };
  }
}
