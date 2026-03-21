// FF1 Devices (Netlify Function)
// Stores FF1 device pairing info per user

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";

function corsHeaders() {
  return {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 204, headers: corsHeaders(), body: "" };
  }

  const authHeader = event.headers.authorization || event.headers.Authorization;
  const user = await authorize({ authorization: authHeader });
  if (!user || !user.sub) {
    return {
      statusCode: 401,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: "Unauthorized" }),
    };
  }

  const database = await connect();
  const collection = database.db.collection("ff1Devices");

  try {
    if (event.httpMethod === "GET") {
      const devices = await collection
        .find({ user: user.sub })
        .sort({ updatedAt: -1 })
        .toArray();

      return {
        statusCode: 200,
        headers: corsHeaders(),
        body: JSON.stringify({ success: true, devices }),
      };
    }

    if (event.httpMethod === "POST") {
      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch (err) {
        return {
          statusCode: 400,
          headers: corsHeaders(),
          body: JSON.stringify({ success: false, error: "Invalid JSON body" }),
        };
      }

      const { topicID, apiKey, deviceId, label } = body || {};
      if (!topicID || typeof topicID !== "string") {
        return {
          statusCode: 400,
          headers: corsHeaders(),
          body: JSON.stringify({ success: false, error: "Missing topicID" }),
        };
      }

      const now = new Date();
      await collection.updateOne(
        { user: user.sub, topicID: topicID.trim() },
        {
          $set: {
            user: user.sub,
            topicID: topicID.trim(),
            apiKey: apiKey || null,
            deviceId: deviceId || null,
            label: label || null,
            updatedAt: now,
          },
          $setOnInsert: {
            createdAt: now,
          },
        },
        { upsert: true },
      );

      return {
        statusCode: 200,
        headers: corsHeaders(),
        body: JSON.stringify({ success: true }),
      };
    }

    return {
      statusCode: 405,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: "Method not allowed" }),
    };
  } catch (err) {
    return {
      statusCode: 500,
      headers: corsHeaders(),
      body: JSON.stringify({ success: false, error: err.message }),
    };
  } finally {
    await database.disconnect();
  }
}
