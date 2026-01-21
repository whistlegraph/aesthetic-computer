// FF1 Pairing (Netlify Function)
// One-time topicID handoff via token (phone -> desktop)

import * as KeyValue from "../backend/kv.mjs";

const COLLECTION = "ff1Pairings";
const TTL_MS = 10 * 60 * 1000; // 10 minutes

function corsHeaders() {
  return {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type",
  };
}

function isValidToken(token) {
  return typeof token === "string" && /^[a-zA-Z0-9_-]{6,64}$/.test(token);
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 204, headers: corsHeaders(), body: "" };
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

    const { token, topicID, apiKey } = body || {};
    if (!isValidToken(token)) {
      return {
        statusCode: 400,
        headers: corsHeaders(),
        body: JSON.stringify({ success: false, error: "Invalid token" }),
      };
    }
    if (!topicID || typeof topicID !== "string") {
      return {
        statusCode: 400,
        headers: corsHeaders(),
        body: JSON.stringify({ success: false, error: "Missing topicID" }),
      };
    }

    const now = Date.now();
    const payload = {
      topicID: topicID.trim(),
      apiKey: apiKey || null,
      createdAt: now,
      expiresAt: now + TTL_MS,
    };

    try {
      await KeyValue.connect();
      await KeyValue.set(COLLECTION, token, JSON.stringify(payload));
      return {
        statusCode: 200,
        headers: corsHeaders(),
        body: JSON.stringify({ success: true }),
      };
    } catch (err) {
      return {
        statusCode: 500,
        headers: corsHeaders(),
        body: JSON.stringify({ success: false, error: err.message }),
      };
    } finally {
      await KeyValue.disconnect();
    }
  }

  if (event.httpMethod === "GET") {
    const token = event.queryStringParameters?.token;
    if (!isValidToken(token)) {
      return {
        statusCode: 400,
        headers: corsHeaders(),
        body: JSON.stringify({ success: false, error: "Invalid token" }),
      };
    }

    try {
      await KeyValue.connect();
      const raw = await KeyValue.get(COLLECTION, token);
      if (!raw) {
        return {
          statusCode: 404,
          headers: corsHeaders(),
          body: JSON.stringify({ success: false, error: "Not found" }),
        };
      }

      let data;
      try {
        data = JSON.parse(raw);
      } catch {
        data = null;
      }

      if (!data || !data.topicID) {
        await KeyValue.del(COLLECTION, token);
        return {
          statusCode: 410,
          headers: corsHeaders(),
          body: JSON.stringify({ success: false, error: "Expired" }),
        };
      }

      if (data.expiresAt && Date.now() > data.expiresAt) {
        await KeyValue.del(COLLECTION, token);
        return {
          statusCode: 410,
          headers: corsHeaders(),
          body: JSON.stringify({ success: false, error: "Expired" }),
        };
      }

      await KeyValue.del(COLLECTION, token);
      return {
        statusCode: 200,
        headers: corsHeaders(),
        body: JSON.stringify({ success: true, topicID: data.topicID, apiKey: data.apiKey || null }),
      };
    } catch (err) {
      return {
        statusCode: 500,
        headers: corsHeaders(),
        body: JSON.stringify({ success: false, error: err.message }),
      };
    } finally {
      await KeyValue.disconnect();
    }
  }

  return {
    statusCode: 405,
    headers: corsHeaders(),
    body: JSON.stringify({ success: false, error: "Method not allowed" }),
  };
}
