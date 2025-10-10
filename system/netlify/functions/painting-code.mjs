// Painting Code Lookup API
// Returns painting slug by short code (e.g., "k3d" or "WDv")

import { MongoClient } from "mongodb";

const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING);
const dbName = process.env.MONGODB_NAME || "aesthetic";

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Headers": "Content-Type",
      "Access-Control-Allow-Methods": "GET, OPTIONS",
    },
    body: JSON.stringify(body),
  };
}

export async function handler(event) {
  // Handle preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  const { code } = event.queryStringParameters || {};

  if (!code) {
    return respond(400, { error: "Missing code parameter" });
  }

  try {
    await client.connect();
    const db = client.db(dbName);
    const paintings = db.collection("paintings");

    // Look up painting by code
    const painting = await paintings.findOne(
      { code },
      { projection: { slug: 1, code: 1, user: 1, _id: 0 } }
    );

    if (!painting) {
      return respond(404, { error: "Painting not found" });
    }

    // Get handle if painting has a user
    let handle = "anon";
    if (painting.user) {
      const handles = db.collection("@handles");
      const handleDoc = await handles.findOne(
        { userId: painting.user },
        { projection: { handle: 1, _id: 0 } }
      );
      if (handleDoc) {
        handle = handleDoc.handle;
      }
    }

    return respond(200, {
      slug: painting.slug,
      code: painting.code,
      handle: handle,
    });

  } catch (error) {
    console.error("Error looking up painting code:", error);
    return respond(500, { error: "Internal server error" });
  } finally {
    await client.close();
  }
}
