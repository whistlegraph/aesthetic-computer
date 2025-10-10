// Painting Metadata API
// Returns painting metadata including code by slug and optional handle

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

  const { slug, handle } = event.queryStringParameters || {};

  if (!slug) {
    return respond(400, { error: "Missing slug parameter" });
  }

  try {
    await client.connect();
    const db = client.db(dbName);
    const paintings = db.collection("paintings");

    // Build query
    const query = { slug };
    
    // Add user filter if handle provided and not "anon"
    if (handle && handle !== "anon") {
      // Look up user ID from handles collection
      const handles = db.collection("@handles");
      const handleDoc = await handles.findOne({ handle });
      
      if (handleDoc) {
        query.user = handleDoc.userId;
      }
    } else {
      // For anon, look for paintings without user field
      query.user = { $exists: false };
    }

    const painting = await paintings.findOne(query, {
      projection: { code: 1, slug: 1, _id: 0 }
    });

    if (!painting) {
      return respond(404, { error: "Painting not found" });
    }

    return respond(200, {
      slug: painting.slug,
      code: painting.code || null,
    });

  } catch (error) {
    console.error("Error fetching painting metadata:", error);
    return respond(500, { error: "Internal server error" });
  } finally {
    await client.close();
  }
}
