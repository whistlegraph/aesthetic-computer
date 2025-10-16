// Painting Metadata API
// Returns painting metadata including code by slug and optional handle
// Also supports listing multiple paintings for gallery display
// 
// Single painting: ?slug=xxx&handle=yyy (optional)
// Gallery list: ?limit=20&sort=recent&offset=0 (no slug parameter)

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

  const { slug, handle, limit, sort, offset } = event.queryStringParameters || {};

  // If no slug, return list of paintings (for gallery)
  if (!slug) {
    try {
      await client.connect();
      const db = client.db(dbName);
      const paintings = db.collection("paintings");
      const users = db.collection("users");

      const limitNum = parseInt(limit) || 20;
      const offsetNum = parseInt(offset) || 0;
      const sortType = sort || "recent";

      // Query for paintings with ATProto records
      const query = {
        "atproto.uri": { $exists: true },
        nuked: { $ne: true }
      };

      // Sort options
      const sortOptions = sortType === "random" 
        ? {} // Will use $sample instead
        : { created: -1 }; // Recent

      let paintingsList;
      if (sortType === "random") {
        paintingsList = await paintings.aggregate([
          { $match: query },
          { $sample: { size: limitNum } }
        ]).toArray();
      } else {
        paintingsList = await paintings
          .find(query)
          .sort(sortOptions)
          .skip(offsetNum)
          .limit(limitNum)
          .toArray();
      }

      // Enrich with user codes
      const enriched = await Promise.all(paintingsList.map(async (p) => {
        let userCode = null;
        let userHandle = null;
        
        if (p.user) {
          const user = await users.findOne({ _id: p.user });
          if (user) {
            userCode = user.code;
            userHandle = user.atproto?.handle || user.handle;
          }
        }

        return {
          slug: p.slug,
          code: p.code,
          userCode,
          handle: userHandle,
          created: p.created,
          atproto: p.atproto ? true : false
        };
      }));

      return respond(200, {
        paintings: enriched,
        count: enriched.length,
        offset: offsetNum,
        limit: limitNum
      });

    } catch (error) {
      console.error("Error fetching paintings list:", error);
      return respond(500, { error: "Internal server error" });
    } finally {
      await client.close();
    }
  }

  // Original single painting lookup by slug
  try {
    await client.connect();
    const db = client.db(dbName);
    const paintings = db.collection("paintings");

    // Build query
    // For anonymous paintings, slug might be in colon format (imageCode:recordingCode)
    // Support both exact match and partial match (just imageCode part)
    const query = handle && handle !== "anon"
      ? { slug } // User paintings use timestamp, exact match
      : { 
          $or: [
            { slug }, // Exact match (no recording)
            { slug: new RegExp(`^${slug.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}:`) } // Partial match (has recording)
          ]
        };
    
    // Add user filter if handle provided and not "anon"
    if (handle && handle !== "anon") {
      // Look up user ID from handles collection
      const handles = db.collection("@handles");
      const handleDoc = await handles.findOne({ handle });
      
      if (handleDoc) {
        query.user = handleDoc._id; // The _id of the handle doc IS the user ID
      }
    } else {
      // For anon, look for paintings without user field
      query.user = { $exists: false };
    }

    const painting = await paintings.findOne(query, {
      projection: { code: 1, slug: 1, nuked: 1, _id: 0 }
    });

    if (!painting) {
      return respond(404, { error: "Painting not found" });
    }

    return respond(200, {
      slug: painting.slug,
      code: painting.code || null,
      nuked: painting.nuked || false,
    });

  } catch (error) {
    console.error("Error fetching painting metadata:", error);
    return respond(500, { error: "Internal server error" });
  } finally {
    await client.close();
  }
}
