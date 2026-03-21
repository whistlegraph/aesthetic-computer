// tv-tapes.mjs — Apple TV Tapes Feed
// GET /api/tv/tapes — Returns tape feed optimized for Apple TV playback
// 2026.01.01

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

const PDS_URL = "https://at.aesthetic.computer";
const DEFAULT_LIMIT = 50;
const MAX_LIMIT = 200;

/**
 * Fetch tapes with ATProto video URLs for Apple TV
 */
async function fetchTapesForTV(db, { limit }) {
  const collection = db.collection("tapes");
  const users = db.collection("users");
  
  // Get recent tapes from MongoDB
  const pipeline = [
    { $match: { nuked: { $ne: true } } },
    { $sort: { when: -1 } },
    { $limit: limit * 3 }, // Fetch more to filter for those with ATProto videos
    {
      $lookup: {
        from: "@handles",
        localField: "user",
        foreignField: "_id",
        as: "handleInfo",
      },
    },
    {
      $addFields: {
        handle: { $arrayElemAt: ["$handleInfo.handle", 0] },
      },
    },
    {
      $project: {
        _id: 1,
        code: 1,
        slug: 1,
        user: 1,
        when: 1,
        handle: 1,
        bucket: 1,
        // Include metadata if stored
        duration: 1,
        resolution: 1,
        fps: 1,
        seed: 1,
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();
  
  // Build user DID map
  const userDids = new Map();
  const uniqueUserIds = [...new Set(records.map(r => r.user).filter(Boolean))];
  
  if (uniqueUserIds.length > 0) {
    const userRecords = await users.find({ _id: { $in: uniqueUserIds } }).toArray();
    for (const u of userRecords) {
      if (u.atproto?.did) {
        userDids.set(u._id, u.atproto.did);
      }
    }
  }
  
  // Get art-guest DID for anonymous tapes
  const artGuest = await users.findOne({ _id: "art-guest" });
  const artGuestDid = artGuest?.atproto?.did;
  
  // Fetch ATProto records to get video blob CIDs
  const atprotoTapesByCode = new Map();
  const uniqueDids = [...new Set([...userDids.values(), artGuestDid].filter(Boolean))];
  
  await Promise.all(uniqueDids.map(async (did) => {
    try {
      const atprotoUrl = `${PDS_URL}/xrpc/com.atproto.repo.listRecords?repo=${did}&collection=computer.aesthetic.tape&limit=100`;
      const atRes = await fetch(atprotoUrl);
      if (!atRes.ok) return;
      
      const atData = await atRes.json();
      for (const atRecord of atData.records || []) {
        if (atRecord.value?.video?.ref?.$link && atRecord.value?.code) {
          atprotoTapesByCode.set(atRecord.value.code, {
            did,
            videoCid: atRecord.value.video.ref.$link,
            // Extract metadata from ATProto record if available
            duration: atRecord.value?.duration,
            resolution: atRecord.value?.resolution,
            fps: atRecord.value?.fps,
          });
        }
      }
    } catch (err) {
      console.error(`Error fetching ATProto tapes for ${did}:`, err);
    }
  }));
  
  // S3/Spaces URL for streaming (supports byte-range requests)
  const SPACES_URL = "https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com";
  
  // Build Apple TV-friendly tape objects
  const tapes = [];
  
  for (const record of records) {
    if (tapes.length >= limit) break;
    
    // Skip if no ATProto record exists (for validation)
    const atInfo = atprotoTapesByCode.get(record.code);
    if (!atInfo) continue;
    
    // Use S3/Spaces URL which supports byte-range streaming for AVPlayer
    const videoUrl = `${SPACES_URL}/tapes/${record.code}.mp4`;
    const thumbnailUrl = `${SPACES_URL}/tapes/${record.code}-thumb.jpg`;
    const handle = record.handle ? `@${record.handle}` : null;
    
    tapes.push({
      id: record.code,
      title: `Tape ${record.code.toUpperCase()}`,
      mp4: videoUrl,
      thumbnail: thumbnailUrl,
      duration: record.duration || atInfo.duration || null,
      resolution: record.resolution || atInfo.resolution || "1280x720",
      fps: record.fps || atInfo.fps || 30,
      seed: record.seed || null,
      generator: "kidlisp-oven",
      created_at: record.when,
      owner: handle,
      tags: ["tape"],
    });
  }
  
  return tapes;
}

export async function handler(event) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 200,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "GET, OPTIONS",
        "Access-Control-Allow-Headers": "Content-Type",
        "Access-Control-Max-Age": "86400",
      },
      body: "",
    };
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const params = event.queryStringParameters || {};
  const limit = Math.min(Math.max(parseInt(params.limit) || DEFAULT_LIMIT, 1), MAX_LIMIT);

  try {
    console.log("📺 Apple TV: Connecting to database...");
    const database = await connect();
    
    console.log(`📺 Apple TV: Fetching up to ${limit} tapes...`);
    const tapes = await fetchTapesForTV(database.db, { limit });
    console.log(`📺 Apple TV: Found ${tapes.length} tapes with videos`);

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
        "Cache-Control": "public, max-age=300", // 5 minute cache
      },
      body: JSON.stringify({
        tapes,
        total: tapes.length,
        lastUpdated: new Date().toISOString(),
      }),
    };

  } catch (error) {
    console.error("📺 Apple TV: Error fetching tapes:", error);
    return respond(500, { error: error.message || String(error) });
  }
}
