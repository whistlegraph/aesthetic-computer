// api/tv, 2025.10.02
// Returns feed data for the TV (For-You) experience.

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

const DEFAULT_MEDIA_TYPES = ["painting"];
const MAX_LIMIT = 500;
const DEFAULT_LIMIT = 120;

const dev =
  process.env.NETLIFY_DEV === "true" ||
  process.env.CONTEXT === "dev" ||
  process.env.NODE_ENV === "development";
const mediaOrigin =
  process.env.MEDIA_BASE_URL || (dev ? "https://localhost:8888" : "https://aesthetic.computer");

function parseMediaTypes(rawTypes) {
  if (!rawTypes) return [...DEFAULT_MEDIA_TYPES];
  return Array.from(
    new Set(
      rawTypes
        .split(",")
        .map((type) => type.trim().toLowerCase())
        .filter(Boolean),
    ),
  );
}

function parseLimit(rawLimit) {
  const numeric = Number.parseInt(rawLimit, 10);
  if (Number.isNaN(numeric)) return DEFAULT_LIMIT;
  return Math.min(Math.max(numeric, 1), MAX_LIMIT);
}

async function fetchPaintings(db, { limit }) {
  const collection = db.collection("paintings");
  const pipeline = [
    { $match: { nuked: { $ne: true } } },
    { $sort: { when: -1 } },
    { $limit: limit },
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
        slug: 1,
        user: 1,
        when: 1,
        handle: 1,
        atproto: 1,
        code: 1,
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();

  return records.map((record) => {
    const handle = record.handle ? `@${record.handle}` : null;
    const ownerSegment = handle ?? record.user;
    const mediaPath = `/media/${ownerSegment}/painting/${record.slug}.png`;

    // Prefer ATProto blob URL if available
    let thumbnailUrl = `${mediaOrigin}${mediaPath}`;
    if (record.atproto?.thumbnail?.ref) {
      const blobCid = record.atproto.thumbnail.ref.$link || record.atproto.thumbnail.ref;
      thumbnailUrl = `https://at.aesthetic.computer/xrpc/com.atproto.sync.getBlob?did=${record.atproto.did}&cid=${blobCid}`;
    }

    return {
      id: record._id?.toString?.() ?? `${record.user}:${record.slug}`,
      type: "painting",
      slug: record.slug,
      code: record.code,
      owner: {
        handle,
        userId: record.user,
      },
      when: record.when,
      media: {
        path: mediaPath,
        url: thumbnailUrl,
      },
      atproto: record.atproto ? {
        uri: record.atproto.uri,
        cid: record.atproto.cid,
        did: record.atproto.did,
      } : undefined,
      meta: {
        // Placeholder hook for future enrichment (e.g., palette, mood affinity)
      },
    };
  });
}

async function fetchKidlisp(db, { limit, sort }) {
  const collection = db.collection("kidlisp");
  
  let sortStage = { $sort: { when: -1 } };
  if (sort === 'hits') {
    sortStage = { $sort: { hits: -1 } };
  }

  const pipeline = [
    { $match: { nuked: { $ne: true } } },
    sortStage,
    { $limit: limit },
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
        user: 1,
        when: 1,
        handle: 1,
        hits: 1,
        source: 1,
      },
    },
  ];

  console.log("Fetching kidlisp with pipeline:", JSON.stringify(pipeline));
  const records = await collection.aggregate(pipeline).toArray();
  console.log(`Fetched ${records.length} kidlisp records`);

  return records.map((record, index) => {
    try {
      const handle = record.handle ? `@${record.handle}` : null;
      
      return {
        id: record._id?.toString?.() ?? `${record.user}:${record.code}`,
        type: "kidlisp",
        code: record.code,
        owner: {
          handle,
          userId: record.user,
        },
        when: record.when,
        hits: record.hits || 0,
        acUrl: `https://aesthetic.computer/#${record.code}`,
        meta: {
          sourceLength: record.source?.length || 0,
        },
      };
    } catch (err) {
      console.error(`Error processing record at index ${index}:`, record);
      throw err;
    }
  });
}

async function fetchTapes(db, { limit }) {
  const collection = db.collection("tapes");
  const pipeline = [
    { $match: { nuked: { $ne: true } } },
    { $sort: { when: -1 } },
    { $limit: limit },
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
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();

  return records.map((record) => {
    const handle = record.handle ? `@${record.handle}` : null;
    const ownerSegment = handle ?? record.user ?? "anonymous";
    
    return {
      id: record._id?.toString?.() ?? `${ownerSegment}:${record.slug}`,
      type: "tape",
      code: record.code,
      slug: record.slug,
      owner: {
        handle,
        userId: record.user ?? null, // null for anonymous tapes
      },
      when: record.when,
      acUrl: `https://aesthetic.computer/!${record.code}`,
      media: {
        bucket: record.bucket,
        // Video URL pattern (MP4 is served from S3)
        videoUrl: record.bucket ? `https://acrecordings.s3.us-east-1.amazonaws.com/${record.bucket}/${record.slug}.mp4` : null,
      },
    };
  });
}

export async function handler(event) {
  try {
    console.log("Node version:", process.version);
    // console.log("MongoDB path:", import.meta.resolve("mongodb")); // This might fail if not supported
    
    console.log("📺 Connecting to database...");
    const { db, disconnect } = await connect();
    console.log("📺 Database connected successfully");
    await disconnect();
    return respond(200, { status: "ok" });
  } catch (error) {
    console.error("Failed to connect", error);
    return respond(500, { error: "Failed to connect", details: error.message });
  }
}
