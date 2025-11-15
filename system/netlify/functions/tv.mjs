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
      when: record.when instanceof Date ? record.when.toISOString() : record.when,
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

async function fetchKidlisp(db, { limit }) {
  const collection = db.collection("kidlisp");
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
        user: 1,
        when: 1,
        handle: 1,
        hits: 1,
        source: 1,
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();

  return records.map((record) => {
    const handle = record.handle ? `@${record.handle}` : null;
    
    return {
      id: record._id?.toString?.() ?? `${record.user}:${record.code}`,
      type: "kidlisp",
      code: record.code,
      owner: {
        handle,
        userId: record.user,
      },
      when: record.when instanceof Date ? record.when.toISOString() : record.when,
      hits: record.hits || 0,
      acUrl: `https://aesthetic.computer/#${record.code}`,
      meta: {
        sourceLength: record.source?.length || 0,
      },
    };
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
      when: record.when instanceof Date ? record.when.toISOString() : record.when,
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
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const params = event.queryStringParameters || {};
  const requestedTypes = parseMediaTypes(params.types);
  const limit = parseLimit(params.limit);

  const media = {};
  const pendingTypes = [];
  const unsupportedTypes = [];

  let database;

  try {
    console.log("📺 Connecting to database...");
    database = await connect();
    console.log("📺 Database connected successfully");

    for (const type of requestedTypes) {
      console.log(`📺 Fetching ${type}...`);
      if (type === "painting") {
        media.paintings = await fetchPaintings(database.db, { limit });
        console.log(`📺 Fetched ${media.paintings.length} paintings`);
      } else if (type === "kidlisp") {
        media.kidlisp = await fetchKidlisp(database.db, { limit });
        console.log(`📺 Fetched ${media.kidlisp.length} kidlisp items`);
      } else if (type === "tape") {
        media.tapes = await fetchTapes(database.db, { limit });
        console.log(`📺 Fetched ${media.tapes.length} tapes`);
      } else if (["mood", "scream", "chat"].includes(type)) {
        // Reserve keys for upcoming media types so clients can experiment early.
        media[`${type}s`] = [];
        pendingTypes.push(type);
      } else {
        unsupportedTypes.push(type);
      }
    }

    const counts = Object.fromEntries(
      Object.entries(media).map(([key, value]) => [key, Array.isArray(value) ? value.length : 0]),
    );

    return respond(200, {
      meta: {
        requestedTypes,
        limit,
        generatedAt: new Date().toISOString(),
        pendingTypes,
        unsupportedTypes,
        counts,
      },
      media,
    });
  } catch (error) {
    console.error("Failed to build TV feed", error);
    console.error("Error stack:", error.stack);
    return respond(500, { error: "Failed to assemble tv feed.", details: error.message });
  } finally {
    await database?.disconnect?.();
  }
}
