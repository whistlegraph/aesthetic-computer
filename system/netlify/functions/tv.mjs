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
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();

  return records.map((record) => {
    const handle = record.handle ? `@${record.handle}` : null;
    const ownerSegment = handle ?? record.user;
    const mediaPath = `/media/${ownerSegment}/painting/${record.slug}.png`;

    return {
      id: record._id?.toString?.() ?? `${record.user}:${record.slug}`,
      type: "painting",
      slug: record.slug,
      owner: {
        handle,
        userId: record.user,
      },
      when: record.when instanceof Date ? record.when.toISOString() : record.when,
      media: {
        path: mediaPath,
        url: `${mediaOrigin}${mediaPath}`,
      },
      meta: {
        // Placeholder hook for future enrichment (e.g., palette, mood affinity)
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
    database = await connect();

    for (const type of requestedTypes) {
      if (type === "painting") {
        media.paintings = await fetchPaintings(database.db, { limit });
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
    return respond(500, { error: "Failed to assemble tv feed." });
  } finally {
    await database?.disconnect?.();
  }
}
