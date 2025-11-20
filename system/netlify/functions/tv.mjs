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
    
    let mediaPath;
    if (ownerSegment) {
      mediaPath = `/media/${ownerSegment}/painting/${record.slug}.png`;
    } else {
      // Anonymous painting path
      mediaPath = `/media/paintings/${record.code}.png`;
    }

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
        source: record.source, // Include source for tooltip previews
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
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const params = event.queryStringParameters || {};
  const requestedTypes = parseMediaTypes(params.types);
  const limit = parseLimit(params.limit);
  const filter = params.filter?.toLowerCase() || "recent"; // "recent" or "sprinkle"

  const media = {};
  const pendingTypes = [];
  const unsupportedTypes = [];

  let database;

  try {
    console.log("Node version:", process.version);
    // console.log("MongoDB path:", import.meta.resolve("mongodb")); // This might fail if not supported
    
    console.log("📺 Connecting to database...");
    database = await connect();
    console.log("📺 Database connected successfully");

    // For "sprinkle" filter, fetch ALL media types mixed with frecency
    // For other filters, fetch only requested types
    const typesToFetch = filter === "sprinkle" 
      ? ["painting", "kidlisp", "tape"] 
      : requestedTypes;

    // Fetch more items per type to ensure good mixing (3x the limit per type)
    // This ensures we have enough items from each type to create a well-mixed feed
    const fetchLimit = limit * 3;

    for (const type of typesToFetch) {
      console.log(`📺 Fetching ${type}...`);
      if (type === "painting") {
        media.paintings = await fetchPaintings(database.db, { limit: fetchLimit });
        console.log(`📺 Fetched ${media.paintings.length} paintings`);
      } else if (type === "kidlisp") {
        media.kidlisp = await fetchKidlisp(database.db, { limit: fetchLimit });
        console.log(`📺 Fetched ${media.kidlisp.length} kidlisp items`);
      } else if (type === "tape") {
        media.tapes = await fetchTapes(database.db, { limit: fetchLimit });
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

    // Interweave all media types by timestamp for mixed feed
    const allItems = [];
    if (media.kidlisp) allItems.push(...media.kidlisp);
    if (media.paintings) allItems.push(...media.paintings);
    if (media.tapes) allItems.push(...media.tapes);
    
    // Apply sorting based on filter type
    if (filter === "sprinkle") {
      // Sprinkle algorithm: ensures even distribution across all media types
      // regardless of recency or popularity, while still considering frecency
      
      // Separate items by type
      const kidlispItems = allItems.filter(i => i.type === 'kidlisp');
      const paintingItems = allItems.filter(i => i.type === 'painting');
      const tapeItems = allItems.filter(i => i.type === 'tape');
      
      // Apply frecency scoring to each type's items
      const now = Date.now();
      const maxAge = 365 * 24 * 60 * 60 * 1000; // 1 year in milliseconds
      
      const scoreFrecency = (items) => {
        items.forEach(item => {
          // Recency score: newer = higher (0-1 scale)
          const age = now - new Date(item.when || 0).getTime();
          const recencyScore = Math.max(0, 1 - (age / maxAge));
          
          // Frequency score: more hits = higher (0-1 scale, capped at 1000 hits)
          const hits = item.hits || 0;
          const frequencyScore = Math.min(hits / 1000, 1);
          
          // Combined frecency score with heavier weight on recency
          item.frecencyScore = (frequencyScore * 0.3) + (recencyScore * 0.7);
        });
        
        // Sort by frecency within type
        items.sort((a, b) => (b.frecencyScore || 0) - (a.frecencyScore || 0));
      };
      
      scoreFrecency(kidlispItems);
      scoreFrecency(paintingItems);
      scoreFrecency(tapeItems);
      
      // Interweave items with natural variation (not perfectly round-robin)
      // This creates a more organic mix that feels less mechanical
      const sprinkledFeed = [];
      const pools = [
        { items: paintingItems, name: 'painting' },
        { items: tapeItems, name: 'tape' },
        { items: kidlispItems, name: 'kidlisp' }
      ].filter(pool => pool.items.length > 0); // Only use pools that have items
      
      if (pools.length === 0) {
        // No items at all
        console.log(`📺 Sprinkle feed empty - no items available`);
      } else {
        let poolIndices = pools.map(() => 0); // Track position in each pool
        let lastType = null; // Track last type added
        let sameTypeCount = 0; // Allow occasional repeats
        
        while (sprinkledFeed.length < limit) {
          // Find pools that still have items
          const availablePools = pools
            .map((pool, idx) => ({ pool, idx }))
            .filter(({ pool, idx }) => poolIndices[idx] < pool.items.length);
          
          if (availablePools.length === 0) break; // All pools exhausted
          
          // Choose next pool with some randomness
          let chosenPool;
          if (availablePools.length === 1) {
            // Only one pool left, use it
            chosenPool = availablePools[0];
          } else {
            // Weighted random selection that prefers variety but allows repeats
            const sameTypePenalty = lastType ? 0.3 : 0; // Reduce chance of same type
            const weights = availablePools.map(({ pool }) => {
              const baseProbability = 1.0 / availablePools.length;
              const penalty = pool.name === lastType ? sameTypePenalty : 0;
              return Math.max(0.1, baseProbability - penalty);
            });
            
            const totalWeight = weights.reduce((sum, w) => sum + w, 0);
            let random = Math.random() * totalWeight;
            
            let selectedIdx = 0;
            for (let i = 0; i < weights.length; i++) {
              random -= weights[i];
              if (random <= 0) {
                selectedIdx = i;
                break;
              }
            }
            
            chosenPool = availablePools[selectedIdx];
          }
          
          // Add item from chosen pool
          const { pool, idx } = chosenPool;
          sprinkledFeed.push(pool.items[poolIndices[idx]]);
          poolIndices[idx]++;
          
          // Update tracking
          if (pool.name === lastType) {
            sameTypeCount++;
          } else {
            sameTypeCount = 1;
            lastType = pool.name;
          }
        }
      }
      
      // Replace allItems with sprinkled feed for final output
      allItems.length = 0;
      allItems.push(...sprinkledFeed);
      
      const typeCounts = {
        kidlisp: sprinkledFeed.filter(i => i.type === 'kidlisp').length,
        paintings: sprinkledFeed.filter(i => i.type === 'painting').length,
        tapes: sprinkledFeed.filter(i => i.type === 'tape').length
      };
      console.log(`📺 Sprinkle feed created with natural distribution: ${allItems.length} items (${typeCounts.kidlisp} kidlisp, ${typeCounts.paintings} paintings, ${typeCounts.tapes} tapes)`);
    } else {
      // Default: sort by timestamp (most recent first)
      allItems.sort((a, b) => {
        const timeA = new Date(a.when || 0).getTime();
        const timeB = new Date(b.when || 0).getTime();
        return timeB - timeA; // Descending order (newest first)
      });
    }
    
    // Limit the total mixed feed
    const mixedFeed = allItems.slice(0, limit);
    
    console.log(`📺 Mixed feed created: ${mixedFeed.length} items (${mixedFeed.filter(i => i.type === 'kidlisp').length} kidlisp, ${mixedFeed.filter(i => i.type === 'painting').length} paintings, ${mixedFeed.filter(i => i.type === 'tape').length} tapes)`);

    return respond(200, {
      meta: {
        requestedTypes,
        limit,
        filter,
        generatedAt: new Date().toISOString(),
        pendingTypes,
        unsupportedTypes,
        counts,
        mixedCount: mixedFeed.length,
      },
      media,
      mixed: mixedFeed, // Add mixed/interwoven feed
    });
  } catch (error) {
    console.error("Failed to build TV feed", error);
    return respond(500, { error: "Failed to build TV feed", details: error.message });
  }
}
