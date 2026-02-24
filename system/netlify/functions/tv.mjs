// api/tv, 2025.10.02
// Returns feed data for the TV (For-You) experience.
// Now with Redis caching (5 min TTL for give page calls).

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";
import { getOrCompute, CACHE_TTLS } from "../../backend/cache.mjs";

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

async function fetchKidlisp(db, { limit, sort, boost }) {
  const collection = db.collection("kidlisp");
  const handlesCollection = db.collection("@handles");
  
  // High-quality handles that get boosted in results
  // These creators consistently produce good KidLisp pieces
  const BOOSTED_HANDLES = ['jeffrey', 'jas', 'rapter'];
  const BOOST_MULTIPLIER = 3; // How many extra slots boosted handles get
  
  let sortStage = { $sort: { when: -1 } };
  if (sort === 'hits') {
    // Sort by hits descending, then by when descending for ties
    // Also handle null/missing hits field (treat as 0)
    sortStage = { $sort: { hits: -1, when: -1 } };
  }

  const pipeline = [
    { $match: { nuked: { $ne: true } } },
    // Add a field to handle null hits before sorting
    ...(sort === 'hits' ? [{ $addFields: { hits: { $ifNull: ["$hits", 0] } } }] : []),
    sortStage,
    { $limit: limit * 2 }, // Fetch extra to allow for boosting/mixing
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
  
  // When sorting by hits, allow disk use to scan entire collection
  const aggregateOptions = sort === 'hits' 
    ? { allowDiskUse: true }
    : {};
  
  let records = await collection.aggregate(pipeline, aggregateOptions).toArray();
  console.log(`Fetched ${records.length} kidlisp records`);
  
  // Apply handle boosting if enabled (default on for give page calls)
  if (boost !== 'false') {
    // Separate boosted and regular records
    const boostedRecords = records.filter(r => BOOSTED_HANDLES.includes(r.handle));
    const regularRecords = records.filter(r => !BOOSTED_HANDLES.includes(r.handle));
    
    // Interleave boosted records more frequently
    // For every BOOST_MULTIPLIER regular records, insert a boosted one
    const mixed = [];
    let boostedIdx = 0;
    let regularIdx = 0;
    let insertCounter = 0;
    
    while (mixed.length < limit && (boostedIdx < boostedRecords.length || regularIdx < regularRecords.length)) {
      // Every few items, try to insert a boosted record
      if (insertCounter % (BOOST_MULTIPLIER + 1) === BOOST_MULTIPLIER && boostedIdx < boostedRecords.length) {
        mixed.push(boostedRecords[boostedIdx++]);
      } else if (regularIdx < regularRecords.length) {
        mixed.push(regularRecords[regularIdx++]);
      } else if (boostedIdx < boostedRecords.length) {
        mixed.push(boostedRecords[boostedIdx++]);
      }
      insertCounter++;
    }
    
    records = mixed;
    console.log(`After boosting: ${records.length} records (boosted handles: ${BOOSTED_HANDLES.join(', ')})`);
  } else {
    records = records.slice(0, limit);
  }

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
        assetUrl: `https://oven.aesthetic.computer/grab/webp/100/100/$${record.code}?duration=3000&fps=8&quality=80&density=1`,
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
  const PDS_URL = "https://at.aesthetic.computer";
  const collection = db.collection("tapes");
  const users = db.collection("users");
  
  // First get recent tapes from MongoDB
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
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();
  
  // Build a map of user DID lookups (for ATProto blob URLs)
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
  
  // Also get the art-guest DID for anonymous tapes
  const artGuest = await users.findOne({ _id: "art-guest" });
  const artGuestDid = artGuest?.atproto?.did;
  
  // Fetch ATProto records for each unique DID (batch per user)
  const atprotoTapesByCode = new Map(); // code -> { did, videoCid }
  const uniqueDids = [...new Set([...userDids.values(), artGuestDid].filter(Boolean))];
  
  await Promise.all(uniqueDids.map(async (did) => {
    try {
      const atprotoUrl = `${PDS_URL}/xrpc/com.atproto.repo.listRecords?repo=${did}&collection=computer.aesthetic.tape&limit=100`;
      const atRes = await fetch(atprotoUrl, { signal: AbortSignal.timeout(5000) });
      if (!atRes.ok) return;

      const atData = await atRes.json();
      for (const atRecord of atData.records || []) {
        if (atRecord.value?.video?.ref?.$link && atRecord.value?.code) {
          atprotoTapesByCode.set(atRecord.value.code, {
            did,
            videoCid: atRecord.value.video.ref.$link,
          });
        }
      }
    } catch (err) {
      console.error(`Error fetching ATProto tapes for ${did}:`, err);
    }
  }));
  
  // S3/Spaces URL for streaming (supports byte-range requests, unlike PDS blob endpoint)
  const SPACES_URL = "https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com";
  
  // Build results - match MongoDB tapes with ATProto video URLs
  const tapesWithVideos = [];
  
  for (const record of records) {
    if (tapesWithVideos.length >= limit) break;
    
    const atInfo = atprotoTapesByCode.get(record.code);
    if (!atInfo) continue;
    
    // Use S3/Spaces URL which supports byte-range streaming (PDS blob endpoint doesn't work for video)
    const videoUrl = `${SPACES_URL}/tapes/${record.code}.mp4`;
    
    const handle = record.handle ? `@${record.handle}` : null;
    const ownerSegment = handle ?? record.user ?? "anonymous";
    
    tapesWithVideos.push({
      id: record._id?.toString?.() ?? `${ownerSegment}:${record.slug}`,
      type: "tape",
      code: record.code,
      slug: record.slug,
      owner: {
        handle,
        userId: record.user ?? null,
      },
      when: record.when,
      acUrl: `https://aesthetic.computer/!${record.code}`,
      media: {
        bucket: record.bucket,
        videoUrl,
        source: "atproto",
      },
    });
  }
  
  return tapesWithVideos;
}

async function fetchClocks(db, { limit }) {
  const collection = db.collection("clocks");
  
  const pipeline = [
    { $match: { nuked: { $ne: true } } },
    // Add a field to handle null hits before sorting
    { $addFields: { hits: { $ifNull: ["$hits", 0] } } },
    { $sort: { hits: -1, when: -1 } },
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
      type: "clock",
      code: record.code,
      source: record.source,
      owner: {
        handle,
        userId: record.user,
      },
      when: record.when,
      hits: record.hits || 0,
      acUrl: `https://aesthetic.computer/*${record.code}`,
      meta: {
        sourceLength: record.source?.length || 0,
      },
    };
  });
}

export async function handler(event) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const params = event.queryStringParameters || {};
  const requestedTypes = parseMediaTypes(params.types);
  const limit = parseLimit(params.limit);
  const filter = params.filter?.toLowerCase() || "recent"; // "recent" or "sprinkle"
  const sort = params.sort?.toLowerCase(); // "hits" for all-time popularity
  const boost = params.boost; // "false" to disable handle boosting for kidlisp
  const format = params.format?.toLowerCase(); // "dp1" for DP-1 playlist format
  const duration = parseInt(params.duration) || 24; // Duration per item in DP-1 playlist (default 24s)

  // Create cache key from query params
  const cacheKey = `give:tv:${requestedTypes.join(',')}:${limit}:${filter}:${sort || 'default'}:${boost || 'true'}:${format || 'json'}:${duration}`;
  
  try {
    // Use caching for TV feed (5 min TTL)
    const result = await getOrCompute(
      cacheKey,
      () => fetchTVFeed({ requestedTypes, limit, filter, sort, boost }),
      CACHE_TTLS.TV_RECENT
    );
    
    // If DP-1 format requested, transform the response
    if (format === 'dp1') {
      const dp1Playlist = transformToDP1(result, { duration, requestedTypes });
      return respond(200, dp1Playlist);
    }
    
    return respond(200, result);
  } catch (error) {
    console.error("Failed to build TV feed", error);
    return respond(500, { error: "Failed to build TV feed", details: error.message });
  }
}

/**
 * Transform TV feed response to DP-1 playlist format
 * Used by FF1 and other DP-1 compatible devices
 */
function transformToDP1(tvResult, { duration, requestedTypes }) {
  const now = new Date();
  const dateStr = now.toLocaleDateString('en-US', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });
  
  // Determine which items to use based on requested types
  let items = [];
  
  // For kidlisp-only, use the kidlisp array directly (maintains sort order)
  if (requestedTypes.length === 1 && requestedTypes[0] === 'kidlisp' && tvResult.media?.kidlisp) {
    items = tvResult.media.kidlisp;
  } else {
    // For mixed feeds, use the mixed array
    items = tvResult.mixed || [];
  }
  
  const total = items.length;
  
  // Build DP-1 playlist
  const playlist = {
    dpVersion: "1.1.0",
    title: `Top ${total} KidLisp Hits`,
    summary: `The ${total} most popular KidLisp pieces by total hits`,
    items: items
      .filter(item => item.type === 'kidlisp') // Only KidLisp items for now
      .map((item, index) => {
        const code = item.code;
        return {
          title: `$${code}`,
          source: `https://device.kidlisp.com/$${code}?playlist=true&duration=${duration}&index=${index}&total=${total}`,
          duration: duration,
          license: "open",
          provenance: {
            type: "offChainURI",
            uri: `https://kidlisp.com/$${code}`
          }
        };
      }),
    defaults: {
      display: {
        scaling: "fit",
        background: "#000000",
        margin: "0%"
      },
      license: "open",
      duration: duration
    }
  };
  
  return playlist;
}

// Extracted TV feed fetching logic for caching
async function fetchTVFeed({ requestedTypes, limit, filter, sort, boost }) {
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
      ? ["painting", "kidlisp", "tape", "clock"] 
      : requestedTypes;

    // Fetch more items per type to ensure good mixing (3x the limit per type)
    // This ensures we have enough items from each type to create a well-mixed feed
    const fetchLimit = limit * 3;

    // Fetch all media types in parallel (they're independent DB queries)
    const fetchPromises = [];
    for (const type of typesToFetch) {
      console.log(`📺 Fetching ${type}...`);
      if (type === "painting") {
        fetchPromises.push(
          fetchPaintings(database.db, { limit: fetchLimit }).then(r => {
            media.paintings = r;
            console.log(`📺 Fetched ${r.length} paintings`);
          })
        );
      } else if (type === "kidlisp") {
        const kidlispLimit = sort === 'hits' ? limit : fetchLimit;
        fetchPromises.push(
          fetchKidlisp(database.db, { limit: kidlispLimit, sort, boost }).then(r => {
            media.kidlisp = r;
            console.log(`📺 Fetched ${r.length} kidlisp items`);
          })
        );
      } else if (type === "tape") {
        fetchPromises.push(
          fetchTapes(database.db, { limit: fetchLimit }).then(r => {
            media.tapes = r;
            console.log(`📺 Fetched ${r.length} tapes`);
          })
        );
      } else if (type === "clock") {
        fetchPromises.push(
          fetchClocks(database.db, { limit: fetchLimit }).then(r => {
            media.clocks = r;
            console.log(`📺 Fetched ${r.length} clocks`);
          })
        );
      } else if (["mood", "scream", "chat"].includes(type)) {
        media[`${type}s`] = [];
        pendingTypes.push(type);
      } else {
        unsupportedTypes.push(type);
      }
    }
    await Promise.all(fetchPromises);

    const counts = Object.fromEntries(
      Object.entries(media).map(([key, value]) => [key, Array.isArray(value) ? value.length : 0]),
    );

    // Interweave all media types by timestamp for mixed feed
    const allItems = [];
    if (media.kidlisp) allItems.push(...media.kidlisp);
    if (media.paintings) allItems.push(...media.paintings);
    if (media.tapes) allItems.push(...media.tapes);
    if (media.clocks) allItems.push(...media.clocks);
    
    // Apply sorting based on filter type
    if (filter === "sprinkle") {
      // Sprinkle algorithm: ensures even distribution across all media types
      // regardless of recency or popularity, while still considering frecency
      
      // Separate items by type
      const kidlispItems = allItems.filter(i => i.type === 'kidlisp');
      const paintingItems = allItems.filter(i => i.type === 'painting');
      const tapeItems = allItems.filter(i => i.type === 'tape');
      const clockItems = allItems.filter(i => i.type === 'clock');
      
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
      scoreFrecency(clockItems);
      
      // Interweave items with natural variation (not perfectly round-robin)
      // This creates a more organic mix that feels less mechanical
      const sprinkledFeed = [];
      const pools = [
        { items: paintingItems, name: 'painting' },
        { items: tapeItems, name: 'tape' },
        { items: kidlispItems, name: 'kidlisp' },
        { items: clockItems, name: 'clock' }
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
        tapes: sprinkledFeed.filter(i => i.type === 'tape').length,
        clocks: sprinkledFeed.filter(i => i.type === 'clock').length
      };
      console.log(`📺 Sprinkle feed created with natural distribution: ${allItems.length} items (${typeCounts.kidlisp} kidlisp, ${typeCounts.paintings} paintings, ${typeCounts.tapes} tapes, ${typeCounts.clocks} clocks)`);
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
    
    console.log(`📺 Mixed feed created: ${mixedFeed.length} items (${mixedFeed.filter(i => i.type === 'kidlisp').length} kidlisp, ${mixedFeed.filter(i => i.type === 'painting').length} paintings, ${mixedFeed.filter(i => i.type === 'tape').length} tapes, ${mixedFeed.filter(i => i.type === 'clock').length} clocks)`);

    // Return data (not respond) since this is called from cache wrapper
    return {
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
    };
  } catch (error) {
    console.error("Failed to fetch TV feed data", error);
    throw error; // Re-throw so cache wrapper can handle
  }
}
