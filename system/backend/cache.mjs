// Cache, 2026.01.04
// Redis-backed caching for expensive database operations.
// Uses TTL-based expiration to reduce load on MongoDB.

import { createClient } from "redis";

const redisConnectionString = process.env.REDIS_CONNECTION_STRING;
const dev = process.env.NETLIFY_DEV;

let client;

async function connect() {
  if (client && client.isOpen) {
    return client;
  }
  client = !dev ? createClient({ url: redisConnectionString }) : createClient();
  client.on("error", (err) => console.log("🔴 Cache Redis error:", err));
  await client.connect();
  return client;
}

async function disconnect() {
  if (!client?.isOpen) return;
  await client.quit();
}

/**
 * Get a cached value, or compute and cache it if missing/expired.
 * 
 * @param {string} key - Cache key (e.g., "metrics:handles")
 * @param {function} computeFn - Async function to compute value if cache miss
 * @param {number} ttlSeconds - Time to live in seconds (default: 30 minutes)
 * @returns {Promise<any>} - The cached or computed value
 */
async function getOrCompute(key, computeFn, ttlSeconds = 1800) {
  // Try reading from Redis cache first
  let cacheAvailable = false;
  try {
    await connect();
    const cached = await client.get(key);
    if (cached) {
      console.log(`📦 Cache HIT: ${key}`);
      return JSON.parse(cached);
    }
    console.log(`📭 Cache MISS: ${key}`);
    cacheAvailable = true;
  } catch (err) {
    console.error(`⚠️ Cache read error for ${key}:`, err.message);
  }

  // Compute the value (let errors propagate to caller)
  const value = await computeFn();

  // Try to store in cache (non-blocking, don't fail if Redis is down)
  if (cacheAvailable) {
    try {
      await client.setEx(key, ttlSeconds, JSON.stringify(value));
    } catch (err) {
      console.error(`⚠️ Cache write error for ${key}:`, err.message);
    }
  }

  return value;
}

/**
 * Invalidate a cache key or pattern.
 * 
 * @param {string} key - Exact key or pattern with * wildcard
 */
async function invalidate(key) {
  try {
    await connect();
    
    if (key.includes('*')) {
      // Pattern-based deletion
      const keys = await client.keys(key);
      if (keys.length > 0) {
        await client.del(keys);
        console.log(`🗑️ Cache invalidated ${keys.length} keys matching: ${key}`);
      }
    } else {
      await client.del(key);
      console.log(`🗑️ Cache invalidated: ${key}`);
    }
  } catch (err) {
    console.error(`⚠️ Cache invalidate error:`, err.message);
  }
}

/**
 * Get cache stats for debugging.
 */
async function stats() {
  try {
    await connect();
    const info = await client.info('memory');
    const keys = await client.dbSize();
    return { keys, memoryInfo: info };
  } catch (err) {
    return { error: err.message };
  }
}

// Pre-defined cache keys and TTLs
const CACHE_KEYS = {
  METRICS: 'give:metrics',           // 30 min - platform stats
  KIDLISP_COUNT: 'give:kidlisp',     // 30 min - kidlisp program count
  TV_RECENT: 'give:tv:recent',       // 5 min - recent tapes
  CHAT_CLOCK: 'give:chat:clock',     // 2 min - clock chat messages  
  CHAT_SYSTEM: 'give:chat:system',   // 2 min - system chat messages
  SHOP: 'give:shop',                 // 10 min - shop items
};

const CACHE_TTLS = {
  METRICS: 30 * 60,      // 30 minutes
  KIDLISP_COUNT: 30 * 60, // 30 minutes
  TV_RECENT: 5 * 60,     // 5 minutes
  CHAT: 2 * 60,          // 2 minutes
  SHOP: 10 * 60,         // 10 minutes
};

export { 
  connect, 
  disconnect, 
  getOrCompute, 
  invalidate, 
  stats,
  CACHE_KEYS,
  CACHE_TTLS 
};
