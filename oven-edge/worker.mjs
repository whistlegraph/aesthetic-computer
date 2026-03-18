// oven-edge — Cloudflare Worker that caches OTA downloads at the edge.
// Proxies from the origin oven (NYC) and caches at the nearest Cloudflare POP.
//
// First request: hits origin
// Subsequent requests from same region: served from edge cache

const ORIGIN = "https://oven.aesthetic.computer";

// Cache TTL by path pattern (seconds)
const CACHE_RULES = [
  { match: /\/os-releases/, ttl: 60 },         // 1 min
  { match: /\/os-image/, ttl: 0 },             // NEVER cache (personalized per user)
  { match: /\/os-template-iso/, ttl: 86400 },  // 24 hours (generic, no creds)
  { match: /\/health/, ttl: 10 },               // 10 sec
  { match: /.*/, ttl: 300 },                    // 5 min default
];

function getCacheTtl(path) {
  for (const rule of CACHE_RULES) {
    if (rule.match.test(path)) return rule.ttl;
  }
  return 300;
}

const CORS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
};

export default {
  async fetch(request) {
    const url = new URL(request.url);

    // CORS preflight
    if (request.method === "OPTIONS") {
      return new Response(null, { headers: CORS });
    }

    // Template ISO — proxy from DO Spaces (cached at edge for 24h)
    if (url.pathname === "/os-template-iso") {
      const isoUrl = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/native-notepat-latest.iso";
      const isoRes = await fetch(isoUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      const out = new Response(isoRes.body, isoRes);
      out.headers.set("Access-Control-Allow-Origin", "*");
      out.headers.set("X-Edge-Pop", request.cf?.colo || "unknown");
      return out;
    }

    const originUrl = ORIGIN + url.pathname + url.search;
    const ttl = getCacheTtl(url.pathname);

    // Proxy to origin with Cloudflare edge caching
    const response = await fetch(originUrl, {
      method: request.method,
      headers: request.headers,
      body: request.method === "GET" || request.method === "HEAD" ? undefined : request.body,
      cf: {
        cacheTtl: request.method === "GET" ? ttl : 0,
        cacheEverything: request.method === "GET",
      },
    });

    // Clone and add debug + CORS headers
    const out = new Response(response.body, response);
    out.headers.set("Access-Control-Allow-Origin", "*");
    out.headers.set("X-Edge-Pop", request.cf?.colo || "unknown");
    out.headers.set("X-Cache-Ttl", String(ttl));
    return out;
  },
};
