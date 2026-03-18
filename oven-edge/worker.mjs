// oven-edge — Cloudflare Worker that caches OTA downloads at the edge.
// Serves at edge.aesthetic.computer, proxies from origin oven (NYC).
//
// Routes:
//   /os/latest.iso         → latest template ISO (cached 24h at edge)
//   /os/<name>.iso         → specific build ISO (cached 24h)
//   /os/<name>.vmlinuz     → specific build kernel (cached 24h)
//   /os-releases           → build list (cached 1 min)
//   /os-image              → personalized image (NEVER cached)
//   /*                     → proxy to oven (cached 5 min)

const ORIGIN = "https://oven.aesthetic.computer";
const SPACES = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com";

const CORS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
};

function edgeHeaders(request, extra = {}) {
  return {
    "Access-Control-Allow-Origin": "*",
    "X-Edge-Pop": request.cf?.colo || "unknown",
    ...extra,
  };
}

export default {
  async fetch(request) {
    const url = new URL(request.url);
    const path = url.pathname;

    // CORS preflight
    if (request.method === "OPTIONS") {
      return new Response(null, { headers: CORS });
    }

    // --- /os/latest.iso → latest template ISO from DO Spaces ---
    if (path === "/os/latest.iso" || path === "/os-template-iso") {
      const isoUrl = SPACES + "/os/native-notepat-latest.iso";
      const res = await fetch(isoUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      const out = new Response(res.body, res);
      out.headers.set("Content-Disposition", "attachment; filename=ac-os-latest.iso");
      for (const [k, v] of Object.entries(edgeHeaders(request))) out.headers.set(k, v);
      return out;
    }

    // --- /os/<name>.vmlinuz → named build kernel from DO Spaces ---
    const vmlinuzMatch = path.match(/^\/os\/([a-z]+-[a-z]+)\.vmlinuz$/);
    if (vmlinuzMatch) {
      const name = vmlinuzMatch[1];
      const vmzUrl = SPACES + "/os/builds/" + name + ".vmlinuz";
      const res = await fetch(vmzUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      if (!res.ok) return new Response("Build not found: " + name, { status: 404 });
      const out = new Response(res.body, res);
      out.headers.set("Content-Disposition", "attachment; filename=" + name + ".vmlinuz");
      for (const [k, v] of Object.entries(edgeHeaders(request))) out.headers.set(k, v);
      return out;
    }

    // --- /os/<name>.iso → named build ISO from DO Spaces ---
    const isoMatch = path.match(/^\/os\/([a-z]+-[a-z]+)\.iso$/);
    if (isoMatch) {
      const name = isoMatch[1];
      const isoUrl = SPACES + "/os/builds/" + name + ".iso";
      const res = await fetch(isoUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      if (!res.ok) return new Response("Build not found: " + name, { status: 404 });
      const out = new Response(res.body, res);
      out.headers.set("Content-Disposition", "attachment; filename=" + name + ".iso");
      for (const [k, v] of Object.entries(edgeHeaders(request))) out.headers.set(k, v);
      return out;
    }

    // --- Proxy everything else to oven origin ---
    const originUrl = ORIGIN + path + url.search;

    // Cache rules by path
    let ttl = 300; // 5 min default
    if (/\/os-releases/.test(path)) ttl = 60;
    else if (/\/os-image/.test(path)) ttl = 0; // personalized — never cache
    else if (/\/health/.test(path)) ttl = 10;

    // Rewrite the URL to use the origin hostname directly.
    // oven-origin.aesthetic.computer is DNS-only (not CF-proxied)
    // so Workers won't loop on it. The redirect: "follow" is safe here.
    const originReq = new Request(originUrl, {
      method: request.method,
      headers: request.headers,
      body: request.method === "GET" || request.method === "HEAD" ? undefined : request.body,
    });
    const response = await fetch(originReq);

    const out = new Response(response.body, response);
    for (const [k, v] of Object.entries(edgeHeaders(request, { "X-Cache-Ttl": String(ttl) }))) out.headers.set(k, v);
    return out;
  },
};
