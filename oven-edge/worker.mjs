// oven-edge — Cloudflare Worker that serves AC OS images from the edge.
// Template ISOs are cached in R2 (or DO Spaces fallback). Personalized
// images are patched on-the-fly by overwriting the 32KB identity block.
//
// Routes:
//   /os/latest.iso         → latest template ISO (cached 24h at edge)
//   /os/<name>.iso         → specific build ISO (cached 24h)
//   /os/<name>.vmlinuz     → specific build kernel (cached 24h)
//   /os-releases           → build list (cached 1 min)
//   /os-image              → personalized image (streaming patch at edge)
//   /*                     → proxy to oven origin (cached 5 min)

const ORIGIN = "https://oven-origin.aesthetic.computer";
const SPACES = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com";

const IDENTITY_MARKER = "AC_IDENTITY_BLOCK_V1\n";
const IDENTITY_BLOCK_SIZE = 32768;

function edgeHeaders(request, extra = {}) {
  return {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
    "X-Edge-Pop": request.cf?.colo || "unknown",
    ...extra,
  };
}

function applyHeaders(res, request, extra = {}) {
  const out = new Response(res.body, res);
  for (const [k, v] of Object.entries(edgeHeaders(request, extra)))
    out.headers.set(k, v);
  return out;
}

// Build a 32KB identity block: marker + JSON + zero-padding
function makeIdentityBlock(config) {
  const json = JSON.stringify(config);
  const header = IDENTITY_MARKER + json;
  const encoder = new TextEncoder();
  const headerBytes = encoder.encode(header);
  const block = new Uint8Array(IDENTITY_BLOCK_SIZE);
  block.set(headerBytes);
  return block;
}

// Stream a template ISO from source, patching the identity block on-the-fly
function streamPatchedISO(templateBody, identityBlock, manifest) {
  const offset = manifest.identityBlockOffset;
  const size = IDENTITY_BLOCK_SIZE;
  let bytesSeen = 0;

  const { readable, writable } = new TransformStream({
    transform(chunk, controller) {
      const chunkStart = bytesSeen;
      const chunkEnd = bytesSeen + chunk.byteLength;
      bytesSeen = chunkEnd;

      // Fast path: chunk doesn't overlap identity block
      if (chunkEnd <= offset || chunkStart >= offset + size) {
        controller.enqueue(chunk);
        return;
      }

      // Slow path: chunk overlaps identity block — patch it
      const buf = new Uint8Array(chunk);
      const patchStart = Math.max(0, offset - chunkStart);
      const patchOffset = Math.max(0, chunkStart - offset);
      const patchLen = Math.min(size - patchOffset, buf.length - patchStart);
      buf.set(
        identityBlock.subarray(patchOffset, patchOffset + patchLen),
        patchStart,
      );
      controller.enqueue(buf);
    },
  });

  templateBody.pipeTo(writable);
  return readable;
}

// Get the latest manifest (build name, identity block offset, etc.)
async function getManifest(env) {
  // Try R2 first
  if (env?.OS_IMAGES) {
    const obj = await env.OS_IMAGES.get("latest-manifest.json");
    if (obj) return await obj.json();
  }
  // Fallback: fetch from DO Spaces
  const res = await fetch(SPACES + "/os/latest-manifest.json");
  if (res.ok) return await res.json();
  return null;
}

// Get template ISO body as a ReadableStream
async function getTemplateStream(env, buildName) {
  // Try R2 first
  if (env?.OS_IMAGES) {
    const obj = await env.OS_IMAGES.get(`builds/${buildName}/template.iso`);
    if (obj) return obj.body;
  }
  // Fallback: fetch from DO Spaces (with edge caching)
  const res = await fetch(SPACES + "/os/native-notepat-latest.iso", {
    cf: { cacheTtl: 86400, cacheEverything: true },
  });
  if (res.ok) return res.body;
  return null;
}

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    const path = url.pathname;

    // CORS preflight
    if (request.method === "OPTIONS") {
      return new Response(null, { headers: edgeHeaders(request) });
    }

    // --- /os-image → personalized ISO (streaming edge patch) ---
    if (path === "/os-image") {
      const auth = request.headers.get("Authorization") || "";
      if (!auth) {
        return new Response(
          JSON.stringify({ error: "Authorization required" }),
          { status: 401, headers: { ...edgeHeaders(request), "Content-Type": "application/json" } },
        );
      }

      // 1. Fetch user config from oven origin (tiny JSON, fast)
      const configRes = await fetch(ORIGIN + "/api/user-config" + url.search, {
        headers: { Authorization: auth },
      });
      if (!configRes.ok) {
        const body = await configRes.text();
        return new Response(body, {
          status: configRes.status,
          headers: { ...edgeHeaders(request), "Content-Type": "application/json" },
        });
      }
      const config = await configRes.json();

      // 2. Get manifest (has identity block offset)
      const manifest = await getManifest(env);
      if (!manifest || manifest.identityBlockOffset < 0) {
        // No manifest or no offset — fall through to oven origin for legacy patching
        const ovenRes = await fetch(ORIGIN + "/os-image" + url.search, {
          headers: { Authorization: auth },
        });
        return applyHeaders(ovenRes, request, { "X-Patch": "origin-fallback" });
      }

      // 3. Get template ISO stream
      const templateStream = await getTemplateStream(env, manifest.name);
      if (!templateStream) {
        // R2 + Spaces both failed — fall through to oven
        const ovenRes = await fetch(ORIGIN + "/os-image" + url.search, {
          headers: { Authorization: auth },
        });
        return applyHeaders(ovenRes, request, { "X-Patch": "origin-fallback" });
      }

      // 4. Build identity block and stream with patch
      const identityBlock = makeIdentityBlock(config);
      const patched = streamPatchedISO(templateStream, identityBlock, manifest);

      const handle = config.handle || "unknown";
      const filename = `@${handle}-os-${config.piece || "notepat"}-AC-${manifest.name}.iso`;

      return new Response(patched, {
        headers: {
          ...edgeHeaders(request),
          "Content-Type": "application/x-iso9660-image",
          "Content-Disposition": `attachment; filename="${filename}"`,
          "Content-Length": String(manifest.isoSize),
          "X-Build": manifest.name,
          "X-Patch": "edge",
        },
      });
    }

    // --- /os/latest.iso → latest template ISO from DO Spaces ---
    if (path === "/os/latest.iso" || path === "/os-template-iso") {
      const isoUrl = SPACES + "/os/native-notepat-latest.iso";
      const res = await fetch(isoUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      const out = new Response(res.body, res);
      out.headers.set(
        "Content-Disposition",
        "attachment; filename=ac-os-latest.iso",
      );
      for (const [k, v] of Object.entries(edgeHeaders(request)))
        out.headers.set(k, v);
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
      if (!res.ok)
        return new Response("Build not found: " + name, { status: 404 });
      const out = new Response(res.body, res);
      out.headers.set(
        "Content-Disposition",
        "attachment; filename=" + name + ".vmlinuz",
      );
      for (const [k, v] of Object.entries(edgeHeaders(request)))
        out.headers.set(k, v);
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
      if (!res.ok)
        return new Response("Build not found: " + name, { status: 404 });
      const out = new Response(res.body, res);
      out.headers.set(
        "Content-Disposition",
        "attachment; filename=" + name + ".iso",
      );
      for (const [k, v] of Object.entries(edgeHeaders(request)))
        out.headers.set(k, v);
      return out;
    }

    // --- Proxy everything else to oven origin ---
    const originUrl = ORIGIN + path + url.search;

    let ttl = 300;
    if (/\/os-releases/.test(path)) ttl = 60;
    else if (/\/health/.test(path)) ttl = 10;

    const originReq = new Request(originUrl, {
      method: request.method,
      headers: request.headers,
      body:
        request.method === "GET" || request.method === "HEAD"
          ? undefined
          : request.body,
    });
    const response = await fetch(originReq);

    return applyHeaders(response, request, { "X-Cache-Ttl": String(ttl) });
  },
};
