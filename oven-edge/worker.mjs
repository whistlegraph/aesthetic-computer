// oven-edge — Cloudflare Worker that serves AC OS images from the edge.
// Template disk images are cached in R2 (or DO Spaces fallback). Personalized
// images are patched on-the-fly by overwriting the identity block and
// legacy config placeholder.
//
// Routes:
//   /os/latest.img         → latest template image (cached 24h at edge)
//   /os/<name>.img         → specific build image (cached 24h)
//   /os/<name>.vmlinuz     → specific build kernel (cached 24h)
//   /os-releases           → build list (cached 1 min)
//   /os-image              → personalized image (streaming patch at edge)
//   /*                     → proxy to oven origin (cached 5 min)

const ORIGIN = "https://oven-origin.aesthetic.computer";
const SPACES = "https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com";

const IDENTITY_MARKER = "AC_IDENTITY_BLOCK_V1\n";
const IDENTITY_BLOCK_SIZE = 32768;
const CONFIG_MARKER = '{"handle":"","piece":"notepat","sub":"","email":""}';
const DEFAULT_CONFIG_PATCH_SIZE = 4096;

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

function makeLegacyConfigBlock(config, size = DEFAULT_CONFIG_PATCH_SIZE) {
  const json = JSON.stringify(config);
  const encoder = new TextEncoder();
  const jsonBytes = encoder.encode(json);
  const block = new Uint8Array(size);
  block.fill(0x20);
  block.set(jsonBytes.subarray(0, size));
  return block;
}

// Stream a template image from source, patching configured ranges on-the-fly.
function streamPatchedImage(templateBody, patches) {
  let bytesSeen = 0;

  const { readable, writable } = new TransformStream({
    transform(chunk, controller) {
      const chunkStart = bytesSeen;
      const chunkEnd = bytesSeen + chunk.byteLength;
      bytesSeen = chunkEnd;

      let buf = null;
      for (const patch of patches) {
        const offset = patch.offset;
        const size = patch.bytes.byteLength;
        if (offset < 0 || chunkEnd <= offset || chunkStart >= offset + size) {
          continue;
        }
        if (!buf) buf = new Uint8Array(chunk);
        const patchStart = Math.max(0, offset - chunkStart);
        const patchOffset = Math.max(0, chunkStart - offset);
        const patchLen = Math.min(size - patchOffset, buf.length - patchStart);
        buf.set(
          patch.bytes.subarray(patchOffset, patchOffset + patchLen),
          patchStart,
        );
      }

      if (!buf) {
        controller.enqueue(chunk);
        return;
      }
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

// Get template image body as a ReadableStream
async function getTemplateStream(env, manifest) {
  const buildName = manifest?.name;
  if (!buildName) return null;

  // Try R2 first
  if (env?.OS_IMAGES) {
    const obj = await env.OS_IMAGES.get(`builds/${buildName}/template.img`);
    if (obj) {
      const size = Number(obj.size || 0);
      if (!manifest?.imageSize || !size || size === manifest.imageSize) {
        return { body: obj.body, size: size || manifest?.imageSize || 0 };
      }
    }
  }

  // Fallback: fetch from DO Spaces (with edge caching)
  const res = await fetch(SPACES + "/os/native-notepat-latest.img", {
    cf: { cacheTtl: 86400, cacheEverything: true },
  });
  if (res.ok) {
    const size = Number(res.headers.get("content-length") || "0");
    if (!manifest?.imageSize || !size || size === manifest.imageSize) {
      return { body: res.body, size: size || manifest?.imageSize || 0 };
    }
  }

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

    // --- /os-image → personalized image (streaming edge patch) ---
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

      // 2. Get manifest (has patch offsets)
      const manifest = await getManifest(env);
      const hasIdentity = Number.isFinite(manifest?.identityBlockOffset) && manifest.identityBlockOffset >= 0;
      const configOffsets = Array.isArray(manifest?.configOffsets) ? manifest.configOffsets : [];
      if (!manifest || (!hasIdentity && configOffsets.length === 0)) {
        // No manifest or no offsets — fall through to oven origin for legacy patching
        const ovenRes = await fetch(ORIGIN + "/os-image" + url.search, {
          headers: { Authorization: auth },
        });
        return applyHeaders(ovenRes, request, { "X-Patch": "origin-fallback" });
      }

      // 3. Get template ISO stream
      const template = await getTemplateStream(env, manifest);
      if (!template) {
        // R2 + Spaces both failed — fall through to oven
        const ovenRes = await fetch(ORIGIN + "/os-image" + url.search, {
          headers: { Authorization: auth },
        });
        return applyHeaders(ovenRes, request, { "X-Patch": "origin-fallback" });
      }

      // 4. Build patch payloads and stream with patch
      const patches = [];
      if (hasIdentity) {
        patches.push({
          offset: manifest.identityBlockOffset,
          bytes: makeIdentityBlock(config),
        });
      }
      const configBlock = makeLegacyConfigBlock(
        config,
        Number(manifest.configPatchSize || DEFAULT_CONFIG_PATCH_SIZE),
      );
      for (const offset of configOffsets) {
        if (Number.isFinite(offset) && offset >= 0) {
          patches.push({ offset, bytes: configBlock });
        }
      }
      patches.sort((a, b) => a.offset - b.offset);
      const patched = streamPatchedImage(template.body, patches);

      const handle = config.handle || "unknown";
      const filename = `@${handle}-os-${config.piece || "notepat"}-AC-${manifest.name}.img`;
      const requestedLayout = (url.searchParams.get("layout") || "img").toLowerCase();

      return new Response(patched, {
        headers: {
          ...edgeHeaders(request),
          "Content-Type": "application/octet-stream",
          "Content-Disposition": `attachment; filename="${filename}"`,
          "Content-Length": String(template.size || manifest.imageSize),
          "X-AC-OS-Requested-Layout": requestedLayout,
          "X-AC-OS-Layout": "img",
          "X-Build": manifest.name,
          "X-Patch": "edge",
        },
      });
    }

    // --- /os/latest.img → latest template image from DO Spaces ---
    if (path === "/os/latest.img" || path === "/os-template-img") {
      const imgUrl = SPACES + "/os/native-notepat-latest.img";
      const res = await fetch(imgUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      const out = new Response(res.body, res);
      out.headers.set(
        "Content-Disposition",
        "attachment; filename=ac-os-latest.img",
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

    // --- /os/<name>.img → named build image from DO Spaces ---
    const imgMatch = path.match(/^\/os\/([a-z]+-[a-z]+)\.img$/);
    if (imgMatch) {
      const name = imgMatch[1];
      const imgUrl = SPACES + "/os/builds/" + name + ".img";
      const res = await fetch(imgUrl, {
        cf: { cacheTtl: 86400, cacheEverything: true },
      });
      if (!res.ok)
        return new Response("Build not found: " + name, { status: 404 });
      const out = new Response(res.body, res);
      out.headers.set(
        "Content-Disposition",
        "attachment; filename=" + name + ".img",
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
