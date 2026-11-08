// 🔊 Linked-Audio Proxy
// Re-serves a sound file someone posted in chat with CORS headers so the
// speaker chip can play it. BIOS loads a sound through an <audio
// crossOrigin="anonymous"> and a fetch-and-decode, and a personal server that
// sends no Access-Control-Allow-Origin fails both, silently. Sibling of
// og-image, which does the same for link-card thumbnails.

export const config = { path: "/api/og-audio" };

const MAX_BYTES = 24 * 1024 * 1024; // A long track, not an album.
const FETCH_TIMEOUT_MS = 45000; // Personal servers stream a track slowly; the abort covers the whole body.
const UNAVAILABLE_TTL = 5 * 60 * 1000;
const unavailable = new Map();

// The same extensions chat's VOX_LINK offers a chip for — this is a sound
// proxy, not a general fetcher.
const AUDIO_EXT = /\.(mp3|wav|ogg|m4a|webm)([?#]|$)/i;
const TYPE_BY_EXT = { mp3: "audio/mpeg", wav: "audio/wav", ogg: "audio/ogg", m4a: "audio/mp4", webm: "audio/webm" };

const corsJson = (status, body, headers = {}) =>
  new Response(JSON.stringify(body), {
    status,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      ...headers,
    },
  });

function unavailableAudio(entry) {
  return corsJson(404, { error: "Audio unavailable", upstreamStatus: entry.status }, {
    "Cache-Control": `public, max-age=${Math.max(0, Math.floor((entry.until - Date.now()) / 1000))}`,
  });
}

export default async function handler(req) {
  if (req.method === "OPTIONS") {
    return new Response(null, {
      status: 204,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "GET, OPTIONS",
        "Access-Control-Allow-Headers": "Content-Type",
      },
    });
  }

  if (req.method !== "GET") return corsJson(405, { error: "Method not allowed" });

  const targetUrl = new URL(req.url).searchParams.get("url");
  if (!targetUrl) return corsJson(400, { error: "Missing 'url' parameter" });

  let parsed;
  try {
    parsed = new URL(targetUrl);
    if (!["http:", "https:"].includes(parsed.protocol)) throw new Error("bad protocol");
  } catch {
    return corsJson(400, { error: "Invalid URL" });
  }
  const ext = parsed.pathname.match(AUDIO_EXT)?.[1]?.toLowerCase();
  if (!ext) return corsJson(415, { error: "Not a sound file" });

  const cached = unavailable.get(targetUrl);
  if (cached?.until > Date.now()) return unavailableAudio(cached);
  unavailable.delete(targetUrl);

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);

  try {
    const upstream = await fetch(targetUrl, {
      headers: {
        "User-Agent": "Mozilla/5.0 (compatible; AestheticComputer/1.0)",
        Accept: "audio/*,*/*;q=0.8",
      },
      signal: controller.signal,
      redirect: "follow",
    });
    clearTimeout(timeout);

    if (!upstream.ok) {
      await upstream.body?.cancel().catch(() => {});
      // A missing or denied remote file is an unavailable chip, not an
      // origin outage. Same short retry window as og-image.
      if ([401, 403, 404, 410].includes(upstream.status)) {
        const entry = { status: upstream.status, until: Date.now() + UNAVAILABLE_TTL };
        if (unavailable.size >= 1000) unavailable.delete(unavailable.keys().next().value);
        unavailable.set(targetUrl, entry);
        return unavailableAudio(entry);
      }
      return corsJson(502, { error: `HTTP ${upstream.status}` });
    }

    // Personal servers often label a sound application/octet-stream; the
    // extension already told us what it is.
    let contentType = (upstream.headers.get("content-type") || "").split(";")[0].trim().toLowerCase();
    if (!/^(audio\/|video\/webm)/.test(contentType)) {
      if (contentType && contentType !== "application/octet-stream" && contentType !== "binary/octet-stream") {
        await upstream.body?.cancel().catch(() => {});
        return corsJson(415, { error: `Not audio: ${contentType}` });
      }
      contentType = TYPE_BY_EXT[ext];
    }

    const declaredLength = parseInt(upstream.headers.get("content-length") || "0", 10);
    if (declaredLength && declaredLength > MAX_BYTES) {
      await upstream.body?.cancel().catch(() => {});
      return corsJson(413, { error: "Audio too large" });
    }

    const buf = await upstream.arrayBuffer();
    if (buf.byteLength > MAX_BYTES) return corsJson(413, { error: "Audio too large" });

    return new Response(buf, {
      status: 200,
      headers: {
        "Content-Type": contentType,
        "Content-Length": String(buf.byteLength),
        "Access-Control-Allow-Origin": "*",
        "Cache-Control": "public, max-age=86400, immutable",
      },
    });
  } catch (err) {
    clearTimeout(timeout);
    const msg = err.name === "AbortError" ? "Request timed out" : err.message;
    return corsJson(500, { error: msg });
  }
}
