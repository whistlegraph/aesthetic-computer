// 🖼️ Open Graph Image Proxy
// Re-serves third-party images with CORS headers so chat link previews
// can load them onto a canvas (toBitmap needs an untainted canvas).

export const config = { path: "/api/og-image" };

const MAX_BYTES = 5 * 1024 * 1024; // 5 MB
const FETCH_TIMEOUT_MS = 8000;

const corsJson = (status, body) =>
  new Response(JSON.stringify(body), {
    status,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
    },
  });

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

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);

  try {
    const upstream = await fetch(targetUrl, {
      headers: {
        "User-Agent": "Mozilla/5.0 (compatible; AestheticComputer/1.0)",
        Accept: "image/*,*/*;q=0.8",
      },
      signal: controller.signal,
      redirect: "follow",
    });
    clearTimeout(timeout);

    if (!upstream.ok) return corsJson(502, { error: `HTTP ${upstream.status}` });

    const contentType = upstream.headers.get("content-type") || "application/octet-stream";
    if (!/^image\//i.test(contentType)) {
      return corsJson(415, { error: `Not an image: ${contentType}` });
    }

    const declaredLength = parseInt(upstream.headers.get("content-length") || "0", 10);
    if (declaredLength && declaredLength > MAX_BYTES) {
      return corsJson(413, { error: "Image too large" });
    }

    const buf = await upstream.arrayBuffer();
    if (buf.byteLength > MAX_BYTES) return corsJson(413, { error: "Image too large" });

    return new Response(buf, {
      status: 200,
      headers: {
        "Content-Type": contentType,
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
