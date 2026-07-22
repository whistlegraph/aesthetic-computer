// Recover a current TikTok poster for public archive records whose durable
// CDN media is missing. The redirect is deliberately short-lived because
// TikTok's signed image URLs expire; callers keep this stable local URL.

import { readFileSync } from "node:fs";
import { join } from "node:path";

const POSTS_PATH = join(process.cwd(), "public", "whistlegraph.org", "posts.json");
const POST_ASSETS = "https://assets.aesthetic.computer/whistlegraph/index/posts";
const CACHE_TTL = 60 * 60 * 1000;
const cache = new Map();
let postCache = null;

function loadPosts() {
  if (!postCache) {
    const data = JSON.parse(readFileSync(POSTS_PATH, "utf8"));
    postCache = new Map((data.posts || []).map((post) => [String(post.id), post]));
  }
  return postCache;
}

function redirect(location, maxAge = 3600) {
  return {
    statusCode: 302,
    headers: {
      Location: location,
      "Cache-Control": `public, max-age=${maxAge}`,
      "Access-Control-Allow-Origin": "*",
    },
    body: "",
  };
}

const error = (statusCode, message) => ({
  statusCode,
  headers: { "Content-Type": "application/json", "Cache-Control": "no-store" },
  body: JSON.stringify({ message }),
});

export function createHandler({ fetchFn = fetch, loadPostsFn = loadPosts, nowFn = Date.now } = {}) {
  return async (event) => {
    if (event.httpMethod !== "GET" && event.httpMethod !== "HEAD") return error(405, "Method Not Allowed.");
    const id = String(event.queryStringParameters?.id || "");
    if (!/^\d{10,24}$/.test(id)) return error(400, "A numeric archive post ID is required.");
    const post = loadPostsFn().get(id);
    if (!post || post.platform !== "tiktok" || !post.url) return error(404, "Video thumbnail not found.");
    if (post.thumb) return redirect(post.thumb, 86400);

    const cached = cache.get(id);
    if (cached && cached.expiresAt > nowFn()) return redirect(cached.url);
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), 6000);
    try {
      const canonical = `${POST_ASSETS}/${id}.jpg`;
      const canonicalResponse = await fetchFn(canonical, { method: "HEAD", signal: controller.signal });
      if (canonicalResponse.ok) {
        cache.set(id, { url: canonical, expiresAt: nowFn() + CACHE_TTL });
        return redirect(canonical, 86400);
      }
      const response = await fetchFn(`https://www.tiktok.com/oembed?url=${encodeURIComponent(post.url)}`, {
        headers: { Accept: "application/json" },
        signal: controller.signal,
      });
      if (!response.ok) throw new Error(`TikTok oEmbed returned ${response.status}`);
      const data = await response.json();
      const url = new URL(data.thumbnail_url);
      if (url.protocol !== "https:" || !/(^|\.)tiktokcdn(?:-us)?\.com$/i.test(url.hostname)) {
        throw new Error("TikTok returned an unexpected thumbnail host");
      }
      cache.set(id, { url: url.href, expiresAt: nowFn() + CACHE_TTL });
      return redirect(url.href);
    } catch (cause) {
      console.warn("Whistlegraph thumbnail recovery failed:", id, cause?.message || cause);
      return error(404, "Video thumbnail is unavailable.");
    } finally {
      clearTimeout(timer);
    }
  };
}

export const handler = createHandler();
