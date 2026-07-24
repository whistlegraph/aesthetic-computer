// 🎠 whistlegraph.org per-work and per-post link previews (Open Graph)
//
// The whistlegraph.org index is a static SPA: Caddy serves one index.html for
// every /<code> deep link, so link-unfurlers (iMessage, Slack, Twitter, …) all
// see the SAME generic card. This function fills that gap — Caddy proxies bare
// code and /post/<id> paths here, and we return index.html with that record's
// own og:title, og:image (its thumbnail), and og:video (the take's mp4)
// injected. Humans still get the full app; only the <head> is rewritten.
//
// Wired in lith/Caddyfile under @whistlegraph:
//   @wgcode path_regexp wgcode ^/([A-Za-z0-9]+)$
//   handle @wgcode { rewrite * /api/whistlegraph-og?code={re.wgcode.1}
//                    reverse_proxy localhost:8888 }

import { readFileSync, statSync } from "fs";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const DIR = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "public", "whistlegraph.org");
const INDEX = join(DIR, "index.html");
const GRAPHS = join(DIR, "graphs.json");
const POSTS = join(DIR, "posts.json");
const DEFAULT_IMG =
  "https://assets.aesthetic.computer/whistlegraph/butterfly-cosplayer/butterfly-cosplayer.webp";

// Cache index.html + the code→work map, refreshed when either file's mtime moves
// (a curation deploy rewrites graphs.json, so previews track it without a restart).
let cache = {
  indexMtime: 0,
  graphsMtime: 0,
  postsMtime: 0,
  html: "",
  byCode: null,
  byPost: null,
  aliases: null,
};

function load() {
  const im = statSync(INDEX).mtimeMs;
  const gm = statSync(GRAPHS).mtimeMs;
  const pm = statSync(POSTS).mtimeMs;
  if (im !== cache.indexMtime || gm !== cache.graphsMtime || pm !== cache.postsMtime || !cache.byCode || !cache.byPost) {
    const html = readFileSync(INDEX, "utf-8");
    const data = JSON.parse(readFileSync(GRAPHS, "utf-8"));
    const postData = JSON.parse(readFileSync(POSTS, "utf-8"));
    const works = Array.isArray(data) ? data : data.graphs || [];
    const posts = Array.isArray(postData) ? postData : postData.posts || [];
    const byCode = new Map(works.map((w) => [w.code, w]));
    const byPost = new Map(posts.map((post) => [String(post.id), post]));
    cache = {
      indexMtime: im,
      graphsMtime: gm,
      postsMtime: pm,
      html,
      byCode,
      byPost,
      aliases: data.aliases || {},
    };
  }
  return cache;
}

const esc = (s) =>
  String(s || "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));

// A thumbnail is …/posts/<id>.jpg; the matching take is the same path as .mp4.
const videoFor = (thumb) =>
  typeof thumb === "string" && /\/posts\/\d+\.jpg$/.test(thumb) ? thumb.replace(/\.jpg$/, ".mp4") : null;

export const handler = async (event) => {
  const code = (event.queryStringParameters?.code || "").trim();
  const postId = (event.queryStringParameters?.id || "").trim();
  let store;
  try {
    store = load();
  } catch (err) {
    // If anything goes wrong reading the files, fail open with a redirect to the
    // static index so the page still loads for humans.
    return { statusCode: 302, headers: { location: "/index.html" }, body: "" };
  }
  let html = store.html;
  const alias = store.aliases[code];
  if (alias && store.byCode.has(alias)) {
    return { statusCode: 302, headers: { location: `/${encodeURIComponent(alias)}` }, body: "" };
  }
  const work = store.byCode.get(code);
  const post = /^\d+$/.test(postId) ? store.byPost.get(postId) : null;

  if (work) {
    const title = work.title || "Whistlegraph";
    const by = work.by && work.by !== "Whistlegraph" ? ` by ${work.by}` : "";
    const yr = work.year ? ` (${work.year})` : "";
    const desc = `${title} — a whistlegraph${by}${yr}. A drawing you sing; the score teaches you how to play it.`;
    const img = work.thumb || DEFAULT_IMG;
    const vid = videoFor(work.thumb);
    const url = `https://whistlegraph.org/${code}`;

    html = html
      .replace(/<title>[^<]*<\/title>/, `<title>${esc(title)} — Whistlegraph</title>`)
      .replace(
        /<meta property="og:title"[^>]*>/,
        `<meta property="og:title" content="${esc(title)}">`,
      )
      .replace(
        /<meta property="og:description"[^>]*>/,
        `<meta property="og:description" content="${esc(desc)}">`,
      )
      .replace(
        /<meta property="og:image"[^>]*>/,
        [
          `<meta property="og:image" content="${esc(img)}">`,
          `<meta property="og:url" content="${esc(url)}">`,
          `<meta property="og:type" content="video.other">`,
          `<meta name="twitter:card" content="${vid ? "player" : "summary_large_image"}">`,
          `<meta name="twitter:title" content="${esc(title)}">`,
          `<meta name="twitter:image" content="${esc(img)}">`,
          ...(vid
            ? [
                `<meta property="og:video" content="${esc(vid)}">`,
                `<meta property="og:video:secure_url" content="${esc(vid)}">`,
                `<meta property="og:video:type" content="video/mp4">`,
              ]
            : []),
        ].join("\n"),
      );
  } else if (post) {
    const rawTitle = String(post.desc || "").trim();
    const title = rawTitle ? (rawTitle.length > 120 ? `${rawTitle.slice(0, 117)}…` : rawTitle) : `Archive post ${postId}`;
    const desc = [
      post.date ? `Whistlegraph archive post from ${post.date}.` : "Whistlegraph archive post.",
      post.views != null ? `${Number(post.views).toLocaleString("en-US")} views.` : "",
    ].filter(Boolean).join(" ");
    const img = post.thumb || DEFAULT_IMG;
    const vid = post.media === "audio" ? null : (post.src || videoFor(post.thumb));
    const url = `https://whistlegraph.org/post/${postId}`;

    html = html
      .replace(/<title>[^<]*<\/title>/, `<title>${esc(title)} — Whistlegraph Archive</title>`)
      .replace(/<meta property="og:title"[^>]*>/, `<meta property="og:title" content="${esc(title)}">`)
      .replace(/<meta property="og:description"[^>]*>/, `<meta property="og:description" content="${esc(desc)}">`)
      .replace(
        /<meta property="og:image"[^>]*>/,
        [
          `<meta property="og:image" content="${esc(img)}">`,
          `<meta property="og:url" content="${esc(url)}">`,
          `<meta property="og:type" content="${vid ? "video.other" : "article"}">`,
          `<meta name="twitter:card" content="${vid ? "player" : "summary_large_image"}">`,
          `<meta name="twitter:title" content="${esc(title)}">`,
          `<meta name="twitter:image" content="${esc(img)}">`,
          ...(vid
            ? [
                `<meta property="og:video" content="${esc(vid)}">`,
                `<meta property="og:video:secure_url" content="${esc(vid)}">`,
                `<meta property="og:video:type" content="video/mp4">`,
              ]
            : []),
        ].join("\n"),
      );
  }

  return {
    statusCode: 200,
    headers: {
      "content-type": "text/html; charset=utf-8",
      // Short cache: curation edits should surface in unfurlers within minutes.
      "cache-control": "public, max-age=300",
    },
    body: html,
  };
};
