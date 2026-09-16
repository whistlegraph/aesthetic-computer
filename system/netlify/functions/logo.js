// Logo, 23.05.02.22.09 · pals.aesthetic.computer
// Serve a random pal from one endpoint — 50/50 a still PNG or a looping
// animated turnaround (WebP / APNG / MP4), a fresh random one every hit.
// Loaded in a browser it returns a tappable HTML page that cycles pals.
//
//   /                          HTML page (50/50 still or animated, click to cycle)
//   /logo.png                  random still, PNG bytes (proxied)
//   /random.webp  /random.apng random animated turnaround, bytes (proxied)
//   /random.mp4   /random.json redirect to a random turnaround / describe a random pal
//   /pals-<slug>.png           a named still            (302 → CDN)
//   /pals-<slug>.webp|apng|mp4 a named turnaround       (302 → CDN)
//   /turnaround.* /auth0.*     aliases of random.* (kept for existing embeds)
//   /pals.json                 the whole catalogue (stills + turnarounds)
// Non-browser clients (curl, wget, <img>) hitting / get 50/50 still-or-webp bytes.

import { respond } from "../../backend/http.mjs";
import {
  logoUrl, turnaroundUrl, randomPal, stillSlugs, turnaroundSlugs, turnaroundFormats,
} from "../../backend/logo.mjs";

const CORS = { "Access-Control-Allow-Origin": "*" };
const NO_CACHE = { "Cache-Control": "no-store" };
const MIME = { png: "image/png", webp: "image/webp", apng: "image/apng", mp4: "video/mp4" };

const redirect = (url) => ({
  statusCode: 302,
  headers: { Location: url, ...CORS, ...NO_CACHE },
  body: "",
});

async function proxy(url, format) {
  const { got } = await import("got");
  const response = await got(url, { responseType: "buffer", https: { rejectUnauthorized: false } });
  return {
    statusCode: 200,
    headers: { "Content-Type": MIME[format] || "application/octet-stream", ...CORS, ...NO_CACHE },
    body: Buffer.from(response.body, "binary").toString("base64"),
    isBase64Encoded: true,
  };
}

export async function handler(event, context) {
  if (event.httpMethod !== "GET") return respond(405, { error: "Wrong request type." });

  const assetPath = event.path.replace(/^\/api\/logo\/?/, "").replace(/^\//, "");
  const query = new URLSearchParams(event.queryStringParameters || {});
  const previous = query.get("previousLogo");

  // ── catalogue ───────────────────────────────────────────────────────
  if (assetPath === "pals.json") {
    return {
      statusCode: 200,
      headers: { "Content-Type": "application/json", ...CORS, "Cache-Control": "public, max-age=300" },
      body: JSON.stringify({
        stills: stillSlugs.map((slug) => ({ slug, png: logoUrl(slug) })),
        turnarounds: turnaroundSlugs.map((slug) => Object.fromEntries([
          ["slug", slug], ...turnaroundFormats.map((f) => [f, turnaroundUrl(slug, f)]),
        ])),
      }),
    };
  }

  // ── named assets → CDN ──────────────────────────────────────────────
  const named = assetPath.match(/^pals-([a-z0-9-]+)\.(png|webp|apng|mp4)$/);
  if (named) {
    const [, slug, format] = named;
    const url = format === "png" ? logoUrl(slug) : turnaroundUrl(slug, format);
    return url ? redirect(url) : respond(404, { error: "Pal not found." });
  }

  // ── random animated (turnaround / auth0 are legacy aliases) ─────────
  const random = assetPath.match(/^(?:random|turnaround|auth0)\.(webp|apng|mp4|json)$/);
  if (random) {
    const format = random[1];
    if (format === "json") {
      const pal = randomPal({ previous });
      return { statusCode: 200, headers: { "Content-Type": "application/json", ...CORS, ...NO_CACHE }, body: JSON.stringify(pal) };
    }
    const url = turnaroundUrl(null, format);
    if (!url) return respond(404, { error: "No turnarounds published." });
    return format === "mp4" ? redirect(url) : proxy(url, format);
  }

  // ── random still bytes (legacy /logo.png) ───────────────────────────
  if (assetPath === "logo.png" || assetPath === "random.png" || assetPath === "still.png") {
    return proxy(randomPal({ previous, animated: false }).url, "png");
  }

  // ── bare endpoint: 50/50 still or animated, fresh each hit ──────────
  const pal = randomPal({ previous });
  const userAgent = event.headers["user-agent"] || "";
  const accept = event.headers["accept"] || "";
  const isServer = /curl|wget|python-requests|node-fetch/i.test(userAgent);
  const wantsImage = accept.startsWith("image/") || (accept.includes("image/*") && !accept.includes("text/html"));

  if (isServer || wantsImage) return proxy(pal.url, pal.format);

  const htmlResponse = `<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>pals · ${pal.slug}${pal.animated ? " ↻" : ""}</title>
    <link rel="icon" href="${logoUrl(pal.slug) || pal.url}" type="image/png">
    <style>
      html { height: 100%; }
      body {
        display: flex; justify-content: center; align-items: center;
        height: 100%; margin: 0; overflow: hidden;
        background-color: ${randomPurple()};
        transition: filter 0.2s;
      }
      body.blurred { filter: blur(5px); }
      img { object-fit: contain; width: 100vw; height: 100%; cursor: pointer; }
    </style>
  </head>
  <body>
    <img crossorigin src="${pal.url}" alt="pals ${pal.slug}" onclick="next()">
    <script>
      const strippedUrl = window.location.origin + window.location.pathname;
      window.history.replaceState({}, document.title, strippedUrl);
      function next() {
        document.body.classList.add("blurred");
        const newURL = strippedUrl + "?previousLogo=" + encodeURIComponent(${JSON.stringify(pal.url)});
        setTimeout(() => { window.location = newURL; }, 200);
      }
    </script>
  </body>
</html>
`;
  return { statusCode: 200, headers: { "Content-Type": "text/html", ...NO_CACHE }, body: htmlResponse };
}

const randomPurple = () => {
  const r = Math.floor(Math.random() * 50 + 50);
  const g = Math.floor(Math.random() * 25);
  const b = Math.floor(Math.random() * 50 + 50);
  return `rgb(${r},${g},${b})`;
};
