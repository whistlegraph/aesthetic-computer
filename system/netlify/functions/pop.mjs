// pop.mjs — serves the /pop dashboard (pop/demos.html) and everything it
// needs at the public `/pop/` route on lith.
//
// The dashboard imports ES modules from pop/lib, pop/demos and
// pop/dance/synths, and plays audio from pop/demos/samples and the lane
// out/ directories. This maps /pop/<rel> → <repo>/pop/<rel>; text assets
// are served utf-8, audio/images base64. A bare /pop redirects to /pop/
// so the page's relative module imports resolve.

import { readFile } from "node:fs/promises";
import { dirname, resolve, extname } from "node:path";
import { fileURLToPath } from "node:url";

// system/netlify/functions/ → repo root → pop/
const POP = resolve(dirname(fileURLToPath(import.meta.url)), "../../../pop");

const TEXT = {
  ".html": "text/html; charset=utf-8",
  ".mjs":  "text/javascript; charset=utf-8",
  ".js":   "text/javascript; charset=utf-8",
  ".css":  "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".np":   "text/plain; charset=utf-8",
  ".txt":  "text/plain; charset=utf-8",
  ".md":   "text/plain; charset=utf-8",
};
const BIN = {
  ".mp3": "audio/mpeg",
  ".wav": "audio/wav",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".png": "image/png",
};

export const handler = async (event) => {
  let rel = (event.path || "/pop").replace(/^\/pop/, "");

  // bare /pop → /pop/ so `./lib/...` imports resolve
  if (rel === "") return { statusCode: 302, headers: { Location: "/pop/" }, body: "" };
  if (rel === "/") rel = "/demos.html";

  const ext = extname(rel).toLowerCase();
  const text = TEXT[ext], bin = BIN[ext];
  if (!text && !bin) return { statusCode: 404, body: "not found" };

  // resolve under pop/ and refuse anything that escapes it
  const file = resolve(POP, "." + rel);
  if (file !== POP && !file.startsWith(POP + "/"))
    return { statusCode: 403, body: "forbidden" };

  try {
    if (text) {
      const body = await readFile(file, "utf8");
      return {
        statusCode: 200,
        headers: { "content-type": text, "cache-control": "public, max-age=120" },
        body,
      };
    }
    const buf = await readFile(file);
    return {
      statusCode: 200,
      headers: { "content-type": bin, "cache-control": "public, max-age=3600" },
      body: buf.toString("base64"),
      isBase64Encoded: true,
    };
  } catch {
    return { statusCode: 404, body: "not found: " + rel };
  }
};
