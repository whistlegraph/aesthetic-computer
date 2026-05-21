// pop-demos.mjs — serves the /pop demo page and its ES-module tree at
// the public `/pop-demos/` route on lith.
//
// pop/demos.html imports modules from pop/lib, pop/demos and
// pop/dance/synths. Those all live under pop/ in the repo (deployed to
// lith), so this maps /pop-demos/<rel> → <repo>/pop/<rel> for text
// assets only. A bare /pop-demos redirects to /pop-demos/ so the page's
// relative module imports resolve correctly.

import { readFile } from "node:fs/promises";
import { dirname, resolve, extname } from "node:path";
import { fileURLToPath } from "node:url";

// system/netlify/functions/ → repo root → pop/
const POP = resolve(dirname(fileURLToPath(import.meta.url)), "../../../pop");

const TYPES = {
  ".html": "text/html; charset=utf-8",
  ".mjs":  "text/javascript; charset=utf-8",
  ".js":   "text/javascript; charset=utf-8",
  ".css":  "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
};

export const handler = async (event) => {
  let rel = (event.path || "/pop-demos").replace(/^\/pop-demos/, "");

  // bare /pop-demos → /pop-demos/ so `./lib/...` imports resolve
  if (rel === "") return { statusCode: 302, headers: { Location: "/pop-demos/" }, body: "" };
  if (rel === "/") rel = "/demos.html";

  const type = TYPES[extname(rel)];
  if (!type) return { statusCode: 404, body: "not found" };

  // resolve under pop/ and refuse anything that escapes it
  const file = resolve(POP, "." + rel);
  if (file !== POP && !file.startsWith(POP + "/"))
    return { statusCode: 403, body: "forbidden" };

  try {
    const body = await readFile(file, "utf8");
    return {
      statusCode: 200,
      headers: { "content-type": type, "cache-control": "public, max-age=300" },
      body,
    };
  } catch {
    return { statusCode: 404, body: "not found: " + rel };
  }
};
