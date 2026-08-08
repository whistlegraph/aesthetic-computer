#!/usr/bin/env node
// Serve xbox/live over HTTP so the working tree can be played in a browser
// with mouse and keyboard, without pushing to lith or the console. Same file
// set oskiewar.com serves, read from disk on every request so a save is a
// reload rather than a deploy.

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const live = resolve(fileURLToPath(new URL(".", import.meta.url)), "../live");
const repo = resolve(live, "../..");
const port = Number(process.argv[2]) || 8123;

const mime = new Map([
  [".html", "text/html; charset=utf-8"],
  [".js", "text/javascript; charset=utf-8"],
  [".mjs", "text/javascript; charset=utf-8"],
  [".json", "application/json"], [".svg", "image/svg+xml"],
  [".ttf", "font/ttf"], [".otf", "font/otf"],
  [".png", "image/png"], [".jpg", "image/jpeg"], [".mp4", "video/mp4"],
]);

// The shell reaches outside xbox/live for the QR encoder, analytics, cursors
// and the two typefaces; oskiewar.com resolves those through lith.
const elsewhere = new Map([
  ["/aesthetic.computer/dep/@akamfoad/qr/qr.mjs",
    "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs"],
  ["/aesthetic.computer/lib/product-analytics.mjs",
    "system/public/aesthetic.computer/lib/product-analytics.mjs"],
  ["/aesthetic.computer/lib/oskiewar-analytics.mjs",
    "system/public/aesthetic.computer/lib/oskiewar-analytics.mjs"],
  ["/aesthetic.computer/cursors/precise.svg",
    "system/public/aesthetic.computer/cursors/precise.svg"],
  ["/aesthetic.computer/cursors/active.svg",
    "system/public/aesthetic.computer/cursors/active.svg"],
  ["/ComicRelief-Regular.ttf",
    "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf"],
]);

function fileFor(pathname) {
  if (pathname === "/" || pathname === "/index.html")
    return join(live, "mac-test.html");
  const mapped = elsewhere.get(pathname);
  if (mapped) return join(repo, mapped);
  // Anything else comes out of xbox/live, and never above it.
  const target = normalize(join(live, pathname));
  return target.startsWith(live) ? target : "";
}

const server = createServer(async (request, response) => {
  const { pathname } = new URL(request.url, "http://127.0.0.1");
  if (pathname === "/api/product-analytics-config") {
    response.writeHead(200, { "content-type": "application/json" });
    response.end("{}");
    return;
  }
  const path = fileFor(pathname);
  if (!path) { response.writeHead(403); response.end("outside xbox/live"); return; }
  try {
    const body = await readFile(path);
    response.writeHead(200, {
      "content-type": mime.get(extname(path)) || "application/octet-stream",
      // Never cache: the point is that a save is the next reload.
      "cache-control": "no-store",
    });
    response.end(body);
  } catch (error) {
    response.writeHead(error.code === "ENOENT" ? 404 : 500);
    response.end(String(error.message));
  }
});

server.listen(port, "127.0.0.1", () => {
  console.log(`oskiewar working tree → http://127.0.0.1:${port}`);
  console.log("  P1  W A S D · Space kick · B punch · G shield · V item");
  console.log("  P2  arrows · K kick · ; punch · L shield · ' item");
  console.log("  Tab debug overlays · Enter menu · edit and reload");
});
