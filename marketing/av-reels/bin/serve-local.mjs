#!/usr/bin/env node
// serve-local.mjs — a tiny static + SPA server for aesthetic.computer, to TEST
// pieces locally WITHOUT `npm run site` (netlify dev, which crash-loops on macOS
// with EMFILE — its chokidar watcher blows past kern.maxfilesperproc=10240).
//
// AC's index is normally rendered by a netlify function, but `system/offline-index.html`
// is a static SPA entry ("run a static http server on the public folder") that loads
// boot.mjs, which reads the URL path to pick the piece. So: serve system/public/* with
// correct MIME + fall back unknown routes to offline-index.html. Self-running pieces
// (no /api backend calls) run fine off this.
//
//   node marketing/av-reels/bin/serve-local.mjs            # → http://localhost:8899
//   node marketing/av-reels/bin/capture-av.mjs <piece> --base http://localhost:8899
//   open -a "Google Chrome" http://localhost:8899/<piece>

import http from "node:http";
import { readFile, stat } from "node:fs/promises";
import { extname, join, normalize, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const ROOT = join(REPO, "system/public");
const INDEX = join(REPO, "system/offline-index.html");
const PORT = parseInt(process.env.PORT || process.argv[2] || "8899", 10);

const MIME = { ".mjs": "text/javascript", ".js": "text/javascript", ".css": "text/css", ".html": "text/html; charset=utf-8", ".json": "application/json", ".svg": "image/svg+xml", ".wasm": "application/wasm", ".png": "image/png", ".jpg": "image/jpeg", ".jpeg": "image/jpeg", ".gif": "image/gif", ".webp": "image/webp", ".otf": "font/otf", ".ttf": "font/ttf", ".woff": "font/woff", ".woff2": "font/woff2", ".ico": "image/x-icon", ".lisp": "text/plain", ".mp3": "audio/mpeg", ".wav": "audio/wav", ".txt": "text/plain", ".map": "application/json" };
// COOP/COEP so SharedArrayBuffer (audio worklet) works; credentialless avoids CORP on subresources.
const hdr = { "Access-Control-Allow-Origin": "*", "Cross-Origin-Opener-Policy": "same-origin", "Cross-Origin-Embedder-Policy": "credentialless" };

http.createServer(async (req, res) => {
  try {
    const p = decodeURIComponent(new URL(req.url, "http://x").pathname);
    const fp = join(ROOT, normalize(p).replace(/^(\.\.[/\\])+/, ""));
    let s = null; try { s = await stat(fp); } catch {}
    if (s && s.isFile()) {
      res.writeHead(200, { ...hdr, "Content-Type": MIME[extname(fp).toLowerCase()] || "application/octet-stream" });
      return res.end(await readFile(fp));
    }
    res.writeHead(200, { ...hdr, "Content-Type": "text/html; charset=utf-8" }); // SPA fallback → offline index
    res.end(await readFile(INDEX));
  } catch (e) { res.writeHead(500); res.end(String(e)); }
}).listen(PORT, () => console.log(`ac-static (piece test server) on http://localhost:${PORT}`));
