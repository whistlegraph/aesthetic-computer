// Minimal AC static server for browser tests. Unknown paths intentionally serve
// index.html because every piece is URL-addressable (`/nopaint:seed`).

import { createReadStream, existsSync, statSync } from "node:fs";
import http from "node:http";
import { dirname, extname, join, normalize } from "node:path";

const root = process.argv[2];
const port = Number(process.argv[3] || 8888);
if (!root) throw new Error("usage: ac-static-server.mjs <root> [port]");

const types = {
  ".css": "text/css; charset=utf-8",
  ".gif": "image/gif",
  ".html": "text/html; charset=utf-8",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".png": "image/png",
  ".svg": "image/svg+xml",
  ".wasm": "application/wasm",
  ".webm": "audio/webm",
  ".woff2": "font/woff2",
};

const shell = `<!doctype html>
<html><head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1,maximum-scale=1,user-scalable=no">
  <link rel="stylesheet" href="/aesthetic.computer/style.css">
  <script type="module" src="/aesthetic.computer/boot.mjs"></script>
</head><body>
  <canvas id="boot-canvas" style="display:none"></canvas>
  <div id="console" class="hidden">booting...</div>
</body></html>`;

http.createServer((request, response) => {
  const pathname = decodeURIComponent(new URL(request.url, "http://localhost").pathname);
  const isNoPaintArchiveAsset = pathname.startsWith("/nopaint.art/");
  const assetPath = pathname.startsWith("/aesthetic.computer/")
    ? pathname.slice("/aesthetic.computer".length)
    : isNoPaintArchiveAsset
      ? pathname.slice("/nopaint.art".length)
      : pathname;
  const relative = normalize(assetPath).replace(/^[/\\]+/, "");
  const assetRoot = isNoPaintArchiveAsset ? join(dirname(root), "nopaint.art") : root;
  let file = join(assetRoot, relative);
  if (!file.startsWith(assetRoot) || !existsSync(file) || statSync(file).isDirectory()) {
    response.writeHead(200, {
      "cache-control": "no-store",
      "content-type": "text/html; charset=utf-8",
    });
    response.end(shell);
    return;
  }
  response.writeHead(200, {
    "cache-control": "no-store",
    "content-type": types[extname(file).toLowerCase()] || "application/octet-stream",
  });
  createReadStream(file).pipe(response);
}).listen(port, "127.0.0.1", () => {
  console.log(`AC browser-test server: http://localhost:${port}`);
});
