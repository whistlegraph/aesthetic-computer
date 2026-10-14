// Static server for xbox/live that answers /<room> with mac-test.html, like lith does for oskiewar.com.
import { createServer } from "node:http"; import { readFile } from "node:fs/promises"; import { extname, join } from "node:path";
import { fileURLToPath } from "node:url";
const REPO = fileURLToPath(new URL("../../../../", import.meta.url));
const ROOT = join(REPO, "xbox/live");
const PUB = join(REPO, "system/public");
const TYPES = { ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript", ".json": "application/json", ".png": "image/png", ".jpg": "image/jpeg", ".ttf": "font/ttf", ".woff2": "font/woff2", ".css": "text/css", ".wasm": "application/wasm", ".mp3": "audio/mpeg", ".svg": "image/svg+xml" };
createServer(async (req, res) => {
  let path = decodeURIComponent(new URL(req.url, "http://x").pathname);
  if (path === "/" || /^\/[a-z]{4,7}[0-9]{1,3}$/.test(path) || path === "/dojo") path = "/mac-test.html";
  try {
    let body; try { body = await readFile(join(ROOT, path)); } catch { body = await readFile(join(PUB, path)); }
    res.writeHead(200, { "content-type": TYPES[extname(path)] || "application/octet-stream", "cache-control": "no-cache", "access-control-allow-origin": "*" });
    res.end(body);
  } catch { res.writeHead(404); res.end("nope " + path); }
}).listen(8787, "0.0.0.0", () => console.log("serving", ROOT, "on :8787"));
