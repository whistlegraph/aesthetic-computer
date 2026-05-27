#!/usr/bin/env node
// serve.mjs — dirt-simple static server so audition.html can play audio.
// file:// blocks Audio() loads, but http://localhost works fine.
//
// usage:  node bin/serve.mjs           # port 7777, opens browser
//         node bin/serve.mjs --port 8080 --no-open

import { createServer } from "node:http";
import { existsSync, statSync, createReadStream } from "node:fs";
import { dirname, join, normalize, extname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
const PORT = Number(flags.port ?? 7777);

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js":   "application/javascript",
  ".mjs":  "application/javascript",
  ".css":  "text/css",
  ".json": "application/json",
  ".wav":  "audio/wav",
  ".mp3":  "audio/mpeg",
  ".ogg":  "audio/ogg",
  ".flac": "audio/flac",
  ".png":  "image/png",
};

const server = createServer((req, res) => {
  const url = decodeURIComponent((req.url || "/").split("?")[0]);
  const safe = normalize(url).replace(/^\/+/, "");
  const target = safe === "" ? "audition.html" : safe;
  const filePath = join(ROOT, target);
  if (!filePath.startsWith(ROOT)) { res.writeHead(403); return res.end("forbidden"); }
  if (!existsSync(filePath) || statSync(filePath).isDirectory()) { res.writeHead(404); return res.end("not found: " + target); }
  const mime = MIME[extname(filePath)] || "application/octet-stream";
  res.writeHead(200, { "content-type": mime, "access-control-allow-origin": "*" });
  createReadStream(filePath).pipe(res);
});

server.listen(PORT, () => {
  const url = `http://localhost:${PORT}/audition.html`;
  console.log(`# americomputadora audition · serving ${ROOT}`);
  console.log(`  → ${url}`);
  console.log(`  ctrl-c to stop`);
  if (!flags["no-open"]) spawn("open", [url], { detached: true, stdio: "ignore" }).unref();
});
