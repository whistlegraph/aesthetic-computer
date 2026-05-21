#!/usr/bin/env node
// demos.mjs — serve pop/demos.html and open it in a browser.
//
// The demo page imports the real /pop modules as ES modules, which the
// browser won't do over file:// — so this serves the pop/ directory
// over http and opens the page.
//
// Usage:
//   node pop/bin/demos.mjs            # serve + open browser
//   node pop/bin/demos.mjs --port 8123 --no-open

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";

const ROOT = fileURLToPath(new URL("..", import.meta.url)); // pop/

const args = process.argv.slice(2);
let port = 0, open = true;
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--port" && args[i + 1]) port = parseInt(args[++i], 10);
  else if (args[i] === "--no-open") open = false;
}

const TYPES = {
  ".html": "text/html; charset=utf-8",
  ".mjs":  "text/javascript; charset=utf-8",
  ".js":   "text/javascript; charset=utf-8",
  ".css":  "text/css; charset=utf-8",
  ".json": "application/json",
  ".wav":  "audio/wav",
  ".mp3":  "audio/mpeg",
  ".png":  "image/png",
  ".svg":  "image/svg+xml",
};

const server = createServer(async (req, res) => {
  let p = decodeURIComponent((req.url || "/").split("?")[0]);
  if (p === "/" || p === "") p = "/demos.html";
  const file = normalize(join(ROOT, p));
  if (!file.startsWith(ROOT.endsWith(sep) ? ROOT : ROOT + sep)) {
    res.writeHead(403); res.end("forbidden"); return;
  }
  try {
    const data = await readFile(file);
    res.writeHead(200, {
      "content-type": TYPES[extname(file)] || "application/octet-stream",
      "cache-control": "no-cache",
    });
    res.end(data);
  } catch {
    res.writeHead(404); res.end("not found: " + p);
  }
});

server.listen(port, "127.0.0.1", () => {
  const actual = server.address().port;
  const url = `http://localhost:${actual}/demos.html`;
  console.log(`pop demos → ${url}`);
  console.log("  serving", ROOT);
  console.log("  ctrl-c to stop");
  if (open) spawn("open", [url], { stdio: "ignore" });
});
