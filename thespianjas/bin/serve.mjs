#!/usr/bin/env node
import { createServer } from "node:http";
import { createReadStream, existsSync, statSync } from "node:fs";
import { extname, join, normalize, resolve } from "node:path";

const root = resolve(new URL("../..", import.meta.url).pathname);
const types = { ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript", ".glb": "model/gltf-binary", ".png": "image/png", ".json": "application/json" };
createServer((req, res) => {
  const rel = normalize(decodeURIComponent((req.url || "/").split("?")[0])).replace(/^\/+/, "");
  let file = join(root, rel);
  if (existsSync(file) && statSync(file).isDirectory()) file = join(file, "index.html");
  if (!file.startsWith(root) || !existsSync(file)) { res.writeHead(404); res.end("not found"); return; }
  res.writeHead(200, { "content-type": types[extname(file)] || "application/octet-stream", "cache-control": "no-store" });
  createReadStream(file).pipe(res);
}).listen(4177, "127.0.0.1", () => console.log("thespianjas studio · http://127.0.0.1:4177/thespianjas/studio/"));
