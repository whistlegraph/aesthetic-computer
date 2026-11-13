#!/usr/bin/env node
// Serve hopalong's working tree for a browser: this folder, plus the WebGL
// scene it borrows from xbox/live. Read from disk on every request, so a save
// is a reload.
//
//   node xbox/hopalong/serve.mjs [port]      → http://127.0.0.1:8126

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const live = resolve(here, "../live");
const port = Number(process.argv[2]) || 8126;
const borrowed = new Map([
  ["/live/scene3d-webgl.mjs", join(live, "scene3d-webgl.mjs")],
  ["/live/scene3d.mjs", join(live, "scene3d.mjs")],
]);
const mime = { ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8", ".png": "image/png" };

function fileFor(pathname) {
  if (pathname === "/" || pathname === "/index.html") return join(here, "index.html");
  if (borrowed.has(pathname)) return borrowed.get(pathname);
  const target = normalize(join(here, pathname));
  return target.startsWith(here + "/") ? target : "";
}

createServer(async (request, response) => {
  const path = fileFor(new URL(request.url, "http://127.0.0.1").pathname);
  if (!path) { response.writeHead(403); response.end("outside xbox/hopalong"); return; }
  try {
    const body = await readFile(path);
    response.writeHead(200, { "content-type": mime[extname(path)] || "application/octet-stream", "cache-control": "no-store" });
    response.end(body);
  } catch (error) {
    response.writeHead(error.code === "ENOENT" ? 404 : 500);
    response.end(String(error.message));
  }
}).listen(port, "127.0.0.1", () => {
  console.log(`hopalong → http://127.0.0.1:${port}`);
  console.log("  arrows / WASD move · space jump · shift or X run · down in the air pounds · enter pause · or an Xbox pad");
});
