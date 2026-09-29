#!/usr/bin/env node
// Serve fiapup's working tree for a browser: xbox/fiapup, plus the two scene
// modules it borrows from xbox/live (the depth-tested WebGL renderer oskiewar
// uses). Read from disk on every request, so a save is a reload.
//
//   node xbox/fiapup/serve.mjs [port]      → http://127.0.0.1:8124

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const live = resolve(here, "../live");
const port = Number(process.argv[2]) || 8124;
const borrowed = new Map([
  ["/live/scene3d-webgl.mjs", join(live, "scene3d-webgl.mjs")],
  ["/live/scene3d.mjs", join(live, "scene3d.mjs")],
]);
const mime = { ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8", ".lisp": "text/plain; charset=utf-8", ".png": "image/png" };

export function fileFor(pathname) {
  if (pathname === "/" || pathname === "/index.html") return join(here, "index.html");
  if (borrowed.has(pathname)) return borrowed.get(pathname);
  const target = normalize(join(here, pathname));
  return target.startsWith(here + "/") ? target : "";
}

export function serve(listenPort = port) {
  const server = createServer(async (request, response) => {
    const path = fileFor(new URL(request.url, "http://127.0.0.1").pathname);
    if (!path) { response.writeHead(403); response.end("outside xbox/fiapup"); return; }
    try {
      const body = await readFile(path);
      response.writeHead(200, { "content-type": mime[extname(path)] || "application/octet-stream",
        "cache-control": "no-store" });
      response.end(body);
    } catch (error) {
      response.writeHead(error.code === "ENOENT" ? 404 : 500);
      response.end(String(error.message));
    }
  });
  return new Promise((ready) => server.listen(listenPort, "127.0.0.1", () => ready(server)));
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  await serve();
  console.log(`fiapup → http://127.0.0.1:${port}`);
  console.log("  WASD/arrows hand · space A · C B (call) · T X (treat) · R Y (play) · or an Xbox pad");
  console.log("  ?stage=fetch|pet|beg|nap|zoomies|tug[&seconds=n][&pause] to open on a moment");
}
