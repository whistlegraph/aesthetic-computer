// The headless shell oskiewar renders inside — one file list, one server.
//
// Every module `mac-test.html` imports has to be listed here, or the page dies
// on a 404 and a capture comes out empty rather than loudly wrong.
//
// `render-social-preview.mjs` and `tests/blackbox-rounds.mjs` still keep their
// own copies of this list. Folding them onto this one is worth doing, but it
// changes the burner's content hash and would leave
// `npm run xbox:check:oskiewar-social` red until the preview is re-burned —
// so it is a deliberate follow-up, not a drive-by.

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const live = resolve(fileURLToPath(new URL("..", import.meta.url)));
export const repo = resolve(live, "../..");

// Hashing these tells a burner whether its output still matches the game.
export const shellSources = ["oskiewar.js", "mac-test.html", "frame-driver.mjs"];

const fromLive = ["oskiewar.js", "oskiewar-sfx.mjs", "oskiewar-midi.mjs",
  "frame-driver.mjs", "round-room.mjs"];
const fromPublic = ["aesthetic.computer/dep/@akamfoad/qr/qr.mjs",
  "aesthetic.computer/lib/product-analytics.mjs",
  "aesthetic.computer/lib/oskiewar-analytics.mjs",
  "aesthetic.computer/cursors/precise.svg",
  "aesthetic.computer/cursors/active.svg"];

const mime = new Map([
  [".html", "text/html; charset=utf-8"], [".js", "text/javascript; charset=utf-8"],
  [".mjs", "text/javascript; charset=utf-8"], [".ttf", "font/ttf"],
  [".svg", "image/svg+xml"], [".json", "application/json"],
]);

export function fileFor(pathname) {
  const name = pathname.replace(/^\//, "");
  if (fromLive.includes(name)) return join(live, name);
  if (fromPublic.includes(name)) return join(repo, "system/public", name);
  if (name === "ComicRelief-Regular.ttf") return join(repo,
    "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf");
  // `/` is the game; `/roundname` is that round's replay room. Both are the
  // same page — `roundNameFromPath` reads the address.
  if (pathname === "/" || pathname === "/mac-test.html" ||
    /^\/[a-z0-9-]+\/?$/.test(pathname)) return join(live, "mac-test.html");
  return "";
}

// `replays` decides what a capture is allowed to touch:
//   "stub"  — nothing leaves the box. Synthetic rounds POST their demos into
//             the void, which is what a marketing render wants: 464 real
//             matches in the store should not be diluted by robot sparring.
//   "proxy" — GET reaches production so a recorded round can be played back.
//             POST is still swallowed.
export async function serveShell({ replays = "stub", log = () => {},
  port = 0, host = "127.0.0.1" } = {}) {
  let posts = 0;
  // Every round the game finishes POSTs its demo here. The factory reads
  // those to know when a *match* is over — `finalRoundWins` reaching the
  // best-of-five target is the only honest end-of-match signal the page
  // gives, and it arrives before the result card does.
  const demos = [];
  const replayBodies = new Map();
  const readBody = (request) => new Promise((done) => {
    let body = "";
    request.on("data", (chunk) => { body += chunk; });
    request.on("end", () => done(body));
  });
  const server = createServer(async (request, response) => {
    const url = new URL(request.url, "http://127.0.0.1");
    if (url.pathname === "/favicon.ico") { response.writeHead(204); response.end(); return; }
    if (url.pathname === "/api/product-analytics-config") {
      response.writeHead(200, { "content-type": "application/json" });
      response.end("{}");
      return;
    }
    if (url.pathname === "/api/oskiewar-replays") {
      if (request.method === "POST") {
        posts++;
        const body = await readBody(request);
        try {
          const demo = JSON.parse(body);
          const key = String(demo.roundName || demo.matchName || "").replace(/^ow-/, "");
          if (key) replayBodies.set(key, demo);
          demos.push({ at: Date.now(), roundName: demo.roundName || demo.matchName,
            roundIndex: demo.roundIndex ?? 0, winner: demo.winner ?? null,
            finalRoundWins: demo.finalRoundWins || [0, 0],
            durationTicks: demo.durationTicks ?? 0 });
        } catch {}
        response.writeHead(201, { "content-type": "application/json" });
        response.end(JSON.stringify({ ok: true, stored: false, sink: true }));
        return;
      }
      const requested = String(url.searchParams.get("id") || "").replace(/^ow-/, "");
      if (request.method === "GET" && requested && replayBodies.has(requested)) {
        response.writeHead(200, { "content-type": "application/json",
          "cache-control": "no-store" });
        response.end(JSON.stringify({ replay: replayBodies.get(requested) }));
        return;
      }
      if (replays === "proxy") {
        const upstream = await fetch(
          `https://aesthetic.computer${url.pathname}${url.search}`,
          { headers: { accept: "application/json" } });
        response.writeHead(upstream.status, {
          "content-type": upstream.headers.get("content-type") || "application/json",
          "cache-control": "no-store",
        });
        response.end(Buffer.from(await upstream.arrayBuffer()));
        return;
      }
      response.writeHead(404, { "content-type": "application/json" });
      response.end('{"error":"replays are stubbed"}');
      return;
    }
    // The live spectator channel and the AC feed are network the render does
    // not need; answering empty is quieter than letting them time out.
    if (url.pathname.startsWith("/api/")) {
      response.writeHead(200, { "content-type": "application/json" });
      response.end("{}");
      return;
    }
    const path = fileFor(url.pathname);
    if (!path) { response.writeHead(404); response.end("not found"); return; }
    try {
      const body = await readFile(path);
      response.writeHead(200, {
        "content-type": mime.get(extname(path)) || "application/octet-stream",
        "cache-control": "no-store",
      });
      response.end(body);
    } catch (error) {
      response.writeHead(500); response.end(error.message);
    }
  });
  // Local captures take any free loopback port. A standing preview service
  // (jasellite) needs a fixed port on a reachable interface instead.
  await new Promise((ready) => server.listen(port, host, ready));
  const origin = `http://${host === "0.0.0.0" ? "127.0.0.1" : host}:${server.address().port}`;
  log(`🎪 shell on ${origin} · replays ${replays}`);
  return {
    origin,
    demos,
    replayBodies,
    get replayPosts() { return posts; },
    close: () => new Promise((closed) => server.close(closed)),
  };
}
