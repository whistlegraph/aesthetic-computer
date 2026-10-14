// oskiewar-relay.mjs — the oskiewar live lane on its own: the same
// OskiewarLiveManager the session-server monolith mounts at /oskiewar-live,
// with nothing else beside it. It exists so the relay can stand in more than
// one place. Through a relay a fight's round trip is each player's round trip
// to it, added together, so the relay belongs between the two players; one
// relay in New Jersey cost two players in Los Angeles about 200 ms to reach
// each other. 26.10.14
//
//   PORT=8890 RELAY_REGION=sfo node oskiewar-relay.mjs
//
// Routes match the monolith's so a client treats every relay alike:
//   ws  /oskiewar-live?match=ow-<room>&role=…   the lane itself
//   GET /oskiewar-room?match=ow-<room>           { live, challenger }
//   GET /oskiewar-open-room                      the front door's matchmaker
//   GET /oskiewar-ping                           a tiny answer for timing
//   GET /health                                  { ok, region, rooms }
import http from "node:http";
import { WebSocketServer } from "ws";
import { OskiewarLiveManager } from "./oskiewar-live-manager.mjs";

const port = Number(process.env.PORT) || 8890;
const region = String(process.env.RELAY_REGION || "relay");
// The commit this box was shipped from, so a deploy can prove what it serves.
const commit = String(process.env.RELAY_COMMIT || "");
const manager = new OskiewarLiveManager();

const json = (res, status, body) => {
  res.writeHead(status, { "content-type": "application/json",
    "access-control-allow-origin": "*", "cache-control": "no-store" });
  res.end(JSON.stringify(body));
};

const server = http.createServer((req, res) => {
  const url = new URL(req.url, "http://relay");
  if (url.pathname === "/oskiewar-room")
    return json(res, 200, manager.roomStatus(url.searchParams.get("match") || ""));
  if (url.pathname === "/oskiewar-open-room") return json(res, 200, manager.openRoom() || {});
  if (url.pathname === "/oskiewar-ping") return json(res, 200, { region, at: Date.now() });
  if (url.pathname === "/health") return json(res, 200, { ok: true, region, commit, rooms: manager.rooms.size });
  json(res, 404, { error: "not found" });
});

const wss = new WebSocketServer({ server, maxPayload: 64 * 1024 });
wss.on("connection", (ws, req) => {
  if (!manager.handleConnection(ws, req)) ws.close(1008, "Unknown route");
});

// The same 15 s ping sweep the monolith runs: a socket that misses two
// pongs is gone, whether or not it ever said goodbye.
setInterval(() => {
  for (const ws of wss.clients) {
    if (ws.isAlive === false) { ws.terminate(); continue; }
    ws.isAlive = false;
    try { ws.ping(); } catch {}
  }
}, 15000).unref();

server.listen(port, "127.0.0.1", () => console.log(`oskiewar relay (${region}) on :${port}`));
