#!/usr/bin/env node
// chat-bridge.mjs — YouTube live chat → AC chat, one way.
//
// Watches the AC channel's active live broadcast (the laer-klokken TV
// station) and forwards each YouTube chat message into the chat-clock
// room as a "televised guest": the server's chat:service-message lane
// stamps it via:"youtube" + the visitor's display name, and both laklok
// sisters render it with the red banner. AC → YouTube needs no bridge —
// the stream itself shows the room.
//
//   node chat-bridge.mjs <env-file>
//
// Env file (0600):
//   CHAT_SERVICE_SECRET=…                    shared with session-server
//   YT_CLIENT_JSON=/home/jas/actv/yt/client.json
//   YT_TOKEN_JSON=/home/jas/actv/yt/token.json   (AC channel refresh token)
//   CHAT_WSS=wss://chat-clock.aesthetic.computer
//
// Quota manners: polls every ACTIVE_POLL_MS (20s) only while a broadcast
// is live AND chat has been active in the last five minutes; otherwise
// IDLE_POLL_MS (90s). Between broadcasts it naps a minute. On 403 quota
// errors it backs off ten minutes rather than burning the project dry.

import { readFileSync } from "node:fs";

const ENV_FILE = process.argv[2];
if (!ENV_FILE) {
  console.error("usage: chat-bridge.mjs <env-file>");
  process.exit(1);
}
for (const line of readFileSync(ENV_FILE, "utf8").split("\n")) {
  const m = line.match(/^([A-Z_]+)=(.*)$/);
  if (m && !process.env[m[1]]) process.env[m[1]] = m[2];
}

const SECRET = process.env.CHAT_SERVICE_SECRET;
const CHAT_WSS = process.env.CHAT_WSS || "wss://chat-clock.aesthetic.computer";
const ACTIVE_POLL_MS = Number(process.env.ACTIVE_POLL_MS || 20_000);
const IDLE_POLL_MS = Number(process.env.IDLE_POLL_MS || 90_000);
const NO_BROADCAST_MS = 60_000;
const QUOTA_BACKOFF_MS = 600_000;
const MAX_TEXT = 128; // chat-manager clamps to shared MAX_CHARS anyway
if (!SECRET) {
  console.error("CHAT_SERVICE_SECRET missing from env file");
  process.exit(1);
}

const log = (...a) => console.log(`[bridge ${new Date().toISOString().slice(11, 19)}]`, ...a);

// ── YouTube auth (same refresh dance as toolchain/youtube/yt.mjs) ────
const client = (() => {
  const raw = JSON.parse(readFileSync(process.env.YT_CLIENT_JSON, "utf8"));
  const c = raw.installed || raw.web || raw;
  return { id: c.client_id, secret: c.client_secret };
})();
const refreshToken = JSON.parse(readFileSync(process.env.YT_TOKEN_JSON, "utf8")).refresh_token;

let accessToken = null;
let accessTokenExpiry = 0;
async function token() {
  if (accessToken && Date.now() < accessTokenExpiry - 300_000) return accessToken;
  const res = await fetch("https://oauth2.googleapis.com/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      client_id: client.id,
      client_secret: client.secret,
      refresh_token: refreshToken,
      grant_type: "refresh_token",
    }),
  });
  const json = await res.json();
  if (!res.ok) throw new Error(`token refresh ${res.status}: ${JSON.stringify(json)}`);
  accessToken = json.access_token;
  accessTokenExpiry = Date.now() + (json.expires_in || 3600) * 1000;
  return accessToken;
}

async function yt(path) {
  const at = await token();
  const res = await fetch(`https://www.googleapis.com/youtube/v3${path}`, {
    headers: { Authorization: `Bearer ${at}` },
  });
  const json = await res.json();
  if (!res.ok) {
    const reason = json.error?.errors?.[0]?.reason || res.status;
    const err = new Error(`GET ${path.split("?")[0]} → ${reason}`);
    err.reason = reason;
    throw err;
  }
  return json;
}

// ── the AC chat side: one lazy socket, reconnect on demand ───────────
let ws = null;
function sendToChat(name, text) {
  const payload = JSON.stringify({
    type: "chat:service-message",
    content: { secret: SECRET, via: "youtube", name, text },
  });
  return new Promise((resolve) => {
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(payload);
      return resolve(true);
    }
    ws = new WebSocket(CHAT_WSS);
    ws.addEventListener("open", () => {
      ws.send(payload);
      resolve(true);
    });
    ws.addEventListener("error", (e) => {
      log("chat ws error:", e.message || e.type);
      resolve(false);
    });
    ws.addEventListener("close", () => (ws = null));
  });
}

// ── main loop ────────────────────────────────────────────────────────
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const perAuthorLast = new Map(); // displayName → last forwarded epoch ms

async function run() {
  let liveChatId = null;
  let pageToken = "";
  let drained = false; // first page after (re)attach is backlog — don't forward
  let lastActivity = 0;

  for (;;) {
    try {
      if (!liveChatId) {
        const b = await yt("/liveBroadcasts?part=snippet&broadcastStatus=active&mine=true&maxResults=1");
        liveChatId = b.items?.[0]?.snippet?.liveChatId || null;
        if (!liveChatId) {
          await sleep(NO_BROADCAST_MS);
          continue;
        }
        pageToken = "";
        drained = false;
        log("attached to live chat", liveChatId.slice(0, 12) + "…");
      }

      const q = new URLSearchParams({ liveChatId, part: "snippet,authorDetails", maxResults: "200" });
      if (pageToken) q.set("pageToken", pageToken);
      const page = await yt(`/liveChatMessages?${q}`);
      pageToken = page.nextPageToken || pageToken;

      if (!drained) {
        drained = true; // history swallowed; everything after this forwards
        log(`backlog drained (${page.items?.length || 0} old messages skipped)`);
      } else {
        for (const item of page.items || []) {
          if (item.snippet?.type !== "textMessageEvent") continue;
          const name = item.authorDetails?.displayName || "viewer";
          let text = (item.snippet.displayMessage || "").trim();
          if (!text) continue;
          if (text.length > MAX_TEXT) text = text.slice(0, MAX_TEXT);
          const last = perAuthorLast.get(name) || 0;
          if (Date.now() - last < 3000) continue; // per-author flood guard
          perAuthorLast.set(name, Date.now());
          lastActivity = Date.now();
          const ok = await sendToChat(name, text);
          log(`${ok ? "→" : "✗ dropped"} [${name}] ${text.slice(0, 60)}`);
        }
      }

      // The chat is gone when the broadcast ends; offlineAt also signals it.
      if (page.offlineAt) {
        log("chat went offline");
        liveChatId = null;
        continue;
      }

      const hint = Number(page.pollingIntervalMillis || 0);
      const busy = Date.now() - lastActivity < 300_000;
      await sleep(Math.max(hint, busy ? ACTIVE_POLL_MS : IDLE_POLL_MS));
    } catch (err) {
      if (err.reason === "quotaExceeded" || err.reason === "rateLimitExceeded") {
        log("quota pressure — backing off 10 min");
        await sleep(QUOTA_BACKOFF_MS);
      } else if (err.reason === "liveChatEnded" || err.reason === "liveChatNotFound") {
        log("live chat ended — reattaching");
        liveChatId = null;
      } else {
        log("error:", err.message);
        await sleep(30_000);
      }
    }
  }
}

log(`bridge up → ${CHAT_WSS} (active ${ACTIVE_POLL_MS}ms / idle ${IDLE_POLL_MS}ms)`);
run();
