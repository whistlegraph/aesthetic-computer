#!/usr/bin/env node
// chat-bridge.mjs — YouTube live chat → AC chat, one way.
//
// Watches the AC channel's active live broadcast (the laer-klokken TV
// station) and forwards each YouTube chat message into the main
// chat-system room as a "televised guest": the server's
// chat:service-message lane stamps it via:"youtube" + the visitor's
// display name, and the chat piece renders it with the red banner.
// Guests land in the main chat, not chat-clock — the laer-klokken room
// stays community-only (their call). AC → YouTube needs no bridge —
// the stream itself shows the room.
//
// The official v3 liveChatMessages API returns bodyless 404s for these
// chats (post-2024 chat-backend migration — Studio- and API-created
// broadcasts alike), so the chat is read the way the watch page reads
// it: InnerTube's get_live_chat with continuations. No OAuth, no quota.
// The v3 API is still used for one thing it does answer: which broadcast
// is live right now.
//
//   node chat-bridge.mjs <env-file>
//
// Env file (0600):
//   CHAT_SERVICE_SECRET=…                    shared with session-server
//   YT_CLIENT_JSON=/home/jas/actv/yt/client.json
//   YT_TOKEN_JSON=/home/jas/actv/yt/token.json   (AC channel refresh token)
//   CHAT_WSS=wss://chat-clock.aesthetic.computer

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
const NO_BROADCAST_MS = 60_000;
const MAX_TEXT = 128; // chat-manager clamps to shared MAX_CHARS anyway
const UA =
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36";
if (!SECRET) {
  console.error("CHAT_SERVICE_SECRET missing from env file");
  process.exit(1);
}

const log = (...a) => console.log(`[bridge ${new Date().toISOString().slice(11, 19)}]`, ...a);

// ── v3 auth, used only to discover the active broadcast id ───────────
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
  if (!res.ok) throw new Error(`token refresh ${res.status}`);
  accessToken = json.access_token;
  accessTokenExpiry = Date.now() + (json.expires_in || 3600) * 1000;
  return accessToken;
}

async function activeBroadcastId() {
  const at = await token();
  const res = await fetch(
    "https://www.googleapis.com/youtube/v3/liveBroadcasts?part=id&broadcastStatus=active&broadcastType=all&maxResults=1",
    { headers: { Authorization: `Bearer ${at}` } },
  );
  const raw = await res.text();
  const json = raw ? JSON.parse(raw) : {};
  if (!res.ok) throw new Error(`liveBroadcasts → ${json.error?.errors?.[0]?.reason || res.status}`);
  return json.items?.[0]?.id || null;
}

// ── InnerTube live chat (what the watch page itself speaks) ──────────
// The watch page carries the API key, client version, and the chat's
// initial continuation; get_live_chat then walks continuation → actions
// → next continuation, with a server-suggested timeout between polls.
async function attachChat(videoId) {
  const res = await fetch(`https://www.youtube.com/watch?v=${videoId}`, {
    headers: { "User-Agent": UA, "Accept-Language": "en" },
  });
  const html = await res.text();
  const key = html.match(/"INNERTUBE_API_KEY":"([^"]+)"/)?.[1];
  const version = html.match(/"INNERTUBE_CLIENT_VERSION":"([^"]+)"/)?.[1] || "2.20260101.00.00";
  // The live chat's reload continuation lives inside the liveChatRenderer.
  const cont = html.match(/"liveChatRenderer":\{"continuations":\[\{"reloadContinuationData":\{"continuation":"([^"]+)"/)?.[1] ||
    html.match(/"continuation":"(0ofMyA[^"]+)"/)?.[1];
  if (!key || !cont) throw new Error("watch page had no live chat continuation");
  return { key, version, continuation: cont };
}

async function pollChat(chat) {
  const res = await fetch(
    `https://www.youtube.com/youtubei/v1/live_chat/get_live_chat?key=${chat.key}&prettyPrint=false`,
    {
      method: "POST",
      headers: { "Content-Type": "application/json", "User-Agent": UA },
      body: JSON.stringify({
        context: { client: { clientName: "WEB", clientVersion: chat.version, hl: "en" } },
        continuation: chat.continuation,
      }),
    },
  );
  if (!res.ok) throw new Error(`get_live_chat → ${res.status}`);
  const json = await res.json();
  const lc = json.continuationContents?.liveChatContinuation;
  if (!lc) return { messages: [], timeoutMs: null, ended: true };

  const contObj = lc.continuations?.[0];
  const data =
    contObj?.timedContinuationData ||
    contObj?.invalidationContinuationData ||
    contObj?.reloadContinuationData;
  if (data?.continuation) chat.continuation = data.continuation;

  const messages = [];
  for (const action of lc.actions || []) {
    const r = action.addChatItemAction?.item?.liveChatTextMessageRenderer;
    if (!r) continue;
    const name = r.authorName?.simpleText || "viewer";
    const text = (r.message?.runs || [])
      .map((run) => run.text || run.emoji?.shortcuts?.[0] || "")
      .join("")
      .trim();
    if (text) messages.push({ name, text });
  }
  return { messages, timeoutMs: data?.timeoutMs ?? 5000, ended: false };
}

// ── the AC chat side: one lazy socket, reconnect on demand ───────────
let ws = null;
let currentVideoId = null; // for the walk-back link on each guest message
function sendToChat(name, text) {
  const payload = JSON.stringify({
    type: "chat:service-message",
    content: {
      secret: SECRET,
      via: "youtube",
      name,
      text,
      // Clicking a televised guest walks you into their room: the popout chat.
      link: currentVideoId
        ? `https://www.youtube.com/live_chat?is_popout=1&v=${currentVideoId}`
        : undefined,
    },
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
  let chat = null;
  let drained = false; // first poll returns backlog — don't forward it

  for (;;) {
    try {
      if (!chat) {
        const videoId = await activeBroadcastId();
        if (!videoId) {
          await sleep(NO_BROADCAST_MS);
          continue;
        }
        chat = await attachChat(videoId);
        currentVideoId = videoId;
        drained = false;
        log(`attached to ${videoId} live chat (InnerTube)`);
      }

      const page = await pollChat(chat);
      if (page.ended) {
        log("chat ended — detaching");
        chat = null;
        continue;
      }

      if (!drained) {
        drained = true;
        log(`backlog drained (${page.messages.length} old messages skipped)`);
      } else {
        for (const { name, text } of page.messages) {
          let out = text.length > MAX_TEXT ? text.slice(0, MAX_TEXT) : text;
          const last = perAuthorLast.get(name) || 0;
          if (Date.now() - last < 3000) continue; // per-author flood guard
          perAuthorLast.set(name, Date.now());
          const ok = await sendToChat(name, out);
          log(`${ok ? "→" : "✗ dropped"} [${name}] ${out.slice(0, 60)}`);
        }
      }

      await sleep(Math.min(Math.max(page.timeoutMs || 5000, 2000), 30_000));
    } catch (err) {
      log("error:", err.message);
      chat = null;
      await sleep(30_000);
    }
  }
}

log(`bridge up → ${CHAT_WSS} (InnerTube reader)`);
run();
