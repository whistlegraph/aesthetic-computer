#!/usr/bin/env node
// live.mjs — YouTube live-stream plumbing for the TV lanes.
//
// Sibling of yt.mjs (same vault credentials, same --as channel switch).
// Manages the two halves of a YouTube live setup:
//
//   liveStream    — the persistent RTMP ingestion point (url + secret key).
//                   Created once per channel, reused forever.
//   liveBroadcast — the watchable "video" viewers see. Bound to a stream;
//                   with autoStart it goes live the moment ffmpeg connects.
//
// ── usage ────────────────────────────────────────────────────────────
//   node toolchain/youtube/live.mjs list            [--as whistlegraph]
//   node toolchain/youtube/live.mjs ensure-stream --title "Whistlegraph TV" \
//        [--env-out <path>]                (writes RTMP_URL/STREAM_KEY env file, 0600)
//   node toolchain/youtube/live.mjs create-broadcast --title "..." \
//        --stream <streamId> [--privacy unlisted] [--description "..."] \
//        [--no-auto-stop]                  (24/7 broadcasts survive reconnects)
//   node toolchain/youtube/live.mjs status [--broadcast <id>]
//   node toolchain/youtube/live.mjs end --broadcast <id>
//
// Stream keys are secrets: output is masked unless --reveal is passed.
// Prefer --env-out and ship the file to the broadcast rig directly.

import { readFileSync, writeFileSync, existsSync, chmodSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");
const VAULT = resolve(REPO, "aesthetic-computer-vault/youtube");
const CLIENT_PATH = process.env.YT_CLIENT_JSON || resolve(VAULT, "client.json");
const API = "https://www.googleapis.com/youtube/v3";

// ── tiny arg parser (same shape as yt.mjs) ───────────────────────────
const argv = process.argv.slice(2);
const cmd = argv.shift();
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  } else positional.push(a);
}

function die(msg) { console.error(`✗ ${msg}`); process.exit(1); }

if (flags.as === true) die(`--as needs a channel name, e.g. --as whistlegraph`);
const CHANNEL_AS = flags.as || process.env.YT_CHANNEL || null;
const TOKEN_PATH = process.env.YT_TOKEN_JSON ||
  resolve(VAULT, CHANNEL_AS ? `${CHANNEL_AS}-token.json` : "token.json");

function loadClient() {
  if (!existsSync(CLIENT_PATH)) die(`OAuth client not found at ${CLIENT_PATH}`);
  const raw = JSON.parse(readFileSync(CLIENT_PATH, "utf8"));
  const c = raw.installed || raw.web || raw;
  if (!c.client_id || !c.client_secret) die(`${CLIENT_PATH} missing client_id/client_secret`);
  return { id: c.client_id, secret: c.client_secret };
}

async function accessToken() {
  if (!existsSync(TOKEN_PATH)) die(`No saved token at ${TOKEN_PATH} — run yt.mjs auth${CHANNEL_AS ? ` --as ${CHANNEL_AS}` : ""}`);
  const tok = JSON.parse(readFileSync(TOKEN_PATH, "utf8"));
  if (!tok.refresh_token) die(`saved token has no refresh_token — re-run yt.mjs auth`);
  const client = loadClient();
  const res = await fetch("https://oauth2.googleapis.com/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      client_id: client.id,
      client_secret: client.secret,
      refresh_token: tok.refresh_token,
      grant_type: "refresh_token",
    }),
  });
  const json = await res.json();
  if (!res.ok) die(`token refresh ${res.status}: ${JSON.stringify(json)}`);
  return json.access_token;
}

async function yt(at, method, path, body) {
  const res = await fetch(`${API}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${at}`,
      ...(body ? { "Content-Type": "application/json" } : {}),
    },
    body: body ? JSON.stringify(body) : undefined,
  });
  const json = res.status === 204 ? {} : await res.json();
  if (!res.ok) die(`${method} ${path} → ${res.status}: ${JSON.stringify(json.error?.errors || json)}`);
  return json;
}

function mask(key) {
  if (flags.reveal) return key;
  return key.length > 8 ? `${key.slice(0, 4)}…${key.slice(-4)} (masked — pass --reveal)` : "(masked)";
}

// ── commands ─────────────────────────────────────────────────────────

async function doList() {
  const at = await accessToken();
  const streams = await yt(at, "GET", "/liveStreams?part=id,snippet,cdn,status&mine=true&maxResults=50");
  console.log(`liveStreams (${streams.items?.length || 0}):`);
  for (const s of streams.items || []) {
    console.log(`  ${s.id}  "${s.snippet.title}"  ${s.cdn.resolution}@${s.cdn.frameRate}  status=${s.status.streamStatus} health=${s.status.healthStatus?.status || "-"}`);
  }
  const bcasts = await yt(at, "GET", "/liveBroadcasts?part=id,snippet,status,contentDetails&mine=true&maxResults=50");
  console.log(`liveBroadcasts (${bcasts.items?.length || 0}):`);
  for (const b of bcasts.items || []) {
    console.log(`  ${b.id}  "${b.snippet.title}"  ${b.status.lifeCycleStatus}/${b.status.privacyStatus}  stream=${b.contentDetails?.boundStreamId || "-"}  https://youtu.be/${b.id}`);
  }
}

async function doEnsureStream() {
  const title = flags.title || die(`--title required`);
  const at = await accessToken();
  const existing = await yt(at, "GET", "/liveStreams?part=id,snippet,cdn,status&mine=true&maxResults=50");
  let stream = (existing.items || []).find((s) => s.snippet.title === title);
  if (stream) {
    console.log(`✓ stream exists: ${stream.id} "${title}"`);
  } else {
    stream = await yt(at, "POST", "/liveStreams?part=snippet,cdn,contentDetails", {
      snippet: { title },
      cdn: { ingestionType: "rtmp", resolution: "variable", frameRate: "variable" },
      contentDetails: { isReusable: true },
    });
    console.log(`✓ stream created: ${stream.id} "${title}"`);
  }
  const ingest = stream.cdn.ingestionInfo;
  console.log(`  rtmp:  ${ingest.ingestionAddress}`);
  console.log(`  key:   ${mask(ingest.streamName)}`);
  if (flags["env-out"]) {
    const out = resolve(flags["env-out"]);
    writeFileSync(out, [
      `# ${title} — YouTube ingest (channel: ${CHANNEL_AS || "default"})`,
      `RTMP_URL=${ingest.ingestionAddress}`,
      `STREAM_KEY=${ingest.streamName}`,
      `STREAM_ID=${stream.id}`,
      ``,
    ].join("\n"));
    chmodSync(out, 0o600);
    console.log(`  env → ${out} (0600)`);
  }
}

async function doCreateBroadcast() {
  const title = flags.title || die(`--title required`);
  const streamId = flags.stream || die(`--stream <streamId> required (see ensure-stream / list)`);
  const privacy = flags.privacy || "unlisted";
  const at = await accessToken();
  const b = await yt(at, "POST", "/liveBroadcasts?part=snippet,status,contentDetails", {
    snippet: {
      title,
      description: flags.description || "",
      scheduledStartTime: new Date().toISOString(),
    },
    status: { privacyStatus: privacy, selfDeclaredMadeForKids: false },
    contentDetails: {
      enableAutoStart: true,
      // 24/7 lanes pass --no-auto-stop so an ffmpeg hiccup doesn't end the show.
      enableAutoStop: !flags["no-auto-stop"],
      monitorStream: { enableMonitorStream: false },
    },
  });
  await yt(at, "POST", `/liveBroadcasts/bind?id=${b.id}&part=id,contentDetails&streamId=${streamId}`);
  console.log(`✓ broadcast ${b.id} (${privacy}) bound to ${streamId}`);
  console.log(`  watch: https://youtu.be/${b.id}`);
  console.log(`  goes live automatically when the encoder connects (autoStart).`);
}

async function doStatus() {
  const at = await accessToken();
  if (flags.broadcast) {
    const b = await yt(at, "GET", `/liveBroadcasts?part=id,snippet,status,contentDetails&id=${flags.broadcast}`);
    const item = b.items?.[0] || die(`broadcast ${flags.broadcast} not found`);
    console.log(`${item.id} "${item.snippet.title}" → ${item.status.lifeCycleStatus} (${item.status.privacyStatus})`);
    if (item.contentDetails?.boundStreamId) {
      const s = await yt(at, "GET", `/liveStreams?part=id,status&id=${item.contentDetails.boundStreamId}`);
      const st = s.items?.[0];
      if (st) console.log(`stream ${st.id}: ${st.status.streamStatus}, health=${st.status.healthStatus?.status || "-"}`);
    }
  } else {
    await doList();
  }
}

async function doSetPrivacy() {
  const id = flags.broadcast || die(`--broadcast <id> required`);
  const privacy = flags.privacy || die(`--privacy public|unlisted|private required`);
  const at = await accessToken();
  // Broadcasts are videos; read-modify-write the status so the other
  // status fields (madeForKids, license, …) survive the part replace.
  const cur = await yt(at, "GET", `/videos?part=status&id=${id}`);
  const status = cur.items?.[0]?.status || die(`video ${id} not found`);
  status.privacyStatus = privacy;
  await yt(at, "PUT", `/videos?part=status`, { id, status });
  console.log(`✓ ${id} → ${privacy}`);
}

async function doEnd() {
  const id = flags.broadcast || die(`--broadcast <id> required`);
  const at = await accessToken();
  const b = await yt(at, "POST", `/liveBroadcasts/transition?broadcastStatus=complete&id=${id}&part=id,status`);
  console.log(`✓ broadcast ${id} → ${b.status.lifeCycleStatus}`);
}

const commands = {
  list: doList,
  "ensure-stream": doEnsureStream,
  "create-broadcast": doCreateBroadcast,
  "set-privacy": doSetPrivacy,
  status: doStatus,
  end: doEnd,
};

if (!cmd || !commands[cmd]) {
  console.log(`usage: live.mjs <${Object.keys(commands).join("|")}> [--as channel] …  (see header)`);
  process.exit(cmd ? 1 : 0);
}
await commands[cmd]();
