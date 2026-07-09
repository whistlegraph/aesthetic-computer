// whistlegraph youtube views — pull view counts from YouTube
//
//   node youtube-views.mjs --channel        # our own @whistlegraph channel
//   node youtube-views.mjs --ids a,b,c       # confirm specific ids as reposts
//   node youtube-views.mjs --search "query"  # search.list (100 quota units!)
//
// Reuses the vault OAuth client + whistlegraph refresh token (same as
// toolchain/youtube/yt.mjs) to mint a read access token.
//
// Two stores, because a repost is only "ours" once verified — search is
// noisy (covers, unrelated memes, our own channel echoing back):
//   downloads/YOUTUBE.json            — COUNTED: own channel + confirmed reposts
//   downloads/YOUTUBE-CANDIDATES.json — search hits awaiting review
// --channel writes own rows; --search appends candidates (never counted);
// --ids promotes ids into the counted store as confirmed reposts. Rows
// on our own channel id are always tagged "own", never "repost", even if
// they surface in a search. Reposts matter: whistlegraphs were re-uploaded
// across YouTube by others and those views are ours too — often dwarfing
// our own channel (one Butterfly repost alone > 250M).

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const VAULT = resolve(REPO, "aesthetic-computer-vault/youtube");
const STORE = join(HERE, "downloads", "YOUTUBE.json");
const CANDIDATES = join(HERE, "downloads", "YOUTUBE-CANDIDATES.json");
const OWN_CHANNEL = "UCZ_5AuCebRbm9t9_Y7SrckQ"; // @whistlegraph

const flags = {};
for (let i = 0; i < process.argv.length; i += 1) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  flags[a.slice(2)] = next === undefined || next.startsWith("--") ? true : next;
}

async function accessToken() {
  const c = JSON.parse(readFileSync(join(VAULT, "client.json"), "utf8"));
  const cl = c.installed || c.web || c;
  const tok = JSON.parse(readFileSync(join(VAULT, "whistlegraph-token.json"), "utf8"));
  const res = await fetch("https://oauth2.googleapis.com/token", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      client_id: cl.client_id,
      client_secret: cl.client_secret,
      refresh_token: tok.refresh_token,
      grant_type: "refresh_token",
    }),
  });
  const json = await res.json();
  if (!res.ok) throw new Error(`token ${res.status}: ${JSON.stringify(json)}`);
  return json.access_token;
}

const API = "https://www.googleapis.com/youtube/v3";
async function api(path, token) {
  const res = await fetch(`${API}/${path}`, { headers: { Authorization: `Bearer ${token}` } });
  const json = await res.json();
  if (!res.ok) throw new Error(`${path.split("?")[0]} ${res.status}: ${JSON.stringify(json.error?.errors || json)}`);
  return json;
}

const loadStore = () => (existsSync(STORE) ? JSON.parse(readFileSync(STORE, "utf8")) : { videos: [] });
const loadCandidates = () => (existsSync(CANDIDATES) ? JSON.parse(readFileSync(CANDIDATES, "utf8")) : { videos: [] });
function saveStore(store) {
  const byId = new Map(store.videos.map((v) => [v.id, v]));
  store.count = byId.size;
  store.updated = new Date().toISOString().slice(0, 10);
  store.totalViews = [...byId.values()].reduce((s, v) => s + (v.views || 0), 0);
  writeFileSync(STORE, JSON.stringify(store, null, 1));
}

const toRow = (item, source) => {
  const channelId = item.snippet?.channelId || "";
  return {
    id: item.id,
    title: item.snippet?.title || "",
    channel: item.snippet?.channelTitle || "",
    channelId,
    views: Number(item.statistics?.viewCount || 0),
    likes: Number(item.statistics?.likeCount || 0),
    comments: Number(item.statistics?.commentCount || 0),
    published: (item.snippet?.publishedAt || "").slice(0, 10),
    source: channelId === OWN_CHANNEL ? "own" : source, // never mis-tag ours as a repost
  };
};

// videos.list in batches of 50 → statistics + snippet.
async function fetchVideos(ids, token, source) {
  const rows = [];
  for (let i = 0; i < ids.length; i += 50) {
    const chunk = ids.slice(i, i + 50).join(",");
    const json = await api(`videos?part=snippet,statistics&id=${chunk}`, token);
    for (const item of json.items || []) rows.push(toRow(item, source));
  }
  return rows;
}

function merge(store, rows) {
  const byId = new Map(store.videos.map((v) => [v.id, v]));
  for (const r of rows) byId.set(r.id, { ...byId.get(r.id), ...r });
  store.videos = [...byId.values()];
}

const token = await accessToken();
const store = loadStore();

if (flags.channel) {
  // Our own channel: channels.list → uploads playlist → all items → stats.
  const ch = await api("channels?part=contentDetails,statistics&mine=true", token);
  const uploads = ch.items[0].contentDetails.relatedPlaylists.uploads;
  const channelViews = Number(ch.items[0].statistics.viewCount || 0);
  const ids = [];
  let pageToken = "";
  do {
    const json = await api(`playlistItems?part=contentDetails&maxResults=50&playlistId=${uploads}${pageToken ? `&pageToken=${pageToken}` : ""}`, token);
    for (const it of json.items) ids.push(it.contentDetails.videoId);
    pageToken = json.nextPageToken || "";
  } while (pageToken);
  const rows = await fetchVideos(ids, token, "own");
  merge(store, rows);
  saveStore(store);
  const sum = rows.reduce((s, v) => s + v.views, 0);
  console.log(`own channel: ${rows.length} videos, ${sum.toLocaleString()} views (channel lifetime stat: ${channelViews.toLocaleString()})`);
} else if (flags.ids) {
  const ids = String(flags.ids).split(",").map((s) => s.trim()).filter(Boolean);
  const rows = await fetchVideos(ids, token, flags.source || "repost");
  merge(store, rows);
  saveStore(store);
  for (const r of rows) console.log(`  ${r.views.toLocaleString().padStart(12)}  ${r.channel} — ${r.title}`);
} else if (flags.search) {
  // search.list: 100 quota units. Hits go to CANDIDATES (never counted) —
  // dropping any already-counted id and any on our own channel.
  const q = encodeURIComponent(String(flags.search));
  const json = await api(`search?part=snippet&type=video&maxResults=25&q=${q}`, token);
  const rows = await fetchVideos(json.items.map((it) => it.id.videoId), token, "repost");
  const counted = new Set(store.videos.map((v) => v.id));
  const cand = loadCandidates();
  const byId = new Map(cand.videos.map((v) => [v.id, v]));
  for (const r of rows) {
    if (r.source === "own" || counted.has(r.id)) continue;
    byId.set(r.id, { ...r, query: String(flags.search) });
  }
  cand.videos = [...byId.values()];
  cand.count = cand.videos.length;
  writeFileSync(CANDIDATES, JSON.stringify(cand, null, 1));
  console.log(`search "${flags.search}": ${rows.length} hits → candidates`);
  for (const r of rows.filter((r) => r.source !== "own").sort((a, b) => b.views - a.views)) {
    const mark = counted.has(r.id) ? "✓counted" : "?";
    console.log(`  ${r.views.toLocaleString().padStart(11)}  ${mark.padEnd(8)} [${r.id}] ${r.channel} — ${r.title.slice(0, 56)}`);
  }
  console.log(`\ncandidates: ${cand.count} awaiting review → ${CANDIDATES}`);
  console.log(`confirm the real ones with:  node youtube-views.mjs --ids <id,id,...>`);
} else {
  console.log("usage: --channel | --ids a,b,c | --search 'query'");
  process.exit(0);
}

console.log(`\ncounted store: ${store.count} videos, ${store.totalViews.toLocaleString()} total views → ${STORE}`);
