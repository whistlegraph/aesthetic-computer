// whistlegraph catalog — snapshot every public @whistlegraph TikTok
//
//   node catalog.mjs                 # rebuild downloads/CATALOG.json
//   node catalog.mjs --account @x    # a different account
//
// One yt-dlp --flat-playlist -J call lists the whole account without
// touching each video page; the extractor already exposes per-post
// counts (views/likes/reposts/comments/saves), timestamp, duration and
// track. That's everything downstream needs — lyrics.mjs whispers from
// CATALOG.json, aggregate-views.mjs sums it, songs.mjs clusters it.
//
// This is the front of the index pipeline (before lyrics/glyphs/songs/
// codes/gen-site-data). It's deliberately additive-safe: a video only
// appears here once TikTok lists it publicly, so unprivating a batch of
// old posts and rerunning grows the catalog. Nothing is deleted from the
// site by a shrinking account — that's a downstream decision.
//
// thumbnails are signed + expiring (x-expires), so we keep only the
// cover URL for reference; comments are counts only (not harvestable).

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = join(HERE, "downloads", "CATALOG.json");

const args = process.argv.slice(2);
const accFlag = args.indexOf("--account");
const ACCOUNT = (accFlag >= 0 ? args[accFlag + 1] : "whistlegraph").replace(/^@/, "");
const URL = `https://www.tiktok.com/@${ACCOUNT}`;

console.log(`→ scraping @${ACCOUNT} (yt-dlp --flat-playlist -J)…`);
const raw = execFileSync(
  "yt-dlp",
  ["--flat-playlist", "-J", "--no-warnings", URL],
  { encoding: "utf8", maxBuffer: 256 * 1024 * 1024 },
);
const { entries } = JSON.parse(raw);

const iso = (ts) => (ts ? new Date(ts * 1000).toISOString().slice(0, 10) : null);
const cover = (e) => (e.thumbnails || []).find((t) => t.id === "cover")?.url
  ?? e.thumbnails?.[0]?.url ?? null;

const videos = entries
  .filter((e) => e?.id)
  .map((e) => ({
    id: e.id,
    url: e.url || `${URL}/video/${e.id}`,
    desc: (e.description || e.title || "").replace(/\s+/g, " ").trim(),
    views: e.view_count ?? null,
    likes: e.like_count ?? null,
    comments: e.comment_count ?? null,
    reposts: e.repost_count ?? null,
    saves: e.save_count ?? null,
    timestamp: e.timestamp ?? null,
    date: iso(e.timestamp),
    duration: e.duration ?? null,
    track: e.track ?? null,
    artists: e.artists ?? null,
    thumbnail: cover(e),
  }))
  .sort((a, b) => (b.timestamp ?? 0) - (a.timestamp ?? 0)); // newest first

const prevCount = existsSync(OUT)
  ? (JSON.parse(readFileSync(OUT, "utf8")).count ?? 0)
  : 0;

writeFileSync(
  OUT,
  JSON.stringify(
    {
      version: 1,
      account: ACCOUNT,
      source: "yt-dlp --flat-playlist -J " + URL,
      fetched: new Date().toISOString().slice(0, 10),
      count: videos.length,
      note: "thumbnail URLs are signed and expire (~x-expires); comments not harvestable via yt-dlp (extractor exposes counts only)",
      videos,
    },
    null,
    1,
  ),
);

const withViews = videos.filter((v) => v.views != null).length;
console.log(
  `wrote ${videos.length} videos (${withViews} with views) → ${OUT}` +
  (prevCount ? `  [was ${prevCount}, +${videos.length - prevCount}]` : ""),
);
