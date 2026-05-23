#!/usr/bin/env node
// Build a clean grid-post-only index for an instaloader archive.
//
// Reads curated/<account>-meta.jsonl, keeps GraphImage/GraphVideo/GraphSidecar,
// sorts newest→oldest by taken_at, joins each post to its local media file
// in ig-archive/<account>/, and emits curated/<account>-grid.json.
//
// Used as the source-of-truth feed for ig-grid-archive (private posts) and
// ig-grid-restore (curatorial unarchive).
//
// Usage:
//   node bin/ig-grid-index.mjs [--account=whistlegraph]

import { readFileSync, readdirSync, writeFileSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");

const args = process.argv.slice(2);
const accountArg = args.find((a) => a.startsWith("--account="));
const account = accountArg ? accountArg.split("=")[1] : "whistlegraph";

const metaPath = join(REPO, "portraits/jeffrey/curated", `${account}-meta.jsonl`);
const archiveDir = join(REPO, "portraits/jeffrey/ig-archive", account);
const outPath = join(REPO, "portraits/jeffrey/curated", `${account}-grid.json`);

const GRID_TYPES = new Set(["GraphImage", "GraphVideo", "GraphSidecar"]);

const archiveFiles = new Set(readdirSync(archiveDir));

function findLocalMedia(date, shortcode) {
  const stem = `${date}_${shortcode}`;
  for (const ext of [".jpg", ".mp4", "_UTC.jpg", "_UTC.mp4"]) {
    if (archiveFiles.has(stem + ext)) return stem + ext;
  }
  // sidecars may have _1.jpg / _2.jpg ...
  for (const f of archiveFiles) {
    if (f.startsWith(stem + "_") && (f.endsWith(".jpg") || f.endsWith(".mp4"))) return f;
  }
  return null;
}

const rows = readFileSync(metaPath, "utf8")
  .split("\n")
  .filter(Boolean)
  .map((l) => JSON.parse(l))
  .filter((r) => GRID_TYPES.has(r.typename));

rows.sort((a, b) => b.taken_at - a.taken_at);

const posts = rows.map((r) => ({
  shortcode: r.shortcode,
  url: `https://www.instagram.com/p/${r.shortcode}/`,
  taken_at: r.taken_at,
  date: r.date,
  typename: r.typename,
  is_video: !!r.is_video,
  likes: r.likes,
  comments: r.comments,
  caption: r.caption,
  local_media: findLocalMedia(r.date, r.shortcode),
}));

const oldest = posts[posts.length - 1];
const newest = posts[0];

const missingLocal = posts.filter((p) => !p.local_media).length;
const totalLikes = posts.reduce((s, p) => s + (p.likes ?? 0), 0);
const totalComments = posts.reduce((s, p) => s + (p.comments ?? 0), 0);
const videoCount = posts.filter((p) => p.is_video).length;
const sidecarCount = posts.filter((p) => p.typename === "GraphSidecar").length;

const index = {
  account,
  generated_at: new Date().toISOString(),
  post_count: posts.length,
  date_range: { oldest: oldest.date, newest: newest.date },
  first_post: {
    shortcode: oldest.shortcode,
    url: oldest.url,
    date: oldest.date,
    caption: oldest.caption,
  },
  stats: {
    total_likes: totalLikes,
    total_comments: totalComments,
    video_count: videoCount,
    sidecar_count: sidecarCount,
    missing_local_media: missingLocal,
  },
  posts,
};

writeFileSync(outPath, JSON.stringify(index, null, 2));

console.log(`wrote ${outPath}`);
console.log(`  ${posts.length} grid posts`);
console.log(`  newest: ${newest.date} ${newest.shortcode}`);
console.log(`  oldest: ${oldest.date} ${oldest.shortcode}   (keep visible)`);
console.log(`  total likes: ${totalLikes}  comments: ${totalComments}`);
console.log(`  video: ${videoCount}  sidecar: ${sidecarCount}  missing local: ${missingLocal}`);
