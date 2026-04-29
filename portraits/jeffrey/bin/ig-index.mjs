#!/usr/bin/env node
// Scan a completed instaloader archive and emit a per-account index summarizing
// what's there: post count, date range, media counts (image vs video), total size,
// and a flat list of shortcodes with date + media filenames.
//
// Usage:
//   node bin/ig-index.mjs whistlegraph
//
// Reads:  portraits/jeffrey/ig-archive/<user>/
// Writes: portraits/jeffrey/ig-archive/<user>-index.json

import { readdirSync, statSync, writeFileSync } from "fs";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO_ROOT = join(HERE, "..", "..", "..");

const username = process.argv[2];
if (!username) {
  console.error("usage: ig-index.mjs <username>");
  process.exit(64);
}

const archiveDir = join(
  REPO_ROOT,
  "portraits/jeffrey/ig-archive",
  username,
);
const indexPath = join(
  REPO_ROOT,
  "portraits/jeffrey/ig-archive",
  `${username}-index.json`,
);

const files = readdirSync(archiveDir);
const byShortcode = new Map();

for (const file of files) {
  // Filenames look like: 2020-12-04_CIXgfX8FdhI.jpg
  //                  or: 2020-12-04_CIXgfX8FdhI_2.jpg  (carousel)
  //                  or: 2020-12-04_CIXgfX8FdhI.json.xz (metadata)
  //                  or: 2020-12-04_CIXgfX8FdhI.mp4 (video)
  const m = file.match(/^(\d{4}-\d{2}-\d{2})_([A-Za-z0-9_-]+?)(?:_\d+)?\.(jpg|mp4|json\.xz)$/);
  if (!m) continue;
  const [, date, shortcode, ext] = m;
  if (!byShortcode.has(shortcode)) {
    byShortcode.set(shortcode, {
      date,
      images: [],
      videos: [],
      has_metadata: false,
      bytes: 0,
    });
  }
  const entry = byShortcode.get(shortcode);
  const fullPath = join(archiveDir, file);
  const size = statSync(fullPath).size;
  entry.bytes += size;
  if (ext === "jpg") entry.images.push(file);
  else if (ext === "mp4") entry.videos.push(file);
  else if (ext === "json.xz") entry.has_metadata = true;
}

const posts = [...byShortcode.entries()]
  .map(([shortcode, e]) => ({ shortcode, ...e }))
  .sort((a, b) => a.date.localeCompare(b.date));

const totalBytes = posts.reduce((s, p) => s + p.bytes, 0);
const dates = posts.map((p) => p.date);
const imageCount = posts.reduce((s, p) => s + p.images.length, 0);
const videoCount = posts.reduce((s, p) => s + p.videos.length, 0);
const carouselCount = posts.filter((p) => p.images.length + p.videos.length > 1).length;

const index = {
  generated: new Date().toISOString(),
  username,
  archive_dir: archiveDir.replace(REPO_ROOT + "/", ""),
  post_count: posts.length,
  date_range: { first: dates[0] || null, last: dates[dates.length - 1] || null },
  media: {
    images: imageCount,
    videos: videoCount,
    carousels: carouselCount,
  },
  total_bytes: totalBytes,
  total_size_human: humanSize(totalBytes),
  posts,
};

writeFileSync(indexPath, JSON.stringify(index, null, 2));

console.log(`wrote ${indexPath.replace(REPO_ROOT + "/", "")}`);
console.log(
  `${posts.length} posts (${imageCount} images, ${videoCount} videos, ${carouselCount} carousels)`,
);
console.log(`${dates[0] ?? "?"} → ${dates[dates.length - 1] ?? "?"}, ${humanSize(totalBytes)}`);

function humanSize(bytes) {
  if (bytes >= 1e9) return (bytes / 1e9).toFixed(2) + " GB";
  if (bytes >= 1e6) return (bytes / 1e6).toFixed(2) + " MB";
  if (bytes >= 1e3) return (bytes / 1e3).toFixed(2) + " KB";
  return bytes + " B";
}
