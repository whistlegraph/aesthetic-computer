#!/usr/bin/env node
// Manifest-driven, resumable YouTube batch uploader.
//
// Each completed upload writes the standard <video>.youtube.json receipt.
// Rerunning the same manifest skips matching receipts, reuses the playlist,
// and continues with the first unfinished video.

import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const YT = resolve(HERE, "yt.mjs");
const manifestArg = process.argv[2];
const dryRun = process.argv.includes("--dry-run");

function die(message) {
  console.error(`✗ ${message}`);
  process.exit(1);
}

if (!manifestArg) die(`usage: node toolchain/youtube/batch.mjs <manifest.json>`);
const manifestPath = resolve(process.cwd(), manifestArg);
if (!existsSync(manifestPath)) die(`manifest not found: ${manifestPath}`);
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
if (!manifest.channel) die(`manifest.channel is required`);
if (!manifest.playlist?.title) die(`manifest.playlist.title is required`);
if (!Array.isArray(manifest.videos) || !manifest.videos.length) die(`manifest.videos must be a non-empty array`);

const baseDir = resolve(dirname(manifestPath), manifest.baseDir || ".");
const defaults = manifest.defaults || {};

for (let index = 0; index < manifest.videos.length; index++) {
  const video = manifest.videos[index];
  if (!video.file || !video.title) die(`videos[${index}] needs file and title`);
  const videoPath = resolve(baseDir, video.file);
  if (!existsSync(videoPath)) die(`video not found: ${videoPath}`);
}

if (dryRun) {
  console.log(`✓ manifest valid · ${manifest.videos.length} videos`);
  console.log(`  channel  · ${manifest.channel}`);
  console.log(`  playlist · ${manifest.playlist.title} (${manifest.playlist.privacy || defaults.privacy || "unlisted"})`);
  console.log(`  base dir · ${baseDir}`);
  process.exit(0);
}

function run(args, { capture = false } = {}) {
  const result = spawnSync(process.execPath, [YT, ...args], {
    cwd: baseDir,
    encoding: "utf8",
    stdio: capture ? ["ignore", "pipe", "pipe"] : "inherit",
  });
  if (result.status !== 0) {
    if (capture) process.stderr.write(result.stderr || result.stdout || "");
    die(`YouTube command failed; rerun this manifest to resume`);
  }
  return capture ? result.stdout : "";
}

console.log(`▸ verifying channel ${manifest.channel}`);
run(["whoami", "--as", manifest.channel]);

const playlistArgs = [
  "playlist-ensure",
  "--as", manifest.channel,
  "--title", manifest.playlist.title,
  "--description", manifest.playlist.description || "",
  "--privacy", manifest.playlist.privacy || defaults.privacy || "unlisted",
  "--json",
];
const playlist = JSON.parse(run(playlistArgs, { capture: true }).trim());
console.log(`✓ playlist ${playlist.created ? "created" : "reused"} · ${playlist.title} (${playlist.id})`);

for (let index = 0; index < manifest.videos.length; index++) {
  const video = manifest.videos[index];
  const videoPath = resolve(baseDir, video.file);
  const receiptPath = videoPath.replace(/\.[^.]+$/, "") + ".youtube.json";
  if (existsSync(receiptPath)) {
    try {
      const receipt = JSON.parse(readFileSync(receiptPath, "utf8"));
      if (receipt.videoId && receipt.channel === manifest.channel) {
        console.log(`↷ ${index + 1}/${manifest.videos.length} already uploaded · ${video.file} · ${receipt.watchUrl}`);
        continue;
      }
    } catch {
      // A malformed receipt should not suppress the upload.
    }
  }

  console.log(`\n▸ ${index + 1}/${manifest.videos.length} ${video.file}`);
  const args = [
    "upload", videoPath,
    "--as", manifest.channel,
    "--title", video.title,
    "--description", video.description || "",
    "--privacy", video.privacy || defaults.privacy || "unlisted",
    "--category", String(video.category || defaults.category || "22"),
    "--playlist", playlist.id,
  ];
  const language = video.language || defaults.language;
  if (language) args.push("--language", language);
  const tags = video.tags || defaults.tags;
  if (Array.isArray(tags) && tags.length) args.push("--tags", tags.join(","));
  run(args);
}

console.log(`\n✓ batch complete · https://www.youtube.com/playlist?list=${playlist.id}`);
