#!/usr/bin/env node
// One-time scan of an instaloader archive's per-post .json.xz files to build a
// flat metadata catalog used by the show resolver and contact-sheet generator.
//
// For each file emits one JSONL row:
//   {
//     shortcode, date, typename,
//     caption, likes, comments,
//     taken_at, expiring_at,
//     is_video, has_iphone_struct
//   }
//
// "post" media kinds: GraphImage | GraphVideo | GraphSidecar
// "story" media kinds: GraphStoryImage | GraphStoryVideo
//
// Usage:
//   node bin/ig-meta-extract.mjs [--account=whistlegraph]

import { readdirSync, writeFileSync, existsSync } from "fs";
import { execFileSync } from "child_process";
import { join } from "path";

const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");

const args = process.argv.slice(2);
const accountArg = args.find((a) => a.startsWith("--account="));
const account = accountArg ? accountArg.split("=")[1] : "whistlegraph";

const archiveDir = join(REPO, "portraits/jeffrey/ig-archive", account);
const outPath = join(
  REPO,
  "portraits/jeffrey/curated",
  `${account}-meta.jsonl`,
);

if (!existsSync(archiveDir)) {
  console.error(`archive dir not found: ${archiveDir}`);
  process.exit(1);
}

const files = readdirSync(archiveDir)
  .filter((f) => f.endsWith(".json.xz"))
  .sort();

console.error(`scanning ${files.length} metadata files in ${archiveDir}`);

const rows = [];
let processed = 0;
let typeCounts = {};

function extract(node) {
  const ip = node?.iphone_struct ?? null;
  const captionFromEdge =
    node?.edge_media_to_caption?.edges?.[0]?.node?.text ?? null;
  const captionFromIp =
    typeof ip?.caption === "string"
      ? ip.caption
      : (ip?.caption?.text ?? null);
  const caption = captionFromEdge ?? captionFromIp ?? null;

  const likes =
    node?.edge_media_preview_like?.count ??
    ip?.like_count ??
    null;
  const comments =
    node?.edge_media_to_comment?.count ??
    ip?.comment_count ??
    null;

  return {
    typename: node?.__typename ?? null,
    caption,
    likes,
    comments,
    taken_at: node?.taken_at_timestamp ?? ip?.taken_at ?? null,
    expiring_at: node?.expiring_at_timestamp ?? null,
    is_video: !!node?.is_video,
    has_iphone_struct: !!ip,
  };
}

for (const f of files) {
  // Filename: 2014-08-23_sDmBX0ir5u.json.xz
  const m = f.match(/^(\d{4}-\d{2}-\d{2})_([A-Za-z0-9_-]+?)\.json\.xz$/);
  if (!m) continue;
  const [, date, shortcode] = m;
  const fullPath = join(archiveDir, f);
  let parsed;
  try {
    const raw = execFileSync("xzcat", [fullPath], {
      encoding: "utf8",
      maxBuffer: 32 * 1024 * 1024,
    });
    parsed = JSON.parse(raw);
  } catch (e) {
    rows.push({ shortcode, date, error: e.message });
    continue;
  }
  const node = parsed?.node ?? parsed;
  const meta = extract(node);
  typeCounts[meta.typename ?? "(none)"] =
    (typeCounts[meta.typename ?? "(none)"] || 0) + 1;
  rows.push({ shortcode, date, ...meta });
  processed++;
  if (processed % 500 === 0) {
    console.error(`  ${processed}/${files.length}`);
  }
}

writeFileSync(outPath, rows.map((r) => JSON.stringify(r)).join("\n") + "\n");

console.error(`wrote ${outPath}`);
console.error("typename distribution:");
for (const [k, v] of Object.entries(typeCounts).sort((a, b) => b[1] - a[1])) {
  console.error(`  ${k.padEnd(20)} ${v}`);
}
