#!/usr/bin/env node
// Publish an accepted Pal turnaround master and animated WebP to stable,
// versioned Art Space keys.
//
// Usage: node bin/publish-pals-turnaround.mjs <slug>

import { existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out/pals/turnarounds");
const slug = process.argv[2];

if (!slug || !/^[a-z0-9]+(?:-[a-z0-9]+)*$/.test(slug)) {
  console.error("usage: node bin/publish-pals-turnaround.mjs <slug>");
  process.exit(1);
}

const env = { ...process.env };
const vault = resolve(REPO, "aesthetic-computer-vault/oven/.env");
if (existsSync(vault)) {
  for (const line of readFileSync(vault, "utf8").split("\n")) {
    const match = line.match(/^\s*(ART_SPACES_ENDPOINT|ART_SPACES_KEY|ART_SPACES_SECRET|ART_SPACES_BUCKET|ART_CDN_BASE)\s*=\s*(.+?)\s*$/);
    if (match) env[match[1]] ||= match[2].replace(/^['"]|['"]$/g, "");
  }
}

if (!env.ART_SPACES_KEY || !env.ART_SPACES_SECRET) {
  throw new Error(`Art Space credentials missing from environment or ${vault}`);
}

const files = [
  { ext: "mp4", type: "video/mp4" },
  { ext: "webp", type: "image/webp" },
].map((asset) => ({
  ...asset,
  path: resolve(OUT, `${slug}.${asset.ext}`),
  key: `pals/turnarounds/v1/${slug}.${asset.ext}`,
}));

for (const asset of files) {
  if (!existsSync(asset.path)) throw new Error(`Missing ${asset.path}`);
}

const client = new S3Client({
  endpoint: env.ART_SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com",
  region: "sfo3",
  credentials: {
    accessKeyId: env.ART_SPACES_KEY,
    secretAccessKey: env.ART_SPACES_SECRET,
  },
});
const bucket = env.ART_SPACES_BUCKET || "art-aesthetic-computer";
const cdn = env.ART_CDN_BASE || "https://art.aesthetic.computer";

for (const asset of files) {
  await client.send(new PutObjectCommand({
    Bucket: bucket,
    Key: asset.key,
    Body: readFileSync(asset.path),
    ContentType: asset.type,
    ACL: "public-read",
    CacheControl: "public, max-age=31536000, immutable",
  }));
  const url = `${cdn}/${asset.key}`;
  const response = await fetch(url, { method: "HEAD", cache: "no-store" });
  if (!response.ok) throw new Error(`Published ${url}, verification returned ${response.status}`);
  console.log(`${asset.ext}: ${url}`);
}
