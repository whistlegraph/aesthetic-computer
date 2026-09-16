#!/usr/bin/env node
// Publish accepted Pal turnarounds (mp4 master + animated WebP + APNG) to
// stable, versioned Art Space keys, then register them in
// system/backend/logo.mjs so pals.aesthetic.computer can serve them.
//
// Usage: node bin/publish-pals-turnaround.mjs <slug...>
//        node bin/publish-pals-turnaround.mjs --all     (every slug with all three files)

import { existsSync, readFileSync, writeFileSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out/pals/turnarounds");
const LOGO = resolve(REPO, "system/backend/logo.mjs");
const FORMATS = [
  { ext: "mp4", type: "video/mp4" },
  { ext: "webp", type: "image/webp" },
  { ext: "apng", type: "image/apng" },
];

const argv = process.argv.slice(2);
const complete = (slug) => FORMATS.every((f) => existsSync(resolve(OUT, `${slug}.${f.ext}`)));
let slugs = argv.filter((a) => /^[a-z0-9]+(?:-[a-z0-9]+)*$/.test(a));
if (argv.includes("--all")) {
  slugs = existsSync(OUT) ? [...new Set(readdirSync(OUT).map((f) => f.replace(/\.(mp4|webp|apng)$/, "")).filter((s) => /^[a-z0-9-]+$/.test(s)))].filter(complete).sort() : [];
}
if (!slugs.length) {
  console.error("usage: node bin/publish-pals-turnaround.mjs <slug...> | --all");
  process.exit(1);
}
for (const slug of slugs) {
  if (!complete(slug)) throw new Error(`Missing one of ${FORMATS.map((f) => `${slug}.${f.ext}`).join(", ")} in ${OUT} — run animate-pals.mjs first`);
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

const client = new S3Client({
  endpoint: env.ART_SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com",
  region: "sfo3",
  credentials: { accessKeyId: env.ART_SPACES_KEY, secretAccessKey: env.ART_SPACES_SECRET },
});
const bucket = env.ART_SPACES_BUCKET || "art-aesthetic-computer";
const cdn = env.ART_CDN_BASE || "https://art.aesthetic.computer";

const published = [];
for (const slug of slugs) {
  for (const f of FORMATS) {
    const key = `pals/turnarounds/v1/${slug}.${f.ext}`;
    await client.send(new PutObjectCommand({
      Bucket: bucket, Key: key, Body: readFileSync(resolve(OUT, `${slug}.${f.ext}`)),
      ContentType: f.type, ACL: "public-read",
      CacheControl: "public, max-age=31536000, immutable",
    }));
    const url = `${cdn}/${key}`;
    const response = await fetch(url, { method: "HEAD", cache: "no-store" });
    if (!response.ok) throw new Error(`Published ${url}, verification returned ${response.status}`);
    console.log(`${slug}.${f.ext}: ${url}`);
  }
  published.push(slug);
}

// Register in logo.mjs — the endpoint only serves turnarounds it knows about.
const src = readFileSync(LOGO, "utf8");
const m = src.match(/export const turnaroundSlugs = \[([\s\S]*?)\];/);
if (!m) throw new Error("turnaroundSlugs array not found in logo.mjs");
const existing = [...m[1].matchAll(/"([a-z0-9-]+)"/g)].map((x) => x[1]);
const merged = [...new Set([...existing, ...published])].sort();
const block = `export const turnaroundSlugs = [\n${merged.map((s) => `  "${s}",`).join("\n")}\n];`;
writeFileSync(LOGO, src.replace(m[0], block));
console.log(`\nlogo.mjs: turnaroundSlugs now ${merged.length} (${published.length} published this run)`);
