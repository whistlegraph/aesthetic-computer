#!/usr/bin/env node
// bake-thumbnails.mjs — ask the oven for a small animated webp of each
// KidLisp program (or piece) a handle has published, then write the oven's
// cached CDN URL onto the record as `thumbnailUrl` — the field mime already
// reads for tape posters, so mime cards and the @handle profile grid pick it
// up with no further plumbing.
//
// The oven renders once (~30s, puppeteer) and caches on Spaces; this script
// is the only thing that should ever ask it to render for a grid.
//
// Usage:
//   node kidlisp/tools/bake-thumbnails.mjs --handle jeffrey [--kind kidlisp|piece]
//        [--limit 36] [--concurrency 3] [--size 100] [--dry] [--force]

import { readFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { DAILY_EPISODE } from "../../system/backend/mime-media.mjs";

const args = Object.fromEntries(process.argv.slice(2).map((a, i, all) =>
  a.startsWith("--") ? [a.slice(2), all[i + 1]?.startsWith("--") || all[i + 1] === undefined ? true : all[i + 1]] : []).filter((p) => p.length));
const handle = String(args.handle || "").replace(/^@/, "").toLowerCase();
if (!handle) { console.error("need --handle"); process.exit(1); }
const kind = args.kind || "kidlisp";
const limit = Number(args.limit || 0);
const concurrency = Number(args.concurrency || 3);
const size = Number(args.size || 100);
const OVEN = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const GRAB = { duration: 3000, fps: 8, quality: 80, density: 1 };

for (const line of readFileSync(new URL("../../system/.env", import.meta.url), "utf8").split("\n")) {
  const m = /^([A-Z_]+)=(.*)$/.exec(line);
  if (m && !process.env[m[1]]) process.env[m[1]] = m[2].replace(/^"|"$/g, "");
}
const { MongoClient } = await import("../../system/node_modules/mongodb/lib/index.js");
const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING, { serverSelectionTimeoutMS: 15000 });
await client.connect();
const db = client.db(process.env.MONGODB_NAME);

const account = await db.collection("@handles").findOne({ handle }, { projection: { _id: 1 } })
  || await db.collection("@handles").findOne({ handle: { $regex: `^${handle.replaceAll(".", "\\.")}$`, $options: "i" } }, { projection: { _id: 1 } });
if (!account) { console.error(`no handle @${handle}`); process.exit(1); }

const visible = { nuked: { $ne: true }, deleted: { $ne: true }, private: { $ne: true }, draft: { $ne: true }, hidden: { $ne: true }, visibility: { $in: [null, "public"] } };
const collection = db.collection(kind === "piece" ? "pieces" : "kidlisp");
const match = { user: account._id, ...visible, ...(kind === "piece" ? { slug: { $type: "string", $ne: "" } }
    // Mime hides the podcast's daily episode programs, so they need no poster.
    : { source: { $type: "string", $ne: "", $not: DAILY_EPISODE } }) };
const records = await collection.find(match, { projection: { code: 1, hash: 1, source: 1, slug: 1, revision: 1, updatedAt: 1, thumbnailUrl: 1, thumbnailHash: 1 } })
  .sort({ when: -1, _id: -1 }).toArray();

// The cache key follows the source, so an edited program bakes again.
const keyOf = (r) => kind === "piece" ? String(r.revision ?? r.updatedAt ?? r._id) : (r.hash || createHash("sha256").update(r.source.trim()).digest("hex"));
const pieceOf = (r) => kind === "piece" ? `@${handle}/${r.slug.split("/").pop()}` : `$${r.code}`;
let todo = records.filter((r) => args.force || !r.thumbnailUrl || r.thumbnailHash !== keyOf(r));
if (limit) todo = todo.slice(0, limit);
console.log(`@${handle} ${kind}: ${records.length} public, ${todo.length} to bake${args.dry ? " (dry run)" : ""}`);
if (args.dry) { for (const r of todo.slice(0, 20)) console.log("  ", pieceOf(r), keyOf(r).slice(0, 12)); await client.close(); process.exit(0); }

async function grab(piece, key, format) {
  const url = `${OVEN}/grab/${format}/${size}/${size}/${piece}?duration=${GRAB.duration}&fps=${GRAB.fps}&quality=${GRAB.quality}&density=${GRAB.density}&cacheKey=src-${key}`;
  for (let attempt = 0; attempt < 2; attempt++) {
    const res = await fetch(url, { redirect: "manual", signal: AbortSignal.timeout(150000) });
    if (res.status === 301 || res.status === 302) return res.headers.get("location");
    if (!res.ok) throw new Error(`${format} ${res.status} ${(await res.text()).slice(0, 120)}`);
    await res.arrayBuffer(); // A fresh render streams the bytes; the next ask redirects to the CDN copy.
  }
  throw new Error(`${format}: oven never redirected to its cache`);
}

let done = 0, failed = 0;
const started = Date.now();
async function bake(r) {
  const piece = pieceOf(r), key = keyOf(r), t = Date.now();
  try {
    let format = "webp", url;
    try { url = await grab(piece, key, format); }
    catch (err) { format = "png"; url = await grab(piece, key, format); console.log(`   ${piece}: still image (${err.message.slice(0, 60)})`); }
    await collection.updateOne({ _id: r._id }, { $set: { thumbnailUrl: url, thumbnailHash: key, thumbnailFormat: format, thumbnailAt: new Date() }, $unset: { thumbnailError: "" } });
    done++;
    console.log(`✓ ${piece} ${((Date.now() - t) / 1000).toFixed(1)}s ${format} ${url}`);
  } catch (err) {
    failed++;
    await collection.updateOne({ _id: r._id }, { $set: { thumbnailError: err.message.slice(0, 200), thumbnailAt: new Date() } });
    console.log(`✗ ${piece} ${err.message.slice(0, 120)}`);
  }
}
const queue = [...todo];
await Promise.all(Array.from({ length: Math.min(concurrency, queue.length) }, async () => { while (queue.length) await bake(queue.shift()); }));
console.log(`baked ${done}, failed ${failed}, ${((Date.now() - started) / 60000).toFixed(1)} min`);
await client.close();
