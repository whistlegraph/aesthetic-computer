#!/usr/bin/env node

// Backfill the `sayings` MongoDB collection from existing tts-cache S3
// objects. The `say` Netlify function stamps text/provider/voice/scream/ts
// onto each cached MP3 as user metadata, so we can rehydrate the catalog
// by listing the bucket and HEAD-ing each object.
//
// Idempotent: backfilled docs are tagged `_backfill: true` and upserted
// keyed on cacheKey, so re-running won't duplicate them. Live ledger
// rows written by say.js (no `_backfill` field) are unaffected.
//
// Env:
//   ART_ENDPOINT, ART_KEY, ART_SECRET, ART_SPACE_NAME — DO Spaces creds
//     (falls back to DO_SPACES_ENDPOINT/KEY/SECRET if ART_* missing)
//   MONGODB_CONNECTION_STRING, MONGODB_NAME — Mongo creds
//   PREFIX       default "tts-cache/"
//   DRY_RUN      "true" to scan + log without writing
//   CONCURRENCY  default 8 (parallel HEAD requests)
//
// Run (deps live in vault env files; ART_* in vault/.env, MONGODB_* in vault/lith/.env):
//   cd system
//   node --env-file=../aesthetic-computer-vault/.env \
//        --env-file=../aesthetic-computer-vault/lith/.env \
//        backend/backfill-sayings.mjs
//
// Dry-run jeffrey only:
//   DRY_RUN=true PREFIX=tts-cache/jeffrey/ node --env-file=... backend/backfill-sayings.mjs
//
// First-time deps (system/node_modules may be stale; slim install works):
//   mkdir -p backend/.backfill-deps && cd backend/.backfill-deps
//   echo '{"type":"module","dependencies":{"@aws-sdk/client-s3":"*","mongodb":"*","dotenv":"*"}}' > package.json
//   npm install && cd .. && ln -sf .backfill-deps/node_modules node_modules

import { S3Client, ListObjectsV2Command, HeadObjectCommand } from "@aws-sdk/client-s3";
import { config } from "dotenv";
import { fileURLToPath } from "url";
import { dirname, resolve } from "path";
import { connect } from "./database.mjs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
config({ path: resolve(__dirname, "../.env") });

const PREFIX = process.env.PREFIX || "tts-cache/";
const DRY_RUN = process.env.DRY_RUN === "true";
const CONCURRENCY = parseInt(process.env.CONCURRENCY || "8", 10);

const ENDPOINT = process.env.ART_ENDPOINT || process.env.DO_SPACES_ENDPOINT;
const KEY = process.env.ART_KEY || process.env.DO_SPACES_KEY;
const SECRET = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
const BUCKET = process.env.ART_SPACE_NAME;
const CDN_URL = "https://art.aesthetic.computer";

if (!ENDPOINT || !KEY || !SECRET || !BUCKET) {
  console.error("Missing Spaces creds (ART_ENDPOINT/ART_KEY/ART_SECRET/ART_SPACE_NAME)");
  process.exit(1);
}

const s3 = new S3Client({
  endpoint: ENDPOINT.startsWith("http") ? ENDPOINT : `https://${ENDPOINT}`,
  region: "us-east-1",
  credentials: { accessKeyId: KEY, secretAccessKey: SECRET },
});

async function* listAll(prefix) {
  let token;
  do {
    const r = await s3.send(new ListObjectsV2Command({
      Bucket: BUCKET,
      Prefix: prefix,
      ContinuationToken: token,
    }));
    for (const obj of r.Contents || []) yield obj;
    token = r.IsTruncated ? r.NextContinuationToken : undefined;
  } while (token);
}

async function head(key) {
  const r = await s3.send(new HeadObjectCommand({ Bucket: BUCKET, Key: key }));
  return { meta: r.Metadata || {}, lastModified: r.LastModified };
}

function buildEntry(key, meta, lastModified) {
  // S3 user-metadata keys are returned lowercase by the SDK.
  let when = meta.ts ? new Date(meta.ts) : null;
  if (!when || isNaN(when.getTime())) when = lastModified || null;
  // Infer provider from path when metadata predates the provider stamp.
  let provider = meta.provider || (key.includes("/jeffrey/") ? "jeffrey" : "unknown");
  return {
    text: meta.text || null,
    provider,
    voice: meta.voice || null,
    voiceSpec: null,
    scream: meta.scream === "1",
    instructions: null,
    cacheKey: key,
    url: `${CDN_URL}/${key}`,
    cached: null, // unknown — backfilled rows aren't tied to a play event
    when: when || new Date(0),
    _backfill: true,
  };
}

async function pool(items, fn, size) {
  const queue = [...items];
  let active = 0;
  const stats = { inserted: 0, already: 0, failed: 0 };
  return new Promise((resolveAll) => {
    function tick() {
      if (!queue.length && active === 0) return resolveAll(stats);
      while (active < size && queue.length) {
        const item = queue.shift();
        active++;
        Promise.resolve(fn(item, stats))
          .catch((e) => {
            stats.failed++;
            console.error(`✗ ${item}:`, e?.message || e);
          })
          .finally(() => {
            active--;
            tick();
          });
      }
    }
    tick();
  });
}

async function main() {
  console.log(`📦 Listing s3://${BUCKET}/${PREFIX}`);
  const keys = [];
  for await (const obj of listAll(PREFIX)) {
    if (obj.Key && obj.Key.endsWith(".mp3")) keys.push(obj.Key);
  }
  console.log(`Found ${keys.length} mp3 objects under ${PREFIX}`);
  if (!keys.length) process.exit(0);

  const database = await connect();
  const sayings = database.db.collection("sayings");
  await sayings.createIndex({ cacheKey: 1 });
  await sayings.createIndex({ when: -1 });
  await sayings.createIndex({ provider: 1, when: -1 });

  let processed = 0;
  const total = keys.length;

  const stats = await pool(keys, async (key, stats) => {
    const { meta, lastModified } = await head(key);
    const entry = buildEntry(key, meta, lastModified);
    processed++;

    if (DRY_RUN) {
      console.log(`[dry ${processed}/${total}] ${entry.provider} "${(entry.text || "").slice(0, 50)}" ← ${key}`);
      stats.inserted++;
      return;
    }

    const r = await sayings.updateOne(
      { cacheKey: key, _backfill: true },
      { $setOnInsert: entry },
      { upsert: true },
    );
    if (r.upsertedCount) {
      stats.inserted++;
      if (processed % 50 === 0) console.log(`✓ ${processed}/${total} (inserted=${stats.inserted})`);
    } else {
      stats.already++;
    }
  }, CONCURRENCY);

  console.log(
    `\nDone — inserted=${stats.inserted} already=${stats.already} failed=${stats.failed} (total=${total})`,
  );
  process.exit(0);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
