// Catch-up sync: Mongo kidlisp → Datomic sidecar.
//
// Unlike backfill-kidlisp-to-datomic.mjs, this script is idempotent: for
// each piece it checks what's already in Datomic and only sends the
// delta. Intended to be run any time the sidecar has missed writes
// (e.g., everything between the 2026-03-24 backfill and the dual-write
// rollout).
//
// Env:
//   SIDECAR_URL      default http://127.0.0.1:8891
//   CLIENT_SECRET    required
//   SINCE            default 2026-03-24T00:00:00Z — low-water mark for
//                    filtering Mongo docs. Only docs touched after this
//                    (by `when`, `kept.keptAt`, `ipfsMedia.createdAt`,
//                    `pendingRebake.createdAt`, or a contract mint date)
//                    are processed.
//   DRY_RUN          if "true", logs intended writes without sending
//   ONLY_CODE        optional — process a single piece by code
//
// Usage (from silo):
//   SIDECAR_URL=http://127.0.0.1:8891 \
//   CLIENT_SECRET=... \
//   node system/backend/catchup-kidlisp-to-datomic.mjs

import { connect } from "./database.mjs";

const SIDECAR_URL = process.env.SIDECAR_URL || "http://127.0.0.1:8891";
const CLIENT_SECRET = process.env.CLIENT_SECRET;
const DRY_RUN = process.env.DRY_RUN === "true";
const SINCE = new Date(process.env.SINCE || "2026-03-24T00:00:00Z");
const ONLY_CODE = process.env.ONLY_CODE || null;

if (!DRY_RUN && !CLIENT_SECRET) {
  console.error("CLIENT_SECRET is required (or set DRY_RUN=true)");
  process.exit(1);
}

function sidecarHeaders() {
  return {
    "content-type": "application/json",
    "x-sidecar-secret": CLIENT_SECRET,
  };
}

async function sidecarReq(method, path, body) {
  if (DRY_RUN && method !== "GET") {
    return { ok: true, status: 200, body: { dryRun: true } };
  }
  const res = await fetch(`${SIDECAR_URL}${path}`, {
    method,
    headers: sidecarHeaders(),
    body: body != null ? JSON.stringify(body) : undefined,
  });
  const text = await res.text();
  let json = null;
  try { json = text ? JSON.parse(text) : null; } catch { /* not JSON */ }
  return { ok: res.ok, status: res.status, body: json ?? text };
}

function normInstant(value) {
  if (!value) return null;
  if (value instanceof Date) return value.toISOString();
  if (typeof value === "string") return value;
  if (typeof value === "number") return new Date(value).toISOString();
  return null;
}

function normalizeKeep(k, defaults = {}) {
  const tokenId = Number(k?.tokenId);
  if (!Number.isInteger(tokenId) || tokenId < 0) return null;
  const contractAddress = k?.contractAddress || defaults.contractAddress || null;
  if (!contractAddress) return null;
  return {
    tokenId,
    contractAddress,
    network: k?.network || defaults.network || "mainnet",
    txHash: k?.txHash || defaults.txHash || null,
    contractProfile: k?.contractProfile || k?.profile || defaults.contractProfile || null,
    contractVersion: k?.contractVersion || k?.version || defaults.contractVersion || null,
    keptAt: normInstant(k?.keptAt || k?.mintedAt || defaults.keptAt),
    keptBy: k?.keptBy || defaults.keptBy || null,
    walletAddress: k?.walletAddress || k?.owner || defaults.walletAddress || null,
    artifactUri: k?.artifactUri || defaults.artifactUri || null,
    thumbnailUri: k?.thumbnailUri || defaults.thumbnailUri || null,
    metadataUri: k?.metadataUri || defaults.metadataUri || null,
    source: defaults.source || "catchup",
  };
}

function existingKeepKey(k) {
  return `${k?.tokenId}::${(k?.contractAddress || "").toLowerCase()}::${k?.txHash || ""}`;
}

function docTouchedSince(doc, since) {
  const t = since.getTime();
  const dates = [
    doc.when,
    doc.kept?.keptAt,
    doc.ipfsMedia?.createdAt,
    doc.pendingRebake?.createdAt,
  ];
  if (doc.tezos?.contracts && typeof doc.tezos.contracts === "object") {
    for (const v of Object.values(doc.tezos.contracts)) {
      if (v?.mintedAt) dates.push(v.mintedAt);
      if (v?.lastUpdatedAt) dates.push(v.lastUpdatedAt);
      if (v?.lastConfirmAt) dates.push(v.lastConfirmAt);
    }
  }
  for (const d of dates) {
    if (!d) continue;
    const dt = new Date(d).getTime();
    if (Number.isFinite(dt) && dt >= t) return true;
  }
  return false;
}

async function ensureEntity(doc, stats) {
  // Idempotent by hash — if present, sidecar bumps hits and returns code.
  const res = await sidecarReq("POST", "/kidlisp", {
    code: doc.code,
    source: doc.source,
    hash: doc.hash,
    user_sub: doc.user || null,
    when: doc.when ? new Date(doc.when).toISOString() : null,
    hits: typeof doc.hits === "number" ? doc.hits : 1,
  });
  if (!res.ok) {
    stats.errors++;
    console.error(`  ! ensure failed ${doc.code}: ${res.status} ${JSON.stringify(res.body)}`);
    return false;
  }
  return true;
}

async function syncIpfsMedia(doc, stats) {
  if (!doc.ipfsMedia) return;
  const res = await sidecarReq("POST", `/kidlisp/${doc.code}/ipfs-media`, {
    artifactUri: doc.ipfsMedia.artifactUri || null,
    thumbnailUri: doc.ipfsMedia.thumbnailUri || null,
    sourceHash: doc.ipfsMedia.sourceHash || null,
    authorHandle: doc.ipfsMedia.authorHandle || null,
    depCount: doc.ipfsMedia.depCount ?? null,
    packDate: doc.ipfsMedia.packDate || null,
    createdAt: normInstant(doc.ipfsMedia.createdAt),
  });
  if (res.ok) stats.ipfs++;
  else {
    stats.errors++;
    console.error(`  ! ipfs-media ${doc.code}: ${res.status}`);
  }
}

async function syncPendingRebake(doc, stats) {
  if (!doc.pendingRebake) return;
  const res = await sidecarReq("POST", `/kidlisp/${doc.code}/pending-rebake`, doc.pendingRebake);
  if (res.ok) stats.pendingRebake++;
  else {
    stats.errors++;
    console.error(`  ! pending-rebake ${doc.code}: ${res.status}`);
  }
}

async function syncTezosState(doc, stats) {
  if (!doc.tezos || typeof doc.tezos !== "object") return;
  const t = doc.tezos;
  const res = await sidecarReq("POST", `/kidlisp/${doc.code}/tezos-state`, {
    minted: !!t.minted,
    exists: !!t.exists,
    tokenId: t.tokenId ?? null,
    txHash: t.txHash ?? null,
    creatorAddress: t.creatorAddress ?? null,
    codeHash: t.codeHash ?? null,
    network: t.network ?? null,
    reason: t.reason ?? null,
    error: t.error ?? null,
  });
  if (res.ok) stats.tezos++;
  else {
    stats.errors++;
    console.error(`  ! tezos-state ${doc.code}: ${res.status}`);
  }
}

async function syncKeeps(doc, stats) {
  // Collect candidate keeps from the Mongo doc
  const candidates = [];
  if (doc.kept) {
    const k = normalizeKeep(doc.kept, { source: "kept" });
    if (k) candidates.push(k);
  }
  if (doc.tezos?.contracts && typeof doc.tezos.contracts === "object") {
    for (const [contractAddress, v] of Object.entries(doc.tezos.contracts)) {
      if (!v || typeof v !== "object" || !v.minted) continue;
      const k = normalizeKeep(v, {
        source: "contract_keyed",
        contractAddress,
        keptAt: v.mintedAt,
      });
      if (k) candidates.push(k);
    }
  }
  if (candidates.length === 0) return;

  // Ask datomic what it already has so we skip duplicates.
  let existing = new Set();
  if (!DRY_RUN) {
    const lookup = await sidecarReq("GET", `/kidlisp/${doc.code}`);
    if (lookup.ok && lookup.body?.keeps) {
      for (const k of lookup.body.keeps) existing.add(existingKeepKey(k));
    }
  }

  for (const k of candidates) {
    if (existing.has(existingKeepKey(k))) {
      stats.keepsSkipped++;
      continue;
    }
    const res = await sidecarReq("POST", `/kidlisp/${doc.code}/mint`, k);
    if (res.ok) stats.keeps++;
    else {
      stats.errors++;
      console.error(`  ! mint ${doc.code} token ${k.tokenId}: ${res.status} ${JSON.stringify(res.body)}`);
    }
  }
}

async function processDoc(doc, stats) {
  stats.processed++;
  const ok = await ensureEntity(doc, stats);
  if (!ok) return;
  await syncIpfsMedia(doc, stats);
  await syncPendingRebake(doc, stats);
  await syncTezosState(doc, stats);
  await syncKeeps(doc, stats);
}

async function main() {
  const started = Date.now();
  console.log(`▶ catchup start — dryRun=${DRY_RUN} sidecar=${SIDECAR_URL} since=${SINCE.toISOString()}`);

  const database = await connect();
  const coll = database.db.collection("kidlisp");

  // Broad filter then precise check — picks up docs that were touched
  // after SINCE via any of the mutable sub-fields.
  const filter = ONLY_CODE
    ? { code: ONLY_CODE }
    : {
        $or: [
          { when: { $gte: SINCE } },
          { "kept.keptAt": { $gte: SINCE } },
          { "ipfsMedia.createdAt": { $gte: SINCE } },
          { "pendingRebake.createdAt": { $gte: SINCE } },
          { "tezos.mintedAt": { $gte: SINCE } },
        ],
      };

  const total = await coll.countDocuments(filter);
  console.log(`  candidate docs: ${total}`);

  const cursor = coll.find(filter).sort({ when: 1 }).batchSize(200);

  const stats = {
    processed: 0, ensured: 0, keeps: 0, keepsSkipped: 0,
    ipfs: 0, pendingRebake: 0, tezos: 0, errors: 0,
  };

  let last = Date.now();
  for await (const doc of cursor) {
    if (!docTouchedSince(doc, SINCE) && !ONLY_CODE) continue;
    await processDoc(doc, stats);
    if (Date.now() - last > 3000) {
      console.log(`  ${stats.processed} — ${JSON.stringify(stats)}`);
      last = Date.now();
    }
  }

  await database.disconnect();
  const secs = ((Date.now() - started) / 1000).toFixed(1);
  console.log(`✓ catchup done in ${secs}s — ${JSON.stringify(stats)}`);
  if (stats.errors > 0) process.exit(1);
}

main().catch((err) => {
  console.error("✗ catchup fatal:", err);
  process.exit(1);
});
