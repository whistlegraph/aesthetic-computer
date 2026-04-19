// Backfills the Mongo `kidlisp` collection into the Datomic sidecar.
// Idempotent: sidecar dedups by hash on POST /kidlisp. Re-runs safely.
//
// Env:
//   SIDECAR_URL      default http://127.0.0.1:8891
//   CLIENT_SECRET    required — matches sidecar.env's CLIENT_SECRET
//   DRY_RUN          if "true", counts but does not POST
//   BATCH_SIZE       default 500
//   START_AFTER      ISO timestamp — resume point (exclusive). Optional.
//
// Usage (from silo after sidecar is up):
//   SIDECAR_URL=http://127.0.0.1:8891 \
//   CLIENT_SECRET=... \
//   node system/backend/backfill-kidlisp-to-datomic.mjs

import { connect } from "./database.mjs";

const SIDECAR_URL = process.env.SIDECAR_URL || "http://127.0.0.1:8891";
const CLIENT_SECRET = process.env.CLIENT_SECRET;
const DRY_RUN = process.env.DRY_RUN === "true";
const BATCH_SIZE = parseInt(process.env.BATCH_SIZE || "500", 10);
const START_AFTER = process.env.START_AFTER ? new Date(process.env.START_AFTER) : null;

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

async function postJSON(path, body) {
  if (DRY_RUN) return { ok: true, status: 200, dryRun: true };
  const res = await fetch(`${SIDECAR_URL}${path}`, {
    method: "POST",
    headers: sidecarHeaders(),
    body: JSON.stringify(body),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => "");
    return { ok: false, status: res.status, body: text };
  }
  return { ok: true, status: res.status, body: await res.json().catch(() => ({})) };
}

function normalizeKeepForSidecar(k = {}, defaults = {}) {
  const tokenId = Number(k.tokenId);
  if (!Number.isInteger(tokenId) || tokenId < 0) return null;
  const contractAddress = k.contractAddress || defaults.contractAddress || null;
  if (!contractAddress) return null;
  return {
    tokenId,
    network: k.network || defaults.network || "mainnet",
    txHash: k.txHash || defaults.txHash || null,
    contractAddress,
    contractProfile: k.contractProfile || k.profile || defaults.contractProfile || null,
    contractVersion: k.contractVersion || k.version || defaults.contractVersion || null,
    keptAt: k.keptAt || k.mintedAt || defaults.keptAt || null,
    keptBy: k.keptBy || defaults.keptBy || null,
    walletAddress: k.walletAddress || k.owner || defaults.walletAddress || null,
    artifactUri: k.artifactUri || defaults.artifactUri || null,
    thumbnailUri: k.thumbnailUri || defaults.thumbnailUri || null,
    metadataUri: k.metadataUri || defaults.metadataUri || null,
    source: defaults.source || "backfill",
  };
}

async function backfillDoc(doc, stats) {
  const base = {
    code: doc.code,
    source: doc.source,
    hash: doc.hash,
    user_sub: doc.user || null,
    when: doc.when ? new Date(doc.when).toISOString() : null,
    hits: typeof doc.hits === "number" ? doc.hits : 1,
  };
  const create = await postJSON("/kidlisp", base);
  if (!create.ok) {
    stats.errors++;
    console.error(`  ! create failed for ${doc.code}: ${create.status} ${create.body}`);
    return;
  }
  stats.created++;

  // Keep records
  const keeps = [];
  if (doc.kept && typeof doc.kept === "object") {
    const k = normalizeKeepForSidecar(doc.kept, { source: "kept" });
    if (k) keeps.push(k);
  }
  if (doc.tezos?.minted) {
    const k = normalizeKeepForSidecar(doc.tezos, {
      source: "legacy_tezos",
      contractAddress: doc.tezos.contractAddress || doc.tezos.contract || null,
      keptAt: doc.tezos.mintedAt || null,
    });
    if (k) keeps.push(k);
  }
  if (doc.tezos?.contracts && typeof doc.tezos.contracts === "object") {
    for (const [contractAddress, v] of Object.entries(doc.tezos.contracts)) {
      if (!v || typeof v !== "object") continue;
      const k = normalizeKeepForSidecar(v, { source: "contract_keyed", contractAddress });
      if (k) keeps.push(k);
    }
  }
  for (const k of keeps) {
    const r = await postJSON(`/kidlisp/${doc.code}/mint`, k);
    if (r.ok) stats.keeps++;
    else { stats.errors++; console.error(`  ! mint failed for ${doc.code}:`, r.status); }
  }

  // Tezos legacy summary (even when not minted — status/reason/error)
  if (doc.tezos && typeof doc.tezos === "object") {
    const t = doc.tezos;
    const r = await postJSON(`/kidlisp/${doc.code}/tezos-state`, {
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
    if (r.ok) stats.tezos++;
    else stats.errors++;
  }

  // Pending rebake
  if (doc.pendingRebake && typeof doc.pendingRebake === "object") {
    const r = await postJSON(`/kidlisp/${doc.code}/pending-rebake`, doc.pendingRebake);
    if (r.ok) stats.pendingRebake++;
    else stats.errors++;
  }

  // IPFS media
  if (doc.ipfsMedia && typeof doc.ipfsMedia === "object") {
    const r = await postJSON(`/kidlisp/${doc.code}/ipfs-media`, doc.ipfsMedia);
    if (r.ok) stats.ipfs++;
    else stats.errors++;
  }

  // ATProto rkey
  if (doc.atproto?.rkey) {
    const r = await postJSON(`/kidlisp/${doc.code}/atproto-rkey`, { rkey: doc.atproto.rkey });
    if (r.ok) stats.atproto++;
    else stats.errors++;
  }
}

async function main() {
  const started = Date.now();
  console.log(`▶ backfill start — dryRun=${DRY_RUN} sidecar=${SIDECAR_URL}`);
  const database = await connect();
  const coll = database.db.collection("kidlisp");

  const filter = START_AFTER ? { when: { $gt: START_AFTER } } : {};
  const total = await coll.countDocuments(filter);
  console.log(`  docs to process: ${total}`);

  const cursor = coll.find(filter).sort({ when: 1 }).batchSize(BATCH_SIZE);

  const stats = {
    processed: 0, created: 0, keeps: 0, tezos: 0,
    pendingRebake: 0, ipfs: 0, atproto: 0, errors: 0,
  };

  let last = Date.now();
  for await (const doc of cursor) {
    await backfillDoc(doc, stats);
    stats.processed++;
    if (Date.now() - last > 3000) {
      console.log(`  ${stats.processed}/${total} — ${JSON.stringify(stats)}`);
      last = Date.now();
    }
  }

  await database.disconnect();
  const secs = ((Date.now() - started) / 1000).toFixed(1);
  console.log(`✓ backfill done in ${secs}s — ${JSON.stringify(stats)}`);
  if (stats.errors > 0) process.exit(1);
}

main().catch((err) => {
  console.error("✗ backfill fatal:", err);
  process.exit(1);
});
