// keep-job.mjs — MongoDB-backed job model for the keeps mint pipeline.
//
// A "keep job" tracks the async preparation of a KidLisp piece for minting.
// Jobs are keyed by (piece, wallet) — only one active job per piece+wallet.
//
// Stages: validate → analyze → bundle → thumbnail → ipfs → metadata → ready
// Terminal states: ready, failed, expired
//
// Jobs auto-expire via MongoDB TTL index on `expiresAt`.

import { connect } from "./database.mjs";

const COLLECTION = "keep-jobs";
const JOB_TTL_MS = 30 * 60 * 1000; // 30 minutes

// ─── Statuses ─────────────────────────────────────────────────────────────────
export const STATUS = {
  PREPARING: "preparing",
  READY: "ready",
  FAILED: "failed",
  EXPIRED: "expired",
  CONFIRMED: "confirmed",
};

// ─── Stage ordering (for progress %) ─────────────────────────────────────────
export const STAGES = [
  "validate",
  "analyze",
  "bundle",
  "thumbnail",
  "ipfs",
  "metadata",
  "security",
  "ready",
];

function stagePercent(stage) {
  const idx = STAGES.indexOf(stage);
  if (idx < 0) return 0;
  return Math.round(((idx + 1) / STAGES.length) * 100);
}

// ─── Collection helper ───────────────────────────────────────────────────────
async function collection() {
  const { db } = await connect();
  return db.collection(COLLECTION);
}

// ─── Ensure TTL index exists (idempotent, call on cold start) ────────────────
let indexEnsured = false;
export async function ensureIndexes() {
  if (indexEnsured) return;
  const col = await collection();
  await col.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }).catch(() => {});
  // Compound unique index for upsert
  await col.createIndex({ piece: 1, wallet: 1 }, { unique: true }).catch(() => {});
  indexEnsured = true;
}

// ─── Create or reset a job ───────────────────────────────────────────────────
export async function upsertJob({ piece, wallet, user, handle, isRebake, regenerate }) {
  await ensureIndexes();
  const col = await collection();
  const now = new Date();

  const doc = {
    $set: {
      status: STATUS.PREPARING,
      stage: "validate",
      stageMessage: "Starting...",
      progress: 0,
      isRebake: !!isRebake,
      regenerate: !!regenerate,
      user: user || null,
      handle: handle || null,
      // Partial results — survive reconnects
      artifactUri: null,
      thumbnailUri: null,
      metadataUri: null,
      // Prepared params for wallet signing
      preparedData: null,
      error: null,
      errorStage: null,
      updatedAt: now,
      expiresAt: new Date(now.getTime() + JOB_TTL_MS),
    },
    $setOnInsert: {
      piece,
      wallet,
      createdAt: now,
    },
  };

  const result = await col.findOneAndUpdate(
    { piece, wallet },
    doc,
    { upsert: true, returnDocument: "after" }
  );

  return result;
}

// ─── Get job by piece + wallet ───────────────────────────────────────────────
export async function getJob(piece, wallet) {
  const col = await collection();
  return col.findOne({ piece, wallet });
}

// ─── Get job by _id ──────────────────────────────────────────────────────────
export async function getJobById(jobId) {
  const col = await collection();
  const { ObjectId } = await import("mongodb");
  return col.findOne({ _id: new ObjectId(jobId) });
}

// ─── Update job stage + message ──────────────────────────────────────────────
export async function updateJobStage(jobId, stage, message, { previewFrame } = {}) {
  const col = await collection();
  const { ObjectId } = await import("mongodb");
  const $set = {
    stage,
    stageMessage: message || stage,
    progress: stagePercent(stage),
    updatedAt: new Date(),
  };
  if (previewFrame !== undefined) $set.previewFrame = previewFrame;
  return col.updateOne({ _id: new ObjectId(jobId) }, { $set });
}

// ─── Set a partial result (e.g. artifactUri, thumbnailUri) ───────────────────
export async function setJobResult(jobId, fields) {
  const col = await collection();
  const { ObjectId } = await import("mongodb");
  return col.updateOne(
    { _id: new ObjectId(jobId) },
    {
      $set: {
        ...fields,
        updatedAt: new Date(),
      },
    }
  );
}

// ─── Mark job as ready with prepared data ────────────────────────────────────
export async function markJobReady(jobId, preparedData) {
  const col = await collection();
  const { ObjectId } = await import("mongodb");
  return col.updateOne(
    { _id: new ObjectId(jobId) },
    {
      $set: {
        status: STATUS.READY,
        stage: "ready",
        stageMessage: "Ready for wallet signature",
        progress: 100,
        preparedData,
        updatedAt: new Date(),
      },
    }
  );
}

// ─── Mark job as failed ──────────────────────────────────────────────────────
export async function markJobFailed(jobId, error, stage) {
  const col = await collection();
  const { ObjectId } = await import("mongodb");
  return col.updateOne(
    { _id: new ObjectId(jobId) },
    {
      $set: {
        status: STATUS.FAILED,
        error: typeof error === "string" ? error : error?.message || "Unknown error",
        errorStage: stage || null,
        updatedAt: new Date(),
      },
    }
  );
}

// ─── Mark job as confirmed (after on-chain tx) ──────────────────────────────
export async function markJobConfirmed(jobId, txHash, tokenId) {
  const col = await collection();
  const { ObjectId } = await import("mongodb");
  return col.updateOne(
    { _id: new ObjectId(jobId) },
    {
      $set: {
        status: STATUS.CONFIRMED,
        txHash,
        tokenId,
        updatedAt: new Date(),
      },
    }
  );
}

// ─── Format job for client response (strip internal fields) ──────────────────
export function formatJobForClient(job) {
  if (!job) return null;
  return {
    jobId: job._id.toString(),
    piece: job.piece,
    wallet: job.wallet,
    status: job.status,
    stage: job.stage,
    stageMessage: job.stageMessage,
    progress: job.progress,
    isRebake: job.isRebake,
    // Partial results
    artifactUri: job.artifactUri,
    thumbnailUri: job.thumbnailUri,
    metadataUri: job.metadataUri,
    // Live preview from oven grab
    previewFrame: job.previewFrame || null,
    // Ready state
    preparedData: job.preparedData,
    // Error state
    error: job.error,
    errorStage: job.errorStage,
    // Timestamps
    createdAt: job.createdAt,
    updatedAt: job.updatedAt,
  };
}
