// Dual-write helpers for the kidlisp Datomic cutover.
//
// Each function mirrors a Mongo write into the sidecar when
// KIDLISP_DATOMIC=on. They are fire-and-forget from the caller's
// perspective: errors are logged and swallowed so that the live
// Mongo write remains the source of truth for request success.
//
// Gate these calls on `kidlispDatomicEnabled()` in the caller so we
// don't pay for the sidecar round-trip when the flag is off.

import { sidecar, kidlispDatomicEnabled } from "./kidlisp-sidecar.mjs";

function normalizeInstant(value) {
  if (!value) return null;
  if (value instanceof Date) return value.toISOString();
  if (typeof value === "string") return value;
  if (typeof value === "number") return new Date(value).toISOString();
  return null;
}

function swallow(label, code) {
  return (err) => {
    const msg = err?.message || String(err);
    console.warn(`⚠️ sidecar ${label} for $${code} failed: ${msg}`);
  };
}

// Record a mint in Datomic. No-ops without a tokenId or contract address,
// since the sidecar's record-mint endpoint requires both. `source` labels
// the origin (kept, update, server_mint, contract_keyed, …) so the primary
// keep selector can prefer the freshest record.
export async function mirrorRecordMint(code, keep, { source = "kept" } = {}) {
  if (!kidlispDatomicEnabled()) return;
  if (!code || !keep) return;
  const tokenId = Number(keep.tokenId);
  if (!Number.isInteger(tokenId) || tokenId < 0) return;
  const contractAddress = keep.contractAddress || null;
  if (!contractAddress) return;

  const body = {
    tokenId,
    contractAddress,
    network: keep.network || "mainnet",
    txHash: keep.txHash || null,
    contractProfile: keep.contractProfile || null,
    contractVersion: keep.contractVersion || null,
    keptAt: normalizeInstant(keep.keptAt || keep.mintedAt || new Date()),
    keptBy: keep.keptBy || null,
    walletAddress: keep.walletAddress || keep.owner || null,
    artifactUri: keep.artifactUri || null,
    thumbnailUri: keep.thumbnailUri || null,
    metadataUri: keep.metadataUri || null,
    source,
  };

  try {
    await sidecar.recordMint(code, body);
  } catch (err) {
    swallow("recordMint", code)(err);
  }
}

export async function mirrorIpfsMedia(code, media) {
  if (!kidlispDatomicEnabled()) return;
  if (!code || !media) return;
  try {
    await sidecar.setIpfsMedia(code, {
      artifactUri: media.artifactUri || null,
      thumbnailUri: media.thumbnailUri || null,
      sourceHash: media.sourceHash || null,
      authorHandle: media.authorHandle || null,
      depCount: media.depCount ?? null,
      packDate: media.packDate || null,
      createdAt: normalizeInstant(media.createdAt),
    });
  } catch (err) {
    swallow("setIpfsMedia", code)(err);
  }
}

export async function mirrorPendingRebake(code, rebake) {
  if (!kidlispDatomicEnabled()) return;
  if (!code || !rebake) return;
  try {
    await sidecar.setPendingRebake(code, {
      artifactUri: rebake.artifactUri || null,
      thumbnailUri: rebake.thumbnailUri || null,
      metadataUri: rebake.metadataUri || null,
      contractAddress: rebake.contractAddress || null,
      contractProfile: rebake.contractProfile || null,
      contractVersion: rebake.contractVersion || null,
    });
  } catch (err) {
    swallow("setPendingRebake", code)(err);
  }
}

export async function mirrorTezosState(code, state) {
  if (!kidlispDatomicEnabled()) return;
  if (!code || !state) return;
  try {
    await sidecar.setTezosState(code, {
      minted: !!state.minted,
      exists: !!state.exists,
      tokenId: state.tokenId ?? null,
      txHash: state.txHash ?? null,
      creatorAddress: state.creatorAddress ?? null,
      codeHash: state.codeHash ?? null,
      network: state.network ?? null,
      reason: state.reason ?? null,
      error: state.error ?? null,
    });
  } catch (err) {
    swallow("setTezosState", code)(err);
  }
}
