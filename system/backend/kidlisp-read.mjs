// kidlisp-read.mjs — Datomic-aware piece lookup.
//
// When KIDLISP_DATOMIC=on, new pieces land in Datomic via store-kidlisp
// (routed through store-kidlisp-datomic.mjs), NOT in MongoDB. Callers like
// keep-prepare that still read Mongo directly miss those new pieces and
// return "Piece '$code' not found" for freshly-created work.
//
// This helper mirrors the store-kidlisp pattern: prefer the sidecar when
// Datomic is enabled (authoritative source for identity + source), then
// overlay the MongoDB doc if present so caller-specific fields like
// ipfsMedia, mediaHistory, kept, tezos, and pendingRebake — which still
// live in Mongo — continue to work for rebake / sync flows.

import { sidecar, kidlispDatomicEnabled } from "./kidlisp-sidecar.mjs";

/**
 * Fetch a kidlisp piece by its short code.
 *
 * @param {object} database - { db } from backend/database.mjs :: connect().
 * @param {string} code - Short piece code without the leading `$`.
 * @returns {Promise<object|null>} Piece doc or null.
 */
export async function loadKidlispPiece(database, code) {
  if (!code) return null;
  const collection = database.db.collection("kidlisp");

  if (kidlispDatomicEnabled()) {
    try {
      const datomic = await sidecar.lookupByCode(code);
      if (datomic) {
        // Authoritative fields come from Datomic. Merge any MongoDB row on
        // top so rebake-era fields (ipfsMedia, kept, tezos, pendingRebake,
        // mediaHistory) keep flowing through. Missing Mongo row just means
        // this is a brand-new piece that has never been kept yet.
        const mongo = await collection.findOne({ code });
        return mongo ? { ...datomic, ...mongo } : { ...datomic };
      }
    } catch (err) {
      console.warn(`kidlisp-read: sidecar lookupByCode(${code}) failed — falling back to Mongo:`, err?.message || err);
    }
  }

  return collection.findOne({ code });
}
