// kidlisp-projection.mjs — the Mongo `kidlisp` collection as a read model.
//
// Datomic is authoritative for KidLisp: identity, source, and every mutable
// field (hits, keeps, tezos, pendingRebake). Reads of a single piece already
// go through backend/kidlisp-read.mjs, which prefers the sidecar.
//
// The *feeds* cannot. They are Mongo aggregations that sort, paginate and
// $lookup across collections — mime's opening posts (backend/mime-media.mjs),
// the TV feed (functions/tv.mjs), plus crm, metrics and atproto-user-stats.
// The sidecar has no equivalent, so between the KIDLISP_DATOMIC cutover and
// this module every piece created after it was invisible on mime.ac and
// /api/tv: 408 pieces, 270 of them handle-owned.
//
// So each new Datomic piece also gets an identity-only row here. The row holds
// exactly what the feeds address, sort and filter by. It deliberately holds
// nothing mutable, so the two stores cannot drift on anything that changes —
// this is a projection, not a second source of truth. Consequences worth
// knowing:
//
//   - `_id` is what earns a piece its stable mime thread key,
//     `kidlisp_<ObjectId>` (see mime-media.mjs), so the projection is what
//     makes a post-cutover piece addressable at all, not merely listed.
//   - `when` must be a BSON Date. The sidecar speaks ISO strings, and in BSON
//     sort order every string sorts below every Date, so a string `when` would
//     land the newest pieces at the *bottom* of a `{when: -1}` feed instead of
//     the top — stale in a way that looks like nothing happened.
//   - `hits` is NOT projected. Popularity is mutable and stays Datomic's, so
//     `sort=hits` feeds rank these pieces as 0 until they are read through
//     /api/store-kidlisp. Ranking by recency is unaffected.
//
// Projection is fire-and-forget at the call site: Datomic has already accepted
// the write, so a Mongo hiccup must not fail the request. Anything missed is
// repaired by `node system/backend/kidlisp-backfill.mjs`.

// Identity fields only. Anything a keep, mint or hit would change is absent by
// design — see the header.
export function projectionRow({ code, source, hash, when, user }) {
  if (!code || typeof source !== "string" || !source.trim()) return null;
  const at = when instanceof Date ? when : new Date(when ?? Date.now());
  return {
    code,
    source: source.trim(),
    hash: hash || null,
    // An unparseable instant would sort as Invalid Date; fall back to now so
    // the piece still reaches the top of the feed rather than vanishing.
    when: Number.isNaN(at.getTime()) ? new Date() : at,
    user: user || null,
  };
}

// Upsert by `code`, writing only on insert. Pre-cutover rows carry hits, kept,
// tezos and mediaHistory that the sidecar does not return; $setOnInsert is what
// keeps this safe to re-run over them and over itself (the backfill leans on
// that idempotence).
export async function projectKidlispPiece(collection, piece) {
  const row = projectionRow(piece);
  if (!row) return { projected: false, reason: "incomplete" };
  try {
    const result = await collection.updateOne(
      { code: row.code },
      { $setOnInsert: row },
      { upsert: true },
    );
    return { projected: true, inserted: !!result.upsertedCount };
  } catch (err) {
    // `hash` carries a unique index. Datomic dedups by hash before minting a
    // code, so a collision here means Mongo already holds this source under an
    // older code — the piece is reachable, nothing to repair.
    if (err?.code === 11000) return { projected: false, reason: "duplicate" };
    throw err;
  }
}

// Call from a write path that must not fail on Mongo. Logs and swallows, in the
// house style of backend/kidlisp-dual-write.mjs.
export function mirrorKidlispPiece(database, piece) {
  try {
    return projectKidlispPiece(database.db.collection("kidlisp"), piece).catch((err) => {
      console.warn(`⚠️ kidlisp projection for $${piece?.code} failed: ${err?.message || err}`);
    });
  } catch (err) {
    console.warn(`⚠️ kidlisp projection for $${piece?.code} failed: ${err?.message || err}`);
    return Promise.resolve();
  }
}
