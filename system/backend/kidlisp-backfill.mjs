#!/usr/bin/env node
// Backfill the Mongo `kidlisp` projection from Datomic.
//
// Between the KIDLISP_DATOMIC cutover and backend/kidlisp-projection.mjs, new
// pieces landed only in Datomic. Everything that reads KidLisp by aggregation
// went stale at that moment: mime.ac's feed and board counts, /api/tv, and
// oven's /bundle-mini?piece=, which resolves source out of the same collection
// (oven/kidlisp-mini/bundle.mjs) and 404s for a piece it cannot find. This
// walks the sidecar corpus and projects anything Mongo is missing, which
// repairs all of them at once.
//
// Oven's *thumbnails* were never part of this. /grab/ drives a headless
// browser against https://aesthetic.computer, so it resolves a piece the same
// way a visitor does and never touched the stale collection.
//
// Dry run by default; nothing is written without --apply.
//
//   node system/backend/kidlisp-backfill.mjs                    # report only
//   node system/backend/kidlisp-backfill.mjs --apply
//   node system/backend/kidlisp-backfill.mjs --since 2026-06-17T00:00:00Z
//
// Safe to re-run: the projection upserts on `code` with $setOnInsert, so an
// existing row is never modified and a partial run simply resumes.

import { connect, closePool } from "./database.mjs";
import { sidecar, kidlispDatomicEnabled } from "./kidlisp-sidecar.mjs";
import { projectKidlispPiece } from "./kidlisp-projection.mjs";

const args = process.argv.slice(2);
const flag = (name) => args.includes(`--${name}`);
const value = (name) => {
  const i = args.indexOf(`--${name}`);
  return i >= 0 ? args[i + 1] : undefined;
};

const apply = flag("apply");
// The sidecar rejects a date-only `since`; a full ISO instant is required.
const since = value("since");
const limit = Number.parseInt(value("limit") ?? "100000", 10);

async function main() {
  if (!kidlispDatomicEnabled()) {
    console.error("❌ KIDLISP_DATOMIC is not 'on' — nothing to backfill from.");
    process.exit(1);
  }
  if (since && Number.isNaN(new Date(since).getTime())) {
    console.error(`❌ --since "${since}" is not a parseable instant.`);
    process.exit(1);
  }

  console.log(`🔎 Listing sidecar corpus (limit ${limit}${since ? `, since ${since}` : ""})…`);
  const listed = await sidecar.listCodes({ limit, sort: "recent", since });
  const pieces = listed?.recent || [];
  console.log(`   ${pieces.length} pieces in Datomic\n`);
  if (!pieces.length) return;

  const database = await connect();
  const collection = database.db.collection("kidlisp");

  // One pass to see the shape of the gap before writing anything.
  const codes = pieces.map((p) => p.code).filter(Boolean);
  const present = new Set(
    (await collection.find({ code: { $in: codes } }).project({ code: 1 }).toArray())
      .map((row) => row.code),
  );
  const missing = pieces.filter((p) => p.code && !present.has(p.code));
  const owned = missing.filter((p) => p.user).length;

  console.log(`📊 ${present.size} already projected, ${missing.length} missing`);
  console.log(`   of the missing: ${owned} handle-owned, ${missing.length - owned} anonymous`);
  if (missing.length) {
    const oldest = missing[missing.length - 1], newest = missing[0];
    console.log(`   span: ${oldest.when} → ${newest.when}\n`);
  }

  if (!apply) {
    console.log("🧪 Dry run. Re-run with --apply to write these rows.");
    return;
  }

  let inserted = 0, duplicate = 0, incomplete = 0, failed = 0;
  for (const piece of missing) {
    try {
      const result = await projectKidlispPiece(collection, piece);
      if (result.inserted) inserted++;
      else if (result.reason === "duplicate") duplicate++;
      else if (result.reason === "incomplete") incomplete++;
      if ((inserted + duplicate + incomplete) % 50 === 0) {
        console.log(`   …${inserted + duplicate + incomplete}/${missing.length}`);
      }
    } catch (err) {
      failed++;
      console.warn(`   ⚠️ $${piece.code}: ${err?.message || err}`);
    }
  }

  console.log(`\n✅ Projected ${inserted} pieces`);
  if (duplicate) console.log(`   ${duplicate} skipped: source already in Mongo under another code`);
  if (incomplete) console.log(`   ${incomplete} skipped: no code or empty source`);
  if (failed) console.log(`   ${failed} failed — re-run to retry, the upsert is idempotent`);
  console.log("\nNothing to purge: the feeds read Mongo live, and oven's thumbnails");
  console.log("never depended on it.");
}

// `connect()` hands back a pooled singleton and its `disconnect()` is a no-op
// unless AC_DB_CLOSE=1 (see database.mjs) — deliberate, so serverless handlers
// reuse the pool. A CLI has to close it by hand or the process just sits there
// after printing its last line, which is exactly what the first run of this
// script did.
main()
  .catch((err) => {
    console.error("❌ Backfill failed:", err?.message || err);
    process.exitCode = 1;
  })
  .finally(() => closePool());
