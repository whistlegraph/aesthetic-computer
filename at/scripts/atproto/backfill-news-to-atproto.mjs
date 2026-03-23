#!/usr/bin/env node

/**
 * Backfill News to ATProto
 *
 * Syncs news-posts from MongoDB to computer.aesthetic.news on PDS.
 * Maps news-posts.title → news.headline
 *
 * Usage:
 *   node backfill-news-to-atproto.mjs [options]
 *
 * Options:
 *   --dry-run    Show what would be synced without making changes
 *   --limit N    Only process N news items
 */

import { connect } from "../../../system/backend/database.mjs";
import { createNewsOnAtproto } from "../../../system/backend/news-atproto.mjs";
import { config } from "dotenv";

config({ path: "../../../system/.env" });

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const limitIndex = args.indexOf("--limit");
const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : null;

async function backfillNews() {
  console.log("📰 Backfill News to ATProto\n");
  console.log(`Mode: ${dryRun ? "🔍 DRY RUN" : "✍️  LIVE"}`);
  if (limit) console.log(`Limit: ${limit}`);
  console.log();

  const database = await connect();
  const newsPosts = database.db.collection("news-posts");
  const handles = database.db.collection("@handles");

  // Find news-posts without ATProto sync
  const query = {
    $or: [
      { atproto: { $exists: false } },
      { "atproto.rkey": { $exists: false } },
    ],
  };

  const cursor = newsPosts.find(query).sort({ when: -1 });
  if (limit) cursor.limit(limit);

  const allNews = await cursor.toArray();
  console.log(`Found ${allNews.length} unsynced news posts\n`);

  if (allNews.length === 0) {
    console.log("✨ All news already synced!");
    await database.disconnect();
    return;
  }

  let synced = 0,
    skipped = 0,
    failed = 0;

  for (let i = 0; i < allNews.length; i++) {
    const post = allNews[i];
    const handle = await handles.findOne({ _id: post.user });
    const h = handle?.handle || "unknown";

    console.log(
      `  [${i + 1}/${allNews.length}] @${h} — "${post.title?.slice(0, 50)}"`,
    );

    if (dryRun) {
      synced++;
      continue;
    }

    try {
      // Map news-posts fields to lexicon fields
      const newsData = {
        headline: post.title,
        body: post.text || undefined,
        link: post.url || undefined,
        when: post.when ? new Date(post.when) : new Date(),
      };

      const result = await createNewsOnAtproto(
        database,
        post.user,
        newsData,
        post._id.toString(),
      );

      if (result.error) {
        console.log(`    ⏭️  ${result.error}`);
        skipped++;
      } else {
        // Store rkey back in MongoDB
        await newsPosts.updateOne(
          { _id: post._id },
          {
            $set: {
              "atproto.rkey": result.rkey,
              "atproto.uri": result.uri,
              "atproto.did": result.did,
              "atproto.syncedAt": new Date().toISOString(),
            },
          },
        );
        console.log(`    ✅ → ${result.rkey}`);
        synced++;
      }
    } catch (error) {
      console.log(`    ❌ ${error.message}`);
      failed++;
    }
  }

  console.log("\n" + "═".repeat(50));
  console.log(`✅ Synced:  ${synced}`);
  console.log(`⏭️  Skipped: ${skipped}`);
  console.log(`❌ Failed:  ${failed}`);
  console.log(`📊 Total:   ${allNews.length}\n`);

  await database.disconnect();
}

backfillNews().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
