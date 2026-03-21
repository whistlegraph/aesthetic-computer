#!/usr/bin/env node
// ants/migrate-news-codes.mjs
// One-shot migration: prefix all news codes with "n"
//
// Updates:
//   news-posts.code:       "abc" → "nabc"
//   news-comments.postCode: "abc" → "nabc"
//   news-votes.itemId (where itemType === "post"): "abc" → "nabc"
//
// Usage:
//   node ants/migrate-news-codes.mjs              # dry run (default)
//   node ants/migrate-news-codes.mjs --apply      # apply changes
//
// Required env vars: MONGODB_CONNECTION_STRING, MONGODB_NAME

import { MongoClient } from "mongodb";

const dryRun = !process.argv.includes("--apply");

const uri = process.env.MONGODB_CONNECTION_STRING;
const dbName = process.env.MONGODB_NAME;

if (!uri || !dbName) {
  console.error("Missing MONGODB_CONNECTION_STRING or MONGODB_NAME");
  process.exit(1);
}

async function migrate() {
  const client = new MongoClient(uri);
  await client.connect();
  const db = client.db(dbName);

  console.log(`📰 News code migration (n-prefix)`);
  console.log(`   Mode: ${dryRun ? "DRY RUN" : "APPLY"}\n`);

  // 1. Migrate news-posts.code
  const posts = db.collection("news-posts");
  const allPosts = await posts.find({ code: { $not: /^n/ } }).toArray();
  console.log(`   news-posts to migrate: ${allPosts.length}`);

  for (const post of allPosts) {
    const newCode = `n${post.code}`;
    if (dryRun) {
      console.log(`     [dry] ${post.code} → ${newCode}  "${post.title?.slice(0, 40)}"`);
    } else {
      await posts.updateOne({ _id: post._id }, { $set: { code: newCode } });
      console.log(`     ✓ ${post.code} → ${newCode}`);
    }
  }

  // 2. Migrate news-comments.postCode
  const comments = db.collection("news-comments");
  const allComments = await comments.find({ postCode: { $not: /^n/ } }).toArray();
  console.log(`\n   news-comments to migrate: ${allComments.length}`);

  for (const comment of allComments) {
    const newPostCode = `n${comment.postCode}`;
    if (dryRun) {
      console.log(`     [dry] postCode ${comment.postCode} → ${newPostCode}`);
    } else {
      await comments.updateOne({ _id: comment._id }, { $set: { postCode: newPostCode } });
      console.log(`     ✓ postCode ${comment.postCode} → ${newPostCode}`);
    }
  }

  // 3. Migrate news-votes.itemId (post votes only)
  const votes = db.collection("news-votes");
  const postVotes = await votes.find({ itemType: "post", itemId: { $not: /^n/ } }).toArray();
  console.log(`\n   news-votes (post) to migrate: ${postVotes.length}`);

  for (const vote of postVotes) {
    const newItemId = `n${vote.itemId}`;
    if (dryRun) {
      console.log(`     [dry] itemId ${vote.itemId} → ${newItemId}`);
    } else {
      await votes.updateOne({ _id: vote._id }, { $set: { itemId: newItemId } });
      console.log(`     ✓ itemId ${vote.itemId} → ${newItemId}`);
    }
  }

  console.log(`\n${dryRun ? "🔍 Dry run complete. Use --apply to execute." : "✅ Migration complete."}`);

  await client.close();
}

migrate().catch((err) => {
  console.error("Migration failed:", err);
  process.exit(1);
});
