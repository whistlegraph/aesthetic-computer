#!/usr/bin/env node

/**
 * Publish a commit summary to news.aesthetic.computer
 *
 * Posts a "Commits From <start> to <end>" news item with a formatted
 * summary of git commits. Connects directly to MongoDB and optionally
 * syncs to ATProto PDS.
 *
 * Usage:
 *   node at/publish-commits.mjs                        # last 20 commits
 *   node at/publish-commits.mjs --count 50             # last 50 commits
 *   node at/publish-commits.mjs --from abc1234         # from a specific hash
 *   node at/publish-commits.mjs --from abc1234 --to def5678
 *   node at/publish-commits.mjs --since "3 days ago"   # git log --since
 *   node at/publish-commits.mjs --dry-run              # preview without posting
 */

import { MongoClient } from "mongodb";
import { AtpAgent } from "@atproto/api";
import { config } from "dotenv";
import { execSync } from "child_process";
import { randomBytes } from "crypto";

// Load env from multiple sources
config({ path: new URL("../.devcontainer/envs/devcontainer.env", import.meta.url) });
config({ path: new URL(".env", import.meta.url) });

const MONGODB_URI = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";
const ADMIN_SUB = process.env.ADMIN_SUB;
const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

// ---------------------------------------------------------------------------
// Args
// ---------------------------------------------------------------------------

function parseArgs(argv) {
  const out = { _: [] };
  for (let i = 0; i < argv.length; i++) {
    const t = argv[i];
    if (!t.startsWith("--")) { out._.push(t); continue; }
    const key = t.slice(2);
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) { out[key] = next; i++; }
    else out[key] = true;
  }
  return out;
}

// ---------------------------------------------------------------------------
// Git log
// ---------------------------------------------------------------------------

function getCommits(args) {
  const gitArgs = ["git", "log", "--oneline", "--no-decorate"];

  if (args.from && args.to) {
    gitArgs.push(`${args.from}..${args.to}`);
  } else if (args.from) {
    gitArgs.push(`${args.from}..HEAD`);
  } else if (args.since) {
    gitArgs.push(`--since="${args.since}"`);
  } else {
    gitArgs.push(`-n`, `${args.count || 20}`);
  }

  gitArgs.push('--format="%h %s"');

  const log = execSync(gitArgs.join(" "), { encoding: "utf8" }).trim();
  if (!log) return { lines: [], first: null, last: null };

  const lines = log.split("\n");
  const first = lines[lines.length - 1].split(" ")[0]; // oldest
  const last = lines[0].split(" ")[0]; // newest
  return { lines, first, last };
}

// ---------------------------------------------------------------------------
// Format
// ---------------------------------------------------------------------------

function formatBody(lines) {
  return lines.map((l) => {
    const space = l.indexOf(" ");
    const hash = l.slice(0, space);
    const msg = l.slice(space + 1);
    return `${hash} ${msg}`;
  }).join("\n");
}

// ---------------------------------------------------------------------------
// Short code (same logic as generate-short-code.mjs)
// ---------------------------------------------------------------------------

const ALPHABET = "bcdfghjklmnpqrstvwxyzaeiou23456789";

function randomCode(len = 3) {
  const bytes = randomBytes(len);
  return Array.from(bytes).map((b) => ALPHABET[b % ALPHABET.length]).join("");
}

async function uniqueCode(collection) {
  for (let i = 0; i < 100; i++) {
    const code = `n${randomCode()}`;
    const exists = await collection.findOne({ code });
    if (!exists) return code;
  }
  throw new Error("Could not generate unique code after 100 attempts");
}

// ---------------------------------------------------------------------------
// ATProto sync
// ---------------------------------------------------------------------------

async function syncToAtproto(db, sub, newsData, refId) {
  const users = db.collection("users");
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    console.log("  No ATProto account for user, skipping PDS sync.");
    return null;
  }

  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({
    identifier: user.atproto.did,
    password: user.atproto.password,
  });

  const record = {
    $type: "computer.aesthetic.news",
    headline: newsData.headline,
    when: newsData.when.toISOString(),
    ref: refId,
  };
  if (newsData.body) record.body = newsData.body;

  const res = await agent.com.atproto.repo.createRecord({
    repo: user.atproto.did,
    collection: "computer.aesthetic.news",
    record,
  });

  return {
    rkey: res.data.uri.split("/").pop(),
    uri: res.data.uri,
    did: user.atproto.did,
  };
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const dryRun = !!args["dry-run"];

  const { lines, first, last } = getCommits(args);

  if (lines.length === 0) {
    console.error("No commits found.");
    process.exit(1);
  }

  const title = `Commits From ${first} to ${last}`;
  const body = formatBody(lines);

  console.log(`\n${title}`);
  console.log(`${lines.length} commit(s)\n`);
  console.log(body);

  if (dryRun) {
    console.log("\n--dry-run: not posting.");
    return;
  }

  if (!MONGODB_URI) {
    console.error("MONGODB_CONNECTION_STRING not set.");
    process.exit(1);
  }
  if (!ADMIN_SUB) {
    console.error("ADMIN_SUB not set.");
    process.exit(1);
  }

  const client = new MongoClient(MONGODB_URI);
  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);
    const posts = db.collection("news-posts");
    const votes = db.collection("news-votes");

    const code = await uniqueCode(posts);
    const now = new Date();

    const doc = {
      code,
      title,
      url: "",
      text: body,
      user: ADMIN_SUB,
      when: now,
      updated: now,
      score: 1,
      commentCount: 0,
      status: "live",
    };

    await posts.insertOne(doc);
    await votes.insertOne({
      itemType: "post",
      itemId: code,
      user: ADMIN_SUB,
      when: now,
    });

    console.log(`\nPosted to news.aesthetic.computer/${code}`);

    // ATProto sync
    try {
      const atproto = await syncToAtproto(
        db,
        ADMIN_SUB,
        { headline: title, body, when: now },
        doc._id?.toString(),
      );
      if (atproto) {
        await posts.updateOne({ code }, { $set: { atproto } });
        console.log(`  ATProto synced: ${atproto.uri}`);
      }
    } catch (e) {
      console.log(`  ATProto sync failed: ${e.message}`);
    }

    console.log(`  URL: https://news.aesthetic.computer/${code}`);
  } finally {
    await client.close();
  }
}

main().catch((err) => {
  console.error("Failed:", err.message);
  process.exit(1);
});
