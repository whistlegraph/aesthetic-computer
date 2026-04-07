#!/usr/bin/env node

/**
 * ac-news — CLI for posting prose updates to news.aesthetic.computer
 *
 * Usage:
 *   ac-news post "Title" "Body prose text"
 *   ac-news post "Title" --file update.md
 *   echo "body" | ac-news post "Title" --stdin
 *   ac-news post "Title" --editor          # opens $EDITOR
 *   ac-news commits                        # show recent commits for reference
 *   ac-news commits --since "1 week ago"
 *   ac-news list                           # list recent posts
 *   ac-news edit <code> --replace "old" --with "new"  # find & replace in body
 *   ac-news edit <code> --editor           # edit body in $EDITOR
 *   ac-news delete <code>                  # delete a post (admin)
 */

import { MongoClient } from "mongodb";
import { AtpAgent } from "@atproto/api";
import { config } from "dotenv";
import { execSync } from "child_process";
import { randomBytes } from "crypto";
import { readFileSync, writeFileSync, unlinkSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";

config({
  path: new URL("../.devcontainer/envs/devcontainer.env", import.meta.url),
});
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
    if (!t.startsWith("--")) {
      out._.push(t);
      continue;
    }
    const key = t.slice(2);
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) {
      out[key] = next;
      i++;
    } else out[key] = true;
  }
  return out;
}

// ---------------------------------------------------------------------------
// Short code (same as publish-commits.mjs)
// ---------------------------------------------------------------------------

const ALPHABET = "bcdfghjklmnpqrstvwxyzaeiou23456789";

function randomCode(len = 3) {
  const bytes = randomBytes(len);
  return Array.from(bytes)
    .map((b) => ALPHABET[b % ALPHABET.length])
    .join("");
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
    console.log("  No ATProto account — skipping PDS sync.");
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
// DB helper
// ---------------------------------------------------------------------------

async function withDb(fn) {
  if (!MONGODB_URI) {
    console.error("MONGODB_CONNECTION_STRING not set.");
    process.exit(1);
  }
  const client = new MongoClient(MONGODB_URI);
  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);
    await fn(db);
  } finally {
    await client.close();
  }
}

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

async function commandCommits(args) {
  const gitArgs = ["git", "log", "--oneline", "--no-decorate"];

  if (args.since) {
    gitArgs.push(`--since="${args.since}"`);
  } else if (args.from) {
    gitArgs.push(args.to ? `${args.from}..${args.to}` : `${args.from}..HEAD`);
  } else {
    gitArgs.push("-n", `${args.count || 20}`);
  }

  gitArgs.push('--format="%h %s"');

  const log = execSync(gitArgs.join(" "), { encoding: "utf8" }).trim();
  if (!log) {
    console.log("No commits found.");
    return;
  }

  const lines = log.split("\n");
  console.log(`\n${lines.length} recent commit(s):\n`);
  for (const line of lines) {
    console.log(`  ${line}`);
  }
  console.log(
    "\nUse these to write your prose update, then post with:\n  ac-news post \"Title\" \"Your prose summary...\"\n",
  );
}

async function commandPost(args) {
  const title = args._[1];
  if (!title) {
    console.error(
      'Usage: ac-news post "Title" "Body"\n' +
        '       ac-news post "Title" --file update.md\n' +
        '       ac-news post "Title" --editor\n' +
        '       echo "body" | ac-news post "Title" --stdin',
    );
    process.exit(1);
  }

  let body;

  if (args.file) {
    body = readFileSync(args.file, "utf8").trim();
  } else if (args.stdin) {
    body = readFileSync("/dev/stdin", "utf8").trim();
  } else if (args.editor) {
    const editor = process.env.EDITOR || "vi";
    const tmpFile = join(tmpdir(), `ac-news-${Date.now()}.md`);
    writeFileSync(tmpFile, "");
    try {
      execSync(`${editor} ${tmpFile}`, { stdio: "inherit" });
      body = readFileSync(tmpFile, "utf8").trim();
    } finally {
      try {
        unlinkSync(tmpFile);
      } catch {}
    }
    if (!body) {
      console.log("Empty body — cancelled.");
      return;
    }
  } else {
    body = args._[2] || "";
  }

  if (!ADMIN_SUB) {
    console.error("ADMIN_SUB not set.");
    process.exit(1);
  }

  const dryRun = !!args["dry-run"];

  console.log(`\n  Title: ${title}`);
  console.log(`  Body:  ${body.length} chars\n`);
  console.log(body);

  if (dryRun) {
    console.log("\n--dry-run: not posting.");
    return;
  }

  await withDb(async (db) => {
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

    console.log(`\nPosted: https://news.aesthetic.computer/${code}`);

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
        console.log(`  ATProto: ${atproto.uri}`);
      }
    } catch (e) {
      console.log(`  ATProto sync failed: ${e.message}`);
    }
  });
}

async function commandList(args) {
  const limit = parseInt(args.limit) || 10;

  await withDb(async (db) => {
    const posts = db.collection("news-posts");
    const items = await posts
      .find({ status: "live" })
      .sort({ when: -1 })
      .limit(limit)
      .toArray();

    if (items.length === 0) {
      console.log("No posts.");
      return;
    }

    console.log(`\n${items.length} recent post(s):\n`);
    for (const item of items) {
      const date = item.when.toISOString().slice(0, 10);
      const comments = item.commentCount || 0;
      const titlePreview =
        item.title.length > 60 ? item.title.slice(0, 60) + "..." : item.title;
      console.log(`  ${item.code}  ${date}  ${titlePreview}  (${comments}c)`);
    }
    console.log();
  });
}

async function commandEdit(args) {
  const code = args._[1];
  if (!code) {
    console.error(
      'Usage: ac-news edit <code> [options]\n' +
        '       ac-news edit ncd2 --title "New Title"\n' +
        '       ac-news edit ncd2 --body "New body text"\n' +
        '       ac-news edit ncd2 --editor            # open $EDITOR with current body\n' +
        '       ac-news edit ncd2 --url "https://..."\n' +
        '       ac-news edit ncd2 --replace "old text" --with "new text"',
    );
    process.exit(1);
  }

  if (!ADMIN_SUB) {
    console.error("ADMIN_SUB not set.");
    process.exit(1);
  }

  const dryRun = !!args["dry-run"];

  await withDb(async (db) => {
    const posts = db.collection("news-posts");
    const post = await posts.findOne({ code });

    if (!post) {
      console.error(`Post not found: ${code}`);
      process.exit(1);
    }

    console.log(`\nEditing: "${post.title}" (${code})`);

    const updates = {};

    // --title "New title"
    if (args.title) {
      updates.title = args.title;
      console.log(`  title → "${args.title}"`);
    }

    // --url "https://..."
    if (args.url !== undefined) {
      updates.url = args.url;
      console.log(`  url → "${args.url}"`);
    }

    // --replace "old" --with "new" (find-and-replace in body text)
    if (args.replace && args.with !== undefined) {
      const oldText = post.text || "";
      const count = oldText.split(args.replace).length - 1;
      if (count === 0) {
        console.error(`  Replace string not found in body: "${args.replace}"`);
        process.exit(1);
      }
      updates.text = oldText.replaceAll(args.replace, args.with);
      console.log(`  body: replaced ${count} occurrence(s) of "${args.replace}" → "${args.with}"`);
    }

    // --body "Full new body"
    if (args.body) {
      updates.text = args.body;
      console.log(`  body → ${args.body.length} chars`);
    }

    // --editor: open current body in $EDITOR
    if (args.editor) {
      const editor = process.env.EDITOR || "vi";
      const tmpFile = join(tmpdir(), `ac-news-edit-${Date.now()}.md`);
      writeFileSync(tmpFile, post.text || "");
      try {
        execSync(`${editor} ${tmpFile}`, { stdio: "inherit" });
        const newBody = readFileSync(tmpFile, "utf8").trim();
        if (newBody === (post.text || "").trim()) {
          console.log("  No changes made.");
          return;
        }
        updates.text = newBody;
        console.log(`  body → ${newBody.length} chars (via editor)`);
      } finally {
        try { unlinkSync(tmpFile); } catch {}
      }
    }

    if (Object.keys(updates).length === 0) {
      console.log("  Nothing to update. Use --title, --body, --url, --replace, or --editor.");
      return;
    }

    updates.updated = new Date();

    if (dryRun) {
      console.log("\n--dry-run: not saving.");
      if (updates.text) {
        console.log("\nNew body preview:\n");
        console.log(updates.text);
      }
      return;
    }

    await posts.updateOne({ code }, { $set: updates });
    console.log(`\nSaved: https://news.aesthetic.computer/${code}`);
  });
}

async function commandDelete(args) {
  const code = args._[1];
  if (!code) {
    console.error("Usage: ac-news delete <code>");
    process.exit(1);
  }

  if (!ADMIN_SUB) {
    console.error("ADMIN_SUB not set.");
    process.exit(1);
  }

  await withDb(async (db) => {
    const posts = db.collection("news-posts");
    const post = await posts.findOne({ code });

    if (!post) {
      console.error(`Post not found: ${code}`);
      process.exit(1);
    }

    console.log(`Deleting: "${post.title}" (${code})`);
    await posts.updateOne({ code }, { $set: { status: "dead" } });
    console.log("Deleted (marked dead).");
  });
}

// ---------------------------------------------------------------------------
// Screenshot (via oven)
// ---------------------------------------------------------------------------

const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";

async function commandScreenshot(args) {
  const piece = args._[1];
  if (!piece) {
    console.error(
      "Usage: ac-news screenshot <piece>\n" +
        "       ac-news screenshot notepat\n" +
        "       ac-news screenshot notepat --force\n" +
        "       ac-news screenshot @jeffrey/my-piece",
    );
    process.exit(1);
  }

  const force = !!args.force;
  const url = `${OVEN_URL}/news-screenshot/${encodeURIComponent(piece)}.png?json=true${force ? "&force=true" : ""}`;

  console.log(`\n  Capturing ${piece}...`);

  const res = await fetch(url);
  if (!res.ok) {
    const body = await res.json().catch(() => ({}));
    console.error(`  Oven error (${res.status}): ${body.error || res.statusText}`);
    process.exit(1);
  }

  const data = await res.json();
  const mdImage = `![${piece}](${data.url})`;

  console.log(`  ${data.cached ? "Cached" : "Captured"}: ${data.width}×${data.height}`);
  console.log(`  URL: ${data.url}`);
  console.log(`\n  Markdown (paste into post body):\n`);
  console.log(`  ${mdImage}\n`);
}

// ---------------------------------------------------------------------------
// Help
// ---------------------------------------------------------------------------

function printHelp() {
  console.log(`ac-news — Post prose updates to news.aesthetic.computer

Usage: ac-news <command> [options]

Compose:
  commits [--count N] [--since "..."]    Show recent commits for reference
  post "Title" "Body"                    Post a prose update
  post "Title" --file path.md            Post from a markdown file
  post "Title" --editor                  Open $EDITOR to write the body
  post "Title" --stdin                   Read body from stdin
  post ... --dry-run                     Preview without posting

Media:
  screenshot <piece>                     Capture a piece via oven (1200×675 PNG)
  screenshot <piece> --force             Force-regenerate (skip cache)

Manage:
  list [--limit N]                       List recent posts
  edit <code> --title "New Title"        Edit post title
  edit <code> --body "New body"          Replace post body
  edit <code> --editor                   Edit body in $EDITOR
  edit <code> --url "https://..."        Change post URL
  edit <code> --replace "old" --with "new"  Find & replace in body
  edit ... --dry-run                     Preview without saving
  delete <code>                          Delete a post (admin)

Examples:
  ac-news commits --since "1 week ago"
  ac-news post "Dev Update" "The native OS build system got a major overhaul..."
  ac-news post "Weekly Update" --file updates/2026-03-24.md
  ac-news post "What's New" --editor
  ac-news edit ncd2 --replace "https://aesthetic.computer)" --with "https://aesthetic.computer/chat)"
  ac-news screenshot notepat
  ac-news list
`);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

const COMMANDS = {
  commits: commandCommits,
  post: commandPost,
  list: commandList,
  edit: commandEdit,
  delete: commandDelete,
  screenshot: commandScreenshot,
};

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const command = args._[0] || "help";

  if (command === "help" || command === "--help" || command === "-h") {
    printHelp();
    return;
  }

  const handler = COMMANDS[command];
  if (!handler) {
    console.error(`Unknown command: ${command}\n`);
    printHelp();
    process.exitCode = 1;
    return;
  }

  await handler(args);
}

main().catch((err) => {
  console.error(`ac-news: ${err.message}`);
  process.exit(1);
});
