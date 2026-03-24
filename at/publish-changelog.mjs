#!/usr/bin/env node

/**
 * Publish a changelog post to change.pckt.blog
 *
 * Creates a site.standard.document record in the aesthetic.computer PDS
 * using the blog.pckt.content block format.
 *
 * Usage:
 *   node at/publish-changelog.mjs "Title" "Body text in markdown-ish format"
 *   node at/publish-changelog.mjs --from-commits 5    # last 5 commits
 *   node at/publish-changelog.mjs --file changelog.md  # from a file
 *   echo "content" | node at/publish-changelog.mjs "Title" --stdin
 *
 * Manage posts:
 *   node at/publish-changelog.mjs --list
 *   node at/publish-changelog.mjs --delete <rkey>
 *
 * Body supports a simple subset:
 *   ## Heading        → blog.pckt.block.heading (level 2)
 *   ### Heading       → blog.pckt.block.heading (level 3)
 *   **bold text**     → blog.pckt.richtext.facet#bold
 *   *italic text*     → blog.pckt.richtext.facet#italic
 *   [text](url)       → blog.pckt.richtext.facet#link
 *   blank line        → paragraph break
 */

import { AtpAgent } from "@atproto/api";
import { config } from "dotenv";
import { readFileSync } from "fs";
import { execSync } from "child_process";

config({ path: new URL(".env", import.meta.url) });

const BSKY_SERVICE = process.env.BSKY_SERVICE || "https://bsky.social";
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER;
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD;

// The publication record URI for change.pckt.blog
const PUBLICATION_URI =
  "at://did:plc:k3k3wknzkcnekbnyde4dbatz/site.standard.publication/3mhrqpf4rqykh";

// ---------------------------------------------------------------------------
// Markdown-ish → pckt blocks
// ---------------------------------------------------------------------------

function parseInline(text) {
  // Extract bold, italic, and link facets from a line of text.
  // Returns { plaintext, facets }
  const facets = [];
  let plain = "";

  // Regex order matters — bold before italic (** before *)
  // We process left-to-right, replacing markup and tracking byte offsets.
  const tokens =
    text.match(/\*\*[^*]+\*\*|\*[^*]+\*|\[[^\]]+\]\([^)]+\)|[^*[\]]+/g) || [];

  for (const tok of tokens) {
    const byteStart = Buffer.byteLength(plain, "utf8");

    if (tok.startsWith("**") && tok.endsWith("**")) {
      const inner = tok.slice(2, -2);
      plain += inner;
      facets.push({
        index: {
          byteStart,
          byteEnd: byteStart + Buffer.byteLength(inner, "utf8"),
        },
        features: [{ $type: "blog.pckt.richtext.facet#bold" }],
      });
    } else if (tok.startsWith("*") && tok.endsWith("*")) {
      const inner = tok.slice(1, -1);
      plain += inner;
      facets.push({
        index: {
          byteStart,
          byteEnd: byteStart + Buffer.byteLength(inner, "utf8"),
        },
        features: [{ $type: "blog.pckt.richtext.facet#italic" }],
      });
    } else if (tok.startsWith("[")) {
      const m = tok.match(/^\[([^\]]+)\]\(([^)]+)\)$/);
      if (m) {
        const [, linkText, url] = m;
        plain += linkText;
        facets.push({
          index: {
            byteStart,
            byteEnd: byteStart + Buffer.byteLength(linkText, "utf8"),
          },
          features: [{ $type: "blog.pckt.richtext.facet#link", uri: url }],
        });
      } else {
        plain += tok;
      }
    } else {
      plain += tok;
    }
  }

  return { plaintext: plain, facets };
}

function markdownToBlocks(md) {
  const lines = md.split("\n");
  const items = [];
  let paragraph = [];

  function flushParagraph() {
    if (paragraph.length === 0) return;
    const text = paragraph.join(" ").trim();
    paragraph = [];
    if (!text) return;

    const { plaintext, facets } = parseInline(text);
    const block = { $type: "blog.pckt.block.text", plaintext };
    if (facets.length > 0) block.facets = facets;
    items.push(block);
  }

  for (const line of lines) {
    const trimmed = line.trim();

    // Heading
    const headingMatch = trimmed.match(/^(#{1,6})\s+(.+)$/);
    if (headingMatch) {
      flushParagraph();
      const level = headingMatch[1].length;
      items.push({
        $type: "blog.pckt.block.heading",
        level,
        plaintext: headingMatch[2],
      });
      continue;
    }

    // Blank line = paragraph break
    if (trimmed === "") {
      flushParagraph();
      continue;
    }

    // Bullet points — prefix with bullet, treat as text
    if (trimmed.startsWith("- ") || trimmed.startsWith("* ")) {
      flushParagraph();
      const bulletText = "\u2022 " + trimmed.slice(2);
      const { plaintext, facets } = parseInline(bulletText);
      const block = { $type: "blog.pckt.block.text", plaintext };
      if (facets.length > 0) block.facets = facets;
      items.push(block);
      continue;
    }

    paragraph.push(trimmed);
  }

  flushParagraph();
  return items;
}

// ---------------------------------------------------------------------------
// Generate changelog from git commits
// ---------------------------------------------------------------------------

function changelogFromCommits(count = 10) {
  const log = execSync(
    `git log --oneline --no-decorate -n ${count} --format="%h %s"`,
    { encoding: "utf8", cwd: process.env.INIT_CWD || process.cwd() },
  ).trim();

  const lines = log.split("\n");
  const date = new Date().toISOString().slice(0, 10);
  let md = `## Changelog ${date}\n\n`;
  for (const line of lines) {
    md += `- ${line}\n`;
  }
  return md;
}

// ---------------------------------------------------------------------------
// Slug generation
// ---------------------------------------------------------------------------

function slugify(title) {
  const slug = title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
  // Append short random suffix like pckt does
  const rand = Math.random().toString(36).slice(2, 9);
  return `/${slug}-${rand}`;
}

// ---------------------------------------------------------------------------
// Auth helper
// ---------------------------------------------------------------------------

async function login() {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error("Missing BSKY_IDENTIFIER or BSKY_APP_PASSWORD in at/.env");
    process.exit(1);
  }
  const agent = new AtpAgent({ service: BSKY_SERVICE });
  await agent.login({
    identifier: BSKY_IDENTIFIER,
    password: BSKY_APP_PASSWORD,
  });
  return agent;
}

// ---------------------------------------------------------------------------
// List posts
// ---------------------------------------------------------------------------

async function listPosts() {
  const agent = await login();
  let cursor;
  const all = [];

  do {
    const res = await agent.com.atproto.repo.listRecords({
      repo: agent.session.did,
      collection: "site.standard.document",
      limit: 100,
      cursor,
    });
    // Only include posts belonging to our change publication
    for (const rec of res.data.records) {
      if (rec.value.site === PUBLICATION_URI) all.push(rec);
    }
    cursor = res.data.cursor;
  } while (cursor);

  if (all.length === 0) {
    console.log("No posts on change.pckt.blog yet.");
    return;
  }

  console.log(`\nchange.pckt.blog — ${all.length} post(s)\n`);
  for (const rec of all) {
    const rkey = rec.uri.split("/").pop();
    const date = rec.value.publishedAt?.slice(0, 10) || "?";
    console.log(`  ${date}  ${rkey}  ${rec.value.title}`);
  }
  console.log(`\nTo delete: node at/publish-changelog.mjs --delete <rkey>`);
}

// ---------------------------------------------------------------------------
// Delete a post
// ---------------------------------------------------------------------------

async function deletePost(rkey) {
  const agent = await login();

  // Verify it exists and belongs to our publication
  try {
    const rec = await agent.com.atproto.repo.getRecord({
      repo: agent.session.did,
      collection: "site.standard.document",
      rkey,
    });
    if (rec.data.value.site !== PUBLICATION_URI) {
      console.error("That record doesn't belong to change.pckt.blog.");
      process.exit(1);
    }
    console.log(`Deleting: "${rec.data.value.title}" (${rkey})`);
  } catch {
    console.error(`Record not found: ${rkey}`);
    process.exit(1);
  }

  await agent.com.atproto.repo.deleteRecord({
    repo: agent.session.did,
    collection: "site.standard.document",
    rkey,
  });

  console.log("Deleted.");
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const args = process.argv.slice(2);

  // Parse flags
  const flagIdx = (f) => args.indexOf(f);

  if (flagIdx("--list") !== -1) {
    return listPosts();
  }

  if (flagIdx("--delete") !== -1) {
    const rkey = args[flagIdx("--delete") + 1];
    if (!rkey) {
      console.error("Usage: --delete <rkey>  (use --list to find rkeys)");
      process.exit(1);
    }
    return deletePost(rkey);
  }

  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error("Missing BSKY_IDENTIFIER or BSKY_APP_PASSWORD in at/.env");
    process.exit(1);
  }

  let title, body;

  if (flagIdx("--from-commits") !== -1) {
    const n = parseInt(args[flagIdx("--from-commits") + 1]) || 10;
    title = args[0] && !args[0].startsWith("--") ? args[0] : `Changelog`;
    body = changelogFromCommits(n);
  } else if (flagIdx("--file") !== -1) {
    const filePath = args[flagIdx("--file") + 1];
    const content = readFileSync(filePath, "utf8");
    // First line starting with # is the title, rest is body
    const lines = content.split("\n");
    const titleLine = lines.findIndex((l) => l.startsWith("# "));
    if (titleLine !== -1) {
      title = lines[titleLine].replace(/^#+\s*/, "");
      body = lines
        .slice(titleLine + 1)
        .join("\n")
        .trim();
    } else {
      title = args[0] || "Update";
      body = content;
    }
  } else if (flagIdx("--stdin") !== -1) {
    title = args[0] || "Update";
    body = readFileSync("/dev/stdin", "utf8");
  } else {
    title = args[0];
    body = args[1];
    if (!title) {
      console.error(
        `Usage:
  node at/publish-changelog.mjs "Title" "Body text"
  node at/publish-changelog.mjs --from-commits 5
  node at/publish-changelog.mjs --file changelog.md
  echo "text" | node at/publish-changelog.mjs "Title" --stdin
  node at/publish-changelog.mjs --list
  node at/publish-changelog.mjs --delete <rkey>`,
      );
      process.exit(1);
    }
    if (!body) {
      body = "";
    }
  }

  const items = markdownToBlocks(body);
  const path = slugify(title);
  const now = new Date().toISOString();

  console.log(`Publishing to change.pckt.blog...`);
  console.log(`  Title: ${title}`);
  console.log(`  Path:  ${path}`);
  console.log(`  Blocks: ${items.length}`);

  const agent = await login();
  console.log(`  Authenticated as @${BSKY_IDENTIFIER}`);

  const record = {
    $type: "site.standard.document",
    site: PUBLICATION_URI,
    title,
    path,
    publishedAt: now,
    content: {
      $type: "blog.pckt.content",
      items,
    },
    textContent: items
      .map((b) => b.plaintext || "")
      .filter(Boolean)
      .join("\n\n"),
    tags: ["changelog"],
  };

  const res = await agent.com.atproto.repo.createRecord({
    repo: agent.session.did,
    collection: "site.standard.document",
    record,
  });

  console.log(`\nPublished!`);
  console.log(`  URI: ${res.data.uri}`);
  console.log(`  CID: ${res.data.cid}`);
  console.log(`  URL: https://change.pckt.blog${path}`);
}

main().catch((err) => {
  console.error("Failed:", err.message);
  process.exit(1);
});
