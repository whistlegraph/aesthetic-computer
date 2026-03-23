#!/usr/bin/env node
// at/cli.mjs — Unified CLI for AT Protocol tooling on Aesthetic Computer.
// Usage: node at/cli.mjs <command> [options]
//
// Consolidates scattered AT scripts into one entry point.
// Follows the same pattern as memory/cli.mjs and papers/cli.mjs.

import { config } from "dotenv";
config(); // Load .env from at/ directory

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";
const BSKY_SERVICE =
  process.env.BSKY_SERVICE || "https://public.api.bsky.app";

// ---------------------------------------------------------------------------
// Argument parser (same style as memory/cli.mjs)
// ---------------------------------------------------------------------------

function parseArgs(argv) {
  const out = { _: [] };
  for (let i = 0; i < argv.length; i++) {
    const token = argv[i];
    if (!token.startsWith("--")) {
      out._.push(token);
      continue;
    }
    const eqIdx = token.indexOf("=");
    if (eqIdx !== -1) {
      out[token.slice(2, eqIdx)] = token.slice(eqIdx + 1);
    } else {
      const next = argv[i + 1];
      if (next && !next.startsWith("--")) {
        out[token.slice(2)] = next;
        i++;
      } else {
        out[token.slice(2)] = true;
      }
    }
  }
  return out;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function requireArg(args, position, name) {
  const val = args._[position];
  if (!val) {
    console.error(`Missing required argument: <${name}>`);
    process.exit(1);
  }
  return val;
}

async function fetchJSON(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`HTTP ${res.status}: ${res.statusText}`);
  return res.json();
}

function adminAuth() {
  const pw = process.env.PDS_ADMIN_PASSWORD;
  if (!pw) {
    console.error("PDS_ADMIN_PASSWORD environment variable is required.");
    process.exit(1);
  }
  return `Basic ${Buffer.from(`admin:${pw}`).toString("base64")}`;
}

// ---------------------------------------------------------------------------
// Commands
// ---------------------------------------------------------------------------

async function commandHealth() {
  console.log(`\nPDS Health Check — ${PDS_URL}\n`);

  // HTTP health
  try {
    const res = await fetch(`${PDS_URL}/xrpc/_health`);
    if (res.ok) {
      const data = await res.json();
      console.log(`  HTTP:    OK (version ${data.version || "unknown"})`);
    } else {
      console.log(`  HTTP:    FAIL (${res.status})`);
    }
  } catch (e) {
    console.log(`  HTTP:    FAIL (${e.message})`);
  }

  // Describe server
  try {
    const desc = await fetchJSON(
      `${PDS_URL}/xrpc/com.atproto.server.describeServer`,
    );
    console.log(`  DID:     ${desc.did || "?"}`);
    console.log(
      `  Invite:  ${desc.inviteCodeRequired ? "required" : "open"}`,
    );
    if (desc.availableUserDomains?.length) {
      console.log(`  Domains: ${desc.availableUserDomains.join(", ")}`);
    }
    if (desc.contact?.email) {
      console.log(`  Contact: ${desc.contact.email}`);
    }
  } catch (e) {
    console.log(`  Server:  Could not describe (${e.message})`);
  }

  console.log();
}

async function commandResolve(args) {
  const input = requireArg(args, 1, "handle-or-did");

  let did = input;

  // Resolve handle → DID
  if (!input.startsWith("did:")) {
    console.log(`Resolving handle: ${input}`);
    const profile = await fetchJSON(
      `${BSKY_SERVICE}/xrpc/app.bsky.actor.getProfile?actor=${encodeURIComponent(input)}`,
    );
    did = profile.did;
    console.log(`  @${input} -> ${did}\n`);
  }

  // Fetch DID document
  if (did.startsWith("did:plc:")) {
    const doc = await fetchJSON(`https://plc.directory/${did}`);

    console.log(`DID:       ${did}`);
    if (doc.alsoKnownAs?.length) {
      doc.alsoKnownAs.forEach((aka) => {
        const label = aka.startsWith("at://") ? `@${aka.slice(5)}` : aka;
        console.log(`AKA:       ${label}`);
      });
    }
    if (doc.service?.length) {
      doc.service.forEach((svc) => {
        const star =
          svc.serviceEndpoint.includes("aesthetic.computer") ? " (ours)" : "";
        console.log(`Service:   ${svc.type} -> ${svc.serviceEndpoint}${star}`);
      });
    }
    if (doc.verificationMethod?.length) {
      doc.verificationMethod.forEach((vm) => {
        const key = vm.publicKeyMultibase
          ? vm.publicKeyMultibase.slice(0, 24) + "..."
          : "?";
        console.log(`Key:       ${vm.id} (${key})`);
      });
    }

    if (args.json) {
      console.log(`\n${JSON.stringify(doc, null, 2)}`);
    }
  } else if (did.startsWith("did:web:")) {
    const domain = did.replace("did:web:", "");
    const doc = await fetchJSON(`https://${domain}/.well-known/did.json`);
    console.log(JSON.stringify(doc, null, 2));
  } else {
    console.error(`Unsupported DID method: ${did}`);
  }
  console.log();
}

async function commandProfile(args) {
  const actor = requireArg(args, 1, "handle-or-did");
  const profile = await fetchJSON(
    `${BSKY_SERVICE}/xrpc/app.bsky.actor.getProfile?actor=${encodeURIComponent(actor)}`,
  );
  const p = profile;

  console.log(`\n  Handle:      @${p.handle}`);
  console.log(`  DID:         ${p.did}`);
  console.log(`  Name:        ${p.displayName || "(none)"}`);
  console.log(`  Bio:         ${p.description || "(none)"}`);
  console.log(`  Followers:   ${p.followersCount || 0}`);
  console.log(`  Following:   ${p.followsCount || 0}`);
  console.log(`  Posts:       ${p.postsCount || 0}`);
  if (p.avatar) console.log(`  Avatar:      ${p.avatar}`);
  console.log();
}

async function commandPosts(args) {
  const actor = requireArg(args, 1, "handle-or-did");
  const limit = parseInt(args.limit) || 10;

  const data = await fetchJSON(
    `${BSKY_SERVICE}/xrpc/app.bsky.feed.getAuthorFeed?actor=${encodeURIComponent(actor)}&limit=${limit}`,
  );

  if (!data.feed?.length) {
    console.log("(no posts)");
    return;
  }

  console.log(`\n${data.feed.length} posts from @${actor}:\n`);

  data.feed.forEach((item, i) => {
    const post = item.post;
    const text = post.record?.text || "(no text)";
    const date = new Date(post.indexedAt).toLocaleDateString();
    const likes = post.likeCount || 0;
    const replies = post.replyCount || 0;
    const reposts = post.repostCount || 0;
    console.log(
      `  ${i + 1}. [${date}] ${text.slice(0, 80)}${text.length > 80 ? "..." : ""}`,
    );
    console.log(`     likes:${likes}  replies:${replies}  reposts:${reposts}`);
    console.log(`     ${post.uri}\n`);
  });
}

async function commandPost(args) {
  const text = requireArg(args, 1, "text");

  const identifier = process.env.BSKY_IDENTIFIER;
  const appPassword = process.env.BSKY_APP_PASSWORD;
  const service = process.env.BSKY_SERVICE || "https://bsky.social";

  if (!identifier || !appPassword) {
    console.error(
      "Set BSKY_IDENTIFIER and BSKY_APP_PASSWORD in your environment.",
    );
    process.exit(1);
  }

  const { AtpAgent, RichText } = await import("@atproto/api");
  const agent = new AtpAgent({ service });

  console.log(`Logging in as @${identifier}...`);
  await agent.login({ identifier, password: appPassword });

  const rt = new RichText({ text });
  await rt.detectFacets(agent);

  const postRecord = {
    text: rt.text,
    facets: rt.facets,
    createdAt: new Date().toISOString(),
  };

  // Attach image if provided
  if (args.image) {
    const { readFileSync } = await import("fs");
    const imageData = readFileSync(args.image);
    const { data } = await agent.uploadBlob(imageData, {
      encoding: "image/png",
    });
    postRecord.embed = {
      $type: "app.bsky.embed.images",
      images: [
        { image: data.blob, alt: args.alt || "Image from Aesthetic Computer" },
      ],
    };
    console.log(`Uploaded image: ${args.image}`);
  }

  const response = await agent.post(postRecord);
  const rkey = response.uri.split("/").pop();

  console.log(`Posted: https://bsky.app/profile/${identifier}/post/${rkey}`);
}

async function commandRecords(args) {
  const repo = requireArg(args, 1, "did");
  const collection = requireArg(args, 2, "collection");
  const limit = parseInt(args.limit) || 25;

  const data = await fetchJSON(
    `${PDS_URL}/xrpc/com.atproto.repo.listRecords?repo=${encodeURIComponent(repo)}&collection=${encodeURIComponent(collection)}&limit=${limit}`,
  );

  if (!data.records?.length) {
    console.log(`(no records in ${collection})`);
    return;
  }

  console.log(`\n${data.records.length} records in ${collection}:\n`);

  data.records.forEach((rec, i) => {
    const rkey = rec.uri.split("/").pop();
    const val = rec.value;
    // Show a compact summary depending on type
    const when = val.when || val.createdAt || "";
    const label =
      val.slug || val.code || val.mood || val.headline || val.text || "";
    console.log(
      `  ${i + 1}. ${rkey}  ${label.slice(0, 60)}  ${when ? `(${when.slice(0, 10)})` : ""}`,
    );
  });
  console.log();
}

async function commandLexicons() {
  const { readdirSync, readFileSync } = await import("fs");
  const { join, dirname } = await import("path");
  const { fileURLToPath } = await import("url");

  const __dirname = dirname(fileURLToPath(import.meta.url));
  const lexDir = join(__dirname, "lexicons", "computer", "aesthetic");

  let files;
  try {
    files = readdirSync(lexDir).filter((f) => f.endsWith(".json"));
  } catch {
    console.error(`Lexicon directory not found: ${lexDir}`);
    return;
  }

  console.log(`\nAesthetic Computer Lexicons (${files.length}):\n`);

  for (const file of files) {
    const lex = JSON.parse(readFileSync(join(lexDir, file), "utf8"));
    const main = lex.defs?.main;
    const desc = main?.description || "";
    const required = main?.record?.required || [];
    const props = Object.keys(main?.record?.properties || {});

    console.log(`  ${lex.id}`);
    console.log(`    ${desc}`);
    console.log(`    required: ${required.join(", ") || "(none)"}`);
    console.log(`    fields:   ${props.join(", ")}`);
    console.log();
  }
}

async function commandInvite() {
  const auth = adminAuth();

  const res = await fetch(
    `${PDS_URL}/xrpc/com.atproto.server.createInviteCode`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: auth,
      },
      body: JSON.stringify({ useCount: 1 }),
    },
  );

  if (!res.ok) {
    const text = await res.text();
    console.error(`Failed to create invite: ${res.status} ${text}`);
    process.exit(1);
  }

  const data = await res.json();
  console.log(`Invite code: ${data.code}`);
}

async function commandAccounts(args) {
  const limit = parseInt(args.limit) || 50;

  // listRepos is a public endpoint
  const res = await fetch(
    `${PDS_URL}/xrpc/com.atproto.sync.listRepos?limit=${limit}`,
  );

  if (!res.ok) {
    console.error(`Failed to list accounts: ${res.status}`);
    process.exit(1);
  }

  const data = await res.json();
  printRepos(data);
}

function printRepos(data) {
  const repos = data.repos || [];
  console.log(`\n${repos.length} accounts on PDS:\n`);
  repos.forEach((repo, i) => {
    const active = repo.active !== false ? "" : " (inactive)";
    console.log(
      `  ${String(i + 1).padStart(3)}. ${repo.did}${active}`,
    );
  });
  console.log();
}

async function commandAccountCheck(args) {
  const input = requireArg(args, 1, "handle-or-did");

  // Resolve to DID if needed
  let did = input;
  if (!input.startsWith("did:")) {
    const profile = await fetchJSON(
      `${BSKY_SERVICE}/xrpc/app.bsky.actor.getProfile?actor=${encodeURIComponent(input)}`,
    );
    did = profile.did;
  }

  console.log(`\nAccount check for: ${did}\n`);

  // Check DID document
  try {
    const doc = await fetchJSON(`https://plc.directory/${did}`);
    const pds = doc.service?.find(
      (s) => s.type === "AtprotoPersonalDataServer",
    );
    const handle = doc.alsoKnownAs
      ?.find((a) => a.startsWith("at://"))
      ?.slice(5);

    console.log(`  Handle:  @${handle || "?"}`);
    console.log(`  PDS:     ${pds?.serviceEndpoint || "?"}`);

    if (pds?.serviceEndpoint?.includes("aesthetic.computer")) {
      console.log(`  Ours:    yes`);
    }
  } catch (e) {
    console.log(`  DID doc: failed (${e.message})`);
  }

  // List collections on our PDS
  const collections = [
    "computer.aesthetic.painting",
    "computer.aesthetic.mood",
    "computer.aesthetic.piece",
    "computer.aesthetic.kidlisp",
    "computer.aesthetic.tape",
    "computer.aesthetic.news",
  ];

  console.log(`\n  Records on ${PDS_URL}:`);
  for (const col of collections) {
    try {
      const data = await fetchJSON(
        `${PDS_URL}/xrpc/com.atproto.repo.listRecords?repo=${encodeURIComponent(did)}&collection=${encodeURIComponent(col)}&limit=1`,
      );
      const count = data.records?.length
        ? `${data.records.length}+ records`
        : "0 records";
      console.log(`    ${col.replace("computer.aesthetic.", "")}: ${count}`);
    } catch {
      console.log(
        `    ${col.replace("computer.aesthetic.", "")}: (error or not found)`,
      );
    }
  }
  console.log();
}

async function commandSyncStatus() {
  console.log(`\nSync Status — ${PDS_URL}\n`);

  // Get the art account DID (guest)
  const artDid = "did:plc:tliuubv7lyv2uiknsjbf4ppw";

  const collections = [
    "computer.aesthetic.painting",
    "computer.aesthetic.mood",
    "computer.aesthetic.piece",
    "computer.aesthetic.kidlisp",
    "computer.aesthetic.tape",
    "computer.aesthetic.news",
  ];

  // Check record counts on art account as a quick indicator
  console.log(`  Art account (${artDid}):`);
  for (const col of collections) {
    try {
      const data = await fetchJSON(
        `${PDS_URL}/xrpc/com.atproto.repo.listRecords?repo=${encodeURIComponent(artDid)}&collection=${encodeURIComponent(col)}&limit=100`,
      );
      const name = col.replace("computer.aesthetic.", "");
      const count = data.records?.length || 0;
      const cursor = data.cursor ? " (more available)" : "";
      console.log(`    ${name.padEnd(12)} ${count} records${cursor}`);
    } catch {
      const name = col.replace("computer.aesthetic.", "");
      console.log(`    ${name.padEnd(12)} (error)`);
    }
  }

  // List all repos to get user count
  try {
    const repos = await fetchJSON(
      `${PDS_URL}/xrpc/com.atproto.sync.listRepos?limit=200`,
    );
    const count = repos.repos?.length || 0;
    console.log(`\n  Total accounts: ${count}`);
  } catch {
    console.log(`\n  Total accounts: (could not fetch)`);
  }

  console.log();
}

// ---------------------------------------------------------------------------
// Help
// ---------------------------------------------------------------------------

function printHelp() {
  console.log(`ac-at — AT Protocol CLI for Aesthetic Computer

Usage: ac-at <command> [options]

Query & Inspect:
  health                             PDS health check
  resolve <handle-or-did> [--json]   Resolve DID document
  profile <handle-or-did>            Query profile
  posts <handle-or-did> [--limit=N]  Query posts
  records <did> <collection> [--limit=N]  List records
  lexicons                           Show AC custom lexicon schemas

Publish:
  post <text> [--image=path] [--alt=text]  Post to Bluesky

Admin (requires PDS_ADMIN_PASSWORD):
  invite                             Generate PDS invite code
  accounts [--limit=N]               List PDS accounts
  account:check <handle-or-did>      Inspect account & record counts
  sync:status                        Record counts across collections

Environment:
  PDS_URL              PDS endpoint (default: https://at.aesthetic.computer)
  PDS_ADMIN_PASSWORD   Admin password for PDS operations
  BSKY_IDENTIFIER      Bluesky handle for posting
  BSKY_APP_PASSWORD    Bluesky app password for posting
  BSKY_SERVICE         Bluesky API (default: https://public.api.bsky.app)

Examples:
  ac-at health
  ac-at resolve aesthetic.computer
  ac-at profile jeffrey.at.aesthetic.computer
  ac-at posts aesthetic.computer --limit=5
  ac-at records did:plc:k3k3wknzkcnekbnyde4dbatz computer.aesthetic.painting
  ac-at post "Hello from AC!" --image=painting.png
  ac-at invite
  ac-at account:check jeffrey.at.aesthetic.computer
  ac-at sync:status
`);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

const COMMANDS = {
  health: commandHealth,
  resolve: commandResolve,
  profile: commandProfile,
  posts: commandPosts,
  post: commandPost,
  records: commandRecords,
  lexicons: commandLexicons,
  invite: commandInvite,
  accounts: commandAccounts,
  "account:check": commandAccountCheck,
  "sync:status": commandSyncStatus,
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
  console.error(`ac-at: ${err.message}`);
  process.exit(1);
});
