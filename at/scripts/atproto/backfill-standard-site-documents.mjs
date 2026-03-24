#!/usr/bin/env node

/**
 * Backfill Standard.site Documents
 *
 * Shallow-copies existing Aesthetic Computer ATProto records into
 * `site.standard.document` records.
 *
 * Default source collections:
 *   - computer.aesthetic.paper
 *   - computer.aesthetic.news
 *   - computer.aesthetic.piece
 *
 * Usage:
 *   node at/scripts/atproto/backfill-standard-site-documents.mjs [options]
 *
 * Options:
 *   --dry-run           Show what would be synced without creating records
 *   --user @handle      Only process a single user's repo
 *   --user-limit N      Only process first N users (for testing/staged rollout)
 *   --sources list      Comma-separated: paper,news,piece,kidlisp,mood
 *   --limit N           Max source records per source collection (per user)
 *   --batch-size N      Pause every N creates (default: 20)
 *   --delay MS          Pause length in milliseconds (default: 300)
 */

import { AtpAgent } from "@atproto/api";
import { connect } from "../../../system/backend/database.mjs";
import { config } from "dotenv";

config({ path: "../../../system/.env" });

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";
const TARGET_COLLECTION = "site.standard.document";
const MAX_PAGE_SIZE = 100;

const SOURCE_CONFIG = {
  paper: {
    collection: "computer.aesthetic.paper",
    toDocument(sourceRecord) {
      const value = sourceRecord.value || {};
      const rkey = rkeyFromUri(sourceRecord.uri);
      const slugRaw = String(value.slug || value.ref || rkey || "").trim();
      const slug = slugRaw.replace(/^\/+/, "");
      const path = slug
        ? slug.endsWith(".pdf")
          ? `/${slug}`
          : `/${slug}.pdf`
        : `/paper/${rkey || Date.now().toString(36)}`;
      const title = truncate(String(value.title || slug || "Untitled Paper"), 5000);

      return {
        site: "https://papers.aesthetic.computer",
        path,
        title,
        description: truncate(
          `Paper from Aesthetic Computer${value.languages?.length ? ` (${value.languages.join(", ")})` : ""}`,
          3000,
        ),
        tags: ["paper", "aesthetic-computer"],
        publishedAt: toIsoString(value.when),
      };
    },
  },
  news: {
    collection: "computer.aesthetic.news",
    toDocument(sourceRecord) {
      const value = sourceRecord.value || {};
      const rkey = rkeyFromUri(sourceRecord.uri);
      const site = "https://news.aesthetic.computer";
      const pathFromLink = pathFromUrlIfSameBase(value.link, site);
      const path = pathFromLink || `/atproto/${rkey || Date.now().toString(36)}`;

      const headline = String(value.headline || "").trim();
      if (!headline) return null;

      const body = String(value.body || "").trim();
      const tags = Array.isArray(value.tags)
        ? value.tags
            .map((tag) => String(tag || "").trim())
            .filter(Boolean)
            .slice(0, 16)
        : [];

      return {
        site,
        path,
        title: truncate(headline, 5000),
        description: truncate(body || `News update: ${headline}`, 3000),
        textContent: body || undefined,
        tags,
        publishedAt: toIsoString(value.when),
      };
    },
  },
  piece: {
    collection: "computer.aesthetic.piece",
    toDocument(sourceRecord) {
      const value = sourceRecord.value || {};
      const slug = String(value.slug || "").trim();
      if (!slug) return null;

      return {
        site: "https://aesthetic.computer",
        path: `/${slug}`,
        title: truncate(slug, 5000),
        description: truncate(`Interactive piece: ${slug}`, 3000),
        tags: ["piece", "interactive"],
        publishedAt: toIsoString(value.when),
      };
    },
  },
  kidlisp: {
    collection: "computer.aesthetic.kidlisp",
    toDocument(sourceRecord) {
      const value = sourceRecord.value || {};
      const code = String(value.code || "").trim();
      if (!code) return null;

      const source = String(value.source || "").trim();
      return {
        site: "https://aesthetic.computer",
        path: `/$${code}`,
        title: truncate(`KidLisp ${code}`, 5000),
        description: truncate(`KidLisp program ${code}`, 3000),
        textContent: source || undefined,
        tags: ["kidlisp", "code"],
        publishedAt: toIsoString(value.when),
      };
    },
  },
  mood: {
    collection: "computer.aesthetic.mood",
    toDocument(sourceRecord) {
      const value = sourceRecord.value || {};
      const mood = String(value.mood || "").trim();
      if (!mood) return null;
      const rkey = rkeyFromUri(sourceRecord.uri);
      const title = truncate(mood.slice(0, 120), 5000);
      return {
        site: "https://aesthetic.computer",
        path: `/mood/${rkey || Date.now().toString(36)}`,
        title,
        description: truncate(mood, 3000),
        textContent: mood,
        tags: ["mood"],
        publishedAt: toIsoString(value.when),
      };
    },
  },
};

function parseArgs(argv) {
  const out = { _: [] };
  for (let i = 0; i < argv.length; i++) {
    const token = argv[i];
    if (!token.startsWith("--")) {
      out._.push(token);
      continue;
    }
    const eq = token.indexOf("=");
    if (eq !== -1) {
      out[token.slice(2, eq)] = token.slice(eq + 1);
      continue;
    }
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) {
      out[token.slice(2)] = next;
      i++;
    } else {
      out[token.slice(2)] = true;
    }
  }
  return out;
}

function toIsoString(value) {
  const date = value instanceof Date ? value : new Date(value || Date.now());
  if (!Number.isNaN(date.getTime())) return date.toISOString();
  return new Date().toISOString();
}

function truncate(value, max) {
  const str = String(value || "");
  if (!max || str.length <= max) return str;
  return str.slice(0, max);
}

function trimTrailingSlash(value) {
  return String(value || "").replace(/\/+$/, "");
}

function pathFromUrlIfSameBase(candidateUrl, baseUrl) {
  if (!candidateUrl) return null;
  try {
    const base = new URL(trimTrailingSlash(baseUrl));
    const candidate = new URL(String(candidateUrl));
    if (candidate.origin !== base.origin) return null;
    const normalizedBasePath = trimTrailingSlash(base.pathname || "");
    const normalizedCandidatePath = candidate.pathname || "/";

    if (
      normalizedBasePath &&
      normalizedBasePath !== "/" &&
      !normalizedCandidatePath.startsWith(normalizedBasePath)
    ) {
      return null;
    }

    const pathname = normalizedCandidatePath.startsWith("/")
      ? normalizedCandidatePath
      : `/${normalizedCandidatePath}`;
    return `${pathname}${candidate.search || ""}${candidate.hash || ""}`;
  } catch {
    return null;
  }
}

function rkeyFromUri(uri) {
  const str = String(uri || "");
  if (!str.includes("/")) return "";
  return str.split("/").pop() || "";
}

async function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function listAllRecords(agent, repo, collection, limit = null) {
  const records = [];
  let cursor;

  while (true) {
    const remaining = limit == null ? MAX_PAGE_SIZE : Math.max(limit - records.length, 0);
    if (remaining <= 0) break;

    const pageLimit = Math.min(MAX_PAGE_SIZE, remaining);
    const response = await agent.com.atproto.repo.listRecords({
      repo,
      collection,
      limit: pageLimit,
      cursor,
    });

    const batch = response.data?.records || [];
    records.push(...batch);

    cursor = response.data?.cursor;
    if (!cursor || batch.length === 0) break;
  }

  return records;
}

async function loadTargetUsers(database, handle, userLimit = null) {
  const users = database.db.collection("users");
  const handles = database.db.collection("@handles");

  if (handle) {
    const clean = handle.replace(/^@/, "");
    const handleDoc = await handles.findOne({ handle: clean });
    if (!handleDoc) {
      throw new Error(`User @${clean} not found`);
    }
    const user = await users.findOne({ _id: handleDoc._id });
    if (!user?.atproto?.did || !user?.atproto?.password) {
      throw new Error(`User @${clean} has no ATProto credentials`);
    }
    return [
      {
        sub: user._id,
        handle: clean,
        did: user.atproto.did,
        password: user.atproto.password,
      },
    ];
  }

  const userDocs = await users
    .find({
      "atproto.did": { $exists: true, $ne: null },
      "atproto.password": { $exists: true, $ne: null },
    })
    .project({ _id: 1, atproto: 1 })
    .toArray();

  const byId = new Map();
  const handleDocs = await handles
    .find({ _id: { $in: userDocs.map((u) => u._id) } })
    .project({ _id: 1, handle: 1 })
    .toArray();

  for (const doc of handleDocs) {
    byId.set(String(doc._id), doc.handle || "unknown");
  }

  const allUsers = userDocs.map((user) => ({
    sub: user._id,
    handle: byId.get(String(user._id)) || "unknown",
    did: user.atproto.did,
    password: user.atproto.password,
  }));

  if (userLimit == null || Number.isNaN(userLimit) || userLimit <= 0) {
    return allUsers;
  }

  return allUsers.slice(0, userLimit);
}

function parseSources(arg) {
  if (!arg) return ["paper", "news", "piece"];
  return String(arg)
    .split(",")
    .map((source) => source.trim().toLowerCase())
    .filter(Boolean);
}

function ensureSourcesValid(sourceNames) {
  const invalid = sourceNames.filter((name) => !SOURCE_CONFIG[name]);
  if (invalid.length > 0) {
    throw new Error(
      `Unknown sources: ${invalid.join(", ")}. Valid sources: ${Object.keys(SOURCE_CONFIG).join(", ")}`,
    );
  }
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const dryRun = Boolean(args["dry-run"]);
  const targetHandle = args.user ? String(args.user) : null;
  const userLimit = args["user-limit"] ? parseInt(args["user-limit"], 10) : null;
  const sources = parseSources(args.sources);
  const limit = args.limit ? parseInt(args.limit, 10) : null;
  const batchSize = args["batch-size"] ? parseInt(args["batch-size"], 10) : 20;
  const delayMs = args.delay ? parseInt(args.delay, 10) : 300;

  ensureSourcesValid(sources);

  console.log("\n🧬 Backfill Standard.site Documents\n");
  console.log(`PDS: ${PDS_URL}`);
  console.log(`Mode: ${dryRun ? "🔍 DRY RUN" : "✍️  LIVE"}`);
  if (targetHandle) console.log(`User: ${targetHandle}`);
  if (!targetHandle && userLimit != null && !Number.isNaN(userLimit)) {
    console.log(`User limit: ${userLimit}`);
  }
  console.log(`Sources: ${sources.join(", ")}`);
  if (limit != null && !Number.isNaN(limit)) console.log(`Limit/source: ${limit}`);
  console.log(`Batch size: ${batchSize}`);
  console.log(`Delay: ${delayMs}ms\n`);

  const database = await connect();

  let usersToProcess;
  try {
    usersToProcess = await loadTargetUsers(database, targetHandle, userLimit);
  } catch (error) {
    await database.disconnect();
    throw error;
  }

  if (usersToProcess.length === 0) {
    console.log("No users with ATProto credentials found.");
    await database.disconnect();
    return;
  }

  const totals = {
    users: usersToProcess.length,
    created: 0,
    skipped: 0,
    failed: 0,
  };

  const bySource = Object.fromEntries(
    sources.map((source) => [source, { created: 0, skipped: 0, failed: 0, scanned: 0 }]),
  );

  for (let u = 0; u < usersToProcess.length; u++) {
    const user = usersToProcess[u];
    console.log(`\n[${u + 1}/${usersToProcess.length}] @${user.handle} (${user.did})`);

    const agent = new AtpAgent({ service: PDS_URL });
    try {
      await agent.login({ identifier: user.did, password: user.password });
    } catch (error) {
      console.log(`  ❌ Login failed: ${error.message}`);
      totals.failed += 1;
      continue;
    }

    let existingDocs = [];
    try {
      existingDocs = await listAllRecords(agent, user.did, TARGET_COLLECTION, null);
    } catch {
      existingDocs = [];
    }

    const existingBySourceUri = new Set(
      existingDocs
        .map((record) => String(record.value?.sourceAtUri || "").trim())
        .filter(Boolean),
    );

    console.log(`  Existing ${TARGET_COLLECTION}: ${existingDocs.length}`);

    for (const sourceName of sources) {
      const source = SOURCE_CONFIG[sourceName];

      let sourceRecords = [];
      try {
        sourceRecords = await listAllRecords(agent, user.did, source.collection, limit);
      } catch {
        sourceRecords = [];
      }

      if (sourceRecords.length === 0) {
        console.log(`  ${sourceName.padEnd(8)} 0 source records`);
        continue;
      }

      console.log(`  ${sourceName.padEnd(8)} scanning ${sourceRecords.length} records`);

      for (let i = 0; i < sourceRecords.length; i++) {
        const sourceRecord = sourceRecords[i];
        bySource[sourceName].scanned += 1;

        const sourceUri = String(sourceRecord.uri || "").trim();
        if (!sourceUri) {
          totals.skipped += 1;
          bySource[sourceName].skipped += 1;
          continue;
        }

        if (existingBySourceUri.has(sourceUri)) {
          totals.skipped += 1;
          bySource[sourceName].skipped += 1;
          continue;
        }

        const mapped = source.toDocument(sourceRecord);
        if (!mapped || !mapped.site || !mapped.title || !mapped.publishedAt) {
          totals.skipped += 1;
          bySource[sourceName].skipped += 1;
          continue;
        }

        const payload = {
          $type: TARGET_COLLECTION,
          ...mapped,
          sourceAtUri: sourceUri,
          sourceCollection: source.collection,
        };

        const ref = sourceRecord.value?.ref;
        if (typeof ref === "string" && ref.trim()) {
          payload.sourceRef = ref.trim();
        }

        if (dryRun) {
          console.log(
            `    [dry] ${sourceName} → ${truncate(mapped.title, 64)} (${mapped.publishedAt.slice(0, 10)})`,
          );
          totals.created += 1;
          bySource[sourceName].created += 1;
          existingBySourceUri.add(sourceUri);
          continue;
        }

        try {
          const result = await agent.com.atproto.repo.createRecord({
            repo: user.did,
            collection: TARGET_COLLECTION,
            record: payload,
          });

          const uri = result.uri || result.data?.uri || "";
          const rkey = rkeyFromUri(uri);
          console.log(`    ✅ ${sourceName} → ${rkey}`);

          totals.created += 1;
          bySource[sourceName].created += 1;
          existingBySourceUri.add(sourceUri);
        } catch (error) {
          console.log(`    ❌ ${sourceName}: ${error.message}`);
          totals.failed += 1;
          bySource[sourceName].failed += 1;
        }

        if ((i + 1) % batchSize === 0 && i < sourceRecords.length - 1) {
          await sleep(delayMs);
        }
      }
    }
  }

  console.log("\n" + "═".repeat(60));
  console.log("Standard.site Sync Summary\n");
  console.log(`Users processed: ${totals.users}`);
  console.log(`Created:         ${totals.created}`);
  console.log(`Skipped:         ${totals.skipped}`);
  console.log(`Failed:          ${totals.failed}\n`);

  for (const sourceName of sources) {
    const stats = bySource[sourceName];
    console.log(
      `${sourceName.padEnd(8)} scanned:${String(stats.scanned).padStart(5)}  created:${String(stats.created).padStart(5)}  skipped:${String(stats.skipped).padStart(5)}  failed:${String(stats.failed).padStart(5)}`,
    );
  }

  if (dryRun) {
    console.log("\n💡 Dry run only. Re-run without --dry-run to create records.");
  }

  console.log();
  await database.disconnect();
}

main()
  .then(() => {
    process.exit(0);
  })
  .catch((error) => {
    console.error(`\n❌ Fatal: ${error.message}`);
    process.exit(1);
  });
