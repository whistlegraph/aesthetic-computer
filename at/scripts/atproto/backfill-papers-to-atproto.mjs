#!/usr/bin/env node

/**
 * Backfill Papers to ATProto
 *
 * Creates computer.aesthetic.paper records on the PDS for each paper
 * in the PAPER_MAP. Papers are published under @jeffrey's PDS account.
 *
 * Usage:
 *   node backfill-papers-to-atproto.mjs [options]
 *
 * Options:
 *   --dry-run    Show what would be created without making changes
 *   --limit N    Only process N papers
 */

import { AtpAgent } from "@atproto/api";
import { readFileSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { connect } from "../../../system/backend/database.mjs";
import { config } from "dotenv";

config({ path: "../../../system/.env" });

const __dirname = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = join(__dirname, "../../../system/public/papers.aesthetic.computer");
const METADATA_PATH = join(__dirname, "../../../papers/metadata.json");

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";
const PAPERS_BASE_URL = "https://papers.aesthetic.computer";
const COLLECTION = "computer.aesthetic.paper";

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const limitIndex = args.indexOf("--limit");
const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : null;

// Same PAPER_MAP from papers/cli.mjs
const PAPER_MAP = {
  "arxiv-ac": { siteName: "aesthetic-computer-26-arxiv", title: "Aesthetic Computer '26" },
  "arxiv-api": { siteName: "piece-api-26-arxiv", title: "From setup() to boot()" },
  "arxiv-archaeology": { siteName: "repo-archaeology-26-arxiv", title: "Repository Archaeology" },
  "arxiv-dead-ends": { siteName: "dead-ends-26-arxiv", title: "Vestigial Features" },
  "arxiv-diversity": { siteName: "citation-diversity-audit-26", title: "Citation Diversity Audit" },
  "arxiv-goodiepal": { siteName: "radical-computer-art-26-arxiv", title: "Radical Computer Art" },
  "arxiv-kidlisp": { siteName: "kidlisp-26-arxiv", title: "KidLisp '26" },
  "arxiv-kidlisp-reference": { siteName: "kidlisp-reference-26-arxiv", title: "KidLisp Language Reference" },
  "arxiv-network-audit": { siteName: "network-audit-26-arxiv", title: "Network Audit" },
  "arxiv-notepat": { siteName: "notepat-26-arxiv", title: "notepat.com" },
  "arxiv-os": { siteName: "ac-native-os-26-arxiv", title: "AC Native OS" },
  "arxiv-pieces": { siteName: "pieces-not-programs-26-arxiv", title: "Pieces Not Programs" },
  "arxiv-sustainability": { siteName: "who-pays-for-creative-tools-26-arxiv", title: "Who Pays for Creative Tools?" },
  "arxiv-whistlegraph": { siteName: "whistlegraph-26-arxiv", title: "Whistlegraph" },
  "arxiv-plork": { siteName: "plorking-the-planet-26-arxiv", title: "PLOrk'ing the Planet" },
  "arxiv-folk-songs": { siteName: "folk-songs-26-arxiv", title: "Playable Folk Songs" },
  "arxiv-complex": { siteName: "sucking-on-the-complex-26-arxiv", title: "Sucking on the Complex" },
  "arxiv-kidlisp-cards": { siteName: "kidlisp-cards-26-arxiv", title: "KidLisp Cards" },
  "arxiv-score-analysis": { siteName: "reading-the-score-26-arxiv", title: "Reading the Score" },
  "arxiv-calarts": { siteName: "calarts-callouts-papers-26-arxiv", title: "CalArts, Callouts, and Papers" },
  "arxiv-open-schools": { siteName: "open-schools-26-arxiv", title: "Get Closed Source Out of Schools" },
  "arxiv-futures": { siteName: "five-years-from-now-26-arxiv", title: "Five Years from Now" },
};

const LANGS = ["en", "da", "es", "zh"];

async function backfillPapers() {
  console.log("📄 Backfill Papers to ATProto\n");
  console.log(`Mode: ${dryRun ? "🔍 DRY RUN" : "✍️  LIVE"}`);
  console.log(`PDS: ${PDS_URL}`);
  console.log();

  // Load metadata
  const metadata = JSON.parse(readFileSync(METADATA_PATH, "utf8"));

  // Get jeffrey's PDS credentials
  const database = await connect();
  const users = database.db.collection("users");
  const handles = database.db.collection("@handles");

  // Find jeffrey's auth0 ID
  const jeffreyHandle = await handles.findOne({ handle: "jeffrey" });
  if (!jeffreyHandle) {
    console.error("❌ Could not find @jeffrey handle");
    await database.disconnect();
    process.exit(1);
  }

  const jeffreyUser = await users.findOne({ _id: jeffreyHandle._id });
  if (!jeffreyUser?.atproto?.did || !jeffreyUser?.atproto?.password) {
    console.error("❌ @jeffrey has no ATProto credentials");
    await database.disconnect();
    process.exit(1);
  }

  const did = jeffreyUser.atproto.did;
  console.log(`Publishing as: ${did} (@jeffrey)\n`);

  // Login to PDS
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({ identifier: did, password: jeffreyUser.atproto.password });

  // Check existing records
  let existingRkeys = new Set();
  try {
    let cursor;
    do {
      const res = await agent.com.atproto.repo.listRecords({
        repo: did,
        collection: COLLECTION,
        limit: 100,
        cursor,
      });
      for (const r of res.data.records || []) {
        existingRkeys.add(r.value.ref || r.value.slug);
      }
      cursor = res.data.cursor;
    } while (cursor);
  } catch {
    // Collection may not exist yet
  }

  console.log(`Existing paper records on PDS: ${existingRkeys.size}\n`);

  const entries = Object.entries(PAPER_MAP);
  const toProcess = limit ? entries.slice(0, limit) : entries;

  let synced = 0, skipped = 0, failed = 0;

  for (let i = 0; i < toProcess.length; i++) {
    const [dir, paper] = toProcess[i];
    const meta = metadata[dir];
    const pdfPath = join(PAPERS_DIR, `${paper.siteName}.pdf`);
    const pdfExists = existsSync(pdfPath);

    // Detect available languages
    const langs = LANGS.filter((l) => {
      if (l === "en") return pdfExists;
      return existsSync(join(PAPERS_DIR, `${paper.siteName}-${l}.pdf`));
    });

    const pdfUrl = `${PAPERS_BASE_URL}/${paper.siteName}.pdf`;

    // Skip if already on PDS
    if (existingRkeys.has(dir)) {
      console.log(`  [${i + 1}/${toProcess.length}] ⏭️  ${paper.title} (already synced)`);
      skipped++;
      continue;
    }

    if (!pdfExists) {
      console.log(`  [${i + 1}/${toProcess.length}] ⚠️  ${paper.title} (no PDF found)`);
      skipped++;
      continue;
    }

    console.log(
      `  [${i + 1}/${toProcess.length}] ${dryRun ? "Would sync" : "Syncing"}: ${paper.title} [${langs.join(",")}]`,
    );

    if (dryRun) {
      synced++;
      continue;
    }

    try {
      const record = {
        $type: COLLECTION,
        title: paper.title,
        slug: paper.siteName,
        pdfUrl,
        languages: langs,
        revisions: meta?.revisions || 1,
        when: meta?.created
          ? new Date(meta.created + "T00:00:00Z").toISOString()
          : new Date().toISOString(),
        ref: dir,
      };

      const res = await agent.com.atproto.repo.createRecord({
        repo: did,
        collection: COLLECTION,
        record,
      });

      const rkey = (res.uri || res.data?.uri).split("/").pop();
      console.log(`    ✅ → ${rkey}`);
      synced++;
    } catch (error) {
      console.log(`    ❌ ${error.message}`);
      failed++;
    }
  }

  console.log("\n" + "═".repeat(50));
  console.log(`✅ Synced:  ${synced}`);
  console.log(`⏭️  Skipped: ${skipped}`);
  console.log(`❌ Failed:  ${failed}`);
  console.log(`📊 Total:   ${toProcess.length}\n`);

  await database.disconnect();
}

backfillPapers().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
