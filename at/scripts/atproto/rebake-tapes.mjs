#!/usr/bin/env node

/**
 * Re-bake Tapes
 *
 * Re-triggers oven MP4 conversion for tapes that were never processed.
 * Sends each tape to the oven /bake endpoint with the correct ZIP URL.
 *
 * Usage:
 *   node rebake-tapes.mjs [options]
 *
 * Options:
 *   --dry-run          Show what would be sent without triggering
 *   --limit N          Only process N tapes
 *   --code CODE        Re-bake a specific tape by code
 *   --delay MS         Delay between requests (default: 2000)
 */

import { connect } from "../../../system/backend/database.mjs";
import { config } from "dotenv";

config({ path: "../../../system/.env" });

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const limitIndex = args.indexOf("--limit");
const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : null;
const codeIndex = args.indexOf("--code");
const targetCode = codeIndex !== -1 ? args[codeIndex + 1] : null;
const delayIndex = args.indexOf("--delay");
const delay = delayIndex !== -1 ? parseInt(args[delayIndex + 1]) : 2000;

const OVEN_URL = process.env.OVEN_URL || "https://oven.aesthetic.computer";
const CALLBACK_URL = process.env.URL
  ? `${process.env.URL}/api/oven-complete`
  : "https://aesthetic.computer/api/oven-complete";
const CALLBACK_SECRET = process.env.OVEN_CALLBACK_SECRET;

async function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function rebakeTapes() {
  console.log("🔥 Re-bake Unprocessed Tapes\n");
  console.log(`Mode: ${dryRun ? "🔍 DRY RUN" : "🔥 LIVE"}`);
  console.log(`Oven: ${OVEN_URL}`);
  console.log(`Callback: ${CALLBACK_URL}`);
  console.log(`Secret: ${CALLBACK_SECRET ? "✅ set" : "❌ missing"}`);
  if (targetCode) console.log(`Code: ${targetCode}`);
  console.log();

  if (!CALLBACK_SECRET && !dryRun) {
    console.error("❌ OVEN_CALLBACK_SECRET is required. Set it in system/.env");
    process.exit(1);
  }

  const database = await connect();
  const tapes = database.db.collection("tapes");
  const handles = database.db.collection("@handles");

  const query = {
    $or: [{ mp4Url: { $exists: false } }, { mp4Url: null }],
  };

  if (targetCode) {
    query.code = targetCode;
  }

  const cursor = tapes.find(query).sort({ when: -1 });
  if (limit) cursor.limit(limit);

  const allTapes = await cursor.toArray();
  console.log(`Found ${allTapes.length} tapes needing MP4 conversion\n`);

  if (allTapes.length === 0) {
    console.log("✨ All tapes already have MP4s!");
    await database.disconnect();
    return;
  }

  let sent = 0,
    skipped = 0,
    failed = 0;

  for (let i = 0; i < allTapes.length; i++) {
    const tape = allTapes[i];
    const handle = await handles.findOne({ _id: tape.user });
    const h = handle?.handle || (tape.user ? "unknown" : "guest");

    const bucket = tape.bucket || (tape.user ? "user-aesthetic-computer" : "art-aesthetic-computer");
    const key = tape.user ? `${tape.user}/${tape.slug}.zip` : `${tape.slug}.zip`;
    const zipUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`;

    console.log(
      `  [${i + 1}/${allTapes.length}] @${h} code:${tape.code} → ${zipUrl.split("/").pop()}`,
    );

    if (dryRun) {
      sent++;
      continue;
    }

    const payload = {
      mongoId: tape._id.toString(),
      slug: tape.slug,
      code: tape.code,
      zipUrl,
      callbackUrl: CALLBACK_URL,
      callbackSecret: CALLBACK_SECRET,
    };

    try {
      const res = await fetch(`${OVEN_URL}/bake`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      });

      if (res.ok) {
        const data = await res.json();
        console.log(`    ✅ Accepted: ${data.message || "ok"}`);
        sent++;
      } else {
        const text = await res.text();
        console.log(`    ❌ ${res.status}: ${text}`);
        failed++;
      }
    } catch (err) {
      console.log(`    ❌ ${err.message}`);
      failed++;
    }

    if (i < allTapes.length - 1) {
      await sleep(delay);
    }
  }

  console.log("\n" + "═".repeat(50));
  console.log(`🔥 Sent:    ${sent}`);
  console.log(`⏭️  Skipped: ${skipped}`);
  console.log(`❌ Failed:  ${failed}`);
  console.log(`📊 Total:   ${allTapes.length}\n`);

  await database.disconnect();
}

rebakeTapes().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
