// Migration script to add short codes to existing paintings
// Run with: npm run migrate:dry (or migrate, migrate:user, migrate:guest)

import { connect } from "../system/backend/database.mjs";
import { generateUniqueCode } from "../system/backend/painting-codes/generate-code.mjs";
import { codeExists } from "../system/backend/painting-codes/lookup.mjs";
import { S3Client, ListObjectsV2Command } from "@aws-sdk/client-s3";

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const userOnly = args.includes("--user-only");
const guestOnly = args.includes("--guest-only");

console.log("🎨 Painting Short Code Migration");
console.log("================================\n");

if (dryRun) {
  console.log("🔍 DRY RUN MODE - No changes will be made\n");
}

// S3 Client setup
const s3Client = new S3Client({
  region: process.env.DO_SPACES_REGION || "sfo3",
  endpoint: process.env.DO_SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com",
  credentials: {
    accessKeyId: process.env.DO_SPACES_KEY || process.env.ART_KEY,
    secretAccessKey: process.env.DO_SPACES_SECRET || process.env.ART_SECRET,
  },
});

async function listGuestPaintings() {
  const bucket = process.env.ART_SPACE_NAME || "art-aesthetic-computer";
  const paintings = [];
  let continuationToken;

  console.log(`📦 Listing guest paintings from bucket: ${bucket}`);

  do {
    const command = new ListObjectsV2Command({
      Bucket: bucket,
      ContinuationToken: continuationToken,
    });

    const response = await s3Client.send(command);

    if (response.Contents) {
      for (const item of response.Contents) {
        if (item.Key.endsWith(".png")) {
          paintings.push({
            key: item.Key,
            slug: item.Key.replace(".png", ""),
            lastModified: item.LastModified,
            size: item.Size,
          });
        }
      }
    }

    continuationToken = response.NextContinuationToken;
  } while (continuationToken);

  console.log(`   Found ${paintings.length} guest paintings\n`);
  return paintings;
}

async function migrateUserPaintings(database) {
  if (guestOnly) {
    console.log("⏭️  Skipping user paintings (--guest-only flag)\n");
    return { processed: 0, updated: 0, skipped: 0 };
  }

  console.log("👤 Migrating USER paintings");
  console.log("---------------------------");

  const paintings = database.db.collection("paintings");

  // Find all user paintings without codes
  const userPaintings = await paintings
    .find({
      user: { $exists: true },
      code: { $exists: false },
    })
    .toArray();

  console.log(`   Found ${userPaintings.length} user paintings without codes`);

  let processed = 0;
  let updated = 0;
  let skipped = 0;

  for (const painting of userPaintings) {
    processed++;

    try {
      // Generate unique code
      const code = await generateUniqueCode(codeExists);

      if (!dryRun) {
        await paintings.updateOne(
          { _id: painting._id },
          {
            $set: {
              code,
              bucket: "user-aesthetic-computer",
              type: "user",
            },
          }
        );
        updated++;
      }

      if (processed % 100 === 0 || dryRun) {
        console.log(`   [${processed}/${userPaintings.length}] ${dryRun ? "Would assign" : "Assigned"} code: ${code} → ${painting.slug}`);
      }
    } catch (error) {
      console.error(`   ❌ Error processing ${painting.slug}:`, error.message);
      skipped++;
    }
  }

  console.log(`\n   ✅ Processed: ${processed}`);
  console.log(`   ${dryRun ? "📝 Would update" : "💾 Updated"}: ${updated}`);
  if (skipped > 0) console.log(`   ⚠️  Skipped: ${skipped}`);
  console.log();

  return { processed, updated, skipped };
}

async function migrateGuestPaintings(database) {
  if (userOnly) {
    console.log("⏭️  Skipping guest paintings (--user-only flag)\n");
    return { processed: 0, created: 0, skipped: 0 };
  }

  console.log("🎭 Migrating GUEST paintings");
  console.log("----------------------------");

  const paintings = database.db.collection("paintings");

  // Get list of guest paintings from S3
  const s3Paintings = await listGuestPaintings();

  let processed = 0;
  let created = 0;
  let skipped = 0;

  for (const s3Painting of s3Paintings) {
    processed++;

    try {
      // Check if painting already exists in database
      const existing = await paintings.findOne({ slug: s3Painting.slug });

      if (existing) {
        // Already has a database record
        if (!existing.code) {
          // Needs a code
          const code = await generateUniqueCode(codeExists);

          if (!dryRun) {
            await paintings.updateOne(
              { _id: existing._id },
              {
                $set: {
                  code,
                  bucket: "art-aesthetic-computer",
                  type: "guest",
                },
              }
            );
            created++;
          }

          if (processed % 100 === 0 || dryRun) {
            console.log(`   [${processed}/${s3Paintings.length}] ${dryRun ? "Would assign" : "Assigned"} code: ${code} → ${s3Painting.slug}`);
          }
        } else {
          skipped++;
        }
      } else {
        // Create new database record
        const code = await generateUniqueCode(codeExists);

        if (!dryRun) {
          await paintings.insertOne({
            code,
            slug: s3Painting.slug,
            // user: undefined, // Don't set user field for guest paintings
            when: s3Painting.lastModified,
            nuked: false,
            bucket: "art-aesthetic-computer",
            type: "guest",
          });
          created++;
        }

        if (processed % 100 === 0 || dryRun) {
          console.log(`   [${processed}/${s3Paintings.length}] ${dryRun ? "Would create" : "Created"} record with code: ${code} → ${s3Painting.slug}`);
        }
      }
    } catch (error) {
      console.error(`   ❌ Error processing ${s3Painting.slug}:`, error.message);
      skipped++;
    }
  }

  console.log(`\n   ✅ Processed: ${processed}`);
  console.log(`   ${dryRun ? "📝 Would create" : "💾 Created"}: ${created}`);
  if (skipped > 0) console.log(`   ⚠️  Skipped: ${skipped}`);
  console.log();

  return { processed, created, skipped };
}

async function run() {
  const database = await connect();

  try {
    const startTime = Date.now();

    // Migrate user paintings
    const userResults = await migrateUserPaintings(database);

    // Migrate guest paintings
    const guestResults = await migrateGuestPaintings(database);

    const endTime = Date.now();
    const duration = ((endTime - startTime) / 1000).toFixed(2);

    console.log("📊 Migration Summary");
    console.log("===================");
    console.log(`   User paintings: ${userResults.updated} ${dryRun ? "would be updated" : "updated"}`);
    console.log(`   Guest paintings: ${guestResults.created} ${dryRun ? "would be created" : "created"}`);
    console.log(`   Total processed: ${userResults.processed + guestResults.processed}`);
    console.log(`   Duration: ${duration}s`);

    if (dryRun) {
      console.log(`\n💡 Run without --dry-run to apply changes`);
    } else {
      console.log(`\n✅ Migration complete!`);
    }
  } finally {
    await database.disconnect();
  }
}

run().catch((error) => {
  console.error("❌ Migration failed:", error);
  process.exit(1);
});
