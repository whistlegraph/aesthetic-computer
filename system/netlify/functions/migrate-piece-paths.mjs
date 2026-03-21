// Migrate Piece Paths, 2026.02.12
// One-time migration to simplify anonymous piece S3 paths
// Changes slug from "YYYY/MM/DD/code" to just "code" for anonymous pieces

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { S3Client, CopyObjectCommand, PutObjectAclCommand } from "@aws-sdk/client-s3";

export async function handler(event, context) {
  console.log(`🔧 Migration request: ${event.httpMethod} ${event.path || event.rawUrl || 'unknown'}`);

  if (event.httpMethod === 'OPTIONS') {
    return respond(204, '');
  }

  if (event.httpMethod !== 'POST') {
    return respond(405, { error: 'Method not allowed' });
  }

  // Require admin authorization
  let user;
  try {
    user = await authorize(event.headers);
    console.log(`👤 User authorized: ${user.sub}`);
  } catch (error) {
    return respond(401, { error: 'Unauthorized - admin access required' });
  }

  // Check if user has admin privileges
  const isAdmin = await hasAdmin(user);
  if (!isAdmin) {
    console.log(`❌ User ${user.sub} is not an admin`);
    return respond(403, { error: 'Forbidden - admin access required' });
  }

  console.log(`✅ Admin access confirmed for ${user.sub}`);

  let database;
  try {
    database = await connect();
  } catch (connectError) {
    console.error('❌ MongoDB connection failed:', connectError.message);
    return respond(503, { error: 'Database temporarily unavailable' });
  }

  try {
    const collection = database.db.collection('pieces');

    // Find all pieces that need S3 files copied (have oldSlug field)
    const piecesToMigrate = await collection.find({
      needsS3Copy: true,
      oldSlug: { $exists: true }
    }).toArray();

    console.log(`📦 Found ${piecesToMigrate.length} pieces to migrate`);

    if (piecesToMigrate.length === 0) {
      await database.disconnect();
      return respond(200, {
        message: 'No pieces need migration',
        migrated: 0
      });
    }

    const s3Client = new S3Client({
      endpoint: `https://sfo3.digitaloceanspaces.com`,
      region: 'us-east-1',
      credentials: {
        accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
        secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
      },
    });

    const results = {
      total: piecesToMigrate.length,
      migrated: 0,
      skipped: 0,
      errors: []
    };

    for (const piece of piecesToMigrate) {
      try {
        const oldSlug = piece.slug;
        const newSlug = piece.code;  // Just use the code!
        const bucket = piece.bucket || "art-aesthetic-computer";

        console.log(`🔄 Migrating ${piece.code}: ${oldSlug} → ${newSlug}`);

        // Copy S3 file from old path to new path
        const copyCommand = new CopyObjectCommand({
          Bucket: bucket,
          Key: `${newSlug}.mjs`,
          CopySource: `${bucket}/${oldSlug}.mjs`,
          ContentType: 'text/javascript',
        });

        await s3Client.send(copyCommand);
        console.log(`✅ Copied S3: ${oldSlug}.mjs → ${newSlug}.mjs`);

        // Make the new file publicly readable
        const aclCommand = new PutObjectAclCommand({
          Bucket: bucket,
          Key: `${newSlug}.mjs`,
          ACL: "public-read",
        });

        await s3Client.send(aclCommand);

        // Update database record to mark S3 copy complete
        await collection.updateOne(
          { _id: piece._id },
          {
            $set: {
              s3MigratedAt: new Date()
            },
            $unset: {
              needsS3Copy: ""  // Remove flag after successful copy
            }
          }
        );

        console.log(`💾 Updated database for ${piece.code}`);
        results.migrated++;

      } catch (error) {
        console.error(`❌ Failed to migrate ${piece.code}:`, error.message);
        results.errors.push({
          code: piece.code,
          error: error.message
        });
      }
    }

    await database.disconnect();

    return respond(200, {
      message: 'Migration complete',
      ...results
    });

  } catch (error) {
    console.error('❌ Migration error:', error);
    await database.disconnect();
    return respond(500, { error: 'Migration failed', details: error.message });
  }
}
