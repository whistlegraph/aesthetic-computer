// Rename video files from short slug format to full timestamp format
// Based on the "when" field in the tapes database collection

import { S3Client, CopyObjectCommand, DeleteObjectCommand } from "@aws-sdk/client-s3";
import { connect } from "../system/backend/database.mjs";

const s3 = new S3Client({
  endpoint: "https://sfo3.digitaloceanspaces.com",
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.SPACES_KEY,
    secretAccessKey: process.env.SPACES_SECRET,
  },
});

const userId = "auth0|63effeeb2a7d55f8098d62f9";
const bucket = "user-aesthetic-computer";

// Map of short slugs to rename
const shortSlugs = ["2025.254", "2025.353", "2025.373", "2025.384", "2025.447", "2025.740", "2025.770", "2025.855"];

async function main() {
  const { db, disconnect } = await connect();
  
  console.log("Finding tapes with short slugs...\n");
  
  const tapes = await db.collection("tapes").find({
    slug: { $in: shortSlugs },
    user: userId
  }).toArray();
  
  for (const tape of tapes) {
    const when = new Date(tape.when);
    const fullSlug = when.toISOString()
      .replace(/[-:]/g, ".")
      .replace("T", ".")
      .replace("Z", "")
      .split(".").slice(0, 7).join(".");
    
    const tapeUserId = tape.user || userId;
    const oldKey = `${tapeUserId}/video/${tape.slug}.zip`;
    const newKey = `${tapeUserId}/video/${fullSlug}.zip`;
    
    console.log(`Tape ${tape.code} (${tape.slug}):`);
    console.log(`  Old: ${oldKey}`);
    console.log(`  New: ${newKey}`);
    
    try {
      // Copy to new location
      await s3.send(new CopyObjectCommand({
        Bucket: bucket,
        CopySource: `${bucket}/${oldKey}`,
        Key: newKey,
      }));
      console.log(`  ✓ Copied`);
      
      // Delete old file
      await s3.send(new DeleteObjectCommand({
        Bucket: bucket,
        Key: oldKey,
      }));
      console.log(`  ✓ Deleted old file`);
      
      // Update database with new slug
      await db.collection("tapes").updateOne(
        { _id: tape._id },
        { $set: { slug: fullSlug } }
      );
      console.log(`  ✓ Updated database\n`);
      
    } catch (error) {
      console.error(`  ✗ Error: ${error.message}\n`);
    }
  }
  
  await disconnect();
  console.log("Done!");
}

main().catch(console.error);
