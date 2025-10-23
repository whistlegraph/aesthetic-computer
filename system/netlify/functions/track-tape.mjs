// Track Tape, 25.10.17
// POST: Create a new record in the database for a user uploaded tape (video recording).
// Generates short codes for ALL tapes (user and guest)

/* #region 🏁 TODO 
  - [] Implement external MP4 conversion microservice
  - [] Add retry logic for failed conversions
#endregion */

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { S3Client, PutObjectAclCommand } from "@aws-sdk/client-s3";

const dev = process.env.CONTEXT === "dev";

// MP4 conversion service endpoint (to be implemented)
const MP4_SERVICE_URL = process.env.MP4_SERVICE_URL || "https://tape-converter.aesthetic.computer/convert";

/**
 * Queue MP4 conversion job (fire-and-forget async call)
 * @param {Object} options - Conversion job parameters
 * @returns {Promise<void>}
 */
async function queueMP4Conversion({ mongoId, bucket, slug, zipUrl, callbackUrl, metadata }) {
  console.log(`🎬 Queueing MP4 conversion for tape ${slug} (${mongoId})`);
  
  try {
    const response = await fetch(MP4_SERVICE_URL, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${process.env.MP4_SERVICE_TOKEN || ""}`,
      },
      body: JSON.stringify({
        mongoId,
        bucket,
        slug,
        zipUrl,
        callbackUrl,
        metadata, // Includes frameCount, totalDuration, timing data
      }),
    });
    
    if (!response.ok) {
      throw new Error(`MP4 service returned ${response.status}: ${await response.text()}`);
    }
    
    const result = await response.json();
    console.log(`✅ MP4 conversion queued: job=${result.jobId || 'unknown'}`);
    
    // Update MongoDB status to "processing"
    const database = await connect();
    const { ObjectId } = await import("mongodb");
    await database.db.collection("tapes").updateOne(
      { _id: new ObjectId(mongoId) },
      { $set: { mp4Status: "processing" } }
    );
    await database.disconnect();
    
  } catch (error) {
    console.error(`❌ Failed to queue MP4 conversion:`, error.message);
    // Don't throw - let the request succeed anyway
    // Conversion can be retried manually or via cron job
  }
}

export async function handler(event, context) {
  if (!["POST", "PUT"].includes(event.httpMethod))
    return respond(405, { message: "Method Not Allowed" });

  let body;
  try {
    body = JSON.parse(event.body);
    
    // Try to authorize user (but don't require it for guest uploads)
    let user;
    try {
      user = await authorize(event.headers);
    } catch (authError) {
      console.log(`🔓 Guest tape upload: ${authError.message}`);
    }

    const database = await connect();
    const tapes = database.db.collection("tapes");

    if (event.httpMethod === "POST") {
      // POST logic for creating a new database record
      const slug = body.slug;
      const metadata = body.metadata || {};
      
      // Create indexes (safe to call multiple times)
      await tapes.createIndex({ code: 1 }, { unique: true });
      await tapes.createIndex({ user: 1 }, { sparse: true }); // sparse: only index docs with user field
      await tapes.createIndex({ when: 1 });
      await tapes.createIndex({ slug: 1 });
      if (user) {
        await tapes.createIndex({ slug: 1, user: 1 }, { unique: true });
      }

      // Generate unique short code (random mode for tapes)
      const code = await generateUniqueCode(tapes, { 
        mode: 'random',
        type: 'tape'
      });
      
      try {
        const record = {
          code,
          slug,
          when: new Date(),
          bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
          nuked: false,
          mp4Status: "pending", // Track conversion status
        };
        
        // Only add user field if authenticated (keep undefined for guests)
        if (user) {
          record.user = user.sub;
        }
        
        const result = await tapes.insertOne(record);
        const mongoId = result.insertedId.toString();
        
        const logType = user ? "user" : "guest";
        console.log(`✅ Created ${logType} tape: slug=${slug}, code=${code}, id=${mongoId}`);
        
        // Make the uploaded ZIP publicly readable
        try {
          const s3Client = new S3Client({
            endpoint: `https://sfo3.digitaloceanspaces.com`,
            credentials: {
              accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
              secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
            },
          });
          
          const key = user ? `${user.sub}/${slug}.zip` : `${slug}.zip`;
          
          const aclCommand = new PutObjectAclCommand({
            Bucket: record.bucket,
            Key: key,
            ACL: "public-read",
          });
          
          await s3Client.send(aclCommand);
          console.log(`✅ Set public-read ACL for ${record.bucket}/${key}`);
        } catch (aclError) {
          console.error(`⚠️  Failed to set ACL (file is uploaded but may not be public):`, aclError.message);
          // Don't fail the request - the file is uploaded, just not public yet
        }
        
        // Queue MP4 conversion job (fire-and-forget)
        queueMP4Conversion({
          mongoId,
          bucket: record.bucket,
          slug: record.slug,
          zipUrl: `https://${record.bucket}.sfo3.digitaloceanspaces.com/${user ? user.sub + '/' : ''}${slug}.zip`,
          callbackUrl: `${process.env.URL}/.netlify/functions/tape-mp4-complete`,
          metadata: metadata, // Pass through frame timing data
        }).catch(err => {
          console.error('❌ MP4 conversion queue error:', err);
          // Don't fail the request - conversion can be retried
        });
        
        return respond(200, { slug, code }); // Return code to client!
      } catch (error) {
        console.error(`❌ Failed to insert tape:`, error);
        return respond(500, { message: error.message || String(error) });
      } finally {
        await database.disconnect();
      }
      
    } else if (event.httpMethod === "PUT") {
      // PUT logic for updating an existing tape record (e.g., nuking)
      const { slug, nuke } = body;
      if (!slug || nuke === undefined || nuke === null) {
        return respond(400, { message: "Slug & nuke must be set for update." });
      }

      // User must be authenticated to nuke their own content
      if (!user) {
        return respond(401, { message: "Unauthorized - login required to nuke media" });
      }

      try {
        const result = await tapes.updateOne(
          { slug, user: user.sub },
          { $set: { nuked: nuke } },
        );

        if (result.matchedCount === 0) {
          return respond(404, { message: "Tape not found." });
        } else if (result.modifiedCount === 1) {
          return respond(200, { message: "Tape nuked successfully." });
        } else {
          return respond(200, { message: "No effect." });
        }
      } catch (error) {
        console.error(`❌ Failed to nuke tape:`, error);
        return respond(500, { message: error.message || String(error) });
      } finally {
        await database.disconnect();
      }
    }
  } catch (error) {
    console.error(`❌ Track tape error:`, error);
    return respond(400, { message: error.message || "Cannot parse input body." });
  }
}
