// Track Media, 25.10.09
// Formerly `Painting`
// POST: Create a new record in the database for user uploaded media (paintings, pieces, tapes).
// Now generates short codes for ALL media (user and guest)

/* #region 🏁 TODO 
  - [] Eventually add metadata to paintings... like titles.
  + [x] Merged track-tape into track-media for code reuse
#endregion */

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { createMediaRecord, deleteMediaRecord, MediaTypes } from "../../backend/media-atproto.mjs";
import { S3Client, PutObjectAclCommand } from "@aws-sdk/client-s3";

const dev = process.env.CONTEXT === "dev";

// Duration limit for tape background processing (in seconds)
const MAX_TAPE_DURATION = 30;

/**
 * Invoke Netlify Background Function for MP4 conversion (tapes only)
 * @param {Object} options - Conversion job parameters
 * @returns {Promise<void>}
 */
async function invokeBackgroundConversion({ mongoId, slug, zipUrl, metadata }) {
  console.log(`🎬 Invoking background conversion for tape ${slug} (${mongoId})`);
  
  try {
    // Netlify background functions are invoked by calling them via /api/ route
    let baseUrl = process.env.URL || 'https://localhost:8888';
    const backgroundUrl = `${baseUrl}/api/tape-convert-background`;
    
    console.log(`🎬 Background function URL: ${backgroundUrl}`);
    
    // For local dev with self-signed certs, we need to disable SSL verification
    const isLocalhost = baseUrl.includes('localhost');
    const originalRejectUnauthorized = process.env.NODE_TLS_REJECT_UNAUTHORIZED;
    
    if (isLocalhost) {
      process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';
    }
    
    const response = await fetch(backgroundUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        mongoId,
        slug,
        zipUrl,
        metadata,
      }),
    });
    
    // Restore original setting
    if (isLocalhost) {
      if (originalRejectUnauthorized === undefined) {
        delete process.env.NODE_TLS_REJECT_UNAUTHORIZED;
      } else {
        process.env.NODE_TLS_REJECT_UNAUTHORIZED = originalRejectUnauthorized;
      }
    }
    
    console.log(`📡 Background function response: ${response.status} ${response.statusText}`);
    
    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`Background function returned ${response.status}: ${errorText}`);
    }
    
    // Background functions return 202 Accepted when successfully queued
    if (response.status === 202) {
      console.log(`✅ Background conversion queued successfully (202)`);
    } else {
      console.log(`✅ Background conversion invoked (${response.status})`);
    }
    
  } catch (error) {
    console.error(`❌ Failed to invoke background conversion:`, error.message);
    // Don't throw - let the request succeed anyway
    // Conversion can be retried via sync-atproto script
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
    // Netlify normalizes headers to lowercase
    const authHeader = event.headers.authorization || event.headers.Authorization;
    console.log(`🔍 Auth header check:`, { 
      hasLowercase: !!event.headers.authorization, 
      hasUppercase: !!event.headers.Authorization,
      headerKeys: Object.keys(event.headers).filter(k => k.toLowerCase().includes('auth'))
    });
    
    if (authHeader) {
      console.log(`🔑 Authorization header present, attempting to authorize...`);
      console.log(`🔑 Token preview: ${authHeader.substring(0, 50)}...`);
      user = await authorize(event.headers);
      if (!user) {
        // Token validation failed - could be expired or invalid
        // For now, log warning and continue as guest rather than blocking
        console.warn(`⚠️  Authorization failed but allowing guest upload. User should refresh their token.`);
      } else {
        console.log(`✅ Authorized user: ${user.sub}`);
      }
    } else {
      console.log(`🔓 No authorization header, treating as guest upload`);
    }

    const database = await connect();

    let type, metadata;
    if (body.ext === "png") {
      type = "paintings";
    } else if (body.ext === "mjs") {
      type = "pieces";
    } else if (body.ext === "zip") {
      type = "tapes";
      metadata = body.metadata || {};
      
      // Validate tape duration
      const duration = metadata.totalDuration || 0;
      console.log(`⏱️  Tape duration: ${duration}s, Max: ${MAX_TAPE_DURATION}s`);
      if (duration > MAX_TAPE_DURATION) {
        return respond(400, { 
          error: "TAPE_TOO_LONG",
          message: `Tape duration ${duration}s exceeds maximum ${MAX_TAPE_DURATION}s`,
          maxDuration: MAX_TAPE_DURATION
        });
      }
    } else {
      throw new Error(`Unsupported media type via extension: ${body.ext}`);
    }

    const collection = database.db.collection(type);

    if (event.httpMethod === "POST") {
      // POST logic for creating a new database record
      const slug = body.slug;
      
      // Create indexes (safe to call multiple times)
      // Note: Tapes already have code_1 index without sparse, so skip for tapes
      try {
        if (type !== 'tapes') {
          await collection.createIndex({ code: 1 }, { unique: true, sparse: true });
        } else {
          // Tapes collection already has { code: 1 } unique index (not sparse)
          // Just ensure it exists without sparse option
          await collection.createIndex({ code: 1 }, { unique: true });
        }
        await collection.createIndex({ user: 1 }); // No sparse - matches existing index
        await collection.createIndex({ when: 1 });
        await collection.createIndex({ slug: 1 });
        if (user) {
          await collection.createIndex({ slug: 1, user: 1 }, { unique: true });
        }
      } catch (indexError) {
        // Index already exists with different options - that's okay
        console.log(`ℹ️  Index creation skipped (already exists): ${indexError.message}`);
      }

      // Generate unique short code (random mode for tapes)
      const code = await generateUniqueCode(collection, { 
        mode: type === 'tapes' ? 'random' : undefined,
        type: type === 'tapes' ? 'tape' : undefined
      });
      
      try {
        const paintingDate = new Date();
        const record = {
          code,
          slug,
          when: paintingDate,
          bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
        };
        
        // Tape-specific fields
        if (type === 'tapes') {
          record.nuked = false;
          record.mp4Status = "pending"; // Track conversion status
        }
        
        // Only add user field if authenticated (keep undefined for guests)
        if (user) {
          record.user = user.sub;
        }
        
        const insertResult = await collection.insertOne(record);
        
        const logType = user ? "user" : "guest";
        console.log(`✅ Created ${logType} ${type.slice(0, -1)}: slug=${slug}, code=${code}`);
        
        const mediaId = insertResult.insertedId;
        
        // Handle tapes differently - background MP4 conversion
        if (type === 'tapes') {
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
            console.error(`⚠️  Failed to set ACL:`, aclError.message);
          }
          
          // Invoke background function for MP4 conversion (fire-and-forget)
          invokeBackgroundConversion({
            mongoId: mediaId.toString(),
            slug: record.slug,
            zipUrl: `https://${record.bucket}.sfo3.digitaloceanspaces.com/${user ? user.sub + '/' : ''}${slug}.zip`,
            metadata: metadata,
          }).catch(err => {
            console.error('❌ Background conversion invocation error:', err);
          });
          
          return respond(200, { 
            slug, 
            code,
            maxDuration: MAX_TAPE_DURATION
          });
        }
        
        // Paintings/Pieces: Sync to ATProto immediately
        console.log("🔄 Syncing to ATProto...");
        try {
          const mediaType = type === "paintings" ? MediaTypes.PAINTING : MediaTypes.PIECE;
          
          // Fetch the full record for ATProto sync
          const savedRecord = await collection.findOne({ _id: mediaId });
          
          if (savedRecord) {
            const atprotoResult = await createMediaRecord(database, mediaType, savedRecord, { 
              userSub: user?.sub || null  // null for anonymous -> uses art-guest account
            });
            
            if (atprotoResult.error) {
              console.error(`⚠️  ATProto sync failed: ${atprotoResult.error}`);
            } else if (atprotoResult.rkey) {
              // Update MongoDB with rkey
              await collection.updateOne(
                { _id: mediaId },
                { $set: { "atproto.rkey": atprotoResult.rkey } }
              );
              console.log(`✅ Synced to ATProto: ${atprotoResult.rkey}`);
            }
          }
        } catch (atprotoError) {
          // Log the error but don't fail the request - media is already saved
          console.error(`⚠️  Failed to sync to ATProto:`, atprotoError);
        }
        
        // Return code and mediaId to client
        return respond(200, { 
          slug, 
          code,
          paintingId: mediaId.toString()
        });
      } catch (error) {
        console.error(`❌ Failed to insert ${type.slice(0, -1)}:`, error);
        return respond(500, { message: error.message || String(error) });
      } finally {
        await database.disconnect();
      }
      
    } else if (event.httpMethod === "PUT") {
      // PUT logic for updating an existing painting record
      const { slug, nuke } = body;
      if (!slug || nuke === undefined || nuke === null) {
        return respond(400, { message: "Slug & nuke must be set for update." });
      }

      // User must be authenticated to nuke their own content
      if (!user) {
        return respond(401, { message: "Unauthorized - login required to nuke media" });
      }

      try {
        // First, get the painting to check if it has an atproto rkey
        const painting = await collection.findOne({ slug, user: user.sub });
        
        if (!painting) {
          return respond(404, { message: "Media not found." });
        }

        // Update the nuked status in MongoDB
        const result = await collection.updateOne(
          { slug, user: user.sub },
          { $set: { nuked: nuke } },
        );

        // If nuking (not un-nuking) and has ATProto record, delete it
        if (nuke && painting.atproto?.rkey) {
          console.log("🔄 Deleting from ATProto...");
          try {
            const mediaType = type === "paintings" ? MediaTypes.PAINTING : MediaTypes.PIECE;
            await deleteMediaRecord(
              database,
              mediaType,
              user.sub,
              painting.atproto.rkey
            );
            console.log("✅ Deleted from ATProto");
          } catch (atprotoError) {
            console.error("⚠️  Failed to delete from ATProto:", atprotoError);
            // Continue anyway - MongoDB is already updated
          }
        }

        if (result.modifiedCount === 1) {
          return respond(200, { message: "Media nuked successfully." });
        } else {
          return respond(200, { message: "No effect." });
        }
      } catch (error) {
        console.error(`❌ Failed to nuke media:`, error);
        return respond(500, { message: error.message || String(error) });
      } finally {
        await database.disconnect();
      }
    }
  } catch (error) {
    console.error(`❌ Track media error:`, error);
    return respond(400, { message: error.message || "Cannot parse input body." });
  }
}
