// Track Media, 25.10.09
// Formerly `Painting`
// POST: Create a new record in the database for a user uploaded painting or piece.
// Now generates short codes for ALL paintings (user and guest)

/* #region 🏁 TODO 
  - [] Eventually add metadata to paintings... like titles.
#endregion */

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { customAlphabet } from 'nanoid';
import { createMediaRecord, deleteMediaRecord, MediaTypes } from "../../backend/media-atproto.mjs";

const dev = process.env.CONTEXT === "dev";

// Code generator - lowercase preferred (3x) but uppercase included for more space
const consonants = 'bcdfghjklmnpqrstvwxyz' + 'bcdfghjklmnpqrstvwxyz' + 'BCDFGHJKLMNPQRSTVWXYZ';
const vowels = 'aeiou' + 'aeiou' + 'AEIOU';
const numbers = '23456789'; // Exclude 0,1 (look like O,l)
const alphabet = consonants + vowels + numbers;
const CODE_LENGTH = 3;
const nanoid = customAlphabet(alphabet, CODE_LENGTH);
const MAX_COLLISION_ATTEMPTS = 100;

async function generateUniqueCode(collection) {
  for (let attempt = 0; attempt < MAX_COLLISION_ATTEMPTS; attempt++) {
    const code = nanoid();
    
    // Check if code already exists
    const existing = await collection.findOne({ code });
    if (!existing) {
      return code;
    }
    
    console.log(`⚠️  Code collision detected: ${code}, retrying...`);
  }
  
  // If we hit max attempts, use a longer code
  const longerNanoid = customAlphabet(alphabet, CODE_LENGTH + 1);
  const longerCode = longerNanoid();
  console.log(`⚠️  Max collisions reached, using longer code: ${longerCode}`);
  return longerCode;
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
      console.log(`🔓 Guest upload (no authorization): ${authError.message}`);
    }

    const database = await connect();

    let type;
    if (body.ext === "png") {
      type = "paintings";
    } else if (body.ext === "mjs") {
      type = "pieces";
    } else {
      throw new Error(`Unsupported media type via extension: ${body.ext}`);
    }

    const collection = database.db.collection(type);

    if (event.httpMethod === "POST") {
      // POST logic for creating a new database record
      const slug = body.slug;
      
      // Create indexes (safe to call multiple times)
      await collection.createIndex({ code: 1 }, { unique: true });
      await collection.createIndex({ user: 1 }, { sparse: true }); // sparse: only index docs with user field
      await collection.createIndex({ when: 1 });
      await collection.createIndex({ slug: 1 });
      if (user) {
        await collection.createIndex({ slug: 1, user: 1 }, { unique: true });
      }

      // Generate unique short code
      const code = await generateUniqueCode(collection);
      
      try {
        const paintingDate = new Date();
        const record = {
          code,          // NEW: short code for #abc lookups
          slug,
          when: paintingDate,
          bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
        };
        
        // Only add user field if authenticated (keep undefined for guests)
        if (user) {
          record.user = user.sub;
        }
        
        const insertResult = await collection.insertOne(record);
        
        const logType = user ? "user" : "guest";
        console.log(`✅ Created ${logType} ${type.slice(0, -1)}: slug=${slug}, code=${code}`);
        
        // Sync to ATProto in background (don't wait for it)
        const mediaType = type === "paintings" ? MediaTypes.PAINTING : MediaTypes.PIECE;
        const paintingId = insertResult.insertedId;
        
        // Fetch the full record for ATProto sync
        const savedRecord = await collection.findOne({ _id: paintingId });
        
        if (savedRecord) {
          createMediaRecord(database, mediaType, savedRecord, { 
            userSub: user?.sub 
          })
          .then(result => {
            if (result.error) {
              console.error(`⚠️  ATProto sync failed: ${result.error}`);
            } else {
              console.log(`✅ Synced to ATProto: ${result.rkey}`);
              // Update MongoDB with rkey (fire and forget)
              collection.updateOne(
                { _id: paintingId },
                { $set: { "atproto.rkey": result.rkey } }
              ).catch(err => console.error(`⚠️  Failed to update rkey: ${err.message}`));
            }
          })
          .catch(err => console.error(`⚠️  ATProto sync error: ${err.message}`));
        }
        
        // Return code and paintingId to client immediately
        return respond(200, { 
          slug, 
          code,
          paintingId: paintingId.toString()
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
