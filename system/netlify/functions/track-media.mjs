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
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";

const dev = process.env.CONTEXT === "dev";

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
      
      // Ensure indexes exist
      const existingIndexes = await collection.indexes();
      const indexNames = existingIndexes.map(idx => idx.name);
      
      if (!indexNames.includes('code_1')) {
        await collection.createIndex({ code: 1 }, { unique: true, sparse: true });
      }
      if (!indexNames.includes('user_1')) {
        await collection.createIndex({ user: 1 }, { sparse: true });
      }
      if (!indexNames.includes('when_1')) {
        await collection.createIndex({ when: 1 });
      }
      if (!indexNames.includes('slug_1')) {
        await collection.createIndex({ slug: 1 });
      }
      if (user && !indexNames.includes('slug_1_user_1')) {
        await collection.createIndex({ slug: 1, user: 1 }, { unique: true });
      }
      
      // Generate unique short code (random mode for paintings/pieces)
      const code = await generateUniqueCode(collection, { 
        mode: 'random',
        type: type === 'paintings' ? 'painting' : 'piece'
      });
      
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
        
        // Automatically sync to ATProto for ALL paintings (paintings only)
        // Guest paintings use the system art.at.aesthetic.computer account
        if (type === "paintings") {
          try {
            const { createPaintingOnAtproto } = await import("../../backend/painting-atproto.mjs");
            const atprotoType = user ? "user" : "guest";
            console.log(`🔄 Auto-syncing ${atprotoType} painting to ATProto: ${slug}`);
            
            // Use /media/paintings/{code} for all paintings
            // This endpoint handles code lookup and redirects to the actual file
            const imageUrl = `https://aesthetic.computer/media/paintings/${code}.png`;
            
            // For recordings, use /media/paintings/{code}.zip
            // The endpoint will look up the code and return the associated recording ZIP
            let recordingUrl = null;
            if (slug.includes(":")) {
              recordingUrl = `https://aesthetic.computer/media/paintings/${code}.zip`;
            }
            
            const atprotoResult = await createPaintingOnAtproto(
              database, // Pass full database object, not database.db
              user?.sub, // undefined for guest paintings
              slug,
              code,
              imageUrl,
              recordingUrl,
              paintingDate,
              insertResult.insertedId.toString(),
              record.bucket
            );
            
            if (atprotoResult.rkey) {
              // Update the painting record with ATProto info
              await collection.updateOne(
                { _id: insertResult.insertedId },
                { $set: { atproto: { rkey: atprotoResult.rkey, uri: atprotoResult.uri } } }
              );
              console.log(`✅ ATProto sync successful: ${atprotoResult.rkey}`);
            } else if (atprotoResult.error) {
              console.warn(`⚠️  ATProto sync failed: ${atprotoResult.error}`);
            }
          } catch (atprotoError) {
            console.error(`⚠️  ATProto sync error: ${atprotoError.message}`);
            // Don't fail the request if ATProto sync fails
          }
        }
        
        // Return code and paintingId to client
        return respond(200, { 
          slug, 
          code,
          paintingId: insertResult.insertedId.toString()
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

        // Handle ATProto sync for paintings
        if (type === "paintings") {
          if (nuke && painting.atproto?.rkey) {
            // Nuking: delete from ATProto
            console.log("🔄 Deleting painting from ATProto...");
            try {
              const { deletePaintingFromAtproto } = await import("../../backend/painting-atproto.mjs");
              await deletePaintingFromAtproto(
                database,
                user.sub,
                painting.atproto.rkey
              );
              console.log("✅ Painting deleted from ATProto");
            } catch (atprotoError) {
              console.error("⚠️  Failed to delete painting from ATProto:", atprotoError);
              // Continue anyway - MongoDB is already updated
            }
          } else if (!nuke && !painting.atproto?.rkey) {
            // Denuking: re-add to ATProto if it doesn't exist
            console.log("🔄 Re-adding painting to ATProto after denuke...");
            try {
              const { createPaintingOnAtproto } = await import("../../backend/painting-atproto.mjs");
              
              // Use /media/paintings/{code} for images
              const imageUrl = `https://aesthetic.computer/media/paintings/${painting.code}.png`;
              
              // For recordings, use /media/paintings/{code}.zip
              let recordingUrl = null;
              if (painting.slug.includes(":")) {
                recordingUrl = `https://aesthetic.computer/media/paintings/${painting.code}.zip`;
              }
              
              const atprotoResult = await createPaintingOnAtproto(
                database,
                user.sub,
                painting.slug,
                painting.code,
                imageUrl,
                recordingUrl,
                painting.when,
                painting._id.toString(),
                painting.bucket
              );
              
              if (atprotoResult.rkey) {
                // Update MongoDB with new ATProto info
                await collection.updateOne(
                  { _id: painting._id },
                  { $set: { atproto: { rkey: atprotoResult.rkey, uri: atprotoResult.uri } } }
                );
                console.log(`✅ Re-added to ATProto: ${atprotoResult.rkey}`);
              }
            } catch (atprotoError) {
              console.error("⚠️  Failed to re-add painting to ATProto:", atprotoError);
              // Continue anyway - MongoDB is already updated
            }
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
