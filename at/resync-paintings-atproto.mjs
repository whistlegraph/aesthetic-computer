// Sync Media to ATProto, 25.10.22
// POST: Sync media (paintings, pieces, tapes) to ATProto after it's been created in MongoDB
// This is called by track-media after successful database insert
// Runs as a separate endpoint to avoid ESM/native module conflicts in edge functions

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { createPaintingOnAtproto } from "../../backend/painting-atproto.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const body = JSON.parse(event.body);
    const { slug, code, mediaId, bucket, type } = body;

    if (!slug || !code || !mediaId || !type) {
      return respond(400, { message: "slug, code, mediaId, and type required" });
    }

    // Must be authenticated
    const user = await authorize(event.headers);
    const database = await connect();

    try {
      // Determine collection based on media type
      let collectionName;
      let mediaPath;
      
      if (type === "paintings") {
        collectionName = "paintings";
        mediaPath = "painting";
      } else if (type === "pieces") {
        collectionName = "pieces";
        mediaPath = "piece";
      } else {
        // For now, only paintings sync to ATProto
        console.log(`ℹ️  Media type ${type} doesn't sync to ATProto yet`);
        return respond(200, { skipped: true, reason: `${type} sync not implemented` });
      }
      
      // Get the user's handle for constructing the image URL
      const handle = await getHandleOrEmail(user.sub);
      const cleanHandle = handle?.replace('@', '') || user.sub;
      
      // Construct the media URL (uses media edge function)
      const ext = type === "paintings" ? "png" : "mjs";
      const mediaUrl = `https://aesthetic.computer/media/@${cleanHandle}/${mediaPath}/${slug}.${ext}`;
      
      // Get media record from MongoDB
      const collection = database.db.collection(collectionName);
      const media = await collection.findOne({ slug, user: user.sub });
      
      if (!media) {
        return respond(404, { message: `${type} not found` });
      }

      const mediaDate = media.when;

      // Only sync paintings to ATProto for now
      if (type === "paintings") {
        console.log("🔄 Syncing painting to ATProto...");
        const atprotoResult = await createPaintingOnAtproto(
          database,
          user.sub,
          slug,
          code,
          mediaUrl,
          mediaDate,
          mediaId,
          bucket || "user-aesthetic-computer"
        );

        if (atprotoResult.rkey) {
          // Update MongoDB with the ATProto rkey
          await collection.updateOne(
            { slug, user: user.sub },
            { $set: { atproto: { rkey: atprotoResult.rkey } } }
          );
          console.log("✅ Painting synced to ATProto with rkey:", atprotoResult.rkey);
          return respond(200, { rkey: atprotoResult.rkey });
        } else {
          console.log("ℹ️  ATProto sync skipped:", atprotoResult.error);
          return respond(200, { skipped: true, reason: atprotoResult.error });
        }
      }

      return respond(200, { skipped: true, reason: "Type not supported yet" });
    } catch (error) {
      console.error(`⚠️  Failed to sync ${type} to ATProto:`, error);
      return respond(500, { message: error.message || String(error) });
    } finally {
      await database.disconnect();
    }
  } catch (error) {
    console.error("❌ Sync media ATProto error:", error);
    return respond(400, { message: error.message || "Cannot parse input body." });
  }
}
