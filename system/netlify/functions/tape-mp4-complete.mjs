// Tape MP4 Complete, 25.10.17
// Webhook handler called when external MP4 conversion completes
// Syncs the tape to ATProto with the MP4 video blob

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { createTapeOnAtproto } from "../../backend/tape-atproto.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body);
  } catch (error) {
    return respond(400, { message: "Invalid JSON body" });
  }

  const { mongoId, mp4Url, status } = body;

  if (!mongoId || !mp4Url) {
    return respond(400, { message: "Missing required fields: mongoId, mp4Url" });
  }

  if (status !== "complete") {
    console.log(`⚠️  Received non-complete status: ${status}`);
    return respond(200, { message: "Status noted" });
  }

  console.log(`🎬 MP4 conversion complete for tape: ${mongoId}`);
  console.log(`📹 MP4 URL: ${mp4Url}`);

  try {
    const database = await connect();
    
    // Update MongoDB with MP4 URL (store in a new field)
    const { ObjectId } = await import("mongodb");
    const tapes = database.db.collection("tapes");
    
    await tapes.updateOne(
      { _id: new ObjectId(mongoId) },
      { $set: { mp4: mp4Url, mp4Status: "complete" } }
    );
    
    console.log(`✅ Updated tape with MP4 URL`);

    // Sync to ATProto (handles guest/no-atproto gracefully)
    const result = await createTapeOnAtproto(database, mongoId, mp4Url);

    await database.disconnect();

    if (result.error) {
      console.log(`ℹ️  ATProto sync skipped: ${result.error}`);
      return respond(200, { 
        message: "MP4 stored, ATProto sync skipped",
        reason: result.error 
      });
    }

    console.log(`🦋 ATProto sync complete: ${result.rkey}`);
    return respond(200, { 
      message: "MP4 stored and synced to ATProto",
      rkey: result.rkey 
    });

  } catch (error) {
    console.error(`❌ Webhook handler error:`, error);
    return respond(500, { message: error.message || String(error) });
  }
}
