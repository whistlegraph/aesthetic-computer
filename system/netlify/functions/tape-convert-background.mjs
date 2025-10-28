// Tape Convert Background Function
// Converts tape ZIP to MP4 with video blob, then syncs to ATProto
// Max execution: 15 minutes (Netlify Background Function)

import { connect } from "../../backend/database.mjs";
import { convertTapeToMp4 } from "../../backend/tape-to-mp4.mjs";
import { createTapeOnAtproto } from "../../backend/tape-atproto.mjs";

const MAX_DURATION_SECONDS = 30; // Only process tapes up to 30 seconds

export async function handler(event, context) {
  const { mongoId, slug, zipUrl, metadata } = JSON.parse(event.body);
  
  console.log(`🎬 Background conversion started for tape: ${slug} (${mongoId})`);
  console.log(`📹 ZIP URL: ${zipUrl}`);

  const database = await connect();
  const tapes = database.db.collection("tapes");
  const { ObjectId } = await import("mongodb");

  try {
    // Update status to processing
    await tapes.updateOne(
      { _id: new ObjectId(mongoId) },
      { $set: { mp4Status: "processing", mp4StartedAt: new Date() } }
    );

    // Check duration from metadata
    const duration = metadata?.totalDuration || 0;
    if (duration > MAX_DURATION_SECONDS) {
      throw new Error(`Tape too long: ${duration}s (max ${MAX_DURATION_SECONDS}s)`);
    }

    // Convert ZIP to MP4
    console.log(`🎬 Converting ZIP to MP4...`);
    const result = await convertTapeToMp4(zipUrl);
    const mp4Buffer = result.video;
    const thumbnailBuffer = result.thumbnail;

    const videoSizeKB = (mp4Buffer.length / 1024).toFixed(2);
    console.log(`✅ MP4 ready: ${videoSizeKB} KB`);

    // Update MongoDB with completion status
    await tapes.updateOne(
      { _id: new ObjectId(mongoId) },
      { 
        $set: { 
          mp4Status: "complete",
          mp4CompletedAt: new Date(),
          mp4Size: mp4Buffer.length
        } 
      }
    );

    console.log(`✅ MongoDB updated with MP4 completion`);

    // Sync to ATProto (uploads blob directly to PDS)
    console.log(`🦋 Syncing to ATProto...`);
    const atprotoResult = await createTapeOnAtproto(database, mongoId, mp4Buffer, thumbnailBuffer);

    if (atprotoResult.error) {
      console.log(`ℹ️  ATProto sync skipped: ${atprotoResult.error}`);
      await database.disconnect();
      return {
        statusCode: 200,
        body: JSON.stringify({
          success: true,
          message: "MP4 created, ATProto sync skipped",
          reason: atprotoResult.error
        })
      };
    }

    console.log(`✅ ATProto sync complete: ${atprotoResult.rkey}`);
    await database.disconnect();

    return {
      statusCode: 200,
      body: JSON.stringify({
        success: true,
        message: "MP4 created and synced to ATProto",
        rkey: atprotoResult.rkey
      })
    };

  } catch (error) {
    console.error(`❌ Background conversion failed:`, error);
    
    // Update MongoDB with error
    await tapes.updateOne(
      { _id: new ObjectId(mongoId) },
      { 
        $set: { 
          mp4Status: "failed",
          mp4Error: error.message,
          mp4FailedAt: new Date()
        } 
      }
    );

    await database.disconnect();

    return {
      statusCode: 500,
      body: JSON.stringify({
        success: false,
        error: error.message
      })
    };
  }
}
