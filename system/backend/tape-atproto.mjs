// tape-atproto.mjs
// Helper functions for syncing tapes with ATProto after MP4 conversion

import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

/**
 * Sync a MongoDB tape to ATProto after MP4 conversion completes
 * @param {Object} database - MongoDB connection
 * @param {string} mongoId - Tape MongoDB _id as string
 * @param {Buffer} mp4Buffer - MP4 video data as Buffer
 * @param {Buffer} thumbnailBuffer - Thumbnail image data as Buffer (optional)
 * @returns {Promise<Object>} { rkey, uri, cid } or { error }
 */
export async function createTapeOnAtproto(database, mongoId, mp4Buffer, thumbnailBuffer) {
  const tapes = database.db.collection("tapes");
  const users = database.db.collection("users");

  // 1. Fetch tape record from MongoDB
  const { ObjectId } = await import("mongodb");
  const tape = await tapes.findOne({ _id: new ObjectId(mongoId) });

  if (!tape) {
    shell.error(`❌ Tape not found: ${mongoId}`);
    return { error: "Tape not found" };
  }

  // 2. Get user account (or art-guest for anonymous tapes)
  let user;
  if (!tape.user) {
    // Guest/anonymous content uses art.at.aesthetic.computer
    shell.log(`ℹ️  Guest tape ${tape.code}, using art-guest account`);
    user = await users.findOne({ _id: "art-guest" });
    
    if (!user?.atproto?.did || !user?.atproto?.password) {
      shell.error(`❌ Art account not configured`);
      return { error: "Art account not configured" };
    }
  } else {
    // Regular user tape
    user = await users.findOne({ _id: tape.user });

    if (!user?.atproto?.did || !user?.atproto?.password) {
      shell.log(`ℹ️  User ${tape.user} has no ATProto account, skipping sync`);
      return { error: "No ATProto account" };
    }
  }

  try {
    // 3. Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    // 4. Upload MP4 as blob directly to PDS
    shell.log(`📤 Uploading MP4 to ATProto PDS (${(mp4Buffer.length / 1024 / 1024).toFixed(2)}MB)`);
    const blobResponse = await agent.uploadBlob(mp4Buffer, {
      encoding: "video/mp4",
    });

    if (!blobResponse.success) {
      throw new Error(`Failed to upload blob: ${JSON.stringify(blobResponse)}`);
    }

    const blob = blobResponse.data.blob;
    shell.log(`✅ Blob uploaded to PDS: ${blob.ref.$link}`);

    // 5. Create ATProto record
    const atprotoRecord = await agent.com.atproto.repo.createRecord({
      repo: user.atproto.did,
      collection: "computer.aesthetic.tape",
      record: {
        $type: "computer.aesthetic.tape",
        slug: tape.slug,
        code: tape.code,
        acUrl: `https://aesthetic.computer/!${tape.code}`,
        when: tape.when.toISOString(),
        video: blob,
        ref: mongoId,
      },
    });

    // Handle different response structures
    const uri = atprotoRecord.uri || atprotoRecord.data?.uri;
    const cid = atprotoRecord.cid || atprotoRecord.data?.cid;

    if (!uri) {
      const errMsg = `ATProto response missing URI: ${JSON.stringify(atprotoRecord)}`;
      shell.error(`⚠️  ${errMsg}`);
      return { error: errMsg };
    }

    const rkey = uri.split("/").pop();
    shell.log(`🦋 Created ATProto tape: ${rkey}`);

    // 6. Update MongoDB with ATProto references
    await tapes.updateOne(
      { _id: new ObjectId(mongoId) },
      {
        $set: {
          at: {
            rkey,
            uri,
            cid,
          },
        },
      }
    );

    shell.log(`✅ Updated MongoDB with ATProto references`);

    return { rkey, uri, cid };
  } catch (error) {
    shell.error(`⚠️  Failed to sync tape to ATProto: ${error.message}`);
    return { error: error.message };
  }
}
