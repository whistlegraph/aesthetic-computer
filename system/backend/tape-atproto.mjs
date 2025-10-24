// tape-atproto.mjs
// Helper functions for syncing tapes with ATProto after MP4 conversion

import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

/**
 * Sync a MongoDB tape to ATProto after MP4 conversion completes
 * @param {Object} database - MongoDB connection
 * @param {string} mongoId - Tape MongoDB _id as string
 * @param {string} mp4Url - Full URL to the MP4 file
 * @returns {Promise<Object>} { rkey, uri, cid } or { error }
 */
export async function createTapeOnAtproto(database, mongoId, mp4Url) {
  const tapes = database.db.collection("tapes");
  const users = database.db.collection("users");

  // 1. Fetch tape record from MongoDB
  const { ObjectId } = await import("mongodb");
  const tape = await tapes.findOne({ _id: new ObjectId(mongoId) });

  if (!tape) {
    shell.error(`❌ Tape not found: ${mongoId}`);
    return { error: "Tape not found" };
  }

  // Guest tapes don't sync to ATProto (no user account)
  if (!tape.user) {
    shell.log(`ℹ️  Guest tape ${tape.code}, skipping ATProto sync`);
    return { error: "Guest tape" };
  }

  // 2. Check if user has ATProto account
  const user = await users.findOne({ _id: tape.user });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    shell.log(`ℹ️  User ${tape.user} has no ATProto account, skipping sync`);
    return { error: "No ATProto account" };
  }

  try {
    // 3. Download MP4 file
    shell.log(`📥 Downloading MP4: ${mp4Url}`);
    const mp4Response = await fetch(mp4Url);
    if (!mp4Response.ok) {
      throw new Error(`Failed to download MP4: ${mp4Response.statusText}`);
    }
    const mp4Blob = await mp4Response.blob();
    const mp4Buffer = await mp4Blob.arrayBuffer();

    // 4. Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    // 5. Upload MP4 as blob
    shell.log(`📤 Uploading MP4 to ATProto (${(mp4Buffer.byteLength / 1024 / 1024).toFixed(2)}MB)`);
    const blobResponse = await agent.uploadBlob(mp4Buffer, {
      encoding: "video/mp4",
    });

    if (!blobResponse.success) {
      throw new Error(`Failed to upload blob: ${JSON.stringify(blobResponse)}`);
    }

    const blob = blobResponse.data.blob;
    shell.log(`✅ Blob uploaded: ${blob.ref.$link}`);

    // 6. Create ATProto record
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

    // 7. Update MongoDB with ATProto references
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
