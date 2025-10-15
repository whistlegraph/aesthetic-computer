// mood-atproto.mjs
// Helper functions for syncing moods with ATProto

import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

/**
 * Sync a MongoDB mood to ATProto after it's been created
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub (auth0|...)
 * @param {string} moodText - Mood content
 * @param {Date} moodDate - Mood timestamp
 * @param {string} refId - Database record _id as string
 * @returns {Promise<Object>} { rkey, uri } or { error }
 */
export async function createMoodOnAtproto(
  database,
  sub,
  moodText,
  moodDate,
  refId
) {
  const users = database.db.collection("users");

  // 1. Check if user has ATProto account
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    shell.log(`ℹ️  User ${sub} has no ATProto account, skipping sync`);
    return { error: "No ATProto account" };
  }

  try {
    // 2. Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    // 3. Create ATProto record
    const atprotoRecord = await agent.com.atproto.repo.createRecord({
      repo: user.atproto.did,
      collection: "computer.aesthetic.mood",
      record: {
        $type: "computer.aesthetic.mood",
        mood: moodText,
        when: moodDate.toISOString(),
        ref: refId,
      },
    });

    // Handle different response structures
    const uri = atprotoRecord.uri || atprotoRecord.data?.uri;
    
    if (!uri) {
      const errMsg = `ATProto response missing URI: ${JSON.stringify(atprotoRecord)}`;
      shell.error(`⚠️  ${errMsg}`);
      return { error: errMsg };
    }

    const rkey = uri.split("/").pop();
    shell.log(`🦋 Created ATProto mood: ${rkey}`);

    return { rkey, uri };
  } catch (error) {
    shell.error(`⚠️  Failed to sync mood to ATProto: ${error.message}`);
    return { error: error.message };
  }
}

/**
 * Create a mood in both MongoDB and ATProto with bi-directional sync
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub (auth0|...)
 * @param {string} moodText - Mood content
 * @returns {Promise<Object>} { mongoId, atproto: { rkey, uri } | null }
 */
export async function createMoodWithAtproto(database, sub, moodText) {
  const moods = database.db.collection("moods");
  const users = database.db.collection("users");

  // 1. Create MongoDB mood first (without atproto field)
  const result = await moods.insertOne({
    user: sub,
    mood: moodText,
    when: new Date(),
  });

  const refId = result.insertedId.toString();
  shell.log(`📝 Created mood in MongoDB: ${refId}`);

  // 2. Check if user has ATProto account
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    shell.log(`ℹ️  User ${sub} has no ATProto account, skipping sync`);
    return { mongoId: result.insertedId, atproto: null };
  }

  try {
    // 3. Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });

    // 4. Create ATProto record
    const atprotoRecord = await agent.com.atproto.repo.createRecord({
      repo: user.atproto.did,
      collection: "computer.aesthetic.mood",
      record: {
        $type: "computer.aesthetic.mood",
        mood: moodText,
        when: new Date().toISOString(),
        ref: refId,
      },
    });

    // Handle different response structures
    const uri = atprotoRecord.uri || atprotoRecord.data?.uri;
    
    if (!uri) {
      const errMsg = `ATProto response missing URI: ${JSON.stringify(atprotoRecord)}`;
      shell.error(`⚠️  ${errMsg}`);
      return { mongoId: result.insertedId, atproto: null };
    }

    const rkey = uri.split("/").pop();
    shell.log(`🦋 Created ATProto mood: ${rkey}`);

    // 5. Update MongoDB with ATProto reference
    await moods.updateOne(
      { _id: result.insertedId },
      {
        $set: {
          atproto: { rkey },
        },
      }
    );

    shell.log(`✅ Bi-directional sync complete`);

    return {
      mongoId: result.insertedId,
      atproto: { rkey, uri: atprotoRecord.uri },
    };
  } catch (error) {
    shell.error(`⚠️  Failed to sync mood to ATProto: ${error.message}`);
    // Don't fail the whole operation - mood is still in MongoDB
    return { mongoId: result.insertedId, atproto: null };
  }
}

/**
 * Delete a mood from both MongoDB and ATProto
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub
 * @param {string} mongoId - MongoDB _id of mood to delete
 * @returns {Promise<boolean>} Success status
 */
export async function deleteMoodFromAtproto(database, sub, mongoId) {
  const moods = database.db.collection("moods");
  const users = database.db.collection("users");

  // 1. Get the mood from MongoDB
  const mood = await moods.findOne({
    _id: mongoId,
    user: sub,
  });

  if (!mood) {
    shell.log(`❌ Mood ${mongoId} not found`);
    return false;
  }

  // 2. Check if user has ATProto account
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    shell.log(`ℹ️  User ${sub} has no ATProto account, skipping ATProto delete`);
    // Just delete from MongoDB
    await moods.updateOne({ _id: mongoId }, { $set: { deleted: true } });
    return true;
  }

  // 3. Delete from ATProto if synced
  if (mood.atproto?.rkey) {
    try {
      const agent = new AtpAgent({ service: PDS_URL });
      await agent.login({
        identifier: user.atproto.did,
        password: user.atproto.password,
      });

      await agent.com.atproto.repo.deleteRecord({
        repo: user.atproto.did,
        collection: "computer.aesthetic.mood",
        rkey: mood.atproto.rkey,
      });

      shell.log(`🦋 Deleted from ATProto: ${mood.atproto.rkey}`);
    } catch (error) {
      shell.error(`⚠️  Failed to delete from ATProto: ${error.message}`);
      // Continue with MongoDB deletion anyway
    }
  }

  // 4. Soft delete in MongoDB
  await moods.updateOne({ _id: mongoId }, { $set: { deleted: true } });
  shell.log(`✅ Mood ${mongoId} marked as deleted`);

  return true;
}

/**
 * Nuke all moods from both MongoDB and ATProto
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub
 * @returns {Promise<number>} Number of moods deleted
 */
export async function nukeAllMoodsFromAtproto(database, sub) {
  const moods = database.db.collection("moods");
  const users = database.db.collection("users");

  // 1. Get all user's moods
  const userMoods = await moods.find({ user: sub }).toArray();
  shell.log(`🗑️  Found ${userMoods.length} moods to nuke`);

  // 2. Check if user has ATProto account
  const user = await users.findOne({ _id: sub });

  if (user?.atproto?.did && user?.atproto?.password) {
    try {
      const agent = new AtpAgent({ service: PDS_URL });
      await agent.login({
        identifier: user.atproto.did,
        password: user.atproto.password,
      });

      // Delete from ATProto
      let atprotoDeleteCount = 0;
      for (const mood of userMoods) {
        if (mood.atproto?.rkey) {
          try {
            await agent.com.atproto.repo.deleteRecord({
              repo: user.atproto.did,
              collection: "computer.aesthetic.mood",
              rkey: mood.atproto.rkey,
            });
            atprotoDeleteCount++;
          } catch (error) {
            shell.error(
              `⚠️  Failed to delete ATProto mood ${mood.atproto.rkey}: ${error.message}`
            );
          }
        }
      }
      shell.log(`🦋 Deleted ${atprotoDeleteCount} moods from ATProto`);
    } catch (error) {
      shell.error(`⚠️  Failed to login to ATProto: ${error.message}`);
    }
  }

  // 3. Mark all as deleted in MongoDB
  const result = await moods.updateMany(
    { user: sub },
    { $set: { deleted: true } }
  );

  shell.log(`✅ Marked ${result.modifiedCount} moods as deleted in MongoDB`);

  return result.modifiedCount;
}
