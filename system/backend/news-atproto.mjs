// news-atproto.mjs
// Helper functions for syncing news to ATProto PDS
// 2026.01.28

import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";
const NEWS_COLLECTION = "computer.aesthetic.news";

/**
 * Create a news item on ATProto PDS using the submitting user's account
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub (auth0|...) 
 * @param {Object} newsData - { headline, body?, link?, tags?, when }
 * @param {string} refId - Database record _id as string
 * @returns {Promise<{rkey: string, uri: string, did: string} | {error: string}>}
 */
export async function createNewsOnAtproto(database, sub, newsData, refId) {
  const users = database.db.collection("users");

  // Look up user's ATProto credentials
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    shell.log(`ℹ️  User ${sub} has no ATProto account, skipping news sync`);
    return { error: "No ATProto account" };
  }

  const atprotoDid = user.atproto.did;
  const atprotoPassword = user.atproto.password;

  try {
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: atprotoDid,
      password: atprotoPassword,
    });

    const record = {
      $type: NEWS_COLLECTION,
      headline: newsData.headline,
      when: newsData.when?.toISOString() || new Date().toISOString(),
      ref: refId,
    };

    // Add optional fields
    if (newsData.body) record.body = newsData.body;
    if (newsData.link) record.link = newsData.link;
    if (newsData.tags?.length) record.tags = newsData.tags;

    const result = await agent.com.atproto.repo.createRecord({
      repo: atprotoDid,
      collection: NEWS_COLLECTION,
      record,
    });

    const uri = result.uri || result.data?.uri;
    if (!uri) {
      shell.error(`⚠️ ATProto response missing URI: ${JSON.stringify(result)}`);
      return { error: "Missing URI in response" };
    }

    const rkey = uri.split("/").pop();
    shell.log(`📰 Created ATProto news for ${atprotoDid}: ${rkey}`);

    return { rkey, uri, did: atprotoDid };
  } catch (error) {
    shell.error(`❌ Failed to create news on ATProto: ${error.message}`);
    return { error: error.message };
  }
}

/**
 * Delete a news item from ATProto PDS
 * @param {Object} database - MongoDB connection
 * @param {string} sub - User sub (auth0|...) 
 * @param {string} rkey - ATProto record key
 * @returns {Promise<boolean>}
 */
export async function deleteNewsFromAtproto(database, sub, rkey) {
  const users = database.db.collection("users");
  const user = await users.findOne({ _id: sub });

  if (!user?.atproto?.did || !user?.atproto?.password) {
    return false;
  }

  const atprotoDid = user.atproto.did;
  const atprotoPassword = user.atproto.password;

  try {
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: atprotoDid,
      password: atprotoPassword,
    });

    await agent.com.atproto.repo.deleteRecord({
      repo: atprotoDid,
      collection: NEWS_COLLECTION,
      rkey,
    });

    shell.log(`🗑️ Deleted ATProto news: ${rkey}`);
    return true;
  } catch (error) {
    shell.error(`❌ Failed to delete news from ATProto: ${error.message}`);
    return false;
  }
}

/**
 * List news items from a specific user's ATProto repo (public, no auth needed)
 * @param {string} did - User's ATProto DID
 * @param {number} limit - Max items to fetch
 * @param {string} cursor - Pagination cursor
 * @returns {Promise<{records: Array, cursor?: string}>}
 */
export async function listNewsFromAtproto(did, limit = 50, cursor = null) {
  if (!did) {
    return { records: [] };
  }

  try {
    const agent = new AtpAgent({ service: PDS_URL });
    
    const params = {
      repo: did,
      collection: NEWS_COLLECTION,
      limit,
    };
    if (cursor) params.cursor = cursor;

    const result = await agent.com.atproto.repo.listRecords(params);

    return {
      records: result.data?.records || [],
      cursor: result.data?.cursor,
    };
  } catch (error) {
    shell.error(`❌ Failed to list news from ATProto: ${error.message}`);
    return { records: [] };
  }
}
