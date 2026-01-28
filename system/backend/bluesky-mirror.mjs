// bluesky-mirror.mjs
// Mirror moods to Bluesky @aesthetic.computer account
// 2026.01.28

import { BskyAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

// Cache for Bluesky credentials from MongoDB
let cachedBlueskyCredentials = null;

/**
 * Get Bluesky credentials from MongoDB secrets collection
 * @param {Object} db - MongoDB database instance
 * @returns {Promise<{identifier: string, appPassword: string, service: string, mirrorHandles: string[]}>}
 */
async function getBlueskyCredentials(db) {
  if (cachedBlueskyCredentials) return cachedBlueskyCredentials;
  
  const secrets = await db.collection("secrets").findOne({ _id: "bluesky" });
  
  if (!secrets) {
    throw new Error("Bluesky credentials not found in database");
  }
  
  cachedBlueskyCredentials = {
    identifier: secrets.identifier,
    appPassword: secrets.appPassword,
    service: secrets.service || "https://bsky.social",
    mirrorHandles: (secrets.mirrorHandles || ["@jeffrey"]).map(h => h.toLowerCase()),
  };
  
  return cachedBlueskyCredentials;
}

/**
 * Check if a handle should have moods mirrored to Bluesky
 * @param {Object} db - MongoDB database instance  
 * @param {string} handle - Handle like "@jeffrey"
 * @returns {Promise<boolean>}
 */
export async function shouldMirror(db, handle) {
  try {
    const creds = await getBlueskyCredentials(db);
    return creds.mirrorHandles.includes(handle.toLowerCase());
  } catch (e) {
    shell.error(`⚠️ Could not check mirror status: ${e.message}`);
    return false;
  }
}

/**
 * Post a mood to the @aesthetic.computer Bluesky account
 * @param {Object} db - MongoDB database instance
 * @param {string} moodText - The mood content
 * @param {string} handle - The author's handle (e.g., "@jeffrey")
 * @param {string} atprotoRkey - The ATProto record key for permalink
 * @returns {Promise<{uri: string, cid: string, rkey: string} | null>}
 */
export async function postMoodToBluesky(db, moodText, handle, atprotoRkey) {
  let creds;
  try {
    creds = await getBlueskyCredentials(db);
  } catch (e) {
    shell.log(`⚠️ ${e.message}, skipping Bluesky mirror`);
    return null;
  }

  try {
    const agent = new BskyAgent({ service: creds.service });

    shell.log(`🦋 Logging into Bluesky as @${creds.identifier}...`);
    await agent.login({
      identifier: creds.identifier,
      password: creds.appPassword,
    });

    // Format post with mood and permalink
    // Handle format: @jeffrey -> jeffrey for URL
    const handleForUrl = handle.replace(/^@/, "");
    const permalink = `https://aesthetic.computer/moods~${handleForUrl}~${atprotoRkey}`;

    // Build post text
    // Keep it concise - Bluesky has 300 char limit
    let postText = moodText;
    
    // Add attribution if not obvious
    if (handle.toLowerCase() !== "@aesthetic.computer") {
      postText = `${handle}: ${moodText}`;
    }
    
    // Add permalink (on new line if there's room)
    if (postText.length + permalink.length + 2 <= 300) {
      postText = `${postText}\n\n${permalink}`;
    }

    // Truncate if still too long (shouldn't happen often with moods)
    if (postText.length > 300) {
      postText = postText.substring(0, 297) + "...";
    }

    shell.log(`📤 Posting to Bluesky: "${postText.substring(0, 50)}..."`);

    const response = await agent.post({
      text: postText,
      createdAt: new Date().toISOString(),
      // TODO: Could add facets for rich links (handle mentions, URLs)
    });

    const rkey = response.uri.split("/").pop();

    shell.log(`✅ Posted to Bluesky: ${rkey}`);
    shell.log(`🔗 View: https://bsky.app/profile/${BSKY_IDENTIFIER}/post/${rkey}`);

    return {
      uri: response.uri,
      cid: response.cid,
      rkey,
    };
  } catch (error) {
    shell.error(`❌ Failed to post to Bluesky: ${error.message}`);
    return null;
  }
}

/**
 * Delete a mirrored post from Bluesky
 * @param {Object} db - MongoDB database instance
 * @param {string} blueskyRkey - The Bluesky post rkey
 * @returns {Promise<boolean>}
 */
export async function deleteMoodFromBluesky(db, blueskyRkey) {
  let creds;
  try {
    creds = await getBlueskyCredentials(db);
  } catch (e) {
    return false;
  }

  try {
    const agent = new BskyAgent({ service: creds.service });

    await agent.login({
      identifier: creds.identifier,
      password: creds.appPassword,
    });

    await agent.deletePost(
      `at://${agent.session.did}/app.bsky.feed.post/${blueskyRkey}`
    );

    shell.log(`🗑️ Deleted Bluesky post: ${blueskyRkey}`);
    return true;
  } catch (error) {
    shell.error(`❌ Failed to delete from Bluesky: ${error.message}`);
    return false;
  }
}
