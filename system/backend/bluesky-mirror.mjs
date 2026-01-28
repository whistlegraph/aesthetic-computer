// bluesky-mirror.mjs
// Mirror moods to Bluesky @aesthetic.computer account
// 2026.01.28

import { BskyAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

const BSKY_SERVICE = process.env.BSKY_SERVICE || "https://bsky.social";
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER; // aesthetic.computer
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD;

// Handles that should be mirrored to Bluesky
// Can be overridden via BSKY_MIRROR_HANDLES env var (comma-separated)
const MIRROR_HANDLES = (process.env.BSKY_MIRROR_HANDLES || "@jeffrey")
  .split(",")
  .map((h) => h.trim().toLowerCase());

/**
 * Check if a handle should have moods mirrored to Bluesky
 * @param {string} handle - Handle like "@jeffrey"
 * @returns {boolean}
 */
export function shouldMirror(handle) {
  return MIRROR_HANDLES.includes(handle.toLowerCase());
}

/**
 * Post a mood to the @aesthetic.computer Bluesky account
 * @param {string} moodText - The mood content
 * @param {string} handle - The author's handle (e.g., "@jeffrey")
 * @param {string} atprotoRkey - The ATProto record key for permalink
 * @returns {Promise<{uri: string, cid: string, rkey: string} | null>}
 */
export async function postMoodToBluesky(moodText, handle, atprotoRkey) {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    shell.log("⚠️ Bluesky credentials not configured, skipping mirror");
    return null;
  }

  try {
    const agent = new BskyAgent({ service: BSKY_SERVICE });

    shell.log(`🦋 Logging into Bluesky as @${BSKY_IDENTIFIER}...`);
    await agent.login({
      identifier: BSKY_IDENTIFIER,
      password: BSKY_APP_PASSWORD,
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
 * @param {string} blueskyRkey - The Bluesky post rkey
 * @returns {Promise<boolean>}
 */
export async function deleteMoodFromBluesky(blueskyRkey) {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    return false;
  }

  try {
    const agent = new BskyAgent({ service: BSKY_SERVICE });

    await agent.login({
      identifier: BSKY_IDENTIFIER,
      password: BSKY_APP_PASSWORD,
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
