// bluesky-engagement.mjs
// Fetch engagement (likes, reposts, replies) from Bluesky for mirrored moods
// 2026.01.28

import { BskyAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

const BSKY_SERVICE = "https://bsky.social";

/**
 * Fetch engagement stats for a Bluesky post
 * @param {string} blueskyUri - The AT URI of the Bluesky post (at://did:plc:xxx/app.bsky.feed.post/rkey)
 * @returns {Promise<{likes: number, reposts: number, replies: Array}>}
 */
export async function fetchBlueskyEngagement(blueskyUri) {
  if (!blueskyUri) {
    return { likes: 0, reposts: 0, replies: [], quoteCount: 0 };
  }

  try {
    // Public API - no auth needed for reading
    const agent = new BskyAgent({ service: BSKY_SERVICE });

    // Get the post thread which includes engagement data
    const response = await agent.getPostThread({
      uri: blueskyUri,
      depth: 1, // Get immediate replies only
    });

    const post = response.data.thread?.post;

    if (!post) {
      shell.log(`⚠️ No post data found for: ${blueskyUri}`);
      return { likes: 0, reposts: 0, replies: [], quoteCount: 0 };
    }

    // Extract engagement counts
    const likes = post.likeCount || 0;
    const reposts = post.repostCount || 0;
    const quoteCount = post.quoteCount || 0;

    // Extract replies
    const replies = (response.data.thread.replies || [])
      .filter((r) => r.post) // Filter out blocked/deleted
      .map((r) => ({
        author: {
          handle: r.post.author?.handle || "unknown",
          displayName: r.post.author?.displayName || null,
          avatar: r.post.author?.avatar || null,
        },
        text: r.post.record?.text || "",
        when: r.post.record?.createdAt || new Date().toISOString(),
        uri: r.post.uri,
        likes: r.post.likeCount || 0,
      }))
      .sort((a, b) => new Date(b.when) - new Date(a.when)); // Most recent first

    shell.log(
      `📊 Bluesky engagement: ${likes} likes, ${reposts} reposts, ${replies.length} replies`
    );

    return {
      likes,
      reposts,
      quoteCount,
      replies,
      // Include the Bluesky web URL for linking
      webUrl: blueskyUriToWebUrl(blueskyUri),
    };
  } catch (error) {
    shell.error(`❌ Failed to fetch Bluesky engagement: ${error.message}`);
    return { likes: 0, reposts: 0, replies: [], quoteCount: 0 };
  }
}

/**
 * Convert an AT Protocol URI to a Bluesky web URL
 * @param {string} atUri - e.g., at://did:plc:xxx/app.bsky.feed.post/rkey
 * @returns {string} - e.g., https://bsky.app/profile/handle/post/rkey
 */
function blueskyUriToWebUrl(atUri) {
  try {
    // Parse: at://did:plc:xxx/app.bsky.feed.post/rkey
    const parts = atUri.replace("at://", "").split("/");
    const did = parts[0];
    const rkey = parts[parts.length - 1];

    // Use DID directly in URL (Bluesky resolves it)
    return `https://bsky.app/profile/${did}/post/${rkey}`;
  } catch {
    return null;
  }
}

/**
 * Resolve a DID to a handle (optional, for nicer URLs)
 * @param {string} did - e.g., did:plc:xxx
 * @returns {Promise<string|null>} - Handle or null
 */
export async function resolveDidToHandle(did) {
  try {
    const agent = new BskyAgent({ service: BSKY_SERVICE });
    const response = await agent.getProfile({ actor: did });
    return response.data.handle;
  } catch {
    return null;
  }
}
