// news-bluesky-ingest.mjs
// Pulls posts from trusted Bluesky accounts into news-posts as "external" entries.
// External posts are attributed to a Bluesky DID rather than an AC @handle.

import { AtpAgent } from "@atproto/api";
import { generateUniqueCode } from "./generate-short-code.mjs";

const BSKY_APPVIEW = "https://public.api.bsky.app";
const DEFAULT_LIMIT = 30;
const MAX_TITLE = 200;
const MAX_TEXT = 5000;

// Comma-separated list of trusted Bluesky sources (DIDs or handles).
// Example: NEWS_EXTERNAL_SOURCES="artistnewsnetwork.bsky.social"
function parseTrustedSources(raw) {
  if (!raw) return [];
  return String(raw)
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
}

export function getConfiguredSources(env = process.env) {
  const configured = parseTrustedSources(env.NEWS_EXTERNAL_SOURCES);
  if (configured.length) return configured;
  return ["artistnewsnetwork.bsky.social"];
}

function truncate(value, max) {
  if (!value) return "";
  const trimmed = String(value).trim();
  return trimmed.length > max ? trimmed.slice(0, max) : trimmed;
}

// Strip URLs from the prose body — they're already surfaced as the `url` field.
function stripUrls(text) {
  if (!text) return "";
  return text.replace(/https?:\/\/\S+/gi, "").replace(/\s+/g, " ").trim();
}

function extractExternalEmbed(post) {
  const embed = post?.embed;
  if (!embed) return null;
  // Hydrated appview shape
  if (embed.$type === "app.bsky.embed.external#view" && embed.external?.uri) {
    return {
      uri: embed.external.uri,
      title: embed.external.title || "",
      description: embed.external.description || "",
    };
  }
  // Raw record shape (fallback)
  const recordEmbed = post?.record?.embed;
  if (recordEmbed?.$type === "app.bsky.embed.external" && recordEmbed.external?.uri) {
    return {
      uri: recordEmbed.external.uri,
      title: recordEmbed.external.title || "",
      description: recordEmbed.external.description || "",
    };
  }
  return null;
}

// First link facet in the post record, if any.
function extractFirstFacetLink(post) {
  const facets = post?.record?.facets;
  if (!Array.isArray(facets)) return null;
  for (const facet of facets) {
    const feature = facet?.features?.find(
      (f) => f?.$type === "app.bsky.richtext.facet#link" && f.uri,
    );
    if (feature) return feature.uri;
  }
  return null;
}

// Produce { title, url, text } for insertion, or null to skip.
function projectPost(post) {
  const record = post?.record;
  if (!record) return null;
  const rawText = record.text || "";
  const external = extractExternalEmbed(post);

  if (external) {
    const title = truncate(external.title || stripUrls(rawText) || "Untitled link", MAX_TITLE);
    const text = truncate(stripUrls(rawText), MAX_TEXT);
    return { title, url: external.uri, text };
  }

  const facetLink = extractFirstFacetLink(post);
  if (facetLink) {
    const title = truncate(stripUrls(rawText) || "Untitled link", MAX_TITLE);
    const text = truncate(stripUrls(rawText), MAX_TEXT);
    return { title, url: facetLink, text };
  }

  // No link at all — skip. We only ingest headline-style posts.
  return null;
}

async function resolveProfile(agent, actor) {
  const result = await agent.app.bsky.actor.getProfile({ actor });
  const data = result?.data || result;
  if (!data?.did) throw new Error(`Could not resolve Bluesky actor: ${actor}`);
  return { did: data.did, handle: data.handle || actor };
}

async function fetchAuthorFeed(agent, did, limit) {
  const result = await agent.app.bsky.feed.getAuthorFeed({
    actor: did,
    filter: "posts_no_replies",
    limit,
  });
  const data = result?.data || result;
  return data?.feed || [];
}

function parseAtUri(uri) {
  // at://did:plc:xxx/app.bsky.feed.post/abc
  const match = /^at:\/\/([^/]+)\/([^/]+)\/(.+)$/.exec(uri || "");
  if (!match) return null;
  return { did: match[1], collection: match[2], rkey: match[3] };
}

/**
 * Ingest recent posts from one Bluesky actor into news-posts.
 * Returns { inserted, skipped, errors, actor }.
 */
export async function ingestFromActor(database, actor, options = {}) {
  const {
    limit = DEFAULT_LIMIT,
    agent: providedAgent,
    now = () => new Date(),
    generateCode = generateUniqueCode,
    log = () => {},
  } = options;

  const posts = database.db.collection("news-posts");

  const agent = providedAgent || new AtpAgent({ service: BSKY_APPVIEW });
  const profile = await resolveProfile(agent, actor);
  const feed = await fetchAuthorFeed(agent, profile.did, limit);

  let inserted = 0;
  let skipped = 0;
  const errors = [];
  const createdCodes = [];

  for (const item of feed) {
    const post = item?.post;
    if (!post?.uri) {
      skipped++;
      continue;
    }

    // Skip reposts — we only want the actor's original posts.
    if (item?.reason?.$type === "app.bsky.feed.defs#reasonRepost") {
      skipped++;
      continue;
    }
    // Skip replies
    if (post?.record?.reply) {
      skipped++;
      continue;
    }

    const existing = await posts.findOne({ "external.postUri": post.uri });
    if (existing) {
      skipped++;
      continue;
    }

    const projected = projectPost(post);
    if (!projected || !projected.title) {
      skipped++;
      continue;
    }

    const parsed = parseAtUri(post.uri);
    const postedAt = post.record?.createdAt
      ? new Date(post.record.createdAt)
      : now();
    const fetchedAt = now();

    try {
      const shortCode = await generateCode(posts, { mode: "random" });
      const code = `n${shortCode}`;
      const doc = {
        code,
        title: projected.title,
        url: projected.url,
        text: projected.text,
        user: null,
        when: postedAt,
        updated: fetchedAt,
        score: 1,
        commentCount: 0,
        status: "live",
        external: {
          source: "bsky",
          did: profile.did,
          handle: profile.handle,
          postUri: post.uri,
          postCid: post.cid || null,
          postedAt,
          fetchedAt,
        },
        // Mirror the Bluesky record into the `atproto` field so the
        // existing AT permalink affordance works for external posts too.
        atproto: parsed
          ? { did: parsed.did, uri: post.uri, rkey: parsed.rkey }
          : undefined,
      };

      await posts.insertOne(doc);
      inserted++;
      createdCodes.push(code);
      log(`✅ ${code} ← ${post.uri}`);
    } catch (error) {
      // Unique-index collision means a concurrent ingest already created it.
      if (error?.code === 11000) {
        skipped++;
        continue;
      }
      errors.push({ uri: post.uri, message: error.message });
      log(`❌ ${post.uri}: ${error.message}`);
    }
  }

  return {
    actor,
    did: profile.did,
    handle: profile.handle,
    inserted,
    skipped,
    errors,
    createdCodes,
  };
}

/**
 * Ingest from every configured trusted source.
 * Returns an array of per-actor result summaries.
 */
export async function ingestAll(database, options = {}) {
  const sources = options.sources || getConfiguredSources();
  const results = [];
  for (const actor of sources) {
    try {
      const result = await ingestFromActor(database, actor, options);
      results.push(result);
    } catch (error) {
      results.push({
        actor,
        inserted: 0,
        skipped: 0,
        errors: [{ message: error.message }],
      });
    }
  }
  return results;
}

export const __testing = { projectPost, extractExternalEmbed, extractFirstFacetLink, stripUrls, parseAtUri };
