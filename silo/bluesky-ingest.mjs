// bluesky-ingest.mjs
// Pulls headlines from trusted Bluesky accounts into news-posts.
// Self-contained: uses fetch + crypto, no extra deps beyond what silo ships.
//
// Mirrors the logic of ../system/backend/news-bluesky-ingest.mjs, but rewritten
// to avoid the @atproto/api and nanoid dependencies so it can run inside silo.

import { randomBytes } from "node:crypto";

const BSKY_APPVIEW = "https://public.api.bsky.app";
const DEFAULT_LIMIT = 30;
const MAX_TITLE = 200;
const MAX_TEXT = 5000;

// ---- Config ----------------------------------------------------------------

export function getConfiguredSources(env = process.env) {
  const raw = env.NEWS_EXTERNAL_SOURCES;
  if (!raw) return ["artistnewsnetwork.bsky.social"];
  return String(raw).split(",").map((s) => s.trim()).filter(Boolean);
}

// ---- Short-code generator (same alphabet as the rest of the system) --------

const CODE_ALPHABET = "bcdfghjklmnpqrstvwxyzaeiou23456789";
const CODE_LEN = 3;

function makeCandidate() {
  const bytes = randomBytes(CODE_LEN);
  return Array.from(bytes).map((b) => CODE_ALPHABET[b % CODE_ALPHABET.length]).join("");
}

async function generateUniqueCode(posts) {
  for (let i = 0; i < 100; i++) {
    const candidate = `n${makeCandidate()}`;
    const existing = await posts.findOne({ code: candidate });
    if (!existing) return candidate;
  }
  throw new Error("Could not generate a unique news code after 100 attempts");
}

// ---- Pure helpers ----------------------------------------------------------

function truncate(value, max) {
  if (!value) return "";
  const trimmed = String(value).trim();
  return trimmed.length > max ? trimmed.slice(0, max) : trimmed;
}

function stripUrls(text) {
  if (!text) return "";
  return text.replace(/https?:\/\/\S+/gi, "").replace(/\s+/g, " ").trim();
}

function extractExternalEmbed(post) {
  const embed = post?.embed;
  if (embed?.$type === "app.bsky.embed.external#view" && embed.external?.uri) {
    return { uri: embed.external.uri, title: embed.external.title || "" };
  }
  const recordEmbed = post?.record?.embed;
  if (recordEmbed?.$type === "app.bsky.embed.external" && recordEmbed.external?.uri) {
    return { uri: recordEmbed.external.uri, title: recordEmbed.external.title || "" };
  }
  return null;
}

function extractFirstFacetLink(post) {
  const facets = post?.record?.facets;
  if (!Array.isArray(facets)) return null;
  for (const f of facets) {
    const link = f?.features?.find((x) => x?.$type === "app.bsky.richtext.facet#link" && x.uri);
    if (link) return link.uri;
  }
  return null;
}

function projectPost(post) {
  const record = post?.record;
  if (!record) return null;
  const rawText = record.text || "";
  const external = extractExternalEmbed(post);
  if (external) {
    return {
      title: truncate(external.title || stripUrls(rawText) || "Untitled link", MAX_TITLE),
      url: external.uri,
      text: truncate(stripUrls(rawText), MAX_TEXT),
    };
  }
  const facetLink = extractFirstFacetLink(post);
  if (facetLink) {
    return {
      title: truncate(stripUrls(rawText) || "Untitled link", MAX_TITLE),
      url: facetLink,
      text: truncate(stripUrls(rawText), MAX_TEXT),
    };
  }
  return null;
}

function parseAtUri(uri) {
  const match = /^at:\/\/([^/]+)\/([^/]+)\/(.+)$/.exec(uri || "");
  if (!match) return null;
  return { did: match[1], collection: match[2], rkey: match[3] };
}

// ---- Bluesky appview calls (fetch, no auth) -------------------------------

async function xrpcGet(method, params) {
  const qs = new URLSearchParams(params).toString();
  const url = `${BSKY_APPVIEW}/xrpc/${method}?${qs}`;
  const res = await fetch(url, {
    headers: { "User-Agent": "AestheticNewsBot/1.0 (+https://news.aesthetic.computer)" },
  });
  if (!res.ok) {
    const body = await res.text().catch(() => "");
    throw new Error(`${method} ${res.status}: ${body.slice(0, 200)}`);
  }
  return res.json();
}

async function resolveProfile(actor) {
  const data = await xrpcGet("app.bsky.actor.getProfile", { actor });
  if (!data?.did) throw new Error(`Could not resolve Bluesky actor: ${actor}`);
  return { did: data.did, handle: data.handle || actor };
}

async function getAuthorFeed(did, limit) {
  const data = await xrpcGet("app.bsky.feed.getAuthorFeed", {
    actor: did,
    filter: "posts_no_replies",
    limit: String(limit),
  });
  return data?.feed || [];
}

// ---- Core ingest ----------------------------------------------------------

/**
 * Pull posts from one Bluesky actor into db.collection("news-posts").
 * Returns { actor, did, handle, inserted, skipped, errors, createdCodes }.
 */
export async function ingestFromActor(db, actor, options = {}) {
  const { limit = DEFAULT_LIMIT, now = () => new Date(), log = () => {} } = options;
  const posts = db.collection("news-posts");

  const profile = await resolveProfile(actor);
  const feed = await getAuthorFeed(profile.did, limit);

  let inserted = 0;
  let skipped = 0;
  const errors = [];
  const createdCodes = [];

  for (const item of feed) {
    const post = item?.post;
    if (!post?.uri) { skipped++; continue; }
    if (item?.reason?.$type === "app.bsky.feed.defs#reasonRepost") { skipped++; continue; }
    if (post?.record?.reply) { skipped++; continue; }

    const existing = await posts.findOne({ "external.postUri": post.uri });
    if (existing) { skipped++; continue; }

    const projected = projectPost(post);
    if (!projected?.title) { skipped++; continue; }

    const parsed = parseAtUri(post.uri);
    const postedAt = post.record?.createdAt ? new Date(post.record.createdAt) : now();
    const fetchedAt = now();

    try {
      const code = await generateUniqueCode(posts);
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
        atproto: parsed ? { did: parsed.did, uri: post.uri, rkey: parsed.rkey } : undefined,
      };
      await posts.insertOne(doc);
      inserted++;
      createdCodes.push(code);
      log(`✅ ${code} ← ${post.uri}`);
    } catch (error) {
      if (error?.code === 11000) { skipped++; continue; }
      errors.push({ uri: post.uri, message: error.message });
      log(`❌ ${post.uri}: ${error.message}`);
    }
  }

  return { actor, did: profile.did, handle: profile.handle, inserted, skipped, errors, createdCodes };
}

/**
 * Pull from every configured source. Returns an array of result summaries.
 */
export async function ingestAll(db, options = {}) {
  const sources = options.sources || getConfiguredSources();
  const results = [];
  for (const actor of sources) {
    try {
      results.push(await ingestFromActor(db, actor, options));
    } catch (error) {
      results.push({ actor, inserted: 0, skipped: 0, errors: [{ message: error.message }] });
    }
  }
  return results;
}

export const __testing = { projectPost, stripUrls, parseAtUri, extractExternalEmbed, extractFirstFacetLink };
