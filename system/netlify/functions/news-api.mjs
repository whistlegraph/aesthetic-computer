// News API, 2026.01.15
// JSON + form handlers for posts, comments, votes

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { ObjectId } from "mongodb";
import { createNewsOnAtproto } from "../../backend/news-atproto.mjs";

// Admin users who can delete/censor content
const ADMIN_SUBS = [process.env.ADMIN_SUB].filter(Boolean);

const MAX_TITLE = 200;
const MAX_TEXT = 5000;
const RATE_LIMIT_HOURS = 24; // Users can only post once per 24 hours

function parseBody(event) {
  if (!event.body) return {};
  const contentType = event.headers?.["content-type"] || event.headers?.["Content-Type"] || "";
  if (contentType.includes("application/json")) {
    try {
      return JSON.parse(event.body);
    } catch {
      return {};
    }
  }
  const params = new URLSearchParams(event.body);
  return Object.fromEntries(params.entries());
}

function wantsHtml(event) {
  const accept = event.headers?.accept || event.headers?.Accept || "";
  return accept.includes("text/html") || accept.includes("application/xhtml");
}

function redirect(location) {
  return {
    statusCode: 302,
    headers: {
      Location: location,
      "Content-Type": "text/html",
    },
    body: `<a href="${location}">Redirecting</a>`,
  };
}

function parseRoute(event) {
  // Extract route from path, stripping the function prefix
  let path = event.path || "";
  const prefixes = ["/.netlify/functions/news-api", "/api/news"];
  for (const prefix of prefixes) {
    if (path.startsWith(prefix)) {
      path = path.slice(prefix.length);
      break;
    }
  }
  // Also check queryStringParameters for backwards compatibility
  if (!path && event.queryStringParameters?.path) {
    path = event.queryStringParameters.path;
  }
  return path.replace(/^\/+/, "").replace(/\/+$/, "");
}

async function ensureIndexes(posts, comments, votes) {
  await posts.createIndex({ code: 1 }, { unique: true, background: true });
  await posts.createIndex({ when: -1 }, { background: true });
  await posts.createIndex({ score: -1 }, { background: true });
  await posts.createIndex({ user: 1 }, { background: true });
  await posts.createIndex({ status: 1 }, { background: true });

  await comments.createIndex({ postCode: 1 }, { background: true });
  await comments.createIndex({ parentId: 1 }, { background: true, sparse: true });
  await comments.createIndex({ when: 1 }, { background: true });
  await comments.createIndex({ user: 1 }, { background: true });

  await votes.createIndex({ itemType: 1, itemId: 1, user: 1 }, { unique: true, background: true });
}

function sanitizeText(value, max) {
  if (!value) return "";
  const trimmed = String(value).trim();
  return trimmed.length > max ? trimmed.slice(0, max) : trimmed;
}

function normalizeUrl(value) {
  if (!value) return "";
  const trimmed = value.trim();
  if (!trimmed) return "";
  if (/^https?:\/\//i.test(trimmed)) return trimmed;
  return `https://${trimmed}`;
}

function resolveBasePath(event) {
  const host = event.headers?.host || "";
  return host.startsWith("news.aesthetic.computer") ? "" : "/news.aesthetic.computer";
}

async function attachCommentCounts(posts, comments) {
  if (!posts || posts.length === 0) return posts || [];
  const codes = posts.map((post) => post.code).filter(Boolean);
  const counts = await comments
    .aggregate([
      { $match: { postCode: { $in: codes }, status: { $ne: "dead" } } },
      { $group: { _id: "$postCode", count: { $sum: 1 } } },
    ])
    .toArray();
  const countMap = new Map(counts.map((row) => [row._id, row.count]));
  return posts.map((post) => ({
    ...post,
    commentCount: countMap.get(post.code) || 0,
  }));
}

export function createHandler({
  connect: connectFn = connect,
  respond: respondFn = respond,
  authorize: authorizeFn = authorize,
  generateUniqueCode: generateUniqueCodeFn = generateUniqueCode,
  ObjectId: ObjectIdImpl = ObjectId,
} = {}) {
  async function requireUserWith(event) {
    const user = await authorizeFn(event.headers || {});
    if (!user) {
      const res = respondFn(401, { error: "Unauthorized" });
      res.headers["WWW-Authenticate"] = "Bearer";
      throw Object.assign(new Error("Unauthorized"), { response: res });
    }
    return user;
  }

  return async function handler(event) {
    const route = parseRoute(event);

    if (event.httpMethod === "OPTIONS") {
      return respondFn(204, "");
    }

    // Unfurl endpoint doesn't need the database
    if (event.httpMethod === "GET" && route === "unfurl") {
      const targetUrl = event.queryStringParameters?.url;
      if (!targetUrl) {
        return respondFn(400, { error: "Missing 'url' parameter" });
      }
      try {
        const normalizedUrl = normalizeUrl(targetUrl);
        const controller = new AbortController();
        const timeout = setTimeout(() => controller.abort(), 5000);
        const res = await fetch(normalizedUrl, {
          signal: controller.signal,
          headers: {
            'User-Agent': 'AestheticNewsBot/1.0 (+https://news.aesthetic.computer)',
            'Accept': 'text/html',
          },
          redirect: 'follow',
        });
        clearTimeout(timeout);
        if (!res.ok) {
          return respondFn(200, { title: '', error: 'Could not fetch URL' });
        }
        const contentType = res.headers.get('content-type') || '';
        if (!contentType.includes('text/html')) {
          return respondFn(200, { title: '', error: 'Not an HTML page' });
        }
        // Read only the first 32KB to find the title
        const reader = res.body.getReader();
        const decoder = new TextDecoder();
        let html = '';
        while (html.length < 32768) {
          const { done, value } = await reader.read();
          if (done) break;
          html += decoder.decode(value, { stream: true });
          // Early exit if we've found </head> or </title>
          if (html.includes('</head>') || html.includes('</title>')) break;
        }
        reader.cancel().catch(() => {});
        
        // Try og:title first, then <title>
        let title = '';
        const ogMatch = html.match(/<meta[^>]+property=["']og:title["'][^>]+content=["']([^"']+)["']/i)
          || html.match(/<meta[^>]+content=["']([^"']+)["'][^>]+property=["']og:title["']/i);
        if (ogMatch) {
          title = ogMatch[1];
        } else {
          const titleMatch = html.match(/<title[^>]*>([^<]+)<\/title>/i);
          if (titleMatch) title = titleMatch[1];
        }
        // Decode HTML entities
        title = title.replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&#x27;/g, "'").replace(/&#x2F;/g, '/').trim();
        return respondFn(200, { title });
      } catch (e) {
        if (e.name === 'AbortError') {
          return respondFn(200, { title: '', error: 'Request timed out' });
        }
        return respondFn(200, { title: '', error: 'Failed to fetch URL' });
      }
    }

    let database;
    try {
      database = await connectFn();
      const posts = database.db.collection("news-posts");
      const comments = database.db.collection("news-comments");
      const votes = database.db.collection("news-votes");

      await ensureIndexes(posts, comments, votes);

      if (event.httpMethod === "GET") {
        if (route === "posts") {
          const limit = Math.min(parseInt(event.queryStringParameters?.limit || "30", 10), 100);
          const sort = event.queryStringParameters?.sort === "new" ? { when: -1 } : { score: -1, when: -1 };
          const includeRecentComments = parseInt(event.queryStringParameters?.includeRecentComments || "0", 10);
          // Show all posts except "dead"
          const docs = await posts.find({ status: { $ne: "dead" } }).sort(sort).limit(limit).toArray();
          const withCounts = await attachCommentCounts(docs, comments);
          
          // Optionally include recent comments for each post (with hydrated handles)
          if (includeRecentComments > 0) {
            const handles = database.db.collection("@handles");
            for (const post of withCounts) {
              const recentComments = await comments
                .find({ postCode: post.code, status: { $ne: "dead" } })
                .sort({ when: -1 })
                .limit(includeRecentComments)
                .toArray();
              
              // Hydrate handles: look up user subs in @handles collection
              const userSubs = recentComments.map(c => c.user).filter(Boolean);
              const handleDocs = userSubs.length > 0 
                ? await handles.find({ _id: { $in: userSubs } }).toArray()
                : [];
              const handleMap = new Map(handleDocs.map(h => [h._id, h.handle]));
              
              post.recentComments = recentComments.map(c => ({
                text: c.text,
                handle: c.user ? (handleMap.get(c.user) || 'anon') : 'anon',
                when: c.when,
              }));
            }
          }
          
          return respondFn(200, { posts: withCounts });
        }
        
        // Live updates endpoint - check for new posts since a timestamp
        if (route === "updates") {
          const since = event.queryStringParameters?.since;
          if (!since) {
            return respondFn(400, { error: "Missing 'since' timestamp parameter" });
          }
          const sinceDate = new Date(parseInt(since, 10));
          const newPosts = await posts.find({ 
            status: "live", 
            when: { $gt: sinceDate } 
          }).sort({ when: -1 }).limit(10).toArray();
          const withCounts = await attachCommentCounts(newPosts, comments);
          
          // Return count and latest timestamp for efficient polling
          const latestWhen = newPosts.length > 0 ? newPosts[0].when.getTime() : parseInt(since, 10);
          return respondFn(200, { 
            newPosts: newPosts.length,
            posts: withCounts,
            latestWhen,
          });
        }
        
        return respondFn(404, { error: "Not found" });
      }

      if (event.httpMethod === "POST") {
        if (route === "submit") {
          const user = await requireUserWith(event);
          const body = parseBody(event);
          const title = sanitizeText(body.title, MAX_TITLE);
          const text = sanitizeText(body.text, MAX_TEXT);
          const url = normalizeUrl(body.url);

          if (!title) {
            return respondFn(400, { error: "Title required" });
          }

          // Check rate limit - 1 post per 24 hours (admins exempt)
          const isAdmin = await hasAdmin(user, "aesthetic");
          if (!isAdmin) {
            const rateLimitCutoff = new Date(Date.now() - RATE_LIMIT_HOURS * 60 * 60 * 1000);
            const recentPost = await posts.findOne({
              user: user.sub,
              when: { $gte: rateLimitCutoff },
              status: { $ne: "dead" },
            });
            if (recentPost) {
              const hoursAgo = Math.round((Date.now() - recentPost.when.getTime()) / (60 * 60 * 1000));
              const hoursLeft = RATE_LIMIT_HOURS - hoursAgo;
              return respondFn(429, { 
                error: `You can only post once every ${RATE_LIMIT_HOURS} hours. Try again in ${hoursLeft} hour${hoursLeft === 1 ? '' : 's'}.`,
                retryAfter: hoursLeft * 60 * 60,
              });
            }
          }

          const shortCode = await generateUniqueCodeFn(posts, { mode: "random" });
          const now = new Date();
          const code = shortCode; // e.g., "icd" (no prefix sigil, like paintings)
          const doc = {
            code,
            title,
            url,
            text,
            user: user.sub,
            when: now,
            updated: now,
            score: 1,
            commentCount: 0,
            status: "live",
          };

          const insertResult = await posts.insertOne(doc);
          const postId = insertResult.insertedId.toString();

          await votes.insertOne({
            itemType: "post",
            itemId: code,
            user: user.sub,
            when: now,
          });

          // Sync to ATProto PDS using the user's account (non-blocking)
          createNewsOnAtproto(database, user.sub, {
            headline: title,
            body: text || null,
            link: url || null,
            when: now,
          }, postId).then(atprotoResult => {
            if (atprotoResult.rkey) {
              posts.updateOne(
                { code },
                { $set: { atproto: { rkey: atprotoResult.rkey, uri: atprotoResult.uri, did: atprotoResult.did } } }
              ).catch(e => console.error('Failed to save ATProto rkey:', e));
            }
          }).catch(e => console.error('ATProto sync error:', e));

          const redirectTo = `${resolveBasePath(event)}/item/${code}`;
          if (wantsHtml(event)) {
            return redirect(redirectTo);
          }
          return respondFn(200, { ok: true, redirect: redirectTo, code });
        }

        if (route === "comment") {
          const user = await requireUserWith(event);
          const body = parseBody(event);
          const postCode = sanitizeText(body.postCode, 32);
          const text = sanitizeText(body.text, MAX_TEXT);

          if (!postCode || !text) {
            return respondFn(400, { error: "Please write something before responding." });
          }

          const now = new Date();
          const doc = {
            postCode,
            parentId: body.parentId || null,
            text,
            user: user.sub,
            when: now,
            score: 1,
            status: "live",
          };
          const insertResult = await comments.insertOne(doc);
          const commentId = insertResult.insertedId.toString();
          await votes.insertOne({
            itemType: "comment",
            itemId: commentId,
            user: user.sub,
            when: now,
          });
          await posts.updateOne({ code: postCode }, { $inc: { commentCount: 1 } });

          const redirectTo = `${resolveBasePath(event)}/item/${postCode}`;
          if (wantsHtml(event)) {
            return redirect(redirectTo);
          }
          return respondFn(200, { ok: true, redirect: redirectTo, commentId });
        }

        if (route === "vote") {
          const user = await requireUserWith(event);
          const body = parseBody(event);
          const itemType = body.itemType;
          const itemId = body.itemId;
          const dir = parseInt(body.dir || "1", 10);

          if (!itemType || !itemId || !["post", "comment"].includes(itemType)) {
            return respondFn(400, { error: "Invalid vote" });
          }

          const now = new Date();
          try {
            await votes.insertOne({ itemType, itemId, user: user.sub, when: now });
          } catch (error) {
            if (error.code === 11000) {
              return respondFn(200, { ok: true, duplicate: true });
            }
            throw error;
          }

          if (itemType === "post") {
            await posts.updateOne({ code: itemId }, { $inc: { score: dir } });
          } else {
            const commentId = ObjectIdImpl.isValid(itemId) ? new ObjectIdImpl(itemId) : itemId;
            await comments.updateOne({ _id: commentId }, { $inc: { score: dir } });
          }

          return respondFn(200, { ok: true });
        }

        // Delete content - admins can delete anything, users can delete their own
        if (route === "delete") {
          const user = await requireUserWith(event);
          const isAdmin = await hasAdmin(user, "aesthetic");

          const body = parseBody(event);
          const itemType = body.itemType;
          const itemId = body.itemId;

          if (!itemType || !itemId || !["post", "comment"].includes(itemType)) {
            return respondFn(400, { error: "Invalid delete request" });
          }

          if (itemType === "post") {
            const post = await posts.findOne({ code: itemId });
            if (!post) {
              return respondFn(404, { error: "Post not found" });
            }
            // Check permission: admin OR post owner
            if (!isAdmin && post.user !== user.sub) {
              return respondFn(403, { error: "You can only delete your own posts" });
            }
            await posts.updateOne({ code: itemId }, { $set: { status: "dead" } });
            // Also mark all comments on this post as dead
            await comments.updateMany({ postCode: itemId }, { $set: { status: "dead" } });
          } else {
            const commentId = ObjectIdImpl.isValid(itemId) ? new ObjectIdImpl(itemId) : itemId;
            const comment = await comments.findOne({ _id: commentId });
            if (!comment) {
              return respondFn(404, { error: "Comment not found" });
            }
            // Check permission: admin OR comment owner
            if (!isAdmin && comment.user !== user.sub) {
              return respondFn(403, { error: "You can only delete your own comments" });
            }
            const wasLive = comment.status !== "dead";
            await comments.updateOne({ _id: commentId }, { $set: { status: "dead" } });
            if (wasLive && comment.postCode) {
              await posts.updateOne({ code: comment.postCode }, { $inc: { commentCount: -1 } });
              await posts.updateOne({ code: comment.postCode }, { $max: { commentCount: 0 } });
            }
          }

          const redirectTo = itemType === "post" ? resolveBasePath(event) + "/" : null;
          if (wantsHtml(event) && redirectTo) {
            return redirect(redirectTo);
          }
          return respondFn(200, { ok: true, deleted: itemId, redirect: redirectTo });
        }
      }

      return respondFn(404, { error: "Not found" });
    } catch (error) {
      if (error.response) {
        return error.response;
      }
      console.error("news-api error:", error);
      if (database) await database.disconnect();
      return respondFn(500, { error: "Server error" });
    } finally {
      if (database) await database.disconnect();
    }
  };
}

export const handler = createHandler();

export { parseBody };
