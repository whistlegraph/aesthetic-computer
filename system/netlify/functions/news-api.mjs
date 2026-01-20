// News API, 2026.01.15
// JSON + form handlers for posts, comments, votes

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { ObjectId } from "mongodb";

// Admin users who can delete/censor content
const ADMIN_SUBS = [process.env.ADMIN_SUB].filter(Boolean);

const MAX_TITLE = 200;
const MAX_TEXT = 5000;
const RATE_LIMIT_HOURS = 24; // Users can only post once per 24 hours

// "Aesthetic Network" domains - links to these are free to post
const AESTHETIC_NETWORK_DOMAINS = [
  "aesthetic.computer",
  "prompt.ac", 
  "kidlisp.com",
  "sotce.net",
  "false.work",
  "whistlegraph.com",
  "digitpain.com",
  "jas.life",
];

// Check if a URL is within the Aesthetic Network (free to post)
function isAestheticNetwork(url) {
  if (!url) return true; // No URL = text-only post = free
  try {
    const hostname = new URL(url).hostname.toLowerCase();
    return AESTHETIC_NETWORK_DOMAINS.some(domain => 
      hostname === domain || hostname.endsWith(`.${domain}`)
    );
  } catch {
    return false;
  }
}

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
          const docs = await posts.find({ status: "live" }).sort(sort).limit(limit).toArray();
          return respondFn(200, { posts: docs });
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
          
          // Return count and latest timestamp for efficient polling
          const latestWhen = newPosts.length > 0 ? newPosts[0].when.getTime() : parseInt(since, 10);
          return respondFn(200, { 
            newPosts: newPosts.length,
            posts: newPosts,
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

          // Check if URL requires troll toll (external links)
          const needsToll = url && !isAestheticNetwork(url) && !isAdmin;

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
            status: needsToll ? "pending-toll" : "live",
            tollRequired: needsToll,
          };

          await posts.insertOne(doc);
          
          // Only create initial vote if post is live
          if (!needsToll) {
            await votes.insertOne({
              itemType: "post",
              itemId: code,
              user: user.sub,
              when: now,
            });
          }

          // If toll required, return special response
          if (needsToll) {
            return respondFn(402, { 
              tollRequired: true, 
              code,
              message: "🧌 External links require a Troll Toll ($2) to post.",
              checkoutUrl: `/api/news/toll`,
            });
          }

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
          await comments.insertOne(doc);
          await votes.insertOne({
            itemType: "comment",
            itemId: doc._id.toString(),
            user: user.sub,
            when: now,
          });
          await posts.updateOne({ code: postCode }, { $inc: { commentCount: 1 } });

          const redirectTo = `${resolveBasePath(event)}/item/${postCode}`;
          if (wantsHtml(event)) {
            return redirect(redirectTo);
          }
          return respondFn(200, { ok: true, redirect: redirectTo });
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
            await comments.updateOne({ _id: commentId }, { $set: { status: "dead" } });
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
