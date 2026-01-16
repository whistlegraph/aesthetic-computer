// News (HN-style), 2026.01.15
// Server-rendered HTML for news.aesthetic.computer

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev" || process.env.NETLIFY_DEV === "true";

function escapeHtml(value = "") {
  return String(value)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function formatDate(date) {
  if (!date) return "";
  const d = new Date(date);
  const now = Date.now();
  const diff = Math.max(0, now - d.getTime());
  const mins = Math.floor(diff / 60000);
  if (mins < 1) return "just now";
  if (mins < 60) return `${mins}m ago`;
  const hours = Math.floor(mins / 60);
  if (hours < 24) return `${hours}h ago`;
  const days = Math.floor(hours / 24);
  return `${days}d ago`;
}

function parseRoute(event) {
  const route = event.queryStringParameters?.path || "";
  const clean = route.replace(/^\/+/, "").replace(/\/+$/, "");
  return clean;
}

function isSubdomainRequest(event) {
  const host = event.headers?.host || "";
  return host.startsWith("news.aesthetic.computer");
}

function layout({ title, body, assetBase }) {
  return `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>${escapeHtml(title)}</title>
    <link rel="icon" href="${assetBase}/favicon.svg" type="image/svg+xml" />
    <link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css">
    <link rel="stylesheet" href="${assetBase}/main.css" />
    <script src="https://cdn.auth0.com/js/auth0-spa-js/2.0/auth0-spa-js.production.js"></script>
  </head>
  <body>
    ${body}
    <script src="${assetBase}/client.js" defer></script>
  </body>
</html>`;
}

function header(basePath) {
  const homeHref = basePath ? `${basePath}/` : "/";
  return `
  <header class="news-header">
    <div class="news-logo">
      <a href="${homeHref}">Aesthetic News</a>
    </div>
    <nav class="news-nav">
      <a href="${homeHref}">top</a>
      <span>•</span>
      <a href="${basePath}/new">new</a>
      <span>•</span>
      <a href="${basePath}/submit">submit</a>
    </nav>
    <div class="news-auth">
      <button id="news-login-btn" class="header-login-btn">login</button>
      <button id="news-signup-btn" class="header-login-btn header-signup-btn">i'm new</button>
      <div id="news-user-menu" class="header-user-menu" style="display:none;">
        <span id="news-user-handle" class="header-user-handle">@anon</span>
        <span class="header-menu-arrow">▾</span>
        <div class="header-user-dropdown" id="news-user-dropdown">
          <button id="news-logout-btn" class="header-logout-btn">logout</button>
        </div>
      </div>
    </div>
  </header>`;
}

function renderPostRow(post, idx, basePath) {
  const title = escapeHtml(post.title || "(untitled)");
  const url = post.url ? escapeHtml(post.url) : "";
  const domain = post.url ? new URL(post.url).hostname.replace(/^www\./, "") : "";
  const itemUrl = `${basePath}/item/${post.code}`;
  return `
  <div class="news-row">
    <div class="news-rank">${idx + 1}.</div>
    <div class="news-main">
      <div class="news-title">
        <a href="${url || itemUrl}" ${url ? 'target="_blank" rel="noreferrer"' : ""}>${title}</a>
        ${domain ? `<span class="news-domain">(${domain})</span>` : ""}
      </div>
      <div class="news-meta">
        <span>${post.score || 0} points</span>
        <span>by ${escapeHtml(post.handle || "@anon")}</span>
        <span>${formatDate(post.when)}</span>
        <span><a href="${itemUrl}">${post.commentCount || 0} comments</a></span>
      </div>
    </div>
    <div class="news-vote">
      <form data-news-action="vote" method="post" action="/api/news/vote">
        <input type="hidden" name="itemType" value="post" />
        <input type="hidden" name="itemId" value="${post.code}" />
        <input type="hidden" name="dir" value="1" />
        <button type="submit" class="news-vote-btn">▲</button>
      </form>
    </div>
  </div>`;
}

function renderComment(comment) {
  return `
  <div class="news-comment">
    <div class="news-comment-meta">
      <span>${escapeHtml(comment.handle || "@anon")}</span>
      <span>${formatDate(comment.when)}</span>
    </div>
    <div class="news-comment-body">${escapeHtml(comment.text || "")}</div>
  </div>`;
}

async function fetchPosts(database, { sort = "top", limit = 30 }) {
  const posts = database.db.collection("news-posts");
  const sortSpec = sort === "new" ? { when: -1 } : { score: -1, when: -1 };
  const docs = await posts.find({ status: { $ne: "dead" } }).sort(sortSpec).limit(limit).toArray();
  return docs;
}

async function hydrateHandles(database, docs) {
  const handles = database.db.collection("@handles");
  const subs = docs.map((doc) => doc.user).filter(Boolean);
  if (subs.length === 0) return docs;
  const handleDocs = await handles.find({ _id: { $in: subs } }).toArray();
  const map = new Map(handleDocs.map((h) => [h._id, h.handle]));
  return docs.map((doc) => ({
    ...doc,
    handle: doc.user ? `@${map.get(doc.user) || "anon"}` : "@anon",
  }));
}

async function renderFrontPage(database, basePath, sort) {
  const posts = await fetchPosts(database, { sort, limit: 30 });
  const hydrated = await hydrateHandles(database, posts);
  const rows = hydrated.map((post, idx) => renderPostRow(post, idx, basePath)).join("\n");
  return `
  ${header(basePath)}
  <main class="news-main">
    <div class="news-list">
      ${rows || '<div class="news-empty">No posts yet.</div>'}
    </div>
  </main>`;
}

async function renderItemPage(database, basePath, code) {
  const posts = database.db.collection("news-posts");
  const comments = database.db.collection("news-comments");
  const post = await posts.findOne({ code });
  if (!post) {
    return `${header(basePath)}<main class="news-main"><p>Post not found.</p></main>`;
  }
  const hydratedPost = (await hydrateHandles(database, [post]))[0];
  const commentDocs = await comments
    .find({ postCode: code, status: { $ne: "dead" } })
    .sort({ when: 1 })
    .toArray();
  const hydratedComments = await hydrateHandles(database, commentDocs);
  const commentsHtml = hydratedComments.map((c) => renderComment(c)).join("\n");

  return `
  ${header(basePath)}
  <main class="news-main">
    <section class="news-item">
      ${renderPostRow(hydratedPost, 0, basePath)}
      ${post.text ? `<div class="news-text">${escapeHtml(post.text)}</div>` : ""}
    </section>
    <section class="news-comment-form">
      <form id="news-comment-form" data-news-action="comment" method="post" action="/api/news/comment">
        <input type="hidden" name="postCode" value="${escapeHtml(code)}" />
        <textarea name="text" rows="6" placeholder="add a comment..."></textarea>
        <button type="submit">comment</button>
      </form>
    </section>
    <section class="news-comments">
      ${commentsHtml || '<div class="news-empty">No comments yet.</div>'}
    </section>
  </main>`;
}

async function renderSubmitPage(basePath) {
  return `
  ${header(basePath)}
  <main class="news-main">
    <section class="news-submit">
      <form id="news-submit-form" data-news-action="submit" method="post" action="/api/news/submit">
        <label>title</label>
        <input type="text" name="title" required />
        <label>url</label>
        <input type="url" name="url" placeholder="https://" />
        <label>text</label>
        <textarea name="text" rows="6"></textarea>
        <button type="submit">submit</button>
      </form>
    </section>
  </main>`;
}

export function createHandler({ connect: connectFn = connect, respond: respondFn = respond } = {}) {
  return async function handler(event) {
    if (event.httpMethod !== "GET") {
      return respondFn(405, { error: "Method not allowed" });
    }

    const basePath = isSubdomainRequest(event) ? "" : "/news.aesthetic.computer";
    const assetBase = "/news.aesthetic.computer";
    const route = parseRoute(event);

    let database;
    try {
      database = await connectFn();
      let body;

      if (!route || route === "") {
        body = await renderFrontPage(database, basePath, "top");
      } else if (route === "new") {
        body = await renderFrontPage(database, basePath, "new");
      } else if (route.startsWith("item/")) {
        const code = route.split("/").slice(1).join("/");
        body = await renderItemPage(database, basePath, code);
      } else if (route === "submit") {
        body = await renderSubmitPage(basePath);
      } else {
        body = `${header(basePath)}<main class="news-main"><p>Page not found.</p></main>`;
      }

      const html = layout({ title: "news.aesthetic.computer", body, assetBase });
      await database.disconnect();
      return respondFn(200, html, { "Content-Type": "text/html" });
    } catch (error) {
      console.error("News render error:", error);
      if (database) await database.disconnect();
      return respondFn(500, "News render error", { "Content-Type": "text/plain" });
    }
  };
}

export const handler = createHandler();
