// News (HN-style), 2026.01.15
// Server-rendered HTML for news.aesthetic.computer

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

// Dev mode: show live reload indicator
const dev = process.env.CONTEXT === "dev" || 
            process.env.NETLIFY_DEV === "true" ||
            process.env.NODE_ENV === "development";

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
    <div class="news-wrapper">
      ${body}
    </div>
    <script src="${assetBase}/client.js" defer></script>
  </body>
</html>`;
}

function header(basePath) {
  const homeHref = basePath ? `${basePath}/` : "/";
  const connectivityHtml = dev ? `
    <div class="news-connectivity header-connectivity" id="news-connectivity">
      <div class="connectivity-dot disconnected" id="connectivity-dot"></div>
      <span id="connectivity-label">offline</span>
    </div>` : '';
  return `
  <header class="news-header">
    <div class="news-logo">
      <a href="${homeHref}" class="news-logo-icon">A</a>
      <a href="${homeHref}"><b>Aesthetic News</b></a>
    </div>
    ${connectivityHtml}
    <div class="news-auth">
      <button id="news-login-btn" class="header-login-btn">Log In</button>
      <button id="news-signup-btn" class="header-login-btn header-signup-btn">I'm New</button>
      <div id="news-user-menu" class="header-user-menu" style="display:none;">
        <button id="news-user-handle" class="header-user-handle" type="button">@anon</button>
      </div>
    </div>
  </header>`;
}

function footer() {
  return `
  <footer class="news-footer">
    <div class="news-footer-links">
      <a href="https://aesthetic.computer/list" class="news-modal-link" data-modal-url="https://aesthetic.computer/list">List</a>
      <span>|</span>
      <a href="https://give.aesthetic.computer" class="news-external-link" target="_blank" rel="noopener">Give</a>
      <span>|</span>
      <a href="https://prompt.ac/commits" class="news-modal-link" data-modal-url="https://prompt.ac/commits">Commits</a>
    </div>
  </footer>`;
}

function renderPostRow(post, idx, basePath) {
  const title = escapeHtml(post.title || "(untitled)");
  const url = post.url ? escapeHtml(post.url) : "";
  const domain = post.url ? new URL(post.url).hostname.replace(/^www\./, "") : "";
  const itemUrl = `${basePath}/item/${post.code}`;
  return `
  <div class="news-row">
    <div class="news-rank">${idx + 1}.</div>
    <div class="news-vote">
      <form data-news-action="vote" method="post" action="/api/news/vote">
        <input type="hidden" name="itemType" value="post" />
        <input type="hidden" name="itemId" value="${post.code}" />
        <input type="hidden" name="dir" value="1" />
        <button type="submit" class="news-vote-btn">▲</button>
      </form>
    </div>
    <div class="news-content">
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
  const emptyState = `
    <div class="news-empty">
      <p>No posts yet.</p>
      <a href="${basePath}/report" class="news-report-link">Report the News</a>
    </div>`;
  return `
  ${header(basePath)}
  <main class="news-main">
    <div class="news-list">
      ${rows || emptyState}
    </div>
  </main>
  ${footer()}`;
}

async function renderItemPage(database, basePath, code) {
  const posts = database.db.collection("news-posts");
  const comments = database.db.collection("news-comments");
  const post = await posts.findOne({ code });
  if (!post) {
    return `${header(basePath)}<main class="news-main"><p>Post not found.</p></main>${footer()}`;
  }
  const hydratedPost = (await hydrateHandles(database, [post]))[0];
  const commentDocs = await comments
    .find({ postCode: code, status: { $ne: "dead" } })
    .sort({ when: 1 })
    .toArray();
  const hydratedComments = await hydrateHandles(database, commentDocs);
  const commentsHtml = hydratedComments.map((c) => renderComment(c)).join("\n");

  const title = escapeHtml(hydratedPost.title || "(untitled)");
  const url = hydratedPost.url ? escapeHtml(hydratedPost.url) : "";
  const domain = hydratedPost.url ? new URL(hydratedPost.url).hostname.replace(/^www\./, "") : "";

  return `
  ${header(basePath)}
  <main class="news-main">
    <table class="news-item-table" border="0" cellpadding="0" cellspacing="0">
      <tr class="news-item-row">
        <td valign="top" class="news-item-vote">
          <form data-news-action="vote" method="post" action="/api/news/vote">
            <input type="hidden" name="itemType" value="post" />
            <input type="hidden" name="itemId" value="${hydratedPost.code}" />
            <input type="hidden" name="dir" value="1" />
            <button type="submit" class="news-vote-btn">▲</button>
          </form>
        </td>
        <td class="news-item-content">
          <span class="news-item-title">
            <a href="${url || '#'}" ${url ? 'target="_blank" rel="noreferrer"' : ""}>${title}</a>
            ${domain ? `<span class="news-domain">(${domain})</span>` : ""}
          </span>
          <div class="news-item-meta">
            ${hydratedPost.score || 0} points by ${escapeHtml(hydratedPost.handle || "@anon")} ${formatDate(hydratedPost.when)}
          </div>
        </td>
      </tr>
    </table>
    ${hydratedPost.text ? `<div class="news-text">${escapeHtml(hydratedPost.text)}</div>` : ""}
    <form id="news-comment-form" class="news-comment-form" data-news-action="comment" method="post" action="/api/news/comment">
      <input type="hidden" name="postCode" value="${escapeHtml(code)}" />
      <div class="news-comment-guidelines">
        <p>Say something interesting or ask a question. Be nice.</p>
      </div>
      <textarea name="text" rows="6" cols="80" placeholder="Add a comment..."></textarea>
      <button type="submit">add comment</button>
    </form>
    <div class="news-comments">
      ${commentsHtml || ''}
    </div>
  </main>
  ${footer()}`;
}

async function renderReportPage(basePath) {
  return `
  ${header(basePath)}
  <main class="news-main">
    <section class="news-report">
      <h2>Report the News</h2>
      <div id="news-login-prompt" class="news-login-prompt">
        <p>You need to <button id="news-prompt-login" class="news-inline-login">log in</button> to post.</p>
        <p class="news-handle-note">You'll also need a @handle — get one at <a href="https://aesthetic.computer" target="_blank">aesthetic.computer</a></p>
      </div>
      <form id="news-submit-form" data-news-action="submit" method="post" action="/api/news/submit">
        <label>Headline</label>
        <input type="text" name="title" required placeholder="What's the story?" />
        <div class="news-either-or">
          <div class="news-pick-hint">pick one or both</div>
          <div class="news-field-group">
            <label>Source URL</label>
            <input type="url" name="url" placeholder="https://" />
          </div>
          <div class="news-field-group">
            <label>Story</label>
            <textarea name="text" rows="4" placeholder="Share context, commentary, or tell your own story..."></textarea>
          </div>
        </div>
        <button type="submit">Post</button>
      </form>
      <div class="news-guidelines news-guidelines-below">
        <p>Share something interesting with the community.</p>
        <ul>
          <li>Links to creative tools, code, art, and experiments are welcome.</li>
          <li>Self-promotion is fine if it's genuinely interesting.</li>
          <li>Be curious. Be kind. Keep it weird.</li>
        </ul>
      </div>
    </section>
  </main>
  ${footer()}`;
}

export function createHandler({ connect: connectFn = connect, respond: respondFn = respond } = {}) {
  return async function handler(event) {
    if (event.httpMethod !== "GET") {
      return respondFn(405, { error: "Method not allowed" });
    }

    const basePath = isSubdomainRequest(event) ? "" : "/news.aesthetic.computer";
    const assetBase = "/news.aesthetic.computer";
    const route = parseRoute(event);
    let title = "Aesthetic News";

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
      } else if (route === "report" || route === "submit") {
        body = await renderReportPage(basePath);
      } else {
        body = `${header(basePath)}<main class="news-main"><p>Page not found.</p></main>${footer()}`;
      }

      const html = layout({ title, body, assetBase });
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
