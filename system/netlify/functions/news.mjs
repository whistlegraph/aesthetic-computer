// News (HN-style), 2026.01.15
// Server-rendered HTML for news.aesthetic.computer

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

// Dev mode: show live reload indicator
const dev = process.env.CONTEXT === "dev" || 
            process.env.NETLIFY_DEV === "true" ||
            process.env.NODE_ENV === "development";

// Detect KidLisp URLs and extract the $code
function parseKidlispUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    // Match kidlisp.com/$CODE or *.kidlisp.com/$CODE
    if (!u.hostname.includes('kidlisp.com')) return null;
    // Path should be /$CODE (with dollar sign in URL)
    const match = u.pathname.match(/^\/\$?([a-zA-Z0-9_-]+)$/);
    if (!match) return null;
    return match[1]; // Return just the code without $
  } catch { return null; }
}

// Detect YouTube URLs and extract video ID
function parseYouTubeUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    // youtube.com/watch?v=VIDEO_ID
    if (u.hostname.includes('youtube.com') && u.pathname === '/watch') {
      return u.searchParams.get('v');
    }
    // youtu.be/VIDEO_ID
    if (u.hostname === 'youtu.be') {
      const match = u.pathname.match(/^\/([a-zA-Z0-9_-]+)/);
      return match ? match[1] : null;
    }
    // youtube.com/embed/VIDEO_ID
    if (u.hostname.includes('youtube.com') && u.pathname.startsWith('/embed/')) {
      const match = u.pathname.match(/^\/embed\/([a-zA-Z0-9_-]+)/);
      return match ? match[1] : null;
    }
    return null;
  } catch { return null; }
}

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

// Generate ATProto permalink URLs
function atprotoPermalink(atprotoData) {
  if (!atprotoData?.uri) return null;
  // at://did:plc:xxx/computer.aesthetic.news/rkey → pds.ls viewer
  const pdsLsUrl = `https://pds.ls/${atprotoData.uri}`;
  return { pdsLs: pdsLsUrl, uri: atprotoData.uri };
}

function renderHandle(handle) {
  const safeHandle = escapeHtml(handle || "@anon");
  // Extract username without @ for the URL
  const username = safeHandle.startsWith("@") ? safeHandle.slice(1) : safeHandle;
  if (username === "anon") return safeHandle;
  const profileUrl = `https://aesthetic.computer/${username}`;
  return `<a href="${profileUrl}" class="news-modal-link news-handle-link" data-modal-url="${profileUrl}">${safeHandle}</a>`;
}

function parseRoute(event) {
  // Extract route from path, stripping the function prefix
  let path = event.path || "";
  // Handle both direct calls and routed calls via /news.aesthetic.computer/ prefix
  const prefixes = ["/.netlify/functions/news", "/news.aesthetic.computer"];
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
  const clean = path.replace(/^\/+/, "").replace(/\/+$/, "");
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
    <link rel="icon" href="https://aesthetic.computer${assetBase}/favicon.svg" type="image/svg+xml" />
    <link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@7.0.0/css/flag-icons.min.css">
    <link rel="stylesheet" href="https://aesthetic.computer${assetBase}/main.css" />
    <script src="https://cdn.auth0.com/js/auth0-spa-js/2.0/auth0-spa-js.production.js"></script>
  </head>
  <body>
    <div class="news-wrapper">
      ${body}
    </div>
    <script src="https://aesthetic.computer${assetBase}/client.js" defer></script>
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
      <a href="${homeHref}"><b data-i18n="site-title">Aesthetic News</b></a>
    </div>
    ${connectivityHtml}
    <div class="news-auth">
      <button id="news-login-btn" class="header-login-btn" data-i18n="log-in">Log In</button>
      <button id="news-signup-btn" class="header-login-btn header-signup-btn" data-i18n="im-new">I'm New</button>
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
      <a href="https://aesthetic.computer/list" class="news-modal-link" data-modal-url="https://aesthetic.computer/list" data-i18n="list">List</a>
      <span>|</span>
      <a href="https://give.aesthetic.computer" class="news-external-link" target="_blank" rel="noopener" data-i18n="give">Give</a>
      <span>|</span>
      <a href="https://prompt.ac/commits" class="news-modal-link" data-modal-url="https://prompt.ac/commits" data-i18n="commits">Commits</a>
      <span>|</span>
      <div class="news-lang-selector" id="news-lang-selector">
        <button type="button" class="lang-trigger">
          <span class="fi fi-gb lang-flag" data-lang-flag></span>
          <span class="lang-text">EN</span>
          <span class="lang-arrow">▾</span>
        </button>
        <div class="lang-dropdown">
          <div class="lang-option" data-lang="en"><span class="fi fi-gb"></span> English</div>
          <div class="lang-option" data-lang="da"><span class="fi fi-dk"></span> Dansk</div>
        </div>
      </div>
    </div>
  </footer>`;
}

function renderPostRow(post, idx, basePath) {
  const title = escapeHtml(post.title || "(untitled)");
  const url = post.url ? escapeHtml(post.url) : "";
  // Show full URL path, not just TLD
  const displayUrl = post.url ? (() => {
    try {
      const u = new URL(post.url);
      // Show host + pathname (truncate if too long)
      let path = u.hostname.replace(/^www\./, "") + u.pathname;
      if (path.endsWith("/")) path = path.slice(0, -1);
      if (path.length > 50) path = path.slice(0, 47) + "...";
      return path;
    } catch { return ""; }
  })() : "";
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
        ${displayUrl ? `<span class="news-domain">(<a href="${url}" target="_blank" rel="noreferrer">${displayUrl}</a>)</span>` : ""}
      </div>
      <div class="news-meta">
        <span>${post.score || 0} points</span>
        <span>by ${renderHandle(post.handle)}</span>
        <span><a href="${itemUrl}">${formatDate(post.when)}</a></span>
        <span><a href="${itemUrl}">${post.commentCount || 0} comments</a></span>
      </div>
    </div>
  </div>`;
}

function renderComment(comment) {
  const commentId = comment._id?.toString() || '';
  return `
  <div class="news-comment" data-comment-id="${commentId}">
    <div class="news-comment-meta">
      <span>${renderHandle(comment.handle)}</span>
      <span>${formatDate(comment.when)}</span>
      <form class="news-admin-delete" data-news-action="delete" data-item-type="comment" data-item-id="${commentId}" data-handle="${escapeHtml(comment.handle?.replace('@', '') || '')}" method="post" action="/api/news/delete" style="display:none;">
        <input type="hidden" name="itemType" value="comment" />
        <input type="hidden" name="itemId" value="${commentId}" />
        <button type="submit" class="news-delete-btn" title="Delete comment">
          <svg class="news-x-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round">
            <line x1="6" y1="6" x2="18" y2="18"/><line x1="18" y1="6" x2="6" y2="18"/>
          </svg>
        </button>
      </form>
    </div>
    <div class="news-comment-body">${escapeHtml(comment.text || "")}</div>
  </div>`;
}

async function applyCommentCounts(database, posts) {
  if (!posts || posts.length === 0) return posts || [];
  const comments = database.db.collection("news-comments");
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

async function fetchPosts(database, { sort = "top", limit = 30 }) {
  const posts = database.db.collection("news-posts");
  const sortSpec = sort === "new" ? { when: -1 } : { score: -1, when: -1 };
  const docs = await posts.find({ status: { $ne: "dead" } }).sort(sortSpec).limit(limit).toArray();
  return applyCommentCounts(database, docs);
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
      <p data-i18n="no-posts">No posts yet.</p>
    </div>`;
  const reportLink = `
    <div class="news-report-cta">
      <a href="${basePath}/report" class="news-report-link" data-i18n="report-the-news">Report the News</a>
    </div>`;
  return `
  ${header(basePath)}
  <main class="news-main">
    <div class="news-list">
      ${rows || emptyState}
    </div>
    ${reportLink}
  </main>
  ${footer()}`;
}

async function renderItemPage(database, basePath, code) {
  const posts = database.db.collection("news-posts");
  const comments = database.db.collection("news-comments");
  const post = await posts.findOne({ code });
  if (!post) {
    return {
      title: "Not Found | Aesthetic News",
      body: `${header(basePath)}<main class="news-main"><p>Post not found.</p></main>${footer()}`
    };
  }
  const hydratedPost = (await hydrateHandles(database, [post]))[0];
  const commentDocs = await comments
    .find({ postCode: code, status: { $ne: "dead" } })
    .sort({ when: 1 })
    .toArray();
  const hydratedComments = await hydrateHandles(database, commentDocs);
  const commentsHtml = hydratedComments.map((c) => renderComment(c)).join("\n");

  const postTitle = escapeHtml(hydratedPost.title || "(untitled)");
  const pageTitle = `${hydratedPost.title || "(untitled)"} | Aesthetic News`;
  const url = hydratedPost.url ? escapeHtml(hydratedPost.url) : "";
  // Show full URL path, not just TLD
  const displayUrl = hydratedPost.url ? (() => {
    try {
      const u = new URL(hydratedPost.url);
      let path = u.hostname.replace(/^www\./, "") + u.pathname;
      if (path.endsWith("/")) path = path.slice(0, -1);
      if (path.length > 60) path = path.slice(0, 57) + "...";
      return path;
    } catch { return ""; }
  })() : "";

  // Generate ATProto permalink if available
  const atLinks = atprotoPermalink(hydratedPost.atproto);
  const atLinkHtml = atLinks ? `
            <span class="news-at-link">
              <a href="${atLinks.pdsLs}" target="_blank" rel="noopener" title="View on ATProto (${atLinks.uri})">🔗 AT</a>
            </span>` : '';

  // Check for YouTube embed
  const youtubeId = parseYouTubeUrl(hydratedPost.url);
  const youtubeEmbedHtml = youtubeId ? `
      <div class="news-youtube-embed" data-youtube-id="${youtubeId}">
        <iframe 
          id="youtube-player"
          src="https://www.youtube.com/embed/${youtubeId}?enablejsapi=1" 
          title="YouTube video" 
          frameborder="0" 
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
          allowfullscreen>
        </iframe>
      </div>` : '';

  // Check for KidLisp preview
  const kidlispCode = parseKidlispUrl(hydratedPost.url);
  const kidlispPreviewHtml = kidlispCode ? `
      <div class="news-kidlisp-preview" data-kidlisp-code="${kidlispCode}">
        <a href="${escapeHtml(hydratedPost.url)}" target="_blank" rel="noreferrer" class="news-kidlisp-link">
          <img src="https://oven.aesthetic.computer/grab/webp/320/320/$${kidlispCode}?duration=4000&fps=8&quality=80&density=1" alt="KidLisp preview" class="news-kidlisp-webp" loading="lazy" />
          <span class="news-kidlisp-badge">▶ $${kidlispCode}</span>
        </a>
      </div>` : '';

  const hasMedia = youtubeId || kidlispCode;

  const body = `
  ${header(basePath)}
  <main class="news-main">
    <div class="news-item-header${hasMedia ? ' has-media' : ''}">
      <div class="news-item-info">
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
                <a href="${url || '#'}" ${url ? 'target="_blank" rel="noreferrer"' : ""}>${postTitle}</a>
                ${displayUrl ? `<span class="news-domain">(<a href="${url}" target="_blank" rel="noreferrer">${displayUrl}</a>)</span>` : ""}
              </span>
              <div class="news-item-meta">
                ${hydratedPost.score || 0} points by ${renderHandle(hydratedPost.handle)} ${formatDate(hydratedPost.when)}${atLinkHtml}
                <form class="news-admin-delete" data-news-action="delete" data-item-type="post" data-item-id="${hydratedPost.code}" data-handle="${escapeHtml(hydratedPost.handle?.replace('@', '') || '')}" method="post" action="/api/news/delete" style="display:none;">
                  <input type="hidden" name="itemType" value="post" />
                  <input type="hidden" name="itemId" value="${hydratedPost.code}" />
                  <button type="submit" class="news-delete-btn" title="Delete post">
                    <svg class="news-x-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round">
                      <line x1="6" y1="6" x2="18" y2="18"/><line x1="18" y1="6" x2="6" y2="18"/>
                    </svg>
                  </button>
                </form>
              </div>
            </td>
          </tr>
        </table>
      </div>
    </div>
    ${youtubeEmbedHtml}${kidlispPreviewHtml}
    ${hydratedPost.text ? `<div class="news-text">${escapeHtml(hydratedPost.text)}</div>` : ""}
    <form id="news-comment-form" class="news-comment-form" data-news-action="comment" method="post" action="/api/news/comment"${youtubeId ? ` data-youtube-id="${youtubeId}"` : ''}>
      <input type="hidden" name="postCode" value="${escapeHtml(code)}" />
      <textarea name="text" rows="6" cols="80"></textarea>
      <div class="news-comment-actions">
        <button type="submit" data-i18n="respond">Respond</button>
        ${youtubeId ? `<button type="button" id="insert-timecode-btn" class="news-timecode-btn" title="Insert current video time">@ 0:00</button>` : ''}
      </div>
    </form>
    <div class="news-comments">
      ${commentsHtml || ''}
    </div>
  </main>
  ${footer()}`;
  
  return { title: pageTitle, body };
}

async function renderReportPage(basePath) {
  return `
  ${header(basePath)}
  <main class="news-main">
    <section class="news-report">
      <div class="news-report-header">
        <h2 data-i18n="report-the-news">Report the News</h2>
        <div class="news-lang-selector news-lang-inline" id="news-lang-selector-report">
          <button type="button" class="lang-trigger">
            <span class="fi fi-gb lang-flag" data-lang-flag></span>
            <span class="lang-text">EN</span>
            <span class="lang-arrow">▾</span>
          </button>
          <div class="lang-dropdown">
            <div class="lang-option" data-lang="en"><span class="fi fi-gb"></span> English</div>
            <div class="lang-option" data-lang="da"><span class="fi fi-dk"></span> Dansk</div>
          </div>
        </div>
      </div>
      <div id="news-login-prompt" class="news-login-prompt">
        <p><span data-i18n="login-prompt-1">You need to</span> <button id="news-prompt-login" class="news-inline-login" data-i18n="log-in">log in</button> <span data-i18n="login-prompt-2">to post.</span></p>
        <p class="news-handle-note" data-i18n="handle-note">You'll also need a @handle — get one at <a href="https://aesthetic.computer" target="_blank">aesthetic.computer</a></p>
      </div>
      <form id="news-submit-form" data-news-action="submit" method="post" action="/api/news/submit">
        <label data-i18n="headline">Headline</label>
        <input type="text" name="title" required data-i18n-placeholder="headline-placeholder" placeholder="What's the story?" />
        <div class="news-either-or">
          <div class="news-pick-hint" data-i18n="pick-hint">pick one or both</div>
          <div class="news-field-group">
            <label data-i18n="source-url">Source URL</label>
            <input type="url" name="url" placeholder="https://" />
          </div>
          <div class="news-field-group">
            <label data-i18n="story">Story</label>
            <textarea name="text" rows="4" data-i18n-placeholder="story-placeholder" placeholder="Share context, commentary, or tell your own story..."></textarea>
          </div>
        </div>
        <button type="submit" data-i18n="post">Post</button>
      </form>
      <p class="news-tagline" data-i18n="tagline">Be curious. Be kind. Keep it weird.</p>
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
    console.log("[news] Route debug:", { path: event.path, queryStringParameters: event.queryStringParameters, parsedRoute: route });
    let title = "Aesthetic News";

    let database;
    try {
      database = await connectFn();
      let body;

      if (!route || route === "") {
        title = "Aesthetic News";
        body = await renderFrontPage(database, basePath, "top");
      } else if (route === "new") {
        title = "New | Aesthetic News";
        body = await renderFrontPage(database, basePath, "new");
      } else if (route.startsWith("item/")) {
        const code = route.split("/").slice(1).join("/");
        const result = await renderItemPage(database, basePath, code);
        body = result.body || result;
        title = result.title || "Aesthetic News";
      } else if (route === "report" || route === "submit") {
        title = "Report · Aesthetic News";
        body = await renderReportPage(basePath);
      } else {
        title = "Not Found | Aesthetic News";
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
