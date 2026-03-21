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

// Detect e-flux article URLs
function parseEfluxUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    if (!u.hostname.includes('e-flux.com')) return null;
    // Match /journal/ISSUE/ID/SLUG or /notes/ID/SLUG or /architecture/*/ID/SLUG
    const match = u.pathname.match(/^\/(journal|notes|architecture)\//i);
    if (!match) return null;
    return url; // Return the full URL for fetching
  } catch { return null; }
}

// Detect Instagram URLs
function parseInstagramUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    // Match instagram.com/p/POST_ID or instagram.com/reel/REEL_ID
    if (!u.hostname.includes('instagram.com')) return null;
    const match = u.pathname.match(/^\/(p|reel|reels)\/([a-zA-Z0-9_-]+)/);
    if (!match) return null;
    return url; // Return the full URL for fetching
  } catch { return null; }
}

// Fetch Instagram post via Graph API (requires Facebook App credentials from database)
async function fetchInstagramEmbed(instagramUrl, database) {
  if (!database) {
    console.log('[news] No database connection, falling back to og:image');
    return null;
  }

  try {
    const secrets = await database.db.collection("secrets").findOne({ _id: "facebook" });

    if (!secrets || !secrets.appId || !secrets.appSecret) {
      console.log('[news] Facebook credentials not found in database, falling back to og:image');
      return null;
    }

    const appId = secrets.appId;
    const appSecret = secrets.appSecret;

    // Get app access token
    const tokenUrl = `https://graph.facebook.com/oauth/access_token?client_id=${appId}&client_secret=${appSecret}&grant_type=client_credentials`;
    const tokenRes = await fetch(tokenUrl);
    const tokenData = await tokenRes.json();

    if (!tokenData.access_token) {
      console.error('[news] Failed to get Facebook access token');
      return null;
    }

    // Fetch Instagram oEmbed
    const oembedUrl = `https://graph.facebook.com/v21.0/instagram_oembed?url=${encodeURIComponent(instagramUrl)}&access_token=${tokenData.access_token}`;
    const res = await fetch(oembedUrl);
    const data = await res.json();

    if (data.error) {
      console.log('[news] Instagram API error:', data.error.message);
      return null;
    }

    return {
      html: data.html,
      authorName: data.author_name,
      mediaType: data.media_type, // 'IMAGE' or 'VIDEO'
    };
  } catch (e) {
    console.error('[news] Instagram API error:', e.message);
    return null;
  }
}

// Fetch generic OpenGraph metadata from any URL
async function fetchOgMeta(url) {
  try {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 5000);
    const res = await fetch(url, {
      signal: controller.signal,
      headers: {
        'User-Agent': 'AestheticNewsBot/1.0 (+https://news.aesthetic.computer)',
        'Accept': 'text/html',
      },
      redirect: 'follow',
    });
    clearTimeout(timeout);
    if (!res.ok) return null;
    // Read first 64KB for meta tags
    const reader = res.body.getReader();
    const decoder = new TextDecoder();
    let html = '';
    while (html.length < 65536) {
      const { done, value } = await reader.read();
      if (done) break;
      html += decoder.decode(value, { stream: true });
    }
    reader.cancel().catch(() => {});

    const getOg = (prop) => {
      const m = html.match(new RegExp(`<meta[^>]+(?:property|name)=["']${prop}["'][^>]+content=["']([^"']+)["']`, 'i'))
        || html.match(new RegExp(`<meta[^>]+content=["']([^"']+)["'][^>]+(?:property|name)=["']${prop}["']`, 'i'));
      return m ? m[1].replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '"').replace(/&#39;/g, "'").trim() : '';
    };

    const title = getOg('og:title');
    const description = getOg('og:description') || getOg('description');
    const image = getOg('og:image');
    const siteName = getOg('og:site_name');

    if (!image) return null; // Only show preview if there's an image
    return { title, description, image, siteName };
  } catch (e) {
    console.error('[news] og:image fetch error:', e.message);
    return null;
  }
}

// Fetch e-flux article metadata from og: tags
async function fetchEfluxMeta(url) {
  try {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 5000);
    const res = await fetch(url, {
      signal: controller.signal,
      headers: {
        'User-Agent': 'AestheticNewsBot/1.0 (+https://news.aesthetic.computer)',
        'Accept': 'text/html',
      },
      redirect: 'follow',
    });
    clearTimeout(timeout);
    if (!res.ok) return null;
    // Read first 64KB for meta tags + some body text
    const reader = res.body.getReader();
    const decoder = new TextDecoder();
    let html = '';
    while (html.length < 65536) {
      const { done, value } = await reader.read();
      if (done) break;
      html += decoder.decode(value, { stream: true });
    }
    reader.cancel().catch(() => {});

    const getOg = (prop) => {
      const m = html.match(new RegExp(`<meta[^>]+(?:property|name)=["']${prop}["'][^>]+content=["']([^"']+)["']`, 'i'))
        || html.match(new RegExp(`<meta[^>]+content=["']([^"']+)["'][^>]+(?:property|name)=["']${prop}["']`, 'i'));
      return m ? m[1].replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '"').replace(/&#39;/g, "'").trim() : '';
    };

    const title = getOg('og:title');
    const description = getOg('og:description') || getOg('description');
    const image = getOg('og:image');

    // Try to extract author from RSC payload: "displayname":"NAME"
    let author = '';
    const authorMatch = html.match(/"displayname"\s*:\s*"([^"]+)"/i);
    if (authorMatch) author = authorMatch[1];

    // Try to extract issue & section from og:title (e.g. "Title - Journal #160")
    let section = '';
    let issue = '';
    const titleParts = title.match(/^(.+?)\s*-\s*(Journal|Notes|Architecture)\s*(#\d+)?$/i);
    if (titleParts) {
      section = titleParts[2] || '';
      issue = titleParts[3] || '';
    }

    // Extract first body paragraph from RSC payload
    let excerpt = '';
    const bodyChunks = [];
    const rscRegex = /self\.__next_f\.push\(\[1,"[0-9a-f]+:T[0-9a-f]+,"\]\)<\/script><script>self\.__next_f\.push\(\[1,"((?:[^"\\]|\\.)*)"\]\)/g;
    let rscMatch;
    while ((rscMatch = rscRegex.exec(html)) !== null) {
      bodyChunks.push(rscMatch[1]);
    }
    if (bodyChunks.length > 0) {
      // Join the first chunk, unescape it, strip HTML tags, get first ~600 chars
      const rawBody = bodyChunks[0]
        .replace(/\\u0026(\w+);/g, (_, e) => ({ amp: '&', lt: '<', gt: '>', quot: '"', rsquo: '\u2019', lsquo: '\u2018', ldquo: '\u201c', rdquo: '\u201d', mdash: '\u2014', ndash: '\u2013', eacute: 'é', Eacute: 'É', atilde: 'ã', oacute: 'ó', iacute: 'í', ccedil: 'ç' }[e] || ''))
        .replace(/\\u003c/g, '<').replace(/\\u003e/g, '>')
        .replace(/<[^>]+>/g, ' ')
        .replace(/\s+/g, ' ')
        .trim();
      if (rawBody.length > 20) {
        excerpt = rawBody.slice(0, 600);
        // Trim to last complete sentence or word
        const lastDot = excerpt.lastIndexOf('. ');
        if (lastDot > 200) excerpt = excerpt.slice(0, lastDot + 1);
        else excerpt = excerpt.slice(0, excerpt.lastIndexOf(' ')) + '\u2026';
      }
    }
    if (!excerpt && description) excerpt = description;

    if (!title) return null;
    return { title: titleParts ? titleParts[1].trim() : title, author, image, description, section, issue, excerpt };
  } catch (e) {
    console.error('[news] e-flux fetch error:', e.message);
    return null;
  }
}

// Detect aesthetic.computer piece URLs
function parseAestheticComputerUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    // Match aesthetic.computer/piece or aesthetic.computer/@handle/piece
    if (!u.hostname.includes('aesthetic.computer')) return null;
    // Extract piece name (after removing leading slash)
    const path = u.pathname.replace(/^\//, '');
    // Skip common non-piece paths
    if (!path || path === '' ||
        path.startsWith('api/') ||
        path.startsWith('news.') ||
        path.startsWith('give.') ||
        path.includes('.')) return null;
    // Return the piece path (e.g., "wipe" or "@user/piece" or "piece:param")
    return path.split('?')[0]; // Remove query params
  } catch { return null; }
}

// Detect Imgur URLs and normalize to direct image URL
function parseImgurUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    if (!u.hostname.includes('imgur.com')) return null;

    // imgur.com/gallery/ID or imgur.com/a/ID (album) - use first image
    // imgur.com/ID - single image
    // i.imgur.com/ID.ext - direct image

    // Already a direct image URL
    if (u.hostname === 'i.imgur.com') {
      return url;
    }

    // Extract image ID from various Imgur URL formats
    const galleryMatch = u.pathname.match(/^\/(?:gallery|a)\/([a-zA-Z0-9]+)/);
    const imageMatch = u.pathname.match(/^\/([a-zA-Z0-9]+)(?:\.|$)/);

    const imageId = galleryMatch ? galleryMatch[1] : (imageMatch ? imageMatch[1] : null);

    if (imageId) {
      // Return direct image URL (Imgur serves JPEG by default, will redirect to actual format)
      return `https://i.imgur.com/${imageId}.jpg`;
    }

    return null;
  } catch { return null; }
}

// Detect direct image URLs
function parseDirectImageUrl(url) {
  if (!url) return null;
  try {
    const u = new URL(url);
    const path = u.pathname.toLowerCase();

    // Check for common image extensions
    const imageExtensions = ['.jpg', '.jpeg', '.png', '.gif', '.webp', '.bmp', '.svg'];
    const hasImageExt = imageExtensions.some(ext => path.endsWith(ext));

    if (hasImageExt) {
      return url;
    }

    return null;
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

function layout({ title, body, assetBase, assetOrigin }) {
  const origin = assetOrigin || "";
  return `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>${escapeHtml(title)}</title>
    <link rel="icon" href="${origin}${assetBase}/favicon.svg" type="image/svg+xml" />
    <link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@7.0.0/css/flag-icons.min.css">
    <link rel="stylesheet" href="${origin}${assetBase}/main.css" />
    <script src="https://cdn.auth0.com/js/auth0-spa-js/2.0/auth0-spa-js.production.js"></script>
  </head>
  <body>
    <div class="news-wrapper">
      ${body}
    </div>
    <script src="${origin}${assetBase}/client.js" defer></script>
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
  // Show URL with reasonable truncation for homepage list
  const displayUrl = post.url ? (() => {
    try {
      const u = new URL(post.url);
      let path = u.hostname.replace(/^www\./, "") + u.pathname;
      if (path.endsWith("/")) path = path.slice(0, -1);
      if (path.length > 40) path = path.slice(0, 37) + "...";
      return path;
    } catch { return ""; }
  })() : "";
  const itemUrl = `${basePath}/${post.code}`;
  return `
  <div class="news-row">
    <div class="news-rank">${idx + 1}.</div>
    <div class="news-content">
      <div class="news-title">
        <a href="${itemUrl}">${title}</a>
        ${displayUrl ? `<span class="news-domain">(<a href="${url}" target="_blank" rel="noreferrer">${displayUrl}</a>)</span>` : ""}
      </div>
      <div class="news-meta">
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
        <button type="submit" class="news-delete-btn" title="Delete comment">delete</button>
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

async function fetchPosts(database, { sort = "new", limit = 30 }) {
  const posts = database.db.collection("news-posts");
  const sortSpec = { when: -1 }; // Chronological (newest first)
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
  // Show full URL
  const displayUrl = hydratedPost.url ? (() => {
    try {
      const u = new URL(hydratedPost.url);
      let path = u.hostname.replace(/^www\./, "") + u.pathname;
      if (path.endsWith("/")) path = path.slice(0, -1);
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
        <a href="https://www.youtube.com/watch?v=${youtubeId}" target="_blank" rel="noopener" class="news-youtube-fallback">
          <img src="https://img.youtube.com/vi/${youtubeId}/maxresdefault.jpg" alt="YouTube video thumbnail" class="news-youtube-thumb" />
        </a>
        <iframe 
          id="youtube-player"
          src="https://www.youtube.com/embed/${youtubeId}?enablejsapi=1&modestbranding=1&rel=0&playsinline=1"
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
          allowfullscreen
        ></iframe>
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

  // Check for aesthetic.computer piece preview
  const acPiece = parseAestheticComputerUrl(hydratedPost.url);
  const acPreviewHtml = acPiece ? `
      <div class="news-ac-preview" data-ac-piece="${escapeHtml(acPiece)}">
        <a href="${escapeHtml(hydratedPost.url)}" target="_blank" rel="noreferrer" class="news-ac-link">
          <img src="https://oven.aesthetic.computer/grab/webp/960/540/${escapeHtml(acPiece)}?duration=4000&fps=8&quality=80&density=1" alt="Aesthetic Computer piece preview" class="news-ac-webp" loading="lazy" />
          <span class="news-ac-badge">aesthetic.computer/${escapeHtml(acPiece)}</span>
        </a>
      </div>` : '';

  // Check for Imgur images
  const imgurUrl = parseImgurUrl(hydratedPost.url);
  const imgurPreviewHtml = imgurUrl ? `
      <div class="news-image-preview">
        <a href="${escapeHtml(hydratedPost.url)}" target="_blank" rel="noreferrer" class="news-image-link">
          <img src="${escapeHtml(imgurUrl)}" alt="Imgur image" class="news-image" loading="lazy" />
        </a>
      </div>` : '';

  // Check for direct image URLs
  const directImageUrl = !imgurUrl ? parseDirectImageUrl(hydratedPost.url) : null;
  const directImagePreviewHtml = directImageUrl ? `
      <div class="news-image-preview">
        <a href="${escapeHtml(hydratedPost.url)}" target="_blank" rel="noreferrer" class="news-image-link">
          <img src="${escapeHtml(directImageUrl)}" alt="Image" class="news-image" loading="lazy" />
        </a>
      </div>` : '';

  // Check for e-flux article preview
  const efluxUrl = parseEfluxUrl(hydratedPost.url);
  let efluxPreviewHtml = '';
  if (efluxUrl && !youtubeId && !kidlispCode && !acPiece && !imgurUrl && !directImageUrl) {
    const meta = await fetchEfluxMeta(efluxUrl);
    if (meta) {
      const sectionBadge = meta.section ? `<span class="news-eflux-section">e-flux ${escapeHtml(meta.section)}${meta.issue ? ` ${escapeHtml(meta.issue)}` : ''}</span>` : '<span class="news-eflux-section">e-flux</span>';
      efluxPreviewHtml = `
      <div class="news-eflux-preview">
        <a href="${escapeHtml(efluxUrl)}" target="_blank" rel="noopener" class="news-eflux-link">
          ${meta.image ? `<div class="news-eflux-hero">
            <img src="${escapeHtml(meta.image)}" alt="${escapeHtml(meta.title)}" class="news-eflux-image" />
          </div>` : ''}
          <div class="news-eflux-body">
            <div class="news-eflux-meta">
              ${sectionBadge}
              ${meta.author ? `<span class="news-eflux-author">${escapeHtml(meta.author)}</span>` : ''}
            </div>
            <h3 class="news-eflux-title">${escapeHtml(meta.title)}</h3>
            ${meta.excerpt ? `<p class="news-eflux-excerpt">${escapeHtml(meta.excerpt)}</p>` : ''}
          </div>
        </a>
      </div>`;
    }
  }

  // Check for Instagram preview (try Graph API first, fall back to og:image)
  const instagramUrl = parseInstagramUrl(hydratedPost.url);
  let instagramPreviewHtml = '';
  if (instagramUrl && !youtubeId && !kidlispCode && !acPiece && !imgurUrl && !directImageUrl && !efluxPreviewHtml) {
    // Try Instagram Graph API first
    const embedData = await fetchInstagramEmbed(instagramUrl, database);
    if (embedData && embedData.html) {
      // Use official Instagram embed HTML
      instagramPreviewHtml = `
      <div class="news-instagram-preview news-instagram-embed">
        ${embedData.html}
      </div>`;
    } else {
      // Fallback to og:image
      const ogMeta = await fetchOgMeta(instagramUrl);
      if (ogMeta && ogMeta.image) {
        instagramPreviewHtml = `
      <div class="news-instagram-preview">
        <a href="${escapeHtml(instagramUrl)}" target="_blank" rel="noopener" class="news-instagram-link">
          <div class="news-instagram-image-container">
            <img src="${escapeHtml(ogMeta.image)}" alt="${escapeHtml(ogMeta.title || 'Instagram post')}" class="news-instagram-image" loading="lazy" />
          </div>
          ${ogMeta.title || ogMeta.description ? `<div class="news-instagram-body">
            <div class="news-instagram-site">📷 Instagram</div>
            ${ogMeta.title ? `<div class="news-instagram-title">${escapeHtml(ogMeta.title)}</div>` : ''}
            ${ogMeta.description ? `<div class="news-instagram-description">${escapeHtml(ogMeta.description)}</div>` : ''}
          </div>` : ''}
        </a>
      </div>`;
      }
    }
  }

  // Generic og:image fallback for any other URL
  let ogPreviewHtml = '';
  if (hydratedPost.url && !youtubeId && !kidlispCode && !acPiece && !imgurUrl && !directImageUrl && !efluxPreviewHtml && !instagramPreviewHtml) {
    const ogMeta = await fetchOgMeta(hydratedPost.url);
    if (ogMeta && ogMeta.image) {
      ogPreviewHtml = `
      <div class="news-og-preview">
        <a href="${escapeHtml(hydratedPost.url)}" target="_blank" rel="noopener" class="news-og-link">
          <div class="news-og-image-container">
            <img src="${escapeHtml(ogMeta.image)}" alt="${escapeHtml(ogMeta.title || 'Preview')}" class="news-og-image" loading="lazy" />
          </div>
          ${ogMeta.title || ogMeta.description ? `<div class="news-og-body">
            ${ogMeta.siteName ? `<div class="news-og-site">${escapeHtml(ogMeta.siteName)}</div>` : ''}
            ${ogMeta.title ? `<div class="news-og-title">${escapeHtml(ogMeta.title)}</div>` : ''}
            ${ogMeta.description ? `<div class="news-og-description">${escapeHtml(ogMeta.description)}</div>` : ''}
          </div>` : ''}
        </a>
      </div>`;
    }
  }

  const hasMedia = youtubeId || kidlispCode || acPiece || imgurUrl || directImageUrl || efluxPreviewHtml || instagramPreviewHtml || ogPreviewHtml;
  const mediaHtml = youtubeEmbedHtml || kidlispPreviewHtml || acPreviewHtml || imgurPreviewHtml || directImagePreviewHtml || efluxPreviewHtml || instagramPreviewHtml || ogPreviewHtml;

  const body = `
  ${header(basePath)}
  ${hasMedia ? `<div class="news-hero-media">
    ${mediaHtml}
  </div>` : ''}
  <main class="news-main">
    <div class="news-item-header">
      <div class="news-item-info">
        <table class="news-item-table" border="0" cellpadding="0" cellspacing="0">
          <tr class="news-item-row">
            <td class="news-item-content">
              <span class="news-item-title">
                <a href="${url || '#'}" ${url ? 'target="_blank" rel="noreferrer"' : ""}>${postTitle}</a>
                ${displayUrl ? `<span class="news-domain">(<a href="${url}" target="_blank" rel="noreferrer">${displayUrl}</a>)</span>` : ""}
                <form class="news-admin-delete" data-news-action="delete" data-item-type="post" data-item-id="${hydratedPost.code}" data-handle="${escapeHtml(hydratedPost.handle?.replace('@', '') || '')}" method="post" action="/api/news/delete" style="display:none;">
                  <input type="hidden" name="itemType" value="post" />
                  <input type="hidden" name="itemId" value="${hydratedPost.code}" />
                  <button type="submit" class="news-delete-btn" title="Delete post">delete</button>
                </form>
              </span>
              <div class="news-item-meta">
                ${atLinkHtml}
              </div>
            </td>
          </tr>
        </table>
      </div>
    </div>
    ${hydratedPost.text ? `
    <div class="news-op-text">
      <div class="news-op-meta">${renderHandle(hydratedPost.handle)} ${formatDate(hydratedPost.when)}</div>
      <div class="news-op-body">${escapeHtml(hydratedPost.text)}</div>
    </div>` : ""}
    <div class="news-comments">
      ${commentsHtml || ''}
    </div>
    <form id="news-comment-form" class="news-comment-form" data-news-action="comment" method="post" action="/api/news/comment"${youtubeId ? ` data-youtube-id="${youtubeId}"` : ''}>
      <input type="hidden" name="postCode" value="${escapeHtml(code)}" />
      <textarea name="text" rows="4" placeholder="Write a comment..."></textarea>
      <div class="news-comment-actions">
        ${youtubeId ? `<button type="button" id="insert-timecode-btn" class="news-timecode-btn" title="Insert current video time">@ 0:00</button>` : ''}
        <button type="submit" data-i18n="respond">Respond</button>
      </div>
    </form>
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
        <div class="news-url-headline-group">
          <div class="news-field-group">
            <label data-i18n="source-url">Source URL</label>
            <input type="url" name="url" placeholder="https://" />
          </div>
          <div class="news-field-group">
            <label data-i18n="headline">Headline</label>
            <input type="text" name="title" required data-i18n-placeholder="headline-placeholder" placeholder="What's the story?" />
            <div class="news-auto-title-status" id="news-auto-title-status"></div>
          </div>
        </div>
        <div class="news-either-or">
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
    const host = event.headers?.host || "";
    const isLocalHost = host.includes("localhost") || host.startsWith("127.0.0.1") || host.startsWith("0.0.0.0");
    const assetOrigin = dev || isLocalHost ? "" : "https://aesthetic.computer";
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
        // Backward compat: redirect old /item/CODE to /nCODE
        const oldCode = route.slice(5);
        await database.disconnect();
        return respondFn(301, "", { Location: `${basePath}/n${oldCode}`, "Content-Type": "text/html" });
      } else if (route === "report" || route === "submit") {
        title = "Report · Aesthetic News";
        body = await renderReportPage(basePath);
      } else if (route === "guidelines") {
        // handled elsewhere, but don't 404 on it
        title = "Not Found | Aesthetic News";
        body = `${header(basePath)}<main class="news-main"><p>Page not found.</p></main>${footer()}`;
      } else {
        // Treat any other route as a potential news code (n-prefixed or legacy)
        const result = await renderItemPage(database, basePath, route);
        body = result.body || result;
        title = result.title || "Aesthetic News";
      }

      const html = layout({ title, body, assetBase, assetOrigin });
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
