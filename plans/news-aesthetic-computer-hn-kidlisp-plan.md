# news.aesthetic.computer — KidLisp‑backed HN‑style microsite plan

Date: 2026-01-15

## 1) Goal
Build a minimalist, classic Hacker News‑style site at news.aesthetic.computer that:
- Uses AC login/logout UI and existing CSS patterns.
- Stores posts, comments, and votes in the existing MongoDB.
- Can render HTML server‑side (classic CGI feel).
- Optionally uses KidLisp as a server‑side templating/logic language.

## 2) Repo findings & anchors (for implementation)
- KidLisp runtime: system/public/aesthetic.computer/lib/kidlisp.mjs (exports `parse`, `evaluate`, `KidLisp`, etc.).
- MongoDB helper: system/backend/database.mjs (reusable connect logic).
- Auth helper: system/backend/authorization.mjs (Auth0 user lookup, handle resolution).
- Netlify routing for subdomains: system/netlify.toml (kidlisp.com + give.aesthetic.computer patterns).
- KidLisp.com Auth0 UI patterns: system/public/kidlisp.com/index.html (Auth0 flow, `acLogin`, `acLogout`).
- Login button styles: system/public/kidlisp.com/css/components.css (header login/logout styles).

## 3) Feasibility summary
**Yes, feasible**, but KidLisp needs a small server‑safe entry point.
- `kidlisp.mjs` already exports `parse`/`evaluate` and includes a Node‑compatible `fetchCachedCode` path. It can run in Node when DOM‑only functions are avoided.
- There are browser‑only hooks (`window`, `document`, QR canvas, navigation). These are mostly guarded, but the server should **not call** graphics/DOM functions.
- Recommendation: add a **server‑safe wrapper** that uses KidLisp for evaluation but supplies a **minimal environment** (HTML templating, string ops, conditionals) and blocks canvas/DOM features.

## 4) Arc / HN background (brief research)
- Arc is a Lisp dialect by Paul Graham/Robert Morris; Hacker News was originally built in Arc.
- Arc emphasized terseness and hackability, and HN’s original ethos was “plain HTML, minimal UI.”
- This aligns with a KidLisp‑powered, server‑rendered “classic CGI” microsite.

## 5) Architecture overview (recommended)
**A) Server‑rendered HTML via Netlify Functions** (recommended)
- Route https://news.aesthetic.computer → a Netlify function that returns HTML.
- Use KidLisp (server wrapper) to render HTML templates and small logic.
- Minimal client JS (optional) for vote/login UI polish.

**B) Static HTML + JSON API** (fallback)
- Serve a static HTML shell; fetch posts/comments from API endpoints.
- Less “classic CGI” feel, but simpler.

## 6) Data model (MongoDB)
Existing AC collections use **lowercase, hyphenated names** (e.g. `piece-hits`, `false.work-builds`, `chat-system`, `sotce-pages`) and mostly **plural nouns** (`paintings`, `tapes`, `clocks`, `users`, `@handles`). Avoid underscores. New news collections should follow this convention.

Create new collections with indexes:

### news-posts
- fields: `_id`, `title`, `url`, `text`, `user`, `handle`, `createdAt`, `updatedAt`, `score`, `commentCount`, `status`, `slug` (or `shortId`), `tags`.
- indexes: `createdAt`, `score`, `user`, `slug` (unique), `status`.

### news-comments
- fields: `_id`, `postId`, `parentId` (nullable), `user`, `handle`, `text`, `createdAt`, `score`, `status`.
- indexes: `postId`, `parentId`, `createdAt`, `score`, `user`.

### news-votes
- fields: `_id`, `itemType` (`post`|`comment`), `itemId`, `user`, `createdAt`.
- indexes: unique compound `(itemType, itemId, user)`.

### news-flags (optional)
- fields: `_id`, `itemType`, `itemId`, `user`, `createdAt`, `reason`.

## 7) Server endpoints & routing
**Netlify Functions** (system/netlify/functions/):
- `news.mjs` → HTML renderer
  - routes: `/`, `/new`, `/ask`, `/show`, `/item`, `/user`, `/submit`.
- `news-api.mjs` → JSON endpoints
  - `GET /api/news/posts`, `GET /api/news/item`, `POST /api/news/submit`, `POST /api/news/comment`, `POST /api/news/vote`.

**Routing in system/netlify.toml**:
- Add redirects similar to give/kidlisp:
  - `https://news.aesthetic.computer` → `/.netlify/functions/news`
  - `https://news.aesthetic.computer/*` → `/.netlify/functions/news?path=:splat`
- Optional: add `/news.aesthetic.computer/*` static fallback if you want static assets.

## 8) KidLisp server wrapper
Create a new module, e.g. `system/backend/kidlisp-server.mjs`:
- Import `parse`, `evaluate` or `KidLisp` from kidlisp.mjs.
- Provide a **server‑safe environment** with functions like `html`, `head`, `body`, `div`, `a`, `p`, `ul`, `li`, `span`, `form`, `input`, `button`.
- Provide helpers: `escape-html`, `link`, `time-ago`, `if`, `when`, `map`, `join`.
- Explicitly block/omit canvas/DOM functions (`wipe`, `ink`, `box`, `paste`, `jump`, `tape`, `qr`, etc.).
- Add a `render(template, data)` helper that binds data into the env and returns HTML string.

**Alternative (bigger) refactor**:
- Split `kidlisp.mjs` into `kidlisp-core.mjs` (parser + evaluator) and `kidlisp-browser.mjs` (canvas/DOM). This makes server use clean and avoids stubs.

## 9) Auth & handle resolution
- Client side: reuse Auth0 flow from kidlisp.com (see `acLogin`, `acLogout`).
- Server side: for writes (submit/vote/comment), require `Authorization` header (Auth0 access token) and validate with `authorize()`.
- Use `handleFor()` to display `@handles` from the `@handles` collection.
- Public pages can be read‑only without auth.

## 10) UI & CSS
- Use existing AC fonts + variables:
  - Font preloads and CSS references from kidlisp.com.
  - Login button styles from `kidlisp.com/css/components.css` (`.header-login-btn`, `.header-logout-btn`).
- Create `system/public/news.aesthetic.computer/` for:
  - `main.css` (HN‑style layout + AC colors)
  - small `client.js` (optional for vote/logout state)

## 11) Implementation phases
### Phase 0 — Design decisions
- Confirm whether KidLisp is a core requirement for rendering or just optional for templating.
- Decide if “ask/show” categories are separate collections or `tags`/`type` on posts.

### Phase 1 — Data + API
- Add collections & indexes.
- Implement `news-api.mjs` with CRUD for posts/comments and vote logic.
- Add server‑side input validation, spam caps, rate limits.

### Phase 2 — Server‑rendered HTML
- Implement `news.mjs` renderer with minimal HTML templates.
- Integrate `kidlisp-server.mjs` for template rendering (if selected).
- Add canonical pages: index, item, user, submit.

### Phase 3 — Styling & Auth UI
- Add CSS in `news.aesthetic.computer/main.css` with AC colors and font stack.
- Add login/logout header using existing button styles.
- Connect Auth0 flow (copy/trim from kidlisp.com).

### Phase 4 — Netlify + domain
- Update system/netlify.toml with news subdomain redirects.
- Add Netlify env vars if needed (Auth0 callback URLs).
- Update Netlify site settings: add `news.aesthetic.computer` domain + DNS.

### Phase 5 — Hardening
- Anti‑spam (rate limit, simple heuristic, optional CAPTCHA).
- Moderation tools (flag, kill, showdead).
- Caching (small HTML cache for front page + comments).

## 12) Risks & mitigations
- **KidLisp browser dependencies**: avoid DOM‑only functions in server mode; isolate core interpreter or add guards.
- **Auth0 in server‑rendered flow**: initial SSR will render guest state; client JS can update to logged‑in UI after Auth0 check.
- **Performance**: use cached queries and incremental ranking; front page can be cached for $\le$ 30s.

## 13) Next concrete actions (recommended)
1) Add `kidlisp-server.mjs` with a minimal HTML environment.
2) Create `news-api.mjs` (posts/comments/votes) using `database.mjs` + `authorization.mjs`.
3) Create `news.mjs` (SSR HTML) using the KidLisp renderer.
4) Add CSS + minimal JS in `system/public/news.aesthetic.computer/`.
5) Update netlify redirects for the subdomain.

---
If you want, I can start Phase 1 (data + API) next and wire up the Netlify routing.

## Appendix A) Existing AC API flows (relevant patterns)
These flows show how AC uses `sub` user IDs and `@handles`, and how new content is stored.

### KidLisp submission (store-kidlisp)
File: system/netlify/functions/store-kidlisp.mjs
- Accepts `POST { source }`, optional Auth0 token.
- Saves into `kidlisp` with `{ code, source, hash, when, lastAccessed, hits, user? }`.
- Uses `generateUniqueCode` + SHA‑256 dedupe by `hash`.
- `user` is **Auth0 `sub`** if authorized.
- Also triggers ATProto sync (background).

### Clock submission (store-clock)
File: system/netlify/functions/store-clock.mjs
- Similar to KidLisp with `source`/`melody`.
- Saves into `clocks` with `{ code, source, hash, when, lastAccessed, hits, user? }`.
- Uses pronounceable code generator + hash dedupe.

### Media creation (track-media)
File: system/netlify/functions/track-media.mjs
- POST creates `paintings`/`pieces`/`tapes` records with `{ code, slug, when, user?, bucket }`.
- `user` only present for authenticated uploads; guests omit `user`.
- Generates short `code` for all media via `generateUniqueCode`.

### Painting lookups (painting-code, painting-metadata)
Files: system/netlify/functions/painting-code.mjs, painting-metadata.mjs
- `paintings` documents store `code`, `slug`, `user` (optional), `nuked`.
- Handle lookup via `@handles` using `_id == user`.

### User/handle resolution
Files: system/netlify/functions/user.js, handles.mjs; backend/authorization.mjs
- `user.js` maps handle/email/code → `sub`.
- Handles live in `@handles` collection; `_id` is the **user `sub`**.
- `handleFor(sub)` looks up `@handles` with `_id == sub` and caches.

### Implications for news site
- Use `user` field as Auth0 `sub` and resolve `@handles` via `@handles`.
- Follow existing patterns: store timestamps as `when`/`created` (pick one, be consistent), keep `user` optional for anonymous posts, and use short `code` or `slug` for URLs.