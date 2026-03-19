# Keeps Permalink Modals — keep.kidlisp.com/$code

**Date:** March 12, 2026
**Status:** Plan
**Goal:** Replace objkt.com link-outs with in-page detail modals, add shareable permalink URLs, and enable rich Twitter/X social cards for each minted keep.

---

## Context

The keep.kidlisp.com market tab currently shows KidLisp keeps as cards that link directly to objkt.com. This sends users away from the AC ecosystem. The Tezos NFT market landscape report (March 12, 2026) notes that "discovery and collector relationship management matter more than squeezing the floor." Permalinks with social cards directly serve discovery — when someone shares `keep.kidlisp.com/$cow` on Twitter, it should unfurl into a rich card with the piece's animated thumbnail and market status, driving traffic back to the keeps site rather than objkt.

**KidLisp collection snapshot:** 34 items, 9 owners, floor 12 XTZ, total volume 254.5 XTZ, 1 active listing.

---

## URL Pattern: `keep.kidlisp.com/$code`

**Recommendation: Use `keep.kidlisp.com/$code`** (not `buy.kidlisp.com`).

Reasons:
- `$` prefix is already the canonical KidLisp piece naming convention
- Short, memorable, consistent with AC's "memorizable paths" philosophy
- No new DNS records, Netlify config, or separate site needed
- `$` is URL-safe (no percent-encoding required)
- Example: `keep.kidlisp.com/$cow`

---

## Phase 1: Keep Detail Modal (Client-Side)

**File:** `system/public/kidlisp.com/keeps.html`

### 1a. Modal HTML
Add `keep-detail-overlay` div (same pattern as existing `mintOverlay`):
- Large animated WebP thumbnail (from IPFS `thumbnail_uri`)
- Piece name (`$code`), token ID
- Market status: "For Sale — 12 XTZ" or "Sold" or "Not Listed"
- Seller/buyer/owner addresses (truncated)
- Minted date
- Prominent "Buy on objkt.com" button (or "View on objkt.com" if sold/unlisted)
- "Copy Permalink" button
- "Share on X" button → `twitter.com/intent/tweet?url=...&text=...`

### 1b. Modal CSS
Follow mint modal conventions:
- Fixed overlay, backdrop blur, `z-index: 20000`
- `.keep-detail-overlay.open` with fade-in animation
- `.keep-detail-modal` max-width 520px, responsive
- Thumbnail area at top, metadata below, action buttons at bottom

### 1c. Modal JS
- `openKeepDetailModal(entry)` — populate fields, add `.open`, push URL state, lock body scroll
- `closeKeepDetailModal()` — remove `.open`, restore scroll, pop state to `/market`
- Escape key + backdrop click to close (same pattern as lines 3998-4003)
- For tokens not in current market data, fetch directly from objkt GraphQL by name

### 1d. Change Market Card Click Behavior
Currently (line 3941):
```html
<a class="market-card" href="${objktUrl}" target="_blank">
```
Change to:
```html
<div class="market-card" onclick="openKeepDetailModal(index)" role="button" tabindex="0">
```
Store sorted entries in a module-level array for index-based lookup.

---

## Phase 2: URL Routing for Permalinks

**File:** `system/public/kidlisp.com/keeps.html`

### 2a. Extend `tabFromLocation()` (line 3961)
Recognize `$`-prefixed paths:
```javascript
const seg = location.pathname.replace(/^\/+/, '').split('/')[0];
if (seg.startsWith('$')) return { tab: 'market', code: seg.slice(1) };
```
Update all call sites to handle the new return shape.

### 2b. Deep-link on Page Load
- Set `pendingDeepLinkCode` when URL has `$code`
- After `loadMarket()` + `renderMarket()` complete, find matching token and auto-open modal
- If token not in active listings/sales, fetch token metadata from objkt GraphQL by name
- Show loading state in modal while fetching

### 2c. pushState Integration
- Open modal: `history.pushState({}, '', '/$' + code)`
- Close modal: `history.pushState({}, '', '/market')`
- Handle popstate for browser back/forward

---

## Phase 3: Twitter/X Social Cards (Server-Side Meta Tags)

Twitter/Facebook crawlers don't run JS, so OG tags must be in the initial HTML.

### Approach: Netlify Edge Function

**New file:** `system/netlify/edge-functions/keeps-social.js`

```javascript
export default async function(request, context) {
  const url = new URL(request.url);
  const host = request.headers.get('host') || '';
  if (!host.includes('keep.kidlisp.com')) return context.next();

  const seg = url.pathname.replace(/^\/+/, '').split('/')[0];
  if (!seg.startsWith('$')) return context.next();

  const ua = request.headers.get('user-agent') || '';
  const isCrawler = /twitterbot|facebookexternalhit|linkedinbot|slackbot|discordbot/i.test(ua);
  if (!isCrawler) return context.next(); // SPA handles normal users

  const code = seg.slice(1);
  // Fetch token from objkt GraphQL → get name, price, thumbnail
  // Build OG image URL: oven.aesthetic.computer/preview/1200x630/CODE.png
  // Inject meta tags into keeps.html and return
}
```

### Meta Tags Injected
```html
<meta property="og:url" content="https://keep.kidlisp.com/$CODE" />
<meta property="og:title" content="$CODE · KidLisp Keep" />
<meta property="og:description" content="For Sale — 12 XTZ | KidLisp generative art on Tezos" />
<meta property="og:image" content="https://oven.aesthetic.computer/preview/1200x630/CODE.png" />
<meta name="twitter:card" content="summary_large_image" />
<meta name="twitter:title" content="$CODE · KidLisp Keep" />
<meta name="twitter:description" content="For Sale — 12 XTZ | KidLisp generative art on Tezos" />
<meta name="twitter:image" content="https://oven.aesthetic.computer/preview/1200x630/CODE.png" />
```

### OG Image Strategy
- **Twitter/X cards**: Static PNG via `oven.aesthetic.computer/preview/1200x630/CODE.png` (already working infrastructure, 24h CDN cache)
- **In-page modal**: Animated WebP via IPFS `thumbnail_uri` (shows animation in browser)
- Twitter doesn't support animated images in cards — static PNG is the correct format

### Netlify Config
**File:** `system/netlify.toml` — add edge function binding:
```toml
[[edge_functions]]
function = "keeps-social"
path = "/*"
```
(Host filtering done inside the function since edge functions may not support subdomain-scoped paths.)

### Fallback
If edge functions don't work well with subdomain routing, fall back to modifying `system/netlify/functions/index.mjs` (the keep.kidlisp.com handler around line 182) to detect `$code` paths and inject meta tags there. This is slightly slower but is a proven pattern used for `top.kidlisp.com`.

---

## Phase 4: OG Image Polish (Optional)

**File:** `oven/server.mjs`

Add a dedicated `/keeps/og/$code.png` endpoint that generates a styled 1200x630 card:
- Piece thumbnail (static frame from WebP) centered on branded background
- `$code` name overlaid
- Price/status text
- KidLisp + keeps branding

This is a nice-to-have — the existing `/preview/` endpoint works fine for MVP.

---

## Implementation Order

| Step | Scope | Files | Shippable? |
|------|-------|-------|------------|
| Phase 1 | Client-side detail modal | `keeps.html` | Yes |
| Phase 2 | URL routing + deep-links | `keeps.html` | Yes (with Phase 1) |
| Phase 3 | SSR meta tags for social cards | `keeps-social.js`, `netlify.toml` | Yes |
| Phase 4 | Branded OG images | `oven/server.mjs` | Optional polish |

Phases 1+2 ship as one commit. Phase 3 is a separate commit. Phase 4 is independent.

---

## Critical Files

| File | Changes |
|------|---------|
| `system/public/kidlisp.com/keeps.html` | Modal HTML/CSS/JS, card click handlers, URL routing, deep-link logic |
| `system/netlify/edge-functions/keeps-social.js` | **New** — crawler detection + SSR meta tag injection |
| `system/netlify.toml` | Edge function binding for keeps-social |
| `system/netlify/functions/index.mjs` | Fallback SSR approach if edge function doesn't work for subdomains |

### Reuse Existing Infrastructure
- `fetchObjktGraphQL()` (keeps.html:3718) — already handles objkt queries with retries
- `shortAddress()` (keeps.html:3707) — address truncation
- `getKeepsContractAddress()` — contract address resolution
- Mint modal open/close pattern (keeps.html:3997-4003) — exact same UX for detail modal
- `oven.aesthetic.computer/preview/1200x630/CODE.png` — existing OG image generation
- `oven.aesthetic.computer/keeps/latest/:piece` — per-piece thumbnail lookup

---

## Verification

1. **Modal**: Click a market card → modal opens with piece details, animated thumbnail, market status. Escape/backdrop closes it.
2. **Permalink**: Navigate to `keep.kidlisp.com/$cow` → market tab activates, modal auto-opens for `$cow`.
3. **Copy/Share**: Copy permalink button copies correct URL. Share on X opens tweet intent with URL.
4. **Social card**: Use Twitter Card Validator or `curl -A Twitterbot keep.kidlisp.com/$cow` → verify OG tags are present with correct title, description, and image URL.
5. **Browser back/forward**: Open modal → press back → modal closes, URL returns to `/market`.
