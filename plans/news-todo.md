# news.aesthetic.computer — TODO

## Bugs / Fixes (from Fía + Jeffrey chat, 2026.02.06)

- [x] **Headline not auto-inferred from Source URL** — The unfurl/auto-title feature code is correct. The `/api/news/unfurl` endpoint and client-side `unfurlUrl()` work locally; they'll be active once deployed.
- [x] **Ditch the troll toll** — Removed all troll toll logic from `news-api.mjs` (needsToll, pending-toll status, 402 response, Aesthetic Network domain list), `client.js` (handleTrollToll function, 402 handler), `main.css` (toll button styles). The `news-toll.mjs` Netlify Function file still exists but is now dead code (can be deleted later).
- [x] **Redirect to your item page after posting** — Redirect was already implemented (`data.redirect` → `window.location.href`). It was blocked by the troll toll's 402 intercept. Now works for all posts.
- [x] **Remove all upvoting** — Vote buttons, forms, CSS, and client handlers removed. Sorting by score disabled (chronological only).
