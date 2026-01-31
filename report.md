# In-Progress Report

## 🤖 Android App Distribution (2026-01-31)

### Status
- **Play Store Account**: Created with `me@jas.life`, identity verification pending
- **GitHub Release**: Published at [android-v1.1.0](https://github.com/whistlegraph/aesthetic-computer/releases/tag/android-v1.1.0)
- **Sideload APK**: `aesthetic-computer-v1.1.0-debug.apk` (~10MB)

### Completed
- [x] Consumer Android app working on Uniherz Jelly (tested via WiFi debugging)
- [x] APK uploaded to GitHub releases
- [x] `/mobile` piece updated with Android sideload option

### Pending
- [ ] Google Play identity verification (email: me@jas.life)
- [ ] Generate signing keystore for production release
- [ ] Build signed release AAB for Play Store submission
- [ ] Create app listing (screenshots, description, etc.)

### Build Flavors
- `consumer`: Points to https://aesthetic.computer (Play Store version)
- `kiosk`: Points to localhost:8443 (device installations)

---

# Aesthetic News — Architecture Notes (2026-01-18)

## Summary of request
- Keep the visual design unchanged.
- Remove visited color on “Report the News” link (done).
- Consider shifting news.aesthetic.computer to a single-page app with proper routing, similar to how sotce-net works.
- Check for any KidLisp-related parts in the news app.

## Findings
### sotce-net architecture
- sotce-net is a **single Netlify function** with an internal router: [system/netlify/functions/sotce-net.mjs](system/netlify/functions/sotce-net.mjs).
- It inspects `event.path` and routes within the handler. This keeps the subdomain effectively “single-function” and centralized for auth/session logic.

### news.aesthetic.computer current architecture
- News is **server-rendered** by [system/netlify/functions/news.mjs](system/netlify/functions/news.mjs) with internal routing logic (e.g., `/`, `/new`, `/comments`, `/item`, `/report`), and assets in [system/public/news.aesthetic.computer](system/public/news.aesthetic.computer).
- There’s a **separate function** for guidelines: [system/netlify/functions/news-guidelines.mjs](system/netlify/functions/news-guidelines.mjs), plus redirects in [system/netlify.toml](system/netlify.toml).
- Netlify redirects already point the subdomain and `/news.aesthetic.computer/*` paths to the `news` function, with static assets served from `system/public/news.aesthetic.computer/`.

### KidLisp presence in News
- Only a CSS comment reference exists: the main page background variable references “kidlisp.com” as a color inspiration in [system/public/news.aesthetic.computer/main.css](system/public/news.aesthetic.computer/main.css).
- No functional KidLisp code paths were found inside the news front-end or news Netlify functions.

## Recommended direction (SPA without design change)
Two viable ways to match sotce-net’s “single function” feel while keeping visuals intact:

### Option A — Single Netlify function entry (minimal change, still SSR)
- Fold `news-guidelines.mjs` into `news.mjs` and route `/guidelines` internally.
- Update `netlify.toml` to point all News routes (including `/guidelines`) to `news.mjs`.
- Result: **single function** for all News pages and auth logic, still server-rendered, zero design change.

### Option B — True SPA shell + client router (larger change)
- Serve a single HTML shell from `news.mjs` for **all** page routes.
- Move route rendering into a client-side router (history API), calling the existing `/api/news` endpoints.
- Keep the same markup/CSS to preserve visuals; simply render via JS instead of server HTML.
- Result: **SPA behavior** with consistent auth state and simplified routing, but higher implementation cost.

## Suggested next step
If you want the fastest flip with minimal risk to design, start with **Option A** (single-function consolidation). If the goal is full SPA behavior, proceed with Option B and migrate the existing SSR render functions into client-side templates without altering styles.

