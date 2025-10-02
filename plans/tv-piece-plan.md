# TV Piece (For-You Feed) Plan

## Overview
- **Goal:** Launch a new `tv` aesthetic computer (AC) piece that continuously curates paintings, moods, and wider AC media into a lean-back "For You" experience with optional second-screen overlays.
- **Audience:** Returning AC painters and viewers who want ambient discovery plus quick actions to remix, save, or respond.
- **Success criteria:**
  - Personalized feed assembles within <$200\,\text{ms}$ P95 and rotates new content every 15–30 seconds.
  - Viewers can jump directly into showcased paintings/pieces or set their own mood in ≤2 interactions.
  - Logged-in usage lifts weekly painting revisits by ≥10% and mood posts by ≥15%.
  - Feed gracefully degrades for anonymous guests, highlighting global/popular content.

## Stack research snapshot
### Client runtime (KidLisp & prompt shell)
- Pieces execute inside the KidLisp/JS runtime under `system/public/aesthetic.computer`, with UI primitives supplied by `lib/kidlisp.mjs` (rendering, async fetch helpers, cached painting lookups, HUD toggles).
- Existing carousel-like behavior lives in prompt disks such as `system/public/aesthetic.computer/disks/prompt.mjs`; reuse its HUD toggle and keyboard navigation for remote-friendly control.
- Pieces are single-module `.mjs` files (see `aesthetic-computer-code/piece.mjs` template) that export a `paint` function receiving drawing helpers (e.g. `wipe`, `ink`, `line`). Complex pieces frequently compose disk utilities from `system/public/aesthetic.computer/disks/*`.

### Realtime & backend services
- `session-server/session.mjs` (Fastify + `ws` + Redis) already handles per-piece WebSocket state, ping/pong upkeep, and broadcast (`pack`, `subscribers`, `code-channel:sub` pattern) suitable for live feed refresh and synchronized playback.
- Netlify Functions under `system/netlify/functions` expose HTTP APIs for media and profiles (e.g. `profile.js`, `media-collection.js`, `mood.mjs`, `playlist.mjs`). These run against MongoDB via `system/backend/database.mjs` and wrap responses with `backend/http.mjs` helpers.
- `ac-event-daemon` can project overlay notifications (UDP → web overlay) if we emit `tv`-specific events (e.g. "Now playing @handle/slug") for studio screens.

### Content & data stores
- **Paintings & pieces:** Metadata persisted to Mongo `paintings`/`pieces` collections (indexes established in `database.mjs`) with S3-backed media served via `/media/{user}/{type}/{slug}.{ext}`. `media-collection.js` already returns signed lists per user.
- **Moods:** Latest and full history retrieved by `mood.mjs` / `moodFor` (sorted by `when`, automatically stitches `@handles`).
- **Profiles:** `profile.js` resolves handle/email to Auth0 `sub` and attaches most recent mood for context.
- **Engagement signals:** `system/netlify/functions/track-media.js` logs playback/interaction events; reuse/extend for ranking.

### Eventing & playback surfaces
- `playlist.mjs` exports DP-1 compliant feeds, showing appetite for declarative playlists. `tv` can adopt similar JSON output, but optimized for AC-native rendering.
- HUD overlays and label toggles already exist in disks (`disks/rect.mjs`, `disks/prompt.mjs`), offering ready-made animation scaffolding for transitions.

## Experience concept
1. **Intro vignette:** Animated channel intro with latest mood headline (e.g., `@jeffrey · 🌧 thoughtful`).
2. **Feature slots:** Rotating spotlight segments (painting replay, taped performance, KidLisp snippet) with progress bar and optional audio.
3. **Mood ticker:** Horizontal crawl of live moods filtered by follow graph or trending handles.
4. **Action rail:** Minimal remote-friendly controls: `Save`, `Remix`, `Send to prompt`, `Mood now`, `Channel select`.
5. **Ambient mode:** When idle, loops curated fallback playlist (global hits) and surfaces new posts via subtle overlay pulses.

## System architecture
### Data flow (personal session)
1. **Bootstrap:** `tv.mjs` piece requests `/profile/{handle}` for signed-in users; anonymous flows skip.
2. **Feed assembly:** Call new `/tv-feed` Netlify Function (to be built) that:
   - Fetches candidate paintings (`media-collection?for={sub}/painting`), pieces (`.../piece`), moods (`mood?handle=@me,follows`), tapes (`track-media` logs), recent following activity.
   - Augments with global trending pool (top downloads, mood velocity) for cold-start or filler.
   - Sorts via weighted blend of recency, social proximity, user affinity (see Personalization).
3. **Asset hydration:** Piece preloads hero asset(s) using KidLisp `preload` APIs and caches painting textures.
4. **Loop control:** `session-server` subscription coordinates multi-client sync (e.g., shared studio TV).
5. **Instrumentation:** Client emits view/start/complete events via existing `track-media` endpoint with `context: "tv"`.

### Personalization heuristics (iteration-friendly)
- **Signals:** user mood similarity (NLP classification), prior likes/saves, shared handle follow, medium preference (painting vs tape), time-of-day.
- **Weighting:** Start with simple linear scoring $S = 0.4 R + 0.25 A + 0.2 M + 0.15 T$, where `R`=recency decay, `A`=affinity, `M`=mood match, `T`=type diversity. Adjust coefficients based on engagement.
- **Diversity guardrails:** Enforce max two consecutive items per handle/media type; introduce periodic global spotlight slots.
- **Cold start:** Use curated editorial playlist plus latest moods from verified handles.

### Services to add/extend
- `system/netlify/functions/tv-feed.mjs` (new): orchestrates Mongo queries, merges Redis-tracked trending counts, returns structured feed JSON.
- Extend `track-media.js` to log `tv` playback events (start, dwell, skip) for learning loop.
- Optional `tv-notify` publisher that emits UDP packets for `ac-event-daemon` overlays whenever slot changes.

## Implementation roadmap

| Phase | Duration | Outcomes |
| --- | --- | --- |
| 0. Discovery & design | 1 week | UX flows, motion storyboard, data audit, API contract draft |
| 1. Backend feed service | 2 weeks | `/tv-feed` endpoint, scoring heuristics, caching, tests |
| 2. Client piece prototype | 2 weeks | `tv.mjs` prototype with mocked data, HUD controls, autoplay loop |
| 3. Integration & realtime sync | 1 week | Session-server channel wiring, multi-screen sync, overlay hooks |
| 4. Personalization tuning | 2+ weeks | Signal instrumentation, weight tuning, A/B toggles |
| 5. Beta & polish | 2 weeks | Accessibility pass, performance tuning, metrics dashboard |

## Workstreams & tasks
1. **Data + ranking**
   - Model Mongo aggregation pipelines for moods & paintings (reuse `allMoods`, add follow filter).
   - Implement Redis-based trending counts (every view increments sorted set; TTL for decay).
   - Build scoring module with adjustable weights via environment variables or feature flags.
2. **API layer**
   - Create `/tv-feed` Netlify Function returning `slots[]` with metadata `{type, handle, assetUrl, mood, duration, actions}`.
   - Add pagination/window parameters for different surfaces (ambient vs interactive).
   - Secure via Auth0 JWT (reuse `authorized.js`) while allowing degraded anonymous access.
3. **Client piece**
   - Author `system/public/aesthetic.computer/pieces/tv.mjs` (or `tv.lisp`) orchestrating segments, animations, remote control mapping.
   - Integrate `session-server` channel subscription for synchronized playback states (`play`, `pause`, `skip`).
   - Implement mood ticker leveraging `allMoods` data and existing `disks/shape.mjs` text layout utilities.
4. **Interaction hooks**
   - Wire `Save`/`Remix` buttons to prompt commands (`publish`, `source`, `channel`).
   - Provide `Mood now` inline form posting to `mood.mjs`.
   - Surface context menus for pieces via `prompt` overlay.
5. **Observability**
   - Extend `track-media.js` schema (`context`, `slotId`, `affinityScore`).
   - Dashboards (Netlify analytics or custom) for slot dwell, skip rate, mood posts per session.
   - Provide admin-only `tv-debug` piece showing raw scoring breakdown.

## Key deliverables
- `system/netlify/functions/tv-feed.mjs` with unit coverage (e.g., feed assembly, cold-start fallback).
- `system/public/aesthetic.computer/pieces/tv.mjs` (plus assets under `pieces/tv/` if needed) and optional `aesthetic-computer-code/tv.mjs` template for community remixing.
- Updated `session-server` channel handlers to support `tv` broadcast commands.
- Documentation: `docs/tv-piece.md` (API contract, client architecture, personalization notes).
- Analytics pipeline updates (`track-media.js`, dashboards).
- Studio overlay integration script (optional) for `ac-event-daemon`.

## Metrics & evaluation
- Playback completion %, average dwell time per slot.
- CTR on `Remix`/`Save`, mood post conversion after exposure.
- Crash/error rate in `tv` piece logs (dev channel watchers).
- Latency: `/tv-feed` P95, asset preload success, websocket reconnect frequency.

## Risks & mitigations
- **Cold-start quality** → curate editorial playlist; backfill with trending.
- **Mongo query cost** → add compound indexes on `{user, when}`, `{handleInfo.handle, when}`; cache feed in Redis for 30s.
- **Mood toxicity** → reuse `shared/filter.mjs` server-side, add client blur/unblur controls.
- **Autoplay fatigue** → provide quick `channel select` to switch to theme-based playlists.
- **Bandwidth-heavy assets** → precompute progressive JPEG/AVIF variants, allow piece to request `?preview=1` URLs.

## Dependencies
- Authenticated user context via prompt (Auth0).  
- Redis availability for ranking signals (existing in `session-server`).
- S3 media consistency (`media-collection` indexes).
- UI design for remote/ambient contexts (collab with art direction).

## Open questions
1. Should `tv` respect per-user privacy toggles for moods or private pieces?
2. Do we need editorial control panel to pin featured segments during events?
3. How to blend audio pieces without clashing (ducking, crossfade)?
4. Should multi-screen sync default to global `tv` channel or per-session invites?
5. Is there appetite for generative interstitials between slots (KidLisp mini loops)?

## Next steps
- Validate data availability for follow graph (confirm collection naming and access path).
- Draft `/tv-feed` response schema and review with client implementers.
- Prototype slot animation in KidLisp using static JSON to de-risk motion design.
- Schedule infrastructure check for Redis throughput and Netlify function cold starts.

## Appendix: existing APIs to leverage
- `GET /profile/{handle}` → resolve user `sub`, latest mood.
- `GET /media-collection?for={sub}/painting` → painting asset URLs.
- `GET /media-collection?for={sub}/piece` → code pieces for remix suggestions.
- `GET /mood?handle=@a,@b` → moods across handles (supports comma list).
- `POST /track-media` → log playback & interactions.
- `POST /mood` → publish mood updates (reuse for inline composer).
