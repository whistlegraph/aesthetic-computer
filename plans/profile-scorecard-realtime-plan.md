# Profile Scorecard + Paintings Route Split

Date: 2026-02-27
Owner: platform/frontend

## Goal

Change user profile behavior from "painting portfolio" to "full user activity scorecard" while preserving easy access to a user's paintings in a dedicated `paintings.mjs` route.

Target behavior:
- `/@handle` shows a live profile scorecard (presence + activity + media summary).
- `paintings~@handle` shows the painting portfolio carousel (moved from current `profile.mjs` behavior).

## Current Routing + Data Flow (As Implemented)

### URL routing to profile

- Netlify catch-all sends app paths to the index function.
- `system/netlify.toml` routes `/*` -> `/.netlify/functions/index`.
- Server-side index routing rewrites `@handle` to load the `profile` disk source.
- `system/netlify/functions/index.mjs` rewrites local disk path to `profile` when path starts with `@`.
- Client-side loader also rewrites `@handle` to the `profile` piece and passes `@handle` as first param.
- `system/public/aesthetic.computer/lib/disk.mjs` rewrites slug `@handle` to hidden slug `profile` and sets params to include `@handle`.

### What profile currently does

- `system/public/aesthetic.computer/disks/profile.mjs`:
- resolves `visiting = params[0] || handle()`.
- fetches `/api/profile/:handle` for identity + latest mood.
- fetches `/media-collection?for=@handle/painting` for user painting URLs.
- loads paintings one-by-one and behaves like a portfolio viewer with prev/next, open in `painting`, and download.

### Realtime session data currently available

- `system/public/aesthetic.computer/lib/disk.mjs` already broadcasts `location:broadcast` over WebSocket on piece changes and periodic keep-alives.
- `session-server/session.mjs` stores per-client `handle`, `user`, and `location` from those broadcasts.
- `session-server/session.mjs` exposes:
- `GET /status` full presence snapshot.
- `WS /status-stream` live status updates every 2s with `clients[]`, including location and world membership/showing metadata.

## Existing HTTP Endpoints Useful for Scorecard

- `/api/profile/:handle` -> basic identity (`sub`) and latest mood.
- `/media-collection?for=@handle/painting` -> painting file URLs only (no rich metadata).
- `/api/painting-metadata?slug=...&handle=...` -> per-painting code/slug metadata.
- `/api/store-kidlisp?recent=true&handle=@handle&limit=...` -> recent KidLisp with timestamps and hits.
- `/api/store-clock?recent=true&limit=...` -> recent clocks with handle included (client-side filter needed today).
- `/api/mood/all?for=@handle` -> mood history.
- `/api/piece-hit` and `/api/piece-fans` -> piece-centric analytics (not user-centric timeline yet).
- `/api/metrics` -> platform-wide totals (global, not per-user).

## Gaps To Close

- No single endpoint returns "user scorecard" (counts + latest media + timeline).
- No dedicated paintings route piece; portfolio is embedded in `profile.mjs`.
- Realtime stream currently broadcasts all clients (`/status-stream`) instead of a per-handle feed.

## Proposed Route Contract

### Keep

- `/@handle` as canonical public profile URL.
- `profile` prompt command and `@handle` links as-is.

### Add

- New disk: `system/public/aesthetic.computer/disks/paintings.mjs`.
- Canonical portfolio route: `paintings~@handle`.

### Optional aliases

- `@handle/paintings` alias (implemented in client parse/load rewrite) -> `paintings~@handle`.
- `profile~@handle` continues working as scorecard alias.

## Proposed Data Contract

Introduce a new Netlify endpoint:

- `GET /api/profile-scorecard/:handle`
- Returns:
- `identity`: handle, sub, optional display fields.
- `presence`: online, currentPiece, worldPiece, showing, lastSeenAt.
- `counts`: paintings, pieces, kidlisp, clocks, tapes, moods.
- `recentMedia`: latest N per media type with slug/code/when.
- `activity`: unified reverse-chronological list (`[{type, when, label, ref}]`).
- `topPieces`: from `piece-user-hits` for this user.

Implementation notes:
- Resolve `sub` from `@handles`.
- Aggregate from Mongo collections: `paintings`, `pieces`, `kidlisp`, `clocks`, `tapes`, `moods`, `piece-user-hits`.
- Cache response briefly (30-60s) with optional `?fresh=true`.

## Realtime Strategy

### Phase 1 (fast path, minimal backend changes)

- In `profile.mjs`, open WebSocket to `wss://session-server.aesthetic.computer/status-stream`.
- Filter incoming `status.data.clients` by target handle.
- Update scorecard presence block in real time:
- online/offline
- current piece slug
- world piece + currently showing media
- ping/connection count

### Phase 2 (scalable / cleaner)

- Add targeted session-server stream:
- `WS /profile-stream?handle=@name` or message-based `profile:watch`.
- Server emits only relevant updates for that handle on:
- `location:broadcast`
- `world:*:join|show|hide|move|slug`
- connect/disconnect for matching handle.

### Activity freshness

- Begin with timed refresh of `/api/profile-scorecard/:handle` every 15-30s.
- Later: push activity events from write endpoints (`track-media`, `store-kidlisp`, `store-clock`, `mood`) through Redis -> session-server -> profile watchers.

## Frontend Plan

### `paintings.mjs`

- Start by moving the current painting-portfolio logic from `profile.mjs` with minimal visual changes.
- Inputs: `params[0]` handle.
- Data source initially remains `/media-collection?for=@handle/painting`.
- Keep existing controls (prev/next, jump to `painting`, download).

### New `profile.mjs`

- Replace painting carousel UI with scorecard blocks:
- header identity (`@handle`, mood, quick stats)
- live presence card (current piece, online state)
- last activity timeline
- recent media strip (painting/piece/kidlisp/clock/tape)
- CTA/link to `paintings~@handle`

## Rollout Steps

1. Add `paintings.mjs` by extracting current portfolio behavior from `profile.mjs`.
2. Update `profile.mjs` to scorecard UI using mocked data shape.
3. Add `/api/profile-scorecard/:handle` and wire profile data fetch.
4. Add realtime presence hookup via `/status-stream` filter.
5. Add optional alias rewrites (`@handle/paintings`) and update sitemap/help text.
6. Optimize with targeted profile stream and push activity events.

## Compatibility + Risks

- Keep `/@handle` canonical; no external link breakage expected.
- Keep `painting~@handle/timestamp` behavior unchanged.
- `status-stream` currently reveals global presence payload; acceptable short-term but should move to targeted stream for scale/privacy.
- `media-collection` returns URLs only; scorecard should not rely on it for activity ordering.

## Acceptance Criteria

- Visiting `/@handle` shows live scorecard, not painting carousel.
- Visiting `paintings~@handle` shows portfolio carousel.
- Presence card updates within ~2s when target user switches pieces.
- Activity/media sections show latest data without manual refresh.

