# Chat Hearts — Implementation Report

**Date:** 2026-02-25
**Feature:** Heart reactions on chat messages (`chat.mjs`, `laer-klokken.mjs`)

---

## What Was Built

Users can now tap a chat message to open the action modal and press `<3` to heart it. Hearts are stored in MongoDB, counts appear inline next to timestamps, and updates propagate live to all connected clients via the session-server WebSocket.

---

## Data Layer — MongoDB `hearts` Collection

Single shared collection. `type` field discriminates content kind so hearts can later apply to paintings, moods, pieces, etc.

**Schema:**
```json
{
  "_id": ObjectId,
  "user": "auth0|63effeeb2a7d55f8098d62f9",
  "type": "chat-system",
  "for": "699f2425b88e72d86a3c326e",
  "when": "2026-02-25T16:32:36.899Z"
}
```

- `user` — Auth0 `sub`, matching `moods`, `chat-system`, `kidlisp`, `news-votes`
- `type` — content kind: `"chat-system"` | `"chat-clock"` | (future: `"painting"`, `"mood"`)
- `for` — `_id.toString()` of the target document, following `tickets` convention
- `when` — server timestamp

**Indexes:**
- `{ type, for, user }` — **unique** — one heart per user per item; duplicate insert triggers toggle-off
- `{ type, for }` — fast count aggregation

---

## Files Changed

### New: `session-server/hearts.mjs`

Shared helper used by both the session server and the Netlify function.

- `ensureIndexes(db)` — creates both indexes at startup
- `toggleHeart(db, { user, type, for })` — insert → on dup-key delete; returns `{ hearted, count }`
- `countHearts(db, type, forIds)` — bulk aggregate; returns `{ [forId]: count }`

### Modified: `session-server/chat-manager.mjs`

- Imports `hearts.mjs` helpers
- `init()` calls `ensureHeartsIndexes` after MongoDB connect
- `handleConnection` (now `async`) loads `heartCounts` via `countHearts` and includes them in the `connected` payload
- `handleMessage` routes `chat:heart` to new `handleChatHeart` method
- `handleChatHeart` — verifies auth token, calls `toggleHeart`, broadcasts `message:hearts { for, count }` to all instance clients

### New: `system/netlify/functions/chat-heart.mjs`

REST fallback for direct API callers (POST).

- Reads `{ for, type }` from body
- Verifies JWT via `authorize()`
- Toggles heart in `hearts` collection
- Returns `{ for, type, hearted, count }`

### Modified: `system/netlify/functions/chat-messages.mjs`

After fetching messages, aggregates heart counts from the `hearts` collection and joins them onto each message object. Each message now includes `id` (string) and `hearts` (count, default 0).

### Modified: `system/public/aesthetic.computer/lib/chat.mjs`

- `this.system.hearts` — `Map<id, { count, heartedByMe }>` for local state
- `this.system.sendHeart(id, token)` — optimistically toggles local state, sends `chat:heart` over WebSocket
- `connected` handler seeds the hearts map from `heartCounts` and per-message `hearts` fields
- `message:hearts` handler updates map entry (preserving `heartedByMe`), sets `layoutChanged` to trigger repaint

### Modified: `system/public/aesthetic.computer/disks/chat.mjs`

- **Modal:** `<3` / `♥` button added to `messageCopyModal` for all messages (not just own); pink theme; filled heart shown if `heartedByMe`
- **Timestamp row:** heart count rendered inline in `MatrixChunky8` if `> 0`, faded pink (`♥N`)
- **Layout:** `btnCount` now 3 or 4 depending on delete eligibility; button widths adjusted to fit

---

## Message Flow

```
Client taps <3
  → chat.system.sendHeart(id, token)         [optimistic local update]
  → WS send: { type: "chat:heart", content: { for, token } }

Session server receives chat:heart
  → verify token → get sub
  → toggleHeart(db, { user, type, for })
  → broadcast to all in instance:
       { type: "message:hearts", content: { for, count } }

All clients receive message:hearts
  → update hearts Map
  → needsPaint() → repaint timestamp row
```

---

## Status

| Layer | Status |
|-------|--------|
| MongoDB schema + indexes | ✅ Designed, created on first toggle |
| `hearts.mjs` helper | ✅ Deployed (session-server) |
| `chat-manager.mjs` updates | ✅ Committed — **needs session server redeploy** |
| `chat-heart.mjs` Netlify function | ✅ Auto-deployed on push |
| `chat-messages.mjs` heart join | ✅ Auto-deployed on push |
| `lib/chat.mjs` client library | ✅ Live |
| `disks/chat.mjs` UI | ✅ Live |
| `laer-klokken.mjs` | ✅ Inherits automatically (shared `disks/chat.mjs` code) |

### Blocking: Session Server Redeploy

The session server is a long-running process — it does **not** auto-deploy with Netlify pushes. Until `npm run session:publish` is run from an environment with Docker:

- `chat:heart` WebSocket messages are silently dropped by the old server
- No hearts are written to MongoDB
- `message:hearts` broadcasts never fire
- The `connected` payload does not include `heartCounts`

The Netlify REST endpoint (`chat-heart.mjs`) does work, but the client currently sends via WebSocket only.

**To deploy:** `npm run session:publish` (requires Docker — run from local machine, not Codespace)

---

## Open / Future Work

- `heartedByMe` on initial load (requires auth token in the GET request to `chat-messages.mjs`)
- Heart animation / bounce on count increment
- Support for non-chat content types (paintings, moods) — schema already supports it
- Anonymous user UX — currently heart button is hidden for logged-out users; could show greyed-out instead
