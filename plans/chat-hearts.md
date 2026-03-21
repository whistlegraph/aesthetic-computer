# Chat Hearts — Implementation Plan

**Feature:** Users tap a message in chat to open the action modal, then tap a `<3` button to heart a message. Hearts are stored in MongoDB, counts display next to timestamps, and updates propagate live via the session-server WebSocket broadcast system.

---

## Scope

Applies to:
- `chat.mjs` (aesthetic.computer main chat)
- `laer-klokken.mjs` (clock chat, warm color theme)

Both share the same `chat-manager.mjs` session server and the same `chat-messages.mjs` REST API path, so the plumbing is written once.

---

## 1. MongoDB — `hearts` Collection

**Single shared collection.** Hearts may eventually apply to any content type
(chat messages, paintings, moods, pieces, etc.), so this mirrors the `news-votes`
pattern rather than per-content-type collections.

Real `news-votes` doc for reference:
```json
{ "itemType": "post", "itemId": "n8b9", "user": "auth0|...", "when": Date }
```

### Document Shape

```json
{
  "_id": ObjectId,
  "user": "auth0|63effeeb2a7d55f8098d62f9",
  "type": "chat-system",
  "for": "699f2425b88e72d86a3c326e",
  "when": "2026-02-25T16:32:36.899Z"
}
```

| Field | Type | Notes |
|-------|------|-------|
| `_id` | ObjectId | Auto-generated |
| `user` | string | Auth0 sub — same as `moods`, `chat-system`, `kidlisp`, `news-votes` |
| `type` | string | Content type: `"chat-system"`, `"chat-clock"`, `"painting"`, `"mood"`, etc. |
| `for` | string | `_id` (as string) of the target document — follows `tickets` `"for"` convention |
| `when` | Date | Server time at heart creation |

**Indexes:**
- `{ type: 1, for: 1, user: 1 }` — **unique**, one heart per user per item
- `{ type: 1, for: 1 }` — fast count aggregation per item

### Invariants
- One heart per `(type, for, user)` triple — hearting again **removes** the heart (toggle).
- Hearts are not cascade-deleted when the target document is soft-deleted.

---

## 2. Netlify Serverless Function — `chat-heart.mjs`

**File:** `system/netlify/functions/chat-heart.mjs`

**Method:** `POST`

**Request body:**
```json
{
  "for": "699f2425b88e72d86a3c326e",
  "type": "chat-system",
  "token": "<Auth0 JWT>"
}
```

**Logic:**
1. Verify JWT via Auth0 introspection (same pattern as `chat-messages.mjs`).
2. Extract `sub` from token → store as `user`.
3. Attempt `insertOne` into `hearts` with `{ user, type, for, when }`.
   - If duplicate key error on `{ type, for, user }` index → `deleteOne` (toggle off).
4. Re-count hearts for `{ type, for }` via `countDocuments`.
5. Return `{ for, type, count, hearted: bool }`.

**This endpoint is used only for the initial toggle action.** Real-time propagation goes through the session server (see §4).

---

## 3. REST API — Message Loading with Heart Counts

**File:** `system/netlify/functions/chat-messages.mjs`

**Change:** After fetching the recent messages array, join heart counts from the shared `hearts` collection:

```js
const messageIds = messages.map(m => m._id.toString());
const heartCounts = await db
  .collection("hearts")
  .aggregate([
    { $match: { type: `chat-${instance}`, for: { $in: messageIds } } },
    { $group: { _id: "$for", count: { $sum: 1 } } }
  ])
  .toArray();

const heartMap = Object.fromEntries(heartCounts.map(h => [h._id, h.count]));
messages.forEach(m => {
  m.hearts = heartMap[m._id.toString()] ?? 0;
});
```

Also, if a user token is present in the request (optional header), mark which messages the requesting user has already hearted (`heartedByMe: bool`) so the UI can show a filled vs. outline heart.

---

## 4. Session Server — `chat-manager.mjs`

**File:** `session-server/chat-manager.mjs`

### New incoming message type: `chat:heart`

```js
// Client sends:
{ type: "chat:heart", content: { for: "<message id>", token } }
```

**Server handler `handleChatHeart(ws, instance, data)`:**
1. Verify token → get `sub` → use as `user`.
2. Toggle heart in shared `hearts` collection with `{ user, type: "chat-system" | "chat-clock", for }`.
3. Count hearts for `{ type, for }`.
4. **Broadcast to all clients in the instance:**
   ```js
   this.broadcast(instance, this.pack("message:hearts", {
     for: messageId,
     count,
   }));
   ```

### New outgoing broadcast type: `message:hearts`

```json
{
  "type": "message:hearts",
  "content": {
    "for": "699f2425b88e72d86a3c326e",
    "count": 7
  }
}
```

Every connected client updates their local message heart count on receipt.

### On `connected` payload

The existing `connected` message sends `{ messages, chatters, handles, id }`.
Add `heartCounts` — a map of `{ [messageId]: count }` for the initial message window — so late-joining clients see current counts without a separate request.

---

## 5. Client Library — `lib/chat.mjs`

**File:** `system/public/aesthetic.computer/lib/chat.mjs`

### Changes

1. **Store heart counts locally.**
   Add a `Map<for, { count, heartedByMe }>` on the `Chat` instance (keyed by message `id`).

2. **Populate from `connected` payload.**
   When the `connected` event fires, merge `heartCounts` into the local map.

3. **Handle `message:hearts` broadcast.**
   On receipt, update the local map entry for `content.for` and call a registered `onHeartsUpdate(id, count)` callback so the piece can re-render.

4. **Expose `sendHeart(id)` method.**
   Sends `{ type: "chat:heart", content: { for: id, token } }` over the WebSocket. Also optimistically toggles `heartedByMe` locally so the UI feels instant.

---

## 6. Chat UI — `disks/chat.mjs`

### 6a. Message Modal — Add `<3` Button

The existing `messageCopyModal` panel appears when a user taps a message. It currently shows **Copy** and **Delete** (for own messages).

**Change:** Add a `<3` button to this panel for **all messages** (not just own).

Button label:
- Outline heart `<3` if current user has not hearted the message.
- Filled/colored heart `♥` if current user has already hearted.

On press:
1. Call `chat.sendHeart(message.id)`.
2. Dismiss modal.
3. The `onHeartsUpdate` callback will trigger a re-render with the new count.

### 6b. Timestamp Area — Heart Count Display

Each message already renders a timestamp near the bottom of its layout block (see existing `timestamp` layout field on messages).

**Change:** After the timestamp, print the heart count if > 0.

Rendering:
- `♥ 3` in a small, muted color (e.g., 60% opacity of the message text color).
- Uses the same per-message font as the rest of the message for consistency.
- Count updates via `onHeartsUpdate` → triggers a repaint (same as how `message:update` currently forces a layout pass).

### 6c. Layout / Repaint

Hearts don't change message height — they're inline with the timestamp row.
On `onHeartsUpdate`, find the message in the local list by `id`, update its `hearts` field, mark the layout dirty for that message, and queue a repaint. No full re-layout needed.

---

## 7. `laer-klokken.mjs`

Laer-Klokken imports the chat library and overrides colors/theme.
**No structural changes needed** — it inherits hearts automatically because:
- It connects to `chat-clock` instance on the session server.
- Hearts are stored in the shared `hearts` collection with `type: "chat-clock"`.
- The client library and UI changes are in shared code.

If Laer-Klokken uses a custom modal rendering path, verify the `messageCopyModal` code path is shared or duplicated and apply the `<3` button there too.

---

## 8. Data & API Summary

| Layer | Change | Notes |
|-------|--------|-------|
| MongoDB | Single `hearts` collection, `type` discriminates content | Unique index on `(type, for, user)` |
| Netlify Function | New `chat-heart.mjs` (POST toggle + count) | Used for direct REST toggle if needed |
| `chat-messages.mjs` | Aggregate heart counts into message load | Optional `heartedByMe` per user |
| `chat-manager.mjs` | Handle `chat:heart` → broadcast `message:hearts` | Shared `hearts.mjs` helper |
| `lib/chat.mjs` | Local heart map, `sendHeart()`, `onHeartsUpdate` cb | Optimistic toggle |
| `disks/chat.mjs` | `<3` button in modal, count in timestamp row | Repaint on update |
| `laer-klokken.mjs` | Inherits automatically; verify modal path | May need minor check |

---

## 9. Implementation Order

1. **MongoDB + helper** — write `session-server/hearts.mjs` with toggle + count logic.
2. **Session server** — add `chat:heart` handler + `message:hearts` broadcast + `connected` heartCounts.
3. **Netlify function** — `chat-heart.mjs` (REST fallback, and for any direct API callers).
4. **`chat-messages.mjs`** — join heart counts on load.
5. **`lib/chat.mjs`** — local map, `sendHeart`, `onHeartsUpdate`.
6. **`disks/chat.mjs`** — `<3` button in modal, count rendering in timestamp.
7. **`laer-klokken.mjs`** — verify modal path, smoke test.

---

## Open Questions

- **Anonymous / logged-out users**: Can they see heart counts? (Yes, counts load with messages.) Can they heart? (No — `sendHeart` requires a token; show the button greyed out or hidden.)
- **Max hearts per message**: No cap proposed — first ship, iterate.
- **Heart animation**: Out of scope for v1; could add a small bounce/flash on the count increment later.
- **Notification**: No push notification for being hearted in v1.
