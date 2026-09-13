# 🍪 Sotce Net — Agent Context

> A paid diary network by **Sotce** & **Aesthetic Computer** — live at [sotce.net](https://sotce.net).

---

## 🧠 What Is This?

A subscription diary platform. The writer (**@amelia** / Sotce) publishes pages; subscribers read, touch, and chat. Private digital magazine with real-time social features and a warm, paper-like, cookie-themed aesthetic.

---

## 🏗️ Architecture

**One giant Netlify function** — `sotce-net.mjs` (~10,270 lines). No framework, no build step. Server-rendered HTML via tagged template literals. Handles GET routes (full HTML pages) and POST/DELETE routes (REST API).

### ⚙️ Stack

| Layer | Tech |
|-------|------|
| 🖥️ Runtime | Netlify Functions (Node.js serverless) |
| 🎨 Frontend | Vanilla JS, Canvas API (page rendering), Monaco Editor (chat input) |
| 🔐 Auth | Auth0 SPA SDK — tenant `sotce` at `sotce.us.auth0.com` |
| 💳 Payments | Stripe subscriptions via Checkout Sessions |
| 🗄️ Database | MongoDB — `sotce-pages`, `sotce-asks`, `sotce-touches`, `chat-sotce`, `@handles` |
| ⚡ Cache | Redis (subscription status via `kv.mjs`) |
| 💬 Real-time | WebSocket chat via session server (room: `chat-sotce`) |
| 🔤 Fonts | Helvetica (custom woff/ttf), Carlito (Google), Wingdings 2 |

### 📁 Key Files

| File | What |
|------|------|
| `system/netlify/functions/sotce-net.mjs` | 🍪 **The entire app** |
| `system/backend/sotce-net-constants.mjs` | 💳 Stripe keys, price/product IDs, SMTP creds |
| `system/backend/authorization.mjs` | 🔐 `authorize()`, `hasAdmin()`, `handleFor()`, `deleteUser()` |
| `system/backend/database.mjs` | 🗄️ MongoDB `connect()` → `{ db, disconnect }` |
| `system/backend/kv.mjs` | ⚡ Redis `connect/get/set/del/disconnect` |
| `system/backend/http.mjs` | 📡 `respond(status, body, headers)` |
| `system/backend/shell.mjs` | 📝 `shell.log()` / `.warn()` / `.error()` |
| `system/public/aesthetic.computer/lib/chat.mjs` | 💬 Client Chat class (WebSocket wrapper) |

### 🖼️ Assets

Served from `https://assets.aesthetic.computer/sotce-net/` (prod) or `/assets/sotce-net/` (dev).
Key: `cookie.png`, `cookie-open.png`, `thumbnail.png`, `helvetica.woff`, `helvetica-bold.woff`, `Wingdings 2.ttf`.

---

## 🛣️ Routes

### 🌐 GET (HTML Pages)

| Route | What |
|-------|------|
| `/` | 🏠 Gate (login/subscribe) or Garden (page feed) |
| `/chat` | 💬 Real-time chat |
| `/gate` | 🚪 Login/subscribe screen |
| `/write` | ✍️ Page editor (admin) |
| `/ask` | ❓ Question submission (subscribers) |
| `/respond` | 📝 Question response editor (admin) |
| `/page/:n` | 📄 Deep link to diary page |
| `/q/:n` | ❓ Deep link to Q&A |
| `/privacy-policy` | 📜 Privacy policy |

### 📡 API Endpoints

| Method | Path | Auth | What |
|--------|------|------|------|
| GET | `/subscribers` | — | Overall subscriber count shown on the gate |
| GET | `/active-subscribers` | — | Active subscriber count for internal metrics |
| POST | `/subscribe` | — | Stripe Checkout session |
| POST | `/notification-choice` | Bearer | Record minimized push permission/toggle state |
| POST | `/subscribed` | Bearer | Check sub + fetch pages/questions |
| POST | `/cancel` | Bearer | Cancel subscription |
| POST | `/write-a-page` | 👑 Admin | Draft CRUD + publish |
| POST | `/touch-a-page` | 🔑 Sub | Record page touch |
| POST | `/delete-account` | Bearer | Full account nuke |
| POST | `/ask` | 🔑 Sub | Submit question |
| GET | `/asks` | Bearer | User's own questions |
| GET | `/asks/pending` | 👑 Admin | Unanswered questions |
| POST | `/ask/:id/respond` | 👑 Admin | Answer question |
| POST | `/ask/:id/save-draft` | 👑 Admin | Save draft response |
| POST | `/ask/:id/reject` | 👑 Admin | Reject question |
| DELETE | `/ask/:id` | Owner | Delete own pending question |
| DELETE | `/asks/clear-all` | 👑 Admin | Clear all (dev tool) |

---

## 👤 User Roles

| Role | Access |
|------|--------|
| 🔒 Logged out | Gate, subscriber count, privacy policy |
| 📧 Unverified | Email verification prompt (polls Auth0) |
| ✅ Verified (no sub) | Subscribe button, read-only chat |
| 🔑 Subscribed | Garden, chat, touch pages, ask questions |
| 👑 Admin | Write pages, respond to Qs, pending Qs. Emails: `me@jas.life`, `sotce.net@gmail.com` (bypass Stripe via `ADMIN_EMAILS`) |

---

## 🎨 UI Concepts

- **🚪 Gate** — Cookie login screen. Click cookie → enter garden. Status-specific buttons per role.
- **🌻 Garden** — Canvas-rendered page feed. 4:5 cards, one at a time, swipe/drag/arrow to navigate. Card flip (ear tap) reveals who "touched" it.
- **💬 Chat** — WebSocket real-time. Monaco Editor input with @handle autocomplete, syntax highlighting for `-5-` (diary links) and `*3*` (Q&A links), URL auto-linking, unread badge.
- **✍️ Page Editor** — Admin. 19-line max, justified+hyphenated, draft/keep/crumple/publish.
- **❓ Ask Editor** — Subscriber. 5-line max, localStorage draft, "my questions" list view.
- **📝 Respond Editor** — Admin. Prev/next Qs, auto-draft to server, dynamic response line count.

---

## 🗄️ Database Schemas

### `sotce-pages`
```js
{ _id, user: "auth0|sub", words: "...", when: Date,
  state: "draft"|"published"|"crumpled",
  questionId?: "...", isQA?: true, updatedAt?: Date }
```

### `sotce-asks`
```js
{ _id, user, handle?, question, when: Date,
  state: "pending"|"answered"|"rejected",
  answer?, answeredBy?, answeredAt?: ISO,
  draftAnswer?, draftStartedAt?: ISO, draftLastEditedAt?: ISO,
  rejectedBy?, rejectedAt?: ISO }
```

### `sotce-touches`
```js
{ _id, user, page: ObjectId, when: Date }
// Unique index on (user, page)
```

### `sotce-notification-choices`
```js
{ user: "sotce-auth0|sub", deviceId, label, platform,
  choice: "denied"|"dismissed"|"disabled"|"enabled"|"error",
  source: "auto"|"bell"|"observed"|"register",
  createdAt: Date, updatedAt: Date,
  deniedCount?, dismissedCount?, disabledCount?, enabledCount?, errorCount? }
```

### `chat-sotce`
```js
{ text, user, from: "@handle", when: Date, count: N }
```

### `@handles`
```js
{ _id: "sotce-{sub}", handle: "amelia" }
// Shared across AC + sotce tenants
```

---

## 🎭 Theme System

CSS custom properties in `:root` (light) and `@media (prefers-color-scheme: dark)`. Canvas reads them via `getThemeColors()` → `getComputedStyle()`.

**Key vars:** `--card-background`, `--card-text`, `--card-border`, `--card-ear`, `--question-card-background`, `--garden-background`, `--chat-background`, `--line-height: 1.76em`, `--page-font: "Helvetica"`, `--max-lines: 19`

**Light:** Pink/cream paper. **Dark:** Warm brown/sepia.

---

## ⚡ Redis Cache

Key `sotce-subscribed` → hash field `{user.sub}` → JSON `{status, current_period_end}`. Set after Stripe lookup, auto-invalidates on expiry or cancellation.

---

## 🧑‍💻 Dev Mode

When `NETLIFY_DEV` is truthy:
- 💳 Test Stripe keys
- 📂 Local asset path `/assets/sotce-net/`
- 🔄 WebSocket live reload (`wss://localhost:8889`)
- 🛣️ URL prefix `/sotce-net` (e.g., `/sotce-net/chat`)
- 💾 Session preserved via `?session-sotce=retrieve`

Run: `npm run aesthetic` (full stack) or `npm run site` (client only).

---

## 🔧 Common Dev Tasks

**➕ New route:** Add path to the `if` condition ~line 270, update `title` assignment, add client routing in `updatePath()`.

**➕ New API endpoint:** Add `else if` clause after ~line 9238. Use `authorize(event.headers, "sotce")` for auth, `respond(status, body)` to return.

**🎨 Canvas changes:** Start at ~line 6367 (`USE_CANVAS_GARDEN`). Key fns: `renderPage()`, `renderCardBack()`, `wrapText()`, `getThemeColors()`. Animation in `loop()`.

**💳 Subscription logic:** `subscribed(user)` checks Redis cache → Stripe. Admin emails bypass. Cache TTL from `current_period_end`.

**💬 Chat:** Connects to `sotce` room via `new Chat(dev, undefined, disconnectCallback)`. Messages via session server WebSocket. History on `connected` event.

---

## 🗺️ Source Map — `sotce-net.mjs`

### 📦 Server-Side Preamble (1–265)

| Lines | What |
|-------|------|
| 1–62 | 📋 TODO list |
| 63–98 | ♻️ Env, imports (Stripe, DB, auth, KV) |
| 106 | 🚀 `export const handler` |
| 129–226 | 💳 `subscribed(user)` — Stripe + Redis cache check |
| 230–264 | 📊 `getActiveSubscriptionCount()` |

### 🎨 CSS (319–3209)

| Lines | What |
|-------|------|
| 319–510 | 🌗 Theme variables (light `:root` + dark `@media`) |
| 511–860 | 🌑 Dark mode DOM overrides |
| 860–1010 | 📐 Base layout (html, body, scroll, print) |
| 1010–1280 | 📄 Page/card rendering (font size, 4:5 ratio) |
| 1280–1620 | ❓ Ask editor styles |
| 1620–1870 | 📝 Respond editor styles |
| 1870–2100 | 🌻 Garden layout (binding, FYP scroll-snap) |
| 2100–2500 | 🐕 Ear fold, backpage, hover states |
| 2500–2770 | 🔘 Buttons, nav, cookie menu |
| 2770–3100 | 🚪 Gate, spinner, chat, veil, tooltips |

### 💬 Client JS — Chat (3227–4140)

| Lines | What |
|-------|------|
| 3227–3280 | 🗺️ Env, `updatePath()`, platform detection |
| 3280–3310 | 💬 Chat init, `connect("sotce")` |
| 3312–3420 | 🖱️ Chat DOM, click/hover handlers, page previews |
| 3420–3550 | 🔧 `chatScrollToBottom()`, `linkifyText()`, `chatAddMessage()`, fading |
| 3555–3740 | ⌨️ Chat input bar, autocomplete system |
| 3740–3950 | 🎹 Monaco Editor — language, themes, keybindings, mobile |
| 3951–4000 | 📤 `chatSend()` |
| 4011–4140 | 📥 `chat.system.receiver` — event handlers |

### 🚪 Gate & 🌻 Garden (4141–5040)

| Lines | What |
|-------|------|
| 4141–4245 | 🏷️ URL param flags, auth UI setup |
| 4246–4810 | 🚪 `gate(status, user, subscription)` — full gate builder |
| 4810–4920 | 🌻 `garden()` — top bar, chat button, ask/respond buttons |
| 4920–5040 | 🔘 Ask + Respond button handlers |

### ✍️ Editors (5045–6290)

| Lines | What |
|-------|------|
| 5045–5405 | ❓ `openAskEditor()` — 5-line question form, "my questions" |
| 5403–5775 | 📝 `openRespondEditor()` — admin Q&A, prev/next, draft save |
| 5831–6290 | ✍️ `compose()` / Write a Page — 19-line editor, keep/crumple/publish |

### 📦 Feed & Cache (6295–6365)

| Lines | What |
|-------|------|
| 6295–6365 | 🔀 Feed construction (pages + questions → sorted feed), IndexedDB cache |

### 🎨 Canvas Garden Renderer (6367–7470)

| Lines | What |
|-------|------|
| 6367 | 🚩 `USE_CANVAS_GARDEN = true` |
| 6370–6455 | 📊 State: page index, transitions, drag, flip, card dims |
| 6457–6485 | 🎭 `getThemeColors()` — CSS vars → canvas colors |
| 6487–6520 | 📐 `resizeCanvas()` — card sizing (4:5, centered) |
| 6525–6555 | 📥 `fetchPage(idx)` — IndexedDB → server fallback |
| 6560–6590 | 📏 `wrapText()` — canvas word wrap |
| 6590–6810 | 📄 `renderPage()` — diary card OR Q&A card |
| 6813–6928 | 🔙 `renderCardBack()` — touch info, @handle hit boxes |
| 6928–7090 | 🖼️ `render()` — main frame (flip, transition, drag) |
| 7090–7095 | 🔄 `loop()` — `requestAnimationFrame` loop |
| 7112–7170 | 👆 Pointer events (drag navigation) |
| 7171–7230 | ⌨️🖱️ Keyboard + wheel navigation |
| 7233–7470 | 🖱️👆 Click, mousemove, touch (ear flip, links, handles) |

### 📜 DOM Fallback Garden (7517–8530)

| Lines | What |
|-------|------|
| 7517–8530 | 📜 DOM-based rendering (fallback when canvas off): scroll-snap FYP, `renderPageContent()`, drag, ear/backpage DOM |

### 🔐 Auth Flow & Session (8532–9230)

| Lines | What |
|-------|------|
| 8534–8600 | 🔐 Auth0 client init |
| 8600–8650 | 💾 Session restore (localStorage / URL params) |
| 8650–9160 | 🔀 Main auth flow → gate/garden routing |
| 8820–8905 | 🔧 `veil()`, `unveil()`, `flash()`, `login()`, `signup()`, `subscribe()` |
| 8905–9015 | 💾 IndexedDB cache fns (`getCachedPage`, `setCachedPage`, `getCacheMeta`, `clearPageCache`) |
| 9017–9090 | 💳 Client `subscribed()`, `cancel()` |
| 9092–9170 | 🚪 `logout()`, `resend()`, `aesthetic()` |
| 9171–9228 | 📡 `userRequest(method, endpoint, body)` — authenticated API caller |

### 📡 REST API Endpoints (9238–10125)

| Lines | What |
|-------|------|
| 9238 | 📊 `GET /subscribers` |
| 9246 | 💳 `POST /subscribe` (Stripe checkout + dup guard) |
| 9321 | 🔑 `POST /subscribed` (sub check + pages/Qs fetch) |
| 9476 | ❌ `POST /cancel` |
| 9482 | ✍️ `POST /write-a-page` (draft CRUD + publish) |
| 9627 | 👆 `POST /touch-a-page` |
| 9698 | 🗑️ `POST /delete-account` |
| 9785 | ❓ `POST /ask` |
| 9823 | 📋 `GET /asks` |
| 9839 | 📋 `GET /asks/pending` |
| 9863 | 📝 `POST /ask/:id/respond` |
| 9926 | 💾 `POST /ask/:id/save-draft` |
| 9964 | 🚫 `POST /ask/:id/reject` |
| 10003 | 🧹 `DELETE /asks/clear-all` |
| 10017 | 🗑️ `DELETE /ask/:id` |
| 10047 | 📜 `GET /privacy-policy` |

### 🧩 Module-Level (10127–10271)

| Lines | What |
|-------|------|
| 10127 | 💳 `cancelSubscription()` — Stripe cancel + Redis clear |
| 10195 | 📊 `analyticsScript` — GA tag |
| 10214 | 🔄 `reloadScript` — dev WebSocket live reload |

---

## 📅 Date Convention

Comment timestamps: `YY.MM.DD.HH.MM` (e.g., `24.06.13.06.38` = Jun 13 2024 6:38 AM).

---

## 📋 Open TODOs

🔊 Sound (sine clicks/beeps) · 📸 Pictures in pages · 🔔 Chat notifications · 📧 Email blasts for new pages · 🧘 Meditation timer · 📟 Multi-user page feed · ♿ Accessibility (tab index, zoom, relational scroll) · 📰 Snippet endpoint (latest page on login)
