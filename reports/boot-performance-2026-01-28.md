# Aesthetic Computer Boot Performance Report
**Date:** January 28, 2026

## Executive Summary

Based on analysis of the boot logs, the total boot time is **~8.7 seconds** from initial load to `disk-loaded-and-booted`. The primary bottleneck is the **`userExists` API fetch which takes 4430ms** (~51% of total boot time).

### ✅ Fixes Implemented

1. **Redis caching for `/user` endpoint** - [user.js](../system/netlify/functions/user.js)
   - Cache key: `email:{email}:{tenant}:{withHandle}` 
   - TTL: 5 minutes (user data rarely changes)
   - Expected savings: **~4000ms on cache hit**

2. **In-memory M2M token caching** - [authorization.mjs](../system/backend/authorization.mjs)
   - Caches Auth0 M2M tokens for 23 hours (tokens valid 24h)
   - Eliminates repeated token requests within same serverless instance
   - Expected savings: **~500-1000ms on warm instances**

3. **Skip Auth0 for anonymous users** - [boot.mjs](../system/public/aesthetic.computer/boot.mjs)
   - Checks localStorage for `@@auth0spajs@@` keys before loading Auth0
   - If no cached auth state, skips entire Auth0 flow and sends `session:started` immediately
   - Expected savings: **~2000-4000ms for anonymous users**

4. **Faster WebSocket fallback** - [boot.mjs](../system/public/aesthetic.computer/boot.mjs) + [module-loader.mjs](../system/public/aesthetic.computer/module-loader.mjs)
   - Reduced WebSocket connection timeout from 1500ms → 800ms
   - WS failure no longer blocks IndexedDB init
   - Expected savings: **~700ms when WS unavailable**

5. **Eager WebSocket connection in HTML** - [index.mjs](../system/netlify/functions/index.mjs) + [module-loader.mjs](../system/public/aesthetic.computer/module-loader.mjs)
   - Starts WebSocket connection in inline `<script>` BEFORE boot.mjs loads
   - By the time boot.mjs runs, WS is already connected
   - module-loader picks up the early connection via `window.acEarlyWS`
   - Expected savings: **~500-1000ms** (WS connects during HTML parse)

## Boot Timeline Analysis

| Phase | Time | Delta | % of Total |
|-------|------|-------|-----------|
| initializing aesthetic.computer | +7ms | 7ms | 0.1% |
| checking storage access | +8ms | 1ms | 0% |
| loading core modules | +9ms | 1ms | 0% |
| **📦 ModuleLoader ready** | - | ~2000ms | 23% |
| parsing: prompt | +2009ms | 2000ms | 23% |
| booting: prompt | +2010ms | 1ms | 0% |
| initializing graphics | +2012ms | 2ms | 0% |
| loading auth0 script | +2020ms | 8ms | 0.1% |
| auth0 script loaded | +2050ms | 30ms | 0.3% |
| auth0 client created | +2051ms | 1ms | 0% |
| auth0 isAuthenticated check | +2051ms | 0ms | 0% |
| auth0 getTokenSilently | +2051ms | 0ms | 0% |
| auth0 getUser | +2051ms | 0ms | 0% |
| loading disk: prompt | +2070ms | 19ms | 0.2% |
| **⚠️ userExists fetch** | +6481ms | **4430ms** | **51%** |
| connecting to worker | +7962ms | 1481ms | 17% |
| setting up display | +8018ms | 56ms | 0.6% |
| running boot | +8305ms | 287ms | 3.3% |
| fetching: prompt disk | +8503ms | 198ms | 2.3% |
| module imported | +8683ms | 180ms | 2.1% |
| loaded: prompt | +8685ms | 2ms | 0% |

---

## 🔴 Critical Bottlenecks

### 1. **`/user` API Endpoint: 4430ms (51% of boot time)**

**Location:** [boot.mjs#L1319-L1327](system/public/aesthetic.computer/boot.mjs#L1319-L1327)

```javascript
const userExists = await fetch(
  `/user?from=${encodeURIComponent(userProfile.email)}&tenant=aesthetic`,
);
```

**Root Cause:** The `/user` Netlify function ([user.js](system/netlify/functions/user.js)) calls `userIDFromEmail()` which:
1. Makes an M2M token request to Auth0 (`getAccessToken()`)
2. Queries Auth0's `/api/v2/users-by-email` endpoint
3. Optionally queries MongoDB for handle

**Backend Flow:**
```
Client → Netlify Function → Auth0 Token Request → Auth0 API → (MongoDB for handle) → Client
```

**Impact:** This **blocks** the `session:started` event from being sent to the disk worker.

---

### 2. **Module Loader Initialization: ~2000ms (23% of boot time)**

**Location:** [boot.mjs#L10-L45](system/public/aesthetic.computer/boot.mjs#L10-L45) and [module-loader.mjs](system/public/aesthetic.computer/module-loader.mjs)

The WebSocket module loader initialization overlaps with boot but causes a visible gap:
- `loading core modules` at +9ms
- `parsing: prompt` at +2009ms

This 2-second gap is the module loader establishing WebSocket connection + IndexedDB + module prefetch.

---

### 3. **Worker Connection: 1481ms (17% of boot time)**

**Location:** [bios.mjs#L3789-L3910](system/public/aesthetic.computer/bios.mjs#L3789-L3910)

The web worker creation and first message exchange adds significant latency. Worker initialization waits for:
1. Worker script to load and parse
2. `worker-ready` signal from disk.mjs
3. First message exchange

---

## 🟡 Secondary Bottlenecks

### 4. **Auth0 Blocking Chain**

The auth flow is sequential and blocking:
```javascript
await auth0Client.isAuthenticated();
await auth0Client.getTokenSilently();
await auth0Client.getUser();
await fetch('/user?from=...');  // ← THE BIG ONE
```

Total Auth Time: **6481ms** (75% of boot)

### 5. **Disk Module Fetch + Import**

- Fetch prompt.mjs: 147ms
- Dynamic import: 17ms
- Total: 164ms

This is relatively fast but still sequential.

---

## 🟢 Recommendations

### Immediate Impact (High Priority)

#### 1. **Cache `/user` Response in Redis/KV Store**

**Expected Savings: ~4000ms**

The user's `sub` and `handle` rarely change. Cache the response:

```javascript
// In user.js Netlify function
const cacheKey = `user:${email}:${tenant}`;
const cached = await KeyValue.get("userCache", cacheKey);
if (cached) return respond(200, JSON.parse(cached));

// ... fetch from Auth0 ...

await KeyValue.set("userCache", cacheKey, JSON.stringify(result), 300); // 5min TTL
```

#### 2. **Parallelize Auth Calls with Disk Loading**

**Expected Savings: ~1500ms**

Start the disk/worker loading in parallel with the userExists fetch:

```javascript
// Start these in parallel
const [userResponse, _] = await Promise.all([
  fetch(`/user?from=${email}&tenant=aesthetic`),
  startDiskLoad()  // Start worker creation early
]);
```

#### 3. **Precompute Auth0 M2M Token**

**Expected Savings: ~500-1000ms**

Cache the Auth0 M2M token server-side with appropriate TTL (tokens are valid for 24h):

```javascript
// In authorization.mjs
let cachedToken = null;
let tokenExpiry = 0;

async function getAccessToken(got, tenant) {
  if (cachedToken && Date.now() < tokenExpiry) {
    return cachedToken;
  }
  // ... fetch new token ...
  tokenExpiry = Date.now() + 23 * 60 * 60 * 1000; // 23 hours
  return cachedToken = tokenResponse.body.access_token;
}
```

### Medium Priority

#### 4. **Lazy Load Auth0 SDK**

Only load Auth0 script if user is likely authenticated (check localStorage first):

```javascript
const hasSession = localStorage.getItem('auth0.is.authenticated');
if (hasSession !== 'true') {
  // Skip auth flow for anonymous users
  sendSessionStarted(null);
} else {
  await loadAuth0();
}
```

#### 5. **Use Service Worker for Module Caching**

The service worker is registered but module caching could be more aggressive. Pre-cache critical modules during idle time.

#### 6. **WebSocket Module Loader Timeout Tuning**

Current timeout is 1.5s for WebSocket connection. Consider:
- Faster fallback to HTTP if WS fails
- Don't block boot waiting for WS

### Lower Priority

#### 7. **Inline Critical CSS/JS**

The boot canvas animation loads separately. Consider inlining boot canvas code in the HTML.

#### 8. **Use `modulepreload` for Critical Modules**

```html
<link rel="modulepreload" href="/aesthetic.computer/bios.mjs">
<link rel="modulepreload" href="/aesthetic.computer/lib/disk.mjs">
```

---

## Projected Impact

| Optimization | Estimated Savings |
|--------------|-------------------|
| Cache `/user` response | 3500-4000ms |
| Parallelize auth + disk | 1000-1500ms |
| Cache M2M token | 500-1000ms |
| Lazy auth0 (anon users) | 2000ms+ |
| **Total Potential** | **5000-8000ms** |

With these optimizations, boot time could potentially drop from **8.7s to ~2-3s** for authenticated users and **<1s** for anonymous users.

---

## Quick Wins (< 1 hour implementation)

1. **Add Redis caching to `/user` endpoint** - Biggest single impact
2. **Cache Auth0 M2M token in memory** - Server-side only
3. **Skip auth flow for anonymous users** - Check localStorage first

---

## Architecture Notes

The boot sequence currently follows this critical path:

```
boot.mjs
  ↓ (parallel)
  ├── ModuleLoader.init() [WebSocket + IndexedDB]
  └── Load bios.mjs + parse.mjs
        ↓
      bios.boot()
        ↓ (sequential)
        ├── Auth0 script load
        ├── Auth0 client create
        ├── isAuthenticated()
        ├── getTokenSilently()  
        ├── getUser()
        ├── fetch('/user?from=...')  ← BOTTLENECK
        └── session:started → Disk Worker
              ↓
            Load prompt.mjs
              ↓
            disk-loaded-and-booted
```

The key insight is that `session:started` **blocks the disk worker** from proceeding with piece loading. By decoupling auth from the critical boot path, we can significantly reduce perceived load time.
