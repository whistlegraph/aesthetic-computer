# API Token System for MCP Authentication

**Status:** Design Phase
**Date:** 2026-02-12
**Author:** Claude (via @jeffrey)

---

## Executive Summary

To enable users to authenticate their MCP publishing with aesthetic.computer accounts, we need a long-lived API token system. The current Auth0 Bearer tokens expire after 24 hours, making them impractical for MCP client integration.

**Recommended Solution:** Implement user-managed API tokens with web UI for generation/revocation.

---

## Current State Analysis

### Authentication Flow (as of 2026.02.12)

```
User Login (Web)
    ↓
  Auth0 OAuth2
    ↓
Access Token (24hr expiry)
    ↓
Bearer Token in Authorization header
    ↓
validate via /userinfo endpoint
```

### Existing Code Components

1. **`system/backend/authorization.mjs`**
   - `authorize()` function validates Bearer tokens via Auth0
   - Calls `https://aesthetic.us.auth0.com/userinfo`
   - Returns user object with `sub` (user ID) and email

2. **`system/netlify/functions/auth-cli-callback.mjs`**
   - Handles OAuth callback for CLI tools
   - Returns `access_token` to authenticated clients
   - Used for temporary CLI authentication

3. **Publishing Endpoints**
   - `store-piece.mjs`, `store-kidlisp.mjs`, `store-clock.mjs`
   - All support optional Bearer token authentication
   - Anonymous publishing works without token

### Current Limitations

| Issue | Impact | Priority |
|-------|--------|----------|
| Short-lived tokens (24hr) | Users must re-authenticate daily | 🔴 High |
| No token management UI | Users can't generate/revoke tokens | 🔴 High |
| No token visibility | Users don't know where to get tokens | 🔴 High |
| Security: Can't revoke individual tokens | Compromised token affects all sessions | 🟡 Medium |

---

## Problem Statement

**Goal:** Enable users to obtain long-lived API tokens for MCP client authentication.

**Requirements:**
1. Tokens must be long-lived (30-365 days or indefinite)
2. Users must be able to self-service generate tokens
3. Users must be able to revoke tokens independently
4. Tokens must be secure (not guessable, properly scoped)
5. System must integrate with existing `authorize()` function
6. Backward compatible with existing Auth0 token validation

**Non-Goals:**
- Replace Auth0 for web authentication
- Implement OAuth2 server
- Support token refresh flows

---

## Proposed Solution: User-Managed API Tokens

### Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│  User Flow                                          │
├─────────────────────────────────────────────────────┤
│                                                     │
│  1. User logs in to aesthetic.computer (Auth0)     │
│  2. Visits /settings/api-tokens page                │
│  3. Clicks "Generate New Token"                     │
│  4. Names token (e.g., "Claude Desktop")            │
│  5. Token displayed ONCE (must copy)                │
│  6. User adds token to MCP client config            │
│  7. MCP client sends: Authorization: Bearer ac_xxx  │
│  8. Server validates token → associates with user   │
│                                                     │
└─────────────────────────────────────────────────────┘
```

### Token Format

```
ac_live_<32 random alphanumeric chars>

Examples:
- ac_live_8k3jf9d2l4m6n8p0q2r4s6t8v0w2x4y6
- ac_live_a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6
```

**Rationale:**
- `ac_` prefix identifies as aesthetic.computer token
- `live_` indicates production environment (future: `test_` for dev)
- 32 chars = ~191 bits entropy (cryptographically secure)
- Alphanumeric only (no special chars for easy copy/paste)

### Database Schema

**Collection:** `api_tokens`

```javascript
{
  _id: "ac_live_8k3jf9d2l4m6n8p0q2r4s6t8v0w2x4y6", // The token itself
  user: "auth0|123456789",                          // User ID (sub)
  name: "Claude Desktop",                           // User-provided name
  created: ISODate("2026-02-12T10:30:00Z"),
  lastUsed: ISODate("2026-02-12T15:45:00Z"),        // Updated on each use
  scopes: ["publish"],                              // Future: ["publish", "read", "admin"]
  revoked: false,                                   // Soft delete
  revokedAt: null,                                  // When revoked (if applicable)
  metadata: {                                       // Optional tracking
    ip: "192.168.1.1",
    userAgent: "Claude Desktop/1.0",
  }
}
```

**Indexes:**
```javascript
// Primary lookup (most frequent query)
{ _id: 1 }  // Automatic

// User lookup (for token list page)
{ user: 1, revoked: 1 }

// Cleanup queries
{ revoked: 1, revokedAt: 1 }
{ lastUsed: 1 }
```

---

## API Endpoints

### 1. Generate Token

**Endpoint:** `POST /api/tokens/generate`

**Authentication:** Required (Auth0 session)

**Request:**
```json
{
  "name": "Claude Desktop"
}
```

**Response:**
```json
{
  "success": true,
  "token": "ac_live_8k3jf9d2l4m6n8p0q2r4s6t8v0w2x4y6",
  "name": "Claude Desktop",
  "created": "2026-02-12T10:30:00Z",
  "warning": "This token will only be shown once. Copy it now!"
}
```

**Error Cases:**
- 401: Not authenticated
- 429: Rate limit (max 10 tokens per user)

---

### 2. List Tokens

**Endpoint:** `GET /api/tokens/list`

**Authentication:** Required (Auth0 session)

**Response:**
```json
{
  "tokens": [
    {
      "id": "ac_live_8k3j...",
      "name": "Claude Desktop",
      "created": "2026-02-12T10:30:00Z",
      "lastUsed": "2026-02-12T15:45:00Z",
      "preview": "ac_live_8k3j...x4y6" // First 12 + last 4 chars
    },
    {
      "id": "ac_live_a1b2...",
      "name": "ChatGPT",
      "created": "2026-02-10T08:00:00Z",
      "lastUsed": "2026-02-12T12:00:00Z",
      "preview": "ac_live_a1b2...o5p6"
    }
  ]
}
```

---

### 3. Revoke Token

**Endpoint:** `DELETE /api/tokens/revoke/:tokenId`

**Authentication:** Required (Auth0 session, must own token)

**Response:**
```json
{
  "success": true,
  "message": "Token 'Claude Desktop' has been revoked"
}
```

**Error Cases:**
- 401: Not authenticated
- 403: Token belongs to different user
- 404: Token not found

---

## Code Changes

### 1. Update `authorization.mjs`

**Current code:**
```javascript
export async function authorize({ authorization }, tenant = "aesthetic") {
  try {
    const { got } = await import("got");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;
    shell.log(`🔐 Attempting to authorize \`${tenant}\` user...`);
    const result = (
      await got(`${baseURI}/userinfo`, {
        headers: { Authorization: authorization },
        responseType: "json",
      })
    ).body;
    // ...
  }
}
```

**New code:**
```javascript
export async function authorize({ authorization }, tenant = "aesthetic") {
  const token = authorization?.replace("Bearer ", "");

  // 🆕 Check if it's an API token (starts with "ac_")
  if (token?.startsWith("ac_live_") || token?.startsWith("ac_test_")) {
    return await validateApiToken(token);
  }

  // Otherwise, validate as Auth0 token (existing logic)
  try {
    const { got } = await import("got");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;
    shell.log(`🔐 Attempting to authorize \`${tenant}\` user...`);
    const result = (
      await got(`${baseURI}/userinfo`, {
        headers: { Authorization: authorization },
        responseType: "json",
      })
    ).body;
    // ...
  }
}

// 🆕 New function
async function validateApiToken(token) {
  const database = await connect();
  const collection = database.db.collection("api_tokens");

  const tokenDoc = await collection.findOne({
    _id: token,
    revoked: false
  });

  if (!tokenDoc) {
    await database.disconnect();
    return undefined;
  }

  // Update lastUsed timestamp (fire and forget)
  collection.updateOne(
    { _id: token },
    { $set: { lastUsed: new Date() } }
  ).catch(err => shell.error("Failed to update token lastUsed:", err));

  await database.disconnect();

  // Return user object in same format as Auth0
  return {
    sub: tokenDoc.user,
    email_verified: true, // Assume verified (token was generated by logged-in user)
    source: "api_token",
    token_name: tokenDoc.name,
  };
}
```

---

### 2. Create New Netlify Functions

**Files to create:**
- `system/netlify/functions/api-token-generate.mjs`
- `system/netlify/functions/api-token-list.mjs`
- `system/netlify/functions/api-token-revoke.mjs`

**Add to `netlify.toml`:**
```toml
[[redirects]]
from = "/api/tokens/*"
to = "/.netlify/functions/api-token-:splat"
status = 200
```

---

### 3. Create Web UI

**New piece:** `@api-tokens` (or add to existing settings)

**Features:**
- List existing tokens with preview, creation date, last used
- "Generate New Token" button
- Modal to name token
- One-time token display with copy button
- Revoke button for each token
- Empty state for no tokens

**Example UI (text-based for piece):**

```
╔══════════════════════════════════════════════════╗
║  API Tokens for MCP Clients                     ║
╠══════════════════════════════════════════════════╣
║                                                  ║
║  Claude Desktop                                  ║
║  Token: ac_live_8k3j...x4y6                     ║
║  Created: Feb 12, 2026                          ║
║  Last used: 2 hours ago                         ║
║  [Revoke]                                       ║
║                                                  ║
║  ──────────────────────────────────────────────  ║
║                                                  ║
║  ChatGPT                                        ║
║  Token: ac_live_a1b2...o5p6                     ║
║  Created: Feb 10, 2026                          ║
║  Last used: 5 minutes ago                       ║
║  [Revoke]                                       ║
║                                                  ║
║  ──────────────────────────────────────────────  ║
║                                                  ║
║  [+ Generate New Token]                         ║
║                                                  ║
╚══════════════════════════════════════════════════╝
```

---

## Security Considerations

### Token Generation
- Use `crypto.randomBytes(32)` for secure random generation
- Hash tokens before comparing? **No** - tokens are stored as-is (like API keys)
- Tokens are secrets - never log full tokens

### Token Storage
- Store tokens as document IDs in MongoDB (no hashing needed)
- Index on `_id` for O(1) lookup
- Add TTL index for automatic cleanup of old revoked tokens

### Rate Limiting
- Max 10 active tokens per user
- Rate limit token generation: 5 requests/hour per user
- Rate limit API calls: 1000 requests/hour per token

### Token Revocation
- Soft delete (set `revoked: true`)
- Allow user to view revoked tokens for audit log
- Cleanup old revoked tokens after 90 days (TTL index)

### Scope Management
- All tokens start with `["publish"]` scope
- Future: Add granular scopes like `["read", "publish:pieces", "publish:kidlisp"]`
- Validate scopes in each endpoint

---

## User Experience Flow

### Happy Path

```
1. User visits aesthetic.computer
2. Clicks profile → "API Tokens" or "Settings"
3. Sees empty state: "No API tokens yet"
4. Clicks "Generate New Token"
5. Modal appears: "Name this token (e.g., Claude Desktop)"
6. User enters "Claude Desktop" and clicks "Generate"
7. Success modal shows token ONE TIME:

   ┌─────────────────────────────────────────────┐
   │  ✅ Token Generated                         │
   ├─────────────────────────────────────────────┤
   │                                             │
   │  Token: ac_live_8k3jf9d2l4m6n8p0q2r4s6...  │
   │         [Copy to Clipboard]                 │
   │                                             │
   │  ⚠️  This token will only be shown once.    │
   │     Copy it now and store it securely!      │
   │                                             │
   │  Add to your MCP client:                    │
   │                                             │
   │  {                                          │
   │    "mcpServers": {                          │
   │      "aesthetic-computer": {                │
   │        "command": "npx",                    │
   │        "args": ["-y", "@aesthetic.compu...  │
   │        "env": {                             │
   │          "AC_TOKEN": "ac_live_8k3jf9d2..."  │
   │        }                                    │
   │      }                                      │
   │    }                                        │
   │  }                                          │
   │                                             │
   │  [I've Saved It] [Download Config]          │
   └─────────────────────────────────────────────┘

8. User copies token
9. Token appears in list (masked)
10. User adds to MCP client config
11. Publishing now associates with user account
```

### Error Cases

**Token Limit Reached:**
```
❌ Token limit reached (10/10)
   You must revoke an existing token before creating a new one.
```

**Unauthorized Revoke Attempt:**
```
❌ Permission denied
   This token belongs to a different user.
```

**Token Already Revoked:**
```
⚠️  Token already revoked
   This token was revoked on Feb 10, 2026.
```

---

## Implementation Checklist

### Phase 1: Backend (Estimated: 4-6 hours)
- [ ] Update `authorization.mjs` with API token validation
- [ ] Create token generation utility (crypto randomness)
- [ ] Create `api-token-generate.mjs` Netlify function
- [ ] Create `api-token-list.mjs` Netlify function
- [ ] Create `api-token-revoke.mjs` Netlify function
- [ ] Add MongoDB indexes to `api_tokens` collection
- [ ] Add rate limiting middleware
- [ ] Update `netlify.toml` with new routes
- [ ] Write unit tests for token validation

### Phase 2: Frontend (Estimated: 6-8 hours)
- [ ] Create `@api-tokens` piece or integrate into settings
- [ ] Implement token list UI
- [ ] Implement token generation modal
- [ ] Implement one-time token display with copy button
- [ ] Implement token revocation with confirmation
- [ ] Add empty state UI
- [ ] Add loading states and error handling
- [ ] Add usage instructions and documentation links

### Phase 3: Documentation (Estimated: 2 hours)
- [ ] Update MCP README with token generation instructions
- [ ] Update website docs with token management guide
- [ ] Add troubleshooting section
- [ ] Create video walkthrough (optional)

### Phase 4: Testing & Launch (Estimated: 2-3 hours)
- [ ] Test token generation flow
- [ ] Test token validation in MCP publishing
- [ ] Test token revocation
- [ ] Test rate limiting
- [ ] Test concurrent token usage
- [ ] Deploy to production
- [ ] Monitor logs for errors
- [ ] Announce feature to users

**Total Estimated Time:** 14-19 hours

---

## Alternative Approaches Considered

### Option A: Extend Auth0 Token Expiry
**Pros:**
- No new infrastructure
- Reuses existing auth flow

**Cons:**
- Auth0 token limits (max ~30 days)
- Can't revoke individual tokens
- More expensive (Auth0 pricing)
- Less user control

**Verdict:** ❌ Not recommended

---

### Option B: Simple Token Page (Auth0 tokens)
**Pros:**
- Very quick to implement (1-2 hours)
- No database changes needed

**Cons:**
- Tokens still expire after 24 hours
- Users must re-authenticate frequently
- Poor UX for MCP clients

**Verdict:** ⚠️ Good for MVP, but not long-term solution

**Implementation:**
```javascript
// GET /api/my-token
export async function handler(event) {
  const user = await authorize(event.headers);
  if (!user) return respond(401, { error: "Unauthorized" });

  // Return the Auth0 token that was just validated
  const token = event.headers.authorization?.replace("Bearer ", "");
  return respond(200, { token, expires: "24 hours" });
}
```

---

### Option C: OAuth2 Device Flow
**Pros:**
- Industry standard
- Good for CLI tools
- Handles refresh tokens

**Cons:**
- Complex implementation
- Overkill for simple use case
- Still requires user interaction

**Verdict:** ❌ Over-engineered

---

## Migration Strategy

### Backward Compatibility

The proposed solution is **100% backward compatible**:

1. Existing Auth0 tokens continue to work
2. No changes to existing API contracts
3. New token format is distinct (`ac_` prefix)
4. Anonymous publishing still works without any token

### Rollout Plan

**Week 1: Soft Launch**
- Deploy backend changes
- Create token management UI
- Announce to beta testers only
- Monitor for issues

**Week 2: Documentation**
- Update all MCP documentation
- Create video tutorials
- Add in-app help tooltips

**Week 3: Public Launch**
- Announce via social media
- Post in Discord/community channels
- Update registry metadata if needed

**Week 4: Monitoring**
- Track token generation rate
- Monitor API performance impact
- Gather user feedback
- Iterate on UX

---

## Success Metrics

### Key Performance Indicators (KPIs)

| Metric | Target | How to Measure |
|--------|--------|---------------|
| Token generation rate | 100+ tokens/week | MongoDB query count |
| Token usage rate | 80%+ tokens used within 7 days | Check `lastUsed` field |
| Revocation rate | <5% tokens revoked within first month | Track revocations |
| Support tickets | <10 token-related tickets/month | Support system |
| MCP publishing auth rate | 30%+ of publishes authenticated | Compare anon vs auth |

### Success Criteria

- [ ] Users can generate tokens without support help
- [ ] Token validation adds <50ms latency to API calls
- [ ] Zero security incidents related to tokens
- [ ] Positive user feedback on token management UX
- [ ] Increased rate of authenticated (non-anonymous) publishing

---

## Open Questions

1. **Token expiry:** Should tokens expire after inactivity (e.g., 1 year unused)?
   - **Recommendation:** Yes, expire after 1 year of inactivity. Send email warning at 11 months.

2. **Token naming:** Should we enforce unique token names per user?
   - **Recommendation:** No, allow duplicates. Users might want "Claude Desktop" on multiple machines.

3. **Token export:** Should users be able to export token list (without secrets)?
   - **Recommendation:** Yes, add "Export to CSV" for audit logs.

4. **Token transfer:** Should tokens be transferable between accounts?
   - **Recommendation:** No, security risk. Users must generate new tokens.

5. **Notification:** Should users get notified when their token is used from new IP?
   - **Recommendation:** Phase 2 feature. Not critical for MVP.

---

## Appendix: Example Code Snippets

### Token Generation (Cryptographic)

```javascript
import crypto from 'crypto';

export function generateApiToken() {
  const randomBytes = crypto.randomBytes(32);
  const base62 = randomBytes.toString('base64')
    .replace(/\+/g, '')
    .replace(/\//g, '')
    .replace(/=/g, '')
    .slice(0, 32);

  return `ac_live_${base62}`;
}

// Example output: ac_live_8k3jf9d2l4m6n8p0q2r4s6t8v0w2x4y6
```

### Token Validation (Fast Path)

```javascript
export async function validateApiToken(token) {
  // Early return for invalid format
  if (!token || !token.startsWith('ac_')) {
    return undefined;
  }

  const database = await connect();
  const collection = database.db.collection('api_tokens');

  // Single query with projection (only fetch needed fields)
  const tokenDoc = await collection.findOne(
    { _id: token, revoked: false },
    { projection: { user: 1, name: 1, scopes: 1 } }
  );

  if (!tokenDoc) {
    await database.disconnect();
    return undefined;
  }

  // Fire-and-forget update (don't await)
  collection.updateOne(
    { _id: token },
    { $set: { lastUsed: new Date() } }
  ).catch(() => {}); // Silently fail

  await database.disconnect();

  return {
    sub: tokenDoc.user,
    email_verified: true,
    source: 'api_token',
    token_name: tokenDoc.name,
    scopes: tokenDoc.scopes || ['publish'],
  };
}
```

### Rate Limiting Middleware

```javascript
import * as KeyValue from "./kv.mjs";

export async function checkRateLimit(userId, action, limit, window) {
  const key = `ratelimit:${action}:${userId}`;
  await KeyValue.connect();

  const count = await KeyValue.get(key) || 0;

  if (count >= limit) {
    await KeyValue.disconnect();
    return { allowed: false, remaining: 0 };
  }

  // Increment counter
  await KeyValue.incr(key);
  await KeyValue.expire(key, window); // TTL in seconds

  await KeyValue.disconnect();

  return { allowed: true, remaining: limit - count - 1 };
}

// Usage:
const rateLimit = await checkRateLimit(user.sub, 'token_generate', 5, 3600); // 5 per hour
if (!rateLimit.allowed) {
  return respond(429, { error: "Rate limit exceeded. Try again later." });
}
```

---

## Conclusion

The proposed API token system provides a secure, user-friendly way for users to authenticate their MCP publishing. The implementation is straightforward, backward compatible, and follows industry best practices.

**Recommended Next Steps:**
1. Review and approve this design document
2. Create implementation tickets
3. Begin Phase 1 (Backend) development
4. Iterate based on beta tester feedback

**Estimated Launch Date:** 2-3 weeks from approval

---

**Questions or Feedback?**
Contact: @jeffrey on aesthetic.computer
GitHub Issues: https://github.com/whistlegraph/aesthetic-computer/issues
