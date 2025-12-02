# Netlify Function Plan: `bundle-keep-html.mjs` → API Endpoint

## Overview

Convert the existing `tezos/bundle-keep-html.mjs` CLI tool into a Netlify serverless function that can generate KidLisp bundles on-demand via HTTP requests.

**Current CLI Usage:**
```bash
node tezos/bundle-keep-html.mjs 39j
```

**Proposed API Usage:**
```
GET https://aesthetic.computer/api/bundle-html?code=39j
# Returns: .lisp.html file as download or base64 response
```

---

## Technical Challenges & Solutions

### 1. **File System Access**
| Challenge | Current CLI | Netlify Function Solution |
|-----------|-------------|---------------------------|
| Reading source files | `fs.readFile()` from disk | Use `included_files` in `netlify.toml` to bundle assets |
| Writing output | `fs.writeFile()` to `keep-bundles/` | Return directly in HTTP response (no file write needed) |
| Git version info | `execSync('git rev-parse')` | Use build-time env var or hardcode at deploy time |

### 2. **Dependencies**
| Dependency | Purpose | Netlify Compatible? |
|------------|---------|---------------------|
| `@swc/wasm` | JS minification | ✅ WASM version - drop-in replacement for `@swc/core` |
| `zlib.gzipSync` | Compression | ✅ Built into Node.js |
| `fetch` | API calls | ✅ Built into Node 18+ |

> **Note:** `@swc/wasm` is the WebAssembly version of SWC with the exact same API as `@swc/core`.
> Simply change the import from `@swc/core` to `@swc/wasm` - no other code changes needed.
> - 1.2M+ weekly downloads on npm
> - Same `transform()` function signature
> - No native binaries, works in serverless environments

**Required Package Change:**
```bash
# Add to system/package.json
cd system && npm install @swc/wasm
```

### 3. **Execution Time**
- **Current bundle time:** ~5-15 seconds (depending on piece complexity)
- **Netlify timeout:** 10 seconds (default), 26 seconds (background functions)
- **Solution:** Use background function or optimize for speed

---

## Implementation Plan

### Phase 1: Create the Function Structure ✅ IMPLEMENTED

```
system/netlify/functions/bundle-html.mjs     # Main function (all-in-one)
```

### Phase 2: Configure `netlify.toml` ✅ IMPLEMENTED

```toml
[functions.bundle-html]
# Include all aesthetic.computer source files needed for VFS
included_files = [
  "public/aesthetic.computer/**/*.mjs",
  "public/aesthetic.computer/**/*.js",
  "public/aesthetic.computer/disks/drawings/font_1/**/*.json",
  "public/aesthetic.computer/dep/**/*.mjs",
  "backend/**/*.mjs"
]

# @swc/wasm is the WASM version of SWC - same API as @swc/core
# It needs to be marked as external so Netlify includes the .wasm files
external_node_modules = ["@swc/wasm"]

# Environment variables
included_env_vars = ["GIT_COMMIT", "CONTEXT"]

# Extended timeout for bundle generation (may take 5-15s)
# Note: Background functions get 26s timeout if needed
```

**Key Configuration Notes:**
- `@swc/wasm` must be in `external_node_modules` so Netlify properly bundles the WASM binary
- The WASM file is ~15MB but loads fast at runtime
- No native compilation required - works on any Node.js runtime

### Phase 3: API Design

#### Request
```
GET /api/bundle-html?code=<piece-code>

Query Parameters:
  code     (required)  KidLisp piece code without $ (e.g., "39j")
  format   (optional)  "html" (default) | "base64" | "json"
```

#### Response Formats

**HTML (default)** - Direct download:
```
Content-Type: text/html
Content-Disposition: attachment; filename="$39j-@fifi-2025.11.27.12.30.00.000.lisp.html"

<!DOCTYPE html>...
```

**Base64** - For programmatic use:
```json
{
  "filename": "$39j-@fifi-2025.11.27.12.30.00.000.lisp.html",
  "content": "PCFET0NUWVBFIGh0bWw+...",
  "sizeKB": 180,
  "author": "@fifi"
}
```

### Phase 4: Code Migration

#### Key Changes from CLI to Function

| CLI Code | Function Equivalent |
|----------|---------------------|
| `process.argv[2]` | `event.queryStringParameters.code` |
| `fs.readFile(fullPath)` | `fs.readFile(path.join(process.cwd(), 'public/aesthetic.computer', relativePath))` |
| `fs.writeFile(outputPath, content)` | `return { body: content, headers: {...} }` |
| `execSync('git rev-parse')` | `process.env.GIT_COMMIT` or hardcoded |
| `console.log(...)` | Kept for CloudWatch logs |

#### Function Skeleton

```javascript
// system/netlify/functions/bundle-keep.mjs

import { promises as fs } from "fs";
import path from "path";
import { gzipSync } from "zlib";
import { transform } from "@swc/wasm";  // Same API as @swc/core!
import { respond } from "../../backend/http.mjs";

// Essential files list (same as CLI)
const ESSENTIAL_FILES = [
  'boot.mjs', 'bios.mjs', 'lib/loop.mjs', 'lib/disk.mjs',
  // ... rest of essential files
];

export async function handler(event) {
  const code = event.queryStringParameters?.code;
  
  if (!code) {
    return respond(400, { error: "Missing 'code' parameter" });
  }

  try {
    // 1. Fetch KidLisp source from API
    const kidlispSources = await getKidLispSourceWithDeps(code);
    
    // 2. Build VFS from included files
    const vfs = await buildVFS(kidlispSources);
    
    // 3. Generate HTML bundle
    const html = generateBundle(code, kidlispSources, vfs);
    
    // 4. Compress with gzip
    const compressed = createGzipBundle(html);
    
    // 5. Return as download
    const filename = generateFilename(code);
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html",
        "Content-Disposition": `attachment; filename="${filename}"`,
      },
      body: compressed,
    };
    
  } catch (error) {
    return respond(500, { error: error.message });
  }
}
```

---

## File Changes Required

### New Files
1. `system/netlify/functions/bundle-keep.mjs` - Main function
2. `system/netlify/functions/bundle-keep/helpers.mjs` - Shared utilities

### Modified Files
1. `system/netlify.toml` - Add function configuration

### Optional Refactoring
- Extract shared code from `tezos/bundle-keep-html.mjs` into a shared module that both CLI and Netlify function can use

---

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| ~~SWC native binary incompatible~~ | ~~High~~ | ~~Blocker~~ | ✅ **SOLVED:** Use `@swc/wasm` (WASM version) |
| Timeout on large pieces | Medium | Degraded UX | Use background function or caching |
| VFS files missing at runtime | Medium | Broken bundles | Extensive `included_files` config |
| Memory limits (~1GB) | Low | Failure | Already well under limit |
| WASM cold start | Low | Slower first request | ~100-200ms overhead, acceptable |

---

## Testing Strategy

1. **Local Testing:**
   ```bash
   netlify dev
   curl "http://localhost:8888/api/bundle-keep?code=39j" > test.html
   open test.html
   ```

2. **Deploy Preview:**
   - Push to branch, test on deploy preview URL
   - Verify paintings load, fonts work, piece runs

3. **Production Validation:**
   - Compare output to CLI-generated bundle
   - Verify byte-for-byte similarity (minus timestamps)

---

## Estimated Effort

| Phase | Time Estimate |
|-------|---------------|
| Phase 1: Function structure | 2-3 hours |
| Phase 2: netlify.toml config | 30 min |
| Phase 3: API implementation | 2-3 hours |
| Phase 4: Testing & debugging | 2-4 hours |
| **Total** | **7-11 hours** |

---

## Future Enhancements

1. **Caching:** Store generated bundles in R2/S3 with TTL
2. **Webhooks:** Trigger bundle regeneration on piece update
3. **Batch Generation:** Generate multiple pieces in one request
4. **IPFS Upload:** Optionally pin bundle to IPFS and return CID

---

## Decision Points

1. **~~Should we use `esbuild` or try to make `@swc/core` work?~~**
   - ✅ **RESOLVED:** Use `@swc/wasm` - it's the WASM version of SWC with identical API
   - No code changes needed beyond `import { transform } from "@swc/wasm"`
   - Keeps the exact same minification behavior as the CLI tool

2. **Background function (26s timeout) or standard (10s)?**
   - Recommendation: Start with standard, switch to background if needed

3. **Should CLI and function share code?**
   - Recommendation: Yes, create `tezos/bundle-keep-shared.mjs` module

4. **Where to store generated bundles?**
   - Option A: Don't store, generate on every request
   - Option B: Cache in S3/R2 with piece code + git hash as key
   - Recommendation: Option A first, add caching if needed
