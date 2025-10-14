# Migration Summary - Netlify to Cloudflare Grab Worker

## Current Implementation (Netlify)

### File: `/system/netlify/functions/screenshot.js`

**Technology Stack:**
- Puppeteer Core (`puppeteer-core`)
- Chrome binary (`@sparticuz/chromium`)
- Node.js 18+
- Netlify Functions (serverless)

**Endpoints:**
```
/icon/{resolution}/{piece-path}.png
/preview/{resolution}/{piece-path}.png
```

**Supported Resolutions:**
- `128x128` - Favicon
- `1200x630` - Open Graph (Facebook, LinkedIn)
- `1800x900` - Twitter Card

**Caching:**
- Development: File-based cache (60s TTL)
- Production: ETag-based HTTP caching
- CDN: Netlify CDN with `Netlify-CDN-Cache-Control` header

**Process:**
1. Parse request path
2. Check dev file cache (if dev mode)
3. Generate ETag from resolution + path
4. Check client ETag (304 if match)
5. Launch Chromium with Puppeteer
6. Navigate to `https://aesthetic.computer/{path}?icon` or `?preview`
7. Wait for load + 2s delay
8. Take screenshot
9. Return PNG with cache headers

---

## Current Usage in Code

### `/system/netlify/functions/index.mjs`

**Lines 360-391:**

```javascript
// Current icon generation
const icon = pixelPerfect(
  baseURL,
  parsed?.host,
  `/icon/128x128/${location}.png`,
  meta,
  "https:",
);

// Current og:image generation  
const ogImage = pixelPerfect(
  baseURL,
  parsed?.host,
  `/preview/1200x630/${location}.png`,
  meta,
  "https:",
);

// Current twitter:image generation
const twitterImage = pixelPerfect(
  baseURL,
  parsed?.host,
  `/preview/1800x900/${location}.png`,
  meta,
  "https:",
);

// Used in HTML meta tags (lines 375-391)
${!previewOrIcon
  ? html`<link rel="icon" href="${icon}" type="image/png" />`
  : ""}
${!previewOrIcon
  ? html`<link rel="apple-touch-icon" href="${icon}" />`
  : ""}
${!previewOrIcon
  ? html`<meta name="og:image" content="${ogImage}" />`
  : ""}
<meta name="twitter:image" content="${twitterImage}" />
```

**Note:** `pixelPerfect()` is a utility that constructs the full URL based on environment.

---

## New Implementation (Cloudflare)

### File: `/grab/src/worker.ts` (new)

**Technology Stack:**
- Cloudflare Browser Rendering API
- Puppeteer-compatible interface (`@cloudflare/puppeteer`)
- Durable Objects for coordination
- R2 Storage for long-term caching
- TypeScript

**Same Endpoints:**
```
/icon/{resolution}/{piece-path}.png
/preview/{resolution}/{piece-path}.png
```

**Same Resolutions:**
- `128x128` - Favicon
- `1200x630` - Open Graph
- `1800x900` - Twitter Card

**Improved Caching:**
- Edge caching via Cache API
- ETag-based HTTP caching (same as before)
- R2 long-term storage (optional)
- Durable Objects for coordination

**Improved Process:**
1. Parse request path (same)
2. Generate ETag from resolution + path (same)
3. Check Cache API (fast edge cache)
4. Get Durable Object stub for URL
5. Request screenshot from DO
6. DO launches Browser Rendering session
7. Navigate to URL with query param
8. Take screenshot
9. Store in Cache API + R2
10. Return PNG with cache headers

**Benefits:**
- ✅ Global edge caching
- ✅ No cold starts (Browser Rendering is faster)
- ✅ Better concurrency control via DOs
- ✅ Lower cost (no full Chrome binary)
- ✅ Better monitoring via Cloudflare Dashboard

---

## Migration Changes

### 1. Update HTML Generator

**File:** `/system/netlify/functions/index.mjs`

**Change lines 360-380:**

```diff
- const icon = pixelPerfect(
-   baseURL,
-   parsed?.host,
-   `/icon/128x128/${location}.png`,
-   meta,
-   "https:",
- );
+ const icon = `https://grab.aesthetic.computer/icon/128x128/${location}.png`;

- const ogImage = pixelPerfect(
-   baseURL,
-   parsed?.host,
-   `/preview/1200x630/${location}.png`,
-   meta,
-   "https:",
- );
+ const ogImage = `https://grab.aesthetic.computer/preview/1200x630/${location}.png`;

- const twitterImage = pixelPerfect(
-   baseURL,
-   parsed?.host,
-   `/preview/1800x900/${location}.png`,
-   meta,
-   "https:",
- );
+ const twitterImage = `https://grab.aesthetic.computer/preview/1800x900/${location}.png`;
```

**Result:** All screenshot requests will go directly to grab worker.

---

### 2. Add Redirects (Optional)

**File:** `/system/netlify.toml`

**Add to redirects section:**

```toml
# Redirect legacy screenshot endpoints to grab worker
[[redirects]]
  from = "/icon/*"
  to = "https://grab.aesthetic.computer/icon/:splat"
  status = 200
  force = true

[[redirects]]
  from = "/preview/*"
  to = "https://grab.aesthetic.computer/preview/:splat"
  status = 200
  force = true
```

**Purpose:** 
- Maintain backwards compatibility
- Allow testing via aesthetic.computer URLs
- Seamless migration

---

### 3. Remove Old Function (After Validation)

**Files to remove:**
```
/system/netlify/functions/screenshot.js
```

**Dependencies to remove from `/system/package.json`:**
```json
{
  "dependencies": {
    "puppeteer-core": "^21.x.x",  // Remove
    "@sparticuz/chromium": "^117.x.x"  // Remove
  }
}
```

**Directories to clean:**
```
/workspaces/aesthetic-computer/.netlify/cache/screenshots/
```

---

## API Comparison

### Request Examples

**Before (Netlify):**
```bash
# Via main domain
curl https://aesthetic.computer/icon/128x128/prompt~wipe.png

# Direct to function
curl https://aesthetic.computer/.netlify/functions/screenshot/icon/128x128/prompt~wipe.png
```

**After (Grab Worker):**
```bash
# Via grab subdomain
curl https://grab.aesthetic.computer/icon/128x128/prompt~wipe.png

# Via redirect (if configured)
curl https://aesthetic.computer/icon/128x128/prompt~wipe.png
# -> redirects to grab.aesthetic.computer
```

---

### Response Headers

**Before (Netlify):**
```http
Content-Type: image/png
Content-Length: 12345
ETag: "abc123def456"
Cache-Control: public, max-age=60
Netlify-CDN-Cache-Control: public, durable, max-age=1800
X-Dev-Cache-Hit: true  (dev only)
```

**After (Grab Worker):**
```http
Content-Type: image/png
Content-Length: 12345
ETag: "abc123def456"
Cache-Control: public, max-age=3600
Cloudflare-CDN-Cache-Control: public, durable, max-age=86400
CF-Cache-Status: HIT | MISS
X-Screenshot-Time: 2345
```

---

### Caching Behavior

**Before:**
- Dev: 60s file cache
- Production: 30m CDN cache (1800s)
- ETag revalidation

**After:**
- Dev: Same behavior
- Production: 24h CDN cache (86400s)
- ETag revalidation (same)
- Additional R2 long-term cache

---

## Performance Comparison

### Expected Improvements

| Metric | Netlify | Cloudflare | Improvement |
|--------|---------|------------|-------------|
| Cold Start | 3-5s | <1s | 3-4x faster |
| Warm Request (cache miss) | 2-3s | 1-2s | 1.5x faster |
| Cached Request | <100ms | <50ms | 2x faster |
| Global Availability | Regional | Global | Better |
| Concurrent Sessions | Limited | Higher | Better |

### Cost Comparison

**Netlify:**
- 125k function invocations/month (free tier)
- ~500ms execution time per screenshot
- Background function time counts toward limit

**Cloudflare:**
- 100k worker requests/day (free tier)
- Browser Rendering: $0.05/second
- Durable Objects: $0.15/million requests
- Estimated: $5-10/month for typical usage

---

## Testing Plan

### Phase 1: Parallel Testing

1. Deploy grab worker to `grab.aesthetic.computer`
2. Keep Netlify function running
3. Test both endpoints side-by-side
4. Compare screenshots visually
5. Compare response times
6. Monitor error rates

### Phase 2: Canary Rollout

1. Update 10% of traffic to use grab worker
2. Monitor for 24 hours
3. Check metrics and errors
4. Increase to 50% if successful
5. Increase to 100% if successful

### Phase 3: Full Migration

1. Update all references to grab worker
2. Add redirects for backwards compatibility
3. Monitor for 1 week
4. Remove Netlify function
5. Clean up dependencies

---

## Rollback Plan

### Quick Rollback

**If grab worker has issues:**

1. Revert commit to index.mjs
2. Remove redirects from netlify.toml
3. Traffic returns to Netlify function

**Time:** <5 minutes

### Partial Rollback

**If specific resolution has issues:**

```javascript
// Emergency feature flag
const useGrabWorkerForIcon = true;
const useGrabWorkerForPreview = true;

const icon = useGrabWorkerForIcon
  ? `https://grab.aesthetic.computer/icon/128x128/${location}.png`
  : pixelPerfect(baseURL, parsed?.host, `/icon/128x128/${location}.png`, meta, "https:");
```

---

## Monitoring

### Metrics to Track

**Before Migration (Baseline):**
- Netlify function invocations
- Average execution time
- Error rate
- Cache hit rate

**After Migration:**
- Grab worker requests
- Browser rendering time
- Durable Object invocations
- Cache hit rate
- Cost per 1000 requests

### Success Criteria

✅ Error rate < 1%  
✅ P95 latency < 5s  
✅ Cache hit rate > 80%  
✅ Cost reasonable (<$20/month)  
✅ No degradation in screenshot quality  
✅ Successful migration with no incidents

---

## Timeline

### Week 1: Implementation
- [ ] Set up grab worker structure
- [ ] Implement TypeScript code
- [ ] Write tests
- [ ] Local testing

### Week 2: Deployment
- [ ] Create Cloudflare resources
- [ ] Deploy to production
- [ ] Configure custom domain
- [ ] Parallel testing

### Week 3: Migration
- [ ] Update index.mjs
- [ ] Add redirects
- [ ] Monitor metrics
- [ ] Gradual rollout

### Week 4: Cleanup
- [ ] Verify stability
- [ ] Remove Netlify function
- [ ] Clean up dependencies
- [ ] Update documentation

---

## Questions & Answers

**Q: Why not keep using Netlify?**  
A: Cloudflare offers better performance, global edge caching, and native browser rendering without cold starts.

**Q: What about existing cached screenshots?**  
A: They'll expire naturally based on TTL. New requests will generate fresh screenshots via grab worker.

**Q: Can we run both simultaneously?**  
A: Yes! During migration, both can run in parallel for testing and gradual rollout.

**Q: What if Cloudflare has an outage?**  
A: We can quickly rollback to Netlify function. No data loss as screenshots are generated on-demand.

**Q: How do we handle the query params (?icon, ?preview)?**  
A: Same as before - grab worker will pass these through when navigating to aesthetic.computer URLs.

**Q: What about dev environment?**  
A: Grab worker has dev mode via `wrangler dev` that works the same as Netlify dev.

---

## Summary

**Current State:**
- Netlify Function with Puppeteer
- Regional execution
- File + HTTP caching
- 3-5s cold starts

**Future State:**
- Cloudflare Worker with Browser Rendering
- Global edge execution  
- Cache API + R2 + HTTP caching
- <1s cold starts

**Migration:**
- Simple URL changes in index.mjs
- Optional redirects for compatibility
- Gradual rollout possible
- Easy rollback if needed

**Benefits:**
- ⚡ Faster response times
- 🌍 Global availability
- 💰 Better cost efficiency
- 📊 Superior monitoring
- 🔄 Better caching

---

**Status:** 📋 Plan Complete - Ready for Implementation  
**Next Steps:** Begin TypeScript implementation in `/grab/src/`
