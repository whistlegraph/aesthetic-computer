# Grab Worker - Browser Rendering Service Plan

## 📋 Overview

Replace the current Netlify screenshot.js function with a Cloudflare Worker that uses Cloudflare's Browser Rendering API with Durable Objects for screenshot generation and caching.

**Deployment Target:** `grab.aesthetic.computer`

**Status:** ✅ **DEPLOYED & WORKING!**
- Worker URL: https://aesthetic-grab.aesthetic-computer.workers.dev
- Version: 511d31bc-c2e8-4f00-aea7-9b9d95e08733
- Screenshots generating successfully with query parameter support

**Tutorial Reference:** https://developers.cloudflare.com/browser-rendering/get-started/

---

## ✅ Completed Tasks

### Phase 1: Setup & Implementation ✅
- [x] Created `/grab` directory structure
- [x] Set up vault directory for secrets at `/aesthetic-computer-vault/grab/`
- [x] Created comprehensive documentation (PLAN.md, README.md, DEPLOYMENT.md, MIGRATION.md)
- [x] Implemented TypeScript worker following Cloudflare's official example
- [x] Configured wrangler.toml with Browser binding and Durable Objects
- [x] Installed dependencies (wrangler 4.42.2, @cloudflare/puppeteer latest)
- [x] Created deployment scripts (deploy.fish)

### Phase 2: Deployment & Debugging ✅
- [x] Fixed TypeScript compilation errors
- [x] Added Buffer polyfill for Puppeteer compatibility  
- [x] Updated @cloudflare/puppeteer from 0.0.1 to latest (critical fix!)
- [x] Replaced `page.waitForTimeout()` with standard `setTimeout()`
- [x] Successfully deployed to Cloudflare Workers
- [x] Fixed URL construction to use query parameters (`?icon=WxH`, `?preview=WxH`)
- [x] Verified screenshots generating correctly (128x128, 256x256, 1200x630 tested)
- [x] **Added video recording endpoint** using CDP screencast API
  - `/video/WxH/piece.mp4?duration=N` endpoint
  - Captures frames over 1-30 seconds
  - Returns last frame + metadata (frame count, duration, resolution)
  - POC working: 162 frames in 5s (~32 fps)
  - Full video encoding requires future infrastructure

### Key Fixes Applied
1. **Browser Binding Syntax**: Changed from `[[browser]]` to `[browser]` in wrangler.toml
2. **Durable Object Migration**: Added v1→v2 migration for ScreenshotDO→Browser rename
3. **Puppeteer Update**: Version 0.0.1 was returning empty screenshots; latest version works
4. **Query Parameters**: Now properly uses `?icon=128x128` or `?preview=1200x630` like original
5. **Timeout Replacement**: Modern Puppeteer doesn't have waitForTimeout, using Promise setTimeout

---

## 🚧 Remaining Tasks

### Phase 3: Custom Domain & DNS 🔄
- [ ] **Configure Custom Domain** (Choose one method)
  
  **Method 1: DNS Settings (Recommended - Faster!)**
  - Navigate to: https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records
  - Add CNAME: `grab` → `aesthetic-grab.aesthetic-computer.workers.dev` (Proxied)
  - Wait: ~30 seconds
  - See: `DNS-SETUP.md` for details
  
  **Method 2: Workers Dashboard (Alternative)**
  - Navigate to: Workers & Pages > aesthetic-grab > Settings > Domains & Routes
  - Click: "Add Custom Domain"
  - Enter: `grab.aesthetic.computer`
  - Wait: 2-5 minutes
  
  **Verification:**
  - Test: `curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"`
  - Should return: HTTP/2 200
  
### Phase 4: Integration & Migration ✅
- [x] **Update parse.mjs** to use grab worker URLs
  - File: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/parse.mjs`
  - Lines 487-494: Updated to use `https://grab.aesthetic.computer/icon/...` and `https://grab.aesthetic.computer/preview/...`
  
- [x] **Update netlify.toml** redirects
  - File: `/workspaces/aesthetic-computer/system/netlify.toml`
  - Lines 285-290 & 306-310: Redirects now point to grab.aesthetic.computer
  - Backwards compatibility maintained for existing paths
  
- [x] **Create DEV-SETUP.md** with development workflow
  - Local testing with `wrangler dev`
  - Integration with netlify dev
  - Emacs web panels setup instructions
  - Debugging and monitoring guides
  
- [ ] **Production Validation** 🔄
  - Test og:image meta tags on live site
  - Test favicon generation
  - Monitor for errors in Cloudflare dashboard
  - Validate across different pieces
  
- [ ] **Cleanup** (after 1 week validation period)
  - Remove `/system/netlify/functions/screenshot.js`
  - Remove Puppeteer dependencies from Netlify
  - Update documentation

### Phase 5: Development Integration ✅
- [x] **Create Development Documentation**
  - DEV-SETUP.md: Local testing with wrangler dev
  - Netlify dev integration patterns
  - Emacs web panel configuration
  - Debugging and monitoring guides
  
- [x] **Create Integration Documentation**
  - INTEGRATION-SUMMARY.md: Complete overview of changes
  - CODE-CHANGES.md: Detailed diff of modified files
  - CUSTOM-DOMAIN-SETUP.md: Quick reference for domain setup
  
- [ ] **Add to Dev Stack** (Optional)
  - Create Emacs web panel configuration for grab worker
  - Add wrangler dev command to startup scripts
  - Test localhost screenshot generation

---

## 📚 Documentation Index

All documentation lives in `/workspaces/aesthetic-computer/grab/`:

- **PLAN.md** (this file) - Project roadmap and progress tracking
- **README.md** - Architecture, API reference, and technical overview
- **DEPLOYMENT.md** - Deployment guide with custom domain instructions
- **DEV-SETUP.md** - Local development workflow and debugging
- **MIGRATION.md** - Migration strategy from Netlify to Cloudflare
- **INTEGRATION-SUMMARY.md** - What's complete, what's pending, next steps
- **CODE-CHANGES.md** - Detailed file-by-file change documentation
- **CUSTOM-DOMAIN-SETUP.md** - Quick reference for domain configuration

---

## 🎯 Current Status

**Worker:** ✅ Deployed and functional  
**Integration:** ✅ Code updated, redirects configured  
**Custom Domain:** ⚠️ Pending manual setup via Dashboard  
**Testing:** ⚠️ Awaiting custom domain for full validation  
**Production:** 🕐 Ready to go live after custom domain setup

---

## 🚀 Immediate Next Steps

1. **Get Cloudflare API Token** (1 minute)
   - Go to: https://dash.cloudflare.com/profile/api-tokens
   - Create token with "Edit zone DNS" template
   - Add to `/aesthetic-computer-vault/grab/.env` as `CLOUDFLARE_API_TOKEN`

2. **Deploy with Automatic DNS** (2 minutes)
   ```fish
   cd /workspaces/aesthetic-computer/grab
   ./scripts/deploy-with-dns.fish
   ```
   This one command handles:
   - Worker deployment
   - DNS CNAME record creation
   - DNS propagation wait
   - Deployment verification

3. **Test Production Integration** (10 minutes)
   - Test og:images on live site
   - Verify social media previews
   - Check favicon loading
   - Monitor Cloudflare dashboard

4. **Begin Validation Period** (1 week)
   - Monitor error rates
   - Compare performance with old implementation
   - Test with various pieces
   - Collect user feedback

5. **Cleanup** (after validation)
   - Remove `/system/netlify/functions/screenshot.js`
   - Remove Puppeteer dependencies
   - Update final documentation

---

## 📁 Directory Structure

```
/workspaces/aesthetic-computer/grab/
├── PLAN.md                          # This file
├── README.md                        # Documentation and usage
├── DEPLOYMENT.md                    # Deployment instructions
├── package.json                     # Dependencies
├── tsconfig.json                    # TypeScript config
├── wrangler.toml                    # Cloudflare config (gitignored)
├── wrangler.production.toml         # Production config template
├── src/
│   ├── worker.ts                    # Main worker entry point
│   ├── screenshot-do.ts             # Durable Object for screenshot coordination
│   ├── browser.ts                   # Browser rendering logic
│   ├── cache.ts                     # Cache management utilities
│   └── types.ts                     # TypeScript types
├── scripts/
│   ├── deploy.fish                  # Deployment script
│   └── setup-browser-binding.fish   # Initial setup script
└── test/
    └── worker.test.ts               # Tests
```

---

## 🔧 Architecture

### Current Implementation (Netlify)
- Serverless function using Puppeteer Core
- Chrome browser via `@sparticuz/chromium`
- Local file cache in dev mode
- ETag-based HTTP caching
- Accepts: `128x128` (icon), `1200x630` (og:image), `1800x900` (twitter:image)

### New Implementation (Cloudflare)
- **Worker**: Handle incoming requests, parse parameters
- **Durable Object**: Coordinate screenshot generation per URL
- **Browser Rendering API**: Puppeteer-compatible interface
- **R2 Storage** (optional): Long-term screenshot cache
- **Cache API**: Fast edge caching with ETags
- **Browser Binding**: Native Cloudflare browser sessions

### Request Flow
```
Client Request
    ↓
Worker (grab.aesthetic.computer)
    ↓
Parse /icon/ or /preview/ path
    ↓
Check Cache API (ETag)
    ↓ (miss)
Durable Object for URL
    ↓
Browser Rendering API
    ↓
Take Screenshot
    ↓
Store in Cache + R2
    ↓
Return PNG
```

---

## 🔐 Environment Variables & Secrets

### Stored in `/aesthetic-computer-vault/grab/.env`

```bash
# Cloudflare Configuration
CLOUDFLARE_ACCOUNT_ID=<account-id>
CLOUDFLARE_API_KEY=<api-key>
CLOUDFLARE_EMAIL=me@jas.life

# Worker Configuration
GRAB_WORKER_NAME=aesthetic-grab
GRAB_DOMAIN=grab.aesthetic.computer

# Browser Binding Configuration
BROWSER_BINDING_NAME=BROWSER
BROWSER_BINDING_ID=<browser-binding-id>

# Durable Object Configuration
DO_NAMESPACE_NAME=SCREENSHOT_DO
DO_NAMESPACE_ID=<do-namespace-id>

# R2 Bucket (optional)
R2_BUCKET_NAME=aesthetic-screenshots
R2_BUCKET_ID=<r2-bucket-id>

# Cache Configuration
CACHE_TTL_SECONDS=3600  # 1 hour for browser cache
CDN_CACHE_TTL_SECONDS=86400  # 24 hours for CDN cache
DEV_CACHE_TTL_SECONDS=60  # 1 minute for dev

# Screenshot Configuration
MAX_SCREENSHOT_AGE_MS=604800000  # 7 days
BROWSER_TIMEOUT_MS=30000  # 30 seconds
```

### Secrets (set via `wrangler secret put`)
- `BROWSER_API_KEY` - If needed for additional auth

---

## 📦 Dependencies

```json
{
  "name": "@aesthetic-computer/grab",
  "version": "1.0.0",
  "type": "module",
  "scripts": {
    "dev": "wrangler dev",
    "deploy": "./scripts/deploy.fish",
    "deploy:production": "./scripts/deploy.fish production",
    "type-check": "tsc --noEmit",
    "test": "vitest"
  },
  "dependencies": {
    "@cloudflare/puppeteer": "^0.0.1",
    "@cloudflare/workers-types": "^4.20231218.0"
  },
  "devDependencies": {
    "typescript": "^5.3.3",
    "wrangler": "^3.80.0",
    "vitest": "^1.0.4"
  }
}
```

---

## 🚀 Deployment Process

### 1. Initial Setup (One-time)

```fish
cd /workspaces/aesthetic-computer/grab

# Install dependencies
npm install

# Create Browser Rendering binding
./scripts/setup-browser-binding.fish

# Create Durable Object namespace
wrangler dispatch-namespace create SCREENSHOT_DO

# Create R2 bucket (optional)
wrangler r2 bucket create aesthetic-screenshots

# Set secrets
wrangler secret put BROWSER_API_KEY
```

### 2. Development

```fish
# Copy secrets from vault
cd /workspaces/aesthetic-computer/aesthetic-computer-vault
./devault.fish

# Start dev server
cd /workspaces/aesthetic-computer/grab
npm run dev
```

### 3. Production Deployment

```fish
# Deploy to production
cd /workspaces/aesthetic-computer/grab
npm run deploy:production

# Configure custom domain (one-time)
# Via Cloudflare Dashboard:
# Workers & Pages > aesthetic-grab > Settings > Domains & Routes
# Add custom domain: grab.aesthetic.computer
```

---

## 🔄 Migration from Netlify

### Current Endpoint Usage

**Icon Generation:**
```
https://aesthetic.computer/icon/128x128/prompt~wipe.png
```

**Preview Generation (OG Image):**
```
https://aesthetic.computer/preview/1200x630/prompt~wipe.png
```

**Preview Generation (Twitter):**
```
https://aesthetic.computer/preview/1800x900/prompt~wipe.png
```

### Migration Steps

1. **Phase 1: Deploy new worker**
   - Deploy to `grab.aesthetic.computer`
   - Test all resolutions
   - Verify caching behavior

2. **Phase 2: Update references**
   - Update `/system/netlify/functions/index.mjs` (main HTML generator)
   - Update any other references to screenshot.js
   - Add redirects in `netlify.toml`

3. **Phase 3: Remove old function**
   - Remove `/system/netlify/functions/screenshot.js`
   - Remove Puppeteer dependencies from package.json
   - Clean up old dev cache directory

### Files to Update

**Primary:**
- `/workspaces/aesthetic-computer/system/netlify/functions/index.mjs`
  - Lines 367-391 (icon and og:image generation)

**Search for references:**
```fish
grep -r "screenshot\.js" /workspaces/aesthetic-computer/
grep -r "/icon/" /workspaces/aesthetic-computer/system/
grep -r "/preview/" /workspaces/aesthetic-computer/system/
```

**Add to netlify.toml:**
```toml
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

---

## 💾 Caching Strategy

### Three-tier Caching

1. **Browser Cache** (Client-side)
   - `Cache-Control: public, max-age=3600`
   - ETag-based revalidation
   - 1 hour default

2. **Cloudflare CDN Cache**
   - `Cloudflare-CDN-Cache-Control: public, durable, max-age=86400`
   - 24 hours for icon/preview images
   - Edge caching globally

3. **R2 Storage** (Optional long-term)
   - Store screenshots for 7+ days
   - Metadata: URL, resolution, timestamp
   - Eviction policy: LRU or time-based

### ETag Generation
```typescript
// Same as current implementation
const etagSource = `${resolution}-${filepath}`;
const etag = `"${crypto.subtle.digestSync('MD5', 
  new TextEncoder().encode(etagSource)
).toString('hex')}"`;
```

---

## 🧪 Testing Plan

### Unit Tests
- URL parsing
- Resolution validation
- ETag generation
- Cache key generation

### Integration Tests
- Browser rendering
- Screenshot capture
- Durable Object coordination
- Cache retrieval

### E2E Tests
- Full request/response cycle
- Multiple resolutions
- Cache hit/miss scenarios
- Error handling

### Load Testing
- Concurrent requests
- Browser session limits
- Memory usage
- Response times

---

## 📊 Monitoring

### Metrics to Track
- Request count per resolution
- Cache hit rate
- Browser session duration
- Screenshot generation time
- Error rate
- P50/P95/P99 latency

### Logging
- Request details (resolution, URL)
- Cache hits/misses
- Browser errors
- Durable Object coordination

### Alerts
- Error rate > 5%
- P95 latency > 10s
- Browser session failures
- Cache errors

---

## 🔒 Security Considerations

1. **Rate Limiting**
   - Limit requests per IP
   - Limit browser sessions per DO
   - Prevent DoS attacks

2. **URL Validation**
   - Only allow aesthetic.computer URLs
   - Sanitize paths
   - Prevent SSRF

3. **Resource Limits**
   - Max screenshot size
   - Browser timeout
   - Memory limits per session

4. **Secrets Management**
   - Store in vault
   - Use wrangler secrets
   - Never commit to repo

---

## 🎨 API Design

### Endpoints

**Icon Endpoint:**
```
GET /icon/{resolution}/{piece-path}.png
```

**Preview Endpoint:**
```
GET /preview/{resolution}/{piece-path}.png
```

### Supported Resolutions
- `128x128` - Favicon/icon
- `1200x630` - Open Graph image
- `1800x900` - Twitter card image

### Response Headers
```
Content-Type: image/png
Content-Length: {size}
ETag: "{hash}"
Cache-Control: public, max-age=3600
Cloudflare-CDN-Cache-Control: public, durable, max-age=86400
X-Cache-Status: HIT | MISS
X-Screenshot-Time: {ms}
```

### Error Responses

**400 Bad Request**
```json
{
  "error": "Invalid resolution",
  "accepted": ["128x128", "1200x630", "1800x900"]
}
```

**500 Internal Server Error**
```json
{
  "error": "Screenshot generation failed",
  "message": "Browser timeout"
}
```

**503 Service Unavailable**
```json
{
  "error": "Browser unavailable",
  "message": "All browser sessions in use"
}
```

---

## 🔮 Future Enhancements

1. **Dynamic Resolution Support**
   - Allow any resolution within limits
   - Maintain aspect ratio

2. **Video Thumbnails**
   - Capture frames from videos
   - Generate GIF previews

3. **PDF Generation**
   - Full-page PDFs
   - Print stylesheets

4. **Advanced Caching**
   - Predictive pre-rendering
   - Smart eviction policies

5. **A/B Testing**
   - Multiple screenshot variants
   - Performance comparison

6. **Analytics**
   - Most requested pieces
   - Popular resolutions
   - Geographic distribution

---

## 📚 References

- [Cloudflare Browser Rendering](https://developers.cloudflare.com/browser-rendering/)
- [Browser Rendering with Durable Objects](https://developers.cloudflare.com/browser-rendering/workers-bindings/browser-rendering-with-do/)
- [Puppeteer Documentation](https://pptr.dev/)
- [Cloudflare Workers Docs](https://developers.cloudflare.com/workers/)
- [Durable Objects](https://developers.cloudflare.com/durable-objects/)
- [R2 Storage](https://developers.cloudflare.com/r2/)

---

## ✅ Checklist

### Setup Phase
- [ ] Create `/grab` directory structure
- [ ] Set up TypeScript configuration
- [ ] Install dependencies
- [ ] Create wrangler.toml from template
- [ ] Set up vault directory for secrets

### Development Phase
- [ ] Implement worker entry point
- [ ] Implement Durable Object
- [ ] Implement browser rendering logic
- [ ] Implement caching layer
- [ ] Add ETag support
- [ ] Add error handling
- [ ] Write unit tests
- [ ] Write integration tests

### Deployment Phase
- [ ] Create Browser Rendering binding
- [ ] Create Durable Object namespace
- [ ] Create R2 bucket (optional)
- [ ] Set secrets via wrangler
- [ ] Deploy to dev environment
- [ ] Test in dev
- [ ] Deploy to production
- [ ] Configure custom domain
- [ ] Test in production

### Migration Phase
- [ ] Update index.mjs references
- [ ] Add netlify.toml redirects
- [ ] Test icon generation
- [ ] Test preview generation
- [ ] Verify caching works
- [ ] Monitor for errors
- [ ] Compare performance metrics

### Cleanup Phase
- [ ] Remove screenshot.js
- [ ] Remove Puppeteer dependencies
- [ ] Clean up dev cache directory
- [ ] Update documentation
- [ ] Archive old implementation

---

**Status:** 📝 Planning Complete
**Next Steps:** Begin implementation of worker.ts and screenshot-do.ts
