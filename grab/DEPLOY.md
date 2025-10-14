# Grab Worker - Deployment Guide

Complete deployment guide for the Aesthetic Computer screenshot generation service.

## 🎯 Current Status

**Production URL**: https://grab.aesthetic.computer  
**Workers.dev URL**: https://aesthetic-grab.aesthetic-computer.workers.dev  
**Latest Version**: e77346d0-3732-4bd2-b03b-e932dc50fac9

### Quick Test
```bash
# Health check
curl "https://grab.aesthetic.computer/health"

# Icon (128x128)
curl "https://grab.aesthetic.computer/icon/128x128/prompt.png" --output test.png

# Preview (1200x630)
curl "https://grab.aesthetic.computer/preview/1200x630/prompt.png" --output test.png
```

---

## 🚀 Deployment

### Method 1: Simple Deploy (Current Domain)

If `grab.aesthetic.computer` is already configured:

```bash
cd /workspaces/aesthetic-computer/grab
npm run deploy
```

This updates the worker code while keeping existing DNS/domain configuration.

### Method 2: Deploy with Custom Domain

The `wrangler.toml` includes the custom domain configuration:

```toml
routes = [
  { pattern = "grab.aesthetic.computer", custom_domain = true }
]
```

When you run `npm run deploy`, Wrangler automatically:
1. Deploys the worker code
2. Configures the custom domain route
3. Sets up DNS (if using Cloudflare-managed domain)

**Note**: Custom domains require your domain to be on Cloudflare. Since `aesthetic.computer` is already managed by Cloudflare, this works automatically.

---

## 📋 Pre-Deployment Checklist

- [ ] Cloudflare account access (`CLOUDFLARE_API_TOKEN` in environment)
- [ ] `aesthetic.computer` domain on Cloudflare
- [ ] Browser Rendering enabled on account
- [ ] Dependencies installed: `npm install`
- [ ] Types checked: `npm run type-check`

---

## 🔧 Configuration

### Environment Variables (wrangler.toml)

```toml
[vars]
ENVIRONMENT = "production"
DOMAIN = "grab.aesthetic.computer"

# Cache Configuration
CACHE_TTL_SECONDS = "3600"           # 1 hour
CDN_CACHE_TTL_SECONDS = "86400"      # 24 hours

# Screenshot Configuration
MAX_SCREENSHOT_AGE_MS = "604800000"  # 7 days
BROWSER_TIMEOUT_MS = "30000"         # 30 seconds
MAX_VIEWPORT_WIDTH = "1920"
MAX_VIEWPORT_HEIGHT = "1080"

# Rate Limiting
MAX_REQUESTS_PER_MINUTE = "60"
MAX_BROWSER_SESSIONS = "10"
```

### Cloudflare Requirements

**Browser Rendering API**:
- Automatically included with Workers Paid plan ($5/month)
- Provides headless Chrome browser instances
- Used for screenshot generation

**Durable Objects**:
- Included with Workers Paid plan
- Used for browser session management
- Reduces cold start times

---

## 🧪 Testing After Deployment

### 1. Health Check
```bash
curl "https://grab.aesthetic.computer/health"
# Expected: {"status":"ok","timestamp":"...","environment":"production"}
```

### 2. Icon Generation
```bash
curl "https://grab.aesthetic.computer/icon/128x128/prompt.png" --output icon.png
file icon.png
# Expected: PNG image data, 128 x 128
```

### 3. Preview Generation
```bash
curl "https://grab.aesthetic.computer/preview/1200x630/prompt.png" --output preview.png
file preview.png
# Expected: PNG image data, 1200 x 630
```

### 4. Cache Headers
```bash
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"
# Expected:
# Cache-Control: public, max-age=3600
# CF-Cache-Status: HIT (on second request)
```

### 5. Error Handling
```bash
# Invalid format
curl "https://grab.aesthetic.computer/invalid"
# Expected: {"error":"Invalid request format..."}

# Oversized dimensions
curl "https://grab.aesthetic.computer/icon/5000x5000/prompt.png"
# Expected: {"error":"Invalid dimensions..."}
```

---

## 📊 Monitoring

### View Logs
```bash
cd /workspaces/aesthetic-computer/grab
npm run logs
```

### Check Recent Deployments
```bash
npx wrangler deployments list
```

### Monitor Usage
- Dashboard: https://dash.cloudflare.com
- Navigate to: Workers & Pages → aesthetic-grab
- View: Requests, Errors, CPU Time, Duration

### Key Metrics
- **Success Rate**: Should be >99%
- **Response Time**: 7-15 seconds for first request, <1s for cached
- **Cache Hit Rate**: Should increase over time
- **Error Rate**: Monitor 4xx/5xx responses

---

## 🔄 Rollback

If issues occur after deployment:

```bash
cd /workspaces/aesthetic-computer/grab

# List recent deployments
npx wrangler deployments list

# Rollback to previous version
npx wrangler rollback [version-id]
```

---

## 🏗️ Architecture

```
User Request
    ↓
grab.aesthetic.computer (Custom Domain)
    ↓
Cloudflare Workers (aesthetic-grab)
    ↓
Durable Object (Browser session management)
    ↓
Browser Rendering API (@cloudflare/puppeteer)
    ↓
Screenshot Generation (PNG)
    ↓
Response (with Cache-Control headers)
    ↓
Cloudflare CDN (24hr cache)
```

### Components
- **Workers**: Serverless compute, handles HTTP requests
- **Durable Objects**: Manages browser session lifecycle
- **Browser Rendering**: Provides Chrome instances for screenshots
- **CDN**: Caches generated screenshots globally

---

## 📁 Integration Points

### parse.mjs
```javascript
// Lines 487-494
const iconURL = `https://grab.aesthetic.computer/icon/128x128/${pieceParam}${query}`;
const previewURL = `https://grab.aesthetic.computer/preview/1200x630/${pieceParam}${query}`;
```

### netlify.toml
```toml
# Lines 285-290: Icon redirect
[[redirects]]
  from = "/aesthetic.computer/api/icon/:piece"
  to = "https://grab.aesthetic.computer/icon/128x128/:piece.png"
  status = 200

# Lines 306-310: Preview redirect  
[[redirects]]
  from = "/aesthetic.computer/api/preview/:piece"
  to = "https://grab.aesthetic.computer/preview/1200x630/:piece.png"
  status = 200
```

---

## 🐛 Troubleshooting

### "Could not resolve host: grab.aesthetic.computer"
- DNS not propagated yet (wait 5-10 minutes)
- Check DNS: `dig grab.aesthetic.computer`
- Verify custom domain in wrangler.toml

### "Browser rendering is not available"
- Ensure Workers Paid plan is active
- Check account settings in Cloudflare dashboard
- Browser Rendering API should be enabled

### Screenshots are empty/white
- Check Browser Rendering API status
- Verify viewport dimensions in env vars
- Review logs: `npm run logs`
- Ensure piece exists on aesthetic.computer

### Slow response times
- First request is always slower (browser startup)
- Check if Durable Object is working (session reuse)
- Monitor Browser Rendering API latency
- Consider adjusting `BROWSER_TIMEOUT_MS`

### High error rates
- Check rate limiting (`MAX_REQUESTS_PER_MINUTE`)
- Review browser timeout settings
- Monitor Durable Object errors
- Check for upstream issues (aesthetic.computer availability)

---

## 📝 Related Documentation

- **README.md**: Usage guide and API reference
- **VIDEO-FUTURE.md**: Future video capture infrastructure
- **docs/**: Detailed technical documentation
  - `ANIMATED-GIF-STATUS.md`: Video recording research
  - `INTEGRATION-SUMMARY.md`: System integration details
  - `DEV-SETUP.md`: Local development guide

---

## 🎯 Next Steps

1. **Monitor Production**: Watch logs and metrics for first few days
2. **Test Integration**: Verify parse.mjs and netlify.toml redirects
3. **Cache Tuning**: Adjust TTLs based on usage patterns
4. **Video Capture**: When needed, see VIDEO-FUTURE.md for VPS approach

---

**Last Updated**: October 14, 2025  
**Deployed Version**: e77346d0-3732-4bd2-b03b-e932dc50fac9
