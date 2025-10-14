# Grab Worker Deployment Guide

Comprehensive deployment guide for the Aesthetic Computer screenshot/preview generation service.

---

## ✅ Current Status

**Deployed Version:** 511d31bc-c2e8-4f00-aea7-9b9d95e08733  
**Worker URL:** https://aesthetic-grab.aesthetic-computer.workers.dev  
**Custom Domain:** grab.aesthetic.computer (⚠️ pending manual setup)

**Test Commands:**
```fish
# Test icon endpoint
curl -I "https://aesthetic-grab.aesthetic-computer.workers.dev/icon/128x128/prompt.png"

# Test preview endpoint  
curl -I "https://aesthetic-grab.aesthetic-computer.workers.dev/preview/1200x630/prompt.png"
```

---

## 🚀 Quick Deploy with Automatic DNS

**Recommended: One-command deployment with automatic DNS configuration**

```fish
cd /workspaces/aesthetic-computer/grab
./scripts/deploy-with-dns.fish
```

This will:
1. ✅ Deploy worker to Cloudflare
2. ✅ Automatically create/update CNAME record via API
3. ✅ Wait for DNS propagation
4. ✅ Verify deployment is accessible

**Requirements:**
- `CLOUDFLARE_EMAIL` in vault/.env
- `CLOUDFLARE_API_TOKEN` in vault/.env (with Zone.DNS Edit permission)

See vault README for how to get API token: `/aesthetic-computer-vault/grab/README.md`

---

## 🌐 Custom Domain Setup

**Automated via deploy script (recommended):** The `deploy-with-dns.fish` script handles this automatically.

**Manual methods if needed:**

### Method 1: Direct DNS Configuration (2 minutes) ⚡

**Quick link:** https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records

1. **Open DNS Settings**
   - Navigate to: Cloudflare Dashboard → aesthetic.computer → DNS → Records

2. **Add CNAME Record**
   - Click: **"Add record"**
   - Type: **CNAME**
   - Name: `grab`
   - Target: `aesthetic-grab.aesthetic-computer.workers.dev`
   - Proxy status: **Proxied** (orange cloud ☁️)
   - Click: **Save**

3. **Wait ~30 seconds** for DNS propagation

4. **Verify Setup**
   ```fish
   curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"
   ```
   
   Should return:
   ```
   HTTP/2 200
   content-type: image/png
   cache-control: public, max-age=3600
   ```

✅ **Done!** Total time: ~2 minutes

### Method 2: Workers Dashboard (Alternative)

1. **Navigate to:** https://dash.cloudflare.com/
2. **Go to:** Workers & Pages → aesthetic-grab → Settings → Domains & Routes
3. **Click:** "Add Custom Domain"
4. **Enter:** `grab.aesthetic.computer`
5. **Save** and wait 2-5 minutes

This automatically creates the same CNAME record as Method 1.

### DNS Record Created

Either method creates:
```
grab.aesthetic.computer → aesthetic-grab.aesthetic-computer.workers.dev (CNAME, Proxied)
```

Cloudflare automatically handles:
- ✅ SSL/TLS certificates
- ✅ Edge routing
- ✅ DDoS protection
- ✅ CDN caching

---

## 📋 Prerequisites

- Cloudflare account with Workers enabled
- Browser Rendering enabled in your Cloudflare account
- Wrangler CLI installed globally (`npm i -g wrangler`)
- Access to `/aesthetic-computer-vault/grab/` secrets
- Node.js 20+ installed

---

## 🔐 Initial Setup

### 1. Clone and Install

```fish
cd /workspaces/aesthetic-computer/grab
npm install
```

### 2. Authenticate with Cloudflare

```fish
wrangler login
```

This will open a browser to authenticate your Cloudflare account.

### 3. Verify Account Access

```fish
wrangler whoami
```

Ensure:
- Account ID is correct
- Browser Rendering is in enabled features
- Workers permission is active

### 4. Enable Browser Rendering

If not already enabled:

```fish
# Enable via Cloudflare Dashboard
# Workers & Pages > Browser Rendering > Enable
```

Or via API:
```fish
curl -X POST "https://api.cloudflare.com/client/v4/accounts/<ACCOUNT_ID>/browser" \
  -H "X-Auth-Email: me@jas.life" \
  -H "X-Auth-Key: <API_KEY>" \
  -H "Content-Type: application/json"
```

---

## 🚀 Deployment Steps

### Phase 1: Create Infrastructure

#### 1.1 Create Browser Binding

```fish
cd /workspaces/aesthetic-computer/grab
./scripts/setup-browser-binding.fish
```

This script will:
- Create the browser binding named `BROWSER`
- Output the binding configuration
- Update the vault `.env` file

Alternatively, manually:
```fish
wrangler browser create BROWSER
```

#### 1.2 Create Durable Object Namespace

```fish
wrangler dispatch-namespace create SCREENSHOT_DO
```

Copy the output ID and update `/aesthetic-computer-vault/grab/.env`:
```bash
DO_NAMESPACE_ID=<paste-id-here>
```

#### 1.3 Create R2 Bucket (Optional)

For long-term caching:

```fish
wrangler r2 bucket create aesthetic-screenshots
```

Note the bucket name/ID for configuration.

### Phase 2: Configure Wrangler

#### 2.1 Copy Production Config

```fish
# Copy template from vault to working directory
cp /workspaces/aesthetic-computer/aesthetic-computer-vault/grab/wrangler.production.toml \
   /workspaces/aesthetic-computer/grab/wrangler.toml
```

#### 2.2 Update IDs in wrangler.toml

Edit `/workspaces/aesthetic-computer/grab/wrangler.toml`:

```toml
# Update these with actual IDs from previous steps
[[durable_objects.bindings]]
name = "SCREENSHOT_DO"
class_name = "ScreenshotDO"
script_name = "aesthetic-grab"
# namespace_id = "<from step 1.2>"  # If needed

[[r2_buckets]]
binding = "SCREENSHOTS"
bucket_name = "aesthetic-screenshots"  # From step 1.3
```

**⚠️ Important:** Add `wrangler.toml` to `.gitignore` (it's environment-specific)

### Phase 3: Set Secrets

If you have additional secrets (optional):

```fish
cd /workspaces/aesthetic-computer/grab
wrangler secret put BROWSER_API_KEY
# Enter secret when prompted
```

### Phase 4: Test Locally

#### 4.1 Start Dev Server

```fish
npm run dev
```

#### 4.2 Test Endpoints

```fish
# Test icon
curl http://localhost:8787/icon/128x128/welcome.png -o test-icon.png
open test-icon.png

# Test preview
curl http://localhost:8787/preview/1200x630/prompt~wipe.png -o test-preview.png
open test-preview.png

# Test caching with ETag
curl -I http://localhost:8787/icon/128x128/welcome.png
# Note the ETag header

curl -H "If-None-Match: <etag-from-above>" \
     http://localhost:8787/icon/128x128/welcome.png
# Should return 304 Not Modified
```

### Phase 5: Deploy to Production

#### 5.1 Type Check

```fish
npm run type-check
```

Fix any TypeScript errors before deploying.

#### 5.2 Deploy

```fish
npm run deploy:production
```

Or manually:
```fish
wrangler deploy --config wrangler.toml
```

#### 5.3 Verify Deployment

```fish
# Check worker is live
curl -I https://aesthetic-grab.<account>.workers.dev/icon/128x128/welcome.png

# View logs
npm run logs
```

### Phase 6: Configure Custom Domain

#### Via Cloudflare Dashboard (Recommended)

1. Go to https://dash.cloudflare.com/
2. Select your account
3. Navigate to **Workers & Pages**
4. Click on **aesthetic-grab**
5. Go to **Settings** > **Domains & Routes**
6. Click **Add Custom Domain**
7. Enter: `grab.aesthetic.computer`
8. Click **Add Domain**

Cloudflare will automatically:
- Create DNS CNAME record
- Set up SSL/TLS
- Route traffic to worker

#### Via Wrangler (Alternative)

```fish
wrangler route add grab.aesthetic.computer/* aesthetic-grab
```

#### Verify Domain

```fish
# Test custom domain
curl -I https://grab.aesthetic.computer/icon/128x128/welcome.png

# Should return 200 with your screenshot
```

---

## 🔄 Migration from Netlify

### Step 1: Verify Worker is Working

Test all resolutions:

```fish
# Icon (128x128)
curl https://grab.aesthetic.computer/icon/128x128/welcome.png -o test-icon.png

# OG Image (1200x630)
curl https://grab.aesthetic.computer/preview/1200x630/prompt~wipe.png -o test-og.png

# Twitter Card (1800x900)
curl https://grab.aesthetic.computer/preview/1800x900/prompt~wipe.png -o test-twitter.png
```

### Step 2: Update index.mjs

Edit `/workspaces/aesthetic-computer/system/netlify/functions/index.mjs`:

Find the icon and ogImage generation (around line 360-380):

**Before:**
```javascript
const icon = pixelPerfect(
  baseURL,
  parsed?.host,
  `/icon/128x128/${location}.png`,
  meta,
  "https:",
);

const ogImage = pixelPerfect(
  baseURL,
  parsed?.host,
  `/preview/1200x630/${location}.png`,
  meta,
  "https:",
);

const twitterImage = pixelPerfect(
  baseURL,
  parsed?.host,
  `/preview/1800x900/${location}.png`,
  meta,
  "https:",
);
```

**After:**
```javascript
// Direct URLs to grab worker
const icon = `https://grab.aesthetic.computer/icon/128x128/${location}.png`;
const ogImage = `https://grab.aesthetic.computer/preview/1200x630/${location}.png`;
const twitterImage = `https://grab.aesthetic.computer/preview/1800x900/${location}.png`;
```

### Step 3: Add Redirects (Optional)

Edit `/workspaces/aesthetic-computer/system/netlify.toml`:

```toml
# Redirect legacy paths to grab worker
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

### Step 4: Deploy and Test

```fish
# Commit changes
git add system/netlify/functions/index.mjs
git commit -m "Switch to grab worker for screenshots"
git push

# Test on live site
curl -I https://aesthetic.computer/icon/128x128/welcome.png
# Should redirect to grab.aesthetic.computer

# Check HTML meta tags
curl https://aesthetic.computer/welcome | grep "og:image"
# Should show grab.aesthetic.computer URLs
```

### Step 5: Monitor

```fish
# Watch grab worker logs
npm run logs:production

# Check for errors
# Monitor traffic in Cloudflare Dashboard
```

### Step 6: Clean Up (After 1 Week)

Once confirmed working:

```fish
# Remove old screenshot function
rm /workspaces/aesthetic-computer/system/netlify/functions/screenshot.js

# Remove Puppeteer dependencies
cd /workspaces/aesthetic-computer/system
npm uninstall puppeteer-core @sparticuz/chromium

# Clean dev cache
rm -rf /workspaces/aesthetic-computer/.netlify/cache/screenshots
```

---

## 📊 Post-Deployment Monitoring

### Check Metrics

In Cloudflare Dashboard:
- Workers & Pages > aesthetic-grab > Metrics
- Look for:
  - Request rate
  - Error rate
  - P95 latency
  - Cache hit rate

### Set Up Alerts

Configure notifications for:
- Error rate > 5%
- P95 latency > 10s
- Browser session failures

### View Logs

```fish
# Real-time logs
wrangler tail aesthetic-grab --env production

# Filter for errors
wrangler tail aesthetic-grab --env production --status error
```

### Test Cache Performance

```fish
# First request (cache miss)
time curl https://grab.aesthetic.computer/icon/128x128/welcome.png -o /dev/null

# Second request (cache hit)
time curl https://grab.aesthetic.computer/icon/128x128/welcome.png -o /dev/null

# Second should be much faster
```

---

## 🐛 Troubleshooting

### Deployment Fails

**Error:** `Browser rendering not enabled`

**Solution:**
```fish
# Enable in dashboard or via API
curl -X POST "https://api.cloudflare.com/client/v4/accounts/<ACCOUNT_ID>/browser" \
  -H "X-Auth-Email: me@jas.life" \
  -H "X-Auth-Key: <API_KEY>"
```

**Error:** `Durable Object namespace not found`

**Solution:**
```fish
# Recreate namespace
wrangler dispatch-namespace create SCREENSHOT_DO

# Update ID in wrangler.toml
```

**Error:** `R2 bucket not found`

**Solution:**
```fish
# Create bucket
wrangler r2 bucket create aesthetic-screenshots

# Or remove R2 binding from wrangler.toml if not using
```

### Runtime Errors

**Error:** `Browser timeout`

**Solution:**
- Increase `BROWSER_TIMEOUT_MS` in wrangler.toml
- Check network connectivity
- Verify aesthetic.computer is accessible

**Error:** `Durable Object not found`

**Solution:**
- Check namespace ID is correct
- Verify migration ran successfully
- Check worker name matches in binding

**Error:** `Rate limit exceeded`

**Solution:**
- Increase `MAX_REQUESTS_PER_MINUTE`
- Implement IP-based throttling
- Check for abuse/spam

### Performance Issues

**Problem:** Slow screenshot generation

**Solutions:**
- Check browser rendering time in logs
- Optimize page load (reduce assets)
- Increase viewport/timeout limits
- Pre-warm Durable Objects

**Problem:** High costs

**Solutions:**
- Review cache hit rate
- Increase cache TTL
- Implement R2 long-term cache
- Reduce browser timeout
- Monitor usage dashboard

---

## 🔄 Rollback Procedure

If issues occur:

### 1. Quick Rollback (Domain)

In Cloudflare Dashboard:
- Workers & Pages > aesthetic-grab
- Domains & Routes
- Remove custom domain temporarily
- Update index.mjs to point back to Netlify

### 2. Full Rollback

```fish
# Revert index.mjs changes
git revert <commit-hash>
git push

# Remove redirects from netlify.toml

# Keep grab worker for testing/debugging
```

### 3. Emergency Fallback

Add to index.mjs:
```javascript
const useGrabWorker = false; // Emergency kill switch

const icon = useGrabWorker
  ? `https://grab.aesthetic.computer/icon/128x128/${location}.png`
  : pixelPerfect(baseURL, parsed?.host, `/icon/128x128/${location}.png`, meta, "https:");
```

---

## 📝 Deployment Checklist

- [ ] Authenticated with Cloudflare
- [ ] Browser Rendering enabled
- [ ] Browser binding created
- [ ] Durable Object namespace created
- [ ] R2 bucket created (optional)
- [ ] wrangler.toml configured
- [ ] Secrets set (if needed)
- [ ] Local testing passed
- [ ] TypeScript check passed
- [ ] Deployed to production
- [ ] Custom domain configured
- [ ] Domain DNS propagated (check with `dig`)
- [ ] All resolutions tested
- [ ] Cache behavior verified
- [ ] index.mjs updated
- [ ] Redirects added (optional)
- [ ] Changes committed and pushed
- [ ] Monitoring set up
- [ ] Alerts configured
- [ ] Documentation updated
- [ ] Team notified

---

## 🎯 Success Criteria

After deployment, verify:

✅ All three resolutions work
✅ Cache hit rate > 80%
✅ P95 latency < 5s
✅ Error rate < 1%
✅ Browser rendering cost reasonable
✅ No 500 errors in logs
✅ Custom domain works
✅ ETag caching works
✅ CDN caching works

---

## 📞 Support

For issues:
1. Check logs: `npm run logs:production`
2. Review Cloudflare Dashboard metrics
3. Check GitHub issues
4. Contact Cloudflare support if infrastructure issues

---

**Status:** 📝 Ready for Deployment  
**Last Updated:** 2025-01-09
