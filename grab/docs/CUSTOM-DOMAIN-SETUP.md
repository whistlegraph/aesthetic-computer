# 🌐 Custom Domain Setup - Quick Reference

## Two Methods Available

### Method 1: DNS Settings (Faster! ⚡)

**Time:** ~2 minutes  
**Link:** https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records

#### Steps:
1. Open DNS settings
2. Click **"Add record"**
3. Fill in:
   - Type: `CNAME`
   - Name: `grab`
   - Target: `aesthetic-grab.aesthetic-computer.workers.dev`
   - Proxy: **Enabled** (orange cloud ☁️)
4. Click **Save**
5. Wait ~30 seconds
6. Test: `curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"`

✅ Done!

---

### Method 2: Workers Dashboard

**Time:** ~5 minutes  
**Link:** https://dash.cloudflare.com/

### Method 2: Workers Dashboard

**Time:** ~5 minutes  
**Link:** https://dash.cloudflare.com/

#### Steps:
1. Go to **Workers & Pages**
2. Select **aesthetic-grab**
3. Click **Settings** → **Domains & Routes**
4. Click **"Add Custom Domain"**
5. Enter: `grab.aesthetic.computer`
6. Save and wait 2-5 minutes

---

## Which Method to Use?

**Use DNS Method (Method 1) if:**
- ✅ You want it done faster (30 seconds vs 2-5 minutes)
- ✅ You prefer direct control over DNS
- ✅ You're comfortable with DNS records

**Use Workers Dashboard (Method 2) if:**
- ✅ You prefer automated setup
- ✅ You don't want to touch DNS directly
- ✅ You're following the feed deployment pattern exactly

**Both create the same result!** Choose whichever you prefer.

---

## What Gets Created

Either method creates this DNS record:
```
Type:    CNAME
Name:    grab.aesthetic.computer
Target:  aesthetic-grab.aesthetic-computer.workers.dev
Proxy:   Enabled (orange cloud)
```

---

## Verification
```fish
# Test the custom domain
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"

# Expected response:
# HTTP/2 200
# content-type: image/png
# cache-control: public, max-age=3600
```

✅ If you see `200 OK`, the setup is complete!

---

## What Happens Automatically

### DNS Configuration
Cloudflare creates:
```
grab.aesthetic.computer → aesthetic-grab.aesthetic-computer.workers.dev (CNAME)
```

### SSL/TLS Certificate
- Automatic issuance via Let's Encrypt
- Full (strict) encryption mode
- HTTP → HTTPS redirect enabled

### Routing
- All requests to `grab.aesthetic.computer/*` → worker
- Edge routing (no origin server needed)
- Global CDN distribution

---

## Troubleshooting

### Domain Not Resolving
**Issue:** `curl: (6) Could not resolve host: grab.aesthetic.computer`

**Solution:** Wait a few more minutes for DNS propagation

**Check DNS:**
```fish
dig grab.aesthetic.computer
nslookup grab.aesthetic.computer
```

### SSL Certificate Error
**Issue:** Certificate mismatch or "not secure" warning

**Solution:** 
1. Wait for certificate provisioning (can take 5-10 minutes)
2. Force refresh: Clear browser cache
3. Check Cloudflare SSL/TLS settings

### 404 Not Found
**Issue:** Domain resolves but returns 404

**Solution:**
1. Verify worker is deployed: `wrangler deployments list`
2. Check custom domain in Dashboard is pointing to correct worker
3. Try workers.dev URL to verify worker is running

---

## After Setup Complete

### Update PLAN.md
Mark custom domain task as complete:
```markdown
- [x] **Configure Custom Domain** ✅
  - Domain: grab.aesthetic.computer
  - SSL: Active
  - DNS: Propagated
```

### Test Production Integration
```fish
# Test on live site
curl -I "https://aesthetic.computer/icon/128x128/prompt.png"

# Should redirect to:
# https://grab.aesthetic.computer/icon/128x128/prompt.png

# Test og:image
curl -s "https://aesthetic.computer/prompt" | grep "og:image"
# Should show: content="https://grab.aesthetic.computer/preview/1200x630/prompt.png"
```

### Monitor Logs
```fish
cd /workspaces/aesthetic-computer/grab
npx wrangler tail
```

---

## Reference URLs

### Before Custom Domain Setup
- Worker: `https://aesthetic-grab.aesthetic-computer.workers.dev`
- Parse.mjs expects: `https://grab.aesthetic.computer` (hardcoded)
- **Status:** Worker works but URLs don't match

### After Custom Domain Setup
- Worker: `https://grab.aesthetic.computer`
- Parse.mjs expects: `https://grab.aesthetic.computer`
- **Status:** ✅ Everything matches and works!

---

## Dashboard Quick Links

**Main Dashboard:**
https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca

**Worker Settings:**
https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/workers/services/view/aesthetic-grab/production/settings

**Domains & Routes:**
https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/workers/services/view/aesthetic-grab/production/settings/domains

---

## Completion Checklist

- [ ] Logged into Cloudflare Dashboard
- [ ] Navigated to aesthetic-grab worker
- [ ] Added custom domain `grab.aesthetic.computer`
- [ ] Waited for DNS propagation
- [ ] Verified with curl command
- [ ] Tested production integration
- [ ] Updated PLAN.md
- [ ] Monitoring logs for errors

**Estimated Total Time:** 5-10 minutes (including DNS propagation)
