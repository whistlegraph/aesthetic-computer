# 🌐 Custom Domain Setup via DNS (Simpler Method!)

## Method 1: Direct DNS Configuration (Recommended)

Instead of going through the Workers dashboard, you can just add a CNAME record in your Cloudflare DNS settings.

---

## Quick Setup (2 minutes)

### Step 1: Open DNS Settings
🔗 **Direct link:** https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records

Or navigate manually:
1. Go to https://dash.cloudflare.com/
2. Select **aesthetic.computer** zone
3. Click **DNS** → **Records**

### Step 2: Add CNAME Record

Click **"Add record"** button and enter:

| Field | Value |
|-------|-------|
| **Type** | CNAME |
| **Name** | `grab` (will become grab.aesthetic.computer) |
| **Target** | `aesthetic-grab.aesthetic-computer.workers.dev` |
| **Proxy status** | ✅ Proxied (orange cloud) |
| **TTL** | Auto |

Click **Save**

### Step 3: Verify (wait ~30 seconds)

```fish
# Check DNS resolution
dig grab.aesthetic.computer

# Test the endpoint
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"
```

Expected response:
```
HTTP/2 200
content-type: image/png
cache-control: public, max-age=3600
```

✅ **Done!** That's it!

---

## Why This Works

### CNAME Record Routing
```
User Request:
https://grab.aesthetic.computer/icon/128x128/prompt.png
         ↓
Cloudflare DNS resolves CNAME:
grab.aesthetic.computer → aesthetic-grab.aesthetic-computer.workers.dev
         ↓
Cloudflare routes through proxy (orange cloud):
Edge worker intercepts and serves request
         ↓
Response:
PNG image data
```

### Benefits of Proxied CNAME
- ✅ Automatic SSL/TLS (Cloudflare handles certificates)
- ✅ DDoS protection
- ✅ CDN caching
- ✅ Edge routing
- ✅ No origin server needed

---

## Alternative Method 2: Workers Dashboard

If you prefer the Workers UI:

1. Go to: https://dash.cloudflare.com/
2. Navigate to: **Workers & Pages** → **aesthetic-grab** → **Settings** → **Domains & Routes**
3. Click: **"Add Custom Domain"**
4. Enter: `grab.aesthetic.computer`
5. Save

This does the same thing automatically (creates the CNAME record for you).

---

## Comparison: DNS vs Workers Dashboard

| Approach | DNS Settings | Workers Dashboard |
|----------|--------------|-------------------|
| **Speed** | ~30 seconds | ~2-5 minutes |
| **Control** | Full DNS control | Automated |
| **Steps** | Manual CNAME | Click button |
| **Result** | Same! | Same! |
| **Recommended** | ✅ Yes (faster, more control) | Also works |

Both methods create the same DNS record and work identically!

---

## Troubleshooting

### DNS Record Not Showing Up
**Wait:** 30 seconds, then refresh the DNS records page

**Check:**
```fish
dig grab.aesthetic.computer
```

Should show:
```
grab.aesthetic.computer. 300 IN CNAME aesthetic-grab.aesthetic-computer.workers.dev.
```

### Certificate Error
**Wait:** 1-2 minutes for Cloudflare to provision SSL

**Check proxy status:** Must be **Proxied** (orange cloud ☁️), not **DNS only** (gray cloud)

### Still Getting 404
**Verify worker is deployed:**
```fish
cd /workspaces/aesthetic-computer/grab
npx wrangler deployments list
```

**Test workers.dev URL directly:**
```fish
curl -I "https://aesthetic-grab.aesthetic-computer.workers.dev/icon/128x128/prompt.png"
```

If workers.dev works but custom domain doesn't, DNS record may be incorrect.

---

## Current DNS Records Reference

Your aesthetic.computer DNS likely has records like:

```
aesthetic.computer     → A record or CNAME to main site
www.aesthetic.computer → CNAME to aesthetic.computer
feed.aesthetic.computer → CNAME to feed worker (if you have this)
grab.aesthetic.computer → CNAME to aesthetic-grab.aesthetic-computer.workers.dev (ADD THIS!)
```

---

## Complete Terminal Commands

```fish
# Add CNAME via Cloudflare API (if you prefer CLI)
curl -X POST "https://api.cloudflare.com/client/v4/zones/a23b54e8877a833a1cf8db7765bce3ca/dns_records" \
  -H "Authorization: Bearer YOUR_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{
    "type": "CNAME",
    "name": "grab",
    "content": "aesthetic-grab.aesthetic-computer.workers.dev",
    "proxied": true
  }'

# Or just use the dashboard - it's easier! 😄
```

---

## After Setup

### Update PLAN.md
Mark as complete:
```markdown
- [x] **Configure Custom Domain** ✅ CNAME added via DNS
```

### Test Production
```fish
# Test custom domain
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"

# Test on live site
curl -s "https://aesthetic.computer/prompt" | grep "og:image"
```

### Monitor
```fish
cd /workspaces/aesthetic-computer/grab
npx wrangler tail
```

---

## Summary

**Recommended Approach:**
1. Go to DNS settings: https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records
2. Add CNAME: `grab` → `aesthetic-grab.aesthetic-computer.workers.dev` (Proxied)
3. Wait 30 seconds
4. Test: `curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"`
5. ✅ Done!

**Total time:** ~2 minutes (vs 5-10 minutes with Workers dashboard method)

You have full control over your DNS and it's faster! 🚀
