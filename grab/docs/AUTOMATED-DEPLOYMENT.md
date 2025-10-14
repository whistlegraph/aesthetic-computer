# 🤖 Automated Deployment - Quick Reference

## Overview

The grab worker now supports **fully automated deployment** with DNS configuration, matching the pattern used in other aesthetic.computer services (nanos, dark-window, etc.).

---

## One-Command Deployment

```fish
cd /workspaces/aesthetic-computer/grab
./scripts/deploy-with-dns.fish
```

**What it does:**
1. ✅ Deploys worker to Cloudflare Workers
2. ✅ Creates/updates CNAME record via Cloudflare API
3. ✅ Waits for DNS propagation (~30-60 seconds)
4. ✅ Verifies deployment is accessible
5. ✅ Provides test commands and summary

**Time:** ~2-3 minutes total (including DNS propagation)

---

## Prerequisites

### 1. Get Cloudflare API Token

**One-time setup** (1 minute):

1. Go to: https://dash.cloudflare.com/profile/api-tokens
2. Click: **"Create Token"**
3. Use template: **"Edit zone DNS"**
4. Configure permissions:
   - Zone → DNS → Edit
   - Zone Resources → Include → Specific zone → aesthetic.computer
5. Click: **"Continue to summary"** → **"Create Token"**
6. Copy the token

### 2. Add to Vault

Edit `/aesthetic-computer-vault/grab/.env`:

```bash
CLOUDFLARE_EMAIL=me@jas.life
CLOUDFLARE_API_TOKEN=your-token-here
```

**Note:** `CLOUDFLARE_API_TOKEN` (scoped DNS token) is different from `CLOUDFLARE_API_KEY` (global API key). We use the scoped token for security.

---

## How It Works

### Deployment Flow

```
1. Deploy Worker
   ↓
2. Check for existing DNS record
   GET /zones/{zone_id}/dns_records?type=CNAME&name=grab.aesthetic.computer
   ↓
3a. Record exists → Update
    PUT /zones/{zone_id}/dns_records/{record_id}
   OR
3b. Record doesn't exist → Create
    POST /zones/{zone_id}/dns_records
   ↓
4. Wait for DNS propagation
   Poll: curl https://grab.aesthetic.computer/icon/128x128/prompt.png
   Retry: Every 5 seconds for up to 60 seconds
   ↓
5. Success!
   Display URLs and test commands
```

### DNS Record Created

```json
{
  "type": "CNAME",
  "name": "grab",
  "content": "aesthetic-grab.aesthetic-computer.workers.dev",
  "ttl": 1,
  "proxied": true
}
```

Result: `grab.aesthetic.computer` → `aesthetic-grab.aesthetic-computer.workers.dev`

---

## Script Output Example

```
🚀 Deploying grab worker with automatic DNS setup...

✅ Loaded environment variables from vault

📦 Step 1: Deploying worker to Cloudflare...
Total Upload: 684.44 KiB / gzip: 183.18 KiB
Uploaded aesthetic-grab (2.34 sec)
Published aesthetic-grab (0.19 sec)
  https://aesthetic-grab.aesthetic-computer.workers.dev
Current Deployment ID: 511d31bc-c2e8-4f00-aea7-9b9d95e08733

✅ Worker deployed successfully!

🌐 Step 2: Configuring DNS (CNAME record)...
   Zone: aesthetic.computer (a23b54e8877a833a1cf8db7765bce3ca)
   Creating: grab.aesthetic.computer → aesthetic-grab.aesthetic-computer.workers.dev

   Checking for existing DNS record...
   🟡 Creating new CNAME record...
   ✅ DNS record configured successfully!

⏳ Step 3: Waiting for DNS propagation...
   This usually takes 30-60 seconds...

   Attempt 1/12: ⏳ Waiting... (HTTP 000)
   Attempt 2/12: ⏳ Waiting... (HTTP 000)
   Attempt 3/12: ✅ DNS working!

✨ Deployment complete!

📊 Deployment Summary:
   Worker URL:  https://aesthetic-grab.aesthetic-computer.workers.dev
   Custom URL:  https://grab.aesthetic.computer
   DNS Record:  grab.aesthetic.computer → aesthetic-grab.aesthetic-computer.workers.dev (CNAME, Proxied)

🧪 Test Commands:
   curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"
   curl -I "https://grab.aesthetic.computer/preview/1200x630/prompt.png"

📋 Next Steps:
   1. Test screenshot generation with various pieces
   2. Monitor logs: npx wrangler tail
   3. Check Cloudflare dashboard for errors
   4. Validate production integration on aesthetic.computer
```

---

## Comparison: Manual vs Automated

| Method | Time | Steps | Automation |
|--------|------|-------|------------|
| **Manual Dashboard** | ~5-10 min | 1. Deploy worker<br>2. Open Dashboard<br>3. Navigate to DNS<br>4. Add record<br>5. Wait<br>6. Test | ❌ None |
| **Manual API** | ~3-5 min | 1. Deploy worker<br>2. Run curl command<br>3. Wait<br>4. Test | ⚠️ Partial |
| **Automated Script** | ~2-3 min | 1. Run script<br>2. ✅ Done! | ✅ Full |

---

## Troubleshooting

### Script Fails: "CLOUDFLARE_EMAIL not set"

**Solution:** Add credentials to vault/.env:
```fish
cd /workspaces/aesthetic-computer/aesthetic-computer-vault/grab
nano .env  # Add CLOUDFLARE_EMAIL and CLOUDFLARE_API_TOKEN
```

### Script Fails: "DNS configuration failed"

**Possible causes:**
1. API token doesn't have DNS edit permission
2. API token is for wrong zone
3. Network issue

**Solution:** Check API token permissions or use manual DNS method:
```fish
# Open dashboard
open "https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records"

# Manually add:
# Type: CNAME
# Name: grab
# Target: aesthetic-grab.aesthetic-computer.workers.dev
# Proxy: Enabled
```

### DNS Not Propagating

**Solution:** Wait longer (up to 5 minutes) or check manually:
```fish
dig grab.aesthetic.computer
curl -I "https://grab.aesthetic.computer/icon/128x128/prompt.png"
```

---

## Pattern Used Across Aesthetic Computer

This automated deployment pattern matches other services:

- **nanos/conductor.mjs** - Deploys GCP instances + updates DNS
- **dark-window/** - Similar DNS automation
- **feed/** - Manual DNS (could be automated with this pattern)
- **grab/** - ✅ Now fully automated!

### Benefits

1. ✅ **Consistency** - Same pattern across all services
2. ✅ **Speed** - Deployment + DNS in one command
3. ✅ **Reliability** - Automated verification
4. ✅ **Developer Experience** - No context switching to Dashboard
5. ✅ **CI/CD Ready** - Can be used in automation pipelines

---

## Alternative: Manual Methods

If you prefer manual control or the script fails:

### Option 1: DNS Dashboard (2 minutes)
```
https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/dns/records
```

### Option 2: Workers Dashboard (5 minutes)
```
Workers & Pages → aesthetic-grab → Settings → Domains & Routes
```

### Option 3: API with curl
```fish
curl -X POST "https://api.cloudflare.com/client/v4/zones/a23b54e8877a833a1cf8db7765bce3ca/dns_records" \
  -H "X-Auth-Email: $CLOUDFLARE_EMAIL" \
  -H "X-Auth-Key: $CLOUDFLARE_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data '{
    "type": "CNAME",
    "name": "grab",
    "content": "aesthetic-grab.aesthetic-computer.workers.dev",
    "proxied": true
  }'
```

All methods produce the same result!

---

## Security Notes

### API Token vs API Key

- **API Token** (scoped, recommended): Limited to DNS editing only
- **API Key** (global, avoid if possible): Full account access

The script uses API Token for better security.

### Secrets Storage

All secrets stored in `/aesthetic-computer-vault/grab/.env`:
- ✅ In gitignore
- ✅ Not committed to repo
- ✅ Separate from code

---

## Summary

**Before:** Manual DNS setup via Dashboard (~5-10 minutes)  
**After:** One command automated deployment (~2-3 minutes)

```fish
./scripts/deploy-with-dns.fish
```

That's it! 🚀
