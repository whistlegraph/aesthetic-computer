# DigitalOcean Account Suspension Audit

**Date:** 2026-03-03
**Account:** me+do@jas.life
**Status:** `locked` (suspended)
**Cause:** Unpaid balance — **$401.20 overdue** + $11.14 current month = **$412.34 total owed**

---

## Billing Summary

### Current Balance

| | Amount |
|---|--------|
| Account Balance (overdue) | **$401.20** |
| March 2026 MTD (2 days) | $11.14 |
| **Total Owed** | **$412.34** |

### Monthly Cost Trend

| Month | Amount | Notes |
|-------|--------|-------|
| Mar 2026 | $11.14 | 2 days only (suspended) |
| Feb 2026 | $132.36 | Oven upgraded to 4vcpu-8gb mid-month |
| Jan 2026 | $110.40 | |
| Dec 2025 | $182.40 | |
| Nov 2025 | $154.74 | Costs started climbing |
| Oct 2025 | $71.30 | |
| Sep 2025 | $62.40 | Baseline |
| ... | ~$62/mo | Stable from mid-2023 through Sep 2025 |

The account ran at ~$62/mo for over two years, then costs roughly doubled starting Nov 2025 with new droplets (silo, help, AT/PDS) and the oven upgrade.

### February 2026 Breakdown (most recent full month)

| Resource | Size | Monthly Cost |
|----------|------|-------------|
| **oven-aesthetic-computer** | s-4vcpu-8gb (upgraded mid-Feb) | ~$34 ($8 at old size + $26 at new) |
| **help-aesthetic-computer** | s-2vcpu-4gb (recreated mid-Feb) | ~$24 |
| **legacy-2016** | 1GB | $12 |
| **Managed Redis (Valkey)** | Basic 1GB/1vCPU | $15 |
| **legacy-2016-volume** | 150GB | $15 |
| **session-server-monolith** | s-1vcpu-1gb-amd | $7 |
| **silo-aesthetic-computer** | s-2vcpu-4gb (resized mid-Feb) | ~$6.60 |
| **aesthetic-computer-at** | s-1vcpu-1gb | $6 |
| **nada-banners** | 512MB (destroyed Feb 24) | $5.10 |
| **Spaces Subscription** | 250GB storage + 1TB bandwidth | $5 |
| **legacy-2016 Backups** | Weekly | $2.40 |
| App Platform (2 starter apps) | Free tier | $0 |
| Spaces Bandwidth | Within allowance | $0 |
| **Total** | | **$132.36** |

---

## All Droplets (Live from API)

All 6 droplets currently have status: **off** (suspended).

| ID | Name | IP | Size | RAM | vCPUs | Disk | Region | Monthly Est. |
|----|------|-----|------|-----|-------|------|--------|-------------|
| 526938082 | **oven-aesthetic-computer** | 137.184.237.166 | s-4vcpu-8gb | 8GB | 4 | 60GB | sfo3 | ~$48 |
| 552385566 | **help-aesthetic-computer** | 146.190.150.173 | s-2vcpu-4gb | 4GB | 2 | 80GB | sfo3 | ~$24 |
| 550789310 | **silo-aesthetic-computer** | 64.23.151.169 | s-2vcpu-4gb | 4GB | 2 | 25GB | sfo3 | ~$24 |
| 387426128 | **session-server-monolith** | 157.245.134.225 | s-1vcpu-1gb-amd | 1GB | 1 | 25GB | nyc1 | ~$7 |
| 523576067 | **aesthetic-computer-at** | 165.227.120.137 | s-1vcpu-1gb | 1GB | 1 | 25GB | nyc3 | ~$6 |
| 31600188 | **legacy-2016** | 162.243.163.221 | 1GB | 1GB | 1 | 30GB | nyc1 | ~$12 |

**Estimated total droplet cost: ~$121/mo**

### What each droplet runs

| Droplet | Domain | Process | Purpose |
|---------|--------|---------|---------|
| oven-aesthetic-computer | `oven.aesthetic.computer` | systemd (Express.js) | Screenshot/OG images, MP4 conversion, piece bundling, OS image builds |
| help-aesthetic-computer | `help.aesthetic.computer` | systemd | Help/documentation service |
| silo-aesthetic-computer | `silo.aesthetic.computer` | systemd | MongoDB host, data dashboard, Spaces monitoring |
| session-server-monolith | `session-server.aesthetic.computer` | pm2/nohup (Node.js) | Real-time WebSocket multiplayer, chat |
| aesthetic-computer-at | `at.aesthetic.computer` | ? | Bluesky PDS service |
| legacy-2016 | none | off | Historical archive (Ubuntu 16.04, 150GB volume) |

---

## Other Billed Resources

### Managed Database (Valkey/Redis)

| Name | Size | Region | Monthly | Endpoint |
|------|------|--------|---------|----------|
| db-redis-sfo3-24903 | Basic 1GB / 1 vCPU / 10GB Disk | sfo3 | **$15** | `db-redis-sfo3-24903-do-user-1732565-0.b.db.ondigitalocean.com:25061` |

Used by: session server, silo, chat/nanos

### Block Storage Volume

| Name | Size | Region | Monthly | Attached To |
|------|------|--------|---------|-------------|
| legacy-2016-volume | 150GB | nyc1 | **$15** | legacy-2016 droplet |

### Spaces Object Storage ($5/mo subscription)

| Bucket | Region | Bandwidth (Feb) |
|--------|--------|-----------------|
| **assets-aesthetic-computer** | sfo3 | 267 GiB (heaviest user) |
| **at-blobs-aesthetic-computer** | sfo3 | 30 GiB |
| **art-aesthetic-computer** | sfo3 | 7 GiB |
| **user-aesthetic-computer** | sfo3 | 4.5 GiB |
| **pals-aesthetic-computer** | sfo3 | 2.7 GiB |
| **wand-aesthetic-computer** | sfo3 | 0.1 GiB |
| **pix.nopaint.art** | sfo3 | 0.02 GiB (No Paint project) |

All within the 1TB bandwidth allowance — no overage charges.

### App Platform (Free Tier)

| App | Status |
|-----|--------|
| king-prawn-app | Starter (free) |
| digitpain-com | Starter (free) |

---

## Spaces Credentials (2 key sets)

| Key ID Prefix | Region | Used By |
|---------------|--------|---------|
| `DO00ZQ8F...` (primary) | sfo3 | Oven, paintings, silo, false.work |
| `DO00HC2A...` (secondary) | nyc3 | AT/PDS service |

---

## Impact Assessment: What Breaks

### CRITICAL (user-facing)

| Impact | Service | Detail |
|--------|---------|--------|
| **No OG images / social previews** | Oven | All link previews on Twitter/Discord/iMessage stop working |
| **No tape MP4 conversion** | Oven | Recordings can't be exported as video |
| **No piece bundling** | Oven | `/bundle-html` and `/pack-html` endpoints down — FedAC kiosk builds fail |
| **No multiplayer / chat** | Session Server | All real-time collaboration and chat breaks |
| **No Redis** | Managed Valkey | Session state, real-time coordination fail |
| **No CDN assets** | Spaces CDN | Any media from `*.sfo3.cdn.digitaloceanspaces.com` returns errors |
| **No art uploads** | Spaces | `presigned-url` function can't generate upload URLs |

### MODERATE (degraded)

| Impact | Service | Detail |
|--------|---------|--------|
| Silo dashboard down | Silo droplet | Monitoring/metrics unavailable |
| Painting tools broken | Spaces | `paintings/inspect-spaces.mjs` fails |
| Asset sync broken | Spaces | `npm run assets:sync:down/up` fails |
| AT/PDS down | AT droplet | Bluesky PDS service unreachable |
| Help service down | Help droplet | Documentation service unreachable |

### NOT AFFECTED (hosted elsewhere)

| Service | Host |
|---------|------|
| Main site (aesthetic.computer) | Netlify |
| Serverless functions (100+) | Netlify (but many call DO services) |
| grab.aesthetic.computer | Cloudflare Workers |
| Feed (feed.feralfile.com) | Cloudflare Workers |
| DNS | Cloudflare (A records point to dead IPs) |
| MongoDB Atlas | MongoDB Cloud |
| Firebase Auth | Google |
| Stripe payments | Stripe |

---

## DNS Records Pointing to DO (via Cloudflare)

| Record | Target | Status |
|--------|--------|--------|
| `oven.aesthetic.computer` | 137.184.237.166 | Unreachable |
| `session-server.aesthetic.computer` | 157.245.134.225 | Unreachable |
| `silo.aesthetic.computer` | 64.23.151.169 | Unreachable |
| `help.aesthetic.computer` | 146.190.150.173 | Unreachable |
| `at.aesthetic.computer` | 165.227.120.137 | Unreachable |

---

## Vault Credential Inventory

All credentials in `aesthetic-computer-vault/`:

| File | Contains |
|------|----------|
| `oven/deploy.env` | `DO_TOKEN` (API), Cloudflare keys, droplet config |
| `oven/.env` | Spaces keys (primary), MongoDB connection, OS build admin key |
| `oven/ssh/oven-deploy-key` | SSH private key for 137.184.237.166 |
| `session-server/.env` | Redis endpoint, MongoDB connection |
| `session-server/session_server` | SSH private key for 157.245.134.225 |
| `silo/.env` | MongoDB admin/app creds, Redis, Spaces monitoring |
| `at/deploy.env` | `DO_TOKEN`, PDS droplet config, NYC3 Spaces keys |
| `judge/deploy.env` | `DO_TOKEN`, droplet config |
| `help/deploy.env` | `DO_TOKEN`, droplet config |
| `paintings/.env` | Spaces credentials (sfo3) |
| `home/.ssh/id_rsa` | General SSH key (silo access) |
| `.env` (root) | `ART_KEY`, `ART_SECRET`, etc. |

Single `DO_TOKEN` shared across oven, judge, at, and help deploy configs.

---

## Cost Reduction Opportunities

If restoring the account, here's what could be trimmed:

| Action | Saves/mo | Risk |
|--------|----------|------|
| Destroy **legacy-2016** droplet + 150GB volume | **$27** | Lose historical archive (back up first) |
| Destroy **nada-banners** | Already destroyed Feb 24 | — |
| Downgrade **oven** back to s-2vcpu-2gb | **$36** | May be too slow for OS builds |
| Downgrade **help** to s-1vcpu-1gb | **$18** | Depends on load |
| Evaluate if **AT/PDS** is actively used | **$6** | Bluesky integration gone |
| Switch Redis to Upstash free tier | **$15** | Need to test compatibility |
| **Total possible savings** | **~$102/mo** | Would bring bill from ~$132 to ~$30 |

---

## Recovery Path

### Option A: Pay and restore
1. Pay the $412.34 balance via DO dashboard
2. All droplets should auto-restore to `off` → power on manually
3. Verify all services start, test endpoints
4. Trim unnecessary resources to prevent future accumulation

### Option B: Migrate away
1. **Data rescue first**: Contact DO support to get temporary access for Spaces export
2. Move Oven to Hetzner/Vultr (~$8/mo for equivalent)
3. Move Session Server to Hetzner/Vultr or lean into Jamsocket
4. Switch Redis to Upstash or Railway
5. Migrate Spaces to Cloudflare R2 (free egress) or Backblaze B2
6. Update all DNS, env vars, and deploy scripts

---

*Generated by Claude Code audit — 2026-03-03*
*Data sourced live from DigitalOcean API via `doctl`*
