# Aesthetic Computer — Full Service & Billing Audit

**Date:** 2026-03-23
**Previous audit:** 2026-03-03
**Account holder:** jeffrey (me@jas.life)

---

## Summary

| Status | Service | Est. Monthly | Notes |
|--------|---------|-------------|-------|
| **CRITICAL** | Netlify | $19 | Period ends ~Mar 27 — site at risk |
| **OFFLINE** | Shopify | $0 | shop.aesthetic.computer down |
| **OVERDUE** | GitHub | ~$500 | Prior months (Codespaces/Copilot) |
| Active | DigitalOcean | ~$132 | Paid — all droplets restored |
| Active | MongoDB | $0 | Self-hosted on DO silo — restored |
| Active | Cloudflare | $0 | Free plan, 8 zones |
| Active | Firebase/GCP | $0–$50 | Free tier auth + Nanos VMs (unknown cost) |
| Active | Auth0 | $0 | Free tier |
| Active | Stripe | Per-txn | 2.9% + $0.30 per charge |
| Active | PayPal | Per-txn | ~2.2% + $0.30 per charge |
| Active | Anthropic (Claude) | Usage-based | API key active |
| Active | Jamsocket | $0 | Free/early tier |
| Dormant | Tezos | Per-txn | Gas fees on mint only |
| Unknown | Domain registrations | ~$80–120/yr | 8 domains across registrars |
| **Total recurring** | | **~$151–201/mo** | **Plus ~$519 overdue (GitHub + Netlify)** |

---

## Changes Since Last Audit (Mar 3)

| Change | Detail |
|--------|--------|
| DigitalOcean **restored** | $412.34 paid (thanks Casey), all 6 droplets back online |
| MongoDB **restored** | Self-hosted on DO silo, back with DO |
| Netlify escalated to **CRITICAL** | Period ends ~Mar 27 — only ~4 days remain |
| Shopify **offline** | shop.aesthetic.computer down, /api/shop broken, product carousel dead |
| Overdue adjusted | DO resolved ($412.34), GitHub (~$500) + Netlify ($19) remain = ~$519 |

---

## 1. Netlify — CRITICAL

**Plan:** Pro (legacy-orb-pro)
**Billing:** $19/mo per member (1 member)
**Status:** `past_due` — **period ends ~Mar 27, site at risk**
**Billing email:** me@jas.life
**Payment:** Stripe (has payment method on file)
**Current period:** Feb 27 – Mar 27, 2026

**Impact if disabled:** aesthetic.computer, sotce.net, botce.ac, notepat.com, kidlisp.com, wipppps.world ALL go down. All serverless functions (100+) stop. Edge functions stop. This is the most critical service.

**Action needed:** Fix payment IMMEDIATELY.

---

## 2. Shopify — OFFLINE

**Store:** shop.aesthetic.computer
**Status:** Offline / store closed
**Impact:**
- Product pages at shop.aesthetic.computer return errors
- `/api/shop` function fails (fetches from Shopify Admin API)
- Product carousel on main site broken
- All shop redirects in netlify.toml point to dead store

**Integration points:**
- `system/netlify/functions/shop.mjs` — fetches products via Admin API
- `system/public/aesthetic.computer/disks/common/products.mjs` — carousel component
- `ac-shop/` — CLI management tools
- Redis cache (10-min TTL) may serve stale data briefly

**Action needed:** Decide whether to reactivate Shopify or remove shop integration.

---

## 3. DigitalOcean — ACTIVE (restored)

**Status:** Paid. All 6 droplets back online.
**Monthly cost:** ~$132

| Resource | Monthly Cost |
|----------|-------------|
| 6 Droplets (all running) | ~$121 |
| Managed Redis/Valkey | $15 |
| 150GB Block Volume | $15 |
| Spaces Subscription | $5 |
| **Total** | **~$132** |

See [digitalocean-2026-03-03.md](digitalocean-2026-03-03.md) for full droplet breakdown (unchanged).

---

## 4. GitHub — OVERDUE (~$500)

**User:** whistlegraph (Free plan)
**Org:** justanothersystem (Free plan, Copilot Business configured, 4 seats)
**Status:** ~$500 unpaid from prior months
**Monthly cost:** Currently $0 (no active paid features)

Check billing: https://github.com/organizations/justanothersystem/billing/history

---

## 5. Cloudflare — FREE

**Account:** me@jas.life
**Plan:** Free Website (all zones)
**Zones:** 8 domains
**Workers:** 2 (grab, dp1-feed)
**Monthly cost: $0**

---

## 6. MongoDB — ACTIVE (self-hosted)

**Previous:** MongoDB Atlas (suspended)
**Current:** Self-hosted on Silo droplet (`silo.aesthetic.computer:27017`)
**Monthly cost:** $0 standalone (included in DO silo droplet cost)

---

## 7–13. Unchanged Services

| # | Service | Status | Monthly | Notes |
|---|---------|--------|---------|-------|
| 7 | Firebase/GCP | Active | $0–50 | Free auth + unknown Nanos VMs |
| 8 | Auth0 | Free | $0 | Free tier (7K users) |
| 9 | Stripe | Active | Per-txn | 2.9% + $0.30 |
| 10 | PayPal | Active | Per-txn | ~2.2% + $0.30 |
| 11 | Anthropic | Active | Usage | Claude Code API |
| 12 | Jamsocket | Free | $0 | Session orchestration |
| 13 | Tezos | Dormant | Gas fees | Keeps minting only |
| 14 | Domains (8) | Active | ~$8/mo | ~$100/yr across registrars |

---

## Priority Actions

### Urgent — Before April

1. **Netlify payment** — Fix NOW. Period ends ~Mar 27. Everything goes down.
2. **Shopify** — Decide: reactivate store or remove integration from codebase.
3. **GitHub ~$500** — Pay at billing history page.

### April Tasks

4. **GCP/Nanos billing** — Log into console.cloud.google.com, check if chat VMs running.
5. **Domain renewals** — Check expiration dates for 8 domains.
6. **Anthropic spend** — Check console.anthropic.com for monthly burn rate.
7. **Cost reduction** — Execute plan: legacy-2016 + Redis + downgrades (~$107/mo savings possible).

### Cost Reduction Opportunities (unchanged)

| Action | Monthly Savings |
|--------|----------------|
| Kill legacy-2016 droplet + 150GB volume | $27 |
| Downgrade oven → s-2vcpu-2gb | $36 |
| Downgrade help → s-1vcpu-1gb | $18 |
| Redis → Upstash free tier | $15 |
| Evaluate AT/PDS droplet | $6 |
| Consolidate domains | $5–10 |
| **Total possible** | **~$107–112/mo** |

Could bring monthly from ~$151 down to ~$44.

---

*Updated by Claude Code — 2026-03-23*
*Previous audit: [all-services-2026-03-03.md](all-services-2026-03-03.md)*
