# Aesthetic Computer — Full Service & Billing Audit

**Date:** 2026-03-03
**Account holder:** jeffrey (me@jas.life)

---

## Summary

| Status | Service | Est. Monthly | Notes |
|--------|---------|-------------|-------|
| **SUSPENDED** | DigitalOcean | ~$132 | $412.34 overdue — account locked |
| **PAST DUE** | Netlify | $19 | 25 days until disabled |
| Active | Cloudflare | $0 | Free plan, 8 zones |
| **OVERDUE** | GitHub | $0 now | ~$500 overdue from prior months |
| **SUSPENDED** | MongoDB (was Atlas) | $0 | Migrated to DO silo — also down |
| Active | Firebase/GCP | $0–$50 | Free tier auth + Nanos VMs (unknown cost) |
| Active | Auth0 | $0 | Free tier |
| Active | Stripe | Per-txn | 2.9% + $0.30 per charge |
| Active | PayPal | Per-txn | ~2.2% + $0.30 per charge |
| Active | Anthropic (Claude) | Usage-based | API key active |
| Active | Jamsocket | $0 | Free/early tier |
| Dormant | Tezos | Per-txn | Gas fees on mint only |
| Unknown | Domain registrations | ~$80–120/yr | 8 domains across registrars |
| **Total recurring** | | **~$151–201/mo** | **Plus ~$912 overdue (DO + GitHub)** |

---

## 1. DigitalOcean — SUSPENDED

**Status:** Account locked. $412.34 total owed ($401.20 overdue + $11.14 MTD).

See [digitalocean-2026-03-03.md](digitalocean-2026-03-03.md) for full breakdown.

| Resource | Monthly Cost |
|----------|-------------|
| 6 Droplets (all off) | ~$121 |
| Managed Redis/Valkey | $15 |
| 150GB Block Volume | $15 |
| Spaces Subscription | $5 |
| **Total** | **~$132** (Feb was $132.36) |

**Action needed:** Pay $412.34 to unlock, or plan migration.

---

## 2. Netlify — PAST DUE

**Plan:** Pro (legacy-orb-pro)
**Billing:** $19/mo per member (1 member)
**Status:** `past_due` — **25 days until site disabled** (as of Mar 3)
**Billing email:** me@jas.life
**Payment:** Stripe (has payment method on file)
**Current period:** Feb 27 – Mar 27, 2026

| Included | Limit | Used |
|----------|-------|------|
| Bandwidth | 1 TB | — |
| Build minutes | 25,000 | — |
| Concurrent builds | 3 | — |
| Serverless functions | 125,000 invocations | — |
| Edge functions | 2,000,000 invocations | — |
| Forms | 100 submissions | — |
| Collaborators | 2 (1 used) | — |

**Sites hosted:** aesthetic.computer (+ custom domains: sotce.net, botce.ac, notepat.com, kidlisp.com, wipppps.world)

**Action needed:** Fix payment method to avoid site going down in ~25 days.

---

## 3. GitHub — OVERDUE (~$500)

**User:** whistlegraph (Free plan)
**Org:** justanothersystem (Free plan, Copilot Business configured, 4 seats)
**Status:** ~$500 unpaid from prior months (billing portal only — API doesn't expose invoices)
**Copilot:** Business plan configured on org (0 active seats currently)
**Codespaces:** None active (running local devcontainer)
**Actions:** Within free tier
**Sponsorships:** None

**Note:** GitHub's billing API does not expose invoice history or outstanding balance.
The ~$500 overdue likely accumulated from prior Codespace usage or Copilot seats.
Check billing at: https://github.com/organizations/justanothersystem/billing/history

**App Platform (on DigitalOcean — see above):**
- king-prawn-app (starter, free)
- digitpain-com (starter, free)

**Monthly cost:** Currently $0 (no active paid features), but **~$500 overdue**

---

## 4. Cloudflare — FREE

**Account:** me@jas.life (a23b54e8877a833a1cf8db7765bce3ca)
**Plan:** Free Website (all zones)

### Zones (8 domains)

| Domain | Registrar | Plan |
|--------|-----------|------|
| aesthetic.computer | Tucows (via Porkbun or similar) | Free |
| kidlisp.com | Key-Systems | Free |
| notepat.com | Key-Systems | Free |
| sotce.net | 1API GmbH | Free |
| prompt.ac | 1API GmbH | Free |
| jas.life | 1API GmbH | Free |
| justanothersystem.org | 1API GmbH | Free |
| false.work | GoDaddy | Free |

### Workers

| Worker | Purpose | Cost |
|--------|---------|------|
| grab.aesthetic.computer | Browser rendering / screenshots | Free tier (100K req/day) |
| dp1-feed (feed.feralfile.com) | Activity feed API | Free tier |

**Monthly cost: $0** (within free tier limits)

---

## 5. Domain Registrations

8 domains across multiple registrars. Typical annual renewal costs:

| Domain | Registrar | Est. Annual |
|--------|-----------|-------------|
| aesthetic.computer | Tucows | ~$35 (.computer TLD) |
| kidlisp.com | Key-Systems | ~$12 |
| notepat.com | Key-Systems | ~$12 |
| sotce.net | 1API | ~$12 |
| prompt.ac | 1API | ~$15 (.ac TLD) |
| jas.life | 1API | ~$12 |
| justanothersystem.org | 1API | ~$12 |
| false.work | GoDaddy | ~$12 |

**Est. total: ~$80–120/year ($7–10/mo averaged)**

Note: botce.ac and wipppps.world are referenced in netlify.toml but not in Cloudflare — may be registered elsewhere or expired.

---

## 6. MongoDB — MIGRATED TO DO (was Atlas, now self-hosted)

**Previous:** MongoDB Atlas (suspended due to billing)
**Current:** Self-hosted on Silo droplet (`silo.aesthetic.computer:27017`)
**Database:** aesthetic
**Collections:** handles, paintings, kidlisp, chat-messages, moods, openers, keeps, secrets, oven-os-builds, etc.
**Connection:** All services now point to `silo.aesthetic.computer:27017` (vault `.env` files)

**Monthly cost:** $0 standalone (cost is part of Silo droplet on DO — currently suspended)

**Risk:** With DO suspended, the MongoDB instance on silo is also down. All database-dependent features (auth, chat, paintings, keeps, etc.) are broken.

---

## 7. Firebase / Google Cloud Platform

**Firebase:** Free tier (Spark plan)
- Authentication (Auth0 is primary, Firebase secondary)
- Cloud Messaging (FCM) for push notifications
- Service account keys in vault: `gcp-firebase-service-key.json`

**GCP Compute (Nanos):**
- Service account: `gcp-service-key.json` in vault
- Used for: NanosVM unikernel chat instances (chat-system, chat-sotce, chat-clock)
- No gcloud auth configured in this environment — **cannot check current billing**
- Could be $0 if instances are stopped, or $20–50/mo if running

**Monthly cost: $0 (Firebase) + unknown (GCP compute)**

**Action needed:** Log into GCP console to check if Nanos VMs are still running and accruing charges.

---

## 8. Auth0 — FREE

**Domain:** aesthetic.us.auth0.com
**Custom domain:** hi.aesthetic.computer
**Tier:** Free (up to 7,000 active users)
**Used for:** Primary user authentication

**Monthly cost: $0**

---

## 9. Stripe — PER TRANSACTION

**Account:** Production + test keys configured
**Also:** Separate Stripe account for sotce.net
**Fee:** 2.9% + $0.30 per successful charge
**Webhook:** Stripe CLI installed (v1.30.0), listener configured in dev

**Monthly cost:** Depends on transaction volume. No fixed fee.

---

## 10. PayPal — PER TRANSACTION

**Account:** mail@aesthetic.computer (Live)
**API:** REST API credentials in vault
**Endpoint:** https://api-m.paypal.com
**Fee:** ~2.2% + $0.30 per transaction

**Monthly cost:** Depends on transaction volume. No fixed fee.

---

## 11. Anthropic (Claude) — USAGE BASED

**API key:** Active in environment (`ANTHROPIC_API_KEY`)
**Used for:** Claude Code (this tool), potentially other AI features
**Pricing:** Per-token (varies by model)

**Monthly cost:** Usage-based. Check console.anthropic.com for actual spend.

---

## 12. Jamsocket — FREE

**Used for:** Session server container orchestration
**Status:** Free/early adopter tier
**Integration:** session-server Dockerfile, npm scripts

**Monthly cost: $0**

---

## 13. Tezos — PER TRANSACTION

**Networks:** Mainnet + staging
**Contracts:** keeps_fa2_v5.py (FA2 token standard)
**Wallets:** Configured in vault (`tezos/aesthetic/.env`, `tezos/kidlisp/.env`, `tezos/staging/.env`)
**Used for:** KidLisp digital art minting (keeps)

**Monthly cost:** Gas fees only when minting. Typically negligible (~$0.01–0.05 per operation).

---

## 14. Social Platform APIs (Free Tier)

| Service | Purpose | Cost |
|---------|---------|------|
| Facebook/Meta | App ID for social login | $0 |
| Instagram | oEmbed, Graph API | $0 |
| Threads | API integration | $0 |
| Bluesky/ATProto | Custom PDS, social sync | $0 (self-hosted on DO) |

---

## 15. Other Vault Services (Low/No Cost)

| Service | Purpose | Est. Cost |
|---------|---------|-----------|
| AWS (credentials in vault) | S3 CLI for DO Spaces compat | $0 (just CLI) |
| GoDaddy (thomaslawson.com gig) | Client domain management | $0 (client pays) |
| Pinata (IPFS pinning) | IPFS storage for art | $0 (free tier) |

---

## Priority Actions

### Urgent (this week)

1. **Netlify is past due** — Fix payment to avoid aesthetic.computer going down in 25 days
2. **DigitalOcean owes $412.34** — Pay or plan migration (all DO services are off, including MongoDB)
3. **GitHub owes ~$500** — Pay at https://github.com/organizations/justanothersystem/billing/history

### Check Soon

3. **GCP/Nanos billing** — Log into console.cloud.google.com to verify if chat VMs are running
4. **Domain renewals** — Check upcoming expiration dates for 8 domains
5. **Anthropic API spend** — Check console.anthropic.com for monthly burn rate

### Cost Reduction Opportunities

| Action | Monthly Savings |
|--------|----------------|
| Kill legacy-2016 droplet + 150GB volume (DO) | $27 |
| Downgrade oven back to s-2vcpu-2gb (DO) | $36 |
| Downgrade help to s-1vcpu-1gb (DO) | $18 |
| Switch Redis to Upstash free tier | $15 |
| Evaluate if AT/PDS droplet is needed | $6 |
| Consolidate to fewer domains | $5–10/mo |
| **Total possible** | **~$107–112/mo** |

This could bring the monthly bill from ~$151 down to ~$40–50.

---

*Generated by Claude Code — 2026-03-03*
