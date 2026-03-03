# Email Blast Status Report: give.aesthetic.computer

**Date:** March 3, 2026 (updated)
**Original Plan:** January 5, 2026
**Status:** Copy updated, test emails sent, ready to send blast

---

## Summary

Email blast tool was built Jan 5, 2026. On March 3, 2026 the email copy was
updated for the DigitalOcean server suspension emergency (~$400 needed) and
multiple test emails were successfully sent to me@jas.life.

---

## March 3, 2026 Progress

### Done today
- [x] Updated email subject to "💾 Save us..."
- [x] Updated email body — brief, plain text, references DO suspension + $400
- [x] Changed sender identity from "jas" to "@jeffrey"
- [x] Added "still working" section: notepat.com, kidlisp.com, offline-capable pieces
- [x] Specified what's down: chat communities, assets and user media archive, database
- [x] Added GitHub Sponsors link alongside give.aesthetic.computer
- [x] Stripped all HTML styling — plain text look
- [x] Fixed SMTP credential loading (added at/deploy.env to dotenv config)
- [x] Sent 5 test emails successfully to me@jas.life
- [x] Added unsubscribe system (done in Feb update)

### Still TODO
- [ ] Re-fetch Auth0 users (`--fetch-all`) — data is from Jan 5
- [ ] Set UNSUBSCRIBE_SECRET in env for valid unsubscribe links
- [ ] Send blast to verified users (`--send`)
- [ ] Monitor sent/failed logs
- [ ] Emacs mail tab integration for managing blast from editor

---

## Current Email Content

**Subject:** 💾 Save us...

**Body (plain text):**
```
Aesthetic.Computer's servers were suspended. We need ~$400 to come back online.

  give.aesthetic.computer
  github.com/sponsors/whistlegraph

Even though chat communities, assets and user media archive, and database
are offline — you can still use notepat.com, kidlisp.com, and explore pieces
that don't require backend connectivity, thanks to our distributed hosting design.

Even $5 helps. Thank you.

— @jeffrey
```

---

## What's Been Built

### Email Blast Tool
- **File:** `artery/email-blast.mjs`
- **Commands:** `--preview`, `--test EMAIL`, `--send`, `--send --resume`, `--fetch-all`
- **Rate limiting:** 1 email/sec, 10s pause every 50, 5min pause every 400
- **Gmail limit:** ~500 emails/day → 5.3k verified users = ~11 days
- **Resume:** Supports `--resume` for multi-day sends

### User Data
- **18,187 Auth0 users** (5,373 verified, 12,814 unverified)
- Cache: `aesthetic-computer-vault/user-reports/email-blast-users.json`
- Last fetched: January 5, 2026 (needs refresh)

### Unsubscribe System
- HMAC-token unsubscribe links in every email
- `List-Unsubscribe` header for Gmail native unsub button
- Netlify function: `system/netlify/functions/unsubscribe.mjs`
- MongoDB collection: `email-blast-unsubscribes`

### Give Infrastructure
- **give.aesthetic.computer** — Stripe + PayPal donations (live)
- **GitHub Sponsors** — github.com/sponsors/whistlegraph (live)
- **Emergency site mode** — blinking ticker, crying kid face, GIVE button (live)

---

## File Map

| File | Purpose |
|------|---------|
| `artery/email-blast.mjs` | Main blast tool |
| `system/netlify/functions/unsubscribe.mjs` | Unsubscribe endpoint |
| `plans/EMAIL-BLAST-GIVE-AC.md` | Plan + quick start guide |
| `reports/funding-action-plan-2026-03-03.md` | Full funding action plan |
| `reports/monetization-stack-2026-02.md` | Monetization analysis |

---

*Updated March 3, 2026*
