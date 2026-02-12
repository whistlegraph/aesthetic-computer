# Email Blast Status Report: give.aesthetic.computer

**Date:** February 12, 2026
**Original Plan:** January 5, 2026
**Status:** Built but NOT yet sent

---

## Summary

~5 weeks ago (Jan 5), an urgent plan was created to email all Auth0 users asking
them to support Aesthetic Computer via https://give.aesthetic.computer. The tool
was fully built and user data was fetched, but **the blast was never actually
sent**. No sent log or failed log exists.

---

## What's Been Done

### 1. User Data (COMPLETE)
- **18,187 Auth0 users** fetched and cached locally
- **5,373 verified** email addresses (~30%)
- **12,814 unverified** (~70%)
- Cache file: `aesthetic-computer-vault/user-reports/email-blast-users.json`
- Last fetched: January 5, 2026
- Data includes: user_id, email, email_verified, created_at

### 2. Email Blast Tool (COMPLETE)
- **File:** `artery/email-blast.mjs` (766 lines, fully functional)
- Commands:
  - `--fetch` / `--fetch-all` — pull users from Auth0
  - `--list` — show cached users
  - `--export` — export to CSV
  - `--preview` — preview email content
  - `--test EMAIL` — send a test email
  - `--send` — send to all users (with confirmation)
  - `--send --resume` — resume interrupted blast
  - `--send --verified-only` — only verified emails
- Features:
  - Rate limiting (1 email/sec, 10s pause every 50, 5min pause every 400)
  - Resume from interruption via sent log
  - Muted user exclusion (pulls from MongoDB mutes collections)
  - Handle mapping via MongoDB `@handles` collection
  - Checkpoint saving during fetch

### 3. Email Content (COMPLETE)
- **From:** `mail@aesthetic.computer`
- **Subject:** "help aesthetic.computer stay online"
- **Body:** Short, personal ask from "jas" directing to give.aesthetic.computer
- Both plain text and HTML versions
- Mentions payment methods: card (USD/DKK), Tezos, Ethereum, Bitcoin
- Mentions monthly subscription option

### 4. Give Page (COMPLETE)
- **Live at:** https://give.aesthetic.computer
- Files: `system/public/give.aesthetic.computer/` (index.html, thanks.html, favicon.svg)
- Stripe integration for one-time and recurring donations
- Multiple currencies (USD, DKK)
- PayPal alternative
- Donor notes via Stripe custom fields
- Subscription management portal (`/give-portal`)
- API endpoints: `give.js`, `gives.mjs`, `give-portal.js`, `give-image.mjs`

### 5. Infrastructure (COMPLETE)
- SMTP via Gmail (`smtp.gmail.com:465`, app password in `at/deploy.env`)
- Auth0 M2M API credentials in `at/.env`
- MongoDB connection for handles + mutes
- nodemailer installed

---

## What Has NOT Been Done

| Task | Status |
|------|--------|
| Send test email to yourself | **NOT DONE** (no logs exist) |
| Send blast to verified users | **NOT DONE** |
| Send blast to all users | **NOT DONE** |
| CSV export | **NOT DONE** (no CSV file found) |
| Review/update email copy (it's been 5 weeks) | **NOT DONE** |

---

## Key Considerations Before Sending

### 1. Stale User Data
- User cache is **5+ weeks old** (Jan 5). New signups since then won't be included.
- Consider re-fetching with `--fetch-all` (bulk export for 16k+ users).

### 2. Gmail Sending Limits
- Gmail app passwords allow ~500 emails/day.
- **18,187 users = ~36 days** to reach everyone at max rate.
- **5,373 verified = ~11 days** if verified-only.
- Consider: Is Gmail SMTP the right tool for this volume? A transactional email
  service (SendGrid, Postmark, AWS SES) would be faster and more deliverable.

### 3. Email Content Freshness
- The copy says "this month's server bills" — still accurate?
- May want to update the tone since it's no longer a Jan 5 emergency.

### 4. Unsubscribe / CAN-SPAM
- No unsubscribe link in the current email template.
- Sending to 18k users without an unsubscribe mechanism could trigger spam
  complaints and get `mail@aesthetic.computer` blacklisted.
- **Recommendation:** Add an unsubscribe link before sending.

### 5. Verified vs. All
- Sending to unverified emails (70% of list) risks high bounce rates.
- High bounce rates can damage sender reputation.
- **Recommendation:** Start with `--send --verified-only` (5,373 users).

---

## Recommended Next Steps

1. **Update email copy** — refresh the message for February 2026
2. **Add unsubscribe mechanism** — even a simple "reply STOP" or link
3. **Send test email** — `node artery/email-blast.mjs --test me@jas.life`
4. **Re-fetch users** — `node artery/email-blast.mjs --fetch-all` (get new signups)
5. **Send to verified only first** — `node artery/email-blast.mjs --send --verified-only`
6. **Monitor** — watch `scratch/email-blast-sent.log` and `scratch/email-blast-failed.log`
7. **Consider a proper email service** for better deliverability at scale

---

## File Map

| File | Purpose |
|------|---------|
| `artery/email-blast.mjs` | Main blast tool |
| `plans/EMAIL-BLAST-GIVE-AC.md` | Original plan (Jan 5) |
| `aesthetic-computer-vault/user-reports/email-blast-users.json` | 18k cached users |
| `system/public/give.aesthetic.computer/index.html` | Give donation page |
| `system/netlify/functions/give.js` | Stripe checkout sessions |
| `system/netlify/functions/gives.mjs` | Recent donations API |
| `system/netlify/functions/give-portal.js` | Subscription management |
| `system/public/aesthetic.computer/lib/give-button.mjs` | Animated GIVE button UI |
| `reports/monetization-stack-2026-02.md` | Full monetization analysis |

---

*Report generated February 12, 2026*
