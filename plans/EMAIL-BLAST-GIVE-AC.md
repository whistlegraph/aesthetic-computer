# Email Blast Tool: give.aesthetic.computer

**Created:** January 5, 2026
**Updated:** February 12, 2026
**Status:** Ready to send (all 5 pre-send issues resolved)
**Goal:** Email Auth0 users asking them to support AC via give.aesthetic.computer

---

## Quick Start

```bash
cd /workspaces/aesthetic-computer

# 1. Preview the email
node artery/email-blast.mjs --preview

# 2. Test with your own email
node artery/email-blast.mjs --test me@jas.life

# 3. Re-fetch users (data is from Jan 5 — get fresh list)
node artery/email-blast.mjs --fetch-all

# 4. Send to verified users only (default, ~5.3k users)
node artery/email-blast.mjs --send

# 5. Resume next day (Gmail caps at ~500/day)
node artery/email-blast.mjs --send --resume
```

---

## Architecture

### Credentials
- **SMTP**: Gmail via `mail@aesthetic.computer` (in `at/.env`)
- **Auth0 M2M**: For listing all users (in `at/.env`)
- **UNSUBSCRIBE_SECRET**: HMAC secret for unsubscribe tokens (in `at/.env` + Netlify)

### Data Sources (MongoDB)
- `@handles` — users who have claimed a handle
- `email-blast-unsubscribes` — users who unsubscribed via email link
- `chat-*-mutes` — muted users (also excluded)

---

## Unsubscribe System

Each email contains a personalized unsubscribe link using HMAC tokens:

```
https://aesthetic.computer/api/unsubscribe?email=X&token=HMAC
```

**Flow:**
1. User clicks link → GET shows confirmation page
2. User clicks "Unsubscribe" button → POST records in MongoDB
3. Next blast run → checks `email-blast-unsubscribes` and skips those emails
4. Users can also resubscribe from the same page

**Files:**
- `system/netlify/functions/unsubscribe.mjs` — Netlify endpoint
- Token generated via HMAC-SHA256 of email + UNSUBSCRIBE_SECRET
- Emails also include `List-Unsubscribe` header (Gmail native unsub button)

---

## Commands

| Command | Description |
|---------|-------------|
| `--fetch` | Fetch users (page-based, max 1k) |
| `--fetch-all` | Bulk export ALL users (16k+) |
| `--list` | Show cached users |
| `--export` | Export to CSV |
| `--preview` | Preview email content |
| `--test EMAIL` | Send test email |
| `--send` | Send to VERIFIED users only (default) |
| `--send --all` | Send to ALL users |
| `--send --resume` | Resume interrupted send |
| `--clear` | Clear cached data |

---

## Rate Limits

- **Gmail SMTP**: ~500 emails/day
- **Verified users (~5.3k)**: ~11 days to complete
- **All users (~18k)**: ~36 days to complete
- Tool auto-pauses at 400 emails and supports `--resume`

---

## February 2026 Updates

- [x] Added unsubscribe system (HMAC tokens, Netlify function, MongoDB)
- [x] Updated email copy (removed stale "this month's" phrasing)
- [x] Made verified-only the default (`--send` = verified, `--send --all` = everyone)
- [x] Fixed import bug in getMutedUsers() (wrong relative path)
- [x] Added `List-Unsubscribe` headers for Gmail native unsub button
- [x] Added Gmail rate limit docs and day estimates in output

---

## File Map

| File | Purpose |
|------|---------|
| `artery/email-blast.mjs` | Main blast tool |
| `system/netlify/functions/unsubscribe.mjs` | Unsubscribe/resubscribe endpoint |
| `system/netlify.toml` | Function config + routing |
| `system/public/give.aesthetic.computer/` | Give donation page |
| `reports/email-blast-give-ac-status-2026-02.md` | Status report |
