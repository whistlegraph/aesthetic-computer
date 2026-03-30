# Email Blast Tool: give.aesthetic.computer

**Created:** January 5, 2026
**Updated:** March 30, 2026
**Status:** Prepared, verified, and intentionally on hold
**Goal:** Warmly email recent AC users about what feels alive in AC this year and invite support via give.aesthetic.computer

---

## Quick Start

```bash
cd /workspaces/aesthetic-computer

# 1. Preview the email
node artery/email-blast.mjs --preview

# 2. Test with your own email
node artery/email-blast.mjs --test me@jas.life

# 3. Use the current 60-day audience snapshot in reports/mail/
#    or re-fetch if you want a newer audience export
node artery/email-blast.mjs --fetch-all

# 4. Hold here unless Jeffrey explicitly wants to send
node artery/email-blast.mjs --send

# 5. Resume next day (Gmail caps at ~500/day)
node artery/email-blast.mjs --send --resume
```

Current recommendation:
- Do not send the blast yet.
- Use `--preview` and `--test me@jas.life` only until the audience and tone feel final.

---

## March 30, 2026 Status

### Audience Snapshot
- `183` verified users logged in within the last 60 days
- `182` emailable after unsubscribe filtering
- `139` have AC handles
- `99` logged in within the last 30 days
- `31` logged in within the last 7 days

Private working files were exported locally to `reports/mail/` and should stay out of git.

### Copy Status
- The blast copy is no longer an emergency ask.
- Current subject: `a little note from aesthetic computer`
- Current body: a softer note about what feels alive in AC this year so far, with links to:
  - `https://give.aesthetic.computer`
  - `https://bills.aesthetic.computer`
- Current stats referenced in the copy came from the live metrics snapshot on March 30, 2026 around 04:20 UTC:
  - `4448` paintings
  - `17145` KidLisp programs
  - `18723` chat messages
  - `252` published pages

### Verification Status
- Lith unsubscribe POST handling was fixed after the migration.
- `application/x-www-form-urlencoded` requests now survive the lith adapter correctly.
- `artery/email-blast.mjs` now loads the unsubscribe secret from Mongo if the env var is absent, matching the live unsubscribe endpoint.
- Verified locally and on production:
  - valid unsubscribe GET returns `200`
  - unsubscribe POST returns `200` and inserts the Mongo unsubscribe record
  - resubscribe POST returns `200` and removes the record
  - invalid tokens return `403`

### Test Sends
- Test sends were sent to `me@jas.life` only while validating the unsubscribe flow and updated copy.
- No audience send has happened.

### Next Move
- Keep the blast paused.
- If Jeffrey wants to proceed later, start with the `99` users active within the last 30 days, not the full `182`.

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

## March 2026 Updates

- [x] Verified the unsubscribe endpoint still works after the lith migration
- [x] Fixed lith form POST body adaptation for urlencoded unsubscribe requests
- [x] Updated the mailer to load the unsubscribe secret from Mongo when needed
- [x] Rewrote the blast copy to be cuter, softer, and more about what is cool in AC this year
- [x] Sent test emails to `me@jas.life` only
- [x] Kept the actual audience send paused

---

## File Map

| File | Purpose |
|------|---------|
| `artery/email-blast.mjs` | Main blast tool |
| `lith/server.mjs` | Lith request adapter fix for urlencoded unsubscribe POSTs |
| `system/netlify/functions/unsubscribe.mjs` | Unsubscribe/resubscribe endpoint |
| `system/netlify.toml` | Function config + routing |
| `system/public/give.aesthetic.computer/` | Give donation page |
| `reports/email-blast-give-ac-status-2026-02.md` | Status report |
