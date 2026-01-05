# Email Blast Tool: give.aesthetic.computer

**Created:** January 5, 2026  
**Status:** Planning / Urgent  
**Goal:** Send a one-sentence donation solicitation email to all Auth0 users

## Context

Bills due tomorrow! Need to reach all AC users with a simple ask:
> Support Aesthetic Computer at **give.aesthetic.computer**

## Architecture

### Credentials Available
- **SMTP**: Gmail via `mail@aesthetic.computer` (in `at/deploy.env`)
  - Server: `smtp.gmail.com:465`
  - User: `mail@aesthetic.computer`
  - Pass: `twbj lbut jwvs xtnx` (app password)
- **Auth0 M2M API**: For listing all users (in `at/.env`)
  - Client ID: `ogNmc0Aqw7DEY9oQZ6JMvJPrauSHe9dY`
  - Domain: `aesthetic.us.auth0.com`

### Data Sources (MongoDB)
- `@handles` collection — users who have claimed a handle
- `users` collection — all registered users with codes

---

## Commands

### 1. List All Auth0 Users
```bash
cd /workspaces/aesthetic-computer
node artery/email-blast.mjs --list
```

Output columns:
- Email
- Sign-up date
- Email verified (y/n)
- Has handle (y/n)
- Handle (if set)

### 2. Preview Email
```bash
node artery/email-blast.mjs --preview
```

### 3. Send Blast (with confirmation)
```bash
node artery/email-blast.mjs --send
```

### 4. Send Test Email (to yourself)
```bash
node artery/email-blast.mjs --test me@jas.life
```

---

## Email Content

**From:** `mail@aesthetic.computer`  
**Subject:** help aesthetic.computer stay online 💾  
**Body:**

```
hi!

aesthetic computer needs your help to pay this month's server bills.

if you can, please visit: https://give.aesthetic.computer

every bit helps. thank you for being part of this 💛

— jas
```

---

## Script Location

`artery/email-blast.mjs`

### Features
- [ ] List all Auth0 users with metadata
- [ ] Join with MongoDB `@handles` collection for handle status
- [ ] Send via Gmail SMTP (nodemailer)
- [ ] Dry-run mode (default)
- [ ] Batch sending with delays (to avoid rate limits)
- [ ] Log sent/failed emails

### Safety
- Requires `--send` flag to actually send (dry-run by default)
- Confirmation prompt before sending
- Max 100 emails per batch with 1s delay between

---

## Implementation Plan

1. **Create `artery/email-blast.mjs`**
   - Load env from `at/.env`
   - Connect to Auth0 Management API
   - Paginate through all users
   - Join with MongoDB handles
   - Send via nodemailer

2. **Test with `--test me@jas.life`**

3. **Preview full list with `--list`**

4. **Send blast with `--send`**

---

## Rate Limits

- **Auth0**: 15 requests/second for Management API
- **Gmail SMTP**: ~500 emails/day (app password)
- **Batch strategy**: Send in groups of 50 with 2s delay between batches

---

## Quick Start

```bash
# 1. Install nodemailer if needed
cd /workspaces/aesthetic-computer && npm install nodemailer --save-dev

# 2. List users
node artery/email-blast.mjs --list

# 3. Test email
node artery/email-blast.mjs --test me@jas.life

# 4. Send for real
node artery/email-blast.mjs --send
```
