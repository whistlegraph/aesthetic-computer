# 2026-02-27 Mail Inbox + Emacs MCP + Secrets Report

## Scope
- Goal: get `mail@aesthetic.computer` inbox into the Emacs workflow used by AC/Emacs-MCP.
- Audit targets: `aesthetic-computer-vault`, MongoDB secrets, Netlify secrets/env vars.

## Executive Summary
- This machine can already access the mailbox credentials from both vault files and Netlify env vars.
- IMAP auth works right now for `mail@aesthetic.computer` against Gmail.
- The current Emacs setup has no mail client config yet (no `mu4e`/`notmuch` config in AC Emacs files).
- Required binaries for a local Emacs mail workflow are not yet installed (`mbsync`, `mu`, `notmuch`, `msmtp`).
- Netlify CLI on this machine can return full secret values (`npx netlify env:list --json`), including `SMTP_PASS`.

## 2026-02-28 Addendum
- Emacs mail config was added on `2026-02-26` in commit `979c14333`:
  - `dotfiles/dot_config/emacs.el` now includes `mu4e` setup and keybindings:
    - `C-c m` => `ac-mail-open`
    - `C-c M-m` => `ac-mail-sync`
- Mail + vault dependencies are now part of container builds:
  - `.devcontainer/Dockerfile`
  - `.devcontainer/Containerfile.apple`
  - Added packages: `isync`, `maildir-utils`, `notmuch`, `msmtp`, `gnupg2`, `pinentry`
- Vault now operates with GPG lock/unlock during devcontainer startup:
  - `.devcontainer/entry.fish` runs `devault.fish` during Phase 9.
  - `devault.fish` auto-unlocks when `.gpg` files exist, copies secrets, then re-locks.
  - `vault-tool.fish` performs GPG encrypt/decrypt using recipient `mail@aesthetic.computer`.

## What I Verified

### 1) Mail credentials exist in vault + Netlify
- Vault file with explicit SMTP creds:
  - `aesthetic-computer-vault/at/deploy.env`
  - contains `SMTP_SERVER=smtp.gmail.com`, `SMTP_USER=mail@aesthetic.computer`, `SMTP_PASS=...`
- Synced workspace copy:
  - `at/deploy.env`
  - same SMTP entries.
- Netlify env also has the same mail vars:
  - `SMTP_USER`
  - `SMTP_PASS`
  - `SMTP_SERVER`

### 2) IMAP login is functional
- I tested IMAP directly:
  - `curl --url 'imaps://imap.gmail.com/INBOX' --user "$SMTP_USER:$SMTP_PASS" --request 'EXAMINE INBOX'`
- Result: successful mailbox metadata response (`EXISTS`, `UIDNEXT`, etc.).
- Current observed state at test time: `7 EXISTS`.

### 3) Emacs config status
- AC Emacs config files inspected:
  - `dotfiles/dot_config/emacs.el`
  - `dotfiles/dot_config/emacs-optimized.el`
- No current mail workflow config in those files (`mu4e`, `notmuch`, `mbsync`, SMTP send config not present).
- Emacs currently focuses on terminal tabs and MCP state helpers; mail can be added as another managed tab/command.

### 4) Package availability in this container (Fedora 43)
- Available in repos:
  - `isync` (provides `mbsync`)
  - `maildir-utils` (provides `mu` and `mu4e.el`)
  - `notmuch`
- Not currently installed.

## Secret Audit Findings

### A) MongoDB secrets
Repeated app credential pattern found in many `.env` files (both repo and vault):
- `mongodb://aesthetic_app:<password>@silo.aesthetic.computer:27017/aesthetic?...`

Examples:
- `aesthetic-computer-vault/help/.env`
- `aesthetic-computer-vault/session-server/.env`
- `aesthetic-computer-vault/at/.env`
- `help/.env`
- `session-server/.env`
- `at/.env`
- `judge/.env`, `censor/.env`, `feed/.env`, `oven/.env`, `nanos/*.env`, etc.

Additional high-sensitivity Mongo secret:
- `aesthetic-computer-vault/silo/.env` has `MONGO_ADMIN_PASSWORD` and admin connection string.

Potential legacy/backup secret:
- `system/.env.backup` contains an Atlas `mongodb+srv://admin:...` URI.

### B) Netlify secrets / env vars
- Linked site metadata present in:
  - `system/.netlify/state.json` (`siteId` present)
- Netlify CLI access here can read all values:
  - `cd system && npx netlify env:list --json`
- Observed key set includes (non-exhaustive):
  - `SMTP_USER`, `SMTP_PASS`, `SMTP_SERVER`
  - `MONGODB_CONNECTION_STRING`, `MONGODB_NAME`
  - `AUTH0_*`, `REDIS_CONNECTION_STRING`, `STRIPE_*`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, etc.

## Recommended Integration Path (Emacs + MCP)

### Recommended stack
- `mbsync` for IMAP sync to local Maildir
- `mu`/`mu4e` for search + reading in Emacs
- `msmtp` or `smtpmail` for sending
- credentials stored in `~/.authinfo.gpg` (not in repo `.env` files)

### Why this path
- Works cleanly with Gmail IMAP/SMTP app password already in use.
- Keeps inbox manageable from Emacs directly.
- Compatible with AC’s existing Emacs terminal/tab orchestration (can add a dedicated `mail` flow later).

### Minimal bootstrap commands (not yet executed)
```bash
sudo dnf install -y isync maildir-utils notmuch msmtp
```

### Minimal config artifacts to add next
- `~/.mbsyncrc`
- `~/.authinfo.gpg`
- Emacs mail config snippet in `dotfiles/dot_config/emacs.el` (or optimized variant)

## Security Notes
- Secrets are currently widely duplicated in plaintext env files across repo + vault sync targets.
- Netlify CLI here can return full secret values; treat this machine as high-trust/high-risk.
- If broad access was unintended, rotate at least:
  - mail app password (`mail@aesthetic.computer`)
  - MongoDB app/admin passwords
  - Netlify-managed API keys/tokens exposed in env.

## Practical Next Step
- If you want, I can implement Phase 1 immediately:
  1. install `isync + maildir-utils + notmuch + msmtp`
  2. set up `mail@aesthetic.computer` Maildir sync
  3. wire a basic `mu4e` entrypoint in the AC Emacs config
  4. add a `mail` terminal/tab command for MCP-managed workflow.
