# Mail Bootstrap on This Mac — 2026-04-24

## Status check

The repo has a fully designed mail stack (`ants/mail-mcp/server.mjs`,
`toolchain/email/style-guide.md`, vault GPG entries for `mail@aesthetic.computer`
and `me@jas.life`, `mu4e` config in `dotfiles/dot_config/emacs.el`). It runs
in the AC devcontainer / Linux side. **It is not bootstrapped on this macOS
host.** Specifically:

- `mbsync`, `mu`, `notmuch`, `msmtp` — none installed (`/opt/homebrew/bin/`
  empty for all four; no `/usr/local/bin/` versions either).
- `~/.mbsyncrc`, `~/.msmtprc`, `~/.authinfo.gpg` — none present.
- `~/.mail-all/` (the Maildir the MCP expects) — does not exist.
- `~/.cache/mu/xapian/` (the mu index path) — does not exist.

The Slab menubar (`slab/menubar-swift/Sources/SlabMenubar/`) **does** have the
Mail menu items the user remembered:

- "Mail: <unread count>" parent
- "Sync both" → `mbsync -a && mu index --quiet`
- "Sync ac-mail" → `mbsync ac-mail && mu index --quiet`
- "Sync jas-mail" → `mbsync jas-mail && mu index --quiet`
- "Open sync log" (opens `~/.mail-all/sync.log`)

The slab daemon and menubar are **installed and loaded** on this Mac
(`~/Library/LaunchAgents/computer.slab.{daemon,menubar}.plist` are both
loaded; `~/.local/share/slab/` is populated). So the menu items show up —
but clicking "Sync both" right now is a no-op because the underlying
`mbsync` / `mu` binaries don't exist on this Mac. That's the gap to close.

## What the mail-mcp expects

From `ants/mail-mcp/server.mjs`:

```js
const MU      = .../opt/homebrew/bin/mu  (or /usr/bin/mu)
const MBSYNC  = .../opt/homebrew/bin/mbsync
const MSMTP   = .../opt/homebrew/bin/msmtp
const MAILDIR = $HOME/.mail-all
const MU_DB   = $HOME/.cache/mu/xapian
const ACCOUNTS = {
  "ac-mail":  "mail@aesthetic.computer",
  "jas-mail": "me@jas.life",
}
const DEFAULT_ACCOUNT = "ac-mail"
```

So the missing piece on this Mac is exactly:

1. **Binaries** (Homebrew): `isync`, `mu`, `notmuch`, `msmtp`, `gnupg`,
   `pinentry-mac`.
2. **`~/.mbsyncrc`** with two channels (`ac-mail` → Gmail IMAP for
   `mail@aesthetic.computer`, `jas-mail` → Gmail IMAP for `me@jas.life`),
   syncing into `~/.mail-all/{ac-mail,jas-mail}/`.
3. **`~/.msmtprc`** with two SMTP accounts mirroring those.
4. **`~/.authinfo.gpg`** holding the Gmail app passwords (encrypted to
   `mail@aesthetic.computer`'s GPG key — same recipient the vault uses).
5. **First-time `mu init` + `mu index`** to build the search DB.
6. **(Optional)** a launchd LaunchAgent that runs `mbsync -a && mu index`
   every 5 minutes so the mailboxes stay current without manual sync.

## Credentials — already exist

Per `reports/2026-02-27-mail-inbox-emacs-mcp-secrets-report.md`:

- `aesthetic-computer-vault/at/deploy.env` has
  `SMTP_USER=mail@aesthetic.computer` and `SMTP_PASS=<app password>`.
- The Gmail app password tested working against `imap.gmail.com` from another
  environment (verified in that report).
- Same recipient (`mail@aesthetic.computer`) is the GPG key for the vault.

The `me@jas.life` Gmail app password — I don't see it surfaced in the report.
Probably needs to be generated fresh in Google Account → Security → App
Passwords if not already in the vault.

## Update 2026-04-24 — bootstrap actually run

**Finished:** binaries installed, configs in place, first sync running.

The vault already had `home/.mbsyncrc.gpg` and `home/.msmtprc.gpg` —
**no need to write fresh config**, just decrypt those. But two macOS-side
gotchas:

1. **Line endings.** Both vault configs are CRLF. macOS `mbsync` 1.5.1
   parses CRLF lines fine *except* that the `$` regex anchor in BSD
   `sed`/`awk` matches before `\n`, so any in-place edit using `^FOO$`
   silently no-ops. **Fix:** `tr -d '\r' < ~/.mbsyncrc > /tmp/x && mv /tmp/x ~/.mbsyncrc`
   before any sed/awk passes.

2. **Missing `AuthMechs LOGIN`.** The vault config doesn't declare an
   auth mechanism. macOS Cyrus SASL then errors with:

   ```
   Error performing SASL authentication step: SASL(-7): invalid parameter
   supplied: ... near plain_clienttoken.c:195
   ```

   **Fix:** insert `AuthMechs LOGIN` after every `TLSType IMAPS` line.

3. **Linux paths.** The vault config points at `/home/me/.mail{,-jas}/`.
   On macOS, rewrite to `/Users/jas/.mail-all/{ac-mail,jas-mail}/` —
   that's also where the Slab menubar's `mu` query expects to find them
   (`maildir:/ac-mail/INBOX OR maildir:/jas-mail/INBOX`).

**One-shot bootstrap that handles all three:**

```bash
brew install isync mu notmuch msmtp gnupg pinentry-mac

mkdir -p ~/.mail-all/ac-mail ~/.mail-all/jas-mail ~/.cache/mu

# mbsyncrc: decrypt → strip CR → remap paths → write 0600
gpg --decrypt ~/aesthetic-computer/aesthetic-computer-vault/home/.mbsyncrc.gpg \
  | tr -d '\r' \
  | sed 's|/home/me/.mail/|'"$HOME"'/.mail-all/ac-mail/|g; s|/home/me/.mail-jas/|'"$HOME"'/.mail-all/jas-mail/|g' \
  | awk '{print} /^TLSType IMAPS$/{print "AuthMechs LOGIN"}' \
  > ~/.mbsyncrc
chmod 600 ~/.mbsyncrc

# msmtprc: decrypt → strip CR → write 0600
gpg --decrypt ~/aesthetic-computer/aesthetic-computer-vault/home/.msmtprc.gpg \
  | tr -d '\r' \
  > ~/.msmtprc
chmod 600 ~/.msmtprc

# first sync + first index
mbsync -a
mu init --my-address=mail@aesthetic.computer --my-address=me@jas.life --maildir=$HOME/.mail-all
mu index
```

The Slab menubar (`computer.slab.menubar` LaunchAgent — already loaded)
will start showing the unread count after `mu index` completes, and
its "Sync both / ac-mail / jas-mail" menu items become functional.

---

## Bootstrap script (proposed, not yet run)

A first-cut script. Needs your review before running — it installs binaries
and writes config to your home directory.

```bash
# 1. install binaries
brew install isync mu notmuch msmtp gnupg pinentry-mac

# 2. create maildir tree
mkdir -p ~/.mail-all/ac-mail ~/.mail-all/jas-mail
mkdir -p ~/.cache/mu

# 3. write ~/.mbsyncrc (dual-account, full Gmail folders)
cat > ~/.mbsyncrc <<'EOF'
# --- ac-mail (mail@aesthetic.computer) ---
IMAPAccount ac-mail
Host imap.gmail.com
User mail@aesthetic.computer
PassCmd "security find-generic-password -a mail@aesthetic.computer -s ac-mail-app-password -w"
SSLType IMAPS
AuthMechs LOGIN

IMAPStore ac-mail-remote
Account ac-mail

MaildirStore ac-mail-local
Path ~/.mail-all/ac-mail/
Inbox ~/.mail-all/ac-mail/INBOX
SubFolders Verbatim

Channel ac-mail
Far :ac-mail-remote:
Near :ac-mail-local:
Patterns * !"[Gmail]/All Mail" !"[Gmail]/Important"
Create Both
Expunge Both
SyncState *

# --- jas-mail (me@jas.life) ---
IMAPAccount jas-mail
Host imap.gmail.com
User me@jas.life
PassCmd "security find-generic-password -a me@jas.life -s jas-mail-app-password -w"
SSLType IMAPS
AuthMechs LOGIN

IMAPStore jas-mail-remote
Account jas-mail

MaildirStore jas-mail-local
Path ~/.mail-all/jas-mail/
Inbox ~/.mail-all/jas-mail/INBOX
SubFolders Verbatim

Channel jas-mail
Far :jas-mail-remote:
Near :jas-mail-local:
Patterns * !"[Gmail]/All Mail" !"[Gmail]/Important"
Create Both
Expunge Both
SyncState *
EOF

# 4. store app passwords in macOS Keychain (no plaintext on disk):
#    you run these by hand so we never echo the password to Bash history
#
#    security add-generic-password -a mail@aesthetic.computer -s ac-mail-app-password -w
#    security add-generic-password -a me@jas.life            -s jas-mail-app-password -w
#
#    (each prompts for the password)

# 5. write ~/.msmtprc
cat > ~/.msmtprc <<'EOF'
defaults
auth        on
tls         on
tls_starttls on
tls_trust_file /opt/homebrew/etc/ca-certificates/cert.pem
logfile     ~/.msmtp.log

account ac-mail
host smtp.gmail.com
port 587
from mail@aesthetic.computer
user mail@aesthetic.computer
passwordeval security find-generic-password -a mail@aesthetic.computer -s ac-mail-app-password -w

account jas-mail
host smtp.gmail.com
port 587
from me@jas.life
user me@jas.life
passwordeval security find-generic-password -a me@jas.life -s jas-mail-app-password -w

account default : ac-mail
EOF
chmod 600 ~/.msmtprc

# 6. first sync + first index
mbsync -a
mu init --my-address=mail@aesthetic.computer --my-address=me@jas.life
mu index
```

Then to keep it fresh:

```bash
# every 5 min via launchd agent at ~/Library/LaunchAgents/computer.aesthetic.mailsync.plist
# (separate task — write the plist after the first manual sync succeeds)
```

## Why I didn't run this yet

1. **App passwords**: the `mail@aesthetic.computer` app password is in the
   vault but I shouldn't pull it out and write it to your Keychain without
   you confirming you want me to. The `me@jas.life` password I haven't
   found in vault — you may need to generate a fresh one.
2. **brew installs are slow + rewrite PATH**: worth a single confirmation.
3. **Vault unlock**: pulling the password requires unlocking the GPG vault
   (`vault-tool.fish` from the vault), which is your call.

## Decision the user needs to make

- **(a) "Run the bootstrap, you have my permission to use the AC mail
  password from the vault and to prompt me for the jas.life one."** I'll
  do steps 1–6 and verify a sync.
- **(b) "Just install the binaries; I'll handle the passwords."** I'll do
  1–3 + 5 (config files), then stop. You add Keychain entries.
- **(c) "Write a launchd plist too so it auto-syncs."** Add the LaunchAgent
  step after first manual sync succeeds.

## Wiring summary

Once steps 1–6 succeed:

- `slab/menubar-swift/` → "Mail: N unread" appears in the menubar with
  working "Sync both / ac-mail / jas-mail / Open sync log" items (already
  installed and loaded; just needs the binaries below it to function).
- `ants/mail-mcp/server.mjs` → MCP server can read/write mail through Claude
  via the same Maildir.
- `dotfiles/dot_config/emacs.el` → `mu4e` keys (`C-c m` open, `C-c M-m` sync)
  start working in Emacs.

That's the whole stack the user was describing — present and wired, just
missing the four Homebrew binaries and the two config files at home.

## Auto-sync — note

Slab menubar **refreshes the unread count every 30 s** (`mu find`) but
does **not** auto-trigger `mbsync`. Sync is on-demand via the menu items.
If you want continuous background sync, install a launchd `StartInterval`
agent that runs `mbsync -a && mu index --quiet` every N minutes. **Not
installed by this bootstrap** — left as an explicit choice. Suggested
plist if you want it:

```xml
<!-- ~/Library/LaunchAgents/computer.aesthetic.mailsync.plist -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>computer.aesthetic.mailsync</string>
  <key>ProgramArguments</key><array>
    <string>/bin/sh</string><string>-c</string>
    <string>/opt/homebrew/bin/mbsync -a >> "$HOME/.mail-all/sync.log" 2>&amp;1 &amp;&amp; /opt/homebrew/bin/mu index --quiet >> "$HOME/.mail-all/sync.log" 2>&amp;1</string>
  </array>
  <key>StartInterval</key><integer>300</integer>  <!-- every 5 min -->
  <key>RunAtLoad</key><true/>
  <key>StandardOutPath</key><string>/tmp/computer.aesthetic.mailsync.out</string>
  <key>StandardErrorPath</key><string>/tmp/computer.aesthetic.mailsync.err</string>
</dict></plist>
```

Load with `launchctl load ~/Library/LaunchAgents/computer.aesthetic.mailsync.plist`.
