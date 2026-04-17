# Mac Native Dev Env Plan

Run the aesthetic-computer stack piecemeal on a lightweight Apple Silicon laptop
(no devcontainer), with `ac-*` fish functions behaving the same way they do
inside the Fedora devcontainer. The primary success criterion is:

```
$ ac-site            # should boot lith on https://localhost:8888
$ ac-session         # should boot session-server on https://localhost:8889
```

…and the full grid of `ac-*` helpers in
[`.devcontainer/config.fish`](../.devcontainer/config.fish) should load into
every new fish session.

---

## 0. Current machine audit (2026-04-17)

Verified state before starting:

| thing                                | state      |
| ------------------------------------ | ---------- |
| macOS                                | 26.4, arm64 |
| git (Apple)                          | ✅ present |
| curl                                 | ✅ present |
| Xcode CLT                            | ✅ present |
| Homebrew                             | ❌ missing |
| fish                                 | ❌ missing |
| fnm / node / npm                     | ❌ missing |
| mkcert                               | ❌ missing |
| `~/aesthetic-computer` checkout      | ✅ present |
| `~/aesthetic-computer/node_modules`  | ✅ present (likely stale) |
| `~/aesthetic-computer/system/node_modules` | ✅ present |
| `~/aesthetic-computer/lith/node_modules`   | ✅ present |
| `~/aesthetic-computer/system/.env`   | ❌ missing (must come from vault) |
| `~/aesthetic-computer/ssl-dev/localhost.pem` | ❌ missing |
| vault repo `aesthetic-computer-vault` | ✅ present, locked (`.gpg` files) |

Everything else in the plan assumes this baseline.

---

## 1. Base toolchain via Homebrew

The devcontainer [Dockerfile](../.devcontainer/Dockerfile) uses `dnf` on
Fedora. The Mac equivalent is Homebrew. Install with:

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Then brew install the minimum set referenced by `ac-*` commands and
`server.mjs`:

```sh
# Core shell + node
brew install fish fnm mkcert

# Utilities that ac-* functions shell out to
brew install jq ripgrep bat gh tree coreutils gnu-sed wget \
             nmap ffmpeg caddy ngrok redis

# CLIs used by the site / session / deploys
brew install stripe/stripe-cli/stripe           # ac-stripe in aesthetic:all
brew install doctl awscli                       # deploys + assets sync
brew install --cask ngrok                       # if tunnel desired
```

Do **not** install the Fedora-only pieces (SmartPy, Octez, Cockpit, signal-cli
native tarball, cage compositor, texlive-*, PowerShell, Ollama). Add them on
demand per sub-project. This is the "lightweight laptop" divergence point.

**Node toolchain** — match the devcontainer exactly so `npm ci` works:

```sh
fnm install lts-jod
fnm install 20.5.0
fnm default lts-jod
```

Add `fnm env --use-on-cd --shell fish | source` to fish config (handled in
step 3).

Then global npm bins the fish functions expect:

```sh
npm i -g @devcontainers/cli @anthropic-ai/claude-code \
          netlify-cli prettier typescript typescript-language-server \
          concurrently kill-port http-server npm-check-updates
```

---

## 2. Path portability — `/workspaces/aesthetic-computer` symlink

[`.devcontainer/config.fish`](../.devcontainer/config.fish) hardcodes
`/workspaces/aesthetic-computer` in **48 places** (e.g. `ac-tv`, `ac-keeps`,
`ac-kidlisp`, `ac-login`, `ac-pack`). Rather than fork the file, create a
system symlink so both paths resolve to the same tree:

```sh
sudo mkdir -p /workspaces
sudo ln -s "$HOME/aesthetic-computer" /workspaces/aesthetic-computer
```

The config at [config.fish:27-32](../.devcontainer/config.fish:27) already
checks for `/workspaces/aesthetic-computer` and will symlink the reverse
direction (`~/aesthetic-computer` → `/workspaces/...`); since the home path
already exists as a real directory it's effectively a no-op, and all the
hardcoded paths resolve via the symlink we just created.

This keeps future upstream edits to `config.fish` working with zero diff.

---

## 3. Fish shell wiring

Three things to configure.

### 3.1 Make fish a login shell (optional but matches devcontainer UX)

```sh
echo /opt/homebrew/bin/fish | sudo tee -a /etc/shells
chsh -s /opt/homebrew/bin/fish
```

### 3.2 Source the devcontainer config from user fish config

The devcontainer copies `config.fish` into `/home/me/.config/fish/config.fish`.
On Mac, source it instead so it stays in sync with the repo:

```fish
# ~/.config/fish/config.fish
set -gx AC_ROOT ~/aesthetic-computer

# fnm — mirrors Dockerfile:244
if test -x /opt/homebrew/bin/fnm
    fnm env --use-on-cd --shell fish | source
end

# Load all ac-* functions from the repo
if test -f $AC_ROOT/.devcontainer/config.fish
    source $AC_ROOT/.devcontainer/config.fish
end
```

Caveats to expect on first source:

- The `fish_greeting` prints `Hi @$AESTHETIC!` — export `AESTHETIC=jas` (or
  skip via `set -U nogreet true`) in the config above.
- `load_envs` is only defined inside the devcontainer `envs/` tree; the
  guarded `if test -f /home/me/envs/load_envs.fish` block at
  [config.fish:7-12](../.devcontainer/config.fish:7) no-ops on Mac. Good.
- The `/home/me/.copilot` permission-fixer at
  [config.fish:38-43](../.devcontainer/config.fish:38) no-ops on Mac. Good.

### 3.3 Per-host tweak for any `/home/me` remnants

None of the `ac-site` / `ac-session` / `ac-lith` hot paths reference
`/home/me`. If something surfaces (e.g. `ac-emacs-*` family), patch
opportunistically rather than up-front.

---

## 4. TLS certs for `https://localhost:8888`

`lith/server.mjs` ([server.mjs:86-89](../lith/server.mjs:86)) requires
`ssl-dev/localhost.pem` + `localhost-key.pem`. The repo already has a Mac-aware
script: [`ssl-dev/ac-ssl-mac.fish`](../ssl-dev/ac-ssl-mac.fish) (older path) and
the generic [`ssl-dev/ssl-install.fish`](../ssl-dev/ssl-install.fish) (current).

Run once:

```sh
cd ~/aesthetic-computer/ssl-dev
mkcert -install                       # adds local CA to System keychain
fish ./ssl-install.fish               # generates pem/key and trusts them
```

This produces:

- `ssl-dev/localhost.pem`
- `ssl-dev/localhost-key.pem`
- `ssl-dev/combined.pem`
- `ssl-dev/localhost.crt` → copied to `system/public/aesthetic.crt`

Verify:

```sh
openssl x509 -in ssl-dev/localhost.pem -noout -subject -issuer
curl -fsSI https://localhost:8888/api/version   # only after step 7
```

---

## 5. Vault unlock + `system/.env`

`ac-lith` auto-loads `system/.env` at
[config.fish:1938-1945](../.devcontainer/config.fish:1938). The canonical copy
for local-dev purposes is `aesthetic-computer-vault/lith/.env.gpg`.

Follow the existing vault-unlock workflow (see memory
`reference_vault_unlock.md`):

```sh
# GPG private key lives on USB /Volumes/HOME — mount first
cd ~/aesthetic-computer/aesthetic-computer-vault
fish vault-tool.fish unlock
```

Then copy the lith env to the location the server reads:

```sh
cp aesthetic-computer-vault/lith/.env ~/aesthetic-computer/system/.env
```

Do the same for session-server if using `ac-session`:

```sh
cp aesthetic-computer-vault/session-server/.env ~/aesthetic-computer/session-server/.env
```

**Never commit** either plaintext `.env` — both are gitignored, but double
check with `git status` before any commit.

---

## 6. Refresh npm dependencies

`node_modules` trees were copied over but were built on a different
machine/architecture. Rebuild to avoid the `esbuild: wrong arch` class of
errors the devcontainer explicitly fixes at
[entry.fish:803-837](../.devcontainer/entry.fish:803).

```sh
cd ~/aesthetic-computer

# Root
rm -rf node_modules package-lock.json
npm install --no-fund --no-audit

# Critical subdirs (mirrors entry.fish:878)
for dir in system session-server lith shared utilities vscode-extension; do
  (cd $dir && rm -rf node_modules && npm install --no-fund --no-audit)
done

# Any long tail
npm run install:everything-else
```

Rough disk footprint expectation: 2–3 GB across all `node_modules` dirs.

---

## 7. Verify `ac-site`

From a fresh fish session:

```fish
ac-site
```

Expected behaviour (mirrors `ac-lith` at
[config.fish:1929](../.devcontainer/config.fish:1929)):

1. `cd` into `~/aesthetic-computer/lith`
2. Kill anything holding port 8888
3. Load `system/.env`
4. `node --watch server.mjs`
5. Console line: `🚀 Starting server on https://localhost:8888...`

Smoke tests:

```sh
curl -fsSI https://localhost:8888/
curl -fsS  https://localhost:8888/api/version | jq
```

Open `https://localhost:8888/prompt` in the browser — the AC runtime should
render.

---

## 8. Verify `ac-session` (optional second server)

```fish
ac-session
```

Should bind `https://localhost:8889`. Confirm with `curl -fsSI
https://localhost:8889/healthz` (or whatever the current health path is — read
`session-server/session.mjs` to be sure).

`npm run aesthetic` launches *both* site + session + stripe + url concurrently
via `concurrently`, which is what the devcontainer uses by default. It should
also work on Mac once step 1 installs the `concurrently` / `stripe` globals.

---

## 9. Skip list (don't install on the laptop)

These are in the devcontainer Dockerfile but are either heavyweight or
Linux-only. Defer unless a specific task needs them:

- `emacs-nox` + emacs daemon (all `ac-emacs-*` functions)
- `xvfb`, `chromium`, `cage` (used for OAuth popup + headless rendering)
- `octez-*`, `tezos-smartpy` (only needed for NFT minting scripts)
- `redis-server` (use `redis` brew cask only if a command complains)
- `turnserver` (WebRTC relay — only needed for multiplayer UDP)
- texlive stack (only `papers` + `help` PDF generation)
- `pwsh` / PowerShell (only the Windows interop scripts)
- `signal-cli`, `gurk-rs` (only `ac-signal`)
- `sbcl`/quicklisp (only lisp experiments)
- `jamsocket` login state (only session-server deploys)

Rule: if an `ac-*` command errors with "command not found", brew install that
one specific dep — don't pre-install the whole Fedora manifest.

---

## 10. Reproducibility — roll-up script

Once verified interactively, commit a `scripts/mac-native-bootstrap.fish`
(or extend [`dotfiles/install.sh`](../dotfiles/install.sh) with a `macos`
branch) that performs steps 1–6 idempotently so the next fresh Mac can run:

```sh
curl -fsSL https://raw.githubusercontent.com/.../mac-native-bootstrap.fish | fish
```

Suggested layout:

```
scripts/
  mac-native-bootstrap.fish      # brew + fnm + npm globals
  mac-native-link-workspaces.sh  # /workspaces symlink (needs sudo)
  mac-native-fish-config.fish    # writes ~/.config/fish/config.fish
```

Keep each step a separate function so reruns are cheap.

---

## 11. Known gotchas to watch for

- **`/workspaces` symlink requires sudo** and must survive reboots. APFS
  preserves it, but if FileVault / boot-arg changes reset `/`, re-create it.
- **`fnm` + fish auto-switch**: `.nvmrc` files in `lith/` or `session-server/`
  will auto-switch node versions on `cd`. That's fine, but `ac-pack` / `ac-ship`
  assume lts-jod — sanity check `node -v` if a command suddenly errors with
  unknown syntax.
- **Apple GPG vs Homebrew GPG**: `vault-tool.fish` expects `gpg` with
  `pinentry-mac` configured. If vault-unlock loops on passphrase prompts,
  `brew install gnupg pinentry-mac` and wire
  `~/.gnupg/gpg-agent.conf` → `pinentry-program /opt/homebrew/bin/pinentry-mac`.
- **Xcode CLT updates** can silently swap `git`. If `git` behaves oddly after
  an OS update, `xcode-select --install`.
- **Port conflicts**: AirPlay Receiver on macOS grabs port 5000 by default;
  AC doesn't use 5000 today but `ac-views` uses 5555 — still fine.
- **Kerning between `ac-site` and `npm run aesthetic`**: `ac-site` starts
  *only* lith. `npm run aesthetic` runs lith + session + stripe + url under
  `concurrently`. Use whichever matches the task; don't double-start lith.

---

## 12. Definition of done

- [ ] `fish` is the login shell; new terminal tab has `ac-help` output ≥ 80 items
- [ ] `ac-site` brings up `https://localhost:8888` with a trusted cert (no
      `-k` needed on `curl`)
- [ ] `ac-session` brings up `https://localhost:8889`
- [ ] `npm test` from repo root passes (or fails only on known
      vault/Linux-only specs)
- [ ] `git status` in both `aesthetic-computer` and
      `aesthetic-computer-vault` is clean (no accidental `.env` commits)
- [ ] Bootstrap script in `scripts/` reproduces the whole setup on a fresh
      user account
