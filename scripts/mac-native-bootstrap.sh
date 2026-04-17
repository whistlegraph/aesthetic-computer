#!/bin/bash
# Bootstrap a lightweight Mac (Apple Silicon or Intel) to run aesthetic-computer
# natively — outside the devcontainer — with all `ac-*` fish commands available.
#
# Matches plans/MAC-NATIVE-DEVENV-PLAN.md. Safe to re-run (idempotent).
# Prereqs: macOS 11+, Xcode CLT (git), the aesthetic-computer repo checked
# out at $HOME/aesthetic-computer, the vault at $HOME/aesthetic-computer/aesthetic-computer-vault.
#
# This script does NOT unlock the vault — do that manually with
# `fish aesthetic-computer-vault/vault-tool.fish unlock` before or after.

set -euo pipefail

AC_ROOT="$HOME/aesthetic-computer"
VAULT="$AC_ROOT/aesthetic-computer-vault"
ASKPASS="/tmp/ac-askpass.sh"
SUDOERS_FILE="/etc/sudoers.d/claude-ac-setup"

step() { printf "\n\033[1;34m▶ %s\033[0m\n" "$*"; }
ok()   { printf "  \033[1;32m✓\033[0m %s\n" "$*"; }
warn() { printf "  \033[1;33m!\033[0m %s\n" "$*"; }
die()  { printf "  \033[1;31m✗\033[0m %s\n" "$*"; exit 1; }

[[ "$(uname)" == "Darwin" ]] || die "this script is macOS-only"
[[ -d "$AC_ROOT" ]] || die "aesthetic-computer repo not found at $AC_ROOT"

# -----------------------------------------------------------------------------
step "1. GUI askpass helper for sudo -A"
# -----------------------------------------------------------------------------
cat > "$ASKPASS" <<'EOF'
#!/bin/bash
/usr/bin/osascript -e 'display dialog "Bootstrap needs sudo — enter your password:" default answer "" with hidden answer with icon caution' -e 'text returned of result' 2>/dev/null
EOF
chmod +x "$ASKPASS"
export SUDO_ASKPASS="$ASKPASS"
sudo -A -v
ok "sudo primed via askpass"

# -----------------------------------------------------------------------------
step "2. Homebrew"
# -----------------------------------------------------------------------------
if ! command -v brew >/dev/null 2>&1 && ! [[ -x /opt/homebrew/bin/brew ]]; then
    NONINTERACTIVE=1 /bin/bash -c \
      "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
eval "$(/opt/homebrew/bin/brew shellenv)"
ok "brew at $(which brew)"

# -----------------------------------------------------------------------------
step "3. Brew formulas"
# -----------------------------------------------------------------------------
# Core — needed for fish, node, mkcert, site tooling
brew install --quiet fish fnm mkcert nss jq ripgrep bat gh tree \
                     coreutils gnu-sed wget nmap ffmpeg \
                     caddy ngrok/ngrok/ngrok redis \
                     stripe/stripe-cli/stripe doctl awscli \
                     gnupg pinentry-mac 2>&1 | tail -3
ok "core brew formulas installed"

# -----------------------------------------------------------------------------
step "4. /workspaces → /Users/$USER via synthetic.conf"
# -----------------------------------------------------------------------------
if [[ -L /workspaces ]] && [[ "$(readlink /workspaces)" == "/Users/$USER" ]]; then
    ok "/workspaces already symlinked"
else
    # macOS SIP prevents writes to /, but synthetic.conf is the sanctioned way.
    printf 'workspaces\t/Users/%s\n' "$USER" | sudo -A tee /etc/synthetic.conf >/dev/null
    sudo -A chmod 644 /etc/synthetic.conf
    sudo -A /System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -t || \
      warn "apfs.util trigger failed — a reboot will also apply synthetic.conf"
    if [[ -L /workspaces ]]; then
        ok "/workspaces → $(readlink /workspaces)"
    else
        warn "/workspaces not yet present; reboot to activate"
    fi
fi

# -----------------------------------------------------------------------------
step "5. Scoped NOPASSWD sudoers"
# -----------------------------------------------------------------------------
SUDOERS_TMP=$(mktemp)
cat > "$SUDOERS_TMP" <<EOF
# Claude Code / aesthetic-computer native dev — narrow NOPASSWD for recurring
# ops. Review with \`sudo visudo -c\`.
Cmnd_Alias AC_DEV_SETUP = \\
    /usr/bin/tee -a /etc/shells, \\
    /usr/bin/security, \\
    /opt/homebrew/bin/mkcert

$USER ALL=(root) NOPASSWD: AC_DEV_SETUP
EOF
sudo -A visudo -cf "$SUDOERS_TMP" >/dev/null
sudo -A install -o root -g wheel -m 0440 "$SUDOERS_TMP" "$SUDOERS_FILE"
rm -f "$SUDOERS_TMP"
ok "sudoers file installed at $SUDOERS_FILE"

# -----------------------------------------------------------------------------
step "6. fnm + Node (lts-jod & 20.5.0)"
# -----------------------------------------------------------------------------
eval "$(fnm env --shell bash)"
fnm install lts-jod
fnm install 20.5.0
fnm default lts-jod
fnm use lts-jod
ok "node $(node --version) via fnm ($(fnm current))"

# -----------------------------------------------------------------------------
step "7. Global npm CLIs (incl. Claude Code)"
# -----------------------------------------------------------------------------
npm i -g --silent \
    @anthropic-ai/claude-code \
    @devcontainers/cli \
    netlify-cli \
    prettier typescript typescript-language-server \
    concurrently kill-port http-server npm-check-updates 2>&1 | tail -1
ok "npm globals installed"

# Native Claude Code binary (matches Dockerfile:223)
if ! [[ -x "$HOME/.local/bin/claude" ]]; then
    curl -fsSL https://claude.ai/install.sh | bash >/dev/null
fi
ok "claude native: $("$HOME/.local/bin/claude" --version | head -1)"

# -----------------------------------------------------------------------------
step "8. fish as default login shell"
# -----------------------------------------------------------------------------
if ! grep -q "/opt/homebrew/bin/fish" /etc/shells; then
    echo "/opt/homebrew/bin/fish" | sudo -n tee -a /etc/shells >/dev/null
fi
CURRENT_SHELL=$(dscl . -read "/Users/$USER" UserShell | awk '{print $2}')
if [[ "$CURRENT_SHELL" != "/opt/homebrew/bin/fish" ]]; then
    sudo -A /usr/bin/dscl . -change "/Users/$USER" UserShell "$CURRENT_SHELL" /opt/homebrew/bin/fish
fi
ok "login shell: $(dscl . -read /Users/$USER UserShell | awk '{print $2}')"

# -----------------------------------------------------------------------------
step "9. ~/.config/fish/config.fish"
# -----------------------------------------------------------------------------
mkdir -p "$HOME/.config/fish/conf.d" "$HOME/.config/fish/functions"
if ! [[ -f "$HOME/.config/fish/config.fish" ]] || \
   ! grep -q "$AC_ROOT/.devcontainer/config.fish" "$HOME/.config/fish/config.fish"; then
    cat > "$HOME/.config/fish/config.fish" <<FISHCFG
# ~/.config/fish/config.fish — native Mac AC dev env
# Generated by scripts/mac-native-bootstrap.sh
set -gx PATH /opt/homebrew/bin /opt/homebrew/sbin \$HOME/.local/bin \$PATH
set -gx AC_ROOT \$HOME/aesthetic-computer
set -gx AESTHETIC $USER
if not status is-interactive
    set -gx nogreet true
end
if type -q fnm
    fnm env --use-on-cd --shell fish | source
end
set -gx PAGER cat
set -gx GIT_PAGER cat
set -gx MANPAGER cat
if test -f \$AC_ROOT/.devcontainer/config.fish
    source \$AC_ROOT/.devcontainer/config.fish
end
FISHCFG
fi
ok "fish config written"

# -----------------------------------------------------------------------------
step "10. GPG agent with pinentry-mac"
# -----------------------------------------------------------------------------
mkdir -p "$HOME/.gnupg"
chmod 700 "$HOME/.gnupg"
if ! grep -q pinentry-mac "$HOME/.gnupg/gpg-agent.conf" 2>/dev/null; then
    cat >> "$HOME/.gnupg/gpg-agent.conf" <<'EOF'
pinentry-program /opt/homebrew/bin/pinentry-mac
default-cache-ttl 3600
max-cache-ttl 7200
EOF
fi
gpgconf --kill gpg-agent >/dev/null 2>&1 || true
gpgconf --launch gpg-agent
ok "gpg-agent uses pinentry-mac"

# -----------------------------------------------------------------------------
step "11. mkcert CA + localhost dev certs"
# -----------------------------------------------------------------------------
mkcert -install >/dev/null 2>&1
ok "mkcert CA installed in System keychain"
cd "$AC_ROOT/ssl-dev"
if ! [[ -f localhost.pem ]]; then
    env nogreet=true /opt/homebrew/bin/fish ./ssl-install.fish >/dev/null 2>&1
fi
ok "ssl-dev/localhost.pem ($(date -r localhost.pem +%Y-%m-%d))"

# -----------------------------------------------------------------------------
step "12. Vault env links"
# -----------------------------------------------------------------------------
# session-server reads .env relative to its own dir
if [[ -f "$VAULT/session-server/.env" ]]; then
    ln -sfn "$VAULT/session-server/.env" "$AC_ROOT/session-server/.env"
    ok "session-server/.env linked from vault"
else
    warn "vault locked? $VAULT/session-server/.env missing"
fi
# system/.env is loaded by ac-lith if present — no default, optional for local dev

# -----------------------------------------------------------------------------
step "13. Smoke test: boot ac-site briefly"
# -----------------------------------------------------------------------------
kill-port 8888 >/dev/null 2>&1 || true
(cd "$AC_ROOT/lith" && node server.mjs >/tmp/ac-bootstrap-lith.log 2>&1 &)
LITH_PID=$!
sleep 4
if curl -sSI --fail https://localhost:8888/ -o /dev/null 2>/dev/null; then
    ok "ac-site responds on https://localhost:8888 with trusted cert"
else
    warn "ac-site smoke test failed — see /tmp/ac-bootstrap-lith.log"
fi
kill "$LITH_PID" 2>/dev/null || true
kill-port 8888 >/dev/null 2>&1 || true

# -----------------------------------------------------------------------------
printf "\n\033[1;32m✓ Bootstrap complete.\033[0m\n"
printf "  Open a new Terminal tab (fish will be the default).\n"
printf "  Run \`ac-help\` to list commands, then \`ac-site\` to boot the site.\n\n"
