#!/bin/bash
# Native Mac shell + tracked dotfiles. Run as the login user, never with sudo.
set -euo pipefail
AC_ROOT=$(cd "$(dirname "$0")/.." && pwd)
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:$HOME/.local/bin:$PATH"
mode=${1:---install}
case "$mode" in --install|--dotfiles-only|--check) ;; *) echo 'Usage: mac-family-setup.sh [--install|--dotfiles-only|--check]' >&2; exit 2;; esac
[[ $(uname -s) == Darwin ]] || { echo 'macOS required' >&2; exit 1; }
[[ $EUID != 0 ]] || { echo 'Run as your login user, not root.' >&2; exit 1; }
[[ -f "$AC_ROOT/.devcontainer/config.fish" ]] || { echo 'Expand the checkout first: git sparse-checkout disable' >&2; exit 1; }
if [[ $mode == --check ]]; then
    failed=0
    for cmd in git brew fish fnm node npm jq rg starship nvim; do
        if command -v "$cmd" >/dev/null; then printf '%s: %s\n' "$cmd" "$(command -v "$cmd")"; else echo "MISSING: $cmd"; failed=1; fi
    done
    for entry in '.config/starship.toml:starship.toml' '.config/nvim:nvim' '.emacs:emacs.el'; do
        dest="$HOME/${entry%%:*}"; src="$AC_ROOT/dotfiles/dot_config/${entry#*:}"
        if [[ -L "$dest" && $(readlink "$dest") == "$src" ]]; then echo "linked: $dest"; else echo "UNLINKED: $dest"; failed=1; fi
    done
    if command -v fish >/dev/null; then
        fish -c 'test "$AC_ROOT" = $argv[1]; and functions -q ac ac-help ac-site ac-piece-logs; and type -q c co cr cor; and node --version; and npm --version' -- "$AC_ROOT" || failed=1
        login_shell=$(dscl . -read "/Users/$(id -un)" UserShell | awk '{print $2}')
        [[ $login_shell == "$(command -v fish)" ]] || { echo "Login shell still $login_shell"; failed=1; }
    fi
    if [[ $(git -C "$AC_ROOT" config --get core.sparseCheckout || true) == true ]]; then
        echo 'AC checkout is still sparse'; failed=1
    fi
    if command -v fish >/dev/null; then
        expected_node="v$(tr -d '[:space:]' < "$AC_ROOT/.node-version")"
        actual_node=$(fish -c 'node --version')
        [[ $actual_node == "$expected_node" ]] || { echo "Node: expected $expected_node, found $actual_node"; failed=1; }
    fi
    if [[ -x "$HOME/.local/bin/claude-sleep" ]]; then
        if sudo -n -l /usr/bin/pmset -a disablesleep 1 >/dev/null 2>&1; then
            echo 'Slab sleep-control permission: OK'
        else
            echo 'Slab sleep-control permission missing: run bash slab/install.sh --sleep-control-only'
            failed=1
        fi
    fi
    git -C "$AC_ROOT" log -1 --format='AC: %h %s'
    exit "$failed"
fi
if [[ $mode == --install ]]; then
    if ! command -v brew >/dev/null; then
        # Homebrew owns the administrator prompt; never store a password.
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    eval "$(brew shellenv)"
    brew install fish fnm jq ripgrep starship neovim coreutils
    eval "$(fnm env --shell bash)"
    node_version=$(tr -d '[:space:]' < "$AC_ROOT/.node-version")
    fnm install "$node_version"
    fnm default "$node_version"
    fnm use "$node_version"
fi
backup_dir=''
backup() {
    if [[ -e "$1" || -L "$1" ]]; then
        if [[ -z $backup_dir ]]; then
            mkdir -p "$HOME/.local/state/ac-dotfiles"
            backup_dir=$(mktemp -d "$HOME/.local/state/ac-dotfiles/backup.XXXXXXXX")
            echo "Previous settings: $backup_dir"
        fi
        mkdir -p "$backup_dir/$(dirname "${1#"$HOME/"}")"
        mv "$1" "$backup_dir/${1#"$HOME/"}"
    fi
}
link() {
    [[ -L "$2" && $(readlink "$2") == "$1" ]] && return 0
    backup "$2"
    mkdir -p "$(dirname "$2")"
    ln -s "$1" "$2"
}
# Older dotfiles linked the entire Fish directory into Git. Detach that
# link before editing config, retaining local completions and variables.
if [[ -L "$HOME/.config/fish" ]]; then
    previous_fish=''
    if [[ -d "$HOME/.config/fish" ]]; then previous_fish=$(cd "$HOME/.config/fish" && pwd -P); fi
    backup "$HOME/.config/fish"
    # Do not put subsequent file backups beneath the saved directory symlink.
    backup_dir=''
    mkdir -p "$HOME/.config/fish"
    if [[ -n $previous_fish ]]; then cp -R "$previous_fish/." "$HOME/.config/fish/"; fi
fi
mkdir -p "$HOME/.config/fish"
config=$(mktemp)
# Fish single-quoted paths only need backslash and quote escaping.
escaped_root=$(printf '%s' "$AC_ROOT" | sed "s/\\\\/\\\\\\\\/g; s/'/\\\\'/g")
printf "# Managed by scripts/mac-family-setup.sh\nsource '%s/dotfiles/fish/native-macos.fish'\n" "$escaped_root" > "$config"
if ! cmp -s "$config" "$HOME/.config/fish/config.fish"; then
    backup "$HOME/.config/fish/config.fish"
    mv "$config" "$HOME/.config/fish/config.fish"
else
    rm "$config"
fi
link "$AC_ROOT/dotfiles/dot_config/nvim" "$HOME/.config/nvim"
link "$AC_ROOT/dotfiles/dot_config/starship.toml" "$HOME/.config/starship.toml"
link "$AC_ROOT/dotfiles/dot_config/emacs.el" "$HOME/.emacs"
if [[ $mode == --install ]]; then
    fish_bin=$(command -v fish)
    if ! grep -Fxq "$fish_bin" /etc/shells; then printf '%s\n' "$fish_bin" | sudo tee -a /etc/shells >/dev/null; fi
    login_shell=$(dscl . -read "/Users/$(id -un)" UserShell | awk '{print $2}')
    if [[ $login_shell != "$fish_bin" ]]; then sudo dscl . -change "/Users/$(id -un)" UserShell "$login_shell" "$fish_bin"; fi
    if [[ -x "$HOME/.local/bin/claude-sleep" ]]; then
        bash "$AC_ROOT/slab/install.sh" --sleep-control-only
    fi
    exec bash "$0" --check
fi
echo 'Dotfiles linked. Run --install for packages and login shell, or --check to audit.'
