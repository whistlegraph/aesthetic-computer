#!/usr/bin/env bash

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_DIR="${AESTHETIC_CODE_BIN_DIR:-${HOME}/.local/bin}"
CONFIG_DIR="${AESTHETIC_CODE_CONFIG_DIR:-${HOME}/.config/aesthetic-code}"
FISH_CONFIG="${XDG_CONFIG_HOME:-${HOME}/.config}/fish/config.fish"
ZSH_PROFILE="${ZDOTDIR:-${HOME}}/.zprofile"

mkdir -p "$BIN_DIR" "$CONFIG_DIR"
chmod +x "$PROJECT_DIR/bin/aesthetic"
ln -sfn "$PROJECT_DIR/bin/aesthetic" "$BIN_DIR/aesthetic"
ln -sfn "$PROJECT_DIR/bin/aesthetic" "$BIN_DIR/ac"

if [[ ! -f "$CONFIG_DIR/config" ]]; then
    if command -v ollama >/dev/null 2>&1 \
        && ollama list 2>/dev/null | awk 'NR > 1 && $1 !~ /:cloud/ && $1 !~ /-cloud/ { found=1 } END { exit !found }'; then
        default_agent="local"
    elif command -v codex >/dev/null 2>&1 || command -v codex-slab >/dev/null 2>&1; then
        default_agent="codex"
    else
        default_agent="claude"
    fi
    umask 077
    printf 'default_agent = "%s"\n' "$default_agent" > "$CONFIG_DIR/config"
fi

if [[ -f "$FISH_CONFIG" ]] && ! grep -Fq '# aesthetic-code:start' "$FISH_CONFIG"; then
    {
        printf '\n# aesthetic-code:start\n'
        printf 'if test -f %q\n' "$PROJECT_DIR/shell/aesthetic-code.fish"
        printf '    source %q\n' "$PROJECT_DIR/shell/aesthetic-code.fish"
        printf 'end\n'
        printf '# aesthetic-code:end\n'
    } >> "$FISH_CONFIG"
fi

if [[ -f "$ZSH_PROFILE" ]] && ! grep -Fq '# aesthetic-code:path:start' "$ZSH_PROFILE"; then
    {
        printf '\n# aesthetic-code:path:start\n'
        printf 'export PATH="$HOME/.local/bin:$PATH"\n'
        printf '# aesthetic-code:path:end\n'
    } >> "$ZSH_PROFILE"
fi

case ":${PATH}:" in
    *":${BIN_DIR}:"*) ;;
    *) printf 'Add %s to PATH to use the commands.\n' "$BIN_DIR" >&2 ;;
esac

printf 'Installed Aesthetic Code:\n'
printf '  %s\n' "$BIN_DIR/ac"
printf '  %s\n' "$BIN_DIR/aesthetic"
