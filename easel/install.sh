#!/usr/bin/env bash

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_DIR="${EASEL_BIN_DIR:-${HOME}/.local/bin}"
CONFIG_DIR="${EASEL_CONFIG_DIR:-${HOME}/.config/easel}"
FISH_CONFIG="${XDG_CONFIG_HOME:-${HOME}/.config}/fish/config.fish"
ZSH_PROFILE="${ZDOTDIR:-${HOME}}/.zprofile"

mkdir -p "$BIN_DIR" "$CONFIG_DIR"
chmod +x "$PROJECT_DIR/bin/easel"
ln -sfn "$PROJECT_DIR/bin/easel" "$BIN_DIR/easel"
ln -sfn "$PROJECT_DIR/bin/easel" "$BIN_DIR/ac"

# The tool was called `aesthetic` before it was called Easel, and that name
# belongs to the platform helper it displaced. Take it back only if it is still
# our own symlink — never touch a real file someone else put there.
if [[ -L "$BIN_DIR/aesthetic" ]] && [[ "$(readlink "$BIN_DIR/aesthetic")" == *"/bin/aesthetic" || "$(readlink "$BIN_DIR/aesthetic")" == *"/bin/easel" ]]; then
    rm -f "$BIN_DIR/aesthetic"
fi

# The tool shipped as Aesthetic Code first, and its Fish block named that path.
# The block is inert once the path is gone — `test -f` simply fails — but it
# lingers on every machine that ever installed the old name, so take it out.
if [[ -f "$FISH_CONFIG" ]] && grep -Fq '# aesthetic-code:start' "$FISH_CONFIG"; then
    /usr/bin/sed -i '' '/# aesthetic-code:start/,/# aesthetic-code:end/d' "$FISH_CONFIG"
fi

if [[ -f "$FISH_CONFIG" ]] && ! grep -Fq '# easel:start' "$FISH_CONFIG"; then
    {
        printf '\n# easel:start\n'
        printf 'if test -f %q\n' "$PROJECT_DIR/shell/easel.fish"
        printf '    source %q\n' "$PROJECT_DIR/shell/easel.fish"
        printf 'end\n'
        printf '# easel:end\n'
    } >> "$FISH_CONFIG"
fi

if [[ -f "$ZSH_PROFILE" ]] && ! grep -Fq '# easel:path:start' "$ZSH_PROFILE"; then
    {
        printf '\n# easel:path:start\n'
        printf 'export PATH="$HOME/.local/bin:$PATH"\n'
        printf '# easel:path:end\n'
    } >> "$ZSH_PROFILE"
fi

case ":${PATH}:" in
    *":${BIN_DIR}:"*) ;;
    *) printf 'Add %s to PATH to use the commands.\n' "$BIN_DIR" >&2 ;;
esac

printf 'Installed Easel:\n'
printf '  %s\n' "$BIN_DIR/ac"
printf '  %s\n' "$BIN_DIR/easel"
