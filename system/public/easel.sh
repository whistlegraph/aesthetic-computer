#!/bin/sh
# Easel — the Aesthetic Computer terminal editor.
#
#   curl -fsSL https://prompt.ac/easel.sh | sh
#
# What this does, in order, so that reading it takes less time than running it:
#
#   1. checks for node
#   2. downloads one tarball from aesthetic.computer and unpacks it to
#      ~/.local/share/easel
#   3. links `ac` and `easel` into ~/.local/bin
#   4. offers to add ~/.local/bin to your PATH, and says how if you decline
#
# It writes nothing outside your home folder, asks for no privileges, and runs
# no `sudo`. If you would rather not pipe a script into a shell — a reasonable
# position — save it, read it, and run it: it is deliberately short enough to
# read in a minute.
#
# POSIX sh on purpose: this has to run under dash, ash, and busybox, not only
# bash, and a login shell on a stripped container is the least forgiving place
# an install can land.

set -eu

SITE="${EASEL_SITE:-https://aesthetic.computer}"
PREFIX="${EASEL_PREFIX:-$HOME/.local/share/easel}"
BIN="${EASEL_BIN:-$HOME/.local/bin}"
# Node 18 is where fetch, and therefore both engine bridges, stopped needing a
# flag. Below it nothing works and the failure is obscure, so check up front.
MIN_NODE=18

say() { printf '%s\n' "$*"; }
die() { printf '\n  %s\n\n' "$*" >&2; exit 1; }

say ""
say "  Easel — the Aesthetic Computer terminal editor"
say ""

# --- node ------------------------------------------------------------------
command -v node >/dev/null 2>&1 || die "Easel needs Node $MIN_NODE or newer. https://nodejs.org"
NODE_MAJOR=$(node -p 'process.versions.node.split(".")[0]' 2>/dev/null || echo 0)
[ "$NODE_MAJOR" -ge "$MIN_NODE" ] 2>/dev/null \
  || die "Easel needs Node $MIN_NODE or newer; found $(node -v 2>/dev/null || echo none)."
say "  node $(node -v)"

# --- fetch -----------------------------------------------------------------
# curl and wget are both common and neither is universal; accept whichever is
# here rather than making the user install one to install this.
if command -v curl >/dev/null 2>&1; then
  fetch() { curl -fsSL "$1" -o "$2"; }
elif command -v wget >/dev/null 2>&1; then
  fetch() { wget -qO "$2" "$1"; }
else
  die "Need curl or wget to download Easel."
fi

TMP=$(mktemp -d 2>/dev/null || mktemp -d -t easel)
# Leave nothing behind on any exit path, including a failed download.
trap 'rm -rf "$TMP"' EXIT INT TERM

say "  downloading…"
fetch "$SITE/easel.tar.gz" "$TMP/easel.tar.gz" \
  || die "Could not download $SITE/easel.tar.gz"

# --- install ---------------------------------------------------------------
# Unpack beside the target and swap, so an interrupted install cannot leave a
# half-written Easel where a working one used to be.
mkdir -p "$TMP/unpack"
tar -xzf "$TMP/easel.tar.gz" -C "$TMP/unpack" || die "Could not unpack the download."
[ -f "$TMP/unpack/bin/easel" ] || die "That download does not look like Easel."

mkdir -p "$(dirname "$PREFIX")" "$BIN"
rm -rf "$PREFIX.old"
[ -d "$PREFIX" ] && mv "$PREFIX" "$PREFIX.old"
mv "$TMP/unpack" "$PREFIX"
rm -rf "$PREFIX.old"

chmod +x "$PREFIX/bin/easel"
ln -sfn "$PREFIX/bin/easel" "$BIN/easel"
ln -sfn "$PREFIX/bin/easel" "$BIN/ac"

VERSION=$(node -p "require('$PREFIX/package.json').version" 2>/dev/null || echo "?")
say "  installed Easel $VERSION to $PREFIX"

# --- path ------------------------------------------------------------------
# Never edit a shell profile without saying so. The check is on the literal
# directory rather than `command -v`, so a stale hash table cannot fake it.
case ":$PATH:" in
  *":$BIN:"*) ON_PATH=1 ;;
  *) ON_PATH=0 ;;
esac

if [ "$ON_PATH" = "0" ]; then
  say ""
  say "  $BIN is not on your PATH. Add this to your shell profile:"
  say ""
  say "      export PATH=\"\$HOME/.local/bin:\$PATH\""
  say ""
  say "  …or run Easel by its full path: $BIN/ac"
fi

say ""
say "  Run  ac  to start. Inside it, /login connects your Aesthetic Computer"
say "  handle — or makes you one — and /help lists everything else."
say ""
