#!/bin/sh
# Aesel — the Aesthetic Computer terminal editor.
#
#   curl -fsSL https://prompt.ac/aesel.sh | sh
#
# (prompt.ac/easel.sh is this same script under the name it had first; the
# only difference is that it fetches easel.tar.gz, the same bytes as
# aesel.tar.gz under the old name.)
#
# What this does, in order, so that reading it takes less time than running it:
#
#   1. checks for node
#   2. downloads one tarball from aesthetic.computer and unpacks it to
#      ~/.local/share/aesel/app
#   3. links `ac`, `a`, `aes`, `aesel` and `easel` into ~/.local/bin
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

# AESEL_* first; EASEL_* is the same setting under the old name.
SITE="${AESEL_SITE:-${EASEL_SITE:-https://aesthetic.computer}}"
# Its own folder, not ~/.local/share/aesel itself: that is where history and
# transcripts live, and an install replaces its folder whole.
PREFIX="${AESEL_PREFIX:-${EASEL_PREFIX:-$HOME/.local/share/aesel/app}}"
BIN="${AESEL_BIN:-${EASEL_BIN:-$HOME/.local/bin}}"
TARBALL=aesel.tar.gz
# Match the runtime APIs used by the desktop harness, including util.parseEnv.
MIN_NODE=22

say() { printf '%s\n' "$*"; }
die() { printf '\n  %s\n\n' "$*" >&2; exit 1; }

say ""
say "  Aesel — the Aesthetic Computer terminal editor"
say ""

# --- node ------------------------------------------------------------------
command -v node >/dev/null 2>&1 || die "Aesel needs Node $MIN_NODE or newer. https://nodejs.org"
NODE_MAJOR=$(node -p 'process.versions.node.split(".")[0]' 2>/dev/null || echo 0)
[ "$NODE_MAJOR" -ge "$MIN_NODE" ] 2>/dev/null \
  || die "Aesel needs Node $MIN_NODE or newer; found $(node -v 2>/dev/null || echo none)."
say "  node $(node -v)"

# --- fetch -----------------------------------------------------------------
# curl and wget are both common and neither is universal; accept whichever is
# here rather than making the user install one to install this.
if command -v curl >/dev/null 2>&1; then
  fetch() { curl -fsSL "$1" -o "$2"; }
elif command -v wget >/dev/null 2>&1; then
  fetch() { wget -qO "$2" "$1"; }
else
  die "Need curl or wget to download Aesel."
fi

TMP=$(mktemp -d 2>/dev/null || mktemp -d -t aesel)
# Leave nothing behind on any exit path, including a failed download.
trap 'rm -rf "$TMP"' EXIT INT TERM

say "  downloading…"
fetch "$SITE/$TARBALL" "$TMP/aesel.tar.gz" \
  || die "Could not download $SITE/$TARBALL"

# --- install ---------------------------------------------------------------
# Unpack beside the target and swap, so an interrupted install cannot leave a
# half-written Aesel where a working one used to be.
mkdir -p "$TMP/unpack"
tar -xzf "$TMP/aesel.tar.gz" -C "$TMP/unpack" || die "Could not unpack the download."
[ -f "$TMP/unpack/bin/easel" ] || die "That download does not look like Aesel."

mkdir -p "$(dirname "$PREFIX")" "$BIN"
rm -rf "$PREFIX.old"
[ -d "$PREFIX" ] && mv "$PREFIX" "$PREFIX.old"
mv "$TMP/unpack" "$PREFIX"
# Whatever the old folder held that a release does not ship comes along. An
# older easel.sh installed into the folder history was kept in, so a plain
# swap would take someone's history with it.
if [ -d "$PREFIX.old" ]; then
  for entry in "$PREFIX.old"/* "$PREFIX.old"/.[!.]*; do
    [ -e "$entry" ] || [ -L "$entry" ] || continue
    name=$(basename "$entry")
    [ -e "$PREFIX/$name" ] || [ -L "$PREFIX/$name" ] || mv "$entry" "$PREFIX/$name"
  done
fi
rm -rf "$PREFIX.old"

chmod +x "$PREFIX/bin/easel" "$PREFIX/bin/aesel" "$PREFIX/bin/easel-desktop" "$PREFIX/bin/a" "$PREFIX/bin/aes"
ln -sfn "$PREFIX/bin/easel" "$BIN/easel"
ln -sfn "$PREFIX/bin/aesel" "$BIN/aesel"
ln -sfn "$PREFIX/bin/a" "$BIN/a"
ln -sfn "$PREFIX/bin/aes" "$BIN/aes"
ln -sfn "$PREFIX/bin/easel" "$BIN/ac"

VERSION=$(node -p "require('$PREFIX/package.json').version" 2>/dev/null || echo "?")
say "  installed Aesel $VERSION to $PREFIX"

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
  say "  …or run Aesel by its full path: $BIN/ac"
fi

say ""
say "  Run  ac  to start. Inside it, /login connects your Aesthetic Computer"
say "  handle — or makes you one — and /help lists everything else."
say ""
