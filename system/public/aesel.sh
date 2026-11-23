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
#   1. checks for Node 22+, and if there is none, fetches the official Node
#      build into ~/.local/share/aesel/node (checksum-verified)
#   2. downloads one tarball from aesthetic.computer and unpacks it to
#      ~/.local/share/aesel/app
#   3. links `ac`, `a`, `aes`, `aesel` and `easel` into ~/.local/bin
#   4. offers to add ~/.local/bin to your PATH, and says how if you decline
#   5. names the command to run: `a` (`ac` and `easel` are the same program)
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
# A Node new enough on PATH is used as is. Otherwise Aesel gets its own: the
# official build from nodejs.org, checked against its published SHA-256, in
# ~/.local/share/aesel/node. Only Aesel's launcher puts it on PATH; your shell
# and any other Node stay as they were.
NODE_HOME="${AESEL_NODE_HOME:-$(dirname "$PREFIX")/node}"
NODE_LINE="${AESEL_NODE_LINE:-24}"

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

node_major() { "$1" -p 'process.versions.node.split(".")[0]' 2>/dev/null || echo 0; }

NODE=""
if command -v node >/dev/null 2>&1 && [ "$(node_major node)" -ge "$MIN_NODE" ] 2>/dev/null; then
  NODE=$(command -v node)
elif [ -x "$NODE_HOME/bin/node" ] && [ "$(node_major "$NODE_HOME/bin/node")" -ge "$MIN_NODE" ] 2>/dev/null; then
  NODE="$NODE_HOME/bin/node"
else
  case "$(uname -s)" in
    Darwin) os=darwin ;;
    Linux) os=linux ;;
    *) die "Aesel needs Node $MIN_NODE or newer. Install it from https://nodejs.org and run this again." ;;
  esac
  case "$(uname -m)" in
    x86_64|amd64) arch=x64 ;;
    arm64|aarch64) arch=arm64 ;;
    *) die "Aesel needs Node $MIN_NODE or newer. Install it from https://nodejs.org and run this again." ;;
  esac
  # nodejs.org publishes no musl (Alpine) builds.
  if [ "$os" = linux ] && ls /lib/ld-musl-* >/dev/null 2>&1; then
    die "Aesel needs Node $MIN_NODE or newer: apk add nodejs, then run this again."
  fi
  say "  Node $MIN_NODE+ not found; fetching Node $NODE_LINE for Aesel…"
  DIST="https://nodejs.org/dist/latest-v$NODE_LINE.x"
  fetch "$DIST/SHASUMS256.txt" "$TMP/SHASUMS256.txt" || die "Could not reach nodejs.org."
  line=$(grep " node-v[0-9.]*-$os-$arch.tar.gz\$" "$TMP/SHASUMS256.txt" | head -n 1)
  [ -n "$line" ] || die "nodejs.org has no Node $NODE_LINE build for $os-$arch."
  sum=${line%% *}
  file=${line##* }
  fetch "$DIST/$file" "$TMP/$file" || die "Could not download $DIST/$file"
  if command -v shasum >/dev/null 2>&1; then got=$(shasum -a 256 "$TMP/$file")
  elif command -v sha256sum >/dev/null 2>&1; then got=$(sha256sum "$TMP/$file")
  else die "Need shasum or sha256sum to check the Node download."; fi
  [ "${got%% *}" = "$sum" ] || die "The Node download failed its checksum."
  mkdir -p "$TMP/node"
  tar -xzf "$TMP/$file" -C "$TMP/node" --strip-components 1 || die "Could not unpack Node."
  mkdir -p "$(dirname "$NODE_HOME")"
  rm -rf "$NODE_HOME.old"
  [ -d "$NODE_HOME" ] && mv "$NODE_HOME" "$NODE_HOME.old"
  mv "$TMP/node" "$NODE_HOME"
  rm -rf "$NODE_HOME.old"
  NODE="$NODE_HOME/bin/node"
fi
say "  node $("$NODE" -v)  ($NODE)"

# --- fetch -----------------------------------------------------------------
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

VERSION=$("$NODE" -p "require('$PREFIX/package.json').version" 2>/dev/null || echo "?")
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
  say "  …or run Aesel by its full path: $BIN/a"
fi

# `a` is the name to give: macOS ships an `ac` of its own (login accounting,
# /usr/sbin/ac) that often answers first, and `aesel` opens the Mac app rather
# than the terminal. Should something else answer to `a`, give the full path.
START=a
[ "$(command -v a 2>/dev/null)" = "$BIN/a" ] || START="$BIN/a"

say ""
say "  Run  $START  to start. Inside it, /login connects your Aesthetic Computer"
say "  handle — or makes you one — and /help lists everything else."
say ""
