#!/bin/sh
# Juke is a Menu Band feature; this compatibility entry point installs the
# owning application and its control-only `jukewizard` shell command.
set -eu

ROOT="$(cd "$(dirname "$0")" && pwd)"
exec "$ROOT/../slab/menuband/install.sh"
