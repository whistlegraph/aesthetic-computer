#!/usr/bin/env fish
# Start AT user-pages dev server
# Usage: ./dev-user-pages.fish

set script_dir (dirname (status --current-filename))
set root_dir (realpath "$script_dir/..")

cd $root_dir
node scripts/dev-user-pages.mjs
