#!/usr/bin/env bash
# AC performance guard Git shim. All commands except worktree add pass through.
set -euo pipefail
source_path="${BASH_SOURCE[0]}"
while [[ -L "$source_path" ]]; do
    source_dir="$(cd "$(dirname "$source_path")" && pwd)"
    source_path="$(readlink "$source_path")"
    [[ "$source_path" == /* ]] || source_path="$source_dir/$source_path"
done
source_dir="$(cd "$(dirname "$source_path")" && pwd)"
real_git=/usr/bin/git
[[ ! -f "$source_dir/real-git" ]] || IFS= read -r real_git < "$source_dir/real-git"
for arg in "$@"; do
    if [[ "$arg" == worktree ]]; then
        exec /bin/bash "$source_dir/performance-guard.sh" --git "$@"
    fi
done
exec "$real_git" "$@"
