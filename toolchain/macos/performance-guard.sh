#!/usr/bin/env bash
# AC performance guard entry point; keep this beside performance_guard.py.
set -euo pipefail
source_path="${BASH_SOURCE[0]}"
while [[ -L "$source_path" ]]; do
    source_dir="$(cd "$(dirname "$source_path")" && pwd)"
    source_path="$(readlink "$source_path")"
    [[ "$source_path" == /* ]] || source_path="$source_dir/$source_path"
done
source_dir="$(cd "$(dirname "$source_path")" && pwd)"
exec python3 "$source_dir/performance_guard.py" "$@"
