#!/usr/bin/env bash
# Include the generated, self-contained piece rather than unresolved imports.
set -euo pipefail
native_dir="$(cd "$(dirname "$0")/.." && pwd)"
repo_dir="$(cd "$native_dir/../.." && pwd)"
target_dir="${1:?Usage: bundle-oskiewar.sh initramfs-root}"
node "$native_dir/tools/oskiewar-live.mjs" --build "$target_dir"
mkdir -p "$target_dir/fonts" "$target_dir/usr/share/glvnd/egl_vendor.d"
cp "$repo_dir/system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf" "$target_dir/fonts/"
# libEGL uses this discovery file even when libEGL_mesa.so.0 is installed.
printf '%s\n' '{"file_format_version":"1.0.0","ICD":{"library_path":"libEGL_mesa.so.0"}}' \
  > "$target_dir/usr/share/glvnd/egl_vendor.d/50_mesa.json"
