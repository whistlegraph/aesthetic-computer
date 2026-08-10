#!/bin/bash
set -euo pipefail

root="$(cd "$(dirname "$0")" && pwd)"
version="0.24.1"
install_root="$HOME/.local/lib/juked"
bin_dir="$HOME/.local/bin"
config_dir="$HOME/.config/juked"
cache_dir="$HOME/.cache/juked"

command -v cargo >/dev/null || {
  echo "juked: Rust is required (brew install rust)" >&2
  exit 69
}

mkdir -p "$install_root" "$bin_dir" "$config_dir" "$cache_dir"
chmod 700 "$config_dir" "$cache_dir"
cargo install spotify_player --version "$version" --locked \
  --root "$install_root" --no-default-features --features daemon,rodio-backend
# A machine with the checkout can symlink this at the repo so edits to the
# script take effect without reinstalling. Copying over that link would put
# the drift back, so leave a link the way we found it.
if [[ -L "$bin_dir/juked" ]]; then
  echo "juked: $bin_dir/juked is a symlink — leaving it pointed at $(readlink "$bin_dir/juked")"
else
  install -m 0755 "$root/bin/juked" "$bin_dir/juked"
fi
if [[ ! -f "$config_dir/app.toml" ]]; then
  install -m 0600 "$root/config/app.toml" "$config_dir/app.toml"
fi

echo "installed juked -> $bin_dir/juked"
echo "authenticate once: juked authenticate"
