#!/bin/zsh
set -euo pipefail

script_dir=${0:A:h}
html_file=${1:-"$PWD/index.html"}

if [[ ! -f "$html_file" ]]; then
  print -u2 "publish-gym: HTML file not found: $html_file"
  print -u2 "usage: ./publish-gym.command [path/to/index.html]"
  exit 2
fi

# Prefer an already-exported token, then a dedicated env file beside this
# script or in the current project. Parse only the one expected key instead of
# sourcing the file as shell code.
token=${GYM_PUBLISH_TOKEN:-}
if [[ -z "$token" ]]; then
  for env_file in \
    "${GYM_ENV_FILE:-}" \
    "$script_dir/gym.anthonyzollo.env" \
    "$PWD/gym.anthonyzollo.env" \
    "$PWD/.env"
  do
    [[ -n "$env_file" && -f "$env_file" ]] || continue
    token=$(sed -n 's/^GYM_PUBLISH_TOKEN=//p' "$env_file" | head -1)
    [[ -n "$token" ]] && break
  done
fi

if [[ ! "$token" =~ '^[0-9a-f]{64}$' ]]; then
  print -u2 "publish-gym: GYM_PUBLISH_TOKEN is missing or invalid."
  print -u2 "Put gym.anthonyzollo.env beside this script or in the project folder."
  exit 2
fi

print "Publishing ${html_file} → https://gym.anthonyzollo.com/"

# Retries cover temporary DNS propagation, resolver, and connection failures.
response=$(curl --silent --show-error --fail-with-body \
  --retry 5 \
  --retry-all-errors \
  --retry-delay 2 \
  --connect-timeout 15 \
  --request PUT \
  --header "Authorization: Bearer $token" \
  --header "Content-Type: text/html; charset=utf-8" \
  --data-binary "@$html_file" \
  https://gym.anthonyzollo.com/api/publish-gym)

unset token
print "$response"
print "Published. Open: https://gym.anthonyzollo.com/"
