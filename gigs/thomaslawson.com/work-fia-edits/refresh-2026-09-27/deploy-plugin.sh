#!/bin/bash
# deploy-plugin.sh — push plugin/tl-design-refresh.zip to thomaslawson.com over SSH.
# Run by @jeffrey (not the agent). Reads the SSH host/user from the vault
# credentials without printing secrets, backs up the live plugin dir, unpacks the
# new build in place, and compares the deployed php sha256 with the local build.
#
#   ./deploy-plugin.sh            # deploy
#   TL_MEDIA_DIR=dir ./deploy-plugin.sh   # also upload media to uploads/tl-refresh/
#   ./deploy-plugin.sh --keys     # list credential field NAMES only, if host/user lookup fails
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
vault="/Users/jas/aesthetic-computer/vault/gigs/thomaslawson.com"
key="$vault/ssh/thomaslawson_ed25519"
zip="$here/plugin/tl-design-refresh.zip"

read -r host user < <(python3 - "$vault/credentials.json" "${1:-}" <<'EOF'
import json, sys
d = json.load(open(sys.argv[1]))
flat = {}
def walk(o, p=""):
    if isinstance(o, dict):
        for k, v in o.items(): walk(v, f"{p}.{k}" if p else k)
    else:
        flat[p.lower()] = o
walk(d)
if sys.argv[2] == "--keys":
    print(" ".join(sorted(flat)), file=sys.stderr); print("- -"); sys.exit()
def pick(*needles, avoid=("pass", "secret", "token", "key")):
    for n in needles:
        for k, v in flat.items():
            if any(a in k for a in avoid): continue
            if n in k and isinstance(v, str) and v: return v
    return ""
host = pick("ssh.host", "ssh_host", "sshhost", "server", "host", "ip")
user = pick("ssh.user", "ssh_user", "sshuser", "cpanel.user", "username", "user")
print(host or "-", user or "-")
EOF
)
[ "${1:-}" = "--keys" ] && exit 0
if [ "$host" = "-" ] || [ "$user" = "-" ]; then echo "could not resolve ssh host/user (run with --keys)"; exit 1; fi

ssh_opts=(-i "$key" -o IdentitiesOnly=yes -o BatchMode=yes -o ConnectTimeout=15 -o StrictHostKeyChecking=accept-new)
php="$here/.deploy-tmp/tl-design-refresh.php"
mkdir -p "$here/.deploy-tmp" "$here/backups"
unzip -p "$zip" tl-design-refresh/tl-design-refresh.php > "$php"
stamp=$(date +%Y%m%d-%H%M%S)
# Shell access is disabled on this GoDaddy account, so no scp/ssh: SFTP only.
# The plugin is a single php file, so swap it in place.
remote_dir="${TL_PLUGIN_DIR:-public_html/wp-content/plugins/tl-design-refresh}"
echo "→ sftp $user@$host:$remote_dir"
sftp "${ssh_opts[@]}" -b - "$user@$host" <<SFTP
cd $remote_dir
get tl-design-refresh.php $here/backups/tl-design-refresh-$stamp.php
put $php tl-design-refresh.php
get tl-design-refresh.php $here/.deploy-tmp/served.php
SFTP
echo "backup of previous live file: backups/tl-design-refresh-$stamp.php"
# Optional: TL_MEDIA_DIR=<dir> also puts every file in <dir> into
# wp-content/uploads/tl-refresh/ (media the plugin references by URL).
if [ -n "${TL_MEDIA_DIR:-}" ]; then
  media_remote="${TL_MEDIA_REMOTE:-public_html/wp-content/uploads/tl-refresh}"
  { echo "-mkdir $media_remote"; echo "cd $media_remote"; for f in "$TL_MEDIA_DIR"/*; do [ -f "$f" ] && echo "put \"$f\""; done; echo "ls -l"; } | sftp "${ssh_opts[@]}" -b - "$user@$host"
fi
echo "live   sha256: $(shasum -a 256 "$here/.deploy-tmp/served.php" | cut -d' ' -f1)"
echo "local  sha256: $(shasum -a 256 "$php" | cut -d' ' -f1)"
grep -m1 -i 'Version:' "$here/.deploy-tmp/served.php"
