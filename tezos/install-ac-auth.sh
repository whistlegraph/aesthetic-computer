#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET_DIR="${HOME}/.local/bin"

mkdir -p "${TARGET_DIR}"

write_wrapper() {
    local name="$1"
    local subcommand="$2"
    local target="${TARGET_DIR}/${name}"

    cat > "${target}" <<EOF
#!/usr/bin/env bash
exec node "${SCRIPT_DIR}/ac-login.mjs" ${subcommand} "\$@"
EOF
    chmod +x "${target}"
}

write_wrapper "ac-login" ""
write_wrapper "ac-login-fresh" "fresh"
write_wrapper "ac-logout" "logout"
write_wrapper "ac-status" "status"
write_wrapper "ac-token" "token"

echo "Installed AC auth commands to ${TARGET_DIR}:"
echo "  ac-login"
echo "  ac-login-fresh"
echo "  ac-logout"
echo "  ac-status"
echo "  ac-token"
