#!/usr/bin/env bash

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CLI="$PROJECT_DIR/bin/aesthetic"
TEST_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/aesthetic-code-test.XXXXXX")"
STUB_BIN="$TEST_ROOT/bin"
CONFIG_DIR="$TEST_ROOT/config"
WORK_DIR="$TEST_ROOT/work space"

cleanup() {
    case "$TEST_ROOT" in
        "${TMPDIR:-/tmp}"/aesthetic-code-test.*|/private/tmp/aesthetic-code-test.*)
            rm -rf "$TEST_ROOT"
            ;;
    esac
}
trap cleanup EXIT

mkdir -p "$STUB_BIN" "$CONFIG_DIR" "$WORK_DIR"
WORK_DIR="$(cd "$WORK_DIR" && pwd -P)"

stub_agent() {
    local name="$1"
    cat > "$STUB_BIN/$name" <<'EOF'
#!/usr/bin/env bash
printf 'stub=%s\n' "${0##*/}"
printf 'directory=%s\n' "$PWD"
printf 'aesthetic_code=%s\n' "${AESTHETIC_CODE:-}"
printf 'network_mode=%s\n' "${AESTHETIC_CODE_NETWORK_MODE:-}"
printf 'arguments='
printf '<%s>' "$@"
printf '\n'
EOF
    chmod +x "$STUB_BIN/$name"
}

stub_agent codex-slab
stub_agent codex
stub_agent claude
cat > "$STUB_BIN/ollama" <<'EOF'
#!/usr/bin/env bash
if [[ "${1:-}" == "list" ]]; then
    printf 'NAME ID SIZE MODIFIED\n'
    printf 'qwen2.5-coder:7b abc 4GB now\n'
    exit 0
fi
if [[ "${1:-}" == "show" ]]; then
    [[ "${2:-}" == "qwen2.5-coder:7b" ]] && exit 0
    exit 1
fi
printf 'stub=ollama\n'
printf 'directory=%s\n' "$PWD"
printf 'aesthetic_code=%s\n' "${AESTHETIC_CODE:-}"
printf 'network_mode=%s\n' "${AESTHETIC_CODE_NETWORK_MODE:-}"
printf 'arguments='
printf '<%s>' "$@"
printf '\n'
EOF
chmod +x "$STUB_BIN/ollama"

run_cli() {
    PATH="$STUB_BIN:/usr/bin:/bin" \
        AESTHETIC_CODE_CONFIG_DIR="$CONFIG_DIR" \
        "$CLI" "$@"
}

assert_contains() {
    local output="$1"
    local expected="$2"
    [[ "$output" == *"$expected"* ]] || {
        printf 'Expected output to contain: %s\nActual output:\n%s\n' "$expected" "$output" >&2
        exit 1
    }
}

output="$(run_cli --version)"
assert_contains "$output" 'Aesthetic Code 0.1.0'

output="$(run_cli codex "$WORK_DIR" -- resume --last)"
assert_contains "$output" 'stub=codex-slab'
assert_contains "$output" "directory=$WORK_DIR"
assert_contains "$output" 'aesthetic_code=1'
assert_contains "$output" 'arguments=<resume><--last>'

output="$(run_cli claude here -- --model sonnet)"
assert_contains "$output" 'stub=claude'
assert_contains "$output" 'arguments=<--model><sonnet>'

run_cli model qwen2.5-coder:7b >/dev/null
output="$(run_cli local "$WORK_DIR" -- --verbose)"
assert_contains "$output" 'stub=claude'
assert_contains "$output" 'network_mode=local-only'
assert_contains "$output" 'arguments=<--model><qwen2.5-coder:7b><--no-chrome><--disallowedTools><WebFetch,WebSearch><--verbose>'

if run_cli model qwen3-coder:cloud >/dev/null 2>&1; then
    printf 'Expected a cloud model in local mode to fail.\n' >&2
    exit 1
fi

run_cli use claude >/dev/null
output="$(run_cli "$WORK_DIR")"
assert_contains "$output" 'stub=claude'

if run_cli use unknown >/dev/null 2>&1; then
    printf 'Expected an invalid agent to fail.\n' >&2
    exit 1
fi

printf 'CLI tests passed.\n'
