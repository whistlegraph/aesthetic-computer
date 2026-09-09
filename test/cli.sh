#!/usr/bin/env bash

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CLI="$PROJECT_DIR/bin/aesthetic"
TEST_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/aesthetic-code-test.XXXXXX")"
WORK_DIR="$TEST_ROOT/work space"
TEST_BIN="$TEST_ROOT/bin"

cleanup() {
    case "$TEST_ROOT" in
        "${TMPDIR:-/tmp}"/aesthetic-code-test.*|/private/tmp/aesthetic-code-test.*)
            rm -rf "$TEST_ROOT"
            ;;
    esac
}
trap cleanup EXIT

mkdir -p "$WORK_DIR" "$TEST_BIN"
WORK_DIR="$(cd "$WORK_DIR" && pwd -P)"
ln -s "$CLI" "$TEST_BIN/ac"

assert_contains() {
    local output="$1"
    local expected="$2"
    [[ "$output" == *"$expected"* ]] || {
        printf 'Expected output to contain: %s\nActual output:\n%s\n' "$expected" "$output" >&2
        exit 1
    }
}

output="$($CLI --version)"
assert_contains "$output" 'Aesthetic Code 0.2.1'

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" "$WORK_DIR")"
assert_contains "$output" 'interface=aesthetic-code'
assert_contains "$output" "directory=$WORK_DIR"

output="$(AESTHETIC_CODE_DRY_RUN=1 "$TEST_BIN/ac" "$WORK_DIR")"
assert_contains "$output" 'interface=aesthetic-code'
assert_contains "$output" "directory=$WORK_DIR"

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" --resume 00000000-0000-0000-0000-000000000001 --prompt continue "$WORK_DIR")"
assert_contains "$output" 'resume=yes'
assert_contains "$output" 'initial_prompt=yes'

output="$($CLI doctor)"
assert_contains "$output" 'control plane: local'
assert_contains "$output" 'telemetry: off'

if AESTHETIC_CODE_DRY_RUN=1 "$CLI" claude >/dev/null 2>&1; then
    printf 'Expected the removed provider shortcut to fail.\n' >&2
    exit 1
fi

if AESTHETIC_CODE_DRY_RUN=1 "$CLI" "$WORK_DIR" extra >/dev/null 2>&1; then
    printf 'Expected extra launcher arguments to fail.\n' >&2
    exit 1
fi

printf 'CLI tests passed.\n'
