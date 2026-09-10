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
assert_contains "$output" 'Aesthetic Code 0.4.0'

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
assert_contains "$output" 'account: '

# Account commands read the shared ~/.ac-token under $HOME.
FAKE_HOME="$TEST_ROOT/home"
mkdir -p "$FAKE_HOME"
if output="$(HOME="$FAKE_HOME" "$CLI" whoami 2>&1)"; then
    printf 'Expected whoami to exit non-zero when signed out.\n' >&2
    exit 1
fi
assert_contains "$output" 'not signed in'

printf '{"access_token":"t","user":{"handle":"tester"}}' > "$FAKE_HOME/.ac-token"
output="$(HOME="$FAKE_HOME" "$CLI" whoami)"
assert_contains "$output" '@tester'

printf 'export function paint() {}\n' > "$WORK_DIR/smiley.mjs"
output="$(HOME="$FAKE_HOME" AESTHETIC_CODE_DRY_RUN=1 "$CLI" publish "$WORK_DIR/smiley.mjs")"
assert_contains "$output" 'https://aesthetic.computer/@tester/smiley'

if HOME="$FAKE_HOME" AESTHETIC_CODE_DRY_RUN=1 "$CLI" publish >/dev/null 2>&1; then
    printf 'Expected publish without a file to fail.\n' >&2
    exit 1
fi

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" --runtime lisp "$WORK_DIR")"
assert_contains "$output" 'runtime=lisp'

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" "$WORK_DIR")"
assert_contains "$output" 'runtime=mjs'

# Publishing is outward-facing, so it is off unless this session asked for it —
# by flag or by environment, with the flag able to say no to the environment.
assert_contains "$output" 'autopublish=off'

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" --autopublish "$WORK_DIR")"
assert_contains "$output" 'autopublish=on'

output="$(AESTHETIC_CODE_DRY_RUN=1 AESTHETIC_CODE_AUTOPUBLISH=1 "$CLI" "$WORK_DIR")"
assert_contains "$output" 'autopublish=on'

output="$(AESTHETIC_CODE_DRY_RUN=1 AESTHETIC_CODE_AUTOPUBLISH=1 "$CLI" --no-autopublish "$WORK_DIR")"
assert_contains "$output" 'autopublish=off'

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" "$WORK_DIR")"

# Claude is the default engine bridge, on Fable; Codex stays selectable.
assert_contains "$output" 'backend=claude'
assert_contains "$output" 'model=claude-opus-5'

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" --backend codex "$WORK_DIR")"
assert_contains "$output" 'backend=codex'

output="$(AESTHETIC_CODE_DRY_RUN=1 "$CLI" --backend claude --model claude-opus-5 "$WORK_DIR")"
assert_contains "$output" 'model=claude-opus-5'

if AESTHETIC_CODE_DRY_RUN=1 "$CLI" --backend gemini "$WORK_DIR" >/dev/null 2>&1; then
    printf 'Expected an unknown backend to fail.\n' >&2
    exit 1
fi

output="$($CLI doctor)"
assert_contains "$output" 'engine bridge claude:'
assert_contains "$output" 'engine bridge codex:'
assert_contains "$output" 'default engine bridge: claude (claude-opus-5)'

if AESTHETIC_CODE_DRY_RUN=1 "$CLI" --runtime rust "$WORK_DIR" >/dev/null 2>&1; then
    printf 'Expected an unknown runtime to fail.\n' >&2
    exit 1
fi

if AESTHETIC_CODE_DRY_RUN=1 "$CLI" claude >/dev/null 2>&1; then
    printf 'Expected the removed provider shortcut to fail.\n' >&2
    exit 1
fi

if AESTHETIC_CODE_DRY_RUN=1 "$CLI" "$WORK_DIR" extra >/dev/null 2>&1; then
    printf 'Expected extra launcher arguments to fail.\n' >&2
    exit 1
fi

printf 'CLI tests passed.\n'
