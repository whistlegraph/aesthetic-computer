#!/bin/sh
set -eu

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/ac-xbox-tests.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT INT TERM

node --check "$repo_dir/xbox/live/controller-probe.js"
node --check "$repo_dir/xbox/live/native-showcase.js"
node "$repo_dir/xbox/live/tests/controller-probe.test.mjs"

clang++ -std=c++20 -Wall -Wextra \
  -I "$repo_dir/xbox/runtime/include" \
  "$repo_dir/xbox/runtime/tests/runtime_contract.cpp" \
  -o "$build_dir/runtime-contract"
"$build_dir/runtime-contract"

qjs="$repo_dir/xbox/native-bios/third_party/quickjs-ng"
for source_file in quickjs.c dtoa.c libregexp.c libunicode.c; do
  clang -std=c11 -O2 -DQUICKJS_NG_BUILD \
    -I "$qjs" -c "$qjs/$source_file" -o "$build_dir/${source_file%.c}.o"
done
clang++ -std=c++17 -O2 -Wall -Wextra \
  -I "$repo_dir/xbox/native-bios" -I "$repo_dir/xbox/runtime/include" -I "$qjs" \
  "$repo_dir/xbox/native-bios/QuickJsEngine.cpp" \
  "$repo_dir/xbox/native-bios/tests/quickjs_engine_smoke.cpp" \
  "$build_dir/quickjs.o" "$build_dir/dtoa.o" \
  "$build_dir/libregexp.o" "$build_dir/libunicode.o" \
  -lm -lpthread -o "$build_dir/quickjs-smoke"
"$build_dir/quickjs-smoke"

echo "xbox native preflight: all portable tests passed"
