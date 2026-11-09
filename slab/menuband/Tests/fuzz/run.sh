#!/usr/bin/env bash
# Run on a compute Mac (Chicken for this seat), using guarded swift on PATH.
# Exercises isolated modules; never launches Menu Band or opens audio output.
set -euo pipefail
package_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
artifacts="${MB_STRESS_ARTIFACTS:-$(mktemp -d "${TMPDIR:-/tmp}/menuband-stress.XXXXXX")}"
mkdir -p "$artifacts"
artifacts="$(cd "$artifacts" && pwd)"
runs="${MB_STRESS_RUNS:-50000}"
seed="${MB_STRESS_SEED:-0x4d4246555a5a}"
filter='ModuleStressTests|UIResponsivenessTests|ArtworkAndLatencyTests|PerformanceIsolationTests'
cd "$package_dir"
for sanitizer in address thread; do
    echo "Running Swift $sanitizer checks; log: $artifacts/swift-$sanitizer.log"
    if ! swift test --jobs 2 --sanitize="$sanitizer" --filter "$filter" > "$artifacts/swift-$sanitizer.log" 2>&1; then
        tail -60 "$artifacts/swift-$sanitizer.log"
        exit 1
    fi
done
sources=(Tests/fuzz/realtime.c Sources/CRealtimeAudio/MBRealtimeQueue.c Sources/CRealtimeAudio/MBLatencyProbe.c)
xcrun clang -g -O1 -DMB_STANDALONE_FUZZ -fsanitize=address,undefined \
    -fprofile-instr-generate -fcoverage-mapping -I Sources/CRealtimeAudio/include \
    "${sources[@]}" -o "$artifacts/realtime-fuzz"
LLVM_PROFILE_FILE="$artifacts/coverage.profraw" \
    "$artifacts/realtime-fuzz" "$runs" "$seed" > "$artifacts/fuzz.log" 2>&1
xcrun llvm-profdata merge -sparse "$artifacts/coverage.profraw" -o "$artifacts/coverage.profdata"
xcrun llvm-cov report "$artifacts/realtime-fuzz" -instr-profile="$artifacts/coverage.profdata" \
    Sources/CRealtimeAudio/MBRealtimeQueue.c Sources/CRealtimeAudio/MBLatencyProbe.c \
    > "$artifacts/coverage.txt"
# A separate unsanitized process lets macOS inspect the native allocator at exit.
xcrun clang -g -O1 -DMB_STANDALONE_FUZZ -I Sources/CRealtimeAudio/include \
    "${sources[@]}" -o "$artifacts/realtime-leaks"
leaks --noContent --atExit -- "$artifacts/realtime-leaks" 1000 "$seed" > "$artifacts/leaks.log" 2>&1
if ! grep -q ': 0 leaks for 0 total leaked bytes' "$artifacts/leaks.log"; then
    cat "$artifacts/leaks.log"
    exit 1
fi
cat "$artifacts/fuzz.log" "$artifacts/coverage.txt"
echo "Leak report: $artifacts/leaks.log"
echo "Artifacts: $artifacts"
