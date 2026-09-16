#!/usr/bin/env bash
# verify-solo-bundle.sh — refuse to ship a Slab that only works on this Mac.
#
# Menu Band shipped a DMG once whose Resources were missing the SwiftPM module
# bundle. It ran perfectly on the machine that built it and crashed on launch
# everywhere else, because the accessor fell back to a path inside the
# developer's own .build directory. This is the check that was written
# afterwards, in Slab's shape.
#
# What it can prove, it proves from the bundle alone: the parts are present,
# nothing points into a checkout or a home directory, and the binary's linked
# libraries all come from the system. What it cannot prove is TCC behaviour or
# how the app reads on a Mac with no config, so the release script says out
# loud that a real first-run test on a clean machine is still owed.
#
#   ./bin/verify-solo-bundle.sh path/to/Slab.app

set -uo pipefail

GREEN=$'\033[1;32m'; RED=$'\033[1;31m'; YELLOW=$'\033[1;33m'; RESET=$'\033[0m'
pass() { printf "%s  ✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
fail() { printf "%s  ✗ %s%s\n" "$RED" "$1" "$RESET"; FAILURES=$((FAILURES+1)); }
note() { printf "%s  ! %s%s\n" "$YELLOW" "$1" "$RESET"; }

APP="${1:-}"
[[ -d "${APP}" ]] || { echo "usage: $0 path/to/Slab.app" >&2; exit 2; }
FAILURES=0

BIN="${APP}/Contents/MacOS/slab-menubar"
RES="${APP}/Contents/Resources"
SOLO="${RES}/solo"

# ── Parts present ─────────────────────────────────────────────────────────
[[ -x "${BIN}" ]]                                 && pass "executable"            || fail "missing executable"
[[ -f "${APP}/Contents/Info.plist" ]]             && pass "Info.plist"            || fail "missing Info.plist"
[[ -f "${RES}/AppIcon.icns" ]]                    && pass "icon"                  || note "no AppIcon.icns (cosmetic)"
[[ -x "${SOLO}/bin/slab-seed-terminal" ]]         && pass "terminal seeder"       || fail "missing solo/bin/slab-seed-terminal"
[[ -f "${SOLO}/seed/terminal-profiles.plist" ]]   && pass "terminal profiles"     || fail "missing solo/seed/terminal-profiles.plist"
[[ -f "${SOLO}/seed/terminal-profiles.list" ]]    && pass "profile manifest"      || fail "missing solo/seed/terminal-profiles.list"
compgen -G "${SOLO}/seed/fonts/*.otf" >/dev/null  && pass "seed fonts"            || fail "missing solo/seed/fonts"

# The seeder resolves its seed as ../seed from its own directory. If that
# relationship breaks, the profiles silently fail to install on first run.
if [[ -f "${SOLO}/bin/../seed/terminal-profiles.plist" ]]; then
    pass "seeder resolves its seed relatively"
else
    fail "seeder cannot reach its seed"
fi

# ── Info.plist expectations ───────────────────────────────────────────────
plist_get() { /usr/libexec/PlistBuddy -c "Print $1" "${APP}/Contents/Info.plist" 2>/dev/null; }
[[ "$(plist_get CFBundleExecutable)" == "slab-menubar" ]] \
    && pass "CFBundleExecutable matches the binary" || fail "CFBundleExecutable mismatch"
[[ "$(plist_get LSUIElement)" == "true" ]] \
    && pass "LSUIElement (no Dock icon)" || fail "LSUIElement not set"
[[ -n "$(plist_get NSAppleEventsUsageDescription)" ]] \
    && pass "Apple Events usage string" || fail "missing NSAppleEventsUsageDescription"
[[ -n "$(plist_get NSScreenCaptureUsageDescription)" ]] \
    && pass "Screen capture usage string" || fail "missing NSScreenCaptureUsageDescription"

# ── Nothing points at this machine ────────────────────────────────────────
# A path into a checkout or a named home directory is the signature of the
# bug this script exists for.
LEAKS="$(strings -a "${BIN}" 2>/dev/null \
    | grep -oE '/Users/[A-Za-z0-9._-]+/[A-Za-z0-9._/-]*' \
    | grep -vE '^/Users/(Shared|\$)' \
    | sort -u | head -5)"
if [[ -z "${LEAKS}" ]]; then
    pass "no absolute paths into a home directory"
else
    fail "binary references developer paths:"
    printf '      %s\n' ${LEAKS}
fi

# ── Linked libraries are all system ───────────────────────────────────────
NONSYSTEM="$(otool -L "${BIN}" 2>/dev/null | tail -n +2 | awk '{print $1}' \
    | grep -vE '^(/usr/lib/|/System/)' | sort -u)"
if [[ -z "${NONSYSTEM}" ]]; then
    pass "links only system libraries"
else
    fail "links non-system libraries:"
    printf '      %s\n' ${NONSYSTEM}
fi

# ── The hook surface actually answers ─────────────────────────────────────
# This is the one runtime check worth making here: if `hook` does not write a
# marker, a downloaded Slab has nothing to draw, and that failure is otherwise
# invisible until somebody installs it.
PROBE="$(mktemp -d)"
trap 'rm -rf "${PROBE}"' EXIT
if echo '{"session_id":"verify","prompt":"bundle self test"}' \
    | SLAB_HOME="${PROBE}" "${BIN}" hook prompt >/dev/null 2>&1 \
    && [[ -f "${PROBE}/state/active-prompts/verify" ]]; then
    pass "hook subcommand writes markers"
else
    fail "hook subcommand did not write a marker"
fi

echo
if [[ "${FAILURES}" -eq 0 ]]; then
    printf "%s  bundle is self-contained%s\n" "$GREEN" "$RESET"
    exit 0
fi
printf "%s  %d check(s) failed%s\n" "$RED" "${FAILURES}" "$RESET"
exit 1
