#!/usr/bin/env bash
# dev.sh — the fast loop for working on NCCd.
#
#   ./dev.sh             build, open the development app, and keep it current
#   ./dev.sh --once      build the library once and stop (no app, no watching)
#
# Save a file in Sources/ and, a few seconds later, the running window is the
# new code: same place on the screen, same tab, same draft. Three things make
# that quick:
#
#   1. Sub-compilation. The sources are compiled one file at a time with no
#      optimisation (swiftc -incremental): a change recompiles that file and
#      the files that depend on it, not the whole app.
#   2. Dynamic linking. The app's code is a dynamic library. A small host that
#      never changes (Dev/host.swift) loads it, and loads it again when it has
#      been rebuilt, without leaving the process.
#   3. No Signal engine. The development app shows what is already on this
#      Mac (the logs, the config) and starts no signal-cli, so it never
#      competes with the installed copy for the account and nothing can be
#      sent from a build in progress. NCC_DEV is set for it.
#
# The development app is its own bundle (computer.aesthetic.nccd-dev), so its
# window is not tiled with the installed app's and its settings are apart.
# The wiki shows the club's login unless NCC_WIKI_TOKEN is in the environment.
# Every test aid of the README works here too (NCC_SNAPSHOT_TOP=1 ./dev.sh).
#
# A compile error is printed and the running app is left as it was.
set -uo pipefail
cd "$(dirname "$0")"

OUT="build/dev"
APP="$OUT/NCCd Dev.app"
LIB="$OUT/libNCCd.dylib"
OBJ="$OUT/obj"
mkdir -p "$OBJ" "$APP/Contents/MacOS" "$APP/Contents/Resources"
FRAMEWORKS=(-framework AppKit -framework Network -framework Security -framework CoreImage
            -framework AVFoundation -framework ImageIO -framework SceneKit -framework WebKit -framework UserNotifications)

# The output file map: where each source's object and dependency files go.
map() {
    python3 - "$OBJ" Sources/*.swift <<'PY' > "$OBJ/map.json"
import json, os, sys
obj, files = sys.argv[1], sys.argv[2:]
out = {"": {"swift-dependencies": os.path.join(obj, "module.swiftdeps")}}
for f in files:
    stem = os.path.splitext(os.path.basename(f))[0]
    out[f] = {"object": os.path.join(obj, stem + ".o"),
              "swift-dependencies": os.path.join(obj, stem + ".swiftdeps"),
              "swiftmodule": os.path.join(obj, stem + "~partial.swiftmodule"),
              "dependencies": os.path.join(obj, stem + ".d")}
print(json.dumps(out, indent=1))
PY
}

build() {
    map
    local started=$SECONDS
    if swiftc -incremental -Onone -swift-version 5 -suppress-warnings -parse-as-library \
        -module-name NCCd -emit-library -emit-module -emit-module-path "$OBJ/NCCd.swiftmodule" \
        -output-file-map "$OBJ/map.json" "${FRAMEWORKS[@]}" Sources/*.swift -o "$LIB.new" 2> "$OBJ/errors.txt"; then
        mv -f "$LIB.new" "$LIB"
        echo "› built in $((SECONDS - started))s"
    else
        grep -E "error" -A3 "$OBJ/errors.txt" | head -30
        echo "✗ not built: the running app is as it was"
        return 1
    fi
}

touch "$OBJ/tried"
build || exit 1
[[ "${1:-}" == "--once" ]] && exit 0

# The host: compiled once, and again only when Dev/host.swift itself changes.
HOST="$APP/Contents/MacOS/NCCdDev"
if [[ ! -x "$HOST" || Dev/host.swift -nt "$HOST" ]]; then
    swiftc -O -swift-version 5 -suppress-warnings -framework AppKit Dev/host.swift -o "$HOST" || exit 1
    sed -e 's/computer.aesthetic.ncc-circles/computer.aesthetic.nccd-dev/' -e 's/<string>NCCd<\/string>/<string>NCCd Dev<\/string>/' \
        -e 's/<key>CFBundleExecutable<\/key><string>NCCd Dev<\/string>/<key>CFBundleExecutable<\/key><string>NCCdDev<\/string>/' \
        Resources/Info.plist > "$APP/Contents/Info.plist"
    cp Resources/AppIcon.icns "$APP/Contents/Resources/"
    codesign --force --sign - "$APP" 2>/dev/null
fi

NCC_DEV=1 NCC_LOGGED_OUT="${NCC_LOGGED_OUT-$([[ -n "${NCC_WIKI_TOKEN:-}" ]] || echo ncc)}" "$HOST" "$PWD/$LIB" &
HOSTPID=$!
trap 'kill $HOSTPID 2>/dev/null' EXIT
echo "› NCCd Dev is open. Save a file in Sources/ to see it change. Ctrl-C stops."

# Watch the sources: when one is newer than the library, build again.
while kill -0 $HOSTPID 2>/dev/null; do
    if [[ -n "$(find Sources -name '*.swift' -newer "$OBJ/tried" -print -quit)" ]]; then
        touch "$OBJ/tried"          # a build that fails is not tried again until the next save
        build
    fi
    /bin/sleep 0.4
done
