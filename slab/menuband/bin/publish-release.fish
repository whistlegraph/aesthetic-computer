#!/usr/bin/env fish
# publish-release.fish — promote a freshly-built (and notarized) DMG to
# the live website by:
#
#   1. Reading the version from Menu-Band-<v>.dmg's filename (or arg).
#   2. Computing size + sha256 of the local DMG.
#   3. Updating system/public/menuband/manifest.json so:
#        - `latest` points at this version
#        - the version's row carries the new size / sha256 / releasedAt
#        - older versions remain in `versions[]` for archival lookups
#
# This does NOT upload the DMG. Use the existing flow to push the file to
# assets.aesthetic.computer/menuband/Menu-Band-<v>.dmg first; this script
# is the bookkeeping step that flips the website over.
#
# Why a manifest?
#   The website's download button + the /api/menuband-downloads counter
#   both read this file. The counter validates POSTed `version` strings
#   against `versions[].version`, so adding a new release here is also
#   what authorizes the counter to start tallying it. Without this step
#   the new version would be advertised by no one.
#
# Usage:
#   ./bin/publish-release.fish                      # auto-detects newest DMG
#   ./bin/publish-release.fish 0.9.2                # explicit version
#   ./bin/publish-release.fish path/to/Menu-Band-0.9.2.dmg

set -l SCRIPT_DIR (cd (dirname (status filename)); and pwd)
set -l MB_DIR (cd $SCRIPT_DIR/..; and pwd)
set -l REPO_ROOT (cd $MB_DIR/../..; and pwd)
set -l MANIFEST $REPO_ROOT/system/public/menuband/manifest.json

set -l CYAN (set_color cyan)
set -l GREEN (set_color green)
set -l RED (set_color red)
set -l RESET (set_color normal)

function say
    echo $CYAN"• "$argv$RESET
end
function ok
    echo $GREEN"✓ "$argv$RESET
end
function err
    echo $RED"✗ "$argv$RESET
end

if not test -f $MANIFEST
    err "manifest not found: $MANIFEST"
    exit 1
end

# ── Resolve which DMG / version to promote. ─────────────────────────────
set -l DMG ""
set -l VERSION ""

if test (count $argv) -eq 0
    # Auto-pick the newest Menu-Band-*.dmg in slab/menuband/.
    set -l candidates $MB_DIR/Menu-Band-*.dmg
    if test (count $candidates) -eq 0
        err "no Menu-Band-*.dmg found in $MB_DIR — build one first"
        exit 1
    end
    # Sort by mtime descending and take the newest.
    set DMG (ls -t $candidates | head -1)
else if test -f $argv[1]
    set DMG $argv[1]
else
    # Treat the arg as a bare version.
    set VERSION $argv[1]
    set DMG $MB_DIR/Menu-Band-$VERSION.dmg
end

if test -z "$VERSION"
    # Pull version out of the filename: Menu-Band-<v>.dmg
    set -l base (basename $DMG .dmg)
    set VERSION (string replace -r '^Menu-Band-' '' -- $base)
end

if not test -f $DMG
    err "DMG not found: $DMG"
    exit 1
end
if test -z "$VERSION"
    err "could not derive version from: $DMG"
    exit 1
end

say "promoting Menu Band $VERSION → manifest"
say "  DMG: $DMG"

# ── Compute size + sha256. ──────────────────────────────────────────────
set -l SIZE (stat -f %z $DMG 2>/dev/null; or stat -c %s $DMG)
set -l SHA256 (shasum -a 256 $DMG | awk '{print $1}')
set -l RELEASED (date -u +%Y-%m-%d)

ok "  size:     $SIZE bytes"
ok "  sha256:   $SHA256"
ok "  released: $RELEASED"

# ── Patch the manifest. We do this in Node so JSON formatting + key
#    ordering stay stable; doing it in awk would invite drift. ──────────
set -l URL "https://assets.aesthetic.computer/menuband/Menu-Band-$VERSION.dmg"

env MB_MANIFEST=$MANIFEST \
    MB_VERSION=$VERSION \
    MB_URL=$URL \
    MB_SIZE=$SIZE \
    MB_SHA256=$SHA256 \
    MB_RELEASED=$RELEASED \
    node -e '
const fs = require("fs");
const path = process.env.MB_MANIFEST;
const manifest = JSON.parse(fs.readFileSync(path, "utf8"));

const entry = {
  version: process.env.MB_VERSION,
  url: process.env.MB_URL,
  size: Number(process.env.MB_SIZE),
  sha256: process.env.MB_SHA256,
  releasedAt: process.env.MB_RELEASED,
  requirements: "macOS 11+ · Apple Silicon",
};

if (!Array.isArray(manifest.versions)) manifest.versions = [];
const ix = manifest.versions.findIndex(v => v && v.version === entry.version);
if (ix === -1) {
  manifest.versions.unshift(entry);
} else {
  // Preserve any extra fields a human added by hand (e.g. notes).
  manifest.versions[ix] = { ...manifest.versions[ix], ...entry };
  // Move it to the front so the latest is always first.
  const [updated] = manifest.versions.splice(ix, 1);
  manifest.versions.unshift(updated);
}
manifest.latest = entry.version;

fs.writeFileSync(path, JSON.stringify(manifest, null, 2) + "\n");
console.log("✓ manifest updated: latest=" + manifest.latest);
'

ok "manifest written"
echo
echo "Next steps:"
echo "  1. Upload the DMG to Spaces:"
echo "       (publish via your usual asset sync — assets.aesthetic.computer/menuband/)"
echo "  2. Commit + push manifest.json so the site flips over:"
echo "       git add system/public/menuband/manifest.json"
echo "       git commit -m \"menuband: release $VERSION\""
echo "       git push"
echo "  3. /api/menuband-downloads will start accepting POST { version: \"$VERSION\" }"
echo "     once the new manifest is live."
