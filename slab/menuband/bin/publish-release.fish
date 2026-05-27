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
# Two-manifest pipeline:
#   • $MANIFEST   — committed, lith-served, big-schema (versions[]).
#                   Powers the website's download button + the
#                   /api/menuband-downloads counter's allowlist.
#   • $LATEST     — gitignored, CDN-synced, small-schema. The macOS
#                   UpdateChecker polls assets.aesthetic.computer/...
set -l MANIFEST $REPO_ROOT/system/public/menuband/manifest.json
set -l LATEST   $REPO_ROOT/system/public/assets/menuband/latest.json
set -l ASSETS_DIR $REPO_ROOT/system/public/assets/menuband

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

# Pre-promotion gate: verify the DMG is self-contained. Menu-Band-1.0.dmg
# shipped without MenuBand_MenuBand.bundle inside the .app, so every fresh
# install crashed on `Bundle.module` lookups falling back to the
# /Users/<dev>/... build path. Refuse to flip the manifest until the DMG
# passes verification.
if not $MB_DIR/bin/verify-bundle.sh --dmg $DMG
    err "DMG failed bundle verification — refusing to promote to manifest"
    err "rebuild with ./install.sh && ./notarize.sh && ./dmg.sh"
    exit 1
end
ok "DMG verified self-contained"

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

# Read optional release notes from --notes flag or NOTES env. The notes
# get written to the top-level `notes` field (Swift app reads it for the
# in-popover update banner) and aren't duplicated into versions[].
set -l NOTES (set -q MB_NOTES; and echo $MB_NOTES; or echo "")

env MB_MANIFEST=$MANIFEST \
    MB_LATEST=$LATEST \
    MB_VERSION=$VERSION \
    MB_URL=$URL \
    MB_SIZE=$SIZE \
    MB_SHA256=$SHA256 \
    MB_RELEASED=$RELEASED \
    MB_NOTES=$NOTES \
    node -e '
const fs = require("fs");

// — Big manifest (web) —
const manifestPath = process.env.MB_MANIFEST;
const manifest = JSON.parse(fs.readFileSync(manifestPath, "utf8"));

const entry = {
  version: process.env.MB_VERSION,
  dmg: process.env.MB_URL,
  size: Number(process.env.MB_SIZE),
  sha256: process.env.MB_SHA256,
  releasedAt: process.env.MB_RELEASED,
  requirements: "macOS 11+ · Universal (Intel + Apple Silicon)",
};

if (!Array.isArray(manifest.versions)) manifest.versions = [];
const ix = manifest.versions.findIndex(v => v && v.version === entry.version);
if (ix === -1) {
  manifest.versions.unshift(entry);
} else {
  // Preserve any extra fields a human added by hand (e.g. release notes).
  manifest.versions[ix] = { ...manifest.versions[ix], ...entry };
  const [updated] = manifest.versions.splice(ix, 1);
  manifest.versions.unshift(updated);
}
manifest.latest = entry.version;
fs.writeFileSync(manifestPath, JSON.stringify(manifest, null, 2) + "\n");
console.log("✓ web manifest updated: latest=" + manifest.latest);

// — Small manifest (macOS app / CDN) —
const latestPath = process.env.MB_LATEST;
let latest = {};
try { latest = JSON.parse(fs.readFileSync(latestPath, "utf8")); } catch {}
latest.version = entry.version;
latest.url = latest.url || "https://aesthetic.computer/menuband";
const newNotes = process.env.MB_NOTES;
if (newNotes) latest.notes = newNotes;
fs.writeFileSync(latestPath, JSON.stringify(latest, null, 2) + "\n");
console.log("✓ CDN latest.json updated: version=" + latest.version);
'

ok "manifests written"
ok "  web:   $MANIFEST"
ok "  CDN:   $LATEST"

# Mirror the DMG into system/public/assets/menuband/ so `npm run
# assets:sync:up` picks it up on the next sync. We copy instead of
# moving so re-running the script is safe and the original stays in
# slab/menuband/ for archival.
set -l ASSET_TARGET $ASSETS_DIR/(basename $DMG)
if not test "$DMG" = "$ASSET_TARGET"
    cp $DMG $ASSET_TARGET
    ok "DMG copied to $ASSET_TARGET"
end

echo
echo "Next steps:"
echo "  1. Sync assets up to Spaces:"
echo "       npm run assets:sync:up"
echo "  2. Commit the manifest + DMG:"
echo "       git add system/public/assets/menuband/"
echo "       git commit -m \"menuband: release $VERSION\""
echo "       git push"
echo "  3. /api/menuband-downloads will start accepting POST { version: \"$VERSION\" }"
echo "     once the new manifest is live (and the macOS app's UpdateChecker"
echo "     will surface a 'new version' banner)."
