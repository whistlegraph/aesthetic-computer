#!/bin/bash
# Update builds.false.work/index.html with new build link
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VAULT_DIR="$SCRIPT_DIR/../../../aesthetic-computer-vault/false.work"
INDEX_SOURCE="$SCRIPT_DIR/../../false.work/builds.false.work/index.html"
INDEX_PUBLIC="$SCRIPT_DIR/../../system/public/builds.false.work/index.html"

PLATFORM=${1:-mac}
VERSION=${2:-$(date +%Y.%m.%d.%H.%M)}

# Load Spaces config
if [ -f "$VAULT_DIR/builds-spaces.env" ]; then
    source "$VAULT_DIR/builds-spaces.env"
else
    echo "❌ ERROR: builds-spaces.env not found"
    exit 1
fi

ARCHIVE="spiderlily-${PLATFORM}-${VERSION}.tar.gz"
CDN_URL="https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/$PLATFORM/$ARCHIVE"
LATEST_URL="https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/$PLATFORM/spiderlily-${PLATFORM}-latest.tar.gz"

# Read current index
if [ ! -f "$INDEX_SOURCE" ]; then
    echo "❌ ERROR: Index not found at $INDEX_SOURCE"
    exit 1
fi

# Create new build entry
BUILD_ENTRY="      <li><a href=\"$CDN_URL\">SpiderLily $PLATFORM $VERSION</a> - $(date '+%B %d, %Y %H:%M')</li>"

# Find the insertion point (after the builds list comment or header)
# Insert after "Latest Builds" section or create it
if grep -q "<!-- BUILD_LIST_$PLATFORM -->" "$INDEX_SOURCE"; then
    # Insert after the comment
    sed -i.bak "/<!-- BUILD_LIST_$PLATFORM -->/a\\
$BUILD_ENTRY
" "$INDEX_SOURCE"
else
    # Add section if it doesn't exist
    SECTION="    <h2>$PLATFORM Builds</h2>
    <ul>
      <!-- BUILD_LIST_$PLATFORM -->
$BUILD_ENTRY
    </ul>"
    
    # Insert before closing body tag
    sed -i.bak "/<\/body>/i\\
$SECTION
" "$INDEX_SOURCE"
fi

# Update "latest" link if exists
if grep -q "Latest $PLATFORM Build" "$INDEX_SOURCE"; then
    sed -i.bak "s|Latest $PLATFORM Build.*</a>|Latest $PLATFORM Build</a> - <a href=\"$LATEST_URL\">Download</a>|" "$INDEX_SOURCE"
fi

# Copy to public directory
cp "$INDEX_SOURCE" "$INDEX_PUBLIC"
rm -f "$INDEX_SOURCE.bak"

echo "✅ Updated builds.false.work index"
echo "  Source: $INDEX_SOURCE"
echo "  Public: $INDEX_PUBLIC"
echo ""
echo "📝 Commit changes:"
echo "  cd /workspaces/aesthetic-computer"
echo "  git add false.work/builds.false.work/ system/public/builds.false.work/"
echo "  git commit -m \"Add SpiderLily $PLATFORM $VERSION build\""
echo "  git push"
