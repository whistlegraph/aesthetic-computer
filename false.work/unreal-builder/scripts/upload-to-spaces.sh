#!/bin/bash
# Upload SpiderLily builds to DigitalOcean Spaces (false.work builds)
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VAULT_DIR="$SCRIPT_DIR/../../../../aesthetic-computer-vault/false.work"

# Load credentials
if [ -f "$VAULT_DIR/builds-spaces.env" ]; then
    source "$VAULT_DIR/builds-spaces.env"
else
    echo "❌ ERROR: builds-spaces.env not found in vault"
    echo "   Create: aesthetic-computer-vault/false.work/builds-spaces.env"
    exit 1
fi

# Required environment variables
: ${BUILDS_SPACES_ENDPOINT:?}
: ${BUILDS_SPACES_KEY:?}
: ${BUILDS_SPACES_SECRET:?}
: ${BUILDS_SPACES_BUCKET:?}
: ${BUILDS_SPACES_REGION:?}

PLATFORM=${1:-mac}
BUILD_PATH=${2}
VERSION=${3:-$(date +%Y.%m.%d.%H.%M)}

echo "========================================="
echo "Upload SpiderLily Build to Spaces"
echo "========================================="
echo ""
echo "Platform: $PLATFORM"
echo "Version: $VERSION"
echo "Bucket: $BUILDS_SPACES_BUCKET"
echo "Region: $BUILDS_SPACES_REGION"
echo ""

if [ -z "$BUILD_PATH" ]; then
    echo "❌ ERROR: Build path required"
    echo "Usage: $0 <platform> <build_path> [version]"
    echo ""
    echo "Examples:"
    echo "  $0 mac ~/Perforce/.../Packaged/Mac/SpiderLily.app"
    echo "  $0 windows D:/Builds/123/WindowsNoEditor"
    echo "  $0 ios ~/Perforce/.../Packaged/IOS/SpiderLily.app 1.0.0"
    exit 1
fi

if [ ! -e "$BUILD_PATH" ]; then
    echo "❌ ERROR: Build not found at: $BUILD_PATH"
    exit 1
fi

# Install s3cmd if needed
if ! command -v s3cmd &> /dev/null; then
    echo "📦 Installing s3cmd..."
    if [ "$(uname)" == "Darwin" ]; then
        brew install s3cmd
    else
        sudo apt-get update && sudo apt-get install -y s3cmd
    fi
fi

# Configure s3cmd
S3CFG=$(mktemp)
cat > "$S3CFG" << EOF
[default]
access_key = $BUILDS_SPACES_KEY
secret_key = $BUILDS_SPACES_SECRET
host_base = $BUILDS_SPACES_REGION.digitaloceanspaces.com
host_bucket = %(bucket)s.$BUILDS_SPACES_REGION.digitaloceanspaces.com
use_https = True
EOF

chmod 600 "$S3CFG"

# Compress build
BUILD_NAME="spiderlily-${PLATFORM}-${VERSION}"
ARCHIVE="${BUILD_NAME}.tar.gz"
TMP_DIR=$(mktemp -d)
ARCHIVE_PATH="$TMP_DIR/$ARCHIVE"

echo "🗜️  Compressing build..."
if [ -d "$BUILD_PATH" ]; then
    tar czf "$ARCHIVE_PATH" -C "$(dirname "$BUILD_PATH")" "$(basename "$BUILD_PATH")"
else
    tar czf "$ARCHIVE_PATH" -C "$(dirname "$BUILD_PATH")" "$(basename "$BUILD_PATH")"
fi

ARCHIVE_SIZE=$(du -h "$ARCHIVE_PATH" | cut -f1)
echo "  ✓ Created: $ARCHIVE ($ARCHIVE_SIZE)"
echo ""

# Upload to Spaces
S3_PATH="s3://$BUILDS_SPACES_BUCKET/$PLATFORM/$ARCHIVE"
echo "☁️  Uploading to Spaces..."
echo "  Target: $S3_PATH"
echo ""

s3cmd -c "$S3CFG" put "$ARCHIVE_PATH" "$S3_PATH" \
    --acl-public \
    --no-mime-magic \
    --guess-mime-type \
    --no-progress

if [ $? -eq 0 ]; then
    # Generate URLs
    DIRECT_URL="https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.digitaloceanspaces.com/$PLATFORM/$ARCHIVE"
    CDN_URL="https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/$PLATFORM/$ARCHIVE"
    
    echo ""
    echo "✅ Upload successful!"
    echo ""
    echo "📦 Download URLs:"
    echo "   Direct: $DIRECT_URL"
    echo "   CDN:    $CDN_URL"
    echo ""
    echo "🌐 Add to builds.false.work:"
    echo "   <a href=\"$CDN_URL\">SpiderLily $PLATFORM $VERSION</a>"
    echo ""
    
    # Create latest symlink
    echo "🔗 Creating 'latest' symlink..."
    LATEST_PATH="s3://$BUILDS_SPACES_BUCKET/$PLATFORM/spiderlily-${PLATFORM}-latest.tar.gz"
    s3cmd -c "$S3CFG" cp "$S3_PATH" "$LATEST_PATH" --acl-public
    
    echo "  ✓ Latest: https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/$PLATFORM/spiderlily-${PLATFORM}-latest.tar.gz"
else
    echo ""
    echo "❌ Upload failed!"
    rm -rf "$TMP_DIR" "$S3CFG"
    exit 1
fi

# Cleanup
rm -rf "$TMP_DIR" "$S3CFG"

echo ""
echo "🎉 Done!"
