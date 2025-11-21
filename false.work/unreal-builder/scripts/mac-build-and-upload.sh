#!/bin/bash
# Remote Build + Upload Script for Mac
# Runs on the Mac build machine, builds SpiderLily, and uploads to Spaces

set -e

VERSION="${1:-$(date +%Y.%m.%d.%H.%M)}"
CONFIG="${2:-Development}"

echo "========================================="
echo "SpiderLily Mac Build Pipeline"
echo "========================================="
echo "Version: $VERSION"
echo "Config: $CONFIG"
echo ""

# Paths
UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
OUTPUT_DIR="$HOME/Builds/$VERSION"
VAULT_DIR="$HOME/aesthetic-computer-vault/false.work"

# Load Spaces credentials from vault
SPACES_ENV="$VAULT_DIR/builds-spaces.env"

if [ ! -f "$SPACES_ENV" ]; then
    echo "❌ Spaces config not found: $SPACES_ENV"
    exit 1
fi

# Source the environment file
source "$SPACES_ENV"

# Step 1: Sync Perforce
echo "📥 Step 1/5: Syncing Perforce..."
cd "$PROJECT_ROOT"
/opt/homebrew/bin/p4 sync
if [ $? -ne 0 ]; then
    echo "❌ Perforce sync failed"
    exit 1
fi

# Step 2: Apply FMOD fix
echo ""
echo "🔧 Step 2/5: Applying FMOD plugin fix..."
FMOD_FILE="$PROJECT_ROOT/Plugins/FMODStudio/Source/FMODStudio/Private/FMODBlueprintStatics.cpp"
if [ -f "$FMOD_FILE" ]; then
    sed -i.bak 's/FMOD_STUDIO_PLAYBACK_STATE pS;/FMOD_STUDIO_PLAYBACK_STATE pS = FMOD_STUDIO_PLAYBACK_STOPPED;/g' "$FMOD_FILE"
    echo "  ✓ Applied fix to FMODBlueprintStatics.cpp"
fi

# Step 3: Build with BuildCookRun
echo ""
echo "🔨 Step 3/5: Building Mac package with BuildCookRun..."

# Clean old builds
rm -rf "$OUTPUT_DIR"
rm -rf "$PROJECT_ROOT/Saved/Cooked/Mac"
rm -rf "$PROJECT_ROOT/Saved/Shaders"
rm -rf "$PROJECT_ROOT/Saved/StagedBuilds"

"$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh" BuildCookRun \
    -project="$PROJECT_FILE" \
    -platform=Mac \
    -clientconfig=$CONFIG \
    -cook \
    -build \
    -stage \
    -pak \
    -archive \
    -archivedirectory="$OUTPUT_DIR" \
    -noP4 \
    -utf8output

if [ $? -ne 0 ]; then
    echo "❌ Build failed"
    exit 1
fi

echo ""
echo "✅ Build complete!"
echo "  Output: $OUTPUT_DIR"
echo ""

# Step 4: Compress
echo "🗜️  Step 4/5: Compressing build..."
ARCHIVE_NAME="spiderlily-mac-$VERSION.zip"
ARCHIVE_PATH="$HOME/Builds/$ARCHIVE_NAME"

cd "$OUTPUT_DIR"
zip -r -q "$ARCHIVE_PATH" MacNoEditor/

ARCHIVE_SIZE=$(du -h "$ARCHIVE_PATH" | cut -f1)
echo "  ✓ Created: $ARCHIVE_NAME ($ARCHIVE_SIZE)"
echo ""

# Step 5: Upload to Spaces using AWS CLI (S3-compatible)
echo "☁️  Step 5/5: Uploading to DigitalOcean Spaces..."

# Set AWS credentials for this session
export AWS_ACCESS_KEY_ID="$BUILDS_SPACES_KEY"
export AWS_SECRET_ACCESS_KEY="$BUILDS_SPACES_SECRET"

# Upload using AWS CLI
S3_URI="s3://$BUILDS_SPACES_BUCKET/mac/$ARCHIVE_NAME"
echo "  Target: $S3_URI"

aws s3 cp "$ARCHIVE_PATH" "$S3_URI" \
    --endpoint-url "$BUILDS_SPACES_ENDPOINT" \
    --acl public-read \
    --region "$BUILDS_SPACES_REGION"

if [ $? -ne 0 ]; then
    echo "❌ Upload failed"
    exit 1
fi

# Create latest symlink
LATEST_URI="s3://$BUILDS_SPACES_BUCKET/mac/spiderlily-mac-latest.zip"
aws s3 cp "$S3_URI" "$LATEST_URI" \
    --endpoint-url "$BUILDS_SPACES_ENDPOINT" \
    --acl public-read \
    --region "$BUILDS_SPACES_REGION"

# Generate URLs
CDN_URL="https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/mac/$ARCHIVE_NAME"
LATEST_CDN_URL="https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/mac/spiderlily-mac-latest.zip"

echo ""
echo "✅ Upload successful!"
echo ""
echo "📦 Download URLs:"
echo "   CDN:    $CDN_URL"
echo "   Latest: $LATEST_CDN_URL"
echo ""
echo "🌐 Add to builds.false.work:"
echo "   <a href=\"$CDN_URL\">SpiderLily Mac $VERSION</a>"
echo ""
echo "🎉 Pipeline complete!"
