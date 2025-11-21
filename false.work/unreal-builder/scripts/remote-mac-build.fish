#!/usr/bin/env fish
# Remote Mac Build & Upload Script
# Runs from dev container, orchestrates build on Mac via SSH (like Windows version)

set VERSION (date +%Y.%m.%d.%H.%M)
if test (count $argv) -ge 1
    set VERSION $argv[1]
end

set CONFIG "Development"
if test (count $argv) -ge 2
    set CONFIG $argv[2]
end

echo "========================================="
echo "SpiderLily Mac Build Pipeline"
echo "========================================="
echo "Version: $VERSION"
echo "Config: $CONFIG"
echo ""

# Load credentials from vault
set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work
set MAC_CREDS $VAULT_DIR/mac-builder-credentials.env
set SPACES_ENV $VAULT_DIR/builds-spaces.env

if not test -f $MAC_CREDS
    echo "❌ Mac credentials not found: $MAC_CREDS"
    exit 1
end

if not test -f $SPACES_ENV
    echo "❌ Spaces config not found: $SPACES_ENV"
    exit 1
end

# Parse credentials (bash format)
set MAC_HOST (grep '^MAC_HOST=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
set MAC_USERNAME (grep '^MAC_USERNAME=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
set MAC_PASSWORD (grep '^MAC_PASSWORD=' $MAC_CREDS | cut -d= -f2 | tr -d '"')

# Parse Spaces config
set BUILDS_SPACES_ENDPOINT (grep '^BUILDS_SPACES_ENDPOINT=' $SPACES_ENV | cut -d= -f2 | tr -d '"')
set BUILDS_SPACES_KEY (grep '^BUILDS_SPACES_KEY=' $SPACES_ENV | cut -d= -f2 | tr -d '"')
set BUILDS_SPACES_SECRET (grep '^BUILDS_SPACES_SECRET=' $SPACES_ENV | cut -d= -f2 | tr -d '"')
set BUILDS_SPACES_BUCKET (grep '^BUILDS_SPACES_BUCKET=' $SPACES_ENV | cut -d= -f2 | tr -d '"')
set BUILDS_SPACES_REGION (grep '^BUILDS_SPACES_REGION=' $SPACES_ENV | cut -d= -f2 | tr -d '"')

if test -z "$MAC_HOST" -o -z "$MAC_USERNAME" -o -z "$MAC_PASSWORD"
    echo "❌ Failed to load Mac credentials"
    exit 1
end

# Check if sshpass is installed
if not command -v sshpass >/dev/null
    echo "⚠️  Installing sshpass..."
    sudo apt-get update -qq && sudo apt-get install -y -qq sshpass
end

# Helper function to run commands on Mac
function ssh_exec
    sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST" $argv
end

# Paths on Mac
set UE_ROOT "/Users/Shared/Epic Games/UE_5.6"
set PROJECT_ROOT "$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
set PROJECT_FILE "$PROJECT_ROOT/SpiderLily.uproject"
set OUTPUT_DIR "$HOME/Builds/$VERSION"

echo "📥 Step 1/5: Syncing Perforce..."
ssh_exec "cd $PROJECT_ROOT && /opt/homebrew/bin/p4 sync"
if test $status -ne 0
    echo "❌ Perforce sync failed"
    exit 1
end

echo ""
echo "🔧 Step 2/5: Applying FMOD plugin fix..."
set FMOD_FILE "$PROJECT_ROOT/Plugins/FMODStudio/Source/FMODStudio/Private/FMODBlueprintStatics.cpp"
ssh_exec "sed -i.bak 's/FMOD_STUDIO_PLAYBACK_STATE pS;/FMOD_STUDIO_PLAYBACK_STATE pS = FMOD_STUDIO_PLAYBACK_STOPPED;/g' '$FMOD_FILE' 2>/dev/null && echo '  ✓ Applied fix' || echo '  ⚠️  Fix already applied or file not found'"

echo ""
echo "🔨 Step 3/5: Building Mac package with BuildCookRun..."
echo "   This will take several minutes..."
echo ""

# Clean old builds and run BuildCookRun
ssh_exec "
    rm -rf '$OUTPUT_DIR' && \
    rm -rf '$PROJECT_ROOT/Saved/Cooked/Mac' && \
    rm -rf '$PROJECT_ROOT/Saved/Shaders' && \
    rm -rf '$PROJECT_ROOT/Saved/StagedBuilds' && \
    '$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh' BuildCookRun \
        -project='$PROJECT_FILE' \
        -platform=Mac \
        -clientconfig=$CONFIG \
        -cook \
        -build \
        -stage \
        -pak \
        -archive \
        -archivedirectory='$OUTPUT_DIR' \
        -noP4 \
        -utf8output
"

if test $status -ne 0
    echo "❌ Build failed"
    exit 1
end

echo ""
echo "✅ Build complete!"
echo ""

echo "🗜️  Step 4/5: Compressing build..."
set ARCHIVE_NAME "spiderlily-mac-$VERSION.zip"
set ARCHIVE_PATH "$HOME/Builds/$ARCHIVE_NAME"

ssh_exec "cd '$OUTPUT_DIR' && zip -r -q '$ARCHIVE_PATH' MacNoEditor/"
if test $status -ne 0
    echo "❌ Compression failed"
    exit 1
end

set ARCHIVE_SIZE (ssh_exec "du -h '$ARCHIVE_PATH' | cut -f1")
echo "  ✓ Created: $ARCHIVE_NAME ($ARCHIVE_SIZE)"
echo ""

echo "☁️  Step 5/5: Uploading to DigitalOcean Spaces..."

# Set AWS credentials and upload
set S3_URI "s3://$BUILDS_SPACES_BUCKET/mac/$ARCHIVE_NAME"
echo "  Target: $S3_URI"

ssh_exec "
    export AWS_ACCESS_KEY_ID='$BUILDS_SPACES_KEY' && \
    export AWS_SECRET_ACCESS_KEY='$BUILDS_SPACES_SECRET' && \
    aws s3 cp '$ARCHIVE_PATH' '$S3_URI' \
        --endpoint-url '$BUILDS_SPACES_ENDPOINT' \
        --acl public-read \
        --region '$BUILDS_SPACES_REGION'
"

if test $status -ne 0
    echo "❌ Upload failed"
    exit 1
end

# Create latest symlink
set LATEST_URI "s3://$BUILDS_SPACES_BUCKET/mac/spiderlily-mac-latest.zip"
ssh_exec "
    export AWS_ACCESS_KEY_ID='$BUILDS_SPACES_KEY' && \
    export AWS_SECRET_ACCESS_KEY='$BUILDS_SPACES_SECRET' && \
    aws s3 cp '$S3_URI' '$LATEST_URI' \
        --endpoint-url '$BUILDS_SPACES_ENDPOINT' \
        --acl public-read \
        --region '$BUILDS_SPACES_REGION'
"

# Generate URLs
set CDN_URL "https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/mac/$ARCHIVE_NAME"
set LATEST_CDN_URL "https://$BUILDS_SPACES_BUCKET.$BUILDS_SPACES_REGION.cdn.digitaloceanspaces.com/mac/spiderlily-mac-latest.zip"

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
