#!/usr/bin/env fish
# Deploy Xbox app package via Device Portal from Mac
# Usage: ./deploy-xbox.fish <path-to-msix>

set -l XBOX_IP "192.168.1.101"
set -l PORTAL_PORT "11443"
set -l PORTAL_URL "https://$XBOX_IP:$PORTAL_PORT"

# Check for package argument
if test (count $argv) -lt 1
    echo "🎮 Xbox App Deployer"
    echo ""
    echo "Usage: ./deploy-xbox.fish <package.msix>"
    echo ""
    echo "Steps:"
    echo "  1. Download .msix from GitHub Actions artifacts"
    echo "  2. Run: ./deploy-xbox.fish AestheticComputer.msix"
    echo ""
    echo "Or deploy via browser:"
    echo "  1. Open $PORTAL_URL in browser"
    echo "  2. Accept certificate warning"
    echo "  3. Go to 'My games & apps' → 'Add'"
    echo "  4. Upload the .msix file"
    exit 1
end

set -l package_path $argv[1]

if not test -f $package_path
    echo "❌ Package not found: $package_path"
    exit 1
end

echo "🎮 Deploying to Xbox at $XBOX_IP..."
echo "   Package: $package_path"
echo ""

# Check if Xbox is reachable
echo "🏓 Checking Xbox connectivity..."
if not ping -c 1 -W 2 $XBOX_IP >/dev/null 2>&1
    # Try via Mac host if in container
    if ssh jas@host.docker.internal "ping -c 1 -W 2 $XBOX_IP" >/dev/null 2>&1
        echo "   ✅ Xbox reachable via Mac host"
        set -l USE_MAC_HOST true
    else
        echo "   ❌ Xbox not reachable"
        exit 1
    end
else
    echo "   ✅ Xbox reachable"
end

# Device Portal API endpoint for app installation
# POST /api/app/packagemanager/package
# Multipart form with file

echo ""
echo "📦 Uploading package to Xbox Device Portal..."
echo "   This may take a minute..."
echo ""

# Note: Device Portal requires authentication if enabled
# You may need to add -u "username:password" 
# Default is no auth in Dev Mode, but you can set one in Dev Home

set -l response (curl -k -s -w "\n%{http_code}" \
    -X POST \
    -F "file=@$package_path" \
    "$PORTAL_URL/api/app/packagemanager/package" 2>&1)

set -l http_code (echo $response | tail -1)
set -l body (echo $response | head -n -1)

if test "$http_code" = "200" -o "$http_code" = "204"
    echo "✅ Package uploaded successfully!"
    echo ""
    echo "📺 The app should now appear in 'My games & apps' on Xbox"
    echo "   Look for 'Aesthetic Computer'"
else if test "$http_code" = "000"
    echo "⚠️  Could not connect to Device Portal"
    echo ""
    echo "Make sure:"
    echo "  1. Xbox is in Developer Mode"
    echo "  2. Device Portal is enabled in Dev Home app"
    echo "  3. You can access $PORTAL_URL in browser"
    echo ""
    echo "Alternative: Upload manually via browser"
    echo "  1. Open $PORTAL_URL"
    echo "  2. Go to 'My games & apps' → 'Add'"
    echo "  3. Select $package_path"
else
    echo "❌ Upload failed (HTTP $http_code)"
    echo "   Response: $body"
    echo ""
    echo "Try manual upload via browser: $PORTAL_URL"
end
