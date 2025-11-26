#!/bin/bash
# Upload SpiderLily iOS build to builds.false.work
set -e

APP_PATH="${1:-$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app}"
REMOTE_USER="root"
REMOTE_HOST="false.work"
REMOTE_PATH="/var/www/builds.false.work"
BUILD_DATE=$(date +%Y%m%d-%H%M%S)

echo "========================================="
echo "Uploading SpiderLily iOS build"
echo "========================================="
echo ""
echo "App: $APP_PATH"
echo "Destination: $REMOTE_HOST:$REMOTE_PATH/ios/"
echo ""

# Check if app exists
if [ ! -d "$APP_PATH" ]; then
    echo "❌ ERROR: App not found at $APP_PATH"
    exit 1
fi

# Create IPA (iOS App Store Package)
echo "📦 Creating IPA package..."
IPA_NAME="SpiderLily-${BUILD_DATE}.ipa"
TMP_PAYLOAD="/tmp/Payload"
TMP_IPA="/tmp/${IPA_NAME}"

rm -rf "$TMP_PAYLOAD" "$TMP_IPA"
mkdir -p "$TMP_PAYLOAD"
cp -R "$APP_PATH" "$TMP_PAYLOAD/"
cd /tmp
zip -r "$TMP_IPA" Payload
rm -rf "$TMP_PAYLOAD"

echo "  ✓ Created: $TMP_IPA ($(du -h "$TMP_IPA" | cut -f1))"
echo ""

# Upload to server
echo "📤 Uploading to builds.false.work..."
ssh "$REMOTE_USER@$REMOTE_HOST" "mkdir -p $REMOTE_PATH/ios"
scp "$TMP_IPA" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_PATH/ios/"
scp "$TMP_IPA" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_PATH/ios/SpiderLily-latest.ipa"

echo "  ✓ Uploaded to: https://builds.false.work/ios/${IPA_NAME}"
echo "  ✓ Latest link: https://builds.false.work/ios/SpiderLily-latest.ipa"
echo ""

# Create/update manifest.plist for wireless installation
echo "📝 Creating installation manifest..."
MANIFEST_CONTENT="<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>items</key>
    <array>
        <dict>
            <key>assets</key>
            <array>
                <dict>
                    <key>kind</key>
                    <string>software-package</string>
                    <key>url</key>
                    <string>https://builds.false.work/ios/SpiderLily-latest.ipa</string>
                </dict>
            </array>
            <key>metadata</key>
            <dict>
                <key>bundle-identifier</key>
                <string>work.false.SpiderLily</string>
                <key>bundle-version</key>
                <string>1.0</string>
                <key>kind</key>
                <string>software</string>
                <key>title</key>
                <string>Spider Lily</string>
            </dict>
        </dict>
    </array>
</dict>
</plist>"

echo "$MANIFEST_CONTENT" > /tmp/manifest.plist
scp /tmp/manifest.plist "$REMOTE_USER@$REMOTE_HOST:$REMOTE_PATH/ios/manifest.plist"
echo "  ✓ Manifest uploaded"
echo ""

# Create/update download page
echo "🌐 Creating download page..."
HTML_CONTENT="<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Spider Lily - iOS Build</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            background: white;
            padding: 40px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        h1 {
            color: #333;
            margin-bottom: 10px;
        }
        .build-info {
            color: #666;
            margin-bottom: 30px;
            font-size: 14px;
        }
        .install-btn {
            display: inline-block;
            background: #007AFF;
            color: white;
            padding: 15px 30px;
            border-radius: 8px;
            text-decoration: none;
            font-weight: 600;
            margin: 10px 0;
        }
        .install-btn:hover {
            background: #0051D5;
        }
        .download-link {
            display: inline-block;
            color: #007AFF;
            text-decoration: none;
            margin: 10px 0;
        }
        .instructions {
            background: #f9f9f9;
            padding: 20px;
            border-radius: 5px;
            margin-top: 30px;
        }
        .instructions h2 {
            margin-top: 0;
            color: #333;
        }
        .instructions ol {
            padding-left: 20px;
        }
        .instructions li {
            margin: 10px 0;
            line-height: 1.6;
        }
        .note {
            background: #fff3cd;
            border-left: 4px solid #ffc107;
            padding: 15px;
            margin: 20px 0;
        }
    </style>
</head>
<body>
    <div class=\"container\">
        <h1>🕷️ Spider Lily</h1>
        <div class=\"build-info\">
            iOS Build - ${BUILD_DATE}<br>
            Bundle ID: work.false.SpiderLily<br>
            Team ID: F7G74Z35B8
        </div>
        
        <a href=\"itms-services://?action=download-manifest&url=https://builds.false.work/ios/manifest.plist\" class=\"install-btn\">
            📱 Install on iOS Device
        </a>
        <br>
        <a href=\"SpiderLily-latest.ipa\" class=\"download-link\" download>
            ⬇️ Download IPA File
        </a>
        
        <div class=\"note\">
            <strong>⚠️ Important:</strong> This is a development build. Your device must be registered with the Apple Developer account (Team ID: F7G74Z35B8) to install this app.
        </div>
        
        <div class=\"instructions\">
            <h2>Installation Instructions</h2>
            <h3>Method 1: Direct Install (Easiest)</h3>
            <ol>
                <li>Open this page on your iOS device in Safari</li>
                <li>Tap the \"Install on iOS Device\" button above</li>
                <li>Tap \"Install\" when prompted</li>
                <li>Go to Settings → General → VPN & Device Management</li>
                <li>Tap on the \"Apple Development: make@false.work\" profile</li>
                <li>Tap \"Trust\" to allow the app to run</li>
            </ol>
            
            <h3>Method 2: Download and Install via Computer</h3>
            <ol>
                <li>Download the IPA file using the link above</li>
                <li>Connect your iOS device to your Mac via USB</li>
                <li>Open Finder and select your device in the sidebar</li>
                <li>Drag and drop the .ipa file onto the device window</li>
                <li>Follow the trust instructions from Method 1, step 4-6</li>
            </ol>
            
            <h3>Method 3: Using Xcode</h3>
            <ol>
                <li>Download the IPA file</li>
                <li>Connect your iOS device to your Mac</li>
                <li>Open Xcode → Window → Devices and Simulators</li>
                <li>Select your device</li>
                <li>Click the \"+\" button under \"Installed Apps\"</li>
                <li>Select the downloaded .ipa file</li>
            </ol>
        </div>
    </div>
</body>
</html>"

echo "$HTML_CONTENT" > /tmp/index.html
scp /tmp/index.html "$REMOTE_USER@$REMOTE_HOST:$REMOTE_PATH/ios/index.html"
echo "  ✓ Download page created"
echo ""

# Clean up temp files
rm -f "$TMP_IPA" /tmp/manifest.plist /tmp/index.html

echo "✅ Upload complete!"
echo ""
echo "📱 Install on device:"
echo "   https://builds.false.work/ios/"
echo ""
echo "⬇️  Direct download:"
echo "   https://builds.false.work/ios/SpiderLily-latest.ipa"
echo ""
echo "📋 Build archive:"
echo "   https://builds.false.work/ios/${IPA_NAME}"
echo ""
