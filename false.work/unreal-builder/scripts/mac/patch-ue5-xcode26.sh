#!/bin/bash
# Patch UE5.6 to support Xcode 26 (macOS Sequoia)
set -e

SDK_FILE="/Users/Shared/Epic Games/UE_5.6/Engine/Config/Apple/Apple_SDK.json"
BACKUP_FILE="/Users/Shared/Epic Games/UE_5.6/Engine/Config/Apple/Apple_SDK.json.backup"

echo "========================================="
echo "Patching UE5.6 for Xcode 26 Support"
echo "========================================="
echo ""

# Backup original file
if [ ! -f "$BACKUP_FILE" ]; then
    echo "📋 Creating backup..."
    sudo cp "$SDK_FILE" "$BACKUP_FILE"
    echo "✓ Backup saved to: $BACKUP_FILE"
else
    echo "✓ Backup already exists"
fi

echo ""
echo "🔧 Updating MaxVersion from 16.9.0 to 26.9.0..."

# Create updated config
sudo cat > "/tmp/Apple_SDK.json" << 'EOF'
{
        "//1": "Xcode versions:",
                "MainVersion": "15.2",
                "MinVersion": "15.2.0",
                "MaxVersion": "26.9.0",
        "//2": "!!!",
        "//3": "NOTE: If you update the MaxVersion, double check the AppleVersionToLLVMVersion array below!!!",
        "//4": "!!!",

        "//5": "The versions on Windows are iTunes versions:",
                "MinVersion_Win64": "1100.0.0.0",
                "MaxVersion_Win64": "8999.0",

        "//6": "This is not a version range, but a mapping of Xcode clang versions to LLVM versions, for shared version checks with other clangs",
        "//7": "Version mapping can be found at https://en.wikipedia.org/wiki/Xcode#Toolchain_versions",
        "//8": "The first half of the pair is the first version that is using the second version source LLVM",
        "//9": "For instance, Xcode 16 uses LLVM 17.0.6",
                "AppleVersionToLLVMVersions": [
                        "14.0.0-14.0.0",
                        "14.0.3-15.0.0",
                        "15.0.0-16.0.0",
                        "16.0.0-17.0.6",
                        "16.3.0-19.1.4",
                        "26.0.0-19.1.4"
                ]
}
EOF

sudo mv "/tmp/Apple_SDK.json" "$SDK_FILE"

echo "✅ Patch applied!"
echo ""
echo "Updated configuration:"
cat "$SDK_FILE" | grep -A2 "MaxVersion"

echo ""
echo "🎉 Done! UE5.6 should now work with Xcode 26."
