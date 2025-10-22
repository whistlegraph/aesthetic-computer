#!/usr/bin/env bash
# Patch Netlify CLI to handle literal % symbols in URLs

FILE="node_modules/netlify-cli/dist/utils/rules-proxy.js"

echo "🔧 Patching Netlify CLI to handle % symbols..."

# Check if already patched
if grep -q "try { return decodeURIComponent" "$FILE"; then
    echo "✅ Already patched!"
    exit 0
fi

# Create backup
cp "$FILE" "$FILE.backup"

# Apply patch using perl for in-place editing
perl -i -pe 's/path: decodeURIComponent\(reqUrl\.pathname\),/path: (() => { try { return decodeURIComponent(reqUrl.pathname); } catch (e) { return reqUrl.pathname; } })(),/' "$FILE"

echo "✅ Patch applied! Backup saved to $FILE.backup"
echo "🔄 Restart your dev server for changes to take effect."
