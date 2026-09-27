#!/bin/bash
# Local, unsigned AppKit prototype. Does not install or change the host toolchain.
set -euo pipefail
cd "$(dirname "$0")"
sdk="${BRUSH_SWIFT_SDK:-$(xcrun --show-sdk-path)}"
app="$PWD/.build/No Paint Brushes.app"
mkdir -p "$app/Contents/"{MacOS,Frameworks,Resources/Brushes} .build/native
nice -n 8 swiftc -sdk "$sdk" -emit-library -emit-module -module-name BrushCore \
  Sources/BrushCore/Brush.swift -emit-module-path .build/native/BrushCore.swiftmodule \
  -Xlinker -install_name -Xlinker @rpath/libBrushCore.dylib -o "$app/Contents/Frameworks/libBrushCore.dylib"
nice -n 8 swiftc -sdk "$sdk" -I .build/native -L "$app/Contents/Frameworks" \
  -lBrushCore -Xlinker -rpath -Xlinker @executable_path/../Frameworks \
  Sources/BrushApp/*.swift -o "$app/Contents/MacOS/BrushApp"
cp fixtures/*.brush.json "$app/Contents/Resources/Brushes/"
node prepare-audio.mjs
cat > "$app/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>CFBundleExecutable</key><string>BrushApp</string>
<key>CFBundleIdentifier</key><string>computer.aesthetic.brush-prototype</string>
<key>CFBundleName</key><string>No Paint Brushes</string>
<key>CFBundlePackageType</key><string>APPL</string>
<key>CFBundleVersion</key><string>1</string>
<key>NSHighResolutionCapable</key><true/>
</dict></plist>
PLIST
codesign --force --sign - "$app/Contents/Frameworks/libBrushCore.dylib"
codesign --force --sign - "$app"
printf '%s\n' "$app"
