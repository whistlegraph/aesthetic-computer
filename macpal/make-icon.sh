#!/usr/bin/env bash
# make-icon.sh — render Resources/AppIcon.icns from the star glyph: the gold
# star sitting on a soft sky-blue squircle, macOS-icon-shaped. Re-run after
# changing star-glyph.svg.
set -euo pipefail
cd "$(dirname "$0")"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "› rasterizing star"
rsvg-convert -w 1024 -h 1024 Resources/star-glyph.svg -o "$TMP/star.png"

echo "› compositing 1024 master"
cat > "$TMP/compose.swift" <<'SWIFT'
import AppKit
let args = CommandLine.arguments
let starPath = args[1], outPath = args[2]
let S: CGFloat = 1024
let img = NSImage(size: NSSize(width: S, height: S))
img.lockFocus()
let ctx = NSGraphicsContext.current!.cgContext

// Soft sky squircle — Big-Sur-style content inset with room for a drop shadow.
let inset: CGFloat = 92
let rect = NSRect(x: inset, y: inset, width: S - inset*2, height: S - inset*2)
let radius: CGFloat = 200
let squircle = NSBezierPath(roundedRect: rect, xRadius: radius, yRadius: radius)

ctx.saveGState()
ctx.setShadow(offset: CGSize(width: 0, height: -14), blur: 36,
              color: NSColor(white: 0, alpha: 0.28).cgColor)
NSColor.white.setFill(); squircle.fill()
ctx.restoreGState()

squircle.addClip()
let top = NSColor(calibratedRed: 0.80, green: 0.92, blue: 1.0, alpha: 1)
let bot = NSColor(calibratedRed: 0.43, green: 0.70, blue: 1.0, alpha: 1)
NSGradient(colors: [top, bot])!.draw(in: rect, angle: -90)

// Warm glow behind the star so the gold pops off the blue.
let glow = NSGradient(colors: [NSColor(calibratedRed: 1, green: 0.97, blue: 0.8, alpha: 0.85),
                               NSColor(calibratedRed: 1, green: 0.97, blue: 0.8, alpha: 0)])!
glow.draw(in: NSRect(x: S/2 - 360, y: S/2 - 330, width: 720, height: 720),
          relativeCenterPosition: .zero)

// The star, centered, nudged up a touch, with its own soft shadow.
NSGraphicsContext.current!.saveGraphicsState()
let sh = NSShadow()
sh.shadowColor = NSColor(white: 0, alpha: 0.30)
sh.shadowOffset = NSSize(width: 0, height: -10)
sh.shadowBlurRadius = 22
sh.set()
let star = NSImage(contentsOfFile: starPath)!
let sw: CGFloat = 660
star.draw(in: NSRect(x: (S - sw)/2, y: (S - sw)/2 + 36, width: sw, height: sw))
NSGraphicsContext.current!.restoreGraphicsState()

img.unlockFocus()
let tiff = img.tiffRepresentation!
let png = NSBitmapImageRep(data: tiff)!.representation(using: .png, properties: [:])!
try! png.write(to: URL(fileURLWithPath: outPath))
SWIFT
swiftc -O "$TMP/compose.swift" -o "$TMP/compose" -framework AppKit
"$TMP/compose" "$TMP/star.png" "$TMP/icon.png"

echo "› building iconset"
SET="$TMP/MacPal.iconset"; mkdir -p "$SET"
for s in 16 32 64 128 256 512 1024; do
    sips -z $s $s "$TMP/icon.png" --out "$SET/tmp_$s.png" >/dev/null
done
cp "$SET/tmp_16.png"   "$SET/icon_16x16.png"
cp "$SET/tmp_32.png"   "$SET/icon_16x16@2x.png"
cp "$SET/tmp_32.png"   "$SET/icon_32x32.png"
cp "$SET/tmp_64.png"   "$SET/icon_32x32@2x.png"
cp "$SET/tmp_128.png"  "$SET/icon_128x128.png"
cp "$SET/tmp_256.png"  "$SET/icon_128x128@2x.png"
cp "$SET/tmp_256.png"  "$SET/icon_256x256.png"
cp "$SET/tmp_512.png"  "$SET/icon_256x256@2x.png"
cp "$SET/tmp_512.png"  "$SET/icon_512x512.png"
cp "$SET/tmp_1024.png" "$SET/icon_512x512@2x.png"
rm -f "$SET"/tmp_*.png

iconutil -c icns "$SET" -o Resources/AppIcon.icns
echo "✓ wrote Resources/AppIcon.icns"
