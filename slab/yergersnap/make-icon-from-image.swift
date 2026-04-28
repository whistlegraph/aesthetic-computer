#!/usr/bin/env swift
//
// make-icon-from-image.swift — turn a source image (e.g. a painting
// photo) into AppIcon.icns. Square-center-crops with an optional
// frame inset, scales to 1024, applies the macOS app-icon corner
// radius, slices an iconset, compiles to .icns.
//
// Usage:
//   swift make-icon-from-image.swift <path> [inset-fraction]
//
//   inset-fraction defaults to 0.08 (trims 8% off each side, useful
//   for gallery photos where there's a visible frame + matte).

import Cocoa

let args = CommandLine.arguments
guard args.count >= 2, !args[1].isEmpty else {
  fputs("usage: make-icon-from-image.swift <path> [inset-fraction]\n", stderr)
  exit(64)
}
let srcPath = args[1]
let inset: CGFloat = args.count >= 3
  ? CGFloat(Double(args[2]) ?? 0.08)
  : 0.08

guard let image = NSImage(contentsOfFile: srcPath) else {
  fputs("couldn't load image: \(srcPath)\n", stderr)
  exit(1)
}

let srcSize = image.size
let side = min(srcSize.width, srcSize.height)
let croppedSide = side * (1.0 - 2 * inset)
let cropX = (srcSize.width  - croppedSide) / 2
let cropY = (srcSize.height - croppedSide) / 2
let cropRect = NSRect(x: cropX, y: cropY,
                      width: croppedSide, height: croppedSide)

let canvas: CGFloat = 1024
let cornerRadius = canvas * 0.2237  // matches macOS app-icon squircle

let master = NSImage(size: NSSize(width: canvas, height: canvas))
master.lockFocus()
NSBezierPath(roundedRect: NSRect(x: 0, y: 0, width: canvas, height: canvas),
             xRadius: cornerRadius,
             yRadius: cornerRadius).addClip()
NSGraphicsContext.current?.imageInterpolation = .high
image.draw(in: NSRect(x: 0, y: 0, width: canvas, height: canvas),
           from: cropRect,
           operation: .sourceOver,
           fraction: 1)
master.unlockFocus()

// Save iconset slices.
let outIconset = "icon.iconset"
let outIcns    = "AppIcon.icns"
let fm = FileManager.default
try? fm.removeItem(atPath: outIconset)
try fm.createDirectory(atPath: outIconset, withIntermediateDirectories: true)

let slices: [(String, Int)] = [
  ("icon_16x16",       16),
  ("icon_16x16@2x",    32),
  ("icon_32x32",       32),
  ("icon_32x32@2x",    64),
  ("icon_128x128",    128),
  ("icon_128x128@2x", 256),
  ("icon_256x256",    256),
  ("icon_256x256@2x", 512),
  ("icon_512x512",    512),
  ("icon_512x512@2x", 1024),
]
for (name, px) in slices {
  let scaled = NSImage(size: NSSize(width: px, height: px))
  scaled.lockFocus()
  NSGraphicsContext.current?.imageInterpolation = .high
  master.draw(in: NSRect(x: 0, y: 0, width: px, height: px),
              from: .zero, operation: .sourceOver, fraction: 1)
  scaled.unlockFocus()
  guard let cg = scaled.cgImage(forProposedRect: nil, context: nil,
                                hints: nil) else {
    fputs("cgImage failed for \(name)\n", stderr); exit(1)
  }
  let bm = NSBitmapImageRep(cgImage: cg)
  guard let data = bm.representation(using: .png, properties: [:]) else {
    fputs("png encode failed for \(name)\n", stderr); exit(1)
  }
  try data.write(to: URL(fileURLWithPath: "\(outIconset)/\(name).png"))
}

let task = Process()
task.executableURL = URL(fileURLWithPath: "/usr/bin/iconutil")
task.arguments = ["-c", "icns", "-o", outIcns, outIconset]
try task.run()
task.waitUntilExit()
if task.terminationStatus != 0 {
  fputs("iconutil failed\n", stderr); exit(1)
}

print("✓ wrote \(outIcns) from \(srcPath) (inset \(inset))")
