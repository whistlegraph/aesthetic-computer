#!/usr/bin/env swift
//
// make-icon.swift — generate AppIcon.icns from scratch.
// Produces icon.iconset/ + AppIcon.icns next to this file.
//
// Run with:  swift make-icon.swift
//
// Design: a camera-aperture shutter centered on a deep purple→navy
// gradient, with two faint concentric ripples around it suggesting
// the "snap" tap.

import Cocoa

let canvas: CGFloat = 1024
let outIconset = "icon.iconset"
let outIcns    = "AppIcon.icns"

// ---- render the master 1024x1024 image ----------------------------------
let master = NSImage(size: NSSize(width: canvas, height: canvas))
master.lockFocus()
let ctx = NSGraphicsContext.current!.cgContext

// Rounded-rect clip (macOS app-icon corner radius is roughly 22.5%).
let bounds = NSRect(x: 0, y: 0, width: canvas, height: canvas)
let cornerRadius = canvas * 0.2237
NSBezierPath(roundedRect: bounds,
             xRadius: cornerRadius,
             yRadius: cornerRadius).addClip()

// Background gradient — deep violet to near-black.
let bg = NSGradient(colors: [
  NSColor(calibratedRed: 0.18, green: 0.10, blue: 0.34, alpha: 1.0),
  NSColor(calibratedRed: 0.05, green: 0.03, blue: 0.13, alpha: 1.0),
])!
bg.draw(in: bounds, angle: 270)

// Soft top-side highlight so it doesn't read as flat.
let highlight = NSGradient(colors: [
  NSColor.white.withAlphaComponent(0.10),
  NSColor.clear,
])!
highlight.draw(
  fromCenter: NSPoint(x: canvas * 0.5, y: canvas * 0.66), radius: 0,
  toCenter:   NSPoint(x: canvas * 0.5, y: canvas * 0.66), radius: canvas * 0.55,
  options: [])

// Shutter — render SF Symbol "camera.aperture" big and tint white.
let symConfig = NSImage.SymbolConfiguration(pointSize: 560, weight: .light)
let symbol = NSImage(systemSymbolName: "camera.aperture",
                     accessibilityDescription: nil)!
  .withSymbolConfiguration(symConfig)!
let symSize = symbol.size

let tinted = NSImage(size: symSize)
tinted.lockFocus()
NSColor.white.setFill()
NSRect(origin: .zero, size: symSize).fill()
symbol.draw(in: NSRect(origin: .zero, size: symSize),
            from: .zero, operation: .destinationIn, fraction: 1)
tinted.unlockFocus()

let symFrame = NSRect(
  x: (canvas - symSize.width)  / 2,
  y: (canvas - symSize.height) / 2,
  width: symSize.width, height: symSize.height)
tinted.draw(in: symFrame, from: .zero, operation: .sourceOver, fraction: 0.96)

// Tap ripples — two thin concentric rings outside the shutter.
ctx.setLineCap(.round)
let center = NSPoint(x: canvas * 0.5, y: canvas * 0.5)
for (radius, alpha, lineWidth) in [
  (canvas * 0.430, CGFloat(0.18), CGFloat(4.0)),
  (canvas * 0.482, CGFloat(0.10), CGFloat(3.0)),
] {
  NSColor.white.withAlphaComponent(alpha).setStroke()
  let ring = NSBezierPath(ovalIn: NSRect(
    x: center.x - radius, y: center.y - radius,
    width: radius * 2,    height: radius * 2))
  ring.lineWidth = lineWidth
  ring.stroke()
}

master.unlockFocus()

// ---- slice into the 10 iconutil sizes -----------------------------------
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
    fputs("failed to cgImage \(name)\n", stderr)
    exit(1)
  }
  let bm = NSBitmapImageRep(cgImage: cg)
  guard let data = bm.representation(using: .png, properties: [:]) else {
    fputs("failed to encode \(name)\n", stderr)
    exit(1)
  }
  try data.write(to: URL(fileURLWithPath: "\(outIconset)/\(name).png"))
}

// ---- compile to .icns ---------------------------------------------------
let task = Process()
task.executableURL = URL(fileURLWithPath: "/usr/bin/iconutil")
task.arguments = ["-c", "icns", "-o", outIcns, outIconset]
try task.run()
task.waitUntilExit()
if task.terminationStatus != 0 {
  fputs("iconutil failed: \(task.terminationStatus)\n", stderr)
  exit(1)
}

print("✓ wrote \(outIcns) (also kept \(outIconset)/ for inspection)")
