// Seat the full-bleed iOS icon master on a macOS icon plate.
//
// iOS masks its icons itself; macOS draws them as authored, so the shared
// square master showed up in the Dock as a raw tile, edge to edge, beside
// every rounded neighbour. The Mac grid wants an 824pt rounded rect centred
// on a 1024 canvas, with the rest left clear for the plate's shadow.
//
//   swift mac-icon-plate.swift <ios-master.png> <out-1024.png>
import AppKit

let args = CommandLine.arguments
guard args.count == 3, let master = NSImage(contentsOfFile: args[1]) else {
  FileHandle.standardError.write("usage: mac-icon-plate.swift <master.png> <out.png>\n".data(using: .utf8)!)
  exit(2)
}

let canvas = 1024
let body: CGFloat = 824
let radius: CGFloat = 185
let origin = (CGFloat(canvas) - body) / 2
let plate = CGRect(x: origin, y: origin, width: body, height: body)

guard let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: canvas,
    pixelsHigh: canvas, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
    isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0),
  let context = NSGraphicsContext(bitmapImageRep: rep) else { exit(1) }
NSGraphicsContext.current = context
let cg = context.cgContext
let path = CGPath(roundedRect: plate, cornerWidth: radius, cornerHeight: radius, transform: nil)

// A soft drop under the plate, the way the system's own icons sit.
cg.saveGState()
cg.setShadow(offset: CGSize(width: 0, height: -10), blur: 28,
  color: NSColor.black.withAlphaComponent(0.32).cgColor)
cg.addPath(path)
cg.setFillColor(NSColor.black.cgColor)
cg.fillPath()
cg.restoreGState()

// The master fills the plate and is clipped to it.
cg.saveGState()
cg.addPath(path)
cg.clip()
master.draw(in: plate, from: .zero, operation: .sourceOver, fraction: 1)
cg.restoreGState()

// A hairline inner edge keeps the plate's shape on a dark Dock.
cg.addPath(CGPath(roundedRect: plate.insetBy(dx: 1, dy: 1), cornerWidth: radius - 1,
  cornerHeight: radius - 1, transform: nil))
cg.setStrokeColor(NSColor.black.withAlphaComponent(0.18).cgColor)
cg.setLineWidth(2)
cg.strokePath()

NSGraphicsContext.current = nil
guard let png = rep.representation(using: .png, properties: [:]) else { exit(1) }
try png.write(to: URL(fileURLWithPath: args[2]))
