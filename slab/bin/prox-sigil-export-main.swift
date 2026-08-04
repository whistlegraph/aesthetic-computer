import AppKit
import ImageIO
import UniformTypeIdentifiers

// SigilRenderer's live-session naming overload references these two types;
// the standalone exporter only needs their tiny surface and never calls it.
struct ClaudeSession { let sessionId: String }
enum Paths { static let loopboyConfig = "" }

guard CommandLine.arguments.count >= 3,
      let seed = UInt64(CommandLine.arguments[1], radix: 16) else {
    fputs("usage: prox-sigil-export <hex-seed> <bundle.prox> [dark|light]\n", stderr)
    exit(2)
}
let bundle = URL(fileURLWithPath: CommandLine.arguments[2], isDirectory: true)
let dark = CommandLine.arguments.count < 4 || CommandLine.arguments[3] != "light"
let frames = SigilRockFrames.render(
    seed: seed, dark: dark,
    sunHx: -0.45, sunElevation: 0.72, sunIntensity: 0.8,
    frameCount: 48, px: 160)
guard let first = frames.first else {
    fputs("prox-sigil-export: rock renderer returned no frames\n", stderr)
    exit(1)
}

func writePNG(_ frame: CGImage, to url: URL) -> Bool {
    guard let dest = CGImageDestinationCreateWithURL(
        url as CFURL, UTType.png.identifier as CFString, 1, nil) else { return false }
    CGImageDestinationAddImage(dest, frame, nil)
    return CGImageDestinationFinalize(dest)
}

func writeGIF(_ images: [CGImage], to url: URL) -> Bool {
    guard let dest = CGImageDestinationCreateWithURL(
        url as CFURL, UTType.gif.identifier as CFString, images.count, nil) else { return false }
    CGImageDestinationSetProperties(dest, [
        kCGImagePropertyGIFDictionary: [kCGImagePropertyGIFLoopCount: 0]
    ] as CFDictionary)
    let properties = [
        kCGImagePropertyGIFDictionary: [kCGImagePropertyGIFDelayTime: 1.0 / 24.0]
    ] as CFDictionary
    for image in images { CGImageDestinationAddImage(dest, image, properties) }
    return CGImageDestinationFinalize(dest)
}

guard writeGIF(frames, to: bundle.appendingPathComponent("sigil.gif")),
      writePNG(first, to: bundle.appendingPathComponent("sigil.png")) else {
    fputs("prox-sigil-export: could not write image assets\n", stderr)
    exit(1)
}
let icon = NSImage(cgImage: first, size: NSSize(width: 160, height: 160))
guard NSWorkspace.shared.setIcon(icon, forFile: bundle.path, options: []) else {
    fputs("prox-sigil-export: Finder rejected custom icon\n", stderr)
    exit(1)
}
print(bundle.appendingPathComponent("sigil.gif").path)
