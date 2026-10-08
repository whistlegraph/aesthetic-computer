import AppKit
import ImageIO
import UniformTypeIdentifiers

// SigilRenderer's live-session naming overload references these two types;
// the standalone exporter only needs their tiny surface and never calls it.
struct ClaudeSession { let sessionId: String; let agentType: String; let piece: String }
enum Paths { static let loopboyConfig = "" }
struct LoopboyRoute { let name: String }
enum LoopboyRoutes {
    static func mode(for id: String) -> [String: Any]? { nil }
    static func verifiedContact(for session: ClaudeSession) -> String? { nil }
    static func all() -> [String: LoopboyRoute] { [:] }
}

guard CommandLine.arguments.count >= 3 else {
    fputs("usage: prox-sigil-export <character.json|legacy-hex-seed> <output-directory> [dark|light] [px] [frames]\n", stderr)
    exit(2)
}
let bundle = URL(fileURLWithPath: CommandLine.arguments[2], isDirectory: true)
let dark = CommandLine.arguments.count < 4 || CommandLine.arguments[3] != "light"
// Optional raster size and frame count: the menubar wants 160 px × 48 frames,
// a print sheet wants one big still.
let px = CGFloat(max(16, min(2048, CommandLine.arguments.count >= 5 ? Int(CommandLine.arguments[4]) ?? 160 : 160)))
let frameCount = max(1, min(120, CommandLine.arguments.count >= 6 ? Int(CommandLine.arguments[5]) ?? 48 : 48))
let source = CommandLine.arguments[1]
let frames: [CGImage]
if let seed = UInt64(source, radix: 16) {
    frames = SigilRockFrames.render(seed: seed, dark: dark,
        sunHx: -0.45, sunElevation: 0.72, sunIntensity: 0.8, frameCount: frameCount, px: px)
} else if let data = try? Data(contentsOf: URL(fileURLWithPath: source)),
          let creature = try? JSONDecoder().decode(ProxCreature.self, from: data), creature.isValid {
    frames = ProxCreatureFrames.render(creature, dark: dark,
        sunHx: -0.45, sunElevation: 0.72, sunIntensity: 0.8, frameCount: frameCount, px: px)
} else {
    fputs("prox-sigil-export: invalid character or unsupported schema\n", stderr)
    exit(2)
}
guard let first = frames.first else {
    fputs("prox-sigil-export: renderer returned no frames\n", stderr)
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
