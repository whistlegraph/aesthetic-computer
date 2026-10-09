import AppKit
import ImageIO
import UniformTypeIdentifiers

// Compile with ProxCreature.swift and ProxCreatureFrames.swift. No daemon or
// session data: these fixtures render the production geometry at badge size.
@main
enum CreaturePreview {
    static func main() throws {
        let directory = URL(fileURLWithPath: CommandLine.arguments[1], isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        let canvas = NSImage(size: NSSize(width: 960, height: 640))
        var portraits: [(String, CGImage, NSRect)] = []
        for i in 0..<8 {
            var creature = ProxCreature.egg(id: "preview", name: "miva", seed: 123)
            creature.grow(by: [0, 1800, 7200, 28800][i % 4])
            // Every stage must read without any inference-selected traits.
            let mood: ProxCreatureFrames.Mood = .working
            let frames = ProxCreatureFrames.render(creature, dark: i < 4,
                sunHx: -0.45, sunElevation: 0.72, sunIntensity: 0.8,
                mood: mood, luminous: false, frameCount: i == 1 ? 72 : 1, px: 240)
            guard let first = frames.first else { fatalError("Missing render \(i)") }
            let rect = NSRect(x: (i % 4) * 240, y: i < 4 ? 320 : 0, width: 240, height: 320)
            portraits.append(("\(creature.stage.rawValue) · \(mood.rawValue)", first, rect))
            try JSONEncoder().encode(creature).write(to: directory.appendingPathComponent("character-\(i).json"))
            if i == 1 {
                let gif = CGImageDestinationCreateWithURL(directory.appendingPathComponent("hatching.gif") as CFURL,
                    UTType.gif.identifier as CFString, frames.count, nil)!
                CGImageDestinationSetProperties(gif, [kCGImagePropertyGIFDictionary: [kCGImagePropertyGIFLoopCount: 0]] as CFDictionary)
                for frame in frames {
                    CGImageDestinationAddImage(gif, frame, [kCGImagePropertyGIFDictionary:
                        [kCGImagePropertyGIFDelayTime: 4.0 / Double(frames.count)]] as CFDictionary)
                }
                guard CGImageDestinationFinalize(gif) else { fatalError("Could not write GIF") }
            }
        }
        canvas.lockFocus()
        NSColor(calibratedWhite: 0.09, alpha: 1).setFill()
        NSRect(x: 0, y: 320, width: 960, height: 320).fill()
        NSColor(calibratedWhite: 0.93, alpha: 1).setFill()
        NSRect(x: 0, y: 0, width: 960, height: 320).fill()
        for (label, cg, rect) in portraits {
            let image = NSImage(cgImage: cg, size: NSSize(width: 240, height: 240))
            image.draw(in: NSRect(x: rect.minX, y: rect.minY + 65, width: 240, height: 240))
            image.draw(in: NSRect(x: rect.minX + 16, y: rect.minY + 8, width: 56, height: 56))
            (label as NSString).draw(at: NSPoint(x: rect.minX + 78, y: rect.minY + 28),
                withAttributes: [.font: NSFont.systemFont(ofSize: 11),
                                 .foregroundColor: rect.minY > 0 ? NSColor.white : NSColor.black])
        }
        canvas.unlockFocus()
        let cg = canvas.cgImage(forProposedRect: nil, context: nil, hints: nil)!
        let png = CGImageDestinationCreateWithURL(directory.appendingPathComponent("preview.png") as CFURL,
            UTType.png.identifier as CFString, 1, nil)!
        CGImageDestinationAddImage(png, cg, nil)
        guard CGImageDestinationFinalize(png) else { fatalError("Could not write PNG") }
        print(directory.appendingPathComponent("preview.png").path)
    }
}
