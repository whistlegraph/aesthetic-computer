import AppKit

struct NarrationSpec: Codable {
    var title: String
    var outDir: String
    var gapMs: Int?
    var lines: [NarrationLine]
}

struct NarrationLine: Codable {
    var id: String
    var title: String
    var text: String
    var image: String?
}

struct NarrationTake: Codable {
    var path: String
    var duration: Double
    var recordedAt: String
}

struct NarrationLineState: Codable {
    var id: String
    var selectedTake: String?
    var takes: [NarrationTake]
}

struct NarrationManifest: Codable {
    var formatVersion: Int
    var projectTitle: String
    var sourceSpec: String
    var updatedAt: String
    var gapMs: Int
    var lines: [NarrationLineState]
}

final class NarratorWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: NarratorWizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.applicationIconImage = NarratorIcon.make(size: 512)
        guard CommandLine.arguments.count >= 2 else {
            let alert = NSAlert()
            alert.messageText = "Narrator Wizard needs a screenplay"
            alert.informativeText = "Run narrator-wizard/bin/narratorwizard and choose a narration-spec.json file."
            alert.runModal()
            NSApp.terminate(nil)
            return
        }
        let specURL = URL(fileURLWithPath: CommandLine.arguments[1]).standardizedFileURL
        do {
            let spec = try JSONDecoder().decode(NarrationSpec.self, from: Data(contentsOf: specURL))
            guard !spec.lines.isEmpty else { throw CocoaError(.fileReadCorruptFile) }
            wizard = try NarratorWizardController(spec: spec, specURL: specURL)
            wizard?.window?.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
        } catch {
            let alert = NSAlert(error: error)
            alert.messageText = "Could not open the narration screenplay"
            alert.runModal()
            NSApp.terminate(nil)
        }
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

enum NarratorIcon {
    static func make(size: CGFloat) -> NSImage {
        NSImage(size: NSSize(width: size, height: size), flipped: false) { rect in
            let inset = size * 0.07
            let bg = NSBezierPath(roundedRect: rect.insetBy(dx: inset, dy: inset), xRadius: size * 0.22, yRadius: size * 0.22)
            NSColor(calibratedRed: 0.95, green: 0.80, blue: 0.25, alpha: 1).setFill()
            bg.fill()
            NSColor(calibratedWhite: 0.10, alpha: 1).setStroke()
            bg.lineWidth = size * 0.025
            bg.stroke()

            let mic = NSBezierPath(roundedRect: NSRect(x: size * 0.37, y: size * 0.34, width: size * 0.26, height: size * 0.40), xRadius: size * 0.13, yRadius: size * 0.13)
            NSColor(calibratedWhite: 0.12, alpha: 1).setFill()
            mic.fill()
            let cradle = NSBezierPath()
            cradle.move(to: NSPoint(x: size * 0.28, y: size * 0.48))
            cradle.curve(to: NSPoint(x: size * 0.72, y: size * 0.48), controlPoint1: NSPoint(x: size * 0.28, y: size * 0.20), controlPoint2: NSPoint(x: size * 0.72, y: size * 0.20))
            cradle.move(to: NSPoint(x: size * 0.50, y: size * 0.28))
            cradle.line(to: NSPoint(x: size * 0.50, y: size * 0.16))
            cradle.move(to: NSPoint(x: size * 0.38, y: size * 0.16))
            cradle.line(to: NSPoint(x: size * 0.62, y: size * 0.16))
            cradle.lineWidth = size * 0.045
            cradle.lineCapStyle = .round
            NSColor(calibratedWhite: 0.12, alpha: 1).setStroke()
            cradle.stroke()
            return true
        }
    }
}
