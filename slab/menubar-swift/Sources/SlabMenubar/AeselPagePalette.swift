import AppKit

/// Terminal.app does not consistently apply live OSC palette changes, so Slab
/// hands each Aesel session its actual status page in sRGB, independent of
/// system theme. Aesel draws its own @handle; Slab only supplies the color.
final class AeselPagePalette {
    private var backgrounds: [String: [Int]] = [:]
    private var directory: URL {
        URL(fileURLWithPath: Paths.activePromptsDir).deletingLastPathComponent().appendingPathComponent("aesel-handles")
    }

    func setBackground(sessionId: String, color: NSColor) {
        guard let color = color.usingColorSpace(.sRGB) else { return }
        let rgb = [color.redComponent, color.greenComponent, color.blueComponent].map { Int(($0 * 255).rounded()) }
        guard backgrounds[sessionId] != rgb else { return }
        let page: [String: Any] = ["schema": 1, "sessionId": sessionId, "background": rgb]
        guard let data = try? JSONSerialization.data(withJSONObject: page) else { return }
        do {
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
            try data.write(to: directory.appendingPathComponent(sessionId + ".json.palette"), options: .atomic)
            backgrounds[sessionId] = rgb
        } catch { }
    }

    func sync(_ live: [ClaudeSession]) {
        let ids = Set(live.filter { $0.agentType == "easel" && !$0.isDesktopEasel }.map(\.sessionId))
        for id in Array(backgrounds.keys) where !ids.contains(id) {
            backgrounds.removeValue(forKey: id)
            try? FileManager.default.removeItem(at: directory.appendingPathComponent(id + ".json.palette"))
        }
    }
}
