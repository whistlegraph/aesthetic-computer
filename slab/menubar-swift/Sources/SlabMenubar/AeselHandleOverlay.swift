import AppKit
import ApplicationServices

/// Aesel reserves the cells; Slab supplies the same living letters as its titles.
/// A short acknowledgement lease keeps a stopped Slab from leaving blank text.
final class AeselHandleOverlays {
    private final class HandleView: NSView {
        var hovered = false
        var onClick: (() -> Void)?
        func setHovered(_ value: Bool) {
            guard hovered != value else { return }
            hovered = value
            guard value, !NSWorkspace.shared.accessibilityDisplayShouldReduceMotion else { return }
            let wiggle = CAKeyframeAnimation(keyPath: "transform.rotation.z")
            wiggle.values = [0, -0.035, 0.035, -0.02, 0.015, 0]
            wiggle.duration = 0.45
            layer?.add(wiggle, forKey: "handleGreeting")
        }
        override func accessibilityPerformPress() -> Bool { onClick?(); return true }
    }

    private struct Slot: Decodable {
        let schema: Int
        let sessionId: String
        let pid: Int
        let token: String
        let at: Double
        let text: String
        let column: Int
        let row: Int
        let columns: Int
        let rows: Int
        let width: Int
        let colors: [[Double]]
        let hovered: Bool?

        func valid(for session: ClaudeSession) -> Bool {
            let age = Date().timeIntervalSince1970 * 1000 - at
            return schema == 1 && sessionId == session.sessionId && pid == session.claudePid
                && UUID(uuidString: token) != nil && age >= -1000 && age < 6000
                && text.hasPrefix("@") && text.count > 1 && text.count <= 64
                && !text.unicodeScalars.contains(where: { CharacterSet.controlCharacters.contains($0) })
                && columns >= 32 && rows >= 10 && width > 0 && column >= 0
                && column + width <= columns && row >= 0 && row < rows
                && colors.count <= 64 && colors.allSatisfy { $0.count == 3 && $0.allSatisfy { $0.isFinite && $0 >= 0 && $0 <= 255 } }
        }
    }

    private final class Surface {
        let panel: NSPanel
        let root = CALayer()
        let view = HandleView()
        let slot: Slot
        let windowID: Int
        let measuredSize: CGSize
        let relative: CGRect
        var lastAck = Date.distantPast
        var lastSeen = Date()
        var probed = Date()

        init(slot: Slot, windowID: Int, window: CGRect, rect: CGRect) {
            self.slot = slot; self.windowID = windowID
            measuredSize = window.size
            relative = rect.offsetBy(dx: -window.minX, dy: -window.minY)
            let initial = CGRect(x: -2000, y: -2000, width: rect.width + 12, height: rect.height + 12)
            panel = NSPanel(contentRect: initial, styleMask: [.borderless, .nonactivatingPanel], backing: .buffered, defer: false)
            panel.isReleasedWhenClosed = false
            panel.isOpaque = false; panel.backgroundColor = .clear; panel.hasShadow = false
            // Terminal owns mouse reporting and Slab's existing cursor bridge.
            // A transparent native label must not intercept the footer click.
            panel.ignoresMouseEvents = true; panel.hidesOnDeactivate = false
            panel.level = .floating
            panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .ignoresCycle]
            view.frame = CGRect(origin: .zero, size: initial.size)
            view.wantsLayer = true; view.layer = root
            view.onClick = {
                var url = URLComponents()
                url.scheme = "https"; url.host = "aesthetic.computer"; url.path = "/" + slot.text
                if let destination = url.url { NSWorkspace.shared.open(destination) }
            }
            view.setAccessibilityElement(true)
            view.setAccessibilityRole(.link)
            view.setAccessibilityLabel("Open \(slot.text)")
            panel.contentView = view
            root.frame = CGRect(x: 0, y: 0, width: rect.width + 12, height: rect.height + 12)
            let natural = min(18, rect.height * 0.8)
            let size = min(natural, natural * max(1, rect.width - 3) / max(1, AeselRock.width(slot.text, size: natural)))
            let scale = NSScreen.main?.backingScaleFactor ?? 2
            let start = CACurrentMediaTime()
            var x = 6 + (rect.width - AeselRock.width(slot.text, size: size)) / 2
            for (index, letter) in slot.text.enumerated() {
                let rgb = index < slot.colors.count ? slot.colors[index] : [255, 255, 255]
                let face = NSColor(srgbRed: rgb[0] / 255, green: rgb[1] / 255, blue: rgb[2] / 255, alpha: 1)
                let glyph = AeselRock.glyph(String(letter), size: size, face: face)
                let rest = AeselRock.rest(index, of: slot.text)
                let layer = CALayer()
                layer.contentsScale = scale
                layer.contents = glyph.image.layerContents(forContentsScale: scale)
                layer.bounds = CGRect(origin: .zero, size: glyph.image.size)
                layer.position = CGPoint(x: x + glyph.advance.width / 2, y: root.bounds.midY)
                layer.transform = CATransform3DRotate(CATransform3DMakeTranslation(0, rest.lift, 0), rest.radians, 0, 0, 1)
                root.addSublayer(layer)
                AeselRock.addSway(to: layer, index: index, from: start)
                x += glyph.advance.width
            }
        }
    }

    private var sessions: [ClaudeSession] = []
    private var surfaces: [String: Surface] = [:]
    private var pending = Set<String>()
    private var nextRead: [String: Date] = [:]
    private var generation = 0
    private var backgrounds: [String: [Int]] = [:]
    private let queue = DispatchQueue(label: "computer.slab.aesel-handles", qos: .utility)
    private var directory: URL {
        URL(fileURLWithPath: Paths.activePromptsDir).deletingLastPathComponent().appendingPathComponent("aesel-handles")
    }

    /// Terminal.app does not consistently apply live OSC palette changes.
    /// Give Aesel the actual status page in sRGB, independent of system theme.
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
        sessions = live.filter { $0.agentType == "easel" && !$0.isDesktopEasel }
        let ids = Set(sessions.map(\.sessionId))
        for id in Array(surfaces.keys) where !ids.contains(id) { remove(id) }
        for id in Array(backgrounds.keys) where !ids.contains(id) {
            backgrounds.removeValue(forKey: id)
            try? FileManager.default.removeItem(at: directory.appendingPathComponent(id + ".json.palette"))
        }
        nextRead = nextRead.filter { ids.contains($0.key) }
    }

    func close() {
        generation += 1; sessions = []; pending.removeAll(); nextRead.removeAll()
        for id in Array(surfaces.keys) { remove(id) }
    }

    private func remove(_ id: String) {
        surfaces.removeValue(forKey: id)?.panel.close()
        try? FileManager.default.removeItem(at: directory.appendingPathComponent(id + ".json.ack"))
    }

    func update(bindings: [String: Int], windows: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)],
                stack: [(num: Int, rect: CGRect)], screenHeight: CGFloat, eco: Bool) {
        let now = Date()
        for session in sessions {
            let id = session.sessionId
            guard let number = bindings[session.overlayBindingKey], let b = windows[number] else {
                surfaces[id]?.panel.orderOut(nil)
                continue
            }
            let window = CGRect(x: b.0, y: b.1, width: b.2, height: b.3)
            if let surface = surfaces[id] {
                let rect = surface.relative.offsetBy(dx: window.minX, dy: window.minY)
                let points = [CGPoint(x: rect.minX, y: rect.midY), CGPoint(x: rect.midX, y: rect.midY), CGPoint(x: rect.maxX, y: rect.midY)]
                let visible = surface.windowID == number && surface.measuredSize == window.size
                    && now.timeIntervalSince(surface.lastSeen) < 6
                    && points.allSatisfy { point in stack.first(where: { $0.rect.contains(point) })?.num == number }
                if visible {
                    let frame = CGRect(x: rect.minX - 6, y: screenHeight - rect.maxY - 6, width: rect.width + 12, height: rect.height + 12)
                    if surface.panel.frame != frame { surface.panel.setFrame(frame, display: false) }
                    surface.root.speed = eco || NSWorkspace.shared.accessibilityDisplayShouldReduceMotion ? 0 : surface.view.hovered ? 2.5 : 1
                    if !surface.panel.isVisible { surface.panel.orderFrontRegardless() }
                    if now.timeIntervalSince(surface.lastAck) >= 1 {
                        let ack: [String: Any] = ["schema": 1, "sessionId": id, "token": surface.slot.token,
                                                  "at": now.timeIntervalSince1970 * 1000]
                        if let data = try? JSONSerialization.data(withJSONObject: ack) {
                            try? data.write(to: directory.appendingPathComponent(id + ".json.ack"), options: .atomic)
                            surface.lastAck = now
                        }
                    }
                } else { surface.panel.orderOut(nil) }
            }
            guard !pending.contains(id), now >= (nextRead[id] ?? .distantPast) else { continue }
            pending.insert(id); nextRead[id] = now.addingTimeInterval(0.5)
            let old = surfaces[id]
            let oldToken = old?.slot.token
            let needsProbe = old == nil || old?.windowID != number || old?.measuredSize != window.size
                || now.timeIntervalSince(old?.probed ?? .distantPast) >= 3
            let file = directory.appendingPathComponent(id + ".json")
            let epoch = generation
            queue.async { [weak self] in
                let slot = Self.read(file, session: session)
                let probe = needsProbe || slot?.token != oldToken
                let rect = probe ? slot.flatMap { Self.bounds(for: $0, windowID: number, window: window) } : nil
                DispatchQueue.main.async {
                    guard let self = self, self.generation == epoch else { return }
                    self.pending.remove(id)
                    guard self.sessions.contains(where: { $0.sessionId == id && $0.claudePid == session.claudePid }) else { return }
                    guard let slot = slot else { self.remove(id); return }
                    if probe {
                        guard let rect = rect else { self.remove(id); return }
                        // Preserve the running letter animation when the measurement is unchanged.
                        if let current = self.surfaces[id], current.slot.token == slot.token,
                           current.windowID == number, current.measuredSize == window.size,
                           current.relative == rect.offsetBy(dx: -window.minX, dy: -window.minY) {
                            current.lastSeen = Date(); current.probed = Date()
                        } else {
                            self.remove(id)
                            self.surfaces[id] = Surface(slot: slot, windowID: number, window: window, rect: rect)
                        }
                    } else { self.surfaces[id]?.lastSeen = Date() }
                    self.surfaces[id]?.view.setHovered(slot.hovered ?? false)
                }
            }
        }
    }

    private static func read(_ file: URL, session: ClaudeSession) -> Slot? {
        guard let attrs = try? FileManager.default.attributesOfItem(atPath: file.path),
              attrs[.type] as? FileAttributeType == .typeRegular,
              (attrs[.ownerAccountID] as? NSNumber)?.uint32Value == getuid(),
              let size = attrs[.size] as? NSNumber, size.intValue <= 16384,
              let data = try? Data(contentsOf: file), let slot = try? JSONDecoder().decode(Slot.self, from: data),
              slot.valid(for: session) else { return nil }
        return slot
    }

    /// AX gives the actual character bounds, including Terminal's font zoom,
    /// padding and tabs. No guessed grid offsets and no focus changes.
    private static func bounds(for slot: Slot, windowID: Int, window: CGRect) -> CGRect? {
        func attribute(_ element: AXUIElement, _ name: String) -> CFTypeRef? {
            var value: CFTypeRef?
            return AXUIElementCopyAttributeValue(element, name as CFString, &value) == .success ? value : nil
        }
        func textArea(_ element: AXUIElement, depth: Int = 0) -> AXUIElement? {
            if attribute(element, kAXRoleAttribute) as? String == kAXTextAreaRole { return element }
            guard depth < 8 else { return nil }
            for child in attribute(element, kAXChildrenAttribute) as? [AXUIElement] ?? [] {
                if let found = textArea(child, depth: depth + 1) { return found }
            }
            return nil
        }
        for app in NSRunningApplication.runningApplications(withBundleIdentifier: "com.apple.Terminal") {
            let ax = AXUIElementCreateApplication(app.processIdentifier)
            AXUIElementSetMessagingTimeout(ax, 0.3)
            for candidate in attribute(ax, kAXWindowsAttribute) as? [AXUIElement] ?? [] {
                guard AXTiler.windowID(candidate) == CGWindowID(windowID), let area = textArea(candidate),
                      let value = attribute(area, kAXValueAttribute) as? String else { continue }
                let range = (value as NSString).range(of: slot.text, options: .backwards)
                guard range.location != NSNotFound else { return nil }
                var cfRange = CFRange(location: range.location, length: range.length)
                guard let parameter = AXValueCreate(.cfRange, &cfRange) else { return nil }
                var result: CFTypeRef?
                guard AXUIElementCopyParameterizedAttributeValue(area, kAXBoundsForRangeParameterizedAttribute as CFString,
                    parameter, &result) == .success, let result = result, CFGetTypeID(result) == AXValueGetTypeID() else { return nil }
                var rect = CGRect.zero
                guard AXValueGetValue(result as! AXValue, .cgRect, &rect), rect.width > 0, rect.height > 0,
                      rect.height < 80, window.contains(rect), rect.maxY > window.maxY - max(120, window.height * 0.25) else { return nil }
                return rect
            }
        }
        return nil
    }
}
