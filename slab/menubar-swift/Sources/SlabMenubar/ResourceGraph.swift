import AppKit
import Darwin
import IOKit

private final class ResourceHoverTracker: NSView {
    var onEnter: (() -> Void)?
    var onExit: (() -> Void)?
    var onMove: ((NSPoint) -> Void)?
    private var tracking: NSTrackingArea?

    override func updateTrackingAreas() {
        if let tracking { removeTrackingArea(tracking) }
        let area = NSTrackingArea(rect: bounds,
                                  options: [.mouseEnteredAndExited, .mouseMoved,
                                            .activeAlways, .inVisibleRect],
                                  owner: self, userInfo: nil)
        addTrackingArea(area)
        tracking = area
        super.updateTrackingAreas()
    }

    override func mouseEntered(with event: NSEvent) {
        onEnter?()
        onMove?(convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        onMove?(convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) { onExit?() }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }
}

/// A Slab-owned replacement for the Stats menu extra. The status item is one
/// continuous data balloon: it grows from one rotating channel to the three
/// most useful live channels, then all five when the current display is wide
/// enough. Fast probes and presentation run at 4 Hz; slower IOKit/filesystem
/// probes are deliberately staggered so the fluid display stays inexpensive.
final class ResourceGraph: NSObject {
    static let shared = ResourceGraph()

    private struct FleetWorker {
        let name: String
        let online: Bool
        let cpu: Double
        let ram: Double
        let disk: Double
        let memoryGB: Double
        let cores: Int
        let pressure: Bool
        let reason: String
        let accepting: Bool
        let active: Int
        let queued: Int
        let role: String
    }

    /// Three transparent, status-item-width rows of the same category TVs as
    /// the main strip. There is intentionally no enclosing card: hovering the
    /// local instrument simply reveals more instruments directly beneath it.
    private final class FleetStripView: NSView {
        var workers: [FleetWorker] = [] { didSet { needsDisplay = true } }
        var highlightedMetricIndex: Int? { didSet { needsDisplay = true } }

        private let colors = [
            NSColor(srgbRed: 0.10, green: 0.84, blue: 0.34, alpha: 1),
            NSColor(srgbRed: 1.00, green: 0.24, blue: 0.55, alpha: 1),
            NSColor(srgbRed: 0.00, green: 0.68, blue: 1.00, alpha: 1),
            NSColor(srgbRed: 1.00, green: 0.55, blue: 0.06, alpha: 1),
            NSColor(srgbRed: 0.62, green: 0.30, blue: 1.00, alpha: 1),
        ]

        private var darkAppearance: Bool {
            effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        }

        override func draw(_ dirtyRect: NSRect) {
            super.draw(dirtyRect)
            let dark = darkAppearance
            (dark
                ? NSColor(srgbRed: 0.035, green: 0.050, blue: 0.070, alpha: 1)
                : NSColor(srgbRed: 0.94, green: 0.96, blue: 0.97, alpha: 1)).setFill()
            bounds.fill()
            (dark ? NSColor.white : NSColor.black).withAlphaComponent(0.14).setStroke()
            let boardEdge = NSBezierPath(rect: bounds.insetBy(dx: 0.5, dy: 0.5))
            boardEdge.lineWidth = 1
            boardEdge.stroke()
            let content = bounds.insetBy(dx: 4, dy: 4)
            let rowHeight: CGFloat = 20
            let labelWidth: CGFloat = 64
            let gap: CGFloat = 2
            let stripWidth = content.width - labelWidth
            for (index, worker) in workers.prefix(5).enumerated() {
                let y = content.maxY - CGFloat(index + 1) * rowHeight - CGFloat(index) * gap
                drawLabel(worker, in: NSRect(x: content.minX, y: y,
                                              width: labelWidth - 4, height: rowHeight))
                drawStrip(worker, in: NSRect(x: content.minX + labelWidth, y: y,
                                              width: stripWidth, height: rowHeight))
            }
        }

        private func drawLabel(_ worker: FleetWorker, in rect: NSRect) {
            let dot = worker.online
                ? (worker.accepting ? NSColor.systemGreen : worker.pressure ? .systemRed : .systemOrange)
                : NSColor.tertiaryLabelColor
            dot.setFill()
            NSBezierPath(ovalIn: NSRect(x: rect.minX + 1, y: rect.midY - 2.5, width: 5, height: 5)).fill()
            let name = worker.name.uppercased()
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 7.2, weight: .semibold),
                .foregroundColor: (darkAppearance ? NSColor.white : NSColor.black)
                    .withAlphaComponent(worker.online ? 0.88 : 0.34),
                .kern: 0.05,
            ]
            let text = NSAttributedString(string: name, attributes: attrs)
            text.draw(at: NSPoint(x: rect.minX + 9, y: rect.midY - text.size().height / 2))
        }

        private func drawStrip(_ worker: FleetWorker, in rect: NSRect) {
            let labels = ["CPU", "RAM", "NET", "SSD", "GPU"]
            let values = worker.online
                ? [String(format: "%.0f%%", worker.cpu), String(format: "%.0f%%", worker.ram),
                   "—", String(format: "%.0f%%", worker.disk), "—"]
                : ["—", "—", "—", "—", "—"]
            let cellWidth = rect.width / 5
            for index in 0..<5 {
                let cell = NSRect(x: rect.minX + CGFloat(index) * cellWidth + 0.5,
                                  y: rect.minY + 0.5, width: cellWidth - 1, height: rect.height - 1)
                let path = NSBezierPath(roundedRect: cell, xRadius: 1, yRadius: 1)
                let color = colors[index]
                (darkAppearance
                    ? NSColor(srgbRed: 0.015, green: 0.025, blue: 0.038, alpha: 0.96)
                    : NSColor(srgbRed: 0.97, green: 0.98, blue: 0.985, alpha: 0.98)).setFill()
                path.fill()
                color.withAlphaComponent(worker.online ? 0.16 : 0.025).setFill()
                path.fill()
                if worker.online {
                    let raw = [worker.cpu, worker.ram, 0, worker.disk, 0][index] / 100
                    drawTexture(value: raw, color: color, in: cell)
                }
                let tagRect = NSRect(x: cell.minX + 1, y: cell.maxY - 7,
                                     width: cell.width - 2, height: 6)
                color.withAlphaComponent(worker.online ? 0.90 : 0.20).setFill()
                tagRect.fill()
                let tag = NSAttributedString(string: labels[index], attributes: [
                    .font: NSFont.monospacedSystemFont(ofSize: 5.2, weight: .heavy),
                    .foregroundColor: NSColor.black.withAlphaComponent(worker.online ? 0.90 : 0.42),
                    .kern: 0.05,
                ])
                tag.draw(at: NSPoint(x: tagRect.midX - tag.size().width / 2,
                                     y: tagRect.midY - tag.size().height / 2 + 0.2))
                (worker.pressure ? NSColor.systemRed : color)
                    .withAlphaComponent(worker.online ? (highlightedMetricIndex == index ? 0.96 : 0.62) : 0.13).setStroke()
                path.lineWidth = worker.pressure || highlightedMetricIndex == index ? 1.05 : 0.55
                path.stroke()
                if highlightedMetricIndex == index {
                    let attrs: [NSAttributedString.Key: Any] = [
                        .font: NSFont.monospacedDigitSystemFont(ofSize: 8.5, weight: .heavy),
                        .foregroundColor: (darkAppearance ? NSColor.white : NSColor.black)
                            .withAlphaComponent(worker.online ? 0.98 : 0.38),
                        .kern: -0.10,
                    ]
                    let text = NSAttributedString(string: values[index], attributes: attrs)
                    text.draw(at: NSPoint(x: cell.midX - text.size().width / 2,
                                          y: cell.midY - text.size().height / 2))
                }
            }
        }

        private func drawTexture(value: Double, color: NSColor, in rect: NSRect) {
            let graph = NSRect(x: rect.minX + 1.5, y: rect.minY + 1.5,
                               width: rect.width - 3, height: max(3, rect.height - 10))
            let columns = 7
            let rows = 3
            let filled = Int(max(0, min(1, value)) * Double(rows))
            let pixelWidth = graph.width / CGFloat(columns)
            let pixelHeight = graph.height / CGFloat(rows)
            for x in 0..<columns {
                for y in 0..<rows {
                    color.withAlphaComponent(y < filled ? 0.82 : (darkAppearance ? 0.09 : 0.13)).setFill()
                    NSRect(x: graph.minX + CGFloat(x) * pixelWidth + 0.45,
                           y: graph.minY + CGFloat(y) * pixelHeight + 0.35,
                           width: max(1, pixelWidth - 0.9),
                           height: max(1, pixelHeight - 0.7)).fill()
                }
            }
        }
    }

    private struct Sample {
        var ram = 0.0, ssd = 0.0, gpu = 0.0, cpu = 0.0, net = 0.0
        var down = 0.0, up = 0.0
        var ramUsedGB = 0.0, ramTotalGB = 0.0
        var ssdUsedGB = 0.0, ssdFreeGB = 0.0, ssdTotalGB = 0.0
        var gpuMemoryGB = 0.0
        var load = 0.0
    }

    private enum Metric: CaseIterable {
        case cpu, ram, network, ssd, gpu

        var name: String {
            switch self {
            case .cpu: return "CPU"
            case .ram: return "RAM"
            case .network: return "NET"
            case .ssd: return "SSD"
            case .gpu: return "GPU"
            }
        }

        var color: NSColor {
            switch self {
            case .cpu: return NSColor(srgbRed: 0.10, green: 0.84, blue: 0.34, alpha: 1)
            case .ram: return NSColor(srgbRed: 1.00, green: 0.24, blue: 0.55, alpha: 1)
            case .network: return NSColor(srgbRed: 0.00, green: 0.68, blue: 1.00, alpha: 1)
            case .ssd: return NSColor(srgbRed: 1.00, green: 0.55, blue: 0.06, alpha: 1)
            case .gpu: return NSColor(srgbRed: 0.62, green: 0.30, blue: 1.00, alpha: 1)
            }
        }

        func value(_ sample: Sample) -> Double {
            switch self {
            case .cpu: return sample.cpu
            case .ram: return sample.ram
            case .network: return sample.net
            case .ssd: return sample.ssd
            case .gpu: return sample.gpu
            }
        }
    }

    private enum Layout: Equatable {
        case compact, live, complete

        var width: CGFloat {
            switch self {
            case .compact: return 48
            case .live: return 120
            case .complete: return 200
            }
        }
    }

    private var item: NSStatusItem?
    private var timer: Timer?
    private var history: [Sample] = []
    private var previousCPU: (idle: UInt64, total: UInt64)?
    private var previousNetwork: (down: UInt64, up: UInt64, at: TimeInterval)?
    private var sample = Sample()
    private var tickCount = 0
    private var sampleInFlight = false
    private var currentLayout = Layout.compact
    private var hoverTracker: ResourceHoverTracker?
    private var hoverPanel: NSPanel?
    private var hoverPinned = false
    private var highlightedMetric: Metric?
    private let fleetStripView = FleetStripView(frame: .zero)
    private var hoverGlobalMonitor: Any?
    private var hoverLocalMonitor: Any?
    private var fleetWorkers: [FleetWorker] = []
    private var fleetRefreshInFlight = false
    private var fleetRefreshedAt = Date.distantPast

    private let sampleInterval: TimeInterval = 1.0 / 4.0
    private let historyLimit = 60        // 15 seconds at 4 Hz.
    private let networkStride = 2        // Network reads at 2 Hz to calm short bursts.
    private let gpuStride = 8            // IOKit once per two seconds.
    private let diskStride = 40          // statfs once per ten seconds.

    /// Match Menu Band's live status-bar sizing: start with its 22pt base
    /// height, then grow into roomier/notched menu bars without clipping.
    private var displayHeight: CGFloat {
        let baseHeight: CGFloat = 22
        let scale = max(1.0, min(1.6,
            (NSStatusBar.system.thickness - 0.5) / baseHeight))
        return ceil(baseHeight * scale)
    }

    var enabled: Bool { FileManager.default.fileExists(atPath: Paths.resourceGraphFlag) }

    func syncEnabled() {
        enabled ? start() : stop()
    }

    func toggle() {
        let fm = FileManager.default
        if enabled {
            try? fm.removeItem(atPath: Paths.resourceGraphFlag)
        } else {
            try? fm.createDirectory(atPath: (Paths.resourceGraphFlag as NSString).deletingLastPathComponent,
                                    withIntermediateDirectories: true)
            fm.createFile(atPath: Paths.resourceGraphFlag, contents: nil)
        }
        syncEnabled()
    }

    func stop() {
        timer?.invalidate()
        timer = nil
        if let item { NSStatusBar.system.removeStatusItem(item) }
        item = nil
        history.removeAll()
        previousCPU = nil
        previousNetwork = nil
        sampleInFlight = false
        hoverPanel?.orderOut(nil)
        hoverPanel = nil
        hoverTracker?.removeFromSuperview()
        hoverTracker = nil
        if let hoverGlobalMonitor { NSEvent.removeMonitor(hoverGlobalMonitor) }
        if let hoverLocalMonitor { NSEvent.removeMonitor(hoverLocalMonitor) }
        hoverGlobalMonitor = nil
        hoverLocalMonitor = nil
    }

    private func start() {
        guard item == nil else { return }
        currentLayout = preferredLayout()
        let status = NSStatusBar.system.statusItem(withLength: currentLayout.width)
        status.button?.imagePosition = .imageOnly
        status.button?.imageScaling = .scaleNone
        status.button?.target = self
        status.button?.action = #selector(toggleHoverBoard(_:))
        status.button?.sendAction(on: [.leftMouseUp])
        item = status
        installHoverTracker(on: status.button)
        tick(forceSlow: true)
        let t = Timer(timeInterval: sampleInterval, repeats: true) { [weak self] _ in self?.tick() }
        t.tolerance = sampleInterval * 0.12
        timer = t
        RunLoop.main.add(t, forMode: .common)
    }

    private func tick(forceSlow: Bool = false) {
        guard !sampleInFlight else { return }
        sampleInFlight = true
        let prior = sample
        let nextTick = tickCount + 1
        let readNetwork = forceSlow || nextTick % networkStride == 0
        let readGPU = forceSlow || nextTick % gpuStride == 0
        let readDisk = forceSlow || nextTick % diskStride == 0
        DispatchQueue.global(qos: .utility).async { [weak self] in
            guard let self else { return }
            let fresh = self.readSample(previous: prior, readNetwork: readNetwork,
                                        readGPU: readGPU, readDisk: readDisk)
            DispatchQueue.main.async { [weak self] in
                guard let self else { return }
                self.sampleInFlight = false
                guard self.item != nil else { return }
                self.sample = fresh
                self.history.append(fresh)
                if self.history.count > self.historyLimit {
                    self.history.removeFirst(self.history.count - self.historyLimit)
                }
                self.tickCount += 1
                if self.tickCount == 1 || self.tickCount % 8 == 0 {
                    self.updateLayout()
                }
                self.redraw()
            }
        }
    }

    private func preferredLayout() -> Layout {
        let screen = item?.button?.window?.screen ?? NSScreen.main
        let width = screen?.visibleFrame.width ?? 1200
        // A 200pt five-channel instrument fits ordinary 1380pt+ desktops while
        // leaving the foreground app menus breathing room. Smaller displays
        // fall back to the live trio, then one rotating channel.
        if width >= 1380 { return .complete }
        if width >= 1100 { return .live }
        return .compact
    }

    private func updateLayout() {
        let layout = preferredLayout()
        guard layout != currentLayout else { return }
        currentLayout = layout
        item?.length = layout.width
    }

    private var visibleMetrics: [Metric] {
        switch currentLayout {
        case .compact:
            let all = Metric.allCases
            return [all[(tickCount / 16) % all.count]]
        case .live:
            return [.cpu, .ram, .network]
        case .complete:
            return Metric.allCases
        }
    }

    private func redraw() {
        guard let button = item?.button else { return }
        button.image = render()
        button.contentTintColor = nil
        // The same board serves hover and click; there is deliberately no
        // second click-only menu or tooltip path.
        guard tickCount == 1 || tickCount % 4 == 0 else { return }
        button.toolTip = nil
        refreshHoverCard()
    }

    private func render() -> NSImage {
        let size = NSSize(width: currentLayout.width - 2, height: displayHeight)
        let image = NSImage(size: size)
        image.lockFocus()
        NSColor.clear.setFill()
        NSRect(origin: .zero, size: size).fill()

        // Five independent LED modules; there is deliberately no wrapper box.
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let metrics = visibleMetrics
        let content = NSRect(x: 0.5, y: 0.5, width: size.width - 1, height: size.height - 1)
        let gap: CGFloat = 2
        let cellWidth = (content.width - gap * CGFloat(max(0, metrics.count - 1))) /
            CGFloat(metrics.count)

        for (index, metric) in metrics.enumerated() {
            let cell = NSRect(x: content.minX + CGFloat(index) * (cellWidth + gap),
                              y: content.minY,
                              width: cellWidth,
                              height: content.height)
            let viewport = NSBezierPath(roundedRect: cell.insetBy(dx: 0.25, dy: 0.25),
                                        xRadius: 1.2, yRadius: 1.2)
            (isDark
                ? NSColor(srgbRed: 0.025, green: 0.035, blue: 0.050, alpha: 0.96)
                : NSColor(srgbRed: 0.965, green: 0.975, blue: 0.98, alpha: 0.98)).setFill()
            viewport.fill()
            metric.color.withAlphaComponent(0.10).setFill()
            viewport.fill()
            drawRaster(metric, in: cell, color: metric.color, dark: isDark)
            drawModuleTag(metric, in: cell)
            if metric == highlightedMetric {
                drawHighlightedValue(metric, in: cell, dark: isDark)
            }
            metric.color.withAlphaComponent(metric == highlightedMetric ? 0.98 : 0.68).setStroke()
            viewport.lineWidth = metric == highlightedMetric ? 1.15 : 0.7
            viewport.stroke()
        }

        image.unlockFocus()
        image.isTemplate = false
        return image
    }

    private func drawRaster(_ metric: Metric, in rect: NSRect, color: NSColor, dark: Bool) {
        let graph = NSRect(x: rect.minX + 1.6, y: rect.minY + 1.5,
                           width: rect.width - 3.2, height: max(3, rect.height - 10))
        let columns = max(4, Int(graph.width / 3.6))
        let rows = 4
        let points = Array(history.suffix(columns))
        guard !points.isEmpty else { return }
        let start = columns - points.count
        let pixelWidth = graph.width / CGFloat(columns)
        let pixelHeight = graph.height / CGFloat(rows)
        for (offset, point) in points.enumerated() {
            let value = max(0, min(1, metric.value(point)))
            let filled = Int(ceil(value * Double(rows)))
            let x = graph.minX + CGFloat(start + offset) * pixelWidth
            for row in 0..<rows {
                let lit = row < filled
                let age = CGFloat(offset + 1) / CGFloat(max(1, points.count))
                color.withAlphaComponent(lit ? 0.48 + age * 0.42 : (dark ? 0.075 : 0.13)).setFill()
                NSRect(x: x + 0.45,
                       y: graph.minY + CGFloat(row) * pixelHeight + 0.35,
                       width: max(1, pixelWidth - 0.9),
                       height: max(1, pixelHeight - 0.7)).fill()
            }
        }
    }

    private func drawModuleTag(_ metric: Metric, in rect: NSRect) {
        let tagRect = NSRect(x: rect.minX + 1.5, y: rect.maxY - 7.8,
                             width: rect.width - 3, height: 6.2)
        metric.color.withAlphaComponent(0.94).setFill()
        tagRect.fill()
        let label = NSAttributedString(string: metric.name, attributes: [
            .font: NSFont.monospacedSystemFont(ofSize: 5.4, weight: .heavy),
            .foregroundColor: NSColor.black.withAlphaComponent(0.88),
            .kern: 0.05,
        ])
        label.draw(at: NSPoint(x: tagRect.midX - label.size().width / 2,
                               y: tagRect.midY - label.size().height / 2 + 0.25))
    }

    /// The TVs are pure raster at rest. Pointer focus reveals one stable label
    /// and value without making the remaining instruments flicker or reflow.
    private func drawHighlightedValue(_ metric: Metric, in rect: NSRect, dark: Bool) {
        (dark
            ? NSColor(srgbRed: 0.015, green: 0.020, blue: 0.030, alpha: 0.94)
            : NSColor(srgbRed: 0.985, green: 0.99, blue: 0.995, alpha: 0.96)).setFill()
        NSBezierPath(roundedRect: rect.insetBy(dx: 0.8, dy: 0.8),
                     xRadius: 0.8, yRadius: 0.8).fill()
        let textColor = dark
            ? NSColor.white.withAlphaComponent(0.94)
            : NSColor.black.withAlphaComponent(0.84)
        let shadow = NSShadow()
        shadow.shadowColor = (dark ? NSColor.black : NSColor.white).withAlphaComponent(0.72)
        shadow.shadowBlurRadius = 1
        shadow.shadowOffset = NSSize(width: 0, height: dark ? -0.5 : 0.5)
        let digitAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 7.5, weight: .heavy),
            .foregroundColor: textColor,
            .kern: -0.12,
            .shadow: shadow,
        ]
        let nameAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: 6.5, weight: .heavy),
            .foregroundColor: textColor.withAlphaComponent(0.94),
            .kern: 0.02,
            .shadow: shadow,
        ]
        let name = NSAttributedString(string: metric.name, attributes: nameAttrs)
        name.draw(at: NSPoint(x: rect.midX - name.size().width / 2,
                              y: rect.midY + 1.5))
        if metric == .network {
            let rate = NSAttributedString(string: "↓\(compactRate(sample.down))",
                                          attributes: digitAttrs)
            rate.draw(at: NSPoint(x: rect.midX - rate.size().width / 2,
                                  y: rect.midY - rate.size().height + 0.5))
            return
        }

        let value = NSAttributedString(
            string: String(format: "%.0f%%", metric.value(sample) * 100),
            attributes: digitAttrs)
        value.draw(at: NSPoint(x: rect.midX - value.size().width / 2,
                               y: rect.midY - value.size().height + 0.5))
    }

    private func compactRate(_ mbPerSecond: Double) -> String {
        if mbPerSecond >= 100 { return String(format: "%.0fM", mbPerSecond) }
        if mbPerSecond >= 10 { return String(format: "%.0fM", mbPerSecond) }
        if mbPerSecond >= 1 { return String(format: "%.1fM", mbPerSecond) }
        let kb = mbPerSecond * 1024
        return String(format: "%.0fK", kb)
    }

    private func drawNetworkBits(in rect: NSRect, dark: Bool) {
        let downColor = Metric.network.color
        let upColor = NSColor(srgbRed: 1.00, green: 0.25, blue: 0.62, alpha: 1)
        drawBitStream(intensity: networkIntensity(sample.down),
                      in: NSRect(x: rect.minX + 2, y: rect.minY + 1.2,
                                 width: rect.width - 4, height: 2),
                      forward: true, color: downColor, dark: dark)
        drawBitStream(intensity: networkIntensity(sample.up),
                      in: NSRect(x: rect.minX + 2, y: rect.maxY - 3.2,
                                 width: rect.width - 4, height: 2),
                      forward: false, color: upColor, dark: dark)
    }

    private func drawMetricBits(_ metric: Metric, in rect: NSRect, dark: Bool) {
        let index = Metric.allCases.firstIndex(of: metric) ?? 0
        drawBitStream(intensity: metric.value(sample),
                      in: NSRect(x: rect.minX + 2, y: rect.minY + 1.2,
                                 width: rect.width - 4, height: 2),
                      forward: index % 2 == 0, color: metric.color, dark: dark)
    }

    private func networkIntensity(_ rate: Double) -> Double {
        min(1, log10(1 + max(0, rate)) / log10(101))
    }

    private func drawBitStream(intensity rawIntensity: Double, in lane: NSRect, forward: Bool,
                               color: NSColor, dark: Bool) {
        let intensity = max(0, min(1, rawIntensity))
        guard intensity >= 0.01 else { return }
        let count = max(1, min(6, Int(ceil(1 + intensity * 5))))
        // Preserve the former visual travel rate after moving 12 → 4 fps.
        let speed = 0.054 + intensity * 0.135
        let bitSize = 1.1 + CGFloat(intensity) * 0.7
        for index in 0..<count {
            var phase = (Double(index) / Double(count)
                         + Double(tickCount) * speed).truncatingRemainder(dividingBy: 1)
            if !forward { phase = 1 - phase }
            let x = lane.minX + CGFloat(phase) * max(0, lane.width - bitSize)
            color.withAlphaComponent(dark ? 0.95 : 0.82).setFill()
            NSRect(x: x, y: lane.midY - bitSize / 2,
                   width: bitSize, height: bitSize).fill()
        }
    }

    private func readSample(previous: Sample, readNetwork: Bool,
                            readGPU: Bool, readDisk: Bool) -> Sample {
        var s = previous
        let memory = memoryUse()
        s.ram = memory.fraction
        s.ramUsedGB = memory.usedGB
        s.ramTotalGB = memory.totalGB
        if readDisk {
            let disk = diskUse()
            s.ssd = disk.fraction
            s.ssdUsedGB = disk.usedGB
            s.ssdFreeGB = disk.freeGB
            s.ssdTotalGB = disk.totalGB
        }
        if readGPU {
            let gpu = gpuUse()
            s.gpu = gpu.fraction
            s.gpuMemoryGB = gpu.memoryGB
        }
        s.cpu = cpuUse()
        var loads = [Double](repeating: 0, count: 3)
        if getloadavg(&loads, 3) > 0 { s.load = loads[0] }
        if readNetwork {
            let network = networkUse()
            // A brief glide suppresses packet-sized flashes while remaining
            // responsive to a sustained transfer. Network samples arrive at
            // 2 Hz, so 0.35 settles in a little over one second.
            let glide = 0.35
            s.down = previous.down + (network.down - previous.down) * glide
            s.up = previous.up + (network.up - previous.up) * glide
            // A log scale keeps ordinary traffic visible while tolerating
            // bursts; 100 MB/s reaches the top of the row.
            let rawNet = min(1, log10(1 + network.down + network.up) / log10(101))
            s.net = previous.net + (rawNet - previous.net) * glide
        }
        return s
    }

    private func fleetTargets() -> [(name: String, ip: String)] {
        let wanted = ["poorslice", "blueberry", "neo", "chicken", "panda"]
        let local = ProcessInfo.processInfo.hostName
            .lowercased().replacingOccurrences(of: ".local", with: "")
        let fallback = [
            "poorslice": "100.86.206.3",
            "blueberry": "100.79.75.53",
            "neo": "100.108.5.81",
            "chicken": "100.98.158.126",
            "panda": "100.88.155.94",
        ]
        let path = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("aesthetic-computer-vault/machines.normalized.json")
        guard let data = try? Data(contentsOf: path),
              let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else {
            return wanted.filter { $0 != local }.compactMap { name in
                fallback[name].map { (name, $0) }
            }
        }
        let machineList: [[String: Any]]
        if let array = root["machines"] as? [[String: Any]] {
            machineList = array
        } else if let dictionary = root["machines"] as? [String: Any] {
            machineList = dictionary.compactMap { key, value in
                guard var machine = value as? [String: Any] else { return nil }
                machine["name"] = machine["name"] ?? key
                return machine
            }
        } else {
            machineList = []
        }
        return wanted.filter { $0 != local }.compactMap { name in
            let machine = machineList.first {
                ($0["name"] as? String)?.lowercased() == name
            }
            let tailscale = machine?["tailscale"] as? [String: Any]
            let ip = tailscale?["ip"] as? String ?? fallback[name]
            return ip.map { (name, $0) }
        }
    }

    private func refreshFleetWorkers(force: Bool = false) {
        guard !fleetRefreshInFlight else { return }
        guard force || Date().timeIntervalSince(fleetRefreshedAt) >= 10 else { return }
        fleetRefreshInFlight = true
        let targets = fleetTargets()
        if fleetWorkers.isEmpty {
            fleetWorkers = targets.map { FleetWorker(name: $0.name, online: false, cpu: 0, ram: 0,
                disk: 0, memoryGB: 0, cores: 0, pressure: false, reason: "", accepting: false,
                active: 0, queued: 0, role: "") }
        }
        refreshHoverCardText()
        let configuration = URLSessionConfiguration.ephemeral
        configuration.timeoutIntervalForRequest = 1.6
        configuration.timeoutIntervalForResource = 2.0
        let session = URLSession(configuration: configuration)
        let group = DispatchGroup()
        let lock = NSLock()
        var results: [String: FleetWorker] = [:]
        for target in targets {
            group.enter()
            let url = URL(string: "http://\(target.ip):5263/health")!
            session.dataTask(with: url) { data, response, _ in
                defer { group.leave() }
                guard let http = response as? HTTPURLResponse, http.statusCode == 200,
                      let data,
                      let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
                else { return }
                let active = (object["active"] as? [Any])?.count ?? 0
                let worker = FleetWorker(
                    name: target.name, online: true,
                    cpu: (object["cpuPct"] as? NSNumber)?.doubleValue ?? 0,
                    ram: (object["memoryUsedPct"] as? NSNumber)?.doubleValue ?? 0,
                    disk: (object["diskUsedPct"] as? NSNumber)?.doubleValue ?? 0,
                    memoryGB: (object["memoryTotalGB"] as? NSNumber)?.doubleValue ?? 0,
                    cores: (object["cores"] as? NSNumber)?.intValue ?? 0,
                    pressure: object["pressure"] as? Bool ?? false,
                    reason: object["pressureReason"] as? String ?? "",
                    accepting: object["accepting"] as? Bool ?? false,
                    active: active,
                    queued: (object["queued"] as? NSNumber)?.intValue ?? 0,
                    role: object["role"] as? String ?? "")
                lock.lock(); results[target.name] = worker; lock.unlock()
            }.resume()
        }
        group.notify(queue: .main) { [weak self] in
            guard let self else { return }
            session.invalidateAndCancel()
            self.fleetWorkers = targets.map { target in
                results[target.name] ?? FleetWorker(name: target.name, online: false, cpu: 0,
                    ram: 0, disk: 0, memoryGB: 0, cores: 0, pressure: false, reason: "",
                    accepting: false, active: 0, queued: 0, role: "")
            }
            self.fleetRefreshInFlight = false
            self.fleetRefreshedAt = Date()
            self.refreshHoverCardText()
        }
    }

    private func installHoverTracker(on button: NSStatusBarButton?) {
        guard let button else { return }
        let tracker = ResourceHoverTracker(frame: button.bounds)
        tracker.autoresizingMask = [.width, .height]
        tracker.onEnter = { [weak self] in self?.showHoverCard() }
        tracker.onMove = { [weak self, weak tracker] point in
            guard let self, let tracker else { return }
            self.highlightMetric(at: point.x, width: tracker.bounds.width)
        }
        tracker.onExit = { [weak self] in
            guard let self else { return }
            self.highlightedMetric = nil
            self.redraw()
            if !self.hoverPinned { self.hideHoverCard() }
        }
        button.addSubview(tracker, positioned: .above, relativeTo: nil)
        hoverTracker = tracker
        // Status-item buttons consume tracking differently across macOS
        // releases, especially when they also own a menu. A mouse-move monitor
        // against the button's real screen rect makes hover deterministic
        // without polling and without making the status item intercept clicks.
        hoverGlobalMonitor = NSEvent.addGlobalMonitorForEvents(matching: .mouseMoved) { [weak self] _ in
            DispatchQueue.main.async { self?.syncHoverToPointer() }
        }
        hoverLocalMonitor = NSEvent.addLocalMonitorForEvents(matching: .mouseMoved) { [weak self] event in
            self?.syncHoverToPointer()
            return event
        }
    }

    private func syncHoverToPointer() {
        guard let button = item?.button, let window = button.window else { return }
        let windowRect = button.convert(button.bounds, to: nil)
        let screenRect = window.convertToScreen(windowRect)
        let pointer = NSEvent.mouseLocation
        if screenRect.insetBy(dx: -1, dy: -2).contains(pointer) {
            highlightMetric(at: pointer.x - screenRect.minX, width: screenRect.width)
            showHoverCard()
        } else if !hoverPinned && hoverPanel?.isVisible == true {
            highlightedMetric = nil
            redraw()
            hideHoverCard()
        }
    }

    private func highlightMetric(at x: CGFloat, width: CGFloat) {
        let metrics = visibleMetrics
        guard width > 0, !metrics.isEmpty else { return }
        let index = max(0, min(metrics.count - 1,
            Int((max(0, min(width - 0.001, x)) / width) * CGFloat(metrics.count))))
        let metric = metrics[index]
        guard metric != highlightedMetric else { return }
        highlightedMetric = metric
        fleetStripView.highlightedMetricIndex = Metric.allCases.firstIndex(of: metric)
        redraw()
    }

    @objc private func toggleHoverBoard(_ sender: Any?) {
        hoverPinned.toggle()
        if hoverPinned {
            showHoverCard()
        } else {
            syncHoverToPointer()
            if let button = item?.button, let window = button.window {
                let rect = window.convertToScreen(button.convert(button.bounds, to: nil))
                if !rect.insetBy(dx: -1, dy: -2).contains(NSEvent.mouseLocation) {
                    hideHoverCard()
                }
            }
        }
    }

    private func showHoverCard() {
        guard hoverPanel?.isVisible != true, let button = item?.button,
              let window = button.window else { return }
        let windowRect = button.convert(button.bounds, to: nil)
        let statusRect = window.convertToScreen(windowRect)
        let rowCount = max(1, fleetWorkers.isEmpty ? fleetTargets().count : fleetWorkers.count)
        let labelWidth: CGFloat = 64
        let padding: CGFloat = 4
        let size = NSSize(width: statusRect.width + labelWidth + padding * 2,
                          height: CGFloat(rowCount * 20 + max(0, rowCount - 1) * 2 + 8))
        let screen = window.screen ?? NSScreen.main
        var origin = NSPoint(x: statusRect.minX - labelWidth - padding,
                             y: statusRect.minY - size.height - 4)
        if let visible = screen?.visibleFrame {
            origin.x = min(max(origin.x, visible.minX + 2), visible.maxX - size.width - 2)
        }
        let panel = NSPanel(contentRect: NSRect(origin: origin, size: size),
                            styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        panel.isOpaque = true
        panel.backgroundColor = .windowBackgroundColor
        panel.hasShadow = false
        panel.level = .statusBar
        panel.isFloatingPanel = true
        panel.hidesOnDeactivate = false
        panel.becomesKeyOnlyIfNeeded = true
        panel.ignoresMouseEvents = true
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
        fleetStripView.frame = NSRect(origin: .zero, size: size)
        fleetStripView.autoresizingMask = [.width, .height]
        fleetStripView.workers = fleetWorkers
        fleetStripView.highlightedMetricIndex = highlightedMetric.flatMap {
            Metric.allCases.firstIndex(of: $0)
        }
        panel.contentView = fleetStripView
        hoverPanel = panel
        panel.orderFrontRegardless()
        refreshFleetWorkers(force: true)
    }

    private func hideHoverCard() {
        hoverPanel?.orderOut(nil)
        hoverPanel = nil
    }

    private func refreshHoverCard() {
        guard hoverPanel?.isVisible == true else { return }
        refreshFleetWorkers()
        refreshHoverCardText()
    }

    private func refreshHoverCardText() {
        guard hoverPanel?.isVisible == true else { return }
        fleetStripView.workers = fleetWorkers
        fleetStripView.highlightedMetricIndex = highlightedMetric.flatMap {
            Metric.allCases.firstIndex(of: $0)
        }
    }

    private func memoryUse() -> (fraction: Double, usedGB: Double, totalGB: Double) {
        var stats = vm_statistics64()
        var count = mach_msg_type_number_t(MemoryLayout<vm_statistics64_data_t>.size / MemoryLayout<integer_t>.size)
        let result = withUnsafeMutablePointer(to: &stats) { p in
            p.withMemoryRebound(to: integer_t.self, capacity: Int(count)) {
                host_statistics64(mach_host_self(), HOST_VM_INFO64, $0, &count)
            }
        }
        guard result == KERN_SUCCESS else { return (0, 0, 0) }
        let pageSize = Double(vm_kernel_page_size)
        let usedPages = Double(stats.active_count + stats.inactive_count + stats.wire_count + stats.compressor_page_count)
        let totalPages = usedPages + Double(stats.free_count + stats.speculative_count)
        let gb = 1_073_741_824.0
        return (totalPages > 0 ? usedPages / totalPages : 0,
                usedPages * pageSize / gb, totalPages * pageSize / gb)
    }

    private func diskUse() -> (fraction: Double, usedGB: Double, freeGB: Double, totalGB: Double) {
        var fs = statfs()
        guard statfs("/", &fs) == 0, fs.f_blocks > 0 else { return (0, 0, 0, 0) }
        let total = Double(fs.f_blocks) * Double(fs.f_bsize)
        let free = Double(fs.f_bavail) * Double(fs.f_bsize)
        let used = total - free
        let gb = 1_073_741_824.0
        return (used / total, used / gb, free / gb, total / gb)
    }

    private func cpuUse() -> Double {
        var info = host_cpu_load_info()
        var count = mach_msg_type_number_t(MemoryLayout<host_cpu_load_info_data_t>.size /
                                            MemoryLayout<integer_t>.size)
        let result = withUnsafeMutablePointer(to: &info) { p in
            p.withMemoryRebound(to: integer_t.self, capacity: Int(count)) {
                host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO, $0, &count)
            }
        }
        guard result == KERN_SUCCESS else { return 0 }
        let idle = UInt64(info.cpu_ticks.2)
        let total = UInt64(info.cpu_ticks.0) + UInt64(info.cpu_ticks.1) + idle + UInt64(info.cpu_ticks.3)
        defer { previousCPU = (idle, total) }
        guard let old = previousCPU, total > old.total else { return 0 }
        return 1 - Double(idle - old.idle) / Double(total - old.total)
    }

    private func networkUse() -> (down: Double, up: Double) {
        var addresses: UnsafeMutablePointer<ifaddrs>?
        guard getifaddrs(&addresses) == 0, let first = addresses else { return (0, 0) }
        defer { freeifaddrs(addresses) }
        var down: UInt64 = 0, up: UInt64 = 0
        var p: UnsafeMutablePointer<ifaddrs>? = first
        while let current = p {
            let flags = Int32(current.pointee.ifa_flags)
            if flags & IFF_UP != 0, flags & IFF_LOOPBACK == 0,
               let data = current.pointee.ifa_data?.assumingMemoryBound(to: if_data.self) {
                down += UInt64(data.pointee.ifi_ibytes)
                up += UInt64(data.pointee.ifi_obytes)
            }
            p = current.pointee.ifa_next
        }
        let now = Date.timeIntervalSinceReferenceDate
        defer { previousNetwork = (down, up, now) }
        guard let old = previousNetwork, now > old.at else { return (0, 0) }
        let elapsed = now - old.at
        let downDelta = down >= old.down ? down - old.down : 0
        let upDelta = up >= old.up ? up - old.up : 0
        return (Double(downDelta) / elapsed / 1_048_576,
                Double(upDelta) / elapsed / 1_048_576)
    }

    private func gpuUse() -> (fraction: Double, memoryGB: Double) {
        var iterator: io_iterator_t = 0
        guard IOServiceGetMatchingServices(kIOMasterPortDefault,
                                           IOServiceMatching("IOAccelerator"),
                                           &iterator) == KERN_SUCCESS else { return (0, 0) }
        defer { IOObjectRelease(iterator) }
        var best = 0.0
        var memoryBytes = 0.0
        var allocatedBytes = 0.0
        while true {
            let service = IOIteratorNext(iterator)
            if service == 0 { break }
            defer { IOObjectRelease(service) }
            guard let raw = IORegistryEntryCreateCFProperty(service, "PerformanceStatistics" as CFString, kCFAllocatorDefault, 0)?.takeRetainedValue() as? [String: Any] else { continue }
            for key in ["Device Utilization %", "GPU Core Utilization", "GPU Activity(%)"] {
                if let n = raw[key] as? NSNumber { best = max(best, n.doubleValue / 100) }
            }
            // Apple Silicon reports shared GPU memory through IOAccelerator.
            // Prefer pages actively in use; allocation is a useful fallback on
            // older drivers that omit the in-use counters.
            for key in ["In use system memory", "In use system memory (driver)",
                        "vramUsedBytes", "VRAM Used Bytes", "gartUsedBytes"] {
                if let n = raw[key] as? NSNumber {
                    memoryBytes = max(memoryBytes, n.doubleValue)
                }
            }
            for key in ["Alloc system memory", "VRAM Allocated Bytes"] {
                if let n = raw[key] as? NSNumber {
                    allocatedBytes = max(allocatedBytes, n.doubleValue)
                }
            }
        }
        let reportedBytes = memoryBytes > 0 ? memoryBytes : allocatedBytes
        return (min(1, best), reportedBytes / 1_073_741_824.0)
    }
}
