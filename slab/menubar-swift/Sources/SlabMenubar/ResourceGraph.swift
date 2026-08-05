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

/// A Slab-owned replacement for the Stats menu extra. Five narrow coloured
/// bottom-to-top bars share one compact meter: CPU, RAM, network, SSD, and GPU.
/// Fast probes and presentation run at 4 Hz; slower IOKit/filesystem probes are
/// deliberately staggered so the fluid display stays inexpensive.
final class ResourceGraph: NSObject {
    static let shared = ResourceGraph()

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

    /// Hover expands the local meter into five readable history rows. It uses
    /// the samples already collected for the status item and performs no
    /// network or fleet polling.
    private final class LocalStatsView: NSView {
        var samples: [Sample] = [] { didSet { needsDisplay = true } }
        var current = Sample() { didSet { needsDisplay = true } }
        var highlightedMetric: Metric? { didSet { needsDisplay = true } }

        private var darkAppearance: Bool {
            effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        }

        override func draw(_ dirtyRect: NSRect) {
            super.draw(dirtyRect)
            let dark = darkAppearance
            let panel = NSBezierPath(roundedRect: bounds.insetBy(dx: 0.5, dy: 0.5),
                                     xRadius: 8, yRadius: 8)
            (dark
                ? NSColor(srgbRed: 0.025, green: 0.035, blue: 0.050, alpha: 1)
                : NSColor(srgbRed: 0.965, green: 0.975, blue: 0.98, alpha: 1)).setFill()
            panel.fill()
            (dark ? NSColor.white : NSColor.black).withAlphaComponent(0.22).setStroke()
            panel.lineWidth = 1
            panel.stroke()

            let content = bounds.insetBy(dx: 8, dy: 8)
            let gap: CGFloat = 4
            let rowHeight = (content.height - gap * CGFloat(Metric.allCases.count - 1)) /
                CGFloat(Metric.allCases.count)
            for (index, metric) in Metric.allCases.enumerated() {
                let y = content.maxY - CGFloat(index + 1) * rowHeight - CGFloat(index) * gap
                drawRow(metric, in: NSRect(x: content.minX, y: y,
                                           width: content.width, height: rowHeight), dark: dark)
            }
        }

        private func drawRow(_ metric: Metric, in rect: NSRect, dark: Bool) {
            let selected = metric == highlightedMetric
            let row = NSBezierPath(roundedRect: rect, xRadius: 4, yRadius: 4)
            metric.color.withAlphaComponent(selected ? 0.20 : (dark ? 0.09 : 0.12)).setFill()
            row.fill()
            metric.color.withAlphaComponent(selected ? 0.95 : 0.34).setStroke()
            row.lineWidth = selected ? 1.2 : 0.7
            row.stroke()

            let foreground = (dark ? NSColor.white : NSColor.black)
                .withAlphaComponent(selected ? 0.98 : 0.84)
            let name = NSAttributedString(string: metric.name, attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 10, weight: .heavy),
                .foregroundColor: metric.color,
                .kern: 0.2,
            ])
            name.draw(at: NSPoint(x: rect.minX + 8,
                                  y: rect.midY - name.size().height / 2))

            let value = NSAttributedString(string: detail(for: metric), attributes: [
                .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .semibold),
                .foregroundColor: foreground,
                .kern: -0.1,
            ])
            value.draw(at: NSPoint(x: rect.minX + 42,
                                   y: rect.midY - value.size().height / 2))

            let graph = NSRect(x: rect.minX + 145, y: rect.minY + 4,
                               width: rect.width - 151, height: rect.height - 8)
            drawHistory(metric, in: graph, dark: dark)
        }

        private func drawHistory(_ metric: Metric, in rect: NSRect, dark: Bool) {
            guard !samples.isEmpty, rect.width > 0, rect.height > 0 else { return }
            let points = Array(samples.suffix(60))
            let columnWidth = rect.width / CGFloat(points.count)
            for (index, sample) in points.enumerated() {
                let value = max(0, min(1, metric.value(sample)))
                let height = max(value > 0 ? 1 : 0, rect.height * CGFloat(value))
                let age = CGFloat(index + 1) / CGFloat(points.count)
                metric.color.withAlphaComponent((dark ? 0.42 : 0.50) + age * 0.46).setFill()
                NSRect(x: rect.minX + CGFloat(index) * columnWidth,
                       y: rect.minY,
                       width: max(1, columnWidth - 0.7), height: height).fill()
            }
        }

        private func detail(for metric: Metric) -> String {
            switch metric {
            case .cpu:
                return String(format: "%.0f%%  %.1f load", current.cpu * 100, current.load)
            case .ram:
                return String(format: "%.1f/%.0f GB", current.ramUsedGB, current.ramTotalGB)
            case .network:
                return "↓\(rate(current.down)) ↑\(rate(current.up))"
            case .ssd:
                return String(format: "%.0f/%.0f GB", current.ssdUsedGB, current.ssdTotalGB)
            case .gpu:
                if current.gpuMemoryGB > 0 {
                    return String(format: "%.0f%%  %.1f GB", current.gpu * 100, current.gpuMemoryGB)
                }
                return String(format: "%.0f%%", current.gpu * 100)
            }
        }

        private func rate(_ mbPerSecond: Double) -> String {
            if mbPerSecond >= 100 { return String(format: "%.0fM", mbPerSecond) }
            if mbPerSecond >= 10 { return String(format: "%.0fM", mbPerSecond) }
            if mbPerSecond >= 1 { return String(format: "%.1fM", mbPerSecond) }
            return String(format: "%.0fK", mbPerSecond * 1024)
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
    private var hoverTracker: ResourceHoverTracker?
    private var hoverPanel: NSPanel?
    private var hoverPinned = false
    private var highlightedMetric: Metric?
    private let localStatsView = LocalStatsView(frame: .zero)
    private var hoverGlobalMonitor: Any?
    private var hoverLocalMonitor: Any?

    private let sampleInterval: TimeInterval = 1.0 / 4.0
    private let barSectionBaseWidth: CGFloat = 13.5
    private let graphInset: CGFloat = 2
    private let graphGap: CGFloat = 1
    private let historyLimit = 60        // 15 seconds at 4 Hz.
    private let networkStride = 2        // Network reads at 2 Hz to calm short bursts.
    private let gpuStride = 8            // IOKit once per two seconds.
    private let diskStride = 40          // statfs once per ten seconds.

    /// MenuBand scales its 13.5pt semitone keys with this same factor.
    private var displayScale: CGFloat {
        let baseHeight: CGFloat = 22
        return max(1.0, min(1.6,
            (NSStatusBar.system.thickness - 0.5) / baseHeight))
    }

    private var barSectionWidth: CGFloat {
        barSectionBaseWidth * displayScale
    }

    private var meterWidth: CGFloat {
        let count = CGFloat(Metric.allCases.count)
        let graphWidth = graphInset * 2 + barSectionWidth * count
            + graphGap * CGFloat(max(0, Metric.allCases.count - 1))
        return graphWidth + 2
    }

    /// Match Menu Band's live status-bar sizing: start with its 22pt base
    /// height, then grow into roomier/notched menu bars without clipping.
    private var displayHeight: CGFloat {
        let baseHeight: CGFloat = 22
        return ceil(baseHeight * displayScale)
    }

    var enabled: Bool { FileManager.default.fileExists(atPath: Paths.resourceGraphFlag) }

    func syncEnabled() {
        let defaults = UserDefaults.standard
        if !defaults.bool(forKey: "resourceGraphConfigured") && !enabled {
            let fm = FileManager.default
            try? fm.createDirectory(
                atPath: (Paths.resourceGraphFlag as NSString).deletingLastPathComponent,
                withIntermediateDirectories: true)
            fm.createFile(atPath: Paths.resourceGraphFlag, contents: nil)
            defaults.set(true, forKey: "resourceGraphConfigured")
        }
        enabled ? start() : stop()
    }

    func toggle() {
        UserDefaults.standard.set(true, forKey: "resourceGraphConfigured")
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
        let status = NSStatusBar.system.statusItem(withLength: meterWidth)
        status.button?.imagePosition = .imageOnly
        status.button?.imageScaling = .scaleNone
        status.button?.target = self
        status.button?.action = #selector(toggleStatsPanel(_:))
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
                self.redraw()
            }
        }
    }

    private var visibleMetrics: [Metric] {
        Metric.allCases
    }

    private func redraw() {
        guard let button = item?.button else { return }
        button.image = render()
        button.contentTintColor = nil
        // The same local panel serves hover and click; there is deliberately no
        // second click-only menu or tooltip path.
        guard tickCount == 1 || tickCount % 4 == 0 else { return }
        button.toolTip = nil
        refreshHoverCard()
    }

    private func render() -> NSImage {
        let size = NSSize(width: meterWidth - 2, height: displayHeight)
        let image = NSImage(size: size)
        image.lockFocus()
        NSColor.clear.setFill()
        NSRect(origin: .zero, size: size).fill()

        // One five-channel instrument, framed like MenuBand's joined piano
        // keys: a softly rounded outer keycap, a shallow vertical face, and a
        // crisp groove around the whole graph rather than around each bar.
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let metrics = visibleMetrics
        // The image canvas already uses MenuBand's adaptive 22pt base-height
        // formula. Let the housing occupy that complete canvas; an earlier
        // extra 1pt top/bottom inset made it visibly shorter than the keys.
        let frameRect = NSRect(origin: .zero, size: size)
        let frame = NSBezierPath(roundedRect: frameRect.insetBy(dx: 0.35, dy: 0.35),
                                 xRadius: 2.2, yRadius: 2.2)
        let faceHi: NSColor
        let faceLo: NSColor
        if isDark {
            faceHi = NSColor(srgbRed: 58/255, green: 70/255, blue: 82/255, alpha: 1)
            faceLo = NSColor(srgbRed: 44/255, green: 54/255, blue: 62/255, alpha: 1)
        } else {
            faceHi = NSColor(srgbRed: 1, green: 1, blue: 1, alpha: 0.88)
            faceLo = NSColor(srgbRed: 238/255, green: 240/255, blue: 244/255, alpha: 0.88)
        }
        NSGradient(starting: faceHi, ending: faceLo)?.draw(in: frame, angle: -90)
        NSColor.black.withAlphaComponent(isDark ? 0.72 : 0.24).setStroke()
        frame.lineWidth = 0.7
        frame.stroke()

        let content = frameRect.insetBy(dx: graphInset, dy: graphInset)
        let cellWidth = barSectionWidth

        for (index, metric) in metrics.enumerated() {
            let stripe = NSRect(x: content.minX + CGFloat(index) * (cellWidth + graphGap),
                                y: content.minY,
                                width: cellWidth,
                                height: content.height)
            drawMetricStripe(metric, in: stripe, color: metric.color, dark: isDark)
        }
        if let highlightedMetric {
            drawHighlightedValue(highlightedMetric, in: content, dark: isDark)
        }

        image.unlockFocus()
        image.isTemplate = false
        return image
    }

    private func drawMetricStripe(_ metric: Metric, in rect: NSRect,
                                  color: NSColor, dark: Bool) {
        // A quiet full-height track gives the five values a shared scale. The
        // live value then rises linearly from the bottom; no easing or minimum
        // lift makes unlike values look artificially alike.
        color.withAlphaComponent(dark ? 0.045 : 0.07).setFill()
        rect.fill()
        let value = max(0, min(1, metric.value(sample)))
        guard value > 0.001 else { return }
        let pixel: CGFloat = 0.5
        let height = min(rect.height,
                         max(pixel, round(rect.height * CGFloat(value) / pixel) * pixel))
        color.withAlphaComponent(metric == highlightedMetric ? 1 : 0.90).setFill()
        NSRect(x: rect.minX, y: rect.minY,
               width: rect.width, height: height).fill()
    }

    /// The TVs are pure raster at rest. Pointer focus reveals one stable label
    /// and value without making the remaining instruments flicker or reflow.
    private func drawHighlightedValue(_ metric: Metric, in rect: NSRect, dark: Bool) {
        (dark
            ? NSColor(srgbRed: 0.015, green: 0.020, blue: 0.030, alpha: 0.94)
            : NSColor(srgbRed: 0.985, green: 0.99, blue: 0.995, alpha: 0.96)).setFill()
        rect.insetBy(dx: 0.5, dy: 0.5).fill()
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
        localStatsView.highlightedMetric = metric
        redraw()
    }

    @objc private func toggleStatsPanel(_ sender: Any?) {
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
        let size = NSSize(width: 320, height: 160)
        let screen = window.screen ?? NSScreen.main
        var origin = NSPoint(x: statusRect.maxX - size.width,
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
        localStatsView.frame = NSRect(origin: .zero, size: size)
        localStatsView.autoresizingMask = [.width, .height]
        localStatsView.samples = history
        localStatsView.current = sample
        localStatsView.highlightedMetric = highlightedMetric
        panel.contentView = localStatsView
        hoverPanel = panel
        panel.orderFrontRegardless()
    }

    private func hideHoverCard() {
        hoverPanel?.orderOut(nil)
        hoverPanel = nil
    }

    private func refreshHoverCard() {
        guard hoverPanel?.isVisible == true else { return }
        localStatsView.samples = history
        localStatsView.current = sample
        localStatsView.highlightedMetric = highlightedMetric
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
