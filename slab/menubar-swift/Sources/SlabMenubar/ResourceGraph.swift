import AppKit
import Darwin
import IOKit

/// A Slab-owned replacement for the Stats menu extra. It reads like a stock
/// ticker: one legible metric/value plus its sparkline, rotating through all
/// five channels; hover/click exposes the complete snapshot.
final class ResourceGraph {
    static let shared = ResourceGraph()

    private struct Sample {
        var ram = 0.0, ssd = 0.0, gpu = 0.0, cpu = 0.0, net = 0.0
        var down = 0.0, up = 0.0
        var ramUsedGB = 0.0, ramTotalGB = 0.0
        var ssdUsedGB = 0.0, ssdFreeGB = 0.0, ssdTotalGB = 0.0
        var load = 0.0
    }

    private var item: NSStatusItem?
    private var timer: Timer?
    private var history: [Sample] = []
    private var previousCPU: (idle: UInt64, total: UInt64)?
    private var previousNetwork: (down: UInt64, up: UInt64, at: TimeInterval)?
    private var sample = Sample()
    private var tickCount = 0

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
    }

    private func start() {
        guard item == nil else { return }
        let status = NSStatusBar.system.statusItem(withLength: 92)
        status.button?.imagePosition = .imageOnly
        let menu = NSMenu()
        menu.autoenablesItems = false
        status.menu = menu
        item = status
        tick()
        let t = Timer(timeInterval: 2, repeats: true) { [weak self] _ in self?.tick() }
        timer = t
        RunLoop.main.add(t, forMode: .common)
    }

    private func tick() {
        DispatchQueue.global(qos: .utility).async { [weak self] in
            guard let self else { return }
            let fresh = self.readSample()
            DispatchQueue.main.async { [weak self] in
                guard let self, self.item != nil else { return }
                self.sample = fresh
                self.history.append(fresh)
                if self.history.count > 36 { self.history.removeFirst(self.history.count - 36) }
                self.tickCount += 1
                self.redraw()
            }
        }
    }

    private func redraw() {
        guard let button = item?.button else { return }
        button.image = render()
        button.contentTintColor = nil
        let text = String(format: "RAM %.0f%% · SSD %.0f%% · GPU %.0f%% · CPU %.0f%% · net ↓%.1f ↑%.1f MB/s",
                          sample.ram * 100, sample.ssd * 100, sample.gpu * 100,
                          sample.cpu * 100, sample.down, sample.up)
        button.toolTip = hoverDetails()
        let menu = item?.menu
        menu?.removeAllItems()
        let row = NSMenuItem(title: text, action: nil, keyEquivalent: "")
        row.isEnabled = false
        menu?.addItem(row)
        let legend = NSMenuItem(title: "RAM  SSD  GPU  CPU  NET", action: nil, keyEquivalent: "")
        legend.isEnabled = false
        menu?.addItem(legend)
    }

    private func render() -> NSImage {
        let size = NSSize(width: 90, height: 18)
        let image = NSImage(size: size)
        image.lockFocus()
        NSColor.clear.setFill()
        NSRect(origin: .zero, size: size).fill()
        let colors: [NSColor] = [.systemPink, .systemOrange, .systemPurple, .systemGreen,
                                NSColor(calibratedRed: 0.1, green: 0.75, blue: 0.9, alpha: 1)]
        let values: [(Sample) -> Double] = [{ $0.ram }, { $0.ssd }, { $0.gpu }, { $0.cpu }, { $0.net }]
        let names = ["RAM", "SSD", "GPU", "CPU", "NET"]
        // Hold each symbol for six seconds (three samples), long enough to
        // read without making the menubar feel static.
        let selected = (tickCount / 3) % names.count
        let color = colors[selected]
        let frame = NSBezierPath(roundedRect: NSRect(x: 0.5, y: 0.5, width: 89, height: 17),
                                 xRadius: 3, yRadius: 3)
        NSColor.labelColor.withAlphaComponent(0.28).setStroke()
        frame.lineWidth = 1
        frame.stroke()

        let current = values[selected](sample)
        let displayValue: String
        if selected == 4 {
            displayValue = rate(sample.down + sample.up).replacingOccurrences(of: "/s", with: "")
        } else {
            displayValue = String(format: "%.0f%%", current * 100)
        }
        let label = "\(names[selected]) \(displayValue)"
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: 9, weight: .semibold),
            .foregroundColor: NSColor.labelColor,
        ]
        label.draw(in: NSRect(x: 4, y: 4, width: 48, height: 11), withAttributes: attrs)

        let graph = NSRect(x: 53, y: 3, width: 32, height: 12)
        let points = history.suffix(32)
        if points.count > 1 {
            let line = NSBezierPath()
            for (offset, point) in points.enumerated() {
                let value = max(0, min(1, values[selected](point)))
                let p = NSPoint(x: graph.minX + CGFloat(offset),
                                y: graph.minY + CGFloat(value) * graph.height)
                offset == 0 ? line.move(to: p) : line.line(to: p)
            }
            color.setStroke()
            line.lineWidth = 1.5
            line.stroke()
        }
        // Five quote-board lamps make the aggregate nature visible even while
        // a single channel gets the readable ticker slot.
        for i in 0..<5 {
            (i == selected ? colors[i] : colors[i].withAlphaComponent(0.25)).setFill()
            NSRect(x: 54 + CGFloat(i * 6), y: 1, width: 4, height: 1).fill()
        }
        image.unlockFocus()
        image.isTemplate = false
        return image
    }

    private func readSample() -> Sample {
        var s = Sample()
        let memory = memoryUse()
        s.ram = memory.fraction
        s.ramUsedGB = memory.usedGB
        s.ramTotalGB = memory.totalGB
        let disk = diskUse()
        s.ssd = disk.fraction
        s.ssdUsedGB = disk.usedGB
        s.ssdFreeGB = disk.freeGB
        s.ssdTotalGB = disk.totalGB
        s.gpu = gpuUse()
        s.cpu = cpuUse()
        var loads = [Double](repeating: 0, count: 3)
        if getloadavg(&loads, 3) > 0 { s.load = loads[0] }
        let network = networkUse()
        s.down = network.down
        s.up = network.up
        // A log scale keeps ordinary traffic visible while tolerating bursts;
        // 100 MB/s reaches the top of the row.
        s.net = min(1, log10(1 + network.down + network.up) / log10(101))
        return s
    }

    private func hoverDetails() -> String {
        let freeRAM = max(0, sample.ramTotalGB - sample.ramUsedGB)
        return [
            String(format: "RAM    %.1f / %.1f GB used (%.1f GB free) · %.0f%%",
                   sample.ramUsedGB, sample.ramTotalGB, freeRAM, sample.ram * 100),
            String(format: "SSD    %.0f / %.0f GB used (%.0f GB free) · %.0f%%",
                   sample.ssdUsedGB, sample.ssdTotalGB, sample.ssdFreeGB, sample.ssd * 100),
            String(format: "GPU    %.0f%% utilization", sample.gpu * 100),
            String(format: "CPU    %.0f%% utilization · load %.2f · %d cores",
                   sample.cpu * 100, sample.load, ProcessInfo.processInfo.processorCount),
            "NET    ↓ \(rate(sample.down))  ↑ \(rate(sample.up))",
            "HISTORY    72 seconds · sampled every 2 seconds",
        ].joined(separator: "\n")
    }

    private func rate(_ mbPerSecond: Double) -> String {
        if mbPerSecond >= 1 { return String(format: "%.1f MB/s", mbPerSecond) }
        return String(format: "%.0f KB/s", mbPerSecond * 1024)
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

    private func gpuUse() -> Double {
        var iterator: io_iterator_t = 0
        guard IOServiceGetMatchingServices(kIOMasterPortDefault, IOServiceMatching("IOAccelerator"), &iterator) == KERN_SUCCESS else { return 0 }
        defer { IOObjectRelease(iterator) }
        var best = 0.0
        while true {
            let service = IOIteratorNext(iterator)
            if service == 0 { break }
            defer { IOObjectRelease(service) }
            guard let raw = IORegistryEntryCreateCFProperty(service, "PerformanceStatistics" as CFString, kCFAllocatorDefault, 0)?.takeRetainedValue() as? [String: Any] else { continue }
            for key in ["Device Utilization %", "GPU Core Utilization", "GPU Activity(%)"] {
                if let n = raw[key] as? NSNumber { best = max(best, n.doubleValue / 100) }
            }
        }
        return min(1, best)
    }
}
