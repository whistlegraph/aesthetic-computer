import Foundation
import Darwin

/// In-process system-resource sampler. Everything here is a Mach / sysctl
/// call — no subprocesses — so it works inside the sandboxed screensaver
/// host. Keep one instance alive across frames: CPU % is a delta between
/// successive `host_processor_info` reads.
final class SystemMetrics {
    struct Sample {
        var cpuBusy: Double = 0          // 0…1 aggregate
        var perCore: [Double] = []       // 0…1 per logical core
        var memUsed: UInt64 = 0          // bytes (active + wired + compressed)
        var memTotal: UInt64 = 0         // bytes
        var load: (Double, Double, Double) = (0, 0, 0)
        var uptime: TimeInterval = 0     // seconds since boot

        var memFraction: Double {
            memTotal == 0 ? 0 : min(1, Double(memUsed) / Double(memTotal))
        }
    }

    private var prevTicks: [(user: UInt32, sys: UInt32, idle: UInt32, nice: UInt32)] = []
    private let memTotal: UInt64 = {
        var size: UInt64 = 0
        var len = MemoryLayout<UInt64>.size
        sysctlbyname("hw.memsize", &size, &len, nil, 0)
        return size
    }()

    func sample() -> Sample {
        var s = Sample()
        s.memTotal = memTotal
        s.uptime = ProcessInfo.processInfo.systemUptime

        var loads = [Double](repeating: 0, count: 3)
        if getloadavg(&loads, 3) == 3 {
            s.load = (loads[0], loads[1], loads[2])
        }

        sampleMemory(into: &s)
        sampleCPU(into: &s)
        return s
    }

    private func sampleMemory(into s: inout Sample) {
        var stats = vm_statistics64()
        var count = mach_msg_type_number_t(
            MemoryLayout<vm_statistics64>.stride / MemoryLayout<integer_t>.stride)
        let kr = withUnsafeMutablePointer(to: &stats) {
            $0.withMemoryRebound(to: integer_t.self, capacity: Int(count)) {
                host_statistics64(mach_host_self(), HOST_VM_INFO64, $0, &count)
            }
        }
        guard kr == KERN_SUCCESS else { return }
        let page = UInt64(vm_kernel_page_size)
        let used = UInt64(stats.active_count)
            + UInt64(stats.wire_count)
            + UInt64(stats.compressor_page_count)
        s.memUsed = used * page
    }

    private func sampleCPU(into s: inout Sample) {
        var cpuInfo: processor_info_array_t?
        var numInfo: mach_msg_type_number_t = 0
        var numCPUs: natural_t = 0
        let kr = host_processor_info(
            mach_host_self(), PROCESSOR_CPU_LOAD_INFO, &numCPUs, &cpuInfo, &numInfo)
        guard kr == KERN_SUCCESS, let info = cpuInfo else { return }
        defer {
            vm_deallocate(mach_task_self_,
                          vm_address_t(bitPattern: info),
                          vm_size_t(numInfo) * vm_size_t(MemoryLayout<integer_t>.stride))
        }

        let n = Int(numCPUs)
        var cur: [(UInt32, UInt32, UInt32, UInt32)] = []
        cur.reserveCapacity(n)
        for i in 0..<n {
            let base = i * Int(CPU_STATE_MAX)
            let user = UInt32(bitPattern: info[base + Int(CPU_STATE_USER)])
            let sys  = UInt32(bitPattern: info[base + Int(CPU_STATE_SYSTEM)])
            let idle = UInt32(bitPattern: info[base + Int(CPU_STATE_IDLE)])
            let nice = UInt32(bitPattern: info[base + Int(CPU_STATE_NICE)])
            cur.append((user, sys, idle, nice))
        }

        guard prevTicks.count == n else {
            // First sample (or core count changed): no delta yet.
            prevTicks = cur.map { (user: $0.0, sys: $0.1, idle: $0.2, nice: $0.3) }
            s.perCore = Array(repeating: 0, count: n)
            return
        }

        var per: [Double] = []
        per.reserveCapacity(n)
        var busyAcc = 0.0, totalAcc = 0.0
        for i in 0..<n {
            let p = prevTicks[i]
            let du = Double(cur[i].0 &- p.user)
            let ds = Double(cur[i].1 &- p.sys)
            let di = Double(cur[i].2 &- p.idle)
            let dn = Double(cur[i].3 &- p.nice)
            let busy = du + ds + dn
            let total = busy + di
            per.append(total > 0 ? min(1, busy / total) : 0)
            busyAcc += busy
            totalAcc += total
        }
        prevTicks = cur.map { (user: $0.0, sys: $0.1, idle: $0.2, nice: $0.3) }
        s.perCore = per
        s.cpuBusy = totalAcc > 0 ? min(1, busyAcc / totalAcc) : 0
    }

    static func formatBytes(_ bytes: UInt64) -> String {
        let gb = Double(bytes) / 1_073_741_824.0
        return String(format: "%.1f", gb)
    }

    static func formatUptime(_ secs: TimeInterval) -> String {
        let t = Int(secs)
        let d = t / 86400, h = (t % 86400) / 3600, m = (t % 3600) / 60
        if d > 0 { return "\(d)d \(h)h" }
        if h > 0 { return "\(h)h \(m)m" }
        return "\(m)m"
    }
}
