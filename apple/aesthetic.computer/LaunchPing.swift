// AC 1.2: cumulative snapshots of foreground sessions. No account, APNs token,
// URL or input text. First observed is not an App Store download.
import Foundation
#if canImport(UIKit)
import UIKit
#endif

struct AppUsageSample: Codable, Equatable {
    private(set) var schema = 1
    private(set) var app = "aestheticcomputer"
    let version: String
    let build: String
    let platform: String
    let install: String
    let session: String
    let startedAt: String
    let firstObserved: Bool
    var activeSeconds = 0
    var ready = false
    var interacted = false
}

enum AppUsagePhase { case active, inactive, background }
struct AppUsageScenes {
    var phases: [UUID: AppUsagePhase] = [:]
    var phase: AppUsagePhase {
        if phases.values.contains(.active) { return .active }
        if phases.values.contains(.inactive) { return .inactive }
        return .background
    }
}

// Foundation-only state machine: tests need not replace the App Store app.
@MainActor final class AppUsageMeter {
    private let defaults: UserDefaults
    private let version: String, build: String, platform: String
    private var activeSince: TimeInterval?
    private var seconds: TimeInterval = 0
    private var backgrounded = true
    private var runtimeReady = false
    private(set) var current: AppUsageSample?
    private(set) var pending: [AppUsageSample]

    init(defaults: UserDefaults, version: String, build: String, platform: String) {
        self.defaults = defaults; self.version = version; self.build = build; self.platform = platform
        pending = defaults.data(forKey: "acUsagePending")
            .flatMap { try? JSONDecoder().decode([AppUsageSample].self, from: $0) } ?? []
    }

    func activate(now: Date = Date(), uptime: TimeInterval = ProcessInfo.processInfo.systemUptime) {
        guard activeSince == nil else { return }
        if backgrounded || current == nil {
            let install = defaults.string(forKey: "acInstallID") ?? UUID().uuidString.lowercased()
            defaults.set(install, forKey: "acInstallID")
            let session = UUID().uuidString.lowercased()
            if defaults.string(forKey: "acUsageFirstSession") == nil {
                defaults.set(session, forKey: "acUsageFirstSession")
            }
            current = AppUsageSample(version: version, build: build, platform: platform,
                install: install, session: session, startedAt: ISO8601DateFormatter().string(from: now),
                firstObserved: defaults.string(forKey: "acUsageFirstSession") == session,
                ready: runtimeReady)
            seconds = 0
        }
        backgrounded = false; activeSince = uptime
        checkpoint(now: now, uptime: uptime)
    }

    func pause(background: Bool, now: Date = Date(), uptime: TimeInterval = ProcessInfo.processInfo.systemUptime) {
        checkpoint(now: now, uptime: uptime)
        activeSince = nil
        if background { backgrounded = true }
    }

    func loaded(_ ready: Bool) {
        runtimeReady = ready
        if ready { current?.ready = true }
    }

    func interact() {
        guard activeSince != nil else { return }
        current?.interacted = true
    }

    func checkpoint(now: Date = Date(), uptime: TimeInterval = ProcessInfo.processInfo.systemUptime) {
        if let since = activeSince {
            seconds = min(86400, seconds + max(0, uptime - since))
            activeSince = uptime
            current?.activeSeconds = Int(seconds)
        }
        if let sample = current {
            pending.removeAll { $0.session == sample.session }
            pending.append(sample)
        }
        let cutoff = now.addingTimeInterval(-7 * 86400), formatter = ISO8601DateFormatter()
        pending = Array(pending.filter { (formatter.date(from: $0.startedAt) ?? .distantPast) >= cutoff }.suffix(128))
        persist()
    }

    func acknowledge(_ sent: AppUsageSample) {
        // An older response must not discard a newer checkpoint.
        pending.removeAll { $0 == sent }; persist()
    }

    func discard() {
        pending.removeAll(); current = nil; activeSince = nil; seconds = 0; backgrounded = true; persist()
    }

    private func persist() {
        if let data = try? JSONEncoder().encode(pending) { defaults.set(data, forKey: "acUsagePending") }
    }
}

#if canImport(UIKit)
@MainActor enum LaunchPing {
    private static let defaults = UserDefaults.standard
    private static let meter = AppUsageMeter(defaults: defaults,
        version: Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String ?? "0",
        build: Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") as? String ?? "0",
        platform: UIDevice.current.userInterfaceIdiom == .pad ? "ipados" : "ios")
    private static var timer: Timer?
    private static var task: URLSessionDataTask?
    private static var requestID = UUID()
    private static var scenes = AppUsageScenes()
    private static let transport = URLSession(configuration: .ephemeral)
    private static var enabled: Bool {
        #if DEBUG
        return false
        #else
        return !defaults.bool(forKey: "acLaunchPingDisabled") &&
            (defaults.object(forKey: "acUsageEnabled") as? Bool ?? true)
        #endif
    }

    static func active(scene: UUID) {
        scenes.phases[scene] = .active
        refreshPhase()
    }

    static func inactive(scene: UUID, background: Bool) {
        scenes.phases[scene] = background ? .background : .inactive
        refreshPhase()
    }

    static func remove(scene: UUID) {
        scenes.phases.removeValue(forKey: scene)
        refreshPhase()
    }

    private static func refreshPhase() {
        guard enabled else { stop(); return }
        if scenes.phase == .active {
            meter.activate()
            if timer == nil {
                timer = Timer.scheduledTimer(withTimeInterval: 30, repeats: true) { _ in
                    Task { @MainActor in checkpoint() }
                }
            }
        } else {
            meter.pause(background: scenes.phase == .background)
            timer?.invalidate(); timer = nil
        }
        flush()
    }

    static func loaded(_ ready: Bool) {
        guard enabled else { return }
        meter.loaded(ready)
        if ready { checkpoint() }
    }

    static func interacted() {
        guard enabled, meter.current?.interacted != true else { return }
        meter.interact(); checkpoint()
    }

    private static func checkpoint() {
        guard enabled else { stop(); return }
        meter.checkpoint(); flush()
    }

    private static func stop() {
        timer?.invalidate(); timer = nil; requestID = UUID(); task?.cancel(); task = nil; meter.discard()
    }

    private static func flush() {
        guard enabled, task == nil, let sample = meter.pending.first,
              let body = try? JSONEncoder().encode(sample) else { return }
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/app-session")!, timeoutInterval: 10)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = body
        let sendingID = UUID(); requestID = sendingID
        task = transport.dataTask(with: request) { _, response, _ in
            let status = (response as? HTTPURLResponse)?.statusCode ?? 0
            Task { @MainActor in
                guard requestID == sendingID else { return }
                task = nil
                guard enabled else { meter.discard(); return }
                if (200..<300).contains(status) || status == 400 {
                    meter.acknowledge(sample); flush()
                }
                // Retry offline/429/5xx responses at the next checkpoint.
            }
        }
        task?.resume()
    }
}
#endif
