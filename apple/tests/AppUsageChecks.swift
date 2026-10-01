import Foundation

@main struct AppUsageChecks {
    @MainActor static func main() throws {
        let suite = "ac.usage.test." + UUID().uuidString
        let defaults = UserDefaults(suiteName: suite)!
        defer { defaults.removePersistentDomain(forName: suite) }
        let time = Date(timeIntervalSince1970: 1_790_856_000)
        let windowA = UUID(), windowB = UUID()
        var scenes = AppUsageScenes()
        scenes.phases[windowA] = .active; scenes.phases[windowB] = .active
        scenes.phases[windowA] = .background
        precondition(scenes.phase == .active, "One background window must not pause another")
        scenes.phases[windowB] = .inactive; precondition(scenes.phase == .inactive)
        scenes.phases.removeValue(forKey: windowB); precondition(scenes.phase == .background)
        let meter = AppUsageMeter(defaults: defaults, version: "1.2", build: "5", platform: "ios")
        meter.activate(now: time, uptime: 100)
        let first = meter.current!
        precondition(first.firstObserved && first.activeSeconds == 0)
        meter.activate(now: time, uptime: 101)
        precondition(meter.current!.session == first.session, "Duplicate active notifications must not create opens")
        meter.loaded(true); meter.interact()
        meter.pause(background: false, now: time, uptime: 112)
        precondition(meter.current!.activeSeconds == 12)
        meter.acknowledge(first)
        precondition(meter.pending.count == 1, "Old acknowledgements must retain newer data")
        meter.activate(now: time, uptime: 200)
        precondition(meter.current!.session == first.session, "Control Center must not create an open")
        meter.pause(background: true, now: time, uptime: 208)
        precondition(meter.current!.activeSeconds == 20, "Inactive time must not count")
        meter.activate(now: time.addingTimeInterval(200), uptime: 300)
        precondition(meter.current!.session != first.session && !meter.current!.firstObserved)
        precondition(meter.current!.install == first.install && !meter.current!.interacted && meter.current!.ready)
        precondition(meter.pending.count == 2)
        let reopened = AppUsageMeter(defaults: defaults, version: "1.2", build: "5", platform: "ios")
        precondition(reopened.pending == meter.pending, "Offline snapshots must survive process death")
        reopened.activate(now: time.addingTimeInterval(300), uptime: 400)
        precondition(reopened.current!.install == first.install && !reopened.current!.firstObserved)
        precondition(reopened.pending.count == 3)
        reopened.acknowledge(reopened.pending[0]); precondition(reopened.pending.count == 2)
        reopened.checkpoint(now: time.addingTimeInterval(8 * 86400), uptime: 400)
        precondition(reopened.pending.isEmpty, "Expired offline snapshots must be dropped")
        meter.discard(); precondition(meter.pending.isEmpty && meter.current == nil)
        let object = try JSONSerialization.jsonObject(with: JSONEncoder().encode(first)) as! [String: Any]
        precondition(Set(object.keys) == Set(["schema", "app", "version", "build", "platform", "install", "session", "startedAt", "firstObserved", "activeSeconds", "ready", "interacted"]))
        print("App usage lifecycle, retry, persistence, expiry and payload checks passed")
    }
}
