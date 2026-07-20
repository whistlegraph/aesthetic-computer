import AppKit

// Promotes this machine to Deskflow server when its physical trackpad is used.
// The fleet fan-out lives in deskflow-claim-control so MacPal stays a small UI
// observer and role changes remain testable from the command line.
#if !MAC_APP_STORE
final class DeskflowHandoff {
    private static let unipointerIdentifier = "unipointer"
    private static let unipointerStateKind = "unipointer-state"

    private struct UnipointerState {
        let screen: String
        let normalizedX: CGFloat
        let normalizedY: CGFloat
        let x: CGFloat
        let y: CGFloat
        let frameX: CGFloat
        let frameY: CGFloat
        let frameWidth: CGFloat
        let frameHeight: CGFloat
    }

    var onControlAcquired: (() -> Void)?

    private let claimPath = NSString(string: "~/.local/bin/deskflow-claim-control").expandingTildeInPath
    private let statePath = NSString(string: "~/.config/slab/deskflow.json").expandingTildeInPath
    private var claimInFlight = false
    private var lastAttempt = Date.distantPast
    private var gestureStart: CGPoint?
    private var gestureLatest: CGPoint?

    init?() {
        guard FileManager.default.isExecutableFile(atPath: claimPath) else { return nil }
        PhysicalTrackpad.shared.onTouchBegan = { [weak self] point in self?.touchBegan(at: point) }
        PhysicalTrackpad.shared.onFrame = { [weak self] point in self?.capture(point) }
        guard PhysicalTrackpad.shared.start() else { return nil }
    }

    private func currentRole() -> String? {
        guard let data = FileManager.default.contents(atPath: statePath),
              let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return nil }
        return object["role"] as? String
    }

    private func touchBegan(at point: CGPoint) {
        gestureStart = point
        gestureLatest = point
        guard currentRole() != "server", !claimInFlight,
              Date().timeIntervalSince(lastAttempt) > 1.5 else { return }
        claimInFlight = true
        lastAttempt = Date()

        let process = Process()
        let output = Pipe()
        process.executableURL = URL(fileURLWithPath: claimPath)
        process.standardOutput = output
        process.standardError = output
        process.terminationHandler = { [weak self] process in
            let data = output.fileHandleForReading.readDataToEndOfFile()
            let message = String(data: data, encoding: .utf8)?
                .trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
            DispatchQueue.main.async {
                guard let self else { return }
                self.claimInFlight = false
                if process.terminationStatus == 0, self.currentRole() == "server" {
                    NSLog("MacPal Deskflow handoff: %@", message)
                    let preservedScreen = self.preservedScreen(in: message)
                    let unipointer = self.unipointerState(in: message)
                    let routeDelay = self.routeToPreservedScreen(preservedScreen)
                    let gesture = self.gestureStart.flatMap { start in
                        self.gestureLatest.map { latest in
                            (dx: latest.x - start.x, dy: latest.y - start.y)
                        }
                    }
                    DispatchQueue.main.asyncAfter(deadline: .now() + routeDelay + 0.02) { [weak self] in
                        guard let self else { return }
                        self.restoreUnipointer(unipointer) { [weak self] in
                            guard let self else { return }
                            if let gesture {
                                self.replayGesture(dx: gesture.dx, dy: gesture.dy)
                            }
                            // Position and motion are the critical path; the
                            // glow/sound may lazily load afterward.
                            DispatchQueue.main.asyncAfter(deadline: .now() + 0.02) { [weak self] in
                                self?.onControlAcquired?()
                            }
                        }
                    }
                } else {
                    NSLog("MacPal Deskflow handoff failed (%d): %@", process.terminationStatus, message)
                }
            }
        }
        do { try process.run() }
        catch {
            claimInFlight = false
            NSLog("MacPal Deskflow handoff could not start: %@", error.localizedDescription)
        }
    }

    private func capture(_ point: CGPoint?) {
        guard claimInFlight, let point else { return }
        gestureLatest = point
    }

    private func preservedScreen(in output: String) -> String? {
        output.split(separator: "\n").reversed().compactMap { line in
            let prefix = "active-screen "
            guard line.hasPrefix(prefix) else { return nil }
            return String(line.dropFirst(prefix.count))
        }.first
    }

    private func localScreenName() -> String? {
        let path = NSString(string: "~/.config/slab/deskflow-handoff.json").expandingTildeInPath
        guard let data = FileManager.default.contents(atPath: path),
              let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return nil }
        return object["screenName"] as? String
    }

    private func handoffConfig() -> [String: Any]? {
        let path = NSString(string: "~/.config/slab/deskflow-handoff.json").expandingTildeInPath
        guard let data = FileManager.default.contents(atPath: path) else { return nil }
        return try? JSONSerialization.jsonObject(with: data) as? [String: Any]
    }

    private func unipointerState(in output: String) -> UnipointerState? {
        let prefix = "unipointer-state "
        for line in output.split(separator: "\n").reversed() where line.hasPrefix(prefix) {
            let rest = line.dropFirst(prefix.count)
            guard let split = rest.firstIndex(of: " ") else { continue }
            let screen = String(rest[..<split])
            let json = Data(rest[rest.index(after: split)...].utf8)
            guard let object = try? JSONSerialization.jsonObject(with: json) as? [String: Any],
                  object["identifier"] as? String == Self.unipointerIdentifier,
                  object["kind"] as? String == Self.unipointerStateKind,
                  (object["version"] as? NSNumber)?.intValue == 1,
                  let x = object["x"] as? NSNumber,
                  let y = object["y"] as? NSNumber,
                  let normalized = object["normalized"] as? [String: Any],
                  let nx = normalized["x"] as? NSNumber,
                  let ny = normalized["y"] as? NSNumber,
                  let frame = object["frame"] as? [String: Any],
                  let fx = frame["x"] as? NSNumber,
                  let fy = frame["y"] as? NSNumber,
                  let width = frame["width"] as? NSNumber,
                  let height = frame["height"] as? NSNumber else { continue }
            return UnipointerState(
                screen: screen,
                normalizedX: CGFloat(nx.doubleValue), normalizedY: CGFloat(ny.doubleValue),
                x: CGFloat(x.doubleValue), y: CGFloat(y.doubleValue),
                frameX: CGFloat(fx.doubleValue), frameY: CGFloat(fy.doubleValue),
                frameWidth: CGFloat(width.doubleValue), frameHeight: CGFloat(height.doubleValue)
            )
        }
        return nil
    }

    private func peer(for screen: String) -> String? {
        guard screen != localScreenName(),
              let clients = handoffConfig()?["clients"] as? [String] else { return nil }
        let token = screen.replacingOccurrences(of: ".local", with: "").lowercased()
        return clients.first { $0.lowercased().contains(token) }
    }

    private func readUnipointer(on screen: String, completion: @escaping (UnipointerState?) -> Void) {
        let process = Process()
        let output = Pipe()
        if let peer = peer(for: screen) {
            process.executableURL = URL(fileURLWithPath: "/usr/bin/ssh")
            process.arguments = [
                "-o", "BatchMode=yes", "-o", "ConnectTimeout=2",
                "-o", "ControlMaster=auto", "-o", "ControlPersist=600",
                "-o", "ControlPath=\(NSHomeDirectory())/.ssh/deskflow-%C",
                peer, "~/.local/bin/unipointer",
            ]
        } else {
            process.executableURL = URL(fileURLWithPath: NSHomeDirectory() + "/.local/bin/unipointer")
        }
        process.standardOutput = output
        process.standardError = FileHandle.nullDevice
        DispatchQueue.global(qos: .userInteractive).async {
            do {
                try process.run()
                process.waitUntilExit()
                let data = output.fileHandleForReading.readDataToEndOfFile()
                let json = String(data: data, encoding: .utf8) ?? ""
                let state = self.unipointerState(in: "unipointer-state \(screen) \(json)")
                DispatchQueue.main.async { completion(state) }
            } catch {
                DispatchQueue.main.async { completion(nil) }
            }
        }
    }

    private func restoreUnipointer(_ saved: UnipointerState?, completion: @escaping () -> Void) {
        guard let saved else { completion(); return }
        readUnipointer(on: saved.screen) { [weak self] current in
            guard let self, let current else { completion(); return }
            let desiredX = current.frameX + saved.normalizedX * current.frameWidth
            let desiredY = current.frameY + saved.normalizedY * current.frameHeight
            let deltaX = desiredX - current.x
            let deltaY = -(desiredY - current.y) // CGEvent Y grows downward; AppKit grows upward.
            self.postMotion(dx: deltaX, dy: deltaY)
            NSLog("MacPal unipointer: restored %@ to %.3f,%.3f (delta %.1f,%.1f)",
                  saved.screen, saved.normalizedX, saved.normalizedY, deltaX, deltaY)
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.015, execute: completion)
        }
    }

    /// Re-enter the old active destination by crossing the same physical edges
    /// the hand would use. This avoids a synthetic-key translation layer and
    /// keeps the handoff behavior identical to ordinary Deskflow mousing.
    private func routeToPreservedScreen(_ screen: String?) -> TimeInterval {
        guard let local = localScreenName(), let screen, screen != local else { return 0 }
        let steps: [CGVector]
        switch (local, screen) {
        case ("neo", "chicken.local"): steps = [CGVector(dx: 0, dy: -4096)]
        case ("neo", "panda.local"):
            steps = [CGVector(dx: 0, dy: -4096), CGVector(dx: 4096, dy: 0)]
        case ("neo", "blueberry.local"): steps = [CGVector(dx: 4096, dy: 0)]
        case ("blueberry.local", "panda.local"): steps = [CGVector(dx: 0, dy: -4096)]
        case ("blueberry.local", "chicken.local"):
            steps = [CGVector(dx: 0, dy: -4096), CGVector(dx: -4096, dy: 0)]
        case ("blueberry.local", "neo"): steps = [CGVector(dx: -4096, dy: 0)]
        default: return 0
        }
        for (index, step) in steps.enumerated() {
            let post: () -> Void = { [weak self] in
                self?.postEdge(step)
            }
            if index == 0 { post() }
            else {
                DispatchQueue.main.asyncAfter(
                    deadline: .now() + Double(index) * 0.035, execute: post
                )
            }
        }
        NSLog("MacPal Deskflow handoff: routing %@ -> %@ in %d edge(s)",
              local, screen, steps.count)
        return Double(steps.count) * 0.035
    }

    private func postEdge(_ delta: CGVector) {
        postMotion(dx: delta.dx, dy: delta.dy)
    }

    private func postMotion(dx: CGFloat, dy: CGFloat) {
        guard let position = CGEvent(source: nil)?.location,
              let event = CGEvent(mouseEventSource: nil,
                                  mouseType: .mouseMoved,
                                  mouseCursorPosition: CGPoint(x: position.x + dx,
                                                               y: position.y + dy),
                                  mouseButton: .left) else { return }
        event.setIntegerValueField(.mouseEventDeltaX, value: Int64(dx.rounded()))
        event.setIntegerValueField(.mouseEventDeltaY, value: Int64(dy.rounded()))
        event.post(tap: .cghidEventTap)
    }

    /// Preserve the physical gesture made while the Deskflow cores were
    /// changing roles. Posting one HID move after restoring the active screen
    /// makes the first wiggle land on the same remote machine.
    private func replayGesture(dx: CGFloat, dy: CGFloat) {
        guard max(abs(dx), abs(dy)) >= 0.006,
              let position = CGEvent(source: nil)?.location,
              let event = CGEvent(mouseEventSource: nil,
                                  mouseType: .mouseMoved,
                                  mouseCursorPosition: position,
                                  mouseButton: .left) else { return }
        func scaled(_ value: CGFloat) -> Int64 {
            let raw = Int64((value * 1600).rounded())
            return max(-96, min(96, raw))
        }
        let deltaX = scaled(dx)
        let deltaY = scaled(-dy)
        event.setIntegerValueField(.mouseEventDeltaX, value: deltaX)
        event.setIntegerValueField(.mouseEventDeltaY, value: deltaY)
        event.post(tap: .cghidEventTap)
        NSLog("MacPal Deskflow handoff: replayed gesture dx=%lld dy=%lld", deltaX, deltaY)
    }
}
#endif
