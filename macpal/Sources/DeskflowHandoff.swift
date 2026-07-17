import AppKit

// Promotes this machine to Deskflow server when its physical trackpad is used.
// The fleet fan-out lives in deskflow-claim-control so MacPal stays a small UI
// observer and role changes remain testable from the command line.
#if !MAC_APP_STORE
final class DeskflowHandoff {
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
                    self.onControlAcquired?()
                    if let start = self.gestureStart, let latest = self.gestureLatest {
                        let dx = latest.x - start.x
                        let dy = latest.y - start.y
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.10) { [weak self] in
                            self?.replaySideBump(dx: dx, dy: dy)
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

    /// Preserve the horizontal intent made while the Deskflow cores were
    /// changing roles. Posting one HID mouse-move with the original direction
    /// lets Deskflow see the outward edge delta that would otherwise vanish.
    private func replaySideBump(dx: CGFloat, dy: CGFloat) {
        guard abs(dx) >= 0.018, abs(dx) > abs(dy) * 1.15,
              let position = CGEvent(source: nil)?.location,
              let event = CGEvent(mouseEventSource: nil,
                                  mouseType: .mouseMoved,
                                  mouseCursorPosition: position,
                                  mouseButton: .left) else { return }
        let delta: Int64 = dx < 0 ? -48 : 48
        event.setIntegerValueField(.mouseEventDeltaX, value: delta)
        event.setIntegerValueField(.mouseEventDeltaY, value: 0)
        event.post(tap: .cghidEventTap)
        NSLog("MacPal Deskflow handoff: replayed side bump dx=%lld", delta)
    }
}
#endif
