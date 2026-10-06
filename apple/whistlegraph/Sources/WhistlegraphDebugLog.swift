import SwiftUI
import UIKit

struct WhistlegraphDebugLog: View {
    @State private var contents = ""
    @State private var failure: String?
    @State private var export: URL?
    @State private var confirmingClear = false
    var body: some View {
        List {
            Section {
                Text("Actions and errors stay on this phone. No words, passwords, tokens, artwork, or audio. Up to 1 MB of recent events is kept; nothing is uploaded automatically.")
                    .font(.footnote).foregroundStyle(.secondary)
                Button("Prepare log to share") {
                    DeviceActionLog.shared.record(.logExport, .requested)
                    do { export = try DeviceActionLog.shared.export(); failure = nil }
                    catch { failure = "Could not prepare the log. Try again." }
                }.accessibilityIdentifier("debug-log-export")
                if let export { ShareLink("Share log…", item: export) }
                Button("Refresh", action: refresh)
                Button("Clear log", role: .destructive) { confirmingClear = true }
            }
            if let failure { Text(failure).foregroundStyle(.red) }
            Section("Recent events") {
                Text(contents.isEmpty ? "No events yet." : contents)
                    .font(.system(.caption, design: .monospaced)).textSelection(.enabled)
                    .accessibilityIdentifier("debug-log-contents")
            }
        }
        .navigationTitle("Debug log")
        .task { DeviceActionLog.shared.record(.screen, .presented, control: .debugLog); refresh() }
        .onDisappear { DeviceActionLog.shared.record(.screen, .dismissed, control: .debugLog) }
        .confirmationDialog("Clear this phone’s debug log?", isPresented: $confirmingClear, titleVisibility: .visible) {
            Button("Clear log", role: .destructive) {
                do { try DeviceActionLog.shared.clear(); export = nil; refresh() }
                catch { failure = "Could not clear the log. Try again." }
            }
        }
    }
    private func refresh() {
        do {
            contents = try DeviceActionLog.shared.snapshot().split(separator: "\n").suffix(120).joined(separator: "\n")
            failure = DeviceActionLog.shared.hasWriteFailure ? "The phone could not save some events. Check available storage." : nil
        } catch { failure = "Could not read the log. Try again." }
    }
}

/// Observe native-window touches, including taps on disabled controls, without
/// recognizing or consuming a gesture. No coordinates or accessibility labels
/// are retained. Keyboard contents and the authentication window are excluded.
struct ActionTouchProbe: UIViewRepresentable {
    func makeUIView(context: Context) -> Probe { Probe() }
    func updateUIView(_ view: Probe, context: Context) {}
    @MainActor final class Probe: UIView {
        private let observer = TouchObserver()
        override func didMoveToWindow() {
            super.didMoveToWindow()
            observer.view?.removeGestureRecognizer(observer)
            window?.addGestureRecognizer(observer)
        }
    }
    @MainActor final class TouchObserver: UIGestureRecognizer {
        static var authenticationPresented = false
        private var started: TimeInterval = 0
        private var control: DeviceActionLog.Control?
        private var recording = false
        override init(target: Any?, action: Selector?) {
            super.init(target: target, action: action)
            cancelsTouchesInView = false; delaysTouchesBegan = false; delaysTouchesEnded = false
        }
        convenience init() { self.init(target: nil, action: nil) }
        required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }
        override func canPrevent(_ preventedGestureRecognizer: UIGestureRecognizer) -> Bool { false }
        override func canBePrevented(by preventingGestureRecognizer: UIGestureRecognizer) -> Bool { false }
        override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent) {
            guard !Self.authenticationPresented else { state = .failed; return }
            var view = touches.first?.view
            control = nil
            let controls: [String: DeviceActionLog.Control] = ["type-control": .type, "talk-control": .talk,
                "draw-control": .chalk, "drawing-pad": .drawingPad, "drawing-send": .send,
                "brain-settings": .brain, "workspace-account": .account, "workspace-settings": .pieces,
                "play-versions": .story, "project-tv": .tv, "typed-request": .requestText,
                "request-send": .send, "request-cancel": .cancel, "ai-consent-allow": .allow, "ai-consent-not-now": .decline]
            while let current = view {
                if current is UITextField || current is UITextView { state = .failed; return }
                if let identifier = current.accessibilityIdentifier, let known = controls[identifier] { control = known; break }
                view = current.superview
            }
            recording = true; started = ProcessInfo.processInfo.systemUptime
            DeviceActionLog.shared.record(.touch, .started, control: control, [.touches: touches.count])
        }
        override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent) { finish(.ended) }
        override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent) { finish(.cancelled) }
        private func finish(_ outcome: DeviceActionLog.Outcome) {
            if recording && !Self.authenticationPresented {
                DeviceActionLog.shared.record(.touch, outcome, control: control,
                    [.durationMs: Int((ProcessInfo.processInfo.systemUptime - started) * 1000)])
            }
            recording = false; state = .failed
        }
        override func reset() { super.reset(); recording = false; control = nil }
    }
}
