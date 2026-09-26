#if os(macOS)
import AppKit
import SwiftUI

/// A bounded status export inside our own sandbox. Slab imports it; the app
/// neither writes into Slab's home nor needs Slab installed to function.
@MainActor
final class AeselProx {
    weak var anchor: NSView?
    private let session: Session
    private let instance = UUID().uuidString
    private let started = Date()
    private var timer: Timer?
    private let directory: URL

    init(session: Session) {
        self.session = session
        directory = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("computer.aesthetic.aesel.native/slab/windows")
    }

    func start() {
        guard timer == nil else { return }
        let timer = Timer(timeInterval: 2, repeats: true) { [weak self] _ in
            Task { @MainActor in self?.publish() }
        }
        self.timer = timer
        RunLoop.main.add(timer, forMode: .common)
        publish()
    }

    static func state(for session: Session) -> String {
        if session.approval != nil { return "awaiting" }
        if session.fatal != nil || session.health == .failed { return "interrupted" }
        if session.busy { return "working" }
        return session.entries.isEmpty && session.source.isEmpty ? "blank" : "complete"
    }

    func publish() {
        guard let anchor, let window = anchor.window, window.windowNumber > 0 else {
            try? FileManager.default.removeItem(at: directory.appendingPathComponent(instance + ".json"))
            return
        }
        let rect = anchor.convert(anchor.bounds, to: nil)
        let size = min(rect.width, rect.height)
        let pid = ProcessInfo.processInfo.processIdentifier
        // A rock follows this live window across thread switches. No prompt,
        // source, transcript, credentials, or provider payload is exported.
        let id = "aesel-native-" + instance
        let formatter = ISO8601DateFormatter()
        let piece = String(session.route.split(separator: "/").last ?? "")
        let marker: [String: Any] = [
            "session_id": id, "agent_type": "easel", "agent_pid": pid,
            "host_app": "computer.aesthetic.aesel.native", "host_pid": pid,
            "host_window_id": window.windowNumber, "tty": "", "cwd": "",
            "subject": session.route.isEmpty ? "aesel" : session.route,
            "summary": session.route.isEmpty ? "aesel" : session.route,
            "piece": piece, "piece_version": session.currentRevision,
            "state": Self.state(for: session), "updated": formatter.string(from: Date()),
            "started_at": formatter.string(from: started)
        ]
        let layout: [String: Any] = [
            "sessionId": id, "visible": window.isVisible && !anchor.isHiddenOrHasHiddenAncestor,
            "x": rect.minX, "y": window.frame.height - rect.maxY, "size": size,
            "windowWidth": window.frame.width, "windowHeight": window.frame.height
        ]
        do {
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true,
                                                    attributes: [.posixPermissions: 0o700])
            let data = try JSONSerialization.data(withJSONObject: ["schema": 1, "marker": marker, "layout": layout])
            let file = directory.appendingPathComponent(instance + ".json")
            try data.write(to: file, options: .atomic)
            try FileManager.default.setAttributes([.posixPermissions: 0o600], ofItemAtPath: file.path)
        } catch { NSLog("Aesel prox status unavailable: %@", error.localizedDescription) }
    }
}

/// Reserve real title-strip space and report its AppKit window coordinates.
struct AeselProxAnchor: NSViewRepresentable {
    let prox: AeselProx
    func makeNSView(context: Context) -> NSView {
        let view = NSView()
        prox.anchor = view
        prox.start()
        return view
    }
    func updateNSView(_ view: NSView, context: Context) { prox.anchor = view }
}
#endif
