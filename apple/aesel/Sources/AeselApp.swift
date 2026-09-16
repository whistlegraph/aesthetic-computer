import SwiftUI

@main
struct AeselApp: App {
    @State private var session = Session()
    @State private var host: SessionHost
    @State private var started = false

    init() {
        let session = Session()
        _session = State(initialValue: session)
        _host = State(initialValue: SessionHost(session: session, store: SessionStore()))
    }

    var body: some Scene {
        WindowGroup {
            ContentView(session: session, host: host)
                .preferredColorScheme(.dark)
                .task {
                    guard !started else { return }
                    started = true
                    host.start(hostURL: Self.hostURL)
                }
        }
    }

    /// Where the shared session is served from. While iterating this is the
    /// laptop running `node easel/phone/serve.mjs --token`, so a JS edit
    /// reaches the phone on reload with no rebuild. `AeselHostURL` in
    /// Info.plist is the knob; a shipped build points at a bundled copy.
    /// Order matters: the environment wins so `run.sh` can point a device at
    /// whatever address the laptop has today without editing a tracked file.
    /// A LAN address is not stable — this machine moved from a phone hotspot
    /// to a Wi-Fi network in the middle of building this, and a baked-in IP
    /// would have shipped broken.
    static var hostURL: URL {
        if let text = ProcessInfo.processInfo.environment["AESEL_HOST"],
           let url = URL(string: text) {
            return url
        }
        if let text = Bundle.main.object(forInfoDictionaryKey: "AeselHostURL") as? String,
           let url = URL(string: text) {
            return url
        }
        return URL(string: "http://localhost:8770/easel/phone/host.html")!
    }
}
