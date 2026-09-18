import SwiftUI

@main
struct AeselApp: App {
    @State private var session = Session()
    @State private var host: SessionHost
    @State private var started = false
    @Environment(\.scenePhase) private var scenePhase

    init() {
        let session = Session()
        _session = State(initialValue: session)
        _host = State(initialValue: SessionHost(session: session, store: SessionStore()))
    }

    var body: some Scene {
        WindowGroup {
            ContentView(session: session, host: host)
                .preferredColorScheme(.dark)
                .onChange(of: scenePhase) { _, phase in
                    if phase != .active { host.save() }
                }
                .task {
                    guard !started else { return }
                    started = true
                    host.start(hostURL: Self.hostURL)
                }
        }
    }

    /// Release sessions load immutable app resources. Developers can explicitly
    /// opt into a hosted session through the launch environment.
    static var hostURL: URL {
        if let text = ProcessInfo.processInfo.environment["AESEL_HOST"],
           let url = URL(string: text) {
            return url
        }
        return URL(string: "aesel-bundle://app/easel/phone/host.html")!
    }
}
