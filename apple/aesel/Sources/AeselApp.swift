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
                    else if started { host.refreshCredits() }
                }
                .task {
                    guard !started else { return }
                    started = true
                    #if DEBUG
                    if ProcessInfo.processInfo.environment["AESEL_NOTEBOOK_PREVIEW"] == "1" {
                        session.signedIn = true
                        session.handle = "jeffrey"
                        session.route = "@jeffrey/notebook"
                        session.shareURL = URL(string: "https://aesthetic.computer/blank")
                        session.model = "openai/gpt-5.6-luna"
                        session.status = "ready"
                        session.braincells = 482000
                        session.braincellDollars = 2.41
                        session.freeDollars = 1
                        session.purchasedDollars = 1.41
                        session.append(.you, "Make a little orange circle.")
                        session.append(.ac, "The **orange** circle follows your pointer.\n\nTry a radius of `24` or a blue background.\n\n```js\nconst radius = 24;\nwipe(\"blue\");\n```\n\n[Open Aesthetic Computer](https://aesthetic.computer)")
                        if ProcessInfo.processInfo.environment["AESEL_PREVIEW_PIECE"] == "1" {
                            session.previewURL = URL(string: "https://aesthetic.computer/blank")
                            session.source = "wipe(\"orange\");"
                        }
                        if ProcessInfo.processInfo.environment["AESEL_PREVIEW_BUSY"] == "1" {
                            session.append(.you, "Now make it bounce.")
                            session.busy = true
                            session.status = "writing"
                            session.health = .working
                        }
                        return
                    }
                    #endif
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
