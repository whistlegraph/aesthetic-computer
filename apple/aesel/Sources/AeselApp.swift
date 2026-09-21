import SwiftUI

@main
struct AeselApp: App {
    @State private var session = Session()
    @State private var host: SessionHost
    @State private var started = false
    @AppStorage("aesel.uiScale") private var uiScale = 1.0
    @Environment(\.scenePhase) private var scenePhase
    @Environment(\.displayScale) private var displayScale

    init() {
        ApplePlatform.registerFonts()
        let session = Session()
        _session = State(initialValue: session)
        _host = State(initialValue: SessionHost(session: session, store: SessionStore()))
    }

    var body: some Scene {
        #if os(macOS)
        Window("aesel", id: "workspace") {
            GeometryReader { geometry in
                workspace
                    .frame(width: ceil(geometry.size.width * displayScale) / displayScale / uiScale,
                           height: ceil(geometry.size.height * displayScale) / displayScale / uiScale)
                    .environment(\.aeselUIScale, uiScale)
                    .scaleEffect(uiScale, anchor: .topLeading)
            }.frame(minWidth: 220, minHeight: 160).clipped()
        }
        .windowToolbarStyle(.expanded)
        .defaultSize(width: 840, height: 680)
        .commands {
            CommandGroup(after: .toolbar) {
                Button("Larger UI") { uiScale = min(1.75, ((uiScale + 0.1) * 100).rounded() / 100) }
                    .keyboardShortcut("=", modifiers: .command).disabled(uiScale >= 1.75)
                Button("Larger UI (+)") { uiScale = min(1.75, ((uiScale + 0.1) * 100).rounded() / 100) }
                    .keyboardShortcut("+", modifiers: .command).disabled(uiScale >= 1.75)
                Button("Smaller UI") { uiScale = max(0.7, ((uiScale - 0.1) * 100).rounded() / 100) }
                    .keyboardShortcut("-", modifiers: .command).disabled(uiScale <= 0.7)
                Button("Actual UI Size") { uiScale = 1 }.keyboardShortcut("0", modifiers: .command)
            }
            CommandGroup(replacing: .newItem) {
                Button("New Piece") { host.newSession(medium: "piece") }
                    .keyboardShortcut("n")
                    .disabled(session.busy)
            }
        }
        #else
        WindowGroup { workspace }
        #endif
    }

    private var workspace: some View {
            ContentView(session: session, host: host)
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
                        session.previewURL = nil
                        if ProcessInfo.processInfo.environment["AESEL_PREVIEW_PIECE"] == "1" {
                            session.previewURL = Session.draftPreviewURL
                            session.source = "export function paint({wipe}) { wipe(\"orange\"); }"
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
