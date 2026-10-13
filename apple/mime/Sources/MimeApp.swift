import SwiftUI
import AVFoundation
import CoreText

/// mime.ac watches the network and shares Whistlegraph's native creation engine.
@main
struct MimeApp: App {
    init() {
        if let root = Bundle.main.url(forResource: "Web", withExtension: nil) {
            for name in ["ComicRelief-Regular.ttf", "ComicRelief-Bold.ttf"] { CTFontManagerRegisterFontsForURL(root.appendingPathComponent(name) as CFURL, .process, nil) }
        }
        // Pieces, KidLisp and videos sound like a reel: on, and mixed with nothing else.
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        try? AVAudioSession.sharedInstance().setActive(true)
    }
    var body: some Scene {
        WindowGroup { MimeRoot() }
    }
}

struct MimeRoot: View {
    @StateObject private var workspace = WhistlegraphSession()
    @State private var creating = false
    @State private var editorDrag: CGFloat = 0
    @State private var requested: Ware?
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        GeometryReader { geometry in
            ZStack {
                Reel(active: !creating, resumeEditor: {
                    requested = nil
                    creating = true
                }) { ware in
                    // A running turn owns the workspace. Reopen it rather than
                    // hiding it behind a pending New command until it finishes.
                    requested = workspace.snapshot.busy || workspace.capturePhase != .idle ? nil : ware
                    creating = true
                }
                    .offset(x: creating && !reduceMotion ? geometry.size.width + editorDrag : 0)
                    .allowsHitTesting(!creating).accessibilityHidden(creating)
                // Stay mounted so account, fonts and engine warm up while the feed is open.
                MimeCreation(session: workspace, requested: $requested, active: $creating, size: geometry.size, drag: $editorDrag)
                    .frame(width: geometry.size.width, height: geometry.size.height)
                    .offset(x: reduceMotion ? 0 : creating ? editorDrag : -geometry.size.width)
                    .opacity(creating ? 1 : 0)
                    .allowsHitTesting(creating).accessibilityHidden(!creating)
                #if DEBUG
                if ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] == "audio" {
                    Text(workspace.playbackTestResult).font(.caption).allowsHitTesting(false)
                        .accessibilityIdentifier("mime-preview-audio-state")
                }
                #endif
            }
            .animation(reduceMotion ? .easeOut(duration: 0.15) : .spring(response: 0.42, dampingFraction: 0.88), value: creating)
        }.preferredColorScheme(.dark)
    }
}
