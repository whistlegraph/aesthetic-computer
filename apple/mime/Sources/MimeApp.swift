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
    @State private var creationStarted = false
    @State private var creating = false
    @State private var requested: Ware?
    var body: some View {
        ZStack {
            if creationStarted {
                MimeCreation(requested: $requested, active: $creating)
                    .opacity(creating ? 1 : 0).allowsHitTesting(creating).accessibilityHidden(!creating).zIndex(creating ? 1 : 0)
            }
            Reel(active: !creating) { ware in
                requested = ware; creationStarted = true; creating = true
            }.opacity(creating ? 0 : 1).allowsHitTesting(!creating).accessibilityHidden(creating).zIndex(creating ? 0 : 1)
        }.preferredColorScheme(.dark)
    }
}
