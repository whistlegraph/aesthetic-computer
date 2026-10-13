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
    @State private var creating = false
    @State private var cardDrag: CGFloat = 0
    @State private var requested: Ware?
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        GeometryReader { geometry in
            let card = CGSize(width: min(620, geometry.size.width - 24), height: geometry.size.height * 0.88)
            ZStack(alignment: .bottom) {
                Reel(active: !creating) { ware in
                    requested = ware
                    creating = true
                }.allowsHitTesting(!creating).accessibilityHidden(creating)
                Color.black.opacity(creating ? 0.3 : 0).ignoresSafeArea().allowsHitTesting(false)
                // Stay mounted so account, fonts and engine warm up while the feed is open.
                MimeCreation(requested: $requested, active: $creating, size: card, drag: $cardDrag)
                    .frame(width: card.width, height: card.height)
                    .clipShape(RoundedRectangle(cornerRadius: 28, style: .continuous))
                    .shadow(color: .black.opacity(0.45), radius: 24, y: 10)
                    .padding(.bottom, 8)
                    .offset(y: creating || reduceMotion ? cardDrag : geometry.size.height + 100)
                    .opacity(creating ? 1 : 0)
                    .allowsHitTesting(creating).accessibilityHidden(!creating)
            }
            .animation(reduceMotion ? .easeOut(duration: 0.15) : .spring(response: 0.42, dampingFraction: 0.88), value: creating)
        }.preferredColorScheme(.dark)
    }
}
