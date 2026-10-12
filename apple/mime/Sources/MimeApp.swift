import SwiftUI
import AVFoundation

/// mime.ac: the one place to watch what everyone makes across Aesthetic
/// Computer, KidLisp and Whistlegraph. Creation lives in those apps; this one
/// only watches.
@main
struct MimeApp: App {
    init() {
        // Pieces, KidLisp and videos sound like a reel: on, and mixed with nothing else.
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        try? AVAudioSession.sharedInstance().setActive(true)
    }
    var body: some Scene {
        WindowGroup { Reel() }
    }
}
