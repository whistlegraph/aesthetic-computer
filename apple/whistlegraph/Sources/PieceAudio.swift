import AVFoundation

// The AC piece owns playback. Recording borrows this session and restores it.
enum PieceAudio {
    static func activate() {
        do {
            let session = AVAudioSession.sharedInstance()
            try session.setCategory(.playback, mode: .default, options: [.mixWithOthers])
            try session.setActive(true)
        } catch { NSLog("Piece audio could not start: %@", error.localizedDescription) }
    }
}
