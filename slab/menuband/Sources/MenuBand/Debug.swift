import Foundation

private let debugLogURL = URL(fileURLWithPath: "/tmp/menuband-debug.log")
private let debugLogQueue = DispatchQueue(label: "menuband.debugLog")
private var debugLogReady = false

func debugLog(_ message: String) {
    debugLogQueue.async {
        if !debugLogReady {
            try? "".write(to: debugLogURL, atomically: true, encoding: .utf8)
            debugLogReady = true
        }
        let line = "\(Date().timeIntervalSince1970) \(message)\n"
        guard let data = line.data(using: .utf8) else { return }
        if let fh = try? FileHandle(forWritingTo: debugLogURL) {
            fh.seekToEndOfFile()
            fh.write(data)
            try? fh.close()
        }
    }
}

/// Diagnostic: the FIRST access to any AVAudioEngine's inputNode in this
/// process builds a private aggregate device, and on the Scarlett that
/// kills the output stream. Log who did it.
func menuBandNoteInputNodeAccess(_ label: String) {
    struct Once { static var logged: Set<String> = [] }
    guard !Once.logged.contains(label) else { return }
    Once.logged.insert(label)
    let frames = Thread.callStackSymbols.dropFirst().prefix(6)
        .map { $0.split(separator: " ").dropFirst(3).prefix(2).joined(separator: " ") }
    NSLog("MenuBand INPUTNODE ACCESS [\(label)] ← \(frames.joined(separator: " < "))")
}
