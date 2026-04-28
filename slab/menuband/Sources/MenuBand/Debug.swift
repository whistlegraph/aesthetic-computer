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
