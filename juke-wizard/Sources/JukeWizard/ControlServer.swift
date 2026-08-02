import Foundation
import Darwin

/// Local, bounded JSON-line control for the running JukeWizard. The Unix
/// socket is user-only; every command is still validated by JukeController.
final class JukeControlServer {
    private let controller: JukeController
    private let socketPath: String
    private var listenFD: Int32 = -1
    private var source: DispatchSourceRead?

    init(controller: JukeController) {
        self.controller = controller
        let config = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".config/jukewizard", isDirectory: true)
        try? FileManager.default.createDirectory(at: config, withIntermediateDirectories: true,
                                                 attributes: [.posixPermissions: 0o700])
        socketPath = config.appendingPathComponent("control.sock").path
    }

    func start() {
        guard socketPath.utf8.count < MemoryLayout<sockaddr_un>.size - 2 else { return }
        unlink(socketPath)
        listenFD = Darwin.socket(AF_UNIX, SOCK_STREAM, 0)
        guard listenFD >= 0 else { return }
        var address = sockaddr_un()
        address.sun_family = sa_family_t(AF_UNIX)
        withUnsafeMutablePointer(to: &address.sun_path) { raw in
            raw.withMemoryRebound(to: CChar.self, capacity: 104) { path in
                _ = socketPath.withCString { strlcpy(path, $0, 104) }
            }
        }
        let length = socklen_t(MemoryLayout<sa_family_t>.size + socketPath.utf8.count + 1)
        let bound = withUnsafePointer(to: &address) {
            $0.withMemoryRebound(to: sockaddr.self, capacity: 1) { Darwin.bind(listenFD, $0, length) }
        }
        guard bound == 0, Darwin.listen(listenFD, 8) == 0 else { stop(); return }
        chmod(socketPath, S_IRUSR | S_IWUSR)
        let s = DispatchSource.makeReadSource(fileDescriptor: listenFD, queue: .main)
        s.setEventHandler { [weak self] in self?.acceptOne() }
        s.setCancelHandler { [fd = listenFD] in if fd >= 0 { Darwin.close(fd) } }
        source = s
        s.resume()
    }

    private func acceptOne() {
        let fd = Darwin.accept(listenFD, nil, nil)
        guard fd >= 0 else { return }
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            var data = Data()
            var byte: UInt8 = 0
            while data.count < 65_536, Darwin.read(fd, &byte, 1) == 1, byte != 10 { data.append(byte) }
            guard !data.isEmpty, data.count < 65_536,
                  let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
                Self.reply(fd, ["ok": false, "error": "invalid or oversized JSON request"]); return
            }
            DispatchQueue.main.async {
                guard let self else { Self.reply(fd, ["ok": false, "error": "JukeWizard unavailable"]); return }
                Self.reply(fd, self.controller.control(object))
            }
        }
    }

    private static func reply(_ fd: Int32, _ object: [String: Any]) {
        var data = (try? JSONSerialization.data(withJSONObject: object, options: [.sortedKeys])) ?? Data("{\"ok\":false}".utf8)
        data.append(10)
        data.withUnsafeBytes { bytes in
            if let base = bytes.baseAddress { _ = Darwin.write(fd, base, data.count) }
        }
        Darwin.close(fd)
    }

    func stop() {
        let hadSource = source != nil
        source?.cancel(); source = nil
        if listenFD >= 0, !hadSource { Darwin.close(listenFD) }
        listenFD = -1
        unlink(socketPath)
    }

    deinit { stop() }
}
