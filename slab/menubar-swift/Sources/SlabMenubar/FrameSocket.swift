import Foundation
import Darwin

/// One bounded request per connection. The Frame queue owns execution and the
/// result; a disconnected client never causes an input request to be replayed.
final class FrameSocket {
    private var listener: Int32 = -1
    private let clients = DispatchQueue(label: "computer.slab.frame.socket", qos: .userInitiated)
    private var seen = Set<String>()
    private var order: [String] = []

    func start(path: String, perform: @escaping (String) -> (Data, Data)) throws {
        let fd = socket(AF_UNIX, SOCK_STREAM, 0)
        guard fd >= 0 else { throw NSError(domain: "frame.socket", code: Int(errno)) }
        var address = sockaddr_un()
        address.sun_family = sa_family_t(AF_UNIX)
        let bytes = Array(path.utf8)
        guard bytes.count < MemoryLayout.size(ofValue: address.sun_path) else {
            close(fd); throw NSError(domain: "frame.socket.path", code: 1)
        }
        withUnsafeMutableBytes(of: &address.sun_path) { $0.copyBytes(from: bytes + [0]) }
        unlink(path)
        let bound = withUnsafePointer(to: &address) { ptr in
            ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                Darwin.bind(fd, $0, socklen_t(MemoryLayout<sockaddr_un>.size))
            }
        }
        guard bound == 0 else { let error = errno; close(fd); throw NSError(domain: "frame.socket.bind", code: Int(error)) }
        chmod(path, 0o600)
        guard listen(fd, 8) == 0 else { let error = errno; close(fd); throw NSError(domain: "frame.socket.listen", code: Int(error)) }
        listener = fd
        clients.async { [self] in
            while listener >= 0 {
                let client = accept(fd, nil, nil)
                if client < 0 { if errno == EINTR { continue }; break }
                handle(client, perform: perform)
            }
        }
    }

    private func handle(_ fd: Int32, perform: (String) -> (Data, Data)) {
        defer { close(fd) }
        var uid: uid_t = 0, gid: gid_t = 0
        guard getpeereid(fd, &uid, &gid) == 0, uid == getuid() else { return }
        var timeout = timeval(tv_sec: 2, tv_usec: 0)
        setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, socklen_t(MemoryLayout<timeval>.size))
        timeout.tv_sec = 5
        setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, socklen_t(MemoryLayout<timeval>.size))
        var one: Int32 = 1
        setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, socklen_t(MemoryLayout<Int32>.size))
        var request = Data(), buffer = [UInt8](repeating: 0, count: 2048)
        while request.count <= 8192 {
            let n = read(fd, &buffer, buffer.count)
            if n < 0 && errno == EINTR { continue }
            guard n > 0 else { return }
            request.append(contentsOf: buffer.prefix(n))
            if request.contains(10) { break }
        }
        guard request.count <= 8192, request.last == 10,
              let body = (try? JSONSerialization.jsonObject(with: request)) as? [String: Any],
              let id = body["id"] as? String, UUID(uuidString: id) != nil,
              let mode = body["mode"] as? String, !mode.contains("\n") else { return }
        let result: (Data, Data)
        if seen.contains(id) {
            result = (Data("{\"error\":\"Duplicate native request; request not retried\"}".utf8), Data())
        } else {
            seen.insert(id); order.append(id)
            if order.count > 64 { seen.remove(order.removeFirst()) }
            result = perform(mode)
        }
        guard let frame = try? JSONSerialization.jsonObject(with: result.0),
              let json = try? JSONSerialization.data(withJSONObject: ["id": id, "frame": frame]),
              json.count + result.1.count <= 16 * 1024 * 1024 else { return }
        var packet = Data("ACF2".utf8)
        for length in [json.count, result.1.count] {
            var value = UInt32(length).bigEndian
            withUnsafeBytes(of: &value) { packet.append(contentsOf: $0) }
        }
        packet.append(json); packet.append(result.1)
        packet.withUnsafeBytes { bytes in
            var offset = 0
            while offset < bytes.count {
                let n = write(fd, bytes.baseAddress!.advanced(by: offset), bytes.count - offset)
                if n < 0 && errno == EINTR { continue }
                guard n > 0 else { return }
                offset += n
            }
        }
    }
}
