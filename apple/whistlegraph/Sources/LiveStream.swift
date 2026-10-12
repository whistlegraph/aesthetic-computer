import Foundation
import Network

// The AirPlay video stream. live-tape.js records the piece as fragmented MP4;
// this cuts it into HLS segments at keyframes and serves them on the Wi-Fi, so
// an Apple TV can play the piece a few seconds behind the phone, like a video.
@MainActor final class LiveStream {
    /// Seconds of piece per segment. Latency is roughly three of these.
    static let target = 1.0
    private(set) var port: UInt16 = 0
    private var listener: NWListener?
    private var parser = FragmentedMP4()
    private var generation = -1
    private var segments: [Segment] = []
    private var pending = Data(), pendingDuration = 0.0, pendingIndependent = true
    private var sequence = 0, discontinuities = 0
    private var inits: [Int: Data] = [:]
    private var newGeneration = false
    var onReady: (() -> Void)?

    struct Segment { let index: Int; let generation: Int; let data: Data; let duration: Double; let discontinuity: Bool }

    var ready: Bool { segments.count >= 2 }
    var url: URL? {
        guard port != 0, let address = LiveStream.wifiAddress() else { return nil }
        return URL(string: "http://\(address):\(port)/live.m3u8")
    }

    func start() throws {
        guard listener == nil else { return }
        let listener = try NWListener(using: .tcp)
        listener.newConnectionHandler = { [weak self] connection in
            Task { @MainActor in self?.serve(connection) }
        }
        listener.stateUpdateHandler = { [weak self] state in
            if case .ready = state { Task { @MainActor in self?.port = listener.port?.rawValue ?? 0 } }
        }
        listener.start(queue: .main)
        self.listener = listener
    }
    func stop() {
        listener?.cancel(); listener = nil; port = 0
        reset()
    }
    func reset() {
        parser = FragmentedMP4(); generation = -1; segments = []; inits = [:]
        pending = Data(); pendingDuration = 0; sequence = 0; discontinuities = 0
    }

    /// Bytes from one recorder run. A new generation means a new recorder, so a new init segment.
    func append(_ data: Data, generation: Int) {
        if generation != self.generation {
            self.generation = generation
            parser = FragmentedMP4(); pending = Data(); pendingDuration = 0
            newGeneration = true
        }
        let pieces = parser.append(data)
        for piece in pieces {
            switch piece {
            case .initialization(let data): inits[generation] = data
            case .fragment(let data, let duration, let independent): add(data, duration, independent)
            }
        }
    }

    private func add(_ fragment: Data, _ duration: Double, _ independent: Bool) {
        // Close the open segment at a keyframe once it is long enough, so every
        // segment can start playback on its own.
        if !pending.isEmpty, independent, pendingDuration >= LiveStream.target - 0.05 { close() }
        if pending.isEmpty { pendingIndependent = independent }
        pending.append(fragment); pendingDuration += duration
        // Without a keyframe for a long while, cut anyway rather than stall the stream.
        if pendingDuration >= LiveStream.target * 4 { close() }
    }
    private func close() {
        guard !pending.isEmpty else { return }
        segments.append(Segment(index: sequence + segments.count, generation: generation, data: pending, duration: pendingDuration, discontinuity: newGeneration && !segments.isEmpty))
        newGeneration = false
        pending = Data(); pendingDuration = 0
        while segments.count > 8 {
            let gone = segments.removeFirst(); sequence += 1
            if gone.discontinuity { discontinuities += 1 }
        }
        inits = inits.filter { key, _ in segments.contains { $0.generation == key } || key == generation }
        if segments.count == 2 { onReady?() }
    }

    var playlist: String {
        let target = Int((segments.map(\.duration).max() ?? LiveStream.target).rounded(.up))
        var lines = ["#EXTM3U", "#EXT-X-VERSION:7", "#EXT-X-TARGETDURATION:\(max(1, target))",
                     "#EXT-X-MEDIA-SEQUENCE:\(sequence)", "#EXT-X-DISCONTINUITY-SEQUENCE:\(discontinuities)",
                     // Start as close to now as the player allows.
                     "#EXT-X-START:TIME-OFFSET=-\(String(format: "%.1f", LiveStream.target * 2)),PRECISE=YES"]
        var map = -1
        for segment in segments {
            if segment.discontinuity { lines.append("#EXT-X-DISCONTINUITY") }
            if segment.generation != map { lines.append("#EXT-X-MAP:URI=\"init-\(segment.generation).mp4\""); map = segment.generation }
            lines.append(String(format: "#EXTINF:%.3f,", segment.duration))
            lines.append("segment-\(segment.index).m4s")
        }
        return lines.joined(separator: "\n") + "\n"
    }

    // MARK: HTTP

    private func serve(_ connection: NWConnection) {
        connection.start(queue: .main)
        connection.receive(minimumIncompleteLength: 1, maximumLength: 8192) { [weak self] data, _, _, _ in
            Task { @MainActor in
                guard let self, let data, let request = String(data: data, encoding: .utf8),
                      let line = request.split(separator: "\r\n").first else { connection.cancel(); return }
                let parts = line.split(separator: " ")
                let path = parts.count > 1 ? String(parts[1]).split(separator: "?").first.map(String.init) ?? "" : ""
                let (status, type, body) = self.response(path)
                let head = parts.first == "HEAD"
                let header = "HTTP/1.1 \(status)\r\nContent-Type: \(type)\r\nContent-Length: \(body.count)\r\nCache-Control: no-cache\r\nAccess-Control-Allow-Origin: *\r\nConnection: close\r\n\r\n"
                var reply = Data(header.utf8); if !head { reply.append(body) }
                connection.send(content: reply, completion: .contentProcessed { _ in connection.cancel() })
            }
        }
    }
    private func response(_ path: String) -> (String, String, Data) {
        if path == "/live.m3u8" { return ("200 OK", "application/vnd.apple.mpegurl", Data(playlist.utf8)) }
        if path.hasPrefix("/init-"), let generation = Int(path.dropFirst(6).dropLast(4)), let data = inits[generation] {
            return ("200 OK", "video/mp4", data)
        }
        if path.hasPrefix("/segment-"), let index = Int(path.dropFirst(9).dropLast(4)), let segment = segments.first(where: { $0.index == index }) {
            return ("200 OK", "video/iso.segment", segment.data)
        }
        return ("404 Not Found", "text/plain", Data("not here\n".utf8))
    }

    /// The phone's Wi-Fi address, which the Apple TV fetches the stream from.
    static func wifiAddress() -> String? {
        var head: UnsafeMutablePointer<ifaddrs>?
        guard getifaddrs(&head) == 0, let first = head else { return nil }
        defer { freeifaddrs(head) }
        var fallback: String?
        for pointer in Swift.sequence(first: first, next: { $0.pointee.ifa_next }) {
            let entry = pointer.pointee
            guard let address = entry.ifa_addr, address.pointee.sa_family == UInt8(AF_INET),
                  entry.ifa_flags & UInt32(IFF_UP) != 0, entry.ifa_flags & UInt32(IFF_LOOPBACK) == 0 else { continue }
            var host = [CChar](repeating: 0, count: Int(NI_MAXHOST))
            guard getnameinfo(address, socklen_t(address.pointee.sa_len), &host, socklen_t(host.count), nil, 0, NI_NUMERICHOST) == 0 else { continue }
            let name = String(cString: entry.ifa_name), value = String(cString: host)
            if name == "en0" { return value }
            if fallback == nil, name.hasPrefix("en") { fallback = value }
        }
        #if targetEnvironment(simulator)
        return fallback ?? "127.0.0.1"
        #else
        return fallback
        #endif
    }
}

/// Splits a fragmented MP4 byte stream into its init segment and its fragments,
/// reading each fragment's video duration and whether it opens on a keyframe.
struct FragmentedMP4 {
    enum Piece { case initialization(Data); case fragment(Data, duration: Double, independent: Bool) }
    private var buffer = Data()
    private var header = Data()
    private var fragment = Data()
    private var tracks: [UInt32: Track] = [:]
    private var video: UInt32?
    struct Track { var timescale: UInt32 = 1000; var duration: UInt32 = 0; var flags: UInt32 = 0 }

    mutating func append(_ data: Data) -> [Piece] {
        buffer.append(data)
        var pieces: [Piece] = []
        while buffer.count >= 8 {
            let start = buffer.startIndex
            var size = Int(buffer.uint32(at: start))
            let type = String(decoding: buffer[start + 4 ..< start + 8], as: UTF8.self)
            if size == 1, buffer.count >= 16 { size = Int(buffer.uint64(at: start + 8)) }
            guard size >= 8, buffer.count >= size else { break }
            let box = Data(buffer[start ..< start + size])
            buffer = Data(buffer[(start + size)...])
            switch type {
            case "ftyp": header = box
            case "moov":
                header.append(box); readMovie(box)
                pieces.append(.initialization(header)); header = Data()
            case "moof", "styp", "sidx", "prft", "emsg":
                fragment.append(box)
            case "mdat":
                fragment.append(box)
                let (duration, independent) = readFragment(fragment)
                pieces.append(.fragment(fragment, duration: duration, independent: independent))
                fragment = Data()
            default: break
            }
        }
        return pieces
    }

    private mutating func readMovie(_ moov: Data) {
        for trak in children(moov, "trak") {
            guard let tkhd = child(trak, "tkhd") else { continue }
            let version = tkhd[tkhd.startIndex + 8]
            let id = tkhd.uint32(at: tkhd.startIndex + 8 + (version == 1 ? 20 : 12))
            var track = tracks[id] ?? Track()
            if let mdia = child(trak, "mdia") {
                if let mdhd = child(mdia, "mdhd") {
                    let v = mdhd[mdhd.startIndex + 8]
                    track.timescale = mdhd.uint32(at: mdhd.startIndex + 8 + (v == 1 ? 20 : 12))
                }
                if let hdlr = child(mdia, "hdlr"), String(decoding: hdlr[hdlr.startIndex + 16 ..< hdlr.startIndex + 20], as: UTF8.self) == "vide" { video = id }
            }
            tracks[id] = track
        }
        if let mvex = child(moov, "mvex") {
            for trex in children(mvex, "trex") {
                let base = trex.startIndex + 12
                let id = trex.uint32(at: base)
                var track = tracks[id] ?? Track()
                track.duration = trex.uint32(at: base + 8); track.flags = trex.uint32(at: base + 16)
                tracks[id] = track
            }
        }
    }

    /// Video duration in seconds, and whether its first video sample is a keyframe.
    private func readFragment(_ data: Data) -> (Double, Bool) {
        guard let moof = children(data, "moof", header: 0).first else { return (0, false) }
        for traf in children(moof, "traf") {
            guard let tfhd = child(traf, "tfhd") else { continue }
            let tf = tfhd.uint32(at: tfhd.startIndex + 8) & 0xFFFFFF
            let id = tfhd.uint32(at: tfhd.startIndex + 12)
            guard id == video || video == nil, let track = tracks[id] else { continue }
            var offset = tfhd.startIndex + 16
            if tf & 0x01 != 0 { offset += 8 }
            if tf & 0x02 != 0 { offset += 4 }
            var defaultDuration = track.duration, defaultFlags = track.flags
            if tf & 0x08 != 0 { defaultDuration = tfhd.uint32(at: offset); offset += 4 }
            if tf & 0x10 != 0 { offset += 4 }
            if tf & 0x20 != 0 { defaultFlags = tfhd.uint32(at: offset) }
            var total: UInt64 = 0, firstFlags: UInt32?
            for trun in children(traf, "trun") {
                let flags = trun.uint32(at: trun.startIndex + 8) & 0xFFFFFF
                let count = Int(trun.uint32(at: trun.startIndex + 12))
                var cursor = trun.startIndex + 16
                if flags & 0x01 != 0 { cursor += 4 }
                var first: UInt32?
                if flags & 0x04 != 0 { first = trun.uint32(at: cursor); cursor += 4 }
                for sample in 0 ..< count {
                    var duration = defaultDuration, sampleFlags = defaultFlags
                    if flags & 0x100 != 0 { duration = trun.uint32(at: cursor); cursor += 4 }
                    if flags & 0x200 != 0 { cursor += 4 }
                    if flags & 0x400 != 0 { sampleFlags = trun.uint32(at: cursor); cursor += 4 }
                    if flags & 0x800 != 0 { cursor += 4 }
                    if sample == 0, firstFlags == nil { firstFlags = first ?? sampleFlags }
                    total += UInt64(duration)
                }
            }
            // sample_is_non_sync_sample is bit 16 of the sample flags.
            let independent = (firstFlags ?? 0) & 0x10000 == 0
            return (Double(total) / Double(max(1, track.timescale)), independent)
        }
        return (0, false)
    }

    /// The boxes of one type inside `box`, whose own header is `header` bytes (0 for a run of top-level boxes).
    private func children(_ box: Data, _ type: String, header: Int = 8) -> [Data] {
        var found: [Data] = []
        var cursor = box.startIndex + header
        while cursor + 8 <= box.endIndex {
            let size = Int(box.uint32(at: cursor))
            guard size >= 8, cursor + size <= box.endIndex else { break }
            if String(decoding: box[cursor + 4 ..< cursor + 8], as: UTF8.self) == type { found.append(box[cursor ..< cursor + size]) }
            cursor += size
        }
        return found
    }
    private func child(_ box: Data, _ type: String) -> Data? { children(box, type).first }
}

private extension Data {
    func uint32(at index: Int) -> UInt32 {
        guard index + 4 <= endIndex else { return 0 }
        return self[index ..< index + 4].reduce(0) { $0 << 8 | UInt32($1) }
    }
    func uint64(at index: Int) -> UInt64 {
        guard index + 8 <= endIndex else { return 0 }
        return self[index ..< index + 8].reduce(0) { $0 << 8 | UInt64($1) }
    }
}

/// The live recorder injected into the AirPlay camera runtime: the shared AC
/// canvas-tape encoder plus live-tape.js.
enum LiveTape {
    static var script: String {
        guard let root = Bundle.main.url(forResource: "Web", withExtension: nil),
              let encoder = try? String(contentsOf: root.appendingPathComponent("canvas-tape.mjs"), encoding: .utf8),
              let tape = try? String(contentsOf: root.appendingPathComponent("live-tape.js"), encoding: .utf8) else { return "" }
        return encoder.replacingOccurrences(of: "export function", with: "function") + "\n" + tape
    }
}
