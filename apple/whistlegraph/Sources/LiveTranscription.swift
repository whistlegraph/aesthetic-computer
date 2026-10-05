import Foundation

/// Only a short-lived transcription credential reaches the phone. Microphone
/// chunks are sent during the take; committing ends the turn, not starts it.
@MainActor final class LiveTranscription {
    var onText: (String, Bool) -> Void = { _, _ in }
    var onFailure: () -> Void = {}
    private var socket: URLSessionWebSocketTask?
    private var receiver: Task<Void, Never>?
    private var sender: Task<Void, Never>?
    private var opening: Task<Void, Never>?
    private var pending = Data()
    private var ready = false, ended = false, committed = false, closed = false
    private var text = "", receivedBytes = 0

    func start(token: @escaping () async throws -> String?) {
        opening = Task { [weak self] in
            do {
                guard let self, let bearer = try await token(), !self.closed else { return }
                var request = URLRequest(url: URL(string: "https://help.aesthetic.computer/api/aesel/transcription-session")!)
                request.httpMethod = "POST"; request.timeoutInterval = 10
                request.setValue("Bearer " + bearer, forHTTPHeaderField: "Authorization")
                let (data, response) = try await URLSession.shared.data(for: request)
                guard !Task.isCancelled, !self.closed else { return }
                guard (response as? HTTPURLResponse)?.statusCode == 200,
                      let value = try JSONSerialization.jsonObject(with: data) as? [String: Any],
                      let secret = value["value"] as? String else { throw URLError(.badServerResponse) }
                var connection = URLRequest(url: URL(string: "wss://api.openai.com/v1/realtime?intent=transcription")!)
                connection.setValue("Bearer " + secret, forHTTPHeaderField: "Authorization")
                let socket = URLSession.shared.webSocketTask(with: connection)
                self.socket = socket; socket.resume()
                self.receiver = Task { [weak self] in
                    do { while !Task.isCancelled, let self, !self.closed { try await self.receive(socket.receive()) } }
                    catch { if !Task.isCancelled { self?.fail() } }
                }
            } catch { if !Task.isCancelled { self?.fail() } }
        }
    }

    func append(_ data: Data) {
        guard !closed, !ended else { return }
        receivedBytes += data.count
        guard receivedBytes <= 46 * 24_000 * 2 else { fail(); return }
        pending.append(data); drain()
    }
    func finish() { ended = true; drain() }
    private func send(_ value: [String: Any]) async throws {
        guard let socket else { throw URLError(.notConnectedToInternet) }
        let data = try JSONSerialization.data(withJSONObject: value)
        try await socket.send(.string(String(decoding: data, as: UTF8.self)))
    }
    private func drain() {
        guard ready, !closed, sender == nil else { return }
        sender = Task { [weak self] in
            guard let self else { return }
            defer { self.sender = nil }
            do {
                while !Task.isCancelled, !self.closed {
                    if self.pending.count >= 4800 || (self.ended && !self.pending.isEmpty) {
                        let size = min(4800, self.pending.count), chunk = self.pending.prefix(size)
                        self.pending.removeFirst(size)
                        try await self.send(["type": "input_audio_buffer.append", "audio": chunk.base64EncodedString()])
                    } else {
                        if self.ended && !self.committed && self.receivedBytes >= 4800 {
                            self.committed = true
                            try await self.send(["type": "input_audio_buffer.commit"])
                        }
                        break
                    }
                }
            } catch { if !Task.isCancelled { self.fail() } }
        }
    }
    private func receive(_ message: URLSessionWebSocketTask.Message) throws {
        let data: Data
        switch message { case .data(let d): data = d; case .string(let s): data = Data(s.utf8); @unknown default: return }
        guard let event = try JSONSerialization.jsonObject(with: data) as? [String: Any], let type = event["type"] as? String else { return }
        switch type {
        case "session.created", "session.updated": ready = true; drain()
        case "conversation.item.input_audio_transcription.delta":
            text += event["delta"] as? String ?? ""; onText(text.trimmingCharacters(in: .whitespacesAndNewlines), false)
        case "conversation.item.input_audio_transcription.completed":
            text = event["transcript"] as? String ?? text; onText(text.trimmingCharacters(in: .whitespacesAndNewlines), true)
        case "error", "conversation.item.input_audio_transcription.failed": fail()
        default: break
        }
    }
    private func fail() { guard !closed else { return }; close(); onFailure() }
    func close() {
        closed = true; opening?.cancel(); receiver?.cancel(); sender?.cancel()
        socket?.cancel(with: .normalClosure, reason: nil); socket = nil; pending.removeAll()
    }
}

/// Stateful resampling keeps chunk boundaries off the performance clock.
final class SpeechPCM {
    private var position = 0.0, offset = 0, last = 0.0
    func append(_ samples: [Double], rate: Double) -> Data {
        guard rate > 0, !samples.isEmpty else { return Data() }
        var data = Data(); data.reserveCapacity(samples.count * 2)
        let end = offset + samples.count - 1
        while position <= Double(end) {
            let lower = Int(floor(position)), fraction = position - Double(lower)
            let a = lower < offset ? last : samples[lower - offset]
            let b = fraction == 0 ? a : samples[lower + 1 - offset]
            let sample = a + (b - a) * fraction
            let value = Int16(max(-32768, min(32767, (sample.isFinite ? sample : 0) * 32767)))
            let bits = UInt16(bitPattern: value)
            data.append(UInt8(bits & 255)); data.append(UInt8(bits >> 8))
            position += rate / 24000
        }
        offset += samples.count; last = samples.last!; return data
    }
}
