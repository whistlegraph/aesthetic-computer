import SwiftUI
import Foundation
import CryptoKit

struct TVDevice: Identifiable, Equatable {
    let id: String
    let name: String
    let url: URL
    /// A screen paired over the knot by its four-letter code (a TV browser, the Xbox's Edge) rather than AC OS on the LAN.
    var knot: String? = nil
}

// Native execution: only painted checkpoints leave the phone, over the LAN.
// lanserv writes the piece atomically before its main loop hot-loads it.
@MainActor final class WhistlegraphTV: NSObject, ObservableObject, NetServiceBrowserDelegate, NetServiceDelegate {
    @Published private(set) var devices: [TVDevice] = []
    @Published private(set) var selected: TVDevice?
    @Published private(set) var status = ""
    @Published private(set) var searching = false
    private let browser = NetServiceBrowser()
    private var services: [NetService] = []
    private var latestSource = ""
    private var sentSource = ""
    private var previousPiece = ""
    private var connected = false
    private var sending: Task<Void, Never>?
    private var connection = UUID()
    private var progress = ""
    private var progressTask: Task<Void, Never>?
    /// The signed-in bearer, for screens paired over the knot. Set by the session.
    var token: () async -> String? = { nil }
    private static let knotAPI = URL(string: "https://aesthetic.computer/api/whistlegraph-screen")!
    private static let pairedKey = "whistlegraph-knot-screens"
    @Published var pairing = ""
    @Published private(set) var pairingStatus = ""

    func updateProgress(_ snapshot: PieceSnapshot) {
        let data: [String: Any] = ["code": snapshot.code, "phase": snapshot.phase, "busy": snapshot.busy]
        guard let bytes = try? JSONSerialization.data(withJSONObject: data), let value = String(data: bytes, encoding: .utf8) else { return }
        progress = value
    }
    private func startProgress() {
        progressTask?.cancel()
        let expected = connection
        progressTask = Task {
            while !Task.isCancelled && connection == expected {
                if let device = selected, connected, let bytes = progress.data(using: .utf8), var value = (try? JSONSerialization.jsonObject(with: bytes)) as? [String: Any] {
                    value["updatedAt"] = Date().timeIntervalSince1970 * 1000
                    if let code = device.knot { _ = try? await knotRequest("PUT", code: code, body: ["status": value]) }
                    else if let data = try? JSONSerialization.data(withJSONObject: value), let text = String(data: data, encoding: .utf8) {
                        try? await upload(text, name: "wgtv-status.json", device: device, jump: false)
                    }
                }
                try? await Task.sleep(for: .milliseconds(750))
            }
        }
    }

    override init() { super.init(); browser.delegate = self }
    func discover() {
        DeviceActionLog.shared.record(.projection, .started)
        Task { await listKnotScreens() }
        guard !searching else { return }
        searching = true
        browser.searchForServices(ofType: "_http._tcp.", inDomain: "local.")
    }
    // MARK: Screens over the knot
    private func knotRequest(_ method: String, code: String?, body: [String: Any]? = nil) async throws -> (Data, Int) {
        guard let token = await token() else { throw URLError(.userAuthenticationRequired) }
        var url = Self.knotAPI
        if let code { url = url.appending(queryItems: [URLQueryItem(name: "code", value: code)]) }
        var request = URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 12)
        request.httpMethod = method
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        if let body { request.setValue("application/json", forHTTPHeaderField: "Content-Type"); request.httpBody = try JSONSerialization.data(withJSONObject: body) }
        let (data, response) = try await URLSession.shared.data(for: request)
        return (data, (response as? HTTPURLResponse)?.statusCode ?? 0)
    }
    private static func knotDevice(_ code: String) -> TVDevice {
        TVDevice(id: "knot:" + code, name: "Screen " + code, url: knotAPI.appending(queryItems: [URLQueryItem(name: "code", value: code)]), knot: code)
    }
    /// Screens this handle paired before and that still poll the knot.
    private func listKnotScreens() async {
        guard let (data, status) = try? await knotRequest("GET", code: nil), status == 200,
              let value = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let screens = value["screens"] as? [[String: Any]] else { return }
        let alive = screens.compactMap { $0["code"] as? String }
        devices.removeAll { $0.knot != nil && !alive.contains($0.knot!) }
        for code in alive where !devices.contains(where: { $0.knot == code }) { devices.append(Self.knotDevice(code)) }
        devices.sort { $0.name < $1.name }
    }
    /// Pair the screen showing these four letters, then project to it.
    func pair() {
        let code = pairing.trimmingCharacters(in: .whitespacesAndNewlines).uppercased()
        guard code.count == 4, code.allSatisfy({ $0.isLetter && $0.isASCII }) else { pairingStatus = "Type the four letters on the screen."; return }
        pairingStatus = "Pairing…"
        Task {
            do {
                let (data, status) = try await knotRequest("POST", code: code, body: ["action": "pair"])
                guard status == 200 else {
                    let message = (try? JSONSerialization.jsonObject(with: data) as? [String: Any])?["error"] as? String
                    pairingStatus = message ?? "No screen shows \(code)."; return
                }
                pairingStatus = ""; pairing = ""
                let device = Self.knotDevice(code)
                devices.removeAll { $0.id == device.id }; devices.append(device); devices.sort { $0.name < $1.name }
                connect(device)
            } catch { pairingStatus = "Sign in to pair a screen." }
        }
    }
    func stopDiscovery() { browser.stop(); searching = false }
    func netServiceBrowser(_ browser: NetServiceBrowser, didFind service: NetService, moreComing: Bool) {
        services.append(service); service.delegate = self; service.resolve(withTimeout: 5)
    }
    func netServiceBrowser(_ browser: NetServiceBrowser, didRemove service: NetService, moreComing: Bool) {
        services.removeAll { $0 == service }; devices.removeAll { $0.id == service.name + service.domain }
    }
    func netServiceBrowser(_ browser: NetServiceBrowser, didNotSearch errorDict: [String: NSNumber]) {
        searching = false; status = "Allow Local Network access in Settings to find AC OS devices."
    }
    func netServiceDidResolveAddress(_ sender: NetService) {
        guard let host = sender.hostName, host.hasSuffix(".local."), sender.port > 0,
              let url = URL(string: "http://\(host.dropLast()):\(sender.port)") else { return }
        Task {
            guard let state = try? await readStatus(url), state.build != nil, state.ip != nil,
                  let name = state.name, state.piece != nil else { return }
            let device = TVDevice(id: sender.name + sender.domain, name: name, url: url)
            devices.removeAll { $0.id == device.id }; devices.append(device); devices.sort { $0.name < $1.name }
        }
    }
    private struct DeviceStatus: Decodable { let name: String?; let build: String?; let ip: String?; let piece: String? }
    private func readStatus(_ url: URL) async throws -> DeviceStatus {
        let request = URLRequest(url: url.appendingPathComponent("status"), cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 5)
        let (data,response) = try await URLSession.shared.data(for: request)
        guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
        return try JSONDecoder().decode(DeviceStatus.self, from: data)
    }
    func connect(_ device: TVDevice) {
        DeviceActionLog.shared.record(.projection, .requested)
        sending?.cancel(); sending = nil; connection = UUID(); sentSource = ""
        selected = device; connected = false; previousPiece = ""; status = "Connecting…"
        let expected = connection
        if let code = device.knot {
            Task {
                guard let (_, status) = try? await knotRequest("GET", code: code), connection == expected else { return }
                if status == 200 { connected = true; sendLatest(); startProgress() }
                else { self.status = "Screen \(code) is not paired to you any more. Pair it again."; devices.removeAll { $0.id == device.id }; selected = nil }
            }
            return
        }
        Task {
            do {
                let state = try await readStatus(device.url)
                guard connection == expected else { return }
                previousPiece = state.piece ?? "prompt"; connected = true; sendLatest(); startProgress()
            } catch { if connection == expected { status = "Could not reach \(device.name)." } }
        }
    }
    func update(_ source: String) { latestSource = source; sendLatest() }
    private func sendLatest() {
        guard let device = selected, connected, sending == nil else { return }
        guard !latestSource.isEmpty else { status = "Waiting for the picture…"; return }
        guard latestSource != sentSource else { return }
        let expected = connection
        sending = Task {
            defer { if connection == expected { sending = nil } }
            do {
                while !Task.isCancelled && connection == expected && latestSource != sentSource {
                    let source = latestSource
                    guard source.utf8.count <= 512_000 else { throw URLError(.dataLengthExceedsMaximum) }
                    status = "Sending…"
                    if let code = device.knot {
                        let (_, code_) = try await knotRequest("PUT", code: code, body: ["source": source])
                        guard code_ == 200 else { throw URLError(.badServerResponse) }
                        guard !Task.isCancelled, connection == expected else { return }
                        sentSource = source; status = "Projecting to \(device.name)"
                        continue
                    }
                    let hash = SHA256.hash(data: Data(source.utf8)).map { String(format: "%02x", $0) }.joined()
                    let name = "wg-" + String(hash.prefix(16)) + ".mjs"
                    guard let runtimeURL = Bundle.main.url(forResource: "wgtv-compat", withExtension: "js", subdirectory: "Web") else { throw URLError(.fileDoesNotExist) }
                    let runtime = try String(contentsOf: runtimeURL, encoding: .utf8)
                    var receiver = "import * as piece from './" + name + "';\n" + runtime + "\n"
                    for hook in ["boot", "sim", "paint", "act", "leave"] {
                        receiver += "export function " + hook + "(api){const result=piece." + hook + "?.(wgtvApi(api));" + (hook == "paint" ? "wgtvPaintStatus(api);" : "") + "return result;}\n"
                    }
                    try await upload(source, name: name, device: device, jump: false)
                    guard !Task.isCancelled, connection == expected else { return }
                    try await upload(receiver, name: "wgtv.mjs", device: device, jump: true)
                    guard !Task.isCancelled, connection == expected else { return }
                    sentSource = source; status = "Projecting to \(device.name)"
                }
            } catch { if !Task.isCancelled && connection == expected { status = "Could not send to \(device.name). Tap the device to reconnect." } }
        }
    }
    private func upload(_ source: String, name: String, device: TVDevice, jump: Bool) async throws {
        let file = device.url.appendingPathComponent("pieces/" + name)
        let url = jump ? file.appending(queryItems: [URLQueryItem(name: "jump", value: "1")]) : file
        var request = URLRequest(url: url, timeoutInterval: 10)
        request.httpMethod = "PUT"; request.setValue("text/javascript; charset=utf-8", forHTTPHeaderField: "Content-Type")
        request.httpBody = Data(source.utf8)
        let (_, response) = try await URLSession.shared.data(for: request)
        guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
        try Task.checkCancellation()
        let readback = URLRequest(url: file, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 5)
        let (bytes, reply) = try await URLSession.shared.data(for: readback)
        guard (reply as? HTTPURLResponse)?.statusCode == 200, bytes == Data(source.utf8) else { throw URLError(.cannotDecodeContentData) }
    }
    func disconnect() {
        DeviceActionLog.shared.record(.projection, .cancelled)
        progressTask?.cancel(); progressTask = nil
        let device = selected, previous = previousPiece
        connection = UUID(); sending?.cancel(); sending = nil; selected = nil; connected = false; sentSource = ""; status = ""
        if let device, let code = device.knot {
            // Unpairing clears the picture; the screen shows its letters again.
            devices.removeAll { $0.id == device.id }
            Task { _ = try? await knotRequest("DELETE", code: code) }
            return
        }
        // Restore only if this phone's receiver slot still owns the screen.
        guard let device, previous != "wgtv", !previous.isEmpty,
              previous.allSatisfy({ $0.isASCII && ($0.isLetter || $0.isNumber || $0 == "-" || $0 == "_") }) else { return }
        Task {
            guard let state = try? await readStatus(device.url), state.piece == "wgtv" else { return }
            var request = URLRequest(url: device.url.appendingPathComponent("jump/" + previous), timeoutInterval: 5)
            request.httpMethod = "POST"; _ = try? await URLSession.shared.data(for: request)
        }
    }
}

struct WhistlegraphTVSheet: View {
    @ObservedObject var tv: WhistlegraphTV
    @Environment(\.dismiss) private var dismiss
    var body: some View {
        NavigationStack {
            List {
                ForEach(tv.devices) { device in
                    Button { tv.connect(device) } label: {
                        HStack { Label(device.name, systemImage: device.knot != nil ? "sparkles.tv" : "tv"); Spacer(); if tv.selected == device { Image(systemName: "checkmark") } }
                    }.accessibilityIdentifier("tv-device-" + device.name)
                }
                if tv.devices.isEmpty { Text("Looking for AC OS on your Wi-Fi…").foregroundStyle(.secondary) }
                if !tv.status.isEmpty { Text(tv.status).accessibilityIdentifier("tv-status") }
                if tv.selected != nil { Button("Stop projecting", role: .destructive) { tv.disconnect() } }
                Section {
                    HStack {
                        TextField("Letters on the screen", text: $tv.pairing)
                            .textInputAutocapitalization(.characters).autocorrectionDisabled().font(.system(.title3, design: .monospaced))
                            .onSubmit { tv.pair() }.accessibilityIdentifier("tv-pair-code")
                        Button("Pair") { tv.pair() }.disabled(tv.pairing.trimmingCharacters(in: .whitespaces).count != 4).accessibilityIdentifier("tv-pair")
                    }
                    if !tv.pairingStatus.isEmpty { Text(tv.pairingStatus).font(.footnote).foregroundStyle(.secondary) }
                } header: { Text("Pair a screen") } footer: {
                    Text("On the TV, open aesthetic.computer/wgtv in its browser (the Xbox's Edge works) and type the four letters it shows. Pieces play there through the knot, so it works off your Wi-Fi too, and a controller on the TV reaches the piece.")
                }
            }
            .navigationTitle("TV")
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
        }.onAppear { tv.discover() }.onDisappear { tv.stopDiscovery() }
    }
}
