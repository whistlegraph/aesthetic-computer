import SwiftUI
import Foundation
import CryptoKit

struct TVDevice: Identifiable, Equatable {
    let id: String
    let name: String
    let url: URL
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
                    if let data = try? JSONSerialization.data(withJSONObject: value), let text = String(data: data, encoding: .utf8) {
                        try? await upload(text, name: "wgtv-status.json", device: device, jump: false)
                    }
                }
                try? await Task.sleep(for: .milliseconds(750))
            }
        }
    }

    override init() { super.init(); browser.delegate = self }
    func discover() {
        guard !searching else { return }
        searching = true
        browser.searchForServices(ofType: "_http._tcp.", inDomain: "local.")
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
        sending?.cancel(); sending = nil; connection = UUID(); sentSource = ""
        selected = device; connected = false; previousPiece = ""; status = "Connecting…"
        let expected = connection
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
        progressTask?.cancel(); progressTask = nil
        let device = selected, previous = previousPiece
        connection = UUID(); sending?.cancel(); sending = nil; selected = nil; connected = false; sentSource = ""; status = ""
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
                        HStack { Label(device.name, systemImage: "tv"); Spacer(); if tv.selected == device { Image(systemName: "checkmark") } }
                    }.accessibilityIdentifier("tv-device-" + device.name)
                }
                if tv.devices.isEmpty { Text("Looking for AC OS on your Wi-Fi…").foregroundStyle(.secondary) }
                if !tv.status.isEmpty { Text(tv.status).accessibilityIdentifier("tv-status") }
                if tv.selected != nil { Button("Stop projecting", role: .destructive) { tv.disconnect() } }
            }
            .navigationTitle("TV")
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
        }.onAppear { tv.discover() }.onDisappear { tv.stopDiscovery() }
    }
}
