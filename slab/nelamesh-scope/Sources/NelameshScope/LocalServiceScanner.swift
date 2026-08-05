import Foundation

struct LocalService: Identifiable, Hashable {
    enum Reachability: String {
        case loopback = "THIS MAC"
        case network = "NETWORK"
        case allInterfaces = "ALL INTERFACES"
        case unknown = "UNKNOWN"
    }

    let pid: Int
    let process: String
    let transport: String
    let address: String
    let port: UInt16?

    var id: String { "\(pid):\(transport):\(address):\(port.map(String.init) ?? "*")" }

    var reachability: Reachability {
        if address == "127.0.0.1" || address == "::1" || address == "localhost" {
            return .loopback
        }
        if address == "*" || address == "0.0.0.0" || address == "::" {
            return .allInterfaces
        }
        return address.isEmpty ? .unknown : .network
    }

    var endpoint: String {
        let displayAddress = address == "*" ? "any interface" : address
        return port.map { "\(displayAddress):\($0)" } ?? "\(displayAddress):dynamic"
    }

    var likelyProtocol: String {
        guard let port else { return transport }
        let names: [UInt16: String] = [
            22: "SSH", 53: "DNS", 80: "HTTP", 443: "HTTPS", 3000: "HTTP",
            5000: "HTTP / AirPlay", 5173: "HTTP · Vite", 5353: "mDNS",
            5432: "PostgreSQL", 6379: "Redis", 7000: "AirPlay / HTTP",
            8000: "HTTP", 8080: "HTTP", 8443: "HTTPS", 8888: "HTTP · Aesthetic Computer",
            11434: "HTTP · Ollama", 24800: "Deskflow", 27017: "MongoDB",
        ]
        return names[port] ?? transport
    }

    var browserURL: URL? {
        guard transport == "TCP", let port,
              likelyProtocol.contains("HTTP") || likelyProtocol.contains("AirPlay") else { return nil }
        let scheme = likelyProtocol.contains("HTTPS") ? "https" : "http"
        let host = reachability == .loopback || reachability == .allInterfaces ? "localhost" : address
        return URL(string: "\(scheme)://\(host):\(port)")
    }
}

@MainActor
final class LocalServiceScanner: ObservableObject {
    @Published private(set) var services: [LocalService] = []
    @Published private(set) var isScanning = false
    @Published private(set) var lastScanned: Date?
    @Published private(set) var error: String?

    var tcpCount: Int { services.filter { $0.transport == "TCP" }.count }
    var udpCount: Int { services.filter { $0.transport == "UDP" }.count }
    var networkCount: Int { services.filter { $0.reachability != .loopback }.count }

    func scan() {
        guard !isScanning else { return }
        isScanning = true
        error = nil

        Task.detached(priority: .userInitiated) {
            let result = Self.readSockets()
            await MainActor.run {
                self.services = result.sockets
                self.error = result.error
                self.lastScanned = Date()
                self.isScanning = false
            }
        }
    }

    nonisolated private static func readSockets() -> (sockets: [LocalService], error: String?) {
        let process = Process()
        let output = Pipe()
        process.executableURL = URL(fileURLWithPath: "/usr/sbin/lsof")
        process.arguments = ["-nP", "-FpcPn", "-iTCP", "-sTCP:LISTEN", "-iUDP"]
        process.standardOutput = output
        process.standardError = Pipe()

        do { try process.run() } catch { return ([], error.localizedDescription) }
        let data = output.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        guard let text = String(data: data, encoding: .utf8) else {
            return ([], "Could not read the socket inventory.")
        }

        var pid = 0
        var command = "unknown"
        var transport = ""
        var sockets: [LocalService] = []

        for line in text.split(separator: "\n").map(String.init) {
            guard let field = line.first else { continue }
            let value = String(line.dropFirst())
            switch field {
            case "p": pid = Int(value) ?? 0
            case "c": command = value
            case "P": transport = value
            case "n":
                guard transport == "TCP" || transport == "UDP" else { continue }
                let parsed = parseEndpoint(value)
                sockets.append(LocalService(pid: pid, process: command, transport: transport,
                                            address: parsed.address, port: parsed.port))
            default: break
            }
        }

        let unique = Dictionary(grouping: sockets, by: \.id).compactMap { $0.value.first }
        return (unique.sorted {
            ($0.port ?? UInt16.max, $0.transport, $0.process) <
            ($1.port ?? UInt16.max, $1.transport, $1.process)
        }, nil)
    }

    nonisolated private static func parseEndpoint(_ raw: String) -> (address: String, port: UInt16?) {
        guard let colon = raw.lastIndex(of: ":") else { return (raw, nil) }
        var address = String(raw[..<colon])
        if address.hasPrefix("[") && address.hasSuffix("]") {
            address = String(address.dropFirst().dropLast())
        }
        let portText = String(raw[raw.index(after: colon)...])
        return (address, UInt16(portText))
    }
}
