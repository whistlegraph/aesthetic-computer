import SwiftUI

@main
struct NelameshScopeApp: App {
    var body: some Scene {
        WindowGroup("Nelamesh Scope") {
            ScopeView()
                .frame(minWidth: 680, minHeight: 420)
        }
        .windowStyle(.hiddenTitleBar)
        .commands {
            ScopeCommands()
            CommandGroup(after: .newItem) {
                Button("Scan Again") {
                    NotificationCenter.default.post(name: .scopeRescan, object: nil)
                }
                .keyboardShortcut("r")
            }
        }

        Window("About Nelamesh Scope", id: "about") {
            AboutScopeView()
        }
        .windowResizability(.contentSize)
        .windowStyle(.hiddenTitleBar)
    }
}

private struct ScopeCommands: Commands {
    @Environment(\.openWindow) private var openWindow

    var body: some Commands {
        CommandGroup(replacing: .appInfo) {
            Button("About Nelamesh Scope") { openWindow(id: "about") }
        }
    }
}

extension Notification.Name {
    static let scopeRescan = Notification.Name("computer.nela.scope.rescan")
}

private struct ScopeView: View {
    @StateObject private var scanner = LocalServiceScanner()
    @Environment(\.openURL) private var openURL

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            header
            SectionLabel("SOCKET INVENTORY")
            content
            footer
        }
        .padding(10)
        .background(NELA.black.ignoresSafeArea())
        .foregroundStyle(NELA.green)
        .font(.system(size: 12, design: .monospaced))
        .preferredColorScheme(.dark)
        .task { scanner.scan() }
        .onReceive(NotificationCenter.default.publisher(for: .scopeRescan)) { _ in
            scanner.scan()
        }
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 2) {
            HStack(spacing: 8) {
                VStack(alignment: .leading, spacing: 0) {
                    Text("░░░░░░░░░░░░░░░░░░░")
                    Text("NELA COMPUTER CLUB / NELAMESH SCOPE")
                        .fontWeight(.bold)
                    Text("░░░░░░░░░░░░░░░░░░░")
                }
                .foregroundStyle(NELA.cyan)
                .lineLimit(1)
                Spacer(minLength: 8)
                summary("ALL", scanner.services.count)
                summary("TCP", scanner.tcpCount)
                summary("UDP", scanner.udpCount)
                summary("NET", scanner.networkCount)
                Button(scanner.isScanning ? "SCANNING…" : "> RESCAN") { scanner.scan() }
                    .buttonStyle(NELAButtonStyle(inverted: true))
                    .disabled(scanner.isScanning)
                    .keyboardShortcut("r")
            }
            Text("▛▚   ▌   ▞▔▔▚   ▞▔▔▚  /  LOCAL SOCKETS + MESH REACHABILITY")
                .foregroundStyle(NELA.dimCyan)
                .lineLimit(1)
        }
    }

    private func summary(_ label: String, _ value: Int) -> some View {
        VStack(spacing: 0) {
            Text(String(value)).fontWeight(.bold).foregroundStyle(NELA.cyan)
            Text(label).font(.system(size: 9, design: .monospaced)).foregroundStyle(NELA.dimCyan)
        }
        .frame(minWidth: 30)
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("\(label), \(value)")
    }

    @ViewBuilder private var content: some View {
        if scanner.isScanning && scanner.services.isEmpty {
            HStack(spacing: 8) {
                ProgressView().controlSize(.small).tint(NELA.green)
                Text("READING TCP + UDP SOCKET TABLE…")
            }
            .frame(maxWidth: .infinity, minHeight: 140)
        } else if let error = scanner.error {
            VStack(spacing: 4) {
                Text("[ INVENTORY ERROR ]").fontWeight(.bold)
                Text(error).foregroundStyle(NELA.cyan)
            }
            .frame(maxWidth: .infinity, minHeight: 140)
        } else if scanner.services.isEmpty {
            VStack(spacing: 4) {
                Text("[ NO SOCKETS IN SCOPE ]").fontWeight(.bold)
                Text("START A SERVICE, THEN RESCAN.").foregroundStyle(NELA.cyan)
            }
            .frame(maxWidth: .infinity, minHeight: 140)
        } else {
            ScrollView {
                LazyVGrid(columns: [GridItem(.adaptive(minimum: 325), spacing: 6)], spacing: 6) {
                    ForEach(scanner.services) { service in
                        ServiceCell(service: service) {
                            if let url = service.browserURL { openURL(url) }
                        }
                    }
                }
                .padding(1)
            }
            .scrollIndicators(.visible)
        }
    }

    private var footer: some View {
        HStack {
            Text("▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒")
            Text("LOCAL INVENTORY ≠ MESH OFFER")
            Spacer()
            if let date = scanner.lastScanned {
                Text("SCANNED ") + Text(date, style: .time)
            }
        }
        .font(.system(size: 10, design: .monospaced))
        .foregroundStyle(NELA.dimCyan)
        .lineLimit(1)
    }
}

private struct ServiceCell: View {
    let service: LocalService
    let open: () -> Void

    var body: some View {
        HStack(spacing: 6) {
            Text(service.transport)
                .fontWeight(.bold)
                .foregroundStyle(NELA.black)
                .padding(.horizontal, 3)
                .frame(minHeight: 22)
                .background(NELA.green)
            VStack(alignment: .leading, spacing: 1) {
                HStack(spacing: 5) {
                    Text(service.process).fontWeight(.bold)
                    Text("#\(service.pid)").foregroundStyle(NELA.dimCyan)
                    Spacer(minLength: 2)
                    Text(service.reachability.rawValue)
                        .font(.system(size: 9, weight: .bold, design: .monospaced))
                        .foregroundStyle(service.reachability == .loopback ? NELA.green : NELA.cyan)
                }
                .lineLimit(1)
                Text("\(service.likelyProtocol)  \(service.endpoint)")
                    .foregroundStyle(NELA.cyan)
                    .lineLimit(1)
                    .truncationMode(.middle)
            }
            if service.browserURL != nil {
                Button("OPEN") { open() }
                    .buttonStyle(NELAButtonStyle())
                    .accessibilityLabel("Open \(service.process) at \(service.endpoint)")
            }
        }
        .font(.system(size: 11, design: .monospaced))
        .padding(5)
        .frame(maxWidth: .infinity, minHeight: 42, alignment: .leading)
        .background(NELA.black)
        .overlay(Rectangle().stroke(NELA.cyan, lineWidth: 1))
        .accessibilityElement(children: .contain)
    }
}

private struct AboutScopeView: View {
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("░░░░░░░░░░░░░░░░░░░")
            Text("NELA COMPUTER CLUB").fontWeight(.bold)
            Text("░░░░░░░░░░░░░░░░░░░")
            Text("""
                 ▛▚   ▌   ▞▔▔▚   ▞▔▔▚
                 ▌ ▚  ▌  ▞      ▞
                 ▌  ▚ ▌  ▚      ▚
                 ▌   ▚▌   ▚▂▂▞   ▚▂▂▞
                 """)
            .foregroundStyle(NELA.cyan)
            SectionLabel("NELAMESH SCOPE / 0.1.0")
            Text("A compact view of local TCP + UDP services and their network reachability.")
                .fixedSize(horizontal: false, vertical: true)
            Text("LOCAL INVENTORY DOES NOT OFFER A SERVICE TO THE MESH.")
                .foregroundStyle(NELA.cyan)
            Text("MADE FOR NELA.COMPUTER · 2026")
                .foregroundStyle(NELA.dimCyan)
            Button("> CLOSE") { dismiss() }
                .buttonStyle(NELAButtonStyle(inverted: true))
        }
        .font(.system(size: 12, design: .monospaced))
        .foregroundStyle(NELA.green)
        .padding(10)
        .frame(width: 390)
        .background(NELA.black)
        .overlay(Rectangle().stroke(NELA.cyan, lineWidth: 1))
        .preferredColorScheme(.dark)
    }
}

private struct SectionLabel: View {
    let title: String
    init(_ title: String) { self.title = title }
    var body: some View {
        Text(title)
            .font(.system(size: 11, weight: .bold, design: .monospaced))
            .foregroundStyle(NELA.black)
            .padding(.horizontal, 3)
            .padding(.vertical, 2)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(NELA.green)
    }
}

private struct NELAButtonStyle: ButtonStyle {
    var inverted = false
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(size: 11, weight: .bold, design: .monospaced))
            .foregroundStyle(inverted || configuration.isPressed ? NELA.black : NELA.green)
            .padding(.horizontal, 7)
            .frame(minHeight: 28)
            .background(inverted || configuration.isPressed ? NELA.green : NELA.black)
            .overlay(Rectangle().stroke(NELA.cyan, lineWidth: 1))
            .contentShape(Rectangle())
    }
}

private enum NELA {
    static let black = Color(red: 0, green: 0, blue: 0)
    static let green = Color(red: 0, green: 208 / 255, blue: 128 / 255)
    static let cyan = Color(red: 0, green: 196 / 255, blue: 253 / 255)
    static let dimCyan = Color(red: 0, green: 104 / 255, blue: 136 / 255)
}
