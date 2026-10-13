import SwiftUI
import UniformTypeIdentifiers

enum Ware: String, CaseIterable, Identifiable {
    case piece, roblox, doom, painting
    var id: String { rawValue }
    var title: String { self == .painting ? "PNG Painting" : self == .piece ? "Aesthetic.Computer Piece" : self == .doom ? "Doom WAD" : "Roblox Room" }
    /// The file-extension spelling in the top bar: /code.ac, /code.rbx.
    var fileExtension: String { self == .painting ? "png" : self == .piece ? "ac" : self == .doom ? "wad" : "rbx" }
}

struct RobloxRoomSnapshot: Decodable {
    var selected: String?
    var drawing: Bool?
    var notice: String?
}

struct PaintingSnapshot: Decodable {
    let width: Int
    let height: Int
    var pixelSize: CGSize? {
        guard (16...2048).contains(width), (16...2048).contains(height) else { return nil }
        return CGSize(width: width, height: height)
    }
}

struct WarePicker: View {
    @ObservedObject var session: WhistlegraphSession
    var size: CGFloat = 18
    let beforeSwitch: () -> Void
    var body: some View {
        Menu {
            ForEach(Ware.allCases) { ware in
                Button {
                    beforeSwitch()
                    session.command("setWare", ware: ware.rawValue)
                } label: {
                    if session.snapshot.wareID == ware.rawValue { Label(ware.title, systemImage: "checkmark") }
                    else { Text(ware.title) }
                }.accessibilityIdentifier("ware-" + ware.rawValue)
            }
        } label: {
            // The ware reads as the piece's extension, right after its /code.
            ComicTitle(text: "." + (Ware(rawValue: session.snapshot.wareID) ?? .piece).fileExtension, size: size)
                .opacity(0.75)
        }
        .disabled(!session.engineReady || session.snapshot.busy || session.capturePhase != .idle)
        .accessibilityLabel("Ware, " + (Ware(rawValue: session.snapshot.wareID)?.title ?? Ware.piece.title))
        .accessibilityIdentifier("ware-picker")
    }
}

struct RobloxRoomControls: View {
    @ObservedObject var session: WhistlegraphSession
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Button("Play in Roblox") { session.command("playRoblox") }
                    .buttonStyle(.borderedProminent).accessibilityIdentifier("play-roblox")
                Spacer()
                Menu {
                    Button("Undo", systemImage: "arrow.uturn.backward") { session.command("undoRoom") }
                    Button("Export Roblox place", systemImage: "square.and.arrow.up") { session.command("exportRoom") }
                } label: { Image(systemName: "ellipsis.circle").font(.title2).frame(width: 44, height: 44) }
                .accessibilityLabel("Room actions")
            }
            if let notice = session.snapshot.roblox?.notice, !notice.isEmpty {
                Text(notice).font(.footnote).fixedSize(horizontal: false, vertical: true)
            }
        }.disabled(session.snapshot.busy || session.capturePhase != .idle)
    }
}

struct DoomControls: View {
    @ObservedObject var session: WhistlegraphSession
    var body: some View {
        HStack {
            Spacer()
            Menu {
                Button("Undo", systemImage: "arrow.uturn.backward") { session.command("undoDoom") }
                Button("New empty WAD", systemImage: "plus") { session.command("newDoom") }
                Button("Restart", systemImage: "arrow.clockwise") { session.command("playDoom") }
                Button("Export WAD", systemImage: "square.and.arrow.up") { session.command("exportWad") }
                Button("Publish to MIME", systemImage: "globe") { session.command("publishDoom") }
            } label: { Image(systemName: "ellipsis.circle").font(.title2).frame(width: 44, height: 44) }
            .accessibilityLabel("Doom actions").accessibilityIdentifier("doom-actions")
        }.disabled(session.snapshot.busy || session.capturePhase != .idle)
    }
}

struct PaintingControls: View {
    @ObservedObject var session: WhistlegraphSession
    @State private var importing = false
    var body: some View {
        HStack {
            if let painting = session.snapshot.painting, painting.pixelSize != nil {
                Text("\(painting.width) × \(painting.height) px")
                    .font(.custom("ComicRelief-Regular", size: 18)).opacity(0.75)
                    .accessibilityLabel("\(painting.width) by \(painting.height) pixels")
                    .accessibilityIdentifier("painting-resolution")
            }
            Spacer()
            Menu {
                Button("Undo", systemImage: "arrow.uturn.backward") { session.command("undoPainting") }
                Button("Import PNG", systemImage: "photo") { importing = true }
                Button("Export PNG", systemImage: "square.and.arrow.up") { session.command("exportPng") }
                Button("Publish to MIME", systemImage: "globe") { session.command("publishPainting") }
            } label: { Image(systemName: "ellipsis.circle").font(.title2).frame(width: 44, height: 44) }
            .accessibilityLabel("Painting actions").accessibilityIdentifier("painting-actions")
        }
        .disabled(session.snapshot.busy || session.capturePhase != .idle)
        .fileImporter(isPresented: $importing, allowedContentTypes: [.png]) { result in
            switch result {
            case .success(let url): session.importPainting(url)
            case .failure: session.emitEngine(["kind": "error", "text": "Could not open the PNG."])
            }
        }
    }
}
