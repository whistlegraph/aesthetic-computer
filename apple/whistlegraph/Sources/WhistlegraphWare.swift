import SwiftUI

enum Ware: String, CaseIterable, Identifiable {
    case piece, roblox
    var id: String { rawValue }
    var title: String { self == .piece ? "Aesthetic.Computer Piece" : "Roblox Room" }
}

struct RobloxRoomSnapshot: Decodable {
    var selected: String?
    var drawing: Bool?
    var notice: String?
}

struct WarePicker: View {
    @ObservedObject var session: WhistlegraphSession
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
            HStack(spacing: 7) {
                if session.snapshot.wareID == "piece" {
                    (Text("Aesthetic") + Text(".").foregroundColor(Color(red: 180/255, green: 72/255, blue: 135/255)) + Text("Computer Piece"))
                } else { Text("Roblox Room") }
                Image(systemName: "chevron.down").font(.caption.bold())
            }
            .font(.custom("ComicRelief-Regular", size: 18, relativeTo: .body))
            .frame(minHeight: 44)
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
