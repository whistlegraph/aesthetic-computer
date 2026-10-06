import AppKit
import SwiftUI

struct PaintingSummary: Decodable, Identifiable {
    let id: String
    let started_at: String
    let paints: Int
    let current: Bool?
    let image: String?
    var date: String {
        let parser = ISO8601DateFormatter()
        parser.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        let parsed = parser.date(from: started_at) ?? ISO8601DateFormatter().date(from: started_at)
        guard let parsed else { return started_at }
        return parsed.formatted(date: .abbreviated, time: .shortened)
    }
}
struct PaintingArchive: Decodable { let paintings: [PaintingSummary] }
struct PaintingStep: Decodable, Identifiable {
    let id: String
    let action: String
    let image: String
    let model: String?
    let seconds: Double?
    let braincells: Int?
    let prompt: String?
    let operation: String?
    let error: String?
    var title: String {
        switch action {
        case "restart", "done", "ready": return "Fresh"
        case "no", "undo": return "No"
        case "error": return "Failed"
        default: return action.capitalized
        }
    }
}
struct PaintingTrack: Decodable {
    let painting: PaintingSummary
    let steps: [PaintingStep]
    let folder: String
}

struct PaintingHistoryView: View {
    @ObservedObject var game: GameStore
    @State private var painting: String?
    @State private var step: String?
    var selected: PaintingStep? { game.paintingTrack?.steps.first { $0.id == step } ?? game.paintingTrack?.steps.last }
    func image(_ path: String) -> some View {
        AsyncImage(url: URL(string: "http://127.0.0.1:8767" + path)) { phase in
            if let image = phase.image { image.resizable().interpolation(.none).aspectRatio(contentMode: .fit) }
            else { Color.clear.overlay(ProgressView().controlSize(.small)) }
        }
    }
    var body: some View {
        VStack(spacing: 12) {
            HStack {
                Text("History").font(.title2)
                Spacer()
                if let track = game.paintingTrack {
                    Button("Reveal Track") { NSWorkspace.shared.open(URL(fileURLWithPath: track.folder)) }.actionPointer()
                }
                Button("Close") { game.showHistory = false }.keyboardShortcut(.cancelAction).actionPointer()
            }
            if let error = game.historyError { Text(error).foregroundStyle(.red) }
            HSplitView {
                List(game.paintings, selection: $painting) { item in
                    HStack(spacing: 8) {
                        if let path = item.image { image(path).frame(width: 48, height: 48) }
                        VStack(alignment: .leading, spacing: 4) {
                            Text(item.date).font(.system(size: 11).monospacedDigit())
                            Text("\(item.paints) paints" + (item.current == true ? " · current" : "")).font(.caption).foregroundStyle(.secondary)
                        }
                    }.padding(.vertical, 4).tag(item.id)
                }.frame(minWidth: 200, idealWidth: 210, maxWidth: 260)
                VStack(spacing: 8) {
                    if let selected {
                        image(selected.image).frame(maxWidth: .infinity, minHeight: 180, maxHeight: 280)
                        if let detail = selected.prompt ?? selected.operation {
                            DisclosureGroup("Move") { Text(detail).font(.system(size: 12)).textSelection(.enabled) }.padding(.horizontal, 10)
                        }
                        if let error = selected.error { Text(error).font(.caption).foregroundStyle(.red).textSelection(.enabled) }
                    }
                    ScrollViewReader { scroll in
                    List(game.paintingTrack?.steps ?? [], selection: $step) { item in
                        HStack(spacing: 8) {
                            image(item.image).frame(width: 42, height: 42)
                            VStack(alignment: .leading, spacing: 3) {
                                Text(item.title + (item.action == "paint" ? " · " + (item.model ?? "") : "")).lineLimit(1)
                                if item.action == "paint" {
                                    Text((item.seconds.map { String(format: "%.2fs · ", $0) } ?? "") + (item.braincells.map { "\($0.formatted()) braincells" } ?? "braincells pending"))
                                        .font(.caption.monospacedDigit()).foregroundStyle(.secondary)
                                }
                            }
                        }.tag(item.id)
                    }.frame(minHeight: 120)
                    .onChange(of: game.paintingTrack?.painting.id) { _, _ in
                        step = game.paintingTrack?.steps.last?.id
                        if let step { scroll.scrollTo(step, anchor: .bottom) }
                    }
                    }
                }.frame(minWidth: 320)
            }
        }.padding(16).frame(minWidth: 610, idealWidth: 720, minHeight: 500, idealHeight: 600)
            .onChange(of: painting) { _, id in if let id { step = nil; game.readTrack(id) } }
            .onChange(of: game.paintings.first?.id, initial: true) { _, id in if painting == nil { painting = id } }
    }
}
