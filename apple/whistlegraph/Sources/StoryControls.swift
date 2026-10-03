import SwiftUI

struct StoryExportProgress: View {
    @ObservedObject var narrator: VersionNarrator
    @ObservedObject var exporter: StoryExport
    let cancel: () -> Void
    private var title: String {
        switch exporter.stage {
        case .preparing: return "Preparing video…"
        case .rendering: return "Rendering card \(narrator.index + 1) of \(narrator.count)"
        case .finishingVideo: return "Finishing video…"
        case .narration: return "Adding audio…"
        case .encoding: return "Encoding MP4"
        }
    }
    private var progress: Double? {
        switch exporter.stage {
        case .rendering: return min(1, Double(narrator.index) / Double(max(1, narrator.count)) + narrator.progress / Double(max(1, narrator.count)))
        case .narration, .encoding: return exporter.progress
        default: return nil
        }
    }
    var body: some View {
        VStack(spacing: 12) {
            HStack {
                Text(title).font(.headline).accessibilityIdentifier("story-export-stage")
                Spacer()
                if exporter.stage == .encoding { Text(exporter.progress, format: .percent.precision(.fractionLength(0))).monospacedDigit() }
            }
            if let progress {
                ProgressView(value: progress).tint(.white)
                    .accessibilityLabel(title).accessibilityIdentifier("story-export-bar")
            } else { ProgressView().tint(.white).frame(maxWidth: .infinity) }
            Button("Cancel export", action: cancel).padding(.vertical, 8)
                .accessibilityIdentifier("story-export-cancel")
        }
        .padding(20).foregroundStyle(.white)
        .background(.black.opacity(0.9), in: RoundedRectangle(cornerRadius: 20))
        .padding(20).accessibilityElement(children: .contain).accessibilityIdentifier("story-export-progress")
    }
}

struct StoryControls: View {
    @ObservedObject var narrator: VersionNarrator
    @ObservedObject var exporter: StoryExport
    let export: () -> Void
    let close: () -> Void
    var body: some View {
        VStack(spacing: 14) {
            HStack(spacing: 4) {
                ForEach(0..<narrator.count, id: \.self) { i in
                    GeometryReader { g in
                        Capsule().fill(.white.opacity(0.25))
                            .overlay(alignment: .leading) {
                                Capsule().fill(.white).frame(width: g.size.width * (i < narrator.index ? 1 : i == narrator.index ? narrator.progress : 0))
                            }
                    }.frame(height: 3)
                }
            }.accessibilityElement(children: .ignore).accessibilityLabel("Card \(narrator.index + 1) of \(narrator.count)")
            HStack {
                Button(action: close) { Image(systemName: "xmark").frame(width: 44, height: 44) }
                    .accessibilityLabel("Close version story")
                Spacer()
                Button(action: export) {
                    if exporter.busy { ProgressView().tint(.white).frame(width: 44, height: 44) }
                    else { Image(systemName: "square.and.arrow.up").frame(width: 44, height: 44) }
                }.accessibilityLabel("Export MP4").accessibilityIdentifier("story-export").disabled(exporter.requested)
                    .popover(item: $exporter.movie, arrowEdge: .top) { movie in
                        StoryMovieSheet(url: movie.url).presentationCompactAdaptation(.popover)
                    }
            }
            if exporter.readyURL != nil || exporter.busy {
                Text(exporter.readyURL != nil ? "Video ready" : "Preparing video · \(exporter.completedCards)/\(narrator.count)")
                    .font(.caption).accessibilityIdentifier("story-video-status")
            }
            Spacer()
            HStack(spacing: 32) {
                Button { narrator.previous() } label: { Image(systemName: "backward.end.fill").frame(width: 52, height: 48) }
                    .accessibilityLabel("Previous card").disabled(exporter.requested)
                Button { narrator.setPaused(!narrator.isPaused) } label: {
                    Image(systemName: narrator.isPaused ? "play.fill" : "pause.fill").frame(width: 52, height: 48)
                }.accessibilityLabel(narrator.isPaused ? "Resume story" : "Pause story").accessibilityIdentifier("story-pause").disabled(exporter.requested)
                Button { narrator.next() } label: { Image(systemName: "forward.end.fill").frame(width: 52, height: 48) }
                    .accessibilityLabel("Next card").disabled(exporter.requested)
            }
        }.font(.system(size: 20, weight: .semibold)).foregroundStyle(.white).buttonStyle(.plain)
            .padding(.horizontal, 20).padding(.vertical, 8)
    }
}
