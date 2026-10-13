import SwiftUI

/// One fixed-height row. The optional buffer replaces the label, so generation
/// never pushes the preview or version history away as more output arrives.
struct GenerationStatusStrip: View {
    let snapshot: PieceSnapshot
    let foreground: Color
    let stop: () -> Void
    @AppStorage("whistlegraph-generation-buffer") private var showsBuffer = false
    @Environment(\.accessibilityReduceMotion) private var reduced
    @Environment(\.scenePhase) private var scenePhase
    #if MIME_APP
    @Environment(\.mimeLettersActive) private var onstage
    #else
    private let onstage = true
    #endif
    @State private var visible = true
    @State private var began = Date()

    private var status: (step: Int, label: String) {
        let reported = snapshot.progress?.phase ?? ""
        let phase = (reported.isEmpty ? snapshot.phase : reported).lowercased()
        if phase.contains("sav") || phase.contains("commit") || phase.contains("sync") { return (3, "Saving") }
        if phase.contains("check") || phase.contains("evaluat") || phase.contains("review") || phase.contains("validat") { return (2, "Checking") }
        if phase.contains("preview") || phase.contains("render") || phase.contains("compil") { return (2, "Previewing") }
        if phase.contains("queue") || phase.contains("worker") { return (0, "Queued") }
        if phase.contains("wait") || phase.contains("connect") || phase.contains("send") || phase.contains("guides") { return (0, "Starting") }
        if phase.contains("think") { return (1, "Thinking") }
        if phase.contains("paint") { return (1, "Painting") }
        if phase.contains("build") { return (1, "Building") }
        return (1, "Making")
    }
    private var buffer: String {
        let candidates = [snapshot.output, snapshot.progress?.steps.last?.text, snapshot.progress?.said, snapshot.phase]
        let newest = candidates.compactMap { $0 }.first { !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty } ?? status.label
        // Bound layout work even for a large source delta, and never wrap.
        return String(newest.suffix(512)).split(whereSeparator: \.isWhitespace).joined(separator: " ")
    }
    var body: some View {
        HStack(spacing: 8) {
            TimelineView(.animation(minimumInterval: 1 / 20, paused: reduced || scenePhase != .active || !visible || !onstage)) { clock in
                let pulse = reduced ? 1 : 0.6 + 0.4 * (sin(clock.date.timeIntervalSince(began) * 5) + 1) / 2
                HStack(spacing: 3) {
                    ForEach(0..<4) { step in
                        if step > 0 { Image(systemName: "chevron.right").font(.system(size: 7, weight: .medium)).opacity(0.3) }
                        Circle().fill(foreground.opacity(step == status.step ? pulse : step < status.step ? 0.8 : 0.2))
                            .frame(width: 6, height: 6)
                    }
                }
            }.frame(width: 58).accessibilityHidden(true)
            Group {
                if showsBuffer { CodeTicker(output: buffer) }
                else {
                    Text(status.label).font(.custom("ComicRelief-Regular", size: 16))
                        .lineLimit(1).truncationMode(.tail)
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .accessibilityIdentifier("generation-status")
                }
            }.frame(maxWidth: .infinity, alignment: .leading).clipped()
            Button { showsBuffer.toggle() } label: {
                Image(systemName: "chevron.left.forwardslash.chevron.right")
                    .font(.system(size: 15, weight: .medium)).opacity(showsBuffer ? 1 : 0.5)
                    .frame(width: 44, height: 44).contentShape(Rectangle())
            }.buttonStyle(.plain)
                .accessibilityLabel(showsBuffer ? "Hide data buffer" : "Show data buffer")
                .accessibilityValue(status.label).accessibilityIdentifier("generation-buffer-toggle")
            Button { ButtonSounds.play(.stop); stop() } label: {
                Image(systemName: "stop.fill").font(.system(size: 16)).foregroundStyle(Color(red: 1, green: 0.42, blue: 0.43))
                    .frame(width: 44, height: 44).contentShape(Rectangle())
            }.buttonStyle(.plain).accessibilityLabel("Stop generation").accessibilityIdentifier("generation-stop")
        }
        .padding(.leading, 10).frame(height: 44).foregroundStyle(foreground)
        .overlay(alignment: .bottom) { Rectangle().fill(foreground.opacity(0.15)).frame(height: 1).accessibilityHidden(true) }
        .accessibilityElement(children: .contain).accessibilityLabel("Generation: \(status.label)")
        .accessibilityIdentifier("generation-strip")
        .onAppear { visible = true }.onDisappear { visible = false }
        .onChange(of: snapshot.busy) { _, busy in if busy { began = Date() } }
    }
}

/// A live, single-line tail. The scroll follows incoming bytes, but stays still
/// between updates so it doesn't suggest activity when the stream has stalled.
struct CodeTicker: View {
    let output: String
    var body: some View {
        ScrollViewReader { scroll in
            ScrollView(.horizontal) {
                HStack(spacing: 0) {
                    Text(verbatim: output).font(.system(size: 13, design: .monospaced))
                        .lineLimit(1).fixedSize(horizontal: true, vertical: false)
                    Color.clear.frame(width: 1, height: 1).id("tail")
                }
            }.scrollIndicators(.hidden).frame(height: 24).clipped()
                .onAppear { scroll.scrollTo("tail", anchor: .trailing) }
                .onChange(of: output) { _, _ in scroll.scrollTo("tail", anchor: .trailing) }
        }.accessibilityElement(children: .ignore).accessibilityLabel("Data buffer")
            .accessibilityValue(output).accessibilityIdentifier("code-ticker")
    }
}
