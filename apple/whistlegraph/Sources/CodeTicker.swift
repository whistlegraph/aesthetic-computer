import SwiftUI

/// The provisional version, its stages, and the arriving source share one row.
struct GenerationStatusStrip: View {
    let snapshot: PieceSnapshot
    let foreground: Color
    let stop: () -> Void
    @AppStorage("whistlegraph-generation-stream") private var showsBuffer = true
    @Environment(\.accessibilityReduceMotion) private var reduced
    @Environment(\.scenePhase) private var scenePhase
    #if MIME_APP
    @Environment(\.mimeLettersActive) private var onstage
    #else
    private let onstage = true
    #endif
    @State private var visible = true
    @State private var began = Date()
    @State private var version: Int
    private let ember = Color(red: 1, green: 0.48, blue: 0.25)
    private let symbols = ["hourglass", "pencil.tip", "eye", "square.and.arrow.down"]

    init(snapshot: PieceSnapshot, foreground: Color, stop: @escaping () -> Void) {
        self.snapshot = snapshot; self.foreground = foreground; self.stop = stop
        // Pin the proposed number for the whole turn, including the commit
        // snapshot. Editing an older version still appends after the latest ID.
        let makesVersion = snapshot.attempt?.status == "working" || snapshot.draft != nil
        _version = State(initialValue: makesVersion ? max(snapshot.head, snapshot.versions.map(\.id).max() ?? 0) + 1 : snapshot.head)
    }

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
        let candidates = [snapshot.output, snapshot.progress?.steps.last?.text, snapshot.progress?.said]
        let newest = candidates.compactMap { $0 }.first { !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty } ?? status.label
        // Bound layout work even for a large source delta, and never wrap.
        return String(newest.suffix(512)).split(whereSeparator: \.isWhitespace).joined(separator: " ")
    }
    var body: some View {
        HStack(spacing: 6) {
            TimelineView(.animation(minimumInterval: 1 / 20, paused: reduced || scenePhase != .active || !visible || !onstage)) { clock in
                let pulse = reduced ? 0.8 : 0.55 + 0.35 * (sin(clock.date.timeIntervalSince(began) * 4) + 1) / 2
                HStack(spacing: 8) {
                    Text("v\(version)").font(.custom("ComicRelief-Bold", size: 22))
                        .foregroundStyle(ember).opacity(pulse)
                        .shadow(color: ember.opacity(0.35), radius: 3)
                        .fixedSize().accessibilityLabel("v\(version)")
                        .accessibilityValue("Under construction")
                        .accessibilityIdentifier("generation-version")
                    Button { showsBuffer.toggle() } label: {
                        HStack(spacing: 3) {
                            ForEach(symbols.indices, id: \.self) { step in
                                if step > 0 { Image(systemName: "chevron.right").font(.system(size: 6, weight: .medium)).opacity(0.3) }
                                Image(systemName: symbols[step])
                                    .font(.system(size: 12, weight: step == status.step ? .bold : .regular))
                                    .frame(width: 16)
                                    .foregroundStyle(step == status.step ? ember : foreground)
                                    .opacity(step == status.step ? pulse : step < status.step ? 0.7 : 0.25)
                            }
                        }.frame(height: 44).contentShape(Rectangle())
                    }.buttonStyle(.plain)
                        .accessibilityLabel("Generation stages").accessibilityValue(status.label)
                        .accessibilityHint(showsBuffer ? "Hide data buffer" : "Show data buffer")
                        .accessibilityIdentifier("generation-status")
                }
            }.fixedSize(horizontal: true, vertical: false)
            Group {
                if showsBuffer { CodeTicker(output: buffer, active: visible && onstage) }
                else {
                    Text(status.label).font(.custom("ComicRelief-Regular", size: 16))
                        .lineLimit(1).truncationMode(.tail)
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
            }.frame(maxWidth: .infinity, alignment: .leading).clipped()
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
    var active = true
    @Environment(\.accessibilityReduceMotion) private var reduced
    @Environment(\.scenePhase) private var scenePhase
    @State private var stream = ""
    @State private var from = 0
    @State private var arrived = Date()
    @State private var duration = 0.0
    @State private var moving = false

    private func displayed(at date: Date) -> String {
        guard moving, !reduced, duration > 0 else { return String(stream.suffix(512)) }
        let progress = min(1, max(0, date.timeIntervalSince(arrived) / duration))
        let count = from + Int(Double(stream.count - from) * progress)
        return String(stream.prefix(count).suffix(512))
    }

    private func receive() async {
        let previous = Array(displayed(at: .now)), next = Array(output)
        // Join consecutive tails at their real overlap. Replacement/status text
        // begins a new line instead of replaying old source to imply progress.
        var overlap = min(previous.count, next.count)
        while overlap > 0 && !previous.suffix(overlap).elementsEqual(next.prefix(overlap)) { overlap -= 1 }
        stream = overlap > 0 ? String(previous) + String(next.dropFirst(overlap)) : output
        from = overlap > 0 ? previous.count : 0
        duration = min(1.2, max(0.15, Double(stream.count - from) / 180))
        arrived = .now; moving = !reduced && active && scenePhase == .active
        guard moving else { return }
        do { try await Task.sleep(for: .seconds(duration)) } catch { return }
        moving = false
    }

    var body: some View {
        GeometryReader { geometry in
            TimelineView(.animation(minimumInterval: 1 / 30, paused: reduced || !active || scenePhase != .active || !moving)) { clock in
                Text(verbatim: displayed(at: clock.date)).font(.system(size: 13, design: .monospaced))
                    .lineLimit(1).fixedSize(horizontal: true, vertical: false)
                    .frame(width: geometry.size.width, height: 24, alignment: .trailing)
            }
            .accessibilityRepresentation {
                Text("Data buffer").frame(width: geometry.size.width, height: 24)
                    .accessibilityValue(output).accessibilityIdentifier("code-ticker")
            }
        }.frame(height: 24).clipped()
            .mask(LinearGradient(stops: [.init(color: .clear, location: 0), .init(color: .black, location: 0.12)], startPoint: .leading, endPoint: .trailing))
            .task(id: output) { await receive() }
    }
}
