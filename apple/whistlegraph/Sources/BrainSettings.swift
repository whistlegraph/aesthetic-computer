import SwiftUI

struct InferenceSnapshot: Decodable {
    struct Model: Decodable, Identifiable { let id: String; let label: String }
    struct Braincells: Decodable {
        let remaining: Double
        let used: Double
        let limit: Double
        let purchased: Double
        let resetsAt: String?
    }
    struct Usage: Decodable {
        let inputTokens: Int
        let outputTokens: Int
        let rounds: Int
        let repairs: Int
        let status: String
    }
    let model: String
    let label: String
    let provider: String
    let selection: String
    let models: [Model]
    let braincells: Braincells?
    let braincellsError: String
    let usage: Usage?
}

struct BrainButton: View {
    @ObservedObject var session: WhistlegraphSession
    let beforeOpening: () -> Void
    @State private var showing = false
    var body: some View {
        Button {
            beforeOpening(); ButtonSounds.play(.pop)
            session.command("refreshBraincells"); showing = true
        } label: {
            Image(systemName: "brain").font(.system(size: 25, weight: .semibold))
                .frame(width: 44, height: 44)
        }
        .accessibilityLabel("Brain settings").accessibilityIdentifier("brain-settings")
        .sheet(isPresented: $showing) { BrainSettings(session: session) }
    }
}

struct BrainSettings: View {
    @ObservedObject var session: WhistlegraphSession
    @Environment(\.dismiss) private var dismiss
    private var disabled: Bool { session.snapshot.busy || session.capturePhase != .idle }
    private func cells(_ number: Double) -> String { number.formatted(.number.precision(.fractionLength(0))) }
    var body: some View {
        NavigationStack {
            List {
                Section {
                    Picker("Pixel size", selection: Binding(get: { session.pixelSize }, set: { session.setPixelSize($0) })) {
                        ForEach(1...4, id: \.self) { Text("\($0)×").tag($0) }
                    }.pickerStyle(.segmented).disabled(disabled).accessibilityIdentifier("brain-pixel-size")
                } header: { Label("Pixel size", systemImage: "eye") }
                if let inference = session.snapshot.inference {
                    Section {
                        Picker("Model", selection: Binding(get: { inference.selection }, set: { session.command("setModel", text: $0) })) {
                            ForEach(inference.models) { Text($0.label).tag($0.id) }
                        }.disabled(disabled || session.snapshot.handle.isEmpty).accessibilityIdentifier("brain-model")
                        LabeledContent("Service provider", value: inference.provider)
                        if session.snapshot.busy { LabeledContent("Running", value: inference.label) }
                    }
                    Section("Braincells") {
                        if let balance = inference.braincells {
                            LabeledContent("Daily remaining", value: cells(balance.remaining) + " / " + cells(balance.limit))
                                .accessibilityIdentifier("brain-balance")
                            ProgressView(value: min(balance.used, balance.limit), total: max(1, balance.limit))
                                .accessibilityLabel("Daily braincells used").accessibilityValue(cells(balance.used))
                            LabeledContent("Purchased", value: cells(balance.purchased))
                            if let reset = balance.resetsAt.flatMap({ ISO8601DateFormatter().date(from: $0) ?? ISO8601DateFormatter.fullPrecision.date(from: $0) }) {
                                LabeledContent("Daily reset", value: reset.formatted(date: .omitted, time: .shortened))
                            }
                        } else if inference.braincellsError.isEmpty {
                            ProgressView("Loading braincells…")
                        } else {
                            Text(inference.braincellsError).foregroundStyle(.secondary)
                        }
                        Button("Refresh") { session.command("refreshBraincells") }
                    }
                    if let usage = inference.usage {
                        Section(session.snapshot.busy ? "This request" : "Last request") {
                            LabeledContent("Input tokens", value: usage.inputTokens.formatted())
                            LabeledContent("Output tokens", value: usage.outputTokens.formatted())
                            LabeledContent("Inference calls", value: usage.rounds.formatted())
                            LabeledContent("Repairs", value: usage.repairs.formatted())
                        }
                    }
                }
            }
            .navigationTitle("Brain")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
        }.presentationDetents([.medium, .large])
    }
}

private extension ISO8601DateFormatter {
    static var fullPrecision: ISO8601DateFormatter {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter
    }
}
