import SwiftUI

struct InferenceSnapshot: Decodable {
    struct Model: Decodable, Identifiable { let id: String; let label: String }
    struct Braincells: Decodable {
        let remaining: Double
        let used: Double
        let limit: Double
        let purchased: Double
        let resetsAt: String?
        let unlimited: Bool?
    }
    struct Usage: Decodable {
        let inputTokens: Int
        let outputTokens: Int
        let rounds: Int
        let repairs: Int
        let status: String
        let cost: ThreadCost?
    }
    let model: String
    let label: String
    let provider: String
    let selection: String
    let models: [Model]
    let braincells: Braincells?
    let braincellsError: String
    let usage: Usage?
    struct ThreadCost: Decodable { let usd: Double; let partial: Bool; let estimated: Bool? }
    let threadCost: ThreadCost?
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
    @Environment(\.scenePhase) private var scenePhase
    @AppStorage(CostUnit.preference) private var costUnit: CostUnit = .usd
    @ObservedObject private var prices = TezDisplayRate.shared
    private var disabled: Bool { session.snapshot.busy || session.capturePhase != .idle }
    private func cells(_ number: Double) -> String { costUnit.amount(usd: number / CostUnit.cellsPerUSD, rate: prices.rate) }
    var body: some View {
        NavigationStack {
            List {
                Section {
                    Picker("Pixel size", selection: Binding(get: { session.pixelSize }, set: { session.setPixelSize($0) })) {
                        ForEach(1...4, id: \.self) { Text("\($0)×").tag($0) }
                    }.pickerStyle(.segmented).disabled(disabled).accessibilityIdentifier("brain-pixel-size")
                    Picker("Aspect ratio", selection: Binding(get: { session.previewFormat }, set: { session.setPreviewFormat($0) })) {
                        ForEach(PreviewFormat.allCases) { Text($0.rawValue).tag($0) }
                    }.pickerStyle(.segmented).disabled(disabled).accessibilityIdentifier("brain-preview-format")
                } header: { Label("Preview", systemImage: "eye") }
                Section {
                    NavigationLink { WhistlegraphSourceSheet(session: session) } label: {
                        Label("View and edit source", systemImage: "curlybraces")
                    }.disabled(!session.engineReady).accessibilityIdentifier("brain-source")
                    NavigationLink { WhistlegraphPrivacySheet(session: session) } label: {
                        Label("AI & privacy", systemImage: "hand.raised")
                    }.accessibilityIdentifier("brain-privacy")
                    NavigationLink { WhistlegraphDebugLog() } label: { Label("Debug log", systemImage: "list.bullet.rectangle") }
                        .accessibilityIdentifier("brain-debug-log")
                }
                if let inference = session.snapshot.inference {
                    Section {
                        Picker("Model", selection: Binding(get: { inference.selection }, set: { session.command("setModel", text: $0) })) {
                            ForEach(inference.models) { Text($0.label).tag($0.id) }
                        }.disabled(disabled || session.snapshot.handle.isEmpty).accessibilityIdentifier("brain-model")
                        LabeledContent("Service provider", value: inference.provider)
                        if session.snapshot.busy { LabeledContent("Running", value: inference.label) }
                    }
                    Section("Balance") {
                        CostUnitPicker()
                        if let balance = inference.braincells {
                            if balance.unlimited == true {
                                LabeledContent("Allowance", value: "Unlimited")
                            } else {
                                LabeledContent("Daily remaining", value: cells(balance.remaining) + " / " + cells(balance.limit))
                                    .accessibilityIdentifier("brain-balance")
                                ProgressView(value: min(balance.used, balance.limit), total: max(1, balance.limit))
                                    .accessibilityLabel("Daily braincells used").accessibilityValue(cells(balance.used))
                            }
                            LabeledContent("Purchased", value: cells(balance.purchased))
                            if balance.unlimited != true, let reset = balance.resetsAt.flatMap({ ISO8601DateFormatter().date(from: $0) ?? ISO8601DateFormatter.fullPrecision.date(from: $0) }) {
                                LabeledContent("Daily reset", value: reset.formatted(date: .omitted, time: .shortened))
                            }
                        } else if inference.braincellsError.isEmpty {
                            ProgressView("Loading braincells…")
                        } else {
                            Text(inference.braincellsError).foregroundStyle(.secondary)
                        }
                        Button("Refresh") { session.command("refreshBraincells"); Task { await prices.refresh() } }
                        BraincellPurchase(purchase: session.braincells, signedIn: !session.snapshot.handle.isEmpty)
                        #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                        TezosPurchaseButton(session: session, purchase: session.tezosBraincells)
                        #endif
                    }
                    if let usage = inference.usage {
                        Section(session.snapshot.busy ? "This request" : "Last request") {
                            if let cost = usage.cost {
                                LabeledContent("Provider cost") { ThreadCostLabel(cost: cost) }
                            }
                            LabeledContent("Input tokens", value: usage.inputTokens.formatted())
                            LabeledContent("Output tokens", value: usage.outputTokens.formatted())
                            LabeledContent("Inference calls", value: usage.rounds.formatted())
                            LabeledContent("Repairs", value: usage.repairs.formatted())
                        }
                    }
                    Section {
                        Text("Inference costs show provider value in the selected unit. Hosted AC inference charges twice provider cost in braincells; free allowance is used first.")
                            .font(.footnote).foregroundStyle(.secondary)
                    }
                }
            }
            .navigationTitle("Brain")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
        }.presentationDetents([.medium, .large])
            .onAppear { DeviceActionLog.shared.record(.screen, .presented, control: .brain) }
            .onDisappear { DeviceActionLog.shared.record(.screen, .dismissed, control: .brain) }
            .onChange(of: costUnit) { _, value in DeviceActionLog.shared.record(.setting, .succeeded, control: .costUnit, [.format: CostUnit.allCases.firstIndex(of: value) ?? -1]) }
            .task {
                await prices.refresh()
                #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                await session.tezosBraincells.prepare()
                await session.tezosBraincells.refresh(session: session)
                #endif
            }
            .task(id: costUnit) { if costUnit == .tezos { await prices.refresh() } }
            .onChange(of: scenePhase) { _, phase in
                if phase == .active {
                    Task {
                        await prices.refresh()
                        #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                        await session.tezosBraincells.refresh(session: session)
                        #endif
                    }
                }
            }
    }
}

#if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
private struct TezosPurchaseButton: View {
    @ObservedObject var session: WhistlegraphSession
    @ObservedObject var purchase: TezosBraincells
    var body: some View {
        if purchase.available {
            Button { Task { await purchase.buy(session: session) } } label: {
                Label(purchase.busy ? "Checking payment…" : "Buy braincells with tez", systemImage: "arrow.up.right.square")
            }.disabled(purchase.busy || session.snapshot.handle.isEmpty)
                .accessibilityIdentifier("brain-buy-tezos")
        }
        if !purchase.notice.isEmpty { Text(purchase.notice).font(.footnote).foregroundStyle(.secondary) }
    }
}

#endif

private extension ISO8601DateFormatter {
    static var fullPrecision: ISO8601DateFormatter {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter
    }
}

struct ThreadCostLabel: View {
    let cost: InferenceSnapshot.ThreadCost
    @AppStorage(CostUnit.preference) private var unit: CostUnit = .usd
    @ObservedObject private var prices = TezDisplayRate.shared
    private var amount: String { unit.amount(usd: cost.usd, rate: prices.rate, partial: cost.partial) }
    private var value: String {
        if cost.partial && cost.usd == 0 { return "Cost unavailable" }
        if unit == .tezos && prices.rate?.isFresh != true { return "Rate unavailable" }
        return (cost.partial ? "≥ " : "") + (cost.estimated == true || unit == .tezos ? "≈ " : "") + amount
    }
    var body: some View {
        Text(value)
            .font(.caption.monospacedDigit()).foregroundStyle(.secondary)
            .lineLimit(1).fixedSize(horizontal: true, vertical: false)
            .accessibilityLabel("Provider inference value, " + unit.label)
            .accessibilityValue(value)
            .accessibilityIdentifier("thread-cost")
            .task(id: unit) { if unit == .tezos { await prices.refresh() } }
    }
}
