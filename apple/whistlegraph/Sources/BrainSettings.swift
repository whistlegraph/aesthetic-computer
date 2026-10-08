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
                .frame(width: 44, height: 44).contentShape(Rectangle())
        }
        .accessibilityLabel("Braincells").accessibilityIdentifier("brain-settings")
        .sheet(isPresented: $showing) { BrainSettings(session: session) }
    }
}

struct BrainSettings: View {
    @ObservedObject var session: WhistlegraphSession
    @Environment(\.dismiss) private var dismiss
    @Environment(\.scenePhase) private var scenePhase
    var body: some View {
        NavigationStack {
            List {
                Section {
                    if session.snapshot.handle.isEmpty {
                        Label("Sign in for your daily braincells", systemImage: "brain")
                        Button("Sign in") { dismiss(); session.command("signIn") }
                    } else if let balance = session.snapshot.inference?.braincells {
                        BraincellMeter(balance: balance)
                    } else if let message = session.snapshot.inference?.braincellsError, !message.isEmpty {
                        Text(message).foregroundStyle(.secondary)
                        Button("Try again") { session.command("refreshBraincells") }
                    } else {
                        ProgressView("Loading braincells…")
                    }
                }
                if !session.snapshot.handle.isEmpty, session.snapshot.inference?.braincells?.unlimited != true {
                    Section {
                        BraincellPurchase(purchase: session.braincells, signedIn: true)
                    }
                }
                Section {
                    NavigationLink { BrainCanvasSettings(session: session) } label: {
                        Label("Canvas", systemImage: "aspectratio")
                    }.accessibilityIdentifier("brain-canvas")
                    NavigationLink { WhistlegraphPrivacySheet(session: session) } label: {
                        Label("AI & privacy", systemImage: "hand.raised")
                    }.accessibilityIdentifier("brain-privacy")
                    NavigationLink { BrainAdvancedSettings(session: session) } label: {
                        Label("Advanced", systemImage: "slider.horizontal.3")
                    }.accessibilityIdentifier("brain-advanced")
                }
            }
            .tint(.purple)
            .navigationTitle("Brain")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .topBarLeading) {
                    Button { session.command("refreshBraincells") } label: { Image(systemName: "arrow.clockwise") }
                        .accessibilityLabel("Refresh balance")
                }
                ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } }
            }
        }.presentationDetents([.large])
            .onAppear { DeviceActionLog.shared.record(.screen, .presented, control: .brain) }
            .onDisappear { DeviceActionLog.shared.record(.screen, .dismissed, control: .brain) }
            .task { await refreshPurchases() }
            .onChange(of: scenePhase) { _, phase in
                if phase == .active {
                    session.command("refreshBraincells")
                    Task { await refreshPurchases() }
                }
            }
    }
    private func refreshPurchases() async {
        await session.braincells.load()
        await session.braincells.recover()
        #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
        await session.tezosBraincells.prepare()
        await session.tezosBraincells.refresh(session: session)
        #endif
    }
}

private struct BraincellMeter: View {
    let balance: InferenceSnapshot.Braincells
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private var unlimited: Bool { balance.unlimited == true }
    private var total: Double { balance.remaining + balance.purchased }
    private var fraction: Double { unlimited ? 1 : min(1, max(0, balance.remaining / max(1, balance.limit))) }
    private func cells(_ number: Double) -> String { number.formatted(.number.precision(.fractionLength(0))) }
    private var reset: Date? {
        balance.resetsAt.flatMap { ISO8601DateFormatter().date(from: $0) ?? ISO8601DateFormatter.fullPrecision.date(from: $0) }
    }
    var body: some View {
        VStack(spacing: 18) {
            HStack(spacing: 16) {
                Image(systemName: "brain")
                    .font(.system(size: 42, weight: .medium)).foregroundStyle(.purple)
                    .padding(14).background(.purple.opacity(0.1), in: Circle())
                    .accessibilityHidden(true)
                VStack(alignment: .leading, spacing: 2) {
                    Text(unlimited ? "Unlimited" : cells(total))
                        .font(.system(.largeTitle, design: .rounded, weight: .bold))
                        .contentTransition(.numericText()).minimumScaleFactor(0.6).lineLimit(1)
                    Text("braincells").font(.headline).foregroundStyle(.secondary)
                }
                .accessibilityElement(children: .ignore)
                .accessibilityLabel("Braincells available")
                .accessibilityValue(unlimited ? "Unlimited" : cells(total))
                .accessibilityIdentifier("brain-balance")
            }.frame(maxWidth: .infinity, alignment: .leading)
            if !unlimited {
                VStack(spacing: 8) {
                    HStack {
                        Text("Free today")
                        Spacer()
                        Text(cells(balance.remaining) + " / " + cells(balance.limit)).monospacedDigit()
                    }.font(.subheadline)
                    HStack(spacing: 5) {
                        ForEach(0..<10, id: \.self) { cell in
                            GeometryReader { geometry in
                                Capsule().fill(.purple.opacity(0.12))
                                    .overlay(alignment: .leading) {
                                        Capsule().fill(.purple)
                                            .frame(width: geometry.size.width * min(1, max(0, fraction * 10 - Double(cell))))
                                    }
                            }
                        }
                    }.frame(height: 12).accessibilityHidden(true)
                    if let reset {
                        Text("Refills at " + reset.formatted(date: .omitted, time: .shortened))
                            .font(.footnote).foregroundStyle(.secondary)
                    }
                }
                HStack {
                    Label("Saved", systemImage: "sparkles")
                    Spacer()
                    Text(cells(balance.purchased)).monospacedDigit()
                }.font(.subheadline)
                Text("Free braincells go first. Saved braincells never expire.")
                    .font(.footnote).foregroundStyle(.secondary).multilineTextAlignment(.center)
            }
        }
        .frame(maxWidth: .infinity).padding(.vertical, 12)
        .animation(reduceMotion ? nil : .easeOut(duration: 0.35), value: total)
    }
}

private struct BrainCanvasSettings: View {
    @ObservedObject var session: WhistlegraphSession
    private var disabled: Bool { session.snapshot.busy || session.capturePhase != .idle }
    var body: some View {
        List {
            Section("Pixel size") {
                Picker("Pixel size", selection: Binding(get: { session.pixelSize }, set: { session.setPixelSize($0) })) {
                    ForEach(1...4, id: \.self) { Text("\($0)×").tag($0) }
                }.pickerStyle(.segmented).disabled(disabled).accessibilityIdentifier("brain-pixel-size")
            }
            Section("Aspect ratio") {
                Picker("Aspect ratio", selection: Binding(get: { session.previewFormat }, set: { session.setPreviewFormat($0) })) {
                    ForEach(PreviewFormat.allCases) { Text($0.rawValue).tag($0) }
                }.pickerStyle(.segmented).disabled(disabled).accessibilityIdentifier("brain-preview-format")
            }
        }.navigationTitle("Canvas")
    }
}

private struct BrainAdvancedSettings: View {
    @ObservedObject var session: WhistlegraphSession
    @AppStorage(CostUnit.preference) private var costUnit: CostUnit = .usd
    private var disabled: Bool { session.snapshot.busy || session.capturePhase != .idle }
    var body: some View {
        List {
            if let inference = session.snapshot.inference {
                Section("Model") {
                    Picker("Model", selection: Binding(get: { inference.selection }, set: { session.command("setModel", text: $0) })) {
                        ForEach(inference.models) { Text($0.label).tag($0.id) }
                    }.disabled(disabled || session.snapshot.handle.isEmpty).accessibilityIdentifier("brain-model")
                    LabeledContent("Service provider", value: inference.provider)
                    if session.snapshot.busy { LabeledContent("Running", value: inference.label) }
                }
                Section {
                    CostUnitPicker()
                    if let cost = inference.threadCost {
                        LabeledContent("Piece provider cost") { ThreadCostLabel(cost: cost) }
                    }
                    if let usage = inference.usage {
                        if let cost = usage.cost {
                            LabeledContent("Last request provider cost") { ThreadCostLabel(cost: cost) }
                        }
                        LabeledContent("Input tokens", value: usage.inputTokens.formatted())
                        LabeledContent("Output tokens", value: usage.outputTokens.formatted())
                        LabeledContent("Inference calls", value: usage.rounds.formatted())
                        LabeledContent("Repairs", value: usage.repairs.formatted())
                    }
                } header: { Text("Usage") } footer: {
                    Text("Provider costs are diagnostic estimates, not your balance. Hosted AC inference charges twice provider cost in braincells; free allowance is used first.")
                }
            }
            Section {
                NavigationLink { WhistlegraphSourceSheet(session: session) } label: {
                    Label("View and edit source", systemImage: "curlybraces")
                }.disabled(!session.engineReady).accessibilityIdentifier("brain-source")
                NavigationLink { WhistlegraphDebugLog() } label: { Label("Debug log", systemImage: "list.bullet.rectangle") }
                    .accessibilityIdentifier("brain-debug-log")
                Button("Check pending purchases") { Task { await session.braincells.recover() } }
                #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                TezosPurchaseButton(session: session, purchase: session.tezosBraincells)
                #endif
            }
        }
        .navigationTitle("Advanced")
        .onChange(of: costUnit) { _, value in
            DeviceActionLog.shared.record(.setting, .succeeded, control: .costUnit, [.format: CostUnit.allCases.firstIndex(of: value) ?? -1])
        }
        .task(id: costUnit) { if costUnit == .tezos { await TezDisplayRate.shared.refresh() } }
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
