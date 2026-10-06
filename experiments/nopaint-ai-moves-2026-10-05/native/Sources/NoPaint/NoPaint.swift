import AppKit
import SwiftUI
import UniformTypeIdentifiers

struct Model: Decodable, Identifiable {
    let id: String
    let name: String
    let location: String
    let model: String
    let available: Bool
    let braincells: Int?
    var paintPrice: Int? { id.hasPrefix("ac-") ? braincells : 0 }
}
struct Candidate: Decodable {
    let id: Int
    let url: String
    let seconds: Double
    let engine: String
}
struct Account: Decodable {
    let connected: Bool
    let working: Bool?
    let handle: String?
    let remaining: Int?
    let purchased: Int?
    let error: String?
    let error_code: String?
    let stale: Bool?
    let reconnecting: Bool?
    let remote_status: String?
    let remote_service: CloudService?
}
struct CloudService: Decodable {
    let available: Bool
    let code: String
    let message: String
    let detail: String?
    let balance_usd: Double?
    let minimum_balance_usd: Double?
    let checked_at: String?
}
struct Trace: Decodable {
    struct Frame: Decodable { let url: String? }
    let latest: Frame?
}
struct GenerationCost: Decodable {
    let braincells: Int?
    let status: String
}
struct MoveQuote: Decodable {
    let id: String
    let engine: String
    let braincells: Int?
    let estimated_seconds: Double?
}
struct Publication: Decodable {
    let busy: Bool
    let pending: Bool?
    let error: String?
    let code: String?
    let url: String?
}
struct UpscaleState: Decodable {
    let id: String?
    let busy: Bool
    let progress: Double?
    let elapsed: Double?
    let url: String?
    let error: String?
}
struct GameState: Decodable {
    let revision: Int
    let ready: Bool
    let busy: Bool
    let error: String?
    let accepted: String
    let before: String?
    let quote: MoveQuote?
    let mask: String?
    let publication: Publication?
    let upscale: UpscaleState?
    let can_undo: Bool
    let candidate: Candidate?
    let strength: Double
    let elapsed: Double?
    let selection: String?
    let cost: GenerationCost?
    let engine: String
    let generation: Int?
    let can_reject: Bool
    let models: [Model]
    let account: Account
    let trace: Trace
}
struct RemoteModel: Decodable, Identifiable {
    let id: String
    let name: String
    let description: String
    let resolutions: [String]
    let provider_previews: Bool
    let url: String
}
struct Catalog: Decodable { let models: [RemoteModel] }
struct StatusIssue {
    let title: String
    let detail: String
    let recovery: String
}

@MainActor final class GameStore: ObservableObject {
    @Published var state: GameState?
    @Published var hint = ""
    @Published var image: NSImage?
    @Published var imagePath = ""
    @Published var imageSequence = ""
    @Published var imageImmediate = true
    @Published var imageBlendBoundary = false
    @Published var imageSettled = false
    @Published var sending = false
    @Published var peeking = false
    @Published var noHovered = false
    @Published var connectionError: String?
    @Published var actionError: String?
    @Published var models: [RemoteModel] = []
    @Published var catalogError: String?
    @Published var catalogLoading = false
    @Published var prices = ""
    @Published var pricingID = ""
    @Published var showModels = false
    @Published var showHistory = false
    @Published var paintings: [PaintingSummary] = []
    @Published var paintingTrack: PaintingTrack?
    @Published var historyError: String?
    private var historyRequest = UUID()
    private var trackRequest = UUID()
    @Published var dragMode = "zoom"
    @Published var brushSize: CGFloat = 16
    @Published var marking = false
    @Published var fitVersion = 0
    @Published var upscalePending = false
    private let base = URL(string: "http://127.0.0.1:8767")!
    private let session = URLSession(configuration: .ephemeral)
    private var loadedImages = [String: NSImage]()
    private var imageTask: Task<Void, Never>?
    private var desiredImage = ""
    private var desiredSequence = ""
    private var pollTask: Task<Void, Never>?
    private var started: Date?
    private var keyMonitor: Any?
    private var launchedBackend = false
    private var lastAccountRefresh = Date.distantPast
    private var savePanel: NSSavePanel?
    private var upscaleID: String?
    private var upscaleDestination: URL?

    var publishing: Bool { state?.publication?.busy == true }
    var pendingDone: Bool { state?.publication?.pending == true }
    var upscaling: Bool { upscalePending || state?.upscale?.busy == true }
    var busy: Bool { sending || state?.busy == true || publishing || upscaling }
    var canPaint: Bool { state?.accepted == imagePath && imageSettled && state?.quote != nil && !busy && !pendingDone && !peeking && !marking && state?.error == nil && connectionError == nil }
    var canNo: Bool { state?.can_reject == true && !pendingDone && !sending && !peeking && !marking && !upscaling && connectionError == nil }
    var canChange: Bool { state != nil && !busy && !pendingDone && !marking && connectionError == nil }
    var canFresh: Bool { state != nil && !busy && !marking && connectionError == nil }
    var canDone: Bool { state?.account.connected == true && state?.account.stale != true && !busy && !marking && connectionError == nil }
    var cloudUnavailable: Bool { state?.account.remote_service?.available == false || state?.account.remote_status != nil }
    var statusIssue: StatusIssue? {
        if let connectionError { return StatusIssue(title: "Connecting…", detail: connectionError, recovery: "connection") }
        if let error = state?.publication?.error { return StatusIssue(title: "Done failed", detail: error, recovery: "done") }
        if let error = state?.error { return StatusIssue(title: "Generation failed", detail: error, recovery: "generation") }
        if let actionError { return StatusIssue(title: "Action failed", detail: actionError, recovery: "dismiss") }
        if let error = state?.account.error {
            return StatusIssue(title: state?.account.reconnecting == true ? "AC offline" : "AC sign-in needed", detail: error, recovery: "account")
        }
        return nil
    }
    func recoverStatus() {
        guard let issue = statusIssue else { return }
        switch issue.recovery {
        case "account": account(state?.account.connected == true || state?.account.reconnecting == true ? "refresh" : "connect")
        case "generation": act("retry")
        case "done": done()
        case "connection": launchedBackend = false
        default: actionError = nil
        }
    }

    func start() {
        guard pollTask == nil else { return }
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp]) { [weak self] event in
            guard let self, NSApp.keyWindow?.identifier?.rawValue == "NoPaintMain",
                  !self.showModels, !self.showHistory, !(NSApp.keyWindow?.firstResponder is NSTextView),
                  event.modifierFlags.intersection([.command,.control,.option]).isEmpty else { return event }
            let key = event.charactersIgnoringModifiers?.lowercased()
            if key == " " { self.peek(event.type == .keyDown); return nil }
            guard event.type == .keyDown, !event.isARepeat else { return event }
            switch key {
            case "n": if self.canNo { self.act("no") }; return nil
            case "p": if self.canPaint { self.act("paint") }; return nil
            case "u": if self.canChange && self.state?.can_undo == true { self.act("undo") }; return nil
            default: return event
            }
        }
        NotificationCenter.default.addObserver(forName: NSWindow.didResignKeyNotification, object: nil, queue: .main) { [weak self] _ in
            Task { @MainActor in self?.peek(false) }
        }
        pollTask = Task {
            while !Task.isCancelled {
                if !sending {
                    do {
                        let next: GameState = try await request("/api/state")
                        if !sending && next.revision >= (state?.revision ?? -1) { receive(next) }
                        if (next.account.connected || next.account.reconnecting == true) && next.account.working != true && Date().timeIntervalSince(lastAccountRefresh) > 30 {
                            lastAccountRefresh = Date(); account("refresh")
                        }
                    } catch {
                        connectionError = "Connecting…"
                        if !launchedBackend { launchBackend() }
                    }
                }
                try? await Task.sleep(for: .milliseconds(250))
            }
        }
    }

    func request<T: Decodable>(_ path: String, body: [String: Any]? = nil) async throws -> T {
        var request = URLRequest(url: base.appendingPathComponent(String(path.dropFirst()).components(separatedBy: "?")[0]))
        if let url = URL(string: path, relativeTo: base) { request.url = url }
        request.timeoutInterval = 15
        if let body {
            request.httpMethod = "POST"
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try JSONSerialization.data(withJSONObject: body)
        }
        let (data, response) = try await session.data(for: request)
        guard let response = response as? HTTPURLResponse, response.statusCode == 200 else {
            let message = (try? JSONSerialization.jsonObject(with: data) as? [String: Any])?["error"] as? String
            throw NSError(domain: "NoPaint", code: 1, userInfo: [NSLocalizedDescriptionKey: message ?? "Server unavailable"])
        }
        return try JSONDecoder().decode(T.self, from: data)
    }

    private func receive(_ next: GameState) {
        if next.accepted != state?.accepted || next.engine != state?.engine || next.generation != state?.generation { noHovered = false }
        if next.quote != nil && next.quote?.id != state?.quote?.id { actionError = nil }
        if next.publication?.code != nil && next.publication?.code != state?.publication?.code { fitVersion += 1 }
        if next.busy && (started == nil || state?.generation != next.generation) {
            started = Date().addingTimeInterval(-(next.elapsed ?? 0))
        } else if !next.busy { started = nil }
        state = next; connectionError = nil
        loadImage()
        finishUpscale(next.upscale)
    }

    func generationStatus(_ date: Date) -> [String] {
        if publishing { return ["Publishing…"] }
        if upscaling { return [String(format: "%.1fs", state?.upscale?.elapsed ?? 0), "Upscaling · 0 braincells"] }
        if !busy, let state, state.quote == nil {
            if state.error != nil { return ["Retry", "Generation failed"] }
            if state.engine.hasPrefix("ac-") && !state.account.connected { return ["Sign in", "to use Braincells"] }
            if state.engine.hasPrefix("ac-"), state.models.first(where: { $0.id == state.engine })?.braincells == nil { return ["Not enabled", "Choose a model"] }
            return ["Unavailable", "Choose a model below"]
        }
        let amount = state?.quote?.braincells ?? state?.cost?.braincells
        return [generationTime(date), amount.map(Self.braincellCost) ?? "braincells pending"]
    }

    // Only AC cloud models read the hint; local engines paint without words.
    var hintable: Bool { state?.engine.hasPrefix("ac-") == true }

    // AC sells 1,000,000 braincells for $5, so the dollar figure is that pack's rate.
    static let braincellsPerUSD = 200_000.0
    static func braincells(_ amount: Int) -> String { "🧠 " + amount.formatted(.number.notation(.compactName)) }
    static func braincellCost(_ amount: Int) -> String {
        guard amount > 0 else { return "free" }
        let usd = Double(amount) / braincellsPerUSD
        return braincells(amount) + " · " + String(format: usd < 0.01 ? "$%.4f" : "$%.2f", usd)
    }

    func generationTime(_ date: Date) -> String {
        busy ? String(format: "%.1fs", max(0, date.timeIntervalSince(started ?? date)))
            : state?.quote?.estimated_seconds.map { String(format: "~%.1fs", $0) } ?? "— s"
    }
    var braincellBalance: Int { max(0, (state?.account.remaining ?? 0) + (state?.account.purchased ?? 0)) }
    func paintsLeft(_ price: Int?) -> String {
        guard let price, price >= 0 else { return "—" }
        if price == 0 { return "∞" }
        guard state?.account.connected == true else { return "—" }
        return (state?.account.stale == true ? "~" : "") + (braincellBalance / price).formatted()
    }

    func peek(_ value: Bool) { peeking = value; loadImage() }
    func hoverNo(_ value: Bool) {
        let next = value && canNo
        guard next != noHovered else { return }
        noHovered = next; loadImage(animatePeek: true)
    }
    func beginMask() { marking = true; loadImage() }
    func endMarking() { marking = false; loadImage() }
    func finishCrop(_ box: [Int], base: String) {
        marking = false; fitVersion += 1
        act("crop", extra: ["box": box, "accepted": base])
    }
    func finishMask(_ bits: String, base: String) {
        marking = false
        act("mask", extra: ["mask": bits, "accepted": base])
    }
    func clearMask() {
        guard let state else { return }
        act("mask", extra: ["mask": NSNull(), "accepted": state.accepted])
    }
    func fresh() {
        guard canFresh else { return }
        fitVersion += 1
        act("restart", extra: ["start": "noise"])
    }
    func done() {
        guard canDone, let state else { return }
        sending = true; actionError = nil
        Task {
            defer { sending = false }
            do {
                let next: GameState = try await request("/api/done", body: ["revision": state.revision, "accepted": state.accepted])
                receive(next)
            } catch { actionError = error.localizedDescription }
        }
    }

    private func loadImage(animatePeek: Bool = false) {
        guard let state else { return }
        let previewing = peeking || noHovered
        let previous = noHovered && state.busy ? state.accepted : (state.before ?? state.accepted)
        let path = previewing ? previous : (marking || publishing || pendingDone) ? state.accepted : state.busy ? (state.trace.latest?.url ?? state.accepted) : state.accepted
        let sequence = previewing ? "before:\(path)" : (state.busy || state.candidate != nil) ? "move:\(state.generation ?? -1)" : "canvas:\(state.accepted)"
        let immediate = !animatePeek && (previewing || marking || (!state.busy && state.candidate == nil) || (state.busy && state.trace.latest?.url == nil))
        guard path != desiredImage || sequence != desiredSequence else { return }
        desiredImage = path; desiredSequence = sequence; imageTask?.cancel()
        func display(_ next: NSImage) {
            imageSettled = false; imageSequence = sequence; imageImmediate = immediate; imageBlendBoundary = animatePeek; image = next; imagePath = path
        }
        if let cached = loadedImages[path] { display(cached); return }
        imageTask = Task {
            do {
                guard path.hasPrefix("/image/"), path.hasSuffix(".png") else { return }
                let (data, response) = try await session.data(from: URL(string: path, relativeTo: base)!)
                guard !Task.isCancelled, desiredImage == path, desiredSequence == sequence, (response as? HTTPURLResponse)?.statusCode == 200,
                      let next = NSImage(data: data) else { return }
                if loadedImages.count > 32 { loadedImages = loadedImages.filter { $0.key == self.state?.accepted || $0.key == self.state?.before } }
                loadedImages[path] = next; display(next)
            } catch { if !Task.isCancelled { desiredImage = "" } }
        }
    }

    func act(_ action: String, extra: [String: Any] = [:]) {
        guard let state, !sending else { return }
        sending = true; started = Date(); actionError = nil
        var body: [String: Any] = ["action": action, "revision": state.revision]
        if let candidate = state.candidate { body["candidate"] = candidate.id }
        if let generation = state.generation { body["generation"] = generation }
        if action == "paint", let quote = state.quote { body["quote"] = quote.id }
        if action == "paint", hintable, !hint.trimmingCharacters(in: .whitespaces).isEmpty { body["hint"] = hint }
        body.merge(extra) { _, new in new }
        Task {
            defer { sending = false }
            do { let next: GameState = try await request("/api/action", body: body); receive(next) }
            catch { actionError = error.localizedDescription }
        }
    }

    func account(_ action: String) {
        Task {
            do { let next: GameState = try await request("/api/account", body: ["action": action]); if next.revision >= (state?.revision ?? -1) { receive(next) } }
            catch { actionError = error.localizedDescription }
        }
    }

    func browse() {
        showModels = true
        guard !catalogLoading else { return }
        catalogLoading = true; catalogError = nil
        Task {
            defer { catalogLoading = false }
            do { let result: Catalog = try await request("/api/models"); models = result.models }
            catch { catalogError = error.localizedDescription }
        }
    }

    func history() {
        showHistory = true; historyError = nil; paintingTrack = nil; paintings = []
        let requestID = UUID(); historyRequest = requestID; trackRequest = UUID()
        Task {
            do {
                let result: PaintingArchive = try await request("/api/tracks")
                guard historyRequest == requestID else { return }
                paintings = result.paintings
            } catch { if historyRequest == requestID { historyError = error.localizedDescription } }
        }
    }

    func readTrack(_ id: String) {
        paintingTrack = nil; historyError = nil
        let requestID = UUID(); trackRequest = requestID
        Task {
            do {
                let result: PaintingTrack = try await request("/api/track?id=" + id)
                guard trackRequest == requestID else { return }
                paintingTrack = result
            } catch { if trackRequest == requestID { historyError = error.localizedDescription } }
        }
    }

    func fetchPrices(_ model: RemoteModel) {
        pricingID = model.id; prices = "Loading prices…"
        Task {
            struct Endpoint: Decodable {
                struct Price: Decodable { let billable: String; let unit: String; let cost_usd: Double; let variant: String? }
                let provider_name: String
                let pricing: [Price]
            }
            struct Pricing: Decodable { let endpoints: [Endpoint] }
            do {
                let escaped = model.id.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed)!
                let result: Pricing = try await request("/api/models?model="+escaped)
                guard pricingID == model.id else { return }
                prices = result.endpoints.map { endpoint in
                    endpoint.provider_name + ": " + endpoint.pricing.map { price in
                        let amount = price.unit == "token" ? price.cost_usd * 1_000_000 : price.cost_usd
                        return String(format: "$%.4g", amount) + "/" + (price.unit == "token" ? "1M tokens" : price.unit) + " " + price.billable.replacingOccurrences(of: "_", with: " ") + (price.variant.map { " (\($0))" } ?? "")
                    }.joined(separator: "; ")
                }.joined(separator: "\n")
                if prices.isEmpty { prices = "No price published" }
            } catch { if pricingID == model.id { prices = error.localizedDescription } }
        }
    }

    func save() {
        guard savePanel == nil, let accepted = state?.accepted, let source = URL(string: accepted, relativeTo: base) else { return }
        let date = DateFormatter(); date.locale = Locale(identifier: "en_US_POSIX"); date.dateFormat = "yyyy-MM-dd_HH-mm-ss-SSS"
        let panel = NSSavePanel(); panel.allowedContentTypes = [.png]; panel.nameFieldStringValue = "nopaint-\(date.string(from: Date())).png"
        savePanel = panel
        panel.begin { [weak self] result in
            Task { @MainActor in
                guard let self else { return }
                self.savePanel = nil
                guard result == .OK, let destination = panel.url else { return }
                do {
                    let (data, response) = try await self.session.data(from: source)
                    guard (response as? HTTPURLResponse)?.statusCode == 200, NSImage(data: data) != nil else { throw URLError(.badServerResponse) }
                    try data.write(to: destination, options: .atomic)
                } catch { self.actionError = error.localizedDescription }
            }
        }
    }

    func saveUpscaled(_ scale: Int) {
        guard canChange, savePanel == nil, let accepted = state?.accepted else { return }
        let date = DateFormatter(); date.locale = Locale(identifier: "en_US_POSIX"); date.dateFormat = "yyyy-MM-dd_HH-mm-ss-SSS"
        let panel = NSSavePanel(); panel.allowedContentTypes = [.png]
        panel.nameFieldStringValue = "nopaint-\(date.string(from: Date()))-\(scale)x.png"
        panel.message = "Real-ESRGAN · Local · 0 Braincells\n256 × 256 → \(256 * scale) × \(256 * scale)"
        panel.prompt = "Upscale & Save"
        savePanel = panel
        panel.begin { [weak self] result in
            Task { @MainActor in
                guard let self else { return }
                self.savePanel = nil
                guard result == .OK, let destination = panel.url else { return }
                self.upscalePending = true; self.actionError = nil
                do {
                    let next: GameState = try await self.request("/api/upscale", body: ["accepted": accepted, "scale": scale])
                    guard let id = next.upscale?.id else { throw URLError(.badServerResponse) }
                    self.upscaleID = id; self.upscaleDestination = destination
                    if next.revision >= (self.state?.revision ?? -1) { self.receive(next) }
                    else { self.finishUpscale(self.state?.upscale) }
                } catch { self.upscalePending = false; self.actionError = error.localizedDescription }
            }
        }
    }

    private func finishUpscale(_ result: UpscaleState?) {
        guard let id = upscaleID, let destination = upscaleDestination, result?.busy != true else { return }
        upscaleID = nil; upscaleDestination = nil
        guard result?.id == id, let path = result?.url, let source = URL(string: path, relativeTo: base) else {
            upscalePending = false
            actionError = result?.error ?? "Upscale interrupted. Try Save Upscaled again."
            return
        }
        Task {
            defer { upscalePending = false }
            do {
                let (data, response) = try await session.data(from: source)
                guard (response as? HTTPURLResponse)?.statusCode == 200, NSImage(data: data) != nil else { throw URLError(.badServerResponse) }
                try data.write(to: destination, options: .atomic)
                NSWorkspace.shared.activateFileViewerSelecting([destination])
            } catch { actionError = error.localizedDescription }
        }
    }

    private func launchBackend() {
        launchedBackend = true
        guard let path = Bundle.main.object(forInfoDictionaryKey: "NoPaintBackend") as? String else {
            connectionError = "Start the No Paint server at 127.0.0.1:8767"; return
        }
        let process = Process(); process.executableURL = URL(fileURLWithPath: "/bin/zsh")
        process.arguments = [path]
        process.standardOutput = FileHandle.nullDevice; process.standardError = FileHandle.nullDevice
        do { try process.run() } catch { connectionError = "Could not start No Paint" }
    }
}

struct PaintButton: ButtonStyle {
    let dark: Bool
    var height: CGFloat = 76
    var fontSize: CGFloat = 32
    @Environment(\.isEnabled) private var enabled
    @State private var hovering = false
    func makeBody(configuration: Configuration) -> some View {
        configuration.label.font(.system(size: fontSize, weight: .semibold)).frame(maxWidth: .infinity).frame(height: height)
            .background(dark ? Color(nsColor: NoPaintPalette.ink) : Color.clear)
            .foregroundStyle(dark ? Color(nsColor: NoPaintPalette.background) : Color.primary)
            .overlay(Color.primary.opacity(hovering && enabled ? 0.08 : 0).allowsHitTesting(false))
            .overlay(Rectangle().strokeBorder(Color.primary.opacity(0.5), lineWidth: 1).allowsHitTesting(false))
            .opacity(enabled ? (configuration.isPressed ? 0.7 : 1) : (dark ? 0.75 : 0.35))
            .contentShape(Rectangle())
            .onHover { hovering = $0 }
            .actionPointer()
    }
}
struct ContentView: View {
    @ObservedObject var game: GameStore
    func status(_ date: Date) -> StatusBand.Content {
        let model = game.state?.models.first { $0.id == game.state?.engine }
        var modelFields = [StatusBand.Field]()
        if game.upscaling {
            modelFields.append(.init(text: "Real-ESRGAN", size: 13, weight: .semibold))
            modelFields.append(.init(text: "\(Int((game.state?.upscale?.progress ?? 0) * 100))%", size: 11, numeric: true))
        } else {
            if game.state?.selection == "random" { modelFields.append(.init(text: "Random", size: 11)) }
            modelFields.append(.init(text: model?.name ?? "Model", size: 13, weight: .semibold))
            modelFields.append(.init(text: game.generationTime(date), size: 11, numeric: true))
        }
        let account = game.state?.account
        let identity: [StatusBand.Field]
        if account?.connected == true {
            let balance = (account?.remaining ?? 0) + (account?.purchased ?? 0)
            identity = [.init(text: account?.handle ?? "", weight: .medium), .init(text: (account?.stale == true ? "~" : "") + GameStore.braincells(balance), numeric: true)]
        } else { identity = [.init(text: account?.reconnecting == true ? "Reconnecting…" : account?.working == true ? "Signing in…" : "Sign in", weight: .medium)] }
        let serviceText = game.statusIssue?.title ?? (game.cloudUnavailable ? "AC cloud unavailable" : nil)
        let service: [StatusBand.Field] = serviceText.map { [.init(text: $0, size: 11, weight: .medium)] } ?? []
        let price = game.state?.quote?.braincells ?? model?.paintPrice
        let capacity: [StatusBand.Field] = price.map { [.init(text: "×\(game.paintsLeft($0))", size: 11, numeric: true)] } ?? []
        let details = game.generationStatus(date)
        // The bar speaks up only when Paint can't run or something else is working.
        let move: [StatusBand.Field] = details.first == game.generationTime(date) ? []
            : [.init(text: details.joined(separator: " "), size: 11, weight: .medium)]
        return StatusBand.Content(groups: [modelFields, move, service, identity, capacity], context: game.upscaling ? "Local · 0 Braincells" : model?.location)
    }
    func canvasSide(_ size: CGSize, status: StatusBand.Content, compact: Bool) -> CGFloat {
        let reserved: CGFloat = compact ? 52 : 76
        return max(1, floor(min(size.width, size.height - reserved - StatusBand.lineHeight)))
    }
    var body: some View {
        TimelineView(.periodic(from: .now, by: 0.1)) { time in
            GeometryReader { geometry in
                let compact = geometry.size.height < 500
                let label = status(time.date)
                let side = canvasSide(geometry.size, status: label, compact: compact)
                let bandHeight = StatusBand.lineHeight
                let buttonHeight = max(1, geometry.size.height - side - bandHeight)
                let buttonFont = min(side / 7, max(compact ? 24 : 32, buttonHeight * 0.42))
                VStack(spacing: 0) {
                    ZStack(alignment: .bottom) {
                        PixelCanvas(game: game).frame(width: side, height: side)
                        if game.busy && game.connectionError == nil {
                            ProgressView(value: game.upscaling ? (game.state?.upscale?.progress ?? 0) : nil).progressViewStyle(.linear).tint(Color(red: 0.85, green: 0.24, blue: 0.44)).frame(width: side, height: 4).accessibilityLabel(game.upscaling ? "Upscaling painting" : game.publishing ? "Publishing painting" : "Generating image")
                        }
                    }
                    HStack(spacing: 0) {
                        Button { game.act("no") } label: {
                            Text("No").frame(width: side / 2, height: buttonHeight).contentShape(Rectangle())
                        }.buttonStyle(PaintButton(dark: false, height: buttonHeight, fontSize: buttonFont)).disabled(!game.canNo)
                            .onHover { game.hoverNo($0) }
                            .help("Hover to preview going back. Click to go back one move or cancel an unfinished move. (N)")
                        Button { game.act("paint") } label: {
                            Text("Paint").frame(width: side / 2, height: buttonHeight).contentShape(Rectangle())
                        }.buttonStyle(PaintButton(dark: true, height: buttonHeight, fontSize: buttonFont)).disabled(!game.canPaint || game.pendingDone)
                            .accessibilityLabel("Paint: confirm one generation").accessibilityValue(game.generationStatus(time.date).joined(separator: ", "))
                            .help(game.busy ? "Generating this confirmed move" : game.state?.quote == nil ? (game.state?.account.remote_status ?? "Choose an available model below.") : "Confirm this move and Braincell cost. ~ means estimated time; — means not measured yet. (P)")
                    }.frame(width: side)
                    HStack(spacing: 0) {
                        HintField(game: game).frame(width: min(240, geometry.size.width * 0.36))
                        StatusBand(game: game, content: label)
                    }.frame(width: geometry.size.width, height: bandHeight)
                        .background(Color.primary.opacity(0.06))
                        .overlay(Rectangle().strokeBorder(Color.primary.opacity(0.35), lineWidth: 1).allowsHitTesting(false))
                }.frame(width: geometry.size.width).frame(maxHeight: .infinity, alignment: .top)
            }
        }
        .background(Color(nsColor: NoPaintPalette.background))
        .sheet(isPresented: $game.showModels) { ModelBrowserView(game: game) }
        .sheet(isPresented: $game.showHistory) { PaintingHistoryView(game: game) }
        .onAppear { game.start() }
    }
}

struct HintField: View {
    @ObservedObject var game: GameStore
    @FocusState private var focused: Bool
    var body: some View {
        HStack(spacing: 5) {
            Image(systemName: "sparkle").font(.system(size: 10, weight: .semibold))
                .foregroundStyle(focused ? Color(red: 0.85, green: 0.24, blue: 0.44) : .secondary)
            TextField("", text: $game.hint, prompt: Text(game.hintable ? "hint" : "hint · cloud only"))
                .textFieldStyle(.plain).font(.system(size: 12, weight: .medium)).focused($focused)
                .onSubmit { if game.canPaint && !game.pendingDone { game.act("paint") }; focused = false }
                .onExitCommand { focused = false }
            if !game.hint.isEmpty {
                Button { game.hint = "" } label: { Image(systemName: "xmark.circle.fill").font(.system(size: 10)) }
                    .buttonStyle(.plain).foregroundStyle(.tertiary).help("Clear hint")
            }
        }
        .padding(.horizontal, 9).frame(height: 22)
        .background(Capsule().fill(Color.primary.opacity(focused ? 0.1 : 0.05)))
        .overlay(Capsule().strokeBorder(Color.primary.opacity(focused ? 0.4 : 0.18), lineWidth: 1))
        .padding(.leading, 6).padding(.trailing, 2)
        .disabled(!game.hintable)
        .help("A word or phrase to steer the next cloud Paint along with your image. Return paints.")
    }
}

struct ModelBrowserView: View {
    @ObservedObject var game: GameStore
    @State private var search = ""
    @State private var selected: String?
    @State private var previewsOnly = false
    var filtered: [RemoteModel] { game.models.filter { (!previewsOnly || $0.provider_previews) && (search.isEmpty || ($0.name + $0.id).localizedCaseInsensitiveContains(search)) } }
    var model: RemoteModel? { game.models.first { $0.id == selected } }
    var configuredOffer: Model? { game.state?.models.first { $0.model == selected } }
    func offer(_ model: RemoteModel) -> Model? { game.state?.models.first { $0.model == model.id } }
    var selectionHelp: String? {
        guard game.state?.account.connected == true else { return "Sign in with AC to use Braincells." }
        if game.state?.account.remote_status != nil { return nil }
        guard let configuredOffer, configuredOffer.braincells != nil else { return "Listed on OpenRouter; not yet enabled and priced on AC." }
        if !configuredOffer.available {
            let balance = (game.state?.account.remaining ?? 0) + (game.state?.account.purchased ?? 0)
            if let price = configuredOffer.braincells, price > balance { return "Requires \(price.formatted()) Braincells; \(balance.formatted()) available." }
            return "This model is unavailable on the AC server."
        }
        return nil
    }
    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack {
                TextField("Search image models", text: $search).textFieldStyle(.roundedBorder)
                Toggle("Partial images", isOn: $previewsOnly).toggleStyle(.checkbox).actionPointer()
            }
            if game.cloudUnavailable || game.state?.account.error != nil { CloudNotice(game: game) }
            if game.catalogLoading { ProgressView().controlSize(.small) }
            if let error = game.catalogError { HStack { Text(error).foregroundStyle(.red); Button("Retry") { game.browse() }.actionPointer() } }
            Table(filtered, selection: $selected) {
                TableColumn("OpenRouter", value: \.name).width(min: 230)
                TableColumn("Braincells / Paint") { model in
                    Text(offer(model)?.braincells.map { $0.formatted() } ?? "Not enabled").monospacedDigit()
                }.width(125)
                TableColumn("Paints left") { model in Text(game.paintsLeft(offer(model)?.braincells)).monospacedDigit() }.width(85)
            }.frame(minHeight: 180).actionPointer()
            if let model {
                Text(model.id).font(.system(size: 12, design: .monospaced)).textSelection(.enabled)
                Text(game.pricingID == model.id ? game.prices : "").font(.system(size: 12)).textSelection(.enabled)
                Text("Paints left uses your daily and purchased Braincells at this model’s AC price.").font(.system(size: 12)).foregroundStyle(.secondary)
                if model.provider_previews { Text("Provider supports partial images. AC cloud currently returns the final image.").font(.system(size: 12)).foregroundStyle(.secondary) }
                if let selectionHelp { Text(selectionHelp).font(.system(size: 12)).foregroundStyle(.secondary) }
            }
            HStack {
                Link("OpenRouter ↗", destination: URL(string: model?.url ?? "https://openrouter.ai/models?output_modalities=image")!)
                    .actionPointer()
                Spacer()
                if let model {
                    Button("Select \(model.name)") {
                        game.act("engine", extra:["engine": "ac-openrouter:" + model.id]); game.showModels = false
                    }.actionPointer().disabled(!game.canChange)
                }
                Button("Done") { game.showModels = false }.actionPointer().keyboardShortcut(.defaultAction)
            }
        }.padding(20).frame(width: 680, height: 560)
            .onChange(of: selected) { _, _ in if let model { game.fetchPrices(model) } }
            .onAppear { selected = game.state?.engine.replacingOccurrences(of: "ac-openrouter:", with: "") }
    }
}

struct CloudNotice: View {
    @ObservedObject var game: GameStore
    var body: some View {
        VStack(alignment: .leading, spacing: 5) {
            HStack {
                Text(game.state?.account.error != nil ? game.statusIssue?.title ?? "AC offline" : "AC cloud unavailable").font(.system(size: 13, weight: .semibold))
                Spacer()
                Button("Refresh") {
                    if game.state?.account.error != nil { game.recoverStatus() } else { game.account("refresh") }
                }.actionPointer().disabled(game.state?.account.working == true)
            }
            Text(game.state?.account.error ?? game.state?.account.remote_status ?? "AC cannot run this remote model yet.").font(.system(size: 12))
                .fixedSize(horizontal: false, vertical: true)
            if game.state?.account.stale != true, let service = game.state?.account.remote_service,
               let balance = service.balance_usd, let minimum = service.minimum_balance_usd {
                Text(String(format: "AC’s OpenRouter balance: $%.2f · Required: $%.2f", balance, minimum))
                    .font(.system(size: 12).monospacedDigit())
            }
        }.padding(10).frame(maxWidth: .infinity, alignment: .leading)
            .background(Color.primary.opacity(0.06))
    }
}

@MainActor final class AppDelegate: NSObject, NSApplicationDelegate, NSMenuItemValidation {
    let game = GameStore()
    var window: NSWindow!
    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.regular)
        // This app starts AppKit directly rather than through a storyboard.
        // Load the bundled artwork explicitly so the running Dock tile updates
        // even when Launch Services cached an earlier icon-less development app.
        if let url = Bundle.main.url(forResource: "NoPaint", withExtension: "icns"),
           let icon = NSImage(contentsOf: url) {
            NSApp.applicationIconImage = icon
        }
        let menu = NSMenu()
        let appItem = NSMenuItem(); menu.addItem(appItem)
        let appMenu = NSMenu(); appItem.submenu = appMenu
        appMenu.addItem(withTitle: "About No Paint", action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: "")
        appMenu.addItem(.separator())
        appMenu.addItem(withTitle: "Quit No Paint", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        let fileItem = NSMenuItem(); menu.addItem(fileItem)
        let fileMenu = NSMenu(title: "File"); fileItem.submenu = fileMenu
        let fresh = fileMenu.addItem(withTitle: "Fresh", action: #selector(freshPainting(_:)), keyEquivalent: "n"); fresh.target = self
        let save = fileMenu.addItem(withTitle: "Save Painting…", action: #selector(savePainting(_:)), keyEquivalent: "s"); save.target = self
        let upscaleItem = fileMenu.addItem(withTitle: "Save Upscaled", action: nil, keyEquivalent: "")
        let upscaleMenu = NSMenu(title: "Save Upscaled"); upscaleItem.submenu = upscaleMenu
        for scale in [2, 4] {
            let item = upscaleMenu.addItem(withTitle: "\(scale)× · \(scale * 256) × \(scale * 256) · 0 Braincells…", action: #selector(saveUpscaled(_:)), keyEquivalent: "")
            item.target = self; item.representedObject = scale
        }
        let done = fileMenu.addItem(withTitle: "Done", action: #selector(donePainting(_:)), keyEquivalent: "d"); done.target = self
        let history = fileMenu.addItem(withTitle: "History…", action: #selector(showHistory(_:)), keyEquivalent: "h"); history.target = self; history.keyEquivalentModifierMask = [.command, .shift]
        fileMenu.addItem(.separator())
        let published = fileMenu.addItem(withTitle: "Open Last Painting", action: #selector(openPainting(_:)), keyEquivalent: ""); published.target = self
        let editItem = NSMenuItem(); menu.addItem(editItem)
        let edit = NSMenu(title: "Edit"); editItem.submenu = edit
        edit.addItem(withTitle: "Copy", action: #selector(NSText.copy(_:)), keyEquivalent: "c")
        edit.addItem(withTitle: "Paste", action: #selector(NSText.paste(_:)), keyEquivalent: "v")
        edit.addItem(withTitle: "Select All", action: #selector(NSText.selectAll(_:)), keyEquivalent: "a")
        let viewItem = NSMenuItem(); menu.addItem(viewItem)
        let view = NSMenu(title: "View"); viewItem.submenu = view
        let before = view.addItem(withTitle: "Before", action: #selector(showBefore(_:)), keyEquivalent: "b"); before.target = self
        before.toolTip = "Show the accepted painting. Hold Space for a temporary look."
        let fit = view.addItem(withTitle: "Fit Image", action: #selector(fitImage(_:)), keyEquivalent: "0"); fit.target = self
        let optionsItem = NSMenuItem(); menu.addItem(optionsItem)
        let options = NSMenu(title: "Options"); optionsItem.submenu = options
        func choice(_ title: String, parent: NSMenu, action: Selector, value: Any) {
            let item = parent.addItem(withTitle: title, action: action, keyEquivalent: "")
            item.target = self; item.representedObject = value
        }
        let change = NSMenu(title: "Change")
        let changeItem = options.addItem(withTitle: "Change", action: nil, keyEquivalent: ""); changeItem.submenu = change
        for (title, value) in [("Small", 0.25), ("Medium", 0.5), ("Large", 0.75)] {
            choice(title, parent: change, action: #selector(changeStrength(_:)), value: value)
        }
        options.addItem(.separator())
        choice("Zoom Crop", parent: options, action: #selector(changeDrag(_:)), value: "zoom")
        choice("Inpaint", parent: options, action: #selector(changeDrag(_:)), value: "inpaint")
        let brush = NSMenu(title: "Brush Size")
        let brushItem = options.addItem(withTitle: "Brush Size", action: nil, keyEquivalent: ""); brushItem.submenu = brush
        for size in [8, 16, 24, 32, 48, 64] {
            choice("\(size) px", parent: brush, action: #selector(changeBrush(_:)), value: size)
        }
        let clear = options.addItem(withTitle: "Clear Mask", action: #selector(clearMask(_:)), keyEquivalent: ""); clear.target = self
        options.addItem(.separator())
        let account = options.addItem(withTitle: "Sign in", action: #selector(changeAccount(_:)), keyEquivalent: ""); account.target = self
        let modelItem = NSMenuItem(); menu.addItem(modelItem)
        let models = NSMenu(title: "Models"); modelItem.submenu = models
        let browse = models.addItem(withTitle: "Browse OpenRouter…", action: #selector(browseModels(_:)), keyEquivalent: "m"); browse.target = self
        NSApp.mainMenu = menu
        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 560, height: 785), styleMask: [.titled,.closable,.miniaturizable,.resizable], backing: .buffered, defer: false)
        window.identifier = NSUserInterfaceItemIdentifier("NoPaintMain")
        window.title = "No Paint"
        window.backgroundColor = NoPaintPalette.background
        window.tabbingMode = .disallowed
        let hosting = NSHostingView(rootView: ContentView(game: game))
        // GeometryReader controls the layout. Do not feed its last rendered
        // dimensions back into NSWindow's minimum size during a tile resize.
        hosting.sizingOptions = []
        window.contentView = hosting
        window.contentMinSize = NSSize(width: 280, height: 278)
        window.setFrameAutosaveName("NoPaintMain"); window.center(); window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }
    @objc func savePainting(_ sender: Any?) { game.save() }
    @objc func saveUpscaled(_ sender: NSMenuItem) {
        if let scale = sender.representedObject as? Int { game.saveUpscaled(scale) }
    }
    @objc func freshPainting(_ sender: Any?) { game.fresh() }
    @objc func donePainting(_ sender: Any?) { game.done() }
    @objc func showHistory(_ sender: Any?) { game.history() }
    @objc func openPainting(_ sender: Any?) {
        if let value = game.state?.publication?.url, let url = URL(string: value), url.scheme == "https", url.host == "aesthetic.computer" { NSWorkspace.shared.open(url) }
    }
    @objc func browseModels(_ sender: Any?) { game.browse() }
    @objc func showBefore(_ sender: Any?) { game.peek(!game.peeking) }
    @objc func fitImage(_ sender: Any?) { game.fitVersion += 1 }
    @objc func changeStrength(_ sender: NSMenuItem) {
        if game.canChange, let strength = sender.representedObject as? Double { game.act("strength", extra: ["strength": strength]) }
    }
    @objc func changeDrag(_ sender: NSMenuItem) {
        if let mode = sender.representedObject as? String { game.dragMode = mode }
    }
    @objc func changeBrush(_ sender: NSMenuItem) {
        if let size = sender.representedObject as? Int { game.brushSize = CGFloat(size) }
    }
    @objc func clearMask(_ sender: Any?) { game.clearMask() }
    @objc func changeAccount(_ sender: Any?) { game.account(game.state?.account.connected == true ? "disconnect" : "connect") }
    func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
        if menuItem.action == #selector(freshPainting(_:)) { return game.canFresh }
        if menuItem.action == #selector(savePainting(_:)) { return game.state != nil }
        if menuItem.action == #selector(saveUpscaled(_:)) { return game.canChange }
        if menuItem.action == #selector(donePainting(_:)) { return game.canDone }
        if menuItem.action == #selector(showHistory(_:)) { return game.state != nil }
        if menuItem.action == #selector(openPainting(_:)) { return game.state?.publication?.url != nil }
        if menuItem.action == #selector(showBefore(_:)) { menuItem.state = game.peeking ? .on : .off; return game.state != nil }
        if menuItem.action == #selector(changeStrength(_:)) {
            menuItem.state = (menuItem.representedObject as? Double) == game.state?.strength ? .on : .off
            return game.canChange
        }
        if menuItem.action == #selector(changeDrag(_:)) {
            menuItem.state = (menuItem.representedObject as? String) == game.dragMode ? .on : .off
            return !game.marking
        }
        if menuItem.action == #selector(changeBrush(_:)) {
            menuItem.state = CGFloat(menuItem.representedObject as? Int ?? 0) == game.brushSize ? .on : .off
            return game.dragMode == "inpaint" && !game.marking
        }
        if menuItem.action == #selector(clearMask(_:)) { return game.state?.mask != nil && !game.sending && !game.pendingDone && !game.marking }
        if menuItem.action == #selector(changeAccount(_:)) {
            menuItem.title = game.state?.account.connected == true ? "Sign out" : game.state?.account.working == true ? "Signing in…" : "Sign in"
            return game.canChange && game.state?.account.working != true
        }
        return true
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

@main struct NoPaint {
    static func main() {
        let app = NSApplication.shared
        let delegate = AppDelegate(); app.delegate = delegate
        withExtendedLifetime(delegate) { app.run() }
    }
}
