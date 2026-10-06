import SwiftUI

// Display values use the existing $5 / 1,000,000 braincell pack.
// Provider-cost equivalents are not a receipt for wallet deductions.
enum CostUnit: String, CaseIterable, Identifiable {
    case braincells, usd, tezos
    var id: String { rawValue }
    var label: String { switch self { case .braincells: "Braincells"; case .usd: "USD"; case .tezos: "Tezos" } }
    static let preference = "whistlegraph-cost-unit"
    static let cellsPerUSD = 200_000.0
    func amount(usd: Double, rate: TezDisplayRate.Rate?, partial: Bool = false) -> String {
        guard usd.isFinite, usd >= 0 else { return "Unavailable" }
        let value: Double
        let suffix: String
        let digits: Int
        switch self {
        case .braincells: value = usd * Self.cellsPerUSD; suffix = " braincells"; digits = 0
        case .usd:
            if usd > 0 && usd < 0.01 && !partial { return "< $0.01" }
            return (partial ? floor(usd * 100) / 100 : usd).formatted(.currency(code: "USD"))
        case .tezos:
            guard let rate, rate.isFresh else { return "Rate unavailable" }
            value = usd / rate.usdPerTez; suffix = " tez"; digits = 6
        }
        let scale = pow(10.0, Double(digits))
        let shown = partial ? floor(value * scale) / scale : value
        if value > 0 && value < 1 / scale && !partial { return "< " + (1 / scale).formatted(.number.precision(.fractionLength(0...digits))) + suffix }
        return shown.formatted(.number.precision(.fractionLength(0...digits))) + suffix
    }
}

@MainActor final class TezDisplayRate: ObservableObject {
    struct Rate: Decodable {
        let usdPerTez: Double
        let asOf: String
        let expiresAt: String
        var date: Date? { ISO8601DateFormatter().date(from: asOf) ?? Self.precise.date(from: asOf) }
        var isFresh: Bool {
            guard usdPerTez.isFinite, usdPerTez > 0,
                  let expiration = ISO8601DateFormatter().date(from: expiresAt) ?? Self.precise.date(from: expiresAt) else { return false }
            return expiration > Date()
        }
        private static var precise: ISO8601DateFormatter {
            let f = ISO8601DateFormatter(); f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]; return f
        }
    }
    static let shared = TezDisplayRate()
    @Published private(set) var rate: Rate?
    private var loading = false
    func refresh() async {
        guard !loading, !(rate?.isFresh == true && Date().timeIntervalSince(rate?.date ?? .distantPast) < 60) else { return }
        loading = true; defer { loading = false }
        do {
            var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/easel-tezos")!)
            request.httpMethod = "POST"; request.timeoutInterval = 15
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = Data("{\"action\":\"price\"}".utf8)
            let (data, response) = try await URLSession.shared.data(for: request)
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { return }
            let next = try JSONDecoder().decode(Rate.self, from: data)
            if next.isFresh { rate = next }
        } catch { /* The UI shows an unavailable rate instead of inventing a price. */ }
    }
}

struct CostUnitPicker: View {
    @AppStorage(CostUnit.preference) private var unit: CostUnit = .usd
    @ObservedObject private var prices = TezDisplayRate.shared
    var body: some View {
        Picker("Cost unit", selection: $unit) {
            ForEach(CostUnit.allCases) { Text($0.label).tag($0) }
        }.pickerStyle(.segmented).accessibilityIdentifier("brain-cost-unit")
        if unit != .braincells {
            Text("Estimated service value.").font(.footnote).foregroundStyle(.secondary)
        }
        if unit == .tezos {
            if let rate = prices.rate, rate.isFresh, let date = rate.date {
                Text("1 tez = \(rate.usdPerTez.formatted(.currency(code: "USD").precision(.fractionLength(4)))) · \(date.formatted(date: .omitted, time: .shortened)) · TzKT")
                    .font(.footnote).foregroundStyle(.secondary)
            } else { Text("Tezos rate unavailable").font(.footnote).foregroundStyle(.secondary) }
        }
    }
}
