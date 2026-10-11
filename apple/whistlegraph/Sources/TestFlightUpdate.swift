import Foundation
import UIKit

/// Offers a newer TestFlight build when the app comes to the foreground.
/// lith's GET /api/whistlegraph-release names the newest build testers can
/// install (`fastlane ios announce` sets it once beta review approves); a copy
/// whose CFBundleVersion is lower shows one alert whose Update opens the app's
/// TestFlight page. Later quiets that build for twelve hours.
/// Only TestFlight copies check: App Store copies update through the store, and
/// debug builds stay quiet unless launched with WHISTLEGRAPH_UPDATE_CHECK=1.
@MainActor final class TestFlightUpdate: ObservableObject {
    struct Offer: Equatable { let build: Int; let version: String }

    static let shared = TestFlightUpdate()
    @Published var offer: Offer?

    private static let endpoint = URL(string: "https://aesthetic.computer/api/whistlegraph-release")!
    private static let testFlightPage = URL(string: "itms-beta://beta.itunes.apple.com/v1/app/6820297686")!
    private static let webPage = URL(string: "https://beta.itunes.apple.com/v1/app/6820297686")!
    private static let laterKey = "whistlegraph-update-later"
    private static let quietFor: TimeInterval = 12 * 60 * 60
    private var checking = false

    static var enabled: Bool {
        if let forced = ProcessInfo.processInfo.environment["WHISTLEGRAPH_UPDATE_CHECK"] { return forced == "1" }
        #if DEBUG
        return false
        #else
        return Bundle.main.appStoreReceiptURL?.lastPathComponent == "sandboxReceipt"
        #endif
    }

    static var installedBuild: Int {
        Int(Bundle.main.infoDictionary?["CFBundleVersion"] as? String ?? "") ?? 0
    }

    func check() async {
        guard Self.enabled, !checking, offer == nil else { return }
        checking = true
        defer { checking = false }
        var request = URLRequest(url: Self.endpoint, timeoutInterval: 10)
        request.cachePolicy = .reloadIgnoringLocalCacheData
        guard let (data, response) = try? await URLSession.shared.data(for: request),
              (response as? HTTPURLResponse)?.statusCode == 200,
              let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let build = json["build"] as? Int, build > Self.installedBuild else { return }
        if let later = UserDefaults.standard.dictionary(forKey: Self.laterKey),
           later["build"] as? Int == build,
           let at = later["at"] as? Double, Date().timeIntervalSince1970 - at < Self.quietFor { return }
        offer = Offer(build: build, version: json["version"] as? String ?? "")
    }

    func update() {
        offer = nil
        UIApplication.shared.open(Self.testFlightPage) { opened in
            if !opened { UIApplication.shared.open(Self.webPage) }
        }
    }

    func later() {
        if let offer {
            UserDefaults.standard.set(["build": offer.build, "at": Date().timeIntervalSince1970], forKey: Self.laterKey)
        }
        offer = nil
    }
}
