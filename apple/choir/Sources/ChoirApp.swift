import SwiftUI

@main
struct ChoirApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) private var delegate
    @StateObject private var account = Account()
    @StateObject private var room = Room()
    @Environment(\.scenePhase) private var phase

    var body: some Scene {
        WindowGroup {
            RoomView()
                .environmentObject(account)
                .environmentObject(room)
                .preferredColorScheme(.light)
                .tint(Theme.ink)
                .environmentObject(Router.shared)
                .task {
                    Devices.account = account; Trail.send("open", ["signedIn": account.signedIn])
                    await account.refreshMe()
                    BugOverlay.shared.update(for: account) // Fía's floating ladybug
                    // People who logged in before push existed get asked once, here.
                    if account.handle != nil, account.notifyChat || account.notifyDiscover, !UserDefaults.standard.bool(forKey: "push-asked") {
                        UserDefaults.standard.set(true, forKey: "push-asked")
                        await Devices.ask()
                    }
                    #if DEBUG // App Store screenshots: -choirShot /discover or /thread/<id>
                    if let shot = UserDefaults.standard.string(forKey: "choirShot") {
                        try? await Task.sleep(for: .seconds(3)); Router.shared.open(shot)
                        let page = UserDefaults.standard.integer(forKey: "choirPage") // -choirPage N: Discover card N
                        if page > 0 {
                            try? await Task.sleep(for: .seconds(6))
                            func pager(_ v: UIView) -> UIScrollView? {
                                if let s = v as? UIScrollView, s.contentSize.height > s.bounds.height * 2 { return s }
                                return v.subviews.lazy.compactMap(pager).first
                            }
                            let windows = UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.flatMap(\.windows)
                            if let s = windows.reversed().lazy.compactMap({ pager($0) }).first {
                                s.setContentOffset(CGPoint(x: 0, y: (s.bounds.height - s.adjustedContentInset.top - s.adjustedContentInset.bottom) * CGFloat(page) - s.adjustedContentInset.top), animated: false)
                            }
                        }
                    }
                    #endif
                }
                .onChange(of: account.handle) { _, _ in BugOverlay.shared.update(for: account) }
                .onChange(of: phase) { _, p in
                    guard p == .active else { return }
                    Devices.report(.open)
                    Task { await room.load(); await Devices.refresh() }
                }
        }
    }
}

/// What the room shows: the feed under the current tag, and the tag list.
@MainActor
final class Room: ObservableObject {
    @Published var posts: [Post] = []
    @Published var tags: [TagCount] = []
    @Published var tag: String?
    @Published var error: String?
    /// A Discover item someone wants to talk about in the main chat (Fía, 22:12).
    @Published var about: Post?
    @Published var here: API.Here?
    @Published var hidden: Set<String> = Set(UserDefaults.standard.stringArray(forKey: "hidden") ?? [])

    func load() async {
        do {
            async let p = API.feed(tag: tag)
            async let t = API.tags()
            (posts, tags) = try await (p, t)
            error = nil
        } catch { self.error = error.localizedDescription }
    }

    func hide(_ handle: String) {
        hidden.insert(handle)
        UserDefaults.standard.set(Array(hidden), forKey: "hidden")
        Trail.send("hide")
    }
    func unhideAll() { hidden = []; UserDefaults.standard.removeObject(forKey: "hidden") }
    /// The chat reads top to bottom: oldest first.
    var chat: [Post] { visible.reversed() }
    var visible: [Post] {
        return posts.filter { !hidden.contains($0.handle) }
    }
}
