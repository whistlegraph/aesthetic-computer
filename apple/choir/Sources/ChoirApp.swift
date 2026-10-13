import SwiftUI

@main
struct ChoirApp: App {
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
                .task { Trail.send("open", ["signedIn": account.signedIn]); await account.refreshMe() }
                .onChange(of: phase) { _, p in if p == .active { Task { await room.load() } } }
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
    var visible: [Post] { posts.filter { !hidden.contains($0.handle) } }
}
