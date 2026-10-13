import UIKit
import SwiftUI
import UniformTypeIdentifiers

/// choir in the system share sheet: a link or text from Instagram, Safari,
/// anywhere, posted to the room with an optional comment. Uses the app's
/// sign-in from the shared keychain group (Account.accessGroup).
final class ShareViewController: UIViewController {
    override func viewDidLoad() {
        super.viewDidLoad()
        let model = ShareModel(context: extensionContext)
        let host = UIHostingController(rootView: ShareView(model: model))
        addChild(host); view.addSubview(host.view)
        host.view.translatesAutoresizingMaskIntoConstraints = false
        NSLayoutConstraint.activate([
            host.view.leadingAnchor.constraint(equalTo: view.leadingAnchor), host.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            host.view.topAnchor.constraint(equalTo: view.topAnchor), host.view.bottomAnchor.constraint(equalTo: view.bottomAnchor),
        ])
        host.didMove(toParent: self)
        Task { await model.load() }
    }
}

struct Preview: Decodable { let title: String?; let description: String?; let image: String?; let siteName: String? }

@MainActor final class ShareModel: ObservableObject {
    weak var context: NSExtensionContext?
    @Published var link: String?
    @Published var text = ""
    @Published var comment = ""
    @Published var preview: Preview?
    @Published var status: String?
    @Published var sending = false
    @Published var done = false
    init(context: NSExtensionContext?) { self.context = context }

    func load() async {
        let providers = (context?.inputItems as? [NSExtensionItem] ?? []).flatMap { $0.attachments ?? [] }
        for p in providers {
            if p.hasItemConformingToTypeIdentifier(UTType.url.identifier),
               let url = try? await p.loadItem(forTypeIdentifier: UTType.url.identifier) as? URL, url.scheme?.hasPrefix("http") == true {
                link = url.absoluteString; break
            }
            if p.hasItemConformingToTypeIdentifier(UTType.plainText.identifier),
               let s = try? await p.loadItem(forTypeIdentifier: UTType.plainText.identifier) as? String {
                if let found = s.range(of: #"https?://\S+"#, options: .regularExpression) { link = String(s[found]) }
                text = s
            }
        }
        if Keychain.tokens() == nil { status = "Open choir and log in first, then share again." }
        guard let link, var c = URLComponents(string: "https://aesthetic.computer/api/og-preview") else { return }
        c.queryItems = [URLQueryItem(name: "url", value: link)]
        if let (data, _) = try? await URLSession.shared.data(from: c.url!) { preview = try? JSONDecoder().decode(Preview.self, from: data) }
    }

    var body: String {
        let base = link ?? text
        let note = comment.trimmingCharacters(in: .whitespacesAndNewlines)
        if link != nil, !text.isEmpty, text != link, note.isEmpty { return text }
        return note.isEmpty ? base : "\(note)\n\(base)"
    }

    func post() async {
        sending = true; defer { sending = false }
        guard let token = await Keychain.freshToken() else { status = "Open choir and log in first."; return }
        var r = URLRequest(url: URL(string: "https://aesthetic.computer/api/choir")!)
        r.httpMethod = "POST"; r.timeoutInterval = 20
        r.setValue("application/json", forHTTPHeaderField: "Content-Type")
        r.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        r.httpBody = try? JSONSerialization.data(withJSONObject: ["text": String(body.prefix(2000)), "tags": []])
        do {
            let (data, resp) = try await URLSession.shared.data(for: r)
            guard (resp as? HTTPURLResponse)?.statusCode == 200 else {
                status = (try? JSONSerialization.jsonObject(with: data) as? [String: Any])?["error"] as? String ?? "Couldn't post."
                return
            }
            done = true
            try? await Task.sleep(for: .seconds(0.8))
            context?.completeRequest(returningItems: nil)
        } catch { status = "Couldn't reach choir." }
    }
    func cancel() { context?.cancelRequest(withError: NSError(domain: "choir", code: 0)) }
}

/// Reads the app's sign-in from the shared keychain group, refreshing it when needed.
enum Keychain {
    struct Tokens: Codable { var access: String; var refresh: String; var expires: Date }
    static let query: [String: Any] = [kSecClass as String: kSecClassGenericPassword,
        kSecAttrAccount as String: "computer.aesthetic.choir.tokens", kSecAttrAccessGroup as String: "FB5948YR3S.computer.aesthetic.choir.shared"]
    static func tokens() -> Tokens? {
        var q = query; q[kSecReturnData as String] = true
        var out: AnyObject?
        guard SecItemCopyMatching(q as CFDictionary, &out) == errSecSuccess, let d = out as? Data else { return nil }
        return try? JSONDecoder().decode(Tokens.self, from: d)
    }
    static func freshToken() async -> String? {
        guard var t = tokens() else { return nil }
        if t.expires.timeIntervalSinceNow > 60 { return t.access }
        guard !t.refresh.isEmpty else { return nil }
        var r = URLRequest(url: URL(string: "https://hi.aesthetic.computer/oauth/token")!)
        r.httpMethod = "POST"; r.setValue("application/json", forHTTPHeaderField: "Content-Type")
        r.httpBody = try? JSONSerialization.data(withJSONObject: ["grant_type": "refresh_token", "client_id": "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt", "refresh_token": t.refresh])
        guard let (data, resp) = try? await URLSession.shared.data(for: r), (resp as? HTTPURLResponse)?.statusCode == 200,
              let v = try? JSONSerialization.jsonObject(with: data) as? [String: Any], let access = v["access_token"] as? String else { return nil }
        t.access = access; t.expires = Date().addingTimeInterval(v["expires_in"] as? Double ?? 3600)
        if let enc = try? JSONEncoder().encode(t) { SecItemUpdate(query as CFDictionary, [kSecValueData as String: enc] as CFDictionary) }
        return access
    }
}

struct ShareView: View {
    @ObservedObject var model: ShareModel
    var body: some View {
        NavigationStack {
            Form {
                if let p = model.preview, model.link != nil {
                    Section {
                        if let img = p.image, let url = URL(string: img) {
                            AsyncImage(url: url) { $0.resizable().scaledToFill() } placeholder: { Color.secondary.opacity(0.1) }
                                .frame(height: 180).frame(maxWidth: .infinity).clipped().clipShape(RoundedRectangle(cornerRadius: 8))
                        }
                        Text((p.siteName ?? "").uppercased()).font(.caption2.weight(.semibold)).foregroundStyle(.secondary)
                        Text((p.title ?? model.link ?? "").replacingOccurrences(of: "&#064;", with: "@")).font(.system(.headline, design: .serif)).lineLimit(3)
                    }
                } else if let link = model.link {
                    Section { Text(link).font(.footnote).foregroundStyle(.secondary) }
                } else if !model.text.isEmpty {
                    Section { Text(model.text).lineLimit(6) }
                }
                Section {
                    TextField("Comments on culture", text: $model.comment, axis: .vertical).lineLimit(2...6)
                }
                if let s = model.status { Section { Text(s).foregroundStyle(.red) } }
                if model.done { Section { Label("Posted to choir", systemImage: "checkmark.circle.fill").foregroundStyle(.green) } }
            }
            .navigationTitle("Share to choir")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) { Button("Cancel") { model.cancel() } }
                ToolbarItem(placement: .confirmationAction) {
                    Button(model.sending ? "Posting…" : "Post") { Task { await model.post() } }
                        .disabled(model.sending || model.done || (model.link == nil && model.text.isEmpty))
                }
            }
        }
    }
}
