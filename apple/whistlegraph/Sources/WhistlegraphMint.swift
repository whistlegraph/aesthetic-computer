#if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
import SwiftUI
import SafariServices

private struct MintBrowser: UIViewControllerRepresentable {
    let url: URL
    func makeUIViewController(context: Context) -> SFSafariViewController { SFSafariViewController(url: url) }
    func updateUIViewController(_ controller: SFSafariViewController, context: Context) {}
}

struct WhistlegraphMintSheet: View {
    @ObservedObject var session: WhistlegraphSession
    @State private var title = ""
    @State private var description = ""
    @State private var editions = 1
    @State private var royalties = 15
    @State private var busy = false
    @State private var notice = ""
    @State private var browser = false
    @State private var mintURL: URL?
    @State private var cover: UIImage?
    @State private var capture: (hash: String, png: String)?
    @State private var pendingBody: Data?
    private var key: String { "whistlegraph-mint:\(session.snapshot.handle):\(session.snapshot.code):\(session.snapshot.head)" }
    var body: some View {
        Form {
            Section {
                if let cover {
                    Image(uiImage: cover).resizable().scaledToFit().frame(height: 180)
                        .frame(maxWidth: .infinity).accessibilityLabel("Artwork cover")
                }
                LabeledContent(session.snapshot.code, value: "Version \(session.snapshot.head)")
                TextField("Title", text: $title).accessibilityIdentifier("mint-title")
                TextField("Description", text: $description, axis: .vertical).lineLimit(2...5)
                Stepper("\(editions) edition\(editions == 1 ? "" : "s")", value: $editions, in: 1...100)
                Stepper("\(royalties)% royalties", value: $royalties, in: 0...25)
            }.disabled(busy || mintURL != nil || pendingBody != nil)
            Section {
                Button(mintURL == nil ? "Pack and preview" : "Resume mint") { Task { await prepare() } }
                    .disabled(busy || (capture == nil && mintURL == nil && pendingBody == nil) || title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
                    .accessibilityIdentifier("mint-prepare")
                if mintURL != nil {
                    Button("Discard preview", role: .destructive) { Task { await discard() } }.disabled(busy)
                }
                if busy { ProgressView() }
                if !notice.isEmpty { Text(notice).accessibilityIdentifier("mint-notice") }
            } footer: {
                Text("Your Tezos wallet signs the mint. The selected version becomes a single HTML artwork on HEN. The artwork will be public; your wallet shows the network fee before approval.")
            }
        }
        .navigationTitle("Mint on HEN").navigationBarTitleDisplayMode(.inline)
        .task {
            title = session.snapshot.code
            description = session.snapshot.caption ?? ""
            pendingBody = UserDefaults.standard.data(forKey: key + ":body")
            if let pendingBody { restoreSettings(pendingBody) }
            if pendingBody == nil, let secret = UserDefaults.standard.string(forKey: key), secret.count == 64 {
                mintURL = URL(string: "https://aesthetic.computer/mint/#" + secret)
                var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/whistlegraph-mint")!)
                request.httpMethod = "POST"; request.timeoutInterval = 15
                request.setValue("Bearer \(secret)", forHTTPHeaderField: "Authorization")
                request.setValue("application/json", forHTTPHeaderField: "Content-Type")
                request.httpBody = Data("{\"action\":\"status\"}".utf8)
                if let (data, response) = try? await URLSession.shared.data(for: request),
                   (response as? HTTPURLResponse)?.statusCode == 200 { restoreSettings(data) }
            }
            do {
                capture = try await session.mintCapture()
                if let capture, let data = Data(base64Encoded: capture.png) { cover = UIImage(data: data) }
            } catch { notice = error.localizedDescription }
        }
        .fullScreenCover(isPresented: $browser) {
            if let mintURL { MintBrowser(url: mintURL).ignoresSafeArea() }
        }
        .onOpenURL { url in if url.scheme == "whistlegraph" && url.host == "mint" { browser = false } }
    }
    private func restoreSettings(_ data: Data) {
        guard let saved = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else { return }
        title = saved["title"] as? String ?? title
        description = saved["description"] as? String ?? description
        editions = saved["editions"] as? Int ?? editions
        if let value = saved["royalties"] as? Int { royalties = value / 10 }
    }
    private func prepare() async {
        if mintURL != nil { browser = true; return }
        guard capture != nil || pendingBody != nil else { return }
        busy = true; notice = ""
        defer { busy = false }
        do {
            guard let token = try await session.account.token() else { throw NativeSignIn.failure("Sign in to AC first.") }
            let secret: String
            if pendingBody != nil, let saved = UserDefaults.standard.string(forKey: key) { secret = saved }
            else { secret = UUID().uuidString.replacingOccurrences(of: "-", with: "").lowercased() + UUID().uuidString.replacingOccurrences(of: "-", with: "").lowercased() }
            let payload: [String: Any] = ["action":"create", "secret":secret, "code":session.snapshot.code,
                "version":session.snapshot.head, "sourceHash":capture?.hash ?? "", "density":session.pixelSize, "aspect":session.previewFormat.rawValue,
                "title":title, "description":description, "editions":editions, "royalties":royalties * 10, "cover":capture?.png ?? ""]
            var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/whistlegraph-mint")!)
            request.httpMethod = "POST"; request.timeoutInterval = 45
            request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try pendingBody ?? JSONSerialization.data(withJSONObject: payload)
            // Save the capability before dispatch so a lost response remains recoverable.
            UserDefaults.standard.set(secret, forKey: key)
            pendingBody = request.httpBody
            UserDefaults.standard.set(pendingBody, forKey: key + ":body")
            let (data, response) = try await URLSession.shared.data(for: request)
            let result = try JSONSerialization.jsonObject(with: data) as? [String: Any]
            guard let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else {
                UserDefaults.standard.removeObject(forKey: key)
                UserDefaults.standard.removeObject(forKey: key + ":body"); pendingBody = nil
                throw NativeSignIn.failure(result?["error"] as? String ?? "Could not prepare this mint.")
            }
            mintURL = URL(string: "https://aesthetic.computer/mint/#" + secret)
            UserDefaults.standard.removeObject(forKey: key + ":body"); pendingBody = nil
            browser = true
        } catch { notice = error.localizedDescription }
    }
    private func discard() async {
        guard let secret = mintURL?.fragment else { return }
        busy = true; defer { busy = false }
        do {
            var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/whistlegraph-mint")!)
            request.httpMethod = "POST"; request.timeoutInterval = 30
            request.setValue("Bearer \(secret)", forHTTPHeaderField: "Authorization")
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = Data("{\"action\":\"cancel\"}".utf8)
            let (data, response) = try await URLSession.shared.data(for: request)
            let state = try JSONSerialization.jsonObject(with: data) as? [String: Any]
            guard (response as? HTTPURLResponse)?.statusCode == 200, state?["status"] as? String == "cancelled" else {
                throw NativeSignIn.failure(state?["error"] as? String ?? "Could not discard the preview.")
            }
            UserDefaults.standard.removeObject(forKey: key)
            UserDefaults.standard.removeObject(forKey: key + ":body")
            mintURL = nil; pendingBody = nil; notice = ""
        } catch { notice = error.localizedDescription }
    }
}

#endif
