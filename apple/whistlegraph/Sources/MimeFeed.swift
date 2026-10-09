import SwiftUI
import WebKit

// mime.ac in the app: a vertical, swipeable feed of mimes people published.
// One AC runtime renders whichever card is in view; the cards carry the
// handle and the words it was asked for. Exploring needs no account.

struct Mime: Decodable, Identifiable, Equatable {
    let code: String
    let handle: String
    let caption: String
    let source: String
    let versions: Int
    let publishedAt: String?
    var id: String { code }
}

@MainActor final class MimeFeedModel: ObservableObject {
    @Published var mimes: [Mime] = []
    @Published var loading = false
    @Published var error = ""
    private var next: String?
    private var exhausted = false
    static let api = URL(string: "https://aesthetic.computer/api/mime-feed")!
    func load(more: Bool = false) async {
        guard !loading, !(more && exhausted) else { return }
        loading = true; defer { loading = false }
        var parts = URLComponents(url: Self.api, resolvingAgainstBaseURL: false)!
        parts.queryItems = [URLQueryItem(name: "limit", value: "20")]
        if more, let next { parts.queryItems?.append(URLQueryItem(name: "before", value: next)) }
        struct Page: Decodable { let mimes: [Mime]; let next: String? }
        do {
            let (data, response) = try await URLSession.shared.data(for: URLRequest(url: parts.url!, timeoutInterval: 20))
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
            let page = try JSONDecoder().decode(Page.self, from: data)
            if more { mimes += page.mimes.filter { mime in !mimes.contains(where: { $0.code == mime.code }) } } else { mimes = page.mimes }
            next = page.next; exhausted = page.mimes.isEmpty || page.next == nil
            error = ""
            DeviceActionLog.shared.record(.feed, .ready, [.version: mimes.count])
        } catch {
            self.error = mimes.isEmpty ? "The feed could not load. Check your connection and try again." : ""
            DeviceActionLog.shared.recordError(.feed, error)
        }
    }
}

/// The one AC runtime behind the feed. It loads once; each card in view hands
/// it a source. Touches go to the pager, not the piece.
struct MimeRuntime: UIViewRepresentable {
    let source: String
    static let url = URL(string: "https://aesthetic.computer/wipe?noauth=true&noplot=true&nogap=true&nolabel=true&preview=walkieware")!
    final class Coordinator: NSObject, WKNavigationDelegate {
        var source = ""
        func render(_ view: WKWebView) {
            guard let data = try? JSONSerialization.data(withJSONObject: [source]), let json = String(data: data, encoding: .utf8) else { return }
            view.evaluateJavaScript("window.__mimeRender?.(\(json)[0]);", completionHandler: nil)
        }
        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { render(webView) }
    }
    func makeCoordinator() -> Coordinator { Coordinator() }
    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.allowsInlineMediaPlayback = true
        config.mediaTypesRequiringUserActionForPlayback = []
        config.userContentController.addUserScript(WKUserScript(source: Self.script, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        let view = WKWebView(frame: .zero, configuration: config)
        view.isOpaque = false; view.backgroundColor = .black
        view.scrollView.isScrollEnabled = false
        view.isUserInteractionEnabled = false
        view.navigationDelegate = context.coordinator
        #if DEBUG
        view.isInspectable = true
        #endif
        view.load(URLRequest(url: Self.url))
        return view
    }
    func updateUIView(_ view: WKWebView, context: Context) {
        guard context.coordinator.source != source else { return }
        context.coordinator.source = source
        context.coordinator.render(view)
    }
    static let script = """
    (() => {
      let pending = null, ready = false;
      const flush = () => {
        if (!ready || pending == null) return;
        const source = pending; pending = null;
        window.AC?.startAudio?.();
        window.acSEND({type:'dropped:piece', content:{name:'mime', source, search:'noauth=true&noplot=true&nogap=true&nolabel=true', isKidLisp:false}});
      };
      window.__mimeRender = source => { pending = source; flush(); };
      const poll = setInterval(() => {
        if (!window.preloaded || !window.acSEND) return;
        clearInterval(poll); ready = true; flush();
      }, 100);
    })();
    """
}

struct MimeFeedView: View {
    let makeYourOwn: () -> Void
    @StateObject private var model = MimeFeedModel()
    @State private var current: String?
    @Environment(\.dismiss) private var dismiss
    private var active: Mime? { model.mimes.first { $0.code == current } ?? model.mimes.first }
    var body: some View {
        ZStack {
            Color.black.ignoresSafeArea()
            if let active { MimeRuntime(source: active.source).ignoresSafeArea() }
            ScrollView(.vertical) {
                LazyVStack(spacing: 0) {
                    ForEach(model.mimes) { mime in
                        MimeCard(mime: mime, makeYourOwn: makeYourOwn)
                            .containerRelativeFrame(.vertical)
                            .id(mime.code)
                            .onAppear { if mime == model.mimes.last { Task { await model.load(more: true) } } }
                    }
                }.scrollTargetLayout()
            }
            .scrollTargetBehavior(.paging)
            .scrollPosition(id: $current)
            .scrollIndicators(.hidden)
            .ignoresSafeArea()
            .accessibilityIdentifier("mime-feed")
            if model.mimes.isEmpty {
                VStack(spacing: 12) {
                    if model.loading { ProgressView().tint(.white) }
                    else if !model.error.isEmpty {
                        Text(model.error).multilineTextAlignment(.center)
                        Button("Try again") { Task { await model.load() } }.buttonStyle(.bordered).tint(.white)
                    } else {
                        Text("Nothing published yet.").font(.custom("ComicRelief-Regular", size: 20, relativeTo: .title3))
                        Text("Make a piece and switch on mime.ac in Your Pieces.").multilineTextAlignment(.center).font(.footnote)
                    }
                }.foregroundStyle(.white).padding(32)
            }
            VStack {
                HStack {
                    Button { ButtonSounds.play(.pop); dismiss() } label: {
                        Image(systemName: "xmark").font(.system(size: 20, weight: .bold)).frame(width: 44, height: 44)
                    }.foregroundStyle(.white).accessibilityLabel("Close feed").accessibilityIdentifier("mime-feed-close")
                    Spacer()
                    ComicTitle(text: "mime.ac", size: 22)
                }.padding(.horizontal, 8)
                Spacer()
            }
        }
        .task { await model.load() }
        .onChange(of: current) { _, _ in ButtonSounds.play(.tick) }
        .preferredColorScheme(.dark)
    }
}

struct MimeCard: View {
    let mime: Mime
    let makeYourOwn: () -> Void
    var body: some View {
        VStack {
            Spacer()
            VStack(alignment: .leading, spacing: 6) {
                if !mime.handle.isEmpty { ComicTitle(text: mime.handle, size: 22) }
                Text(mime.caption.isEmpty ? "/" + mime.code : mime.caption)
                    .font(.custom("ComicRelief-Regular", size: 20, relativeTo: .title3)).foregroundStyle(.white)
                Text("/\(mime.code) · v\(mime.versions)").font(.footnote).foregroundStyle(.white.opacity(0.7))
                HStack {
                    Button(action: makeYourOwn) {
                        Text("Make your own").font(.custom("ComicRelief-Bold", size: 18, relativeTo: .body)).foregroundStyle(.black)
                    }.buttonStyle(.borderedProminent).tint(Color(red: 0.40, green: 0.83, blue: 0.95))
                        .accessibilityIdentifier("mime-make-your-own")
                    Spacer()
                    // Apple asks hosted content to be reportable; mail is the channel until there is a desk.
                    Link("Report", destination: URL(string: "mailto:mail@aesthetic.computer?subject=Report%20mime%20/" + mime.code)!)
                        .font(.footnote).foregroundStyle(.white.opacity(0.7))
                }.padding(.top, 6)
            }
            .padding(20).padding(.bottom, 28)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(LinearGradient(colors: [.clear, .black.opacity(0.75)], startPoint: .top, endPoint: .bottom))
        }
        .contentShape(Rectangle())
        .accessibilityIdentifier("mime-" + mime.code)
    }
}

/// Publishing the open piece to mime.ac, from Your Pieces. The thread row on
/// the server carries the flag; the phone reads it when the sheet opens.
@MainActor final class MimePublishModel: ObservableObject {
    @Published var published: Bool? = nil
    @Published var busy = false
    @Published var notice = ""
    private var code = ""
    private struct ThreadState: Decodable { let published: Bool? }
    func load(code: String, account: WhistlegraphAccount) async {
        self.code = code; published = nil; notice = ""
        guard !code.isEmpty, let token = try? await account.token() else { return }
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/whistlegraph?code=\(code)")!, timeoutInterval: 10)
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        guard let (data, response) = try? await URLSession.shared.data(for: request),
              (response as? HTTPURLResponse)?.statusCode == 200,
              let thread = try? JSONDecoder().decode(ThreadState.self, from: data) else { return }
        published = thread.published ?? false
    }
    func set(_ value: Bool, account: WhistlegraphAccount) async {
        guard !busy, !code.isEmpty else { return }
        busy = true; defer { busy = false }
        guard let token = try? await account.token() else { notice = "Sign in again to publish."; return }
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/whistlegraph-publish")!, timeoutInterval: 15)
        request.httpMethod = "POST"
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try? JSONSerialization.data(withJSONObject: ["code": code, "published": value])
        do {
            let (data, response) = try await URLSession.shared.data(for: request)
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
            struct Reply: Decodable { let published: Bool }
            published = try JSONDecoder().decode(Reply.self, from: data).published
            notice = published == true ? "On mime.ac now." : "Off mime.ac."
            DeviceActionLog.shared.record(.publish, value ? .enabled : .disabled)
        } catch {
            notice = "Could not change that right now. Try again."
            DeviceActionLog.shared.recordError(.publish, error)
        }
    }
}
