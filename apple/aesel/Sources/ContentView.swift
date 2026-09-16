import SwiftUI
import WebKit

/// Aesel's palette, taken from the desktop app's window background so the two
/// clients look like one product.
enum Paint {
    static let bg = Color(red: 0x46 / 255, green: 0x32 / 255, blue: 0x64 / 255)
    static let deep = Color(red: 0x24 / 255, green: 0x1d / 255, blue: 0x35 / 255)
    static let ink = Color(red: 0xef / 255, green: 0xe9 / 255, blue: 0xff / 255)
    static let dim = Color(red: 0xb3 / 255, green: 0xa4 / 255, blue: 0xcf / 255)
    static let rule = Color(red: 0x5e / 255, green: 0x48 / 255, blue: 0x81 / 255)
    static let you = Color(red: 0xff / 255, green: 0x4d / 255, blue: 0x88 / 255)
    static let ac = Color(red: 0xb5 / 255, green: 0x8c / 255, blue: 0xff / 255)
    static let edit = Color(red: 0x6e / 255, green: 0xe7 / 255, blue: 0xa8 / 255)
    static let bad = Color(red: 0xff / 255, green: 0x7a / 255, blue: 0x7a / 255)
}

struct ContentView: View {
    let session: Session
    let host: SessionHost

    @State private var draft = ""
    @FocusState private var writing: Bool

    var body: some View {
        VStack(spacing: 0) {
            bar
            stage
            transcript
            composer
        }
        .background(Paint.bg)
        .foregroundStyle(Paint.ink)
        // The off-screen session host. Zero-sized and hidden, but in the
        // hierarchy so WebKit keeps it running.
        .background(HostCarrier(host: host).frame(width: 0, height: 0))
        .overlay { if let fatal = session.fatal { failure(fatal) } }
    }

    private var bar: some View {
        HStack {
            Text(session.route.isEmpty ? "Aesel" : session.route)
                .lineLimit(1)
                .truncationMode(.middle)
            Spacer(minLength: 12)
            Text(session.status)
                .font(.system(size: 12, design: .monospaced))
                .foregroundStyle(colour(session.health))
        }
        .font(.system(size: 15, design: .monospaced))
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .overlay(alignment: .bottom) { Divider().overlay(Paint.rule) }
    }

    /// The piece, live. A webview because the Aesthetic Computer runtime is one
    /// on every platform — that constraint is why hosting the session in a
    /// second, hidden webview costs nothing.
    private var stage: some View {
        ZStack {
            Paint.deep
            if let url = session.previewURL {
                PieceView(url: url)
            } else {
                Text("Your piece appears here, live, as it is written.")
                    .font(.system(size: 13, design: .monospaced))
                    .foregroundStyle(Paint.dim)
                    .multilineTextAlignment(.center)
                    .padding(.horizontal, 24)
            }
        }
        .aspectRatio(4 / 3, contentMode: .fit)
        .frame(maxWidth: .infinity)
        .clipped()
        .overlay(alignment: .bottom) { Divider().overlay(Paint.rule) }
    }

    private var transcript: some View {
        ScrollViewReader { scroller in
            ScrollView {
                LazyVStack(alignment: .leading, spacing: 10) {
                    ForEach(session.entries) { entry in
                        row(entry).id(entry.id)
                    }
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(12)
            }
            .onChange(of: session.entries.last?.text) {
                guard let last = session.entries.last else { return }
                withAnimation(.easeOut(duration: 0.15)) {
                    scroller.scrollTo(last.id, anchor: .bottom)
                }
            }
        }
        .frame(maxHeight: .infinity)
    }

    private func row(_ entry: Entry) -> some View {
        HStack(alignment: .top, spacing: 8) {
            if let tag = tag(entry.kind) {
                Text(tag)
                    .font(.system(size: 11, weight: .regular, design: .monospaced))
                    .foregroundStyle(colour(entry.kind))
                    .frame(width: 34, alignment: .leading)
            }
            Text(entry.text)
                .font(.system(size: 15, design: .monospaced))
                .foregroundStyle(entry.kind == .note || entry.kind == .bad ? colour(entry.kind) : Paint.ink)
                .textSelection(.enabled)
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    private var composer: some View {
        HStack(alignment: .bottom, spacing: 8) {
            TextField("Describe what to make…", text: $draft, axis: .vertical)
                .lineLimit(1 ... 6)
                .textFieldStyle(.plain)
                .font(.system(size: 16, design: .monospaced))
                .padding(10)
                .background(Paint.deep, in: RoundedRectangle(cornerRadius: 10))
                .overlay { RoundedRectangle(cornerRadius: 10).stroke(Paint.rule) }
                .focused($writing)
                .submitLabel(.send)

            Button {
                if session.busy {
                    host.stop()
                } else {
                    send()
                }
            } label: {
                Image(systemName: session.busy ? "stop.fill" : "arrow.right")
                    .font(.system(size: 18, weight: .semibold))
                    .frame(width: 44, height: 44)
                    .background(session.busy ? Paint.rule : Paint.you, in: RoundedRectangle(cornerRadius: 10))
                    .foregroundStyle(.white)
            }
            .disabled(!session.busy && draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .overlay(alignment: .top) { Divider().overlay(Paint.rule) }
    }

    private func send() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        draft = ""
        host.ask(text)
    }

    private func failure(_ message: String) -> some View {
        VStack(spacing: 12) {
            Text("Aesel could not reach its session.")
                .font(.system(size: 16, weight: .semibold, design: .monospaced))
            Text(message)
                .font(.system(size: 13, design: .monospaced))
                .foregroundStyle(Paint.dim)
                .multilineTextAlignment(.center)
        }
        .padding(24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Paint.bg)
    }

    private func tag(_ kind: Entry.Kind) -> String? {
        switch kind {
        case .you: return "YOU"
        case .ac: return "AC"
        case .edit: return "EDIT"
        case .bad: return "!"
        case .note: return nil
        }
    }

    private func colour(_ kind: Entry.Kind) -> Color {
        switch kind {
        case .you: return Paint.you
        case .ac: return Paint.ac
        case .edit: return Paint.edit
        case .bad: return Paint.bad
        case .note: return Paint.dim
        }
    }

    private func colour(_ health: Health) -> Color {
        switch health {
        case .idle: return Paint.dim
        case .working: return Paint.ac
        case .live: return Paint.edit
        case .failed: return Paint.bad
        }
    }
}

/// Puts the hidden session webview in the hierarchy without drawing it.
private struct HostCarrier: UIViewRepresentable {
    let host: SessionHost

    func makeUIView(context: Context) -> WKWebView { host.attachable }
    func updateUIView(_ view: WKWebView, context: Context) {}
}

/// The live piece. Scrolling is off because a piece owns its own gestures.
struct PieceView: UIViewRepresentable {
    let url: URL

    func makeUIView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.allowsInlineMediaPlayback = true
        configuration.mediaTypesRequiringUserActionForPlayback = []
        let view = WKWebView(frame: .zero, configuration: configuration)
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.isOpaque = false
        view.backgroundColor = .clear
        view.isMultipleTouchEnabled = true
        return view
    }

    func updateUIView(_ view: WKWebView, context: Context) {
        // The fragment changes on every publish, so compare the whole URL: a
        // reload is exactly what a new revision wants.
        guard view.url?.absoluteString != url.absoluteString else { return }
        view.load(URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData))
    }
}
