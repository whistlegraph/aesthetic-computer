#if os(macOS)
import AppKit
import AuthenticationServices
import Network

/// AC's registered callback is loopback HTTP. Receive it only on localhost,
/// then redirect into the system authentication session's private callback.
@MainActor final class NativeWebAuthentication: NSObject, ASWebAuthenticationPresentationContextProviding {
    private let anchor: () -> NSWindow
    private let completion: (Result<URL, Error>) -> Void
    private var authentication: ASWebAuthenticationSession?
    private var listener: NWListener?
    private var timeout: Task<Void, Never>?
    private var connections: [NWConnection] = []
    private var finished = false
    private var receivedCallback = false

    init(anchor: @escaping () -> NSWindow, completion: @escaping (Result<URL, Error>) -> Void) {
        self.anchor = anchor; self.completion = completion
    }

    func presentationAnchor(for session: ASWebAuthenticationSession) -> ASPresentationAnchor { anchor() }

    func start(url: URL, state: String) throws {
        let parameters = NWParameters.tcp
        parameters.requiredLocalEndpoint = .hostPort(host: "127.0.0.1", port: 44233)
        let listener = try NWListener(using: parameters)
        self.listener = listener
        listener.stateUpdateHandler = { [weak self] status in
            MainActor.assumeIsolated {
                guard let self, !self.finished else { return }
                switch status {
                case .ready: self.open(url)
                case .failed: self.finish(.failure(NativeSignIn.failure("Close any other AC sign-in window and try again.")))
                default: break
                }
            }
        }
        listener.newConnectionHandler = { [weak self] connection in
            MainActor.assumeIsolated {
                guard let self, !self.finished, self.connections.count < 32 else { connection.cancel(); return }
                self.connections.append(connection)
                connection.start(queue: .main)
                self.receive(connection, state: state, bytes: Data())
            }
        }
        listener.start(queue: .main)
        timeout = Task { [weak self] in
            try? await Task.sleep(nanoseconds: 300_000_000_000)
            guard !Task.isCancelled else { return }
            self?.finish(.failure(NativeSignIn.failure("Sign-in timed out. Try again.")))
        }
    }

    private func open(_ url: URL) {
        guard authentication == nil else { return }
        let authentication = ASWebAuthenticationSession(url: url, callbackURLScheme: "aesel-auth") { [weak self] url, error in
            Task { @MainActor in
                guard let self, !self.finished else { return }
                if let url, var parts = URLComponents(url: url, resolvingAgainstBaseURL: false),
                   parts.scheme == "aesel-auth", parts.host == "callback" {
                    parts.scheme = "http"; parts.host = "localhost"; parts.port = 44233; parts.path = "/callback"
                    if let callback = parts.url { self.finish(.success(callback)); return }
                }
                self.finish(.failure(error ?? NativeSignIn.failure("Sign-in did not return a callback.")))
            }
        }
        authentication.presentationContextProvider = self
        authentication.prefersEphemeralWebBrowserSession = true
        self.authentication = authentication
        if !authentication.start() { finish(.failure(NativeSignIn.failure("Could not open secure sign-in."))) }
    }

    static func redirect(for target: String, expectedState: String) -> URL? {
        guard target.hasPrefix("/callback?"), target.utf8.count < 8192,
              let parts = URLComponents(string: "http://localhost:44233" + target), parts.path == "/callback",
              parts.fragment == nil, let items = parts.queryItems,
              items.filter({ $0.name == "state" }).count == 1,
              items.first(where: { $0.name == "state" })?.value == expectedState,
              items.contains(where: { $0.name == "code" || $0.name == "error" }) else { return nil }
        var redirect = URLComponents(string: "aesel-auth://callback")!
        redirect.queryItems = items
        return redirect.url
    }

    private func receive(_ connection: NWConnection, state: String, bytes: Data) {
        connection.receive(minimumIncompleteLength: 1, maximumLength: 8192) { [weak self] chunk, _, complete, error in
            MainActor.assumeIsolated {
                guard let self, !self.finished, error == nil else { connection.cancel(); return }
                var data = bytes; if let chunk { data.append(chunk) }
                guard data.count <= 8192 else { connection.cancel(); return }
                guard let text = String(data: data, encoding: .utf8), text.contains("\r\n\r\n") else {
                    if complete { connection.cancel() } else { self.receive(connection, state: state, bytes: data) }
                    return
                }
                let request = text.components(separatedBy: "\r\n")[0].split(separator: " ")
                let redirect = request.count == 3 && request[0] == "GET" && !self.receivedCallback
                    ? Self.redirect(for: String(request[1]), expectedState: state) : nil
                let response: String
                if let redirect {
                    self.receivedCallback = true
                    response = "HTTP/1.1 302 Found\r\nLocation: \(redirect.absoluteString)\r\n"
                } else { response = "HTTP/1.1 400 Bad Request\r\n" }
                let payload = response + "Content-Length: 0\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n"
                connection.send(content: Data(payload.utf8), completion: .contentProcessed { _ in connection.cancel() })
            }
        }
    }

    func cancel() {
        finished = true
        timeout?.cancel(); listener?.cancel(); authentication?.cancel()
        connections.forEach { $0.cancel() }; connections.removeAll()
    }

    private func finish(_ result: Result<URL, Error>) {
        guard !finished else { return }
        cancel()
        completion(result)
    }
}
#endif
