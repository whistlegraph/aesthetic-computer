import AppKit
import Foundation
import UniformTypeIdentifiers

struct JukeCloudTrack: Codable, Equatable {
    let key: String
    let name: String
    let bytes: Int64
    let updatedAt: String?
    let contentType: String?
    let url: URL
    let command: String
}

private struct JukeCloudList: Codable { let tracks: [JukeCloudTrack] }
private struct JukeCloudPreparedUpload: Codable {
    let uploadURL: URL
    let headers: [String: String]
    let track: JukeCloudTrack
}
private struct JukeCloudDownload: Codable { let url: URL }
private struct JukeCloudError: Codable { let error: String }

enum JukeCloudClientError: LocalizedError {
    case signedOut
    case response(String)

    var errorDescription: String? {
        switch self {
        case .signedOut: return "Sign in to Aesthetic Computer first."
        case .response(let message): return message
        }
    }
}

final class JukeCloudClient {
    private let session: URLSession
    private let endpoint: URL

    init(session: URLSession = .shared) {
        self.session = session
        let origin = ProcessInfo.processInfo.environment["AC_API_ORIGIN"]
            ?? "https://aesthetic.computer"
        self.endpoint = URL(string: origin.trimmingCharacters(in: CharacterSet(charactersIn: "/"))
                            + "/api/juke-cloud")!
    }

    private func request(_ method: String = "GET", body: [String: Any]? = nil) throws -> URLRequest {
        guard let token = ACSession.shared.token() else { throw JukeCloudClientError.signedOut }
        var request = URLRequest(url: endpoint)
        request.httpMethod = method
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        if let body {
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try JSONSerialization.data(withJSONObject: body)
        }
        return request
    }

    private func decode<T: Decodable>(_ type: T.Type, data: Data, response: URLResponse) throws -> T {
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard (200..<300).contains(status) else {
            let message = (try? JSONDecoder().decode(JukeCloudError.self, from: data).error)
                ?? "Cloud request failed (\(status))."
            throw JukeCloudClientError.response(message)
        }
        return try JSONDecoder().decode(type, from: data)
    }

    func list() async throws -> [JukeCloudTrack] {
        let (data, response) = try await session.data(for: request())
        return try decode(JukeCloudList.self, data: data, response: response).tracks
    }

    func upload(file: URL) async throws -> JukeCloudTrack {
        let values = try file.resourceValues(forKeys: [.fileSizeKey, .isRegularFileKey])
        guard values.isRegularFile == true, let bytes = values.fileSize, bytes > 0 else {
            throw JukeCloudClientError.response("Choose a non-empty audio file.")
        }
        let preparedRequest = try request("POST", body: [
            "action": "upload", "filename": file.lastPathComponent, "bytes": bytes,
        ])
        let (preparedData, preparedResponse) = try await session.data(for: preparedRequest)
        let prepared = try decode(JukeCloudPreparedUpload.self,
                                  data: preparedData, response: preparedResponse)
        var upload = URLRequest(url: prepared.uploadURL)
        upload.httpMethod = "PUT"
        for (name, value) in prepared.headers { upload.setValue(value, forHTTPHeaderField: name) }
        upload.setValue(String(bytes), forHTTPHeaderField: "Content-Length")
        let (uploadData, response) = try await session.upload(for: upload, fromFile: file)
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard (200..<300).contains(status) else {
            let body = String(data: uploadData, encoding: .utf8) ?? ""
            let detail = body.components(separatedBy: "<Message>").dropFirst().first?
                .components(separatedBy: "</Message>").first
            throw JukeCloudClientError.response(
                "Upload failed (\(status))\(detail.map { ": \($0)" } ?? ".")")
        }
        return prepared.track
    }

    func download(_ track: JukeCloudTrack) async throws -> URL {
        let signedRequest = try request("POST", body: ["action": "download", "key": track.key])
        let (signedData, signedResponse) = try await session.data(for: signedRequest)
        let signed = try decode(JukeCloudDownload.self, data: signedData, response: signedResponse)
        let (temporary, response) = try await session.download(from: signed.url)
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard (200..<300).contains(status) else {
            throw JukeCloudClientError.response("Download failed (\(status)).")
        }
        let base = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("Aesthetic Computer/JukeWizard/Cloud", isDirectory: true)
        try FileManager.default.createDirectory(at: base, withIntermediateDirectories: true)
        let destination = base.appendingPathComponent("\(UUID().uuidString)-\(track.name)")
        try FileManager.default.moveItem(at: temporary, to: destination)
        return destination
    }
}

final class JukeCloudWindowController: NSWindowController,
                                       NSTableViewDataSource, NSTableViewDelegate {
    private let client = JukeCloudClient()
    private let currentFile: () -> URL?
    private let loadLocalFile: (URL) -> Void
    private var sessionWatch: UUID?
    private var tracks: [JukeCloudTrack] = []
    private let account = NSTextField(labelWithString: "")
    private let status = NSTextField(labelWithString: "")
    private let signIn = NSButton()
    private let uploadCurrent = NSButton()
    private let uploadOther = NSButton()
    private let refreshButton = NSButton()
    private let loadButton = NSButton()
    private let copyButton = NSButton()
    private let table = NSTableView()
    private let scroll = NSScrollView()

    init(currentFile: @escaping () -> URL?, loadLocalFile: @escaping (URL) -> Void) {
        self.currentFile = currentFile
        self.loadLocalFile = loadLocalFile
        let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 580, height: 390),
                              styleMask: [.titled, .closable, .miniaturizable, .resizable],
                              backing: .buffered, defer: false)
        window.title = "Juke Cloud"
        window.minSize = NSSize(width: 480, height: 300)
        window.center()
        super.init(window: window)
        buildUI()
        sessionWatch = ACSession.shared.startWatching { [weak self] in self?.sessionChanged() }
    }

    required init?(coder: NSCoder) { fatalError() }
    deinit { if let sessionWatch { ACSession.shared.stopWatching(sessionWatch) } }

    func prepareForDisplay() { sessionChanged() }

    private func buildUI() {
        guard let content = window?.contentView else { return }
        account.font = .systemFont(ofSize: 13, weight: .semibold)
        account.frame = NSRect(x: 14, y: 352, width: 230, height: 22)
        account.autoresizingMask = [.maxXMargin, .minYMargin]
        content.addSubview(account)

        configure(signIn, "Sign in", #selector(signInAction))
        configure(uploadCurrent, "Upload playing", #selector(uploadCurrentAction))
        configure(uploadOther, "Upload…", #selector(uploadOtherAction))
        configure(refreshButton, "Refresh", #selector(refreshAction))
        signIn.frame = NSRect(x: 240, y: 347, width: 78, height: 28)
        uploadCurrent.frame = NSRect(x: 322, y: 347, width: 110, height: 28)
        uploadOther.frame = NSRect(x: 436, y: 347, width: 72, height: 28)
        refreshButton.frame = NSRect(x: 512, y: 347, width: 58, height: 28)
        for button in [signIn, uploadCurrent, uploadOther, refreshButton] {
            button.autoresizingMask = [.minXMargin, .minYMargin]
            content.addSubview(button)
        }

        let name = NSTableColumn(identifier: .init("name"))
        name.title = "Track"; name.width = 350
        let size = NSTableColumn(identifier: .init("size"))
        size.title = "Size"; size.width = 90
        table.addTableColumn(name); table.addTableColumn(size)
        table.dataSource = self; table.delegate = self
        table.target = self; table.doubleAction = #selector(loadAction)
        table.usesAlternatingRowBackgroundColors = true
        scroll.documentView = table
        scroll.hasVerticalScroller = true
        scroll.frame = NSRect(x: 12, y: 54, width: 556, height: 286)
        scroll.autoresizingMask = [.width, .height]
        content.addSubview(scroll)

        configure(loadButton, "Load in JukeWizard", #selector(loadAction))
        configure(copyButton, "Copy play command", #selector(copyAction))
        loadButton.frame = NSRect(x: 12, y: 12, width: 144, height: 30)
        copyButton.frame = NSRect(x: 160, y: 12, width: 138, height: 30)
        status.frame = NSRect(x: 306, y: 17, width: 262, height: 20)
        status.alignment = .right
        status.lineBreakMode = .byTruncatingTail
        status.textColor = .secondaryLabelColor
        status.autoresizingMask = [.width, .maxYMargin]
        content.addSubview(loadButton); content.addSubview(copyButton); content.addSubview(status)
        updateSelection()
    }

    private func configure(_ button: NSButton, _ title: String, _ action: Selector) {
        button.title = title; button.target = self; button.action = action
        button.bezelStyle = .rounded
    }

    private func sessionChanged() {
        let signedIn = ACSession.shared.token() != nil
        account.stringValue = ACSession.shared.displayName.map { "☁︎ \($0)" } ?? "Juke Cloud"
        signIn.isHidden = signedIn
        uploadCurrent.isEnabled = signedIn && currentFile() != nil
        uploadOther.isEnabled = signedIn
        refreshButton.isEnabled = signedIn
        if signedIn { refresh() }
        else {
            tracks = []; table.reloadData(); status.stringValue = "Sign in to sync tracks"
            updateSelection()
        }
    }

    private func refresh() {
        status.stringValue = "Loading…"
        Task { [weak self] in
            guard let self else { return }
            do {
                let tracks = try await client.list()
                await MainActor.run {
                    self.tracks = tracks; self.table.reloadData()
                    self.status.stringValue = tracks.isEmpty ? "No cloud tracks" : "\(tracks.count) cloud track\(tracks.count == 1 ? "" : "s")"
                    self.updateSelection()
                }
            } catch { await MainActor.run { self.status.stringValue = error.localizedDescription } }
        }
    }

    private func upload(_ file: URL) {
        status.stringValue = "Uploading \(file.lastPathComponent)…"
        uploadCurrent.isEnabled = false; uploadOther.isEnabled = false
        Task { [weak self] in
            guard let self else { return }
            do {
                _ = try await client.upload(file: file)
                await MainActor.run {
                    self.uploadOther.isEnabled = true
                    self.uploadCurrent.isEnabled = self.currentFile() != nil
                    self.refresh()
                }
            } catch {
                await MainActor.run {
                    self.status.stringValue = error.localizedDescription
                    self.uploadOther.isEnabled = true
                    self.uploadCurrent.isEnabled = self.currentFile() != nil
                }
            }
        }
    }

    private var selected: JukeCloudTrack? {
        let row = table.selectedRow
        return row >= 0 && row < tracks.count ? tracks[row] : nil
    }

    private func updateSelection() {
        let enabled = selected != nil
        loadButton.isEnabled = enabled; copyButton.isEnabled = enabled
    }

    func numberOfRows(in tableView: NSTableView) -> Int { tracks.count }
    func tableViewSelectionDidChange(_ notification: Notification) { updateSelection() }
    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        let track = tracks[row]
        let field = NSTextField(labelWithString: tableColumn?.identifier.rawValue == "size"
            ? ByteCountFormatter.string(fromByteCount: track.bytes, countStyle: .file)
            : track.name)
        field.lineBreakMode = .byTruncatingMiddle
        return field
    }

    @objc private func signInAction() {
        status.stringValue = "Opening browser…"
        ACLogin.shared.signIn { [weak self] result in
            if case .failure(let error) = result { self?.status.stringValue = error.localizedDescription }
            else { self?.sessionChanged() }
        }
    }

    @objc private func uploadCurrentAction() { if let file = currentFile() { upload(file) } }
    @objc private func uploadOtherAction() {
        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = true; panel.canChooseDirectories = false
        panel.allowedContentTypes = ["mp3", "wav", "flac", "ogg", "m4a", "aac", "aif", "aiff", "caf"]
            .compactMap { UTType(filenameExtension: $0) }
        guard panel.runModal() == .OK else { return }
        for file in panel.urls { upload(file) }
    }
    @objc private func refreshAction() { refresh() }
    @objc private func loadAction() {
        guard let selected else { return }
        status.stringValue = "Downloading \(selected.name)…"
        Task { [weak self] in
            guard let self else { return }
            do {
                let file = try await client.download(selected)
                await MainActor.run {
                    self.loadLocalFile(file)
                    self.status.stringValue = "Loaded \(selected.name)"
                }
            } catch { await MainActor.run { self.status.stringValue = error.localizedDescription } }
        }
    }
    @objc private func copyAction() {
        guard let selected else { return }
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(selected.command, forType: .string)
        status.stringValue = "Copied for aesthetic.computer"
    }
}
