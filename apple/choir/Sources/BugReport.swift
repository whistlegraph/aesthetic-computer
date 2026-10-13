import SwiftUI
import UIKit

/// The red bug, top right: snaps what's on screen, then asks what happened.
/// Filed to /api/choir?bug=1 with the screenshot, note, build and device; the
/// admins' phones get a push (system/netlify/functions/choir.mjs).
enum BugShot {
    /// The key window as it looks right now, before any sheet covers it.
    @MainActor static func capture() -> UIImage? {
        guard let window = UIApplication.shared.connectedScenes.compactMap({ ($0 as? UIWindowScene)?.keyWindow }).first else { return nil }
        let renderer = UIGraphicsImageRenderer(bounds: window.bounds)
        return renderer.image { _ in window.drawHierarchy(in: window.bounds, afterScreenUpdates: false) }
    }
    /// JPEG at most 1080 px wide, base64, to keep the report small.
    static func encode(_ image: UIImage) -> String? {
        let scale = min(1, 1080 / max(image.size.width * image.scale, 1))
        let size = CGSize(width: image.size.width * image.scale * scale, height: image.size.height * image.scale * scale)
        let format = UIGraphicsImageRendererFormat(); format.scale = 1
        let small = UIGraphicsImageRenderer(size: size, format: format).image { _ in image.draw(in: CGRect(origin: .zero, size: size)) }
        return small.jpegData(compressionQuality: 0.7)?.base64EncodedString()
    }
}

struct BugButton: View {
    @State private var shot: UIImage?
    @State private var open = false
    var body: some View {
        Button {
            shot = BugShot.capture()
            Trail.send("bug:open")
            open = true
        } label: {
            Image(systemName: "ladybug.fill").foregroundStyle(.red)
        }
        .accessibilityLabel("Report a bug")
        .sheet(isPresented: $open) { BugSheet(shot: shot) }
    }
}

struct BugSheet: View {
    let shot: UIImage?
    @EnvironmentObject var account: Account
    @Environment(\.dismiss) private var dismiss
    @State private var text = ""
    @State private var includeShot = true
    @State private var sending = false
    @State private var sent: String?
    @State private var error: String?
    @FocusState private var focused: Bool

    var body: some View {
        NavigationStack {
            Form {
                // The note comes first so the keyboard never covers it.
                Section {
                    TextField("What's going on, or what did you notice?", text: $text, axis: .vertical)
                        .lineLimit(3...8).focused($focused)
                }
                if let error { Section { Text(error).foregroundStyle(.red) } }
                if let sent { Section { Text("Thank you. Filed as bug \(sent).").foregroundStyle(.secondary) } }
                if let shot {
                    Section {
                        Toggle("Include this screenshot", isOn: $includeShot)
                        Image(uiImage: shot).resizable().scaledToFit().frame(maxHeight: 220)
                            .clipShape(RoundedRectangle(cornerRadius: 8)).opacity(includeShot ? 1 : 0.3)
                            .frame(maxWidth: .infinity)
                    }
                }
            }
            .scrollDismissesKeyboard(.interactively)
            .navigationTitle("Report a bug")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) { Button("Cancel") { dismiss() } }
                ToolbarItem(placement: .confirmationAction) {
                    Button(sending ? "Sending…" : "Send") { Task { await send() } }
                        .disabled(sending || sent != nil || (text.trimmingCharacters(in: .whitespaces).isEmpty && !(includeShot && shot != nil)))
                }
            }
            .onAppear { focused = true }
        }
        .tint(Theme.ink)
    }

    func send() async {
        sending = true; error = nil; defer { sending = false }
        guard let token = await account.token() else { error = "Log in to send a report."; return }
        let info = Bundle.main.infoDictionary ?? [:]
        var body: [String: Any] = [
            "text": text,
            "context": [
                "version": info["CFBundleShortVersionString"] as? String ?? "", "build": info["CFBundleVersion"] as? String ?? "",
                "os": UIDevice.current.systemVersion, "device": UIDevice.current.model,
                "screen": "\(Int(UIScreen.main.bounds.width))x\(Int(UIScreen.main.bounds.height))", "session": Trail.session,
            ],
        ]
        if includeShot, let shot, let b64 = BugShot.encode(shot) { body["image"] = b64 }
        var r = API.request("POST", "?bug=1", body: try? JSONSerialization.data(withJSONObject: body), token: token)
        r.timeoutInterval = 60
        do {
            struct Filed: Decodable { let id: String }
            let f: Filed = try await API.run(r)
            sent = f.id
            try? await Task.sleep(for: .seconds(1.2))
            dismiss()
        } catch { self.error = error.localizedDescription }
    }
}
