// NotificationService, 2026.11.18
// Gives a push its picture. The server sends an alert with `mutable-content`
// and an `image` URL (shared/push.mjs); iOS hands the alert here first, this
// downloads the media and attaches it, and the banner shows a thumbnail that
// opens to the full image, GIF or clip on a long press. With no `image`, or
// when the download fails or runs out of time, the plain alert is delivered.

import UserNotifications
import UIKit

final class NotificationService: UNNotificationServiceExtension {
    private var deliver: ((UNNotificationContent) -> Void)?
    private var content: UNMutableNotificationContent?
    private var task: URLSessionDownloadTask?

    // What a notification may carry, by the type the server answers with.
    // iOS will not attach anything else (no WebP, no SVG).
    static let attachable: [String: String] = [
        "image/jpeg": "jpg", "image/png": "png", "image/gif": "gif",
        "video/mp4": "mp4", "audio/mpeg": "mp3", "audio/mp4": "m4a",
    ]
    static let maxBytes: Int64 = 10 * 1024 * 1024

    static func mediaURL(_ payload: [AnyHashable: Any]) -> URL? {
        guard let text = payload["image"] as? String, let url = URL(string: text),
              url.scheme == "https", url.host != nil else { return nil }
        return url
    }

    override func didReceive(
        _ request: UNNotificationRequest,
        withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void
    ) {
        deliver = contentHandler
        content = request.content.mutableCopy() as? UNMutableNotificationContent
        guard let content, let url = Self.mediaURL(request.content.userInfo) else {
            return finish()
        }
        var fetch = URLRequest(url: url, timeoutInterval: 20)
        fetch.setValue("image/png,image/jpeg,image/gif,video/mp4;q=0.9,*/*;q=0.5", forHTTPHeaderField: "Accept")
        task = URLSession.shared.downloadTask(with: fetch) { [weak self] file, response, _ in
            defer { self?.finish() }
            guard let file, let http = response as? HTTPURLResponse, http.statusCode == 200,
                  http.expectedContentLength <= Self.maxBytes,
                  let attachment = Self.attachment(from: file, type: http.mimeType) else { return }
            content.attachments = [attachment]
        }
        task?.resume()
    }

    // iOS is about to stop this extension: send what there is.
    override func serviceExtensionTimeWillExpire() {
        task?.cancel()
        finish()
    }

    private func finish() {
        guard let deliver, let content else { return }
        self.deliver = nil
        deliver(content)
    }

    // The download is deleted when the task's handler returns, and iOS reads an
    // attachment's kind from its extension, so move it to a named file first.
    static func attachment(from file: URL, type: String?) -> UNNotificationAttachment? {
        let folder = FileManager.default.temporaryDirectory
        let name = UUID().uuidString
        if let ext = attachable[type?.lowercased() ?? ""] {
            let named = folder.appendingPathComponent(name + "." + ext)
            guard (try? FileManager.default.moveItem(at: file, to: named)) != nil else { return nil }
            return try? UNNotificationAttachment(identifier: name, url: named)
        }
        // Anything else that decodes as a picture (WebP, HEIC) goes as a PNG.
        guard let data = try? Data(contentsOf: file), let image = UIImage(data: data),
              let png = image.pngData() else { return nil }
        let named = folder.appendingPathComponent(name + ".png")
        guard (try? png.write(to: named)) != nil else { return nil }
        return try? UNNotificationAttachment(identifier: name, url: named)
    }
}
