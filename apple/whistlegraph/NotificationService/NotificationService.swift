import UserNotifications

/// Puts the piece's own picture on its "is ready" push. The payload carries
/// `image`, an https URL to the still the turn worker saved; anything that
/// goes wrong delivers the push as it came, without the picture.
final class NotificationService: UNNotificationServiceExtension {
    private var contentHandler: ((UNNotificationContent) -> Void)?
    private var content: UNMutableNotificationContent?
    private var task: URLSessionDownloadTask?

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        self.contentHandler = contentHandler
        let content = (request.content.mutableCopy() as? UNMutableNotificationContent) ?? UNMutableNotificationContent()
        self.content = content
        guard let text = request.content.userInfo["image"] as? String, let url = URL(string: text), url.scheme == "https" else {
            return deliver()
        }
        task = URLSession.shared.downloadTask(with: url) { [weak self] location, response, _ in
            guard let self else { return }
            if let location, (response as? HTTPURLResponse)?.statusCode == 200 {
                // An attachment needs a file whose extension names its type.
                let file = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".png")
                if (try? FileManager.default.moveItem(at: location, to: file)) != nil,
                   let attachment = try? UNNotificationAttachment(identifier: "piece", url: file) {
                    content.attachments = [attachment]
                }
            }
            self.deliver()
        }
        task?.resume()
    }

    override func serviceExtensionTimeWillExpire() {
        task?.cancel()
        deliver()
    }

    private func deliver() {
        guard let handler = contentHandler, let content else { return }
        contentHandler = nil
        handler(content)
    }
}
