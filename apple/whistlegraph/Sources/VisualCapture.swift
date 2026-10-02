import UIKit
import WebKit
import CryptoKit

/// Cropped, transient evidence from the actual phone preview, never the controls.
@MainActor enum VisualCapture {
    static func hash(_ source: String) -> String {
        SHA256.hash(data: Data(source.utf8)).map { String(format: "%02x", $0) }.joined()
    }

    static func frames(view: WKWebView, rect: [String: Double], viewport: [String: Double], current: () -> Bool) async throws -> [[String: Any]] {
        let values = ["x", "y", "width", "height"].compactMap { rect[$0] }
        guard values.count == 4, values.allSatisfy({ $0.isFinite }) else { throw failure("Invalid preview bounds") }
        guard let cssWidth = viewport["width"], cssWidth.isFinite, cssWidth > 0,
              let cssHeight = viewport["height"], cssHeight.isFinite, cssHeight > 0 else { throw failure("Invalid preview viewport") }
        // CSS viewport units can differ from UIKit points. Use one scale for
        // both axes, then absorb only subpixel rounding at the view boundary.
        let cssScale = view.bounds.width / cssWidth
        let requested = CGRect(x: values[0] * cssScale, y: values[1] * cssScale,
                               width: values[2] * cssScale, height: values[3] * cssScale)
        guard abs(cssHeight * cssScale - view.bounds.height) <= 1,
              requested.width >= 8, requested.height >= 8,
              view.bounds.insetBy(dx: -1, dy: -1).contains(requested) else { throw failure("Preview is outside the visible workspace") }
        let crop = requested.intersection(view.bounds)
        let started = ProcessInfo.processInfo.systemUptime
        var frames: [[String: Any]] = []
        for index in 0..<4 {
            if index > 0 { try await Task.sleep(nanoseconds: 800_000_000) }
            try Task.checkCancellation()
            guard current(), UIApplication.shared.applicationState == .active else { throw failure("Preview changed or app left the foreground") }
            let config = WKSnapshotConfiguration()
            config.rect = crop
            config.snapshotWidth = NSNumber(value: min(384, 384 * crop.width / crop.height))
            let image: UIImage = try await withCheckedThrowingContinuation { continuation in
                view.takeSnapshot(with: config) { image, error in
                    if let image { continuation.resume(returning: image) }
                    else { continuation.resume(throwing: error ?? failure("Preview snapshot unavailable")) }
                }
            }
            try Task.checkCancellation()
            guard current() else { throw failure("Preview changed during capture") }
            // UIKit snapshots use device scale. Encode a bounded 1× PNG instead.
            let scale = min(1, 384 / max(image.size.width, image.size.height))
            let size = CGSize(width: max(1, floor(image.size.width * scale)), height: max(1, floor(image.size.height * scale)))
            let format = UIGraphicsImageRendererFormat(); format.scale = 1; format.opaque = true
            let png = UIGraphicsImageRenderer(size: size, format: format).pngData { _ in image.draw(in: CGRect(origin: .zero, size: size)) }
            let encoded = png.base64EncodedString()
            guard encoded.count <= 700_000 else { throw failure("Preview snapshot is too large") }
            frames.append(["png": encoded, "width": Int(size.width), "height": Int(size.height),
                           "atMs": (ProcessInfo.processInfo.systemUptime - started) * 1000])
        }
        return frames
    }

    private static func failure(_ message: String) -> NSError {
        NSError(domain: "VisualCapture", code: 1, userInfo: [NSLocalizedDescriptionKey: message])
    }
}
