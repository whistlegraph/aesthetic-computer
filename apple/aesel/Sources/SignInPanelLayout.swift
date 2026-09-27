import Foundation

/// Keep the authentication page's layout stable, then fit the whole widget to
/// the window. Resizing must not clip its submit button or reset typed fields.
struct SignInPanelLayout {
    static let canvas = CGSize(width: 380, height: 580)
    let scale: CGFloat
    var size: CGSize { CGSize(width: Self.canvas.width * scale, height: Self.canvas.height * scale) }

    init(available: CGSize) {
        scale = min(1, max(0, min((available.width - 24) / Self.canvas.width,
                                  (available.height - 24) / Self.canvas.height)))
    }
}
