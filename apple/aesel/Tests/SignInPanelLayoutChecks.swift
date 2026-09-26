import Foundation

@main struct SignInPanelLayoutChecks {
    static func main() {
        for window in [CGSize(width: 840, height: 680), CGSize(width: 495, height: 400),
                       CGSize(width: 300, height: 250), CGSize(width: 220, height: 160)] {
            let layout = SignInPanelLayout(available: window)
            precondition(layout.size.width <= window.width - 24 + 0.001)
            precondition(layout.size.height <= window.height - 24 + 0.001)
            precondition(abs(layout.size.width / layout.size.height - 380.0 / 580.0) < 0.001)
            precondition(layout.scale > 0 && layout.scale <= 1)
        }
        precondition(SignInPanelLayout(available: CGSize(width: 220, height: 160)).scale < 0.5)
        print("Login widget fits small windows without cropping or a minimum zoom floor.")
    }
}
