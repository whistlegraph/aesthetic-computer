import SwiftUI
import UIKit

// Track only the finger that starts on Talk. A second finger on the preview
// belongs to the chalk view and must not end or cancel this hold.
struct TalkHoldInput: UIViewRepresentable {
    var enabled: Bool
    var began: () -> Void
    var moved: (CGFloat) -> Void = { _ in }
    var ended: () -> Void
    var cancelled: () -> Void

    func makeUIView(context: Context) -> TalkHoldView {
        let view = TalkHoldView()
        view.backgroundColor = .clear
        view.isMultipleTouchEnabled = false
        view.isExclusiveTouch = false
        return view
    }

    func updateUIView(_ view: TalkHoldView, context: Context) {
        view.began = began; view.moved = moved; view.ended = ended; view.cancelled = cancelled
        view.isUserInteractionEnabled = enabled
    }

    static func dismantleUIView(_ view: TalkHoldView, coordinator: ()) {
        view.cancelTracking()
    }
}

final class TalkHoldView: UIView {
    var began: (() -> Void)?
    var moved: ((CGFloat) -> Void)?
    private var origin: CGPoint = .zero
    var ended: (() -> Void)?
    var cancelled: (() -> Void)?
    private var finger: UITouch?

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard finger == nil, let touch = touches.first else { return }
        finger = touch; origin = touch.location(in: window)
        began?()
    }

    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let finger, touches.contains(finger) else { return }
        let delta = finger.location(in: window).x - origin.x
        moved?(delta)
    }

    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let finger, touches.contains(finger) else { return }
        self.finger = nil
        ended?()
    }

    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let finger, touches.contains(finger) else { return }
        cancelTracking()
    }

    func cancelTracking() {
        guard finger != nil else { return }
        finger = nil
        cancelled?()
    }
}
