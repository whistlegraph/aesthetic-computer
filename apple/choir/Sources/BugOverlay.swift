import SwiftUI
import UIKit

/// Fía's ladybug as its own floating layer: a window above every screen
/// (chat, Discover, listings, comments, sheets), draggable into whatever
/// negative space a view has, snapping to the nearest side and remembered.
/// Touches anywhere else pass straight through to the app.
@MainActor final class BugOverlay {
    static let shared = BugOverlay()
    private var window: PassthroughWindow?
    /// Where the bug is now, in window points; the only spot that takes touches.
    var bugFrame: CGRect = .zero

    func update(for account: Account) {
        if account.handle == "@fifi" { show(account) } else { hide() }
    }

    func show(_ account: Account) {
        guard window == nil,
              let scene = UIApplication.shared.connectedScenes.compactMap({ $0 as? UIWindowScene }).first(where: { $0.activationState == .foregroundActive })
                ?? UIApplication.shared.connectedScenes.compactMap({ $0 as? UIWindowScene }).first else { return }
        let w = PassthroughWindow(windowScene: scene)
        w.windowLevel = .alert + 1
        w.backgroundColor = .clear
        let host = UIHostingController(rootView: FloatingBug().environmentObject(account))
        host.view.backgroundColor = .clear
        w.rootViewController = host
        w.isHidden = false
        window = w
    }

    func hide() { window?.isHidden = true; window = nil }

    /// The app's own window, for screenshots that leave the bug out.
    static var appWindow: UIWindow? {
        UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }
            .flatMap(\.windows).first { !($0 is PassthroughWindow) && $0.windowLevel == .normal && !$0.isHidden }
    }
}

/// Only the bug (and its sheet) take touches; the rest falls through.
/// SwiftUI answers every hit test with its hosting view, so the bug can't be
/// told from empty space by the view returned; test against its frame instead
/// (Fía, 10-12: "the bug tap is still not working").
final class PassthroughWindow: UIWindow {
    override func hitTest(_ point: CGPoint, with event: UIEvent?) -> UIView? {
        if rootViewController?.presentedViewController != nil { return super.hitTest(point, with: event) }
        guard BugOverlay.shared.bugFrame.insetBy(dx: -10, dy: -10).contains(point) else { return nil }
        return super.hitTest(point, with: event)
    }
}

struct FloatingBug: View {
    @AppStorage("bug-x") private var savedX = 1.0      // 0 = left edge, 1 = right edge
    @AppStorage("bug-y") private var savedY = 0.72     // fraction of the height
    @State private var drag: CGSize = .zero
    @State private var dragging = false
    @State private var shot: UIImage?
    @State private var open = false
    private let size: CGFloat = 46

    var body: some View {
        GeometryReader { geo in
            let inset = geo.safeAreaInsets
            let minX = size / 2 + 8, maxX = geo.size.width - size / 2 - 8
            let minY = size / 2 + inset.top + 8, maxY = geo.size.height - size / 2 - inset.bottom - 8
            let base = CGPoint(x: minX + (maxX - minX) * savedX, y: minY + (maxY - minY) * savedY)
            Image(systemName: "ladybug.fill")
                .font(.system(size: 22, weight: .semibold))
                .foregroundStyle(.red)
                .frame(width: size, height: size)
                .background(Circle().fill(.white).shadow(color: .black.opacity(0.25), radius: dragging ? 10 : 5, y: dragging ? 5 : 2))
                .overlay(Circle().strokeBorder(.red.opacity(0.25), lineWidth: 1))
                .scaleEffect(dragging ? 1.12 : 1)
                .onGeometryChange(for: CGRect.self) { $0.frame(in: .global) } action: { BugOverlay.shared.bugFrame = $0 }
                .position(x: base.x + drag.width, y: base.y + drag.height)
                .gesture(
                    DragGesture(minimumDistance: 4)
                        .onChanged { drag = $0.translation; dragging = true }
                        .onEnded { v in
                            let x = min(max(base.x + v.translation.width, minX), maxX)
                            let y = min(max(base.y + v.translation.height, minY), maxY)
                            withAnimation(.spring(response: 0.35, dampingFraction: 0.75)) {
                                savedX = x < geo.size.width / 2 ? 0 : 1          // snap to a side
                                savedY = (y - minY) / max(maxY - minY, 1)
                                drag = .zero; dragging = false
                            }
                        }
                )
                .simultaneousGesture(TapGesture().onEnded {
                    shot = BugShot.capture()
                    Trail.send("bug:open")
                    open = true
                })
                .accessibilityLabel("Report a bug")
        }
        .ignoresSafeArea()
        .sheet(isPresented: $open) { BugSheet(shot: shot) }
    }
}
