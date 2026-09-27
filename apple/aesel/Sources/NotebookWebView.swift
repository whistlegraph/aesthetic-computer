#if os(macOS)
import AppKit
import WebKit

/// The notebook grows to its document height; its enclosing sheet owns scrolling.
final class NotebookWebView: WKWebView {
    private var wheelMonitor: Any?

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if let wheelMonitor { NSEvent.removeMonitor(wheelMonitor) }
        wheelMonitor = nil
        guard window != nil else { return }
        wheelMonitor = NSEvent.addLocalMonitorForEvents(matching: .scrollWheel) { [weak self] event in
            guard let self, self.routeScrollWheel(event) else { return event }
            return nil
        }
    }

    deinit {
        if let wheelMonitor { NSEvent.removeMonitor(wheelMonitor) }
    }

    @discardableResult
    func routeScrollWheel(_ event: NSEvent) -> Bool {
        guard event.window === window, let window, let content = window.contentView,
              !isHiddenOrHasHiddenAncestor, let scroll = enclosingScrollView,
              abs(event.scrollingDeltaY) >= abs(event.scrollingDeltaX) else { return false }
        // Hit-test from the window so preview and popover overlays keep their input.
        let point = content.convert(event.locationInWindow, from: nil)
        guard let hit = content.hitTest(point), hit === self || hit.isDescendant(of: self) else { return false }
        scroll.verticalScrollElasticity = .allowed
        // Preserve the original phase, precise deltas and momentum for AppKit's physics.
        scroll.scrollWheel(with: event)
        return true
    }
}
#endif
