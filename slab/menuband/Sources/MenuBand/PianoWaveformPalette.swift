import AppKit

final class PianoWaveformPalette {
    enum State {
        case collapsed
        case expanded
    }

    private let menuBand: MenuBandController
    private let expandedPalette: FloatingPlayPaletteController
    private let collapsedPalette: MenuBarWaveformStrip
    private weak var statusItemButton: NSStatusBarButton?
    private var state: State = .collapsed
    private var dismissHandler: (() -> Void)?

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        expandedPalette = FloatingPlayPaletteController(menuBand: menuBand)
        collapsedPalette = MenuBarWaveformStrip(menuBand: menuBand)
        expandedPalette.onDismiss = { [weak self] in
            self?.state = .collapsed
            self?.dismissHandler?()
        }
        expandedPalette.onExpandCollapseToggle = { [weak self] in
            self?.collapse()
        }
        collapsedPalette.onExpandRequested = { [weak self] in
            self?.expandFromCollapsed()
        }
    }

    var expandedState: State { .expanded }
    var collapsedState: State { .collapsed }

    var onDismiss: (() -> Void)? {
        get { dismissHandler }
        set { dismissHandler = newValue }
    }

    var onFocusRelease: (() -> Void)? {
        get { expandedPalette.onFocusRelease }
        set { expandedPalette.onFocusRelease = newValue }
    }

    var onToggleKeymap: (() -> Void)? {
        get { expandedPalette.onToggleKeymap }
        set { expandedPalette.onToggleKeymap = newValue }
    }

    var isPianoFocusActive: (() -> Bool)? {
        get { expandedPalette.isPianoFocusActive }
        set { expandedPalette.isPianoFocusActive = newValue }
    }

    var isShown: Bool { expandedPalette.isShown }
    var isKeyboardFocused: Bool { expandedPalette.isKeyboardFocused }
    var isCollapsedState: Bool { state == .collapsed }

    var onStepBackward: (() -> Void)? {
        get { collapsedPalette.onStepBackward }
        set { collapsedPalette.onStepBackward = newValue }
    }

    var onStepForward: (() -> Void)? {
        get { collapsedPalette.onStepForward }
        set { collapsedPalette.onStepForward = newValue }
    }

    var onStepUp: (() -> Void)? {
        get { collapsedPalette.onStepUp }
        set { collapsedPalette.onStepUp = newValue }
    }

    var onStepDown: (() -> Void)? {
        get { collapsedPalette.onStepDown }
        set { collapsedPalette.onStepDown = newValue }
    }

    var suppressed: Bool {
        get { collapsedPalette.suppressed }
        set { collapsedPalette.suppressed = newValue }
    }

    var isDocked: Bool { collapsedPalette.isDocked }

    func toggleFromShortcut() {
        if expandedPalette.isShown {
            state = .collapsed
            expandedPalette.toggleFromShortcut()
            return
        }
        collapsedPalette.dismiss()
        state = .expanded
        expandedPalette.toggleFromShortcut()
    }

    func showFromCommand(restoringTo previousApp: NSRunningApplication? = nil) {
        collapsedPalette.dismiss()
        state = .expanded
        expandedPalette.showFromCommand(restoringTo: previousApp)
    }

    func show(restoringTo previousApp: NSRunningApplication? = nil) {
        collapsedPalette.dismiss()
        state = .expanded
        expandedPalette.show(restoringTo: previousApp)
    }

    func dismiss(reason: FloatingPlayPaletteController.DismissReason = .programmatic) {
        state = .collapsed
        expandedPalette.dismiss(reason: reason)
    }

    func refresh() {
        expandedPalette.refresh()
    }

    func clearInteraction() {
        expandedPalette.clearInteraction()
    }

    func releaseKeyboardFocus() {
        expandedPalette.releaseKeyboardFocus()
    }

    func warmUp() {
        collapsedPalette.warmUp()
    }

    func showIfNeeded() {
        guard state == .collapsed else { return }
        collapsedPalette.reposition(statusItemButton: statusItemButton)
        collapsedPalette.refreshAppearance()
        collapsedPalette.showIfNeeded()
    }

    func scheduleHide() {
        guard state == .collapsed else { return }
        collapsedPalette.scheduleHide()
    }

    func reposition(statusItemButton: NSStatusBarButton?) {
        self.statusItemButton = statusItemButton
        collapsedPalette.reposition(statusItemButton: statusItemButton)
    }

    func refreshAppearance() {
        guard state == .collapsed else { return }
        collapsedPalette.refreshAppearance()
    }

    func registerArrowInput() {
        guard state == .collapsed else { return }
        collapsedPalette.registerArrowInput()
    }

    private func collapse() {
        guard state != .collapsed else { return }
        state = .collapsed
        expandedPalette.dismiss(reason: .closeButton)
        collapsedPalette.reposition(statusItemButton: statusItemButton)
        collapsedPalette.refreshAppearance()
        collapsedPalette.showIfNeeded()
        if menuBand.litNotes.isEmpty {
            collapsedPalette.scheduleHide()
        }
    }

    private func expandFromCollapsed() {
        guard state != .expanded else { return }
        state = .expanded
        collapsedPalette.dismiss()
        expandedPalette.show()
    }
}
