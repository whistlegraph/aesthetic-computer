import AppKit

enum PianoWaveformVisualStyleOverride: String {
    case automatic
    case liquid
    case legacy

    init(rawValue: String?) {
        switch rawValue?.trimmingCharacters(in: .whitespacesAndNewlines).lowercased() {
        case Self.liquid.rawValue:
            self = .liquid
        case Self.legacy.rawValue:
            self = .legacy
        default:
            self = .automatic
        }
    }
}

enum PianoWaveformWindowStyle {
    private static let styleOverrideDefaultsKey = "MenuBandFloatingPlayPaletteStyle"
    private static let styleOverrideEnvironmentKey = "MENUBAND_FLOATING_PLAY_PALETTE_STYLE"

    static var visualStyleOverride: PianoWaveformVisualStyleOverride {
        let environmentValue = ProcessInfo.processInfo.environment[styleOverrideEnvironmentKey]
        if environmentValue != nil {
            return PianoWaveformVisualStyleOverride(rawValue: environmentValue)
        }
        let defaultsValue = UserDefaults.standard.string(forKey: styleOverrideDefaultsKey)
            ?? UserDefaults.standard.string(forKey: styleOverrideDefaultsKey)
        return PianoWaveformVisualStyleOverride(rawValue: defaultsValue)
    }

    static var shouldUseLiquidGlass: Bool {
        switch visualStyleOverride {
        case .liquid:
            if #available(macOS 26.0, *) { return true }
            return false
        case .legacy:
            return false
        case .automatic:
            if #available(macOS 26.0, *) { return true }
            return false
        }
    }
}

final class PianoWaveformPanel: NSPanel {
    var allowsSurfaceDrag = false

    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }
    /// Keep the glass + control chrome painted in "active" state at
    /// all times. The popover steals key focus when it opens, which
    /// would otherwise shift NSGlassEffectView into a desaturated/
    /// reduced-blur "inactive" appearance — visually the panel
    /// looked thicker the moment the popover went up. Pinning
    /// `isMainWindow` to true keeps the glass uniform regardless of
    /// focus state.
    override var isMainWindow: Bool { true }

    override func sendEvent(_ event: NSEvent) {
        if event.type == .leftMouseDown,
           allowsSurfaceDrag,
           shouldBeginSurfaceDrag(with: event) {
            performDrag(with: event)
            return
        }
        super.sendEvent(event)
    }

    private func shouldBeginSurfaceDrag(with event: NSEvent) -> Bool {
        guard let contentView else { return false }
        let location = event.locationInWindow
        guard let hitView = contentView.hitTest(location) else { return false }

        var view: NSView? = hitView
        while let current = view {
            if (current is NSControl && !(current is NSTextField)) ||
                current is PianoKeyboardView ||
                current is QwertyLayoutView {
                return false
            }
            if current is ExpandedPianoWaveformView ||
                current is CollapsedPianoWaveformView ||
                current is WaveformView {
                return true
            }
            view = current.superview
        }
        return false
    }
}
