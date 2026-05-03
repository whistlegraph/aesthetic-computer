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
    private static let defaultsDomain = "computer.aestheticcomputer.menuband"
    private static let styleOverrideDefaultsKey = "MenuBandFloatingPlayPaletteStyle"
    private static let styleOverrideEnvironmentKey = "MENUBAND_FLOATING_PLAY_PALETTE_STYLE"

    static var visualStyleOverride: PianoWaveformVisualStyleOverride {
        let environmentValue = ProcessInfo.processInfo.environment[styleOverrideEnvironmentKey]
        if environmentValue != nil {
            return PianoWaveformVisualStyleOverride(rawValue: environmentValue)
        }
        let defaultsValue = UserDefaults(suiteName: defaultsDomain)?.string(forKey: styleOverrideDefaultsKey)
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
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }
}
