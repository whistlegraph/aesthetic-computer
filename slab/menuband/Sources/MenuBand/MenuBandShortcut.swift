import AppKit
import Carbon

struct MenuBandShortcut: Equatable {
    let keyCode: UInt32
    let modifiers: UInt32

    static let defaultFocus = MenuBandShortcut(
        keyCode: UInt32(kVK_ANSI_K),
        modifiers: UInt32(cmdKey | controlKey | optionKey)
    )

    static let typeMode = MenuBandShortcut(
        keyCode: UInt32(kVK_ANSI_P),
        modifiers: UInt32(cmdKey | controlKey | optionKey)
    )

    var isValidForRecording: Bool {
        (modifiers & UInt32(cmdKey | controlKey | optionKey)) != 0
    }

    var isReservedForTypeMode: Bool {
        self == Self.typeMode
    }

    var displayString: String {
        var parts = ""
        if (modifiers & UInt32(controlKey)) != 0 { parts += "⌃" }
        if (modifiers & UInt32(optionKey)) != 0 { parts += "⌥" }
        if (modifiers & UInt32(shiftKey)) != 0 { parts += "⇧" }
        if (modifiers & UInt32(cmdKey)) != 0 { parts += "⌘" }
        return parts + Self.keyLabel(for: keyCode)
    }

    func matches(event: NSEvent) -> Bool {
        UInt32(event.keyCode) == keyCode &&
        Self.carbonModifiers(from: event.modifierFlags) == modifiers
    }

    static func carbonModifiers(from flags: NSEvent.ModifierFlags) -> UInt32 {
        var mask: UInt32 = 0
        if flags.contains(.command) { mask |= UInt32(cmdKey) }
        if flags.contains(.control) { mask |= UInt32(controlKey) }
        if flags.contains(.option) { mask |= UInt32(optionKey) }
        if flags.contains(.shift) { mask |= UInt32(shiftKey) }
        return mask
    }

    private static func keyLabel(for keyCode: UInt32) -> String {
        if let label = ansiKeyLabels[keyCode] { return label }
        if let label = specialKeyLabels[keyCode] { return label }
        return "Key \(keyCode)"
    }

    private static let ansiKeyLabels: [UInt32: String] = [
        0: "A", 1: "S", 2: "D", 3: "F", 4: "H", 5: "G", 6: "Z",
        7: "X", 8: "C", 9: "V", 11: "B", 12: "Q", 13: "W",
        14: "E", 15: "R", 16: "Y", 17: "T", 18: "1", 19: "2",
        20: "3", 21: "4", 22: "6", 23: "5", 24: "=", 25: "9",
        26: "7", 27: "-", 28: "8", 29: "0", 30: "]", 31: "O",
        32: "U", 33: "[", 34: "I", 35: "P", 37: "L", 38: "J",
        39: "'", 40: "K", 41: ";", 42: "\\", 43: ",", 44: "/",
        45: "N", 46: "M", 47: ".", 50: "`"
    ]

    private static let specialKeyLabels: [UInt32: String] = [
        UInt32(kVK_Space): "Space",
        UInt32(kVK_Return): "Return",
        UInt32(kVK_Tab): "Tab",
        UInt32(kVK_Escape): "Esc",
        UInt32(kVK_Delete): "Delete",
        UInt32(kVK_ForwardDelete): "FwdDel",
        UInt32(kVK_Home): "Home",
        UInt32(kVK_End): "End",
        UInt32(kVK_PageUp): "PgUp",
        UInt32(kVK_PageDown): "PgDn",
        UInt32(kVK_LeftArrow): "←",
        UInt32(kVK_RightArrow): "→",
        UInt32(kVK_UpArrow): "↑",
        UInt32(kVK_DownArrow): "↓",
        UInt32(kVK_F1): "F1",
        UInt32(kVK_F2): "F2",
        UInt32(kVK_F3): "F3",
        UInt32(kVK_F4): "F4",
        UInt32(kVK_F5): "F5",
        UInt32(kVK_F6): "F6",
        UInt32(kVK_F7): "F7",
        UInt32(kVK_F8): "F8",
        UInt32(kVK_F9): "F9",
        UInt32(kVK_F10): "F10",
        UInt32(kVK_F11): "F11",
        UInt32(kVK_F12): "F12"
    ]
}

enum MenuBandShortcutPreferences {
    private static let focusKeyCodeKey = "notepat.focusShortcut.keyCode"
    private static let focusModifiersKey = "notepat.focusShortcut.modifiers"

    static var focusShortcut: MenuBandShortcut {
        get {
            let defaults = UserDefaults.standard
            guard defaults.object(forKey: focusKeyCodeKey) != nil,
                  defaults.object(forKey: focusModifiersKey) != nil else {
                return .defaultFocus
            }
            let shortcut = MenuBandShortcut(
                keyCode: UInt32(defaults.integer(forKey: focusKeyCodeKey)),
                modifiers: UInt32(defaults.integer(forKey: focusModifiersKey))
            )
            return shortcut.isValidForRecording && !shortcut.isReservedForTypeMode
                ? shortcut
                : .defaultFocus
        }
        set {
            UserDefaults.standard.set(Int(newValue.keyCode), forKey: focusKeyCodeKey)
            UserDefaults.standard.set(Int(newValue.modifiers), forKey: focusModifiersKey)
        }
    }
}
