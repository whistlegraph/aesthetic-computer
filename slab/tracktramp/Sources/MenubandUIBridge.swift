import AppKit

// PitchBendCursor.swift is compiled directly from Menu Band. These two seams
// are supplied by the larger app there and by this tiny shell here.
enum KeyboardIconRenderer {
    static var accent: NSColor { NSColor.controlAccentColor }
}

extension Bundle {
    static var module: Bundle { .main }
    static var appResources: Bundle { .main }
}
