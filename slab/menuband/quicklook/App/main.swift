// Tiny agent container whose only job is to host (and get registered for) the
// ScorePreview QuickLook extension. It never shows UI.
import Cocoa

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
app.run()
