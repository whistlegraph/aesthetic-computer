// DateWizard — native macOS AppKit wizard for the AesthetiCal calendar.
//
// A pop-up window that shows the user's WEEK (Sun→Sat) and lets them
// schedule. It reads/writes the AesthetiCal backend over HTTPS and
// overlays the user's Google calendar(s) read-only next to their
// editable aesthetical events.
//
// Sibling to wave-wizard/, clip-wizard/, juke-wizard/ — same conventions:
// a Swift Package executableTarget, AppKit (NSApplication + AppDelegate +
// NSWindow + custom-drawn NSView subclasses). NOT SwiftUI.
import AppKit

let app = NSApplication.shared
let delegate = DateWizardAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
