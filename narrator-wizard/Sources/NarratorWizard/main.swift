// NarratorWizard — record a screenplay one reviewable line at a time.
//
// Usage: NarratorWizard <narration-spec.json>
import AppKit

let app = NSApplication.shared
let delegate = NarratorWizardAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
