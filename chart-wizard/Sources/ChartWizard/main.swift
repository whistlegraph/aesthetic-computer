// ChartWizard — drag a word onto the beat, hear it, then render it.
//
// Usage:  ChartWizard [pop/<lane>/vox4/.wizard.json]
//
// The lane's chart is authored in Python (halo3.py's CHART) and rendered
// through WORLD and a C engine. Everything about that is right except the
// loop: a boundary is a float you type, and hearing it costs half a
// minute. This is the same chart with a handle on it — the blocks are the
// units halo3 will warp, the audio inside them is the take, and Render
// hands the edits back to the pipeline that ships.
import AppKit

let app = NSApplication.shared
let delegate = ChartWizardAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
