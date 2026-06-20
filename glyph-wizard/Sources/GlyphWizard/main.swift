// GlyphWizard — native macOS AppKit type-authoring wizard for Aesthetic Inc.
//
// From-scratch parametric glyph generation: you author glyph SKELETONS
// (centerlines), and the TypeWizard axes (weight, contrast, width, slant,
// x-height, tracking) expand them into filled outlines, live. Export to UFO,
// then compile to OTF/TTF with fontTools (see bin/compile.sh).
//
// Sibling to date-wizard/, wave-wizard/, clip-wizard/, juke-wizard/,
// shot-wizard/ — same conventions: a Swift Package executableTarget, AppKit
// (NSApplication + AppDelegate + NSWindow + custom-drawn NSView). NOT SwiftUI.
//
// Prototype lineage: the regarde web labs (platter + seed→parametric wizard)
// proved the concept; GlyphWizard is the homegrown, font-producing version.
import AppKit

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
