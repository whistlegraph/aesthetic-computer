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

// Headless: `swift run GlyphWizard contact <outDir>` renders the 128-variation
// light+dark contact sheets and exits, no window.
if let ci = CommandLine.arguments.firstIndex(of: "contact") {
    _ = NSApplication.shared   // init AppKit for text/bitmap drawing
    let outDir = CommandLine.arguments.count > ci + 1 ? CommandLine.arguments[ci + 1] : FileManager.default.currentDirectoryPath
    ContactSheet.run(outDir: outDir)
    exit(0)
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
