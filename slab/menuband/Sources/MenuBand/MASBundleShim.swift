import Foundation

// SwiftPM synthesizes `Bundle.module` (via a generated
// resource_bundle_accessor.swift) for the package build. The hand-built
// Xcode "Menu Band MAS" target — the one used for Mac App Store archiving —
// does NOT get that accessor, and its resources live directly in the app's
// main bundle instead of a nested `MenuBand_MenuBand.bundle`. This shim
// supplies `Bundle.module` for that target so the six existing
// `Bundle.module.url(...)` lookups (fonts, sheet.html, the keymaps PDF, the
// WaveformShaders.metal source) keep working unchanged.
//
// Guarded on XCODE_APP_TARGET — a flag set ONLY by project.yml's Xcode
// target — NOT on MAC_APP_STORE. That distinction matters: the
// compile-verification `swift build -Xswiftc -DMAC_APP_STORE` still runs
// under SwiftPM, where the generated `Bundle.module` accessor is present;
// defining this shim there too would be a duplicate-declaration error. Only
// the Xcode target (which lacks the SwiftPM accessor) gets the shim.
#if XCODE_APP_TARGET
extension Bundle {
    static var module: Bundle { Bundle.main }
}
#endif
