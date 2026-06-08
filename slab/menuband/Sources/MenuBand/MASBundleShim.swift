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

extension Bundle {
    /// The bundle to load app resources (fonts, sheet.html, the keymaps PDF,
    /// looking-for-players.png, verovio wasm, default.metallib) from.
    ///
    /// Prefers `Bundle.main` — the installed `.app` and the Mac App Store build
    /// keep these in `Contents/Resources/`, which codesign seals cleanly. Only
    /// falls back to `Bundle.module` (the SwiftPM nested `MenuBand_MenuBand.bundle`)
    /// for `swift run` dev builds.
    ///
    /// Why not just `Bundle.module`: Swift 6.3's generated SwiftPM accessor for
    /// this executable target resolves the nested bundle at `Bundle.main.bundleURL`
    /// — i.e. the `.app` ROOT, NOT `Contents/Resources` — and anything at the
    /// bundle root makes `codesign --strict` fail ("unsealed contents present in
    /// the bundle root"), breaking notarization. Flattening the resources into
    /// `Contents/Resources` and reading them via `Bundle.main` avoids that
    /// entirely; `Bundle.module` is never touched in the installed app, so its
    /// fatalError accessor never fires.
    static var appResources: Bundle {
        if Bundle.main.url(forResource: "Bravura", withExtension: "otf") != nil {
            return Bundle.main
        }
        return Bundle.module
    }
}
