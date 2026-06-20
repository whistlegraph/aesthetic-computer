// swift-tools-version:5.9
import PackageDescription

// arena-native — a standalone first-person Metal renderer of AC's `arena`
// piece. The player physics (PMove.swift) and world geometry
// (ArenaWorld.swift) are faithful ports of the shared JS modules
// system/public/aesthetic.computer/lib/{pmove,arena-world}.mjs so the
// movement feel matches the web arena. The Metal layer replaces AC's
// Form/cam-doll immediate-mode renderer.
//
// The Metal shader is compiled at runtime from an embedded source string
// (see Shaders.swift) — that keeps the build a plain `swift build`/`swift
// run` with no .metallib resource compilation step.
let package = Package(
    name: "ArenaNative",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "ArenaNative",
            path: "Sources/ArenaNative"
        )
    ]
)
