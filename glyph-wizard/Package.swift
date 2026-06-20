// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "GlyphWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "GlyphWizard",
            path: "Sources/GlyphWizard"
        ),
    ]
)
