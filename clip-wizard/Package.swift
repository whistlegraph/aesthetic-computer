// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "ClipWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "ClipWizard",
            path: "Sources/ClipWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
    ]
)
