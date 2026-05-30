// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "WaveWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "WaveWizard",
            path: "Sources/WaveWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
    ]
)
