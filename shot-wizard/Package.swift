// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "ShotWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "ShotWizard",
            path: "Sources/ShotWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
    ]
)
