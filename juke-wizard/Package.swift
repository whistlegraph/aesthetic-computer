// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "JukeWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "JukeWizard",
            path: "Sources/JukeWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
    ]
)
