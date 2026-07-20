// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "JukeWizard",
    platforms: [.macOS(.v12)],
    dependencies: [
        .package(path: "../slab/macos-audio"),
    ],
    targets: [
        .executableTarget(
            name: "JukeWizard",
            dependencies: [
                .product(name: "ACMacAudio", package: "macos-audio"),
            ],
            path: "Sources/JukeWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
    ]
)
