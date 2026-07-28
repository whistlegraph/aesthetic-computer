// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "JukeWizard",
    platforms: [.macOS(.v12)],
    dependencies: [
        .package(path: "../slab/macos-audio"),
    ],
    targets: [
        .target(
            name: "JukeDSP",
            path: "Sources/JukeDSP",
            publicHeadersPath: "include"
        ),
        .executableTarget(
            name: "JukeWizard",
            dependencies: [
                .product(name: "ACMacAudio", package: "macos-audio"),
                "JukeDSP",
            ],
            path: "Sources/JukeWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
        .testTarget(
            name: "JukeDSPTests",
            dependencies: ["JukeDSP"],
            path: "Tests/JukeDSPTests"
        ),
    ]
)
