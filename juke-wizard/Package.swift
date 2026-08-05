// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "MenuBandJuke",
    platforms: [.macOS(.v12)],
    products: [
        .library(name: "MenuBandJuke", targets: ["MenuBandJuke"]),
    ],
    dependencies: [
        .package(path: "../slab/macos-audio"),
    ],
    targets: [
        .target(
            name: "JukeDSP",
            path: "Sources/JukeDSP",
            publicHeadersPath: "include"
        ),
        .target(
            name: "MenuBandJuke",
            dependencies: [
                .product(name: "ACMacAudio", package: "macos-audio"),
                "JukeDSP",
            ],
            path: "Sources/JukeWizard",
            exclude: ["Assets"]
        ),
        .testTarget(
            name: "JukeDSPTests",
            dependencies: ["JukeDSP"],
            path: "Tests/JukeDSPTests"
        ),
        .testTarget(
            name: "JukeWizardTests",
            dependencies: ["MenuBandJuke"],
            path: "Tests/JukeWizardTests"
        ),
    ]
)
