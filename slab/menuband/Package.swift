// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "MenuBand",
    platforms: [.macOS(.v11)],
    targets: [
        .executableTarget(
            name: "MenuBand",
            path: "Sources/MenuBand",
            resources: [
                .process("Resources"),
            ]
        ),
    ]
)
