// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "slab-menubar-swift",
    platforms: [.macOS(.v11)],
    targets: [
        .executableTarget(
            name: "slab-menubar-swift",
            path: "Sources/SlabMenubar"
        ),
    ]
)
