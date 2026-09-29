// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "slab-menubar-swift",
    platforms: [.macOS(.v11)],
    dependencies: [
        // Shared with the Aesel app; rides the minis' rsync beside slab/bin.
        .package(path: "../packages/ACWaveform"),
    ],
    targets: [
        .executableTarget(
            name: "slab-menubar-swift",
            dependencies: ["ACWaveform"],
            path: "Sources/SlabMenubar"
        ),
    ]
)
