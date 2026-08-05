// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "NelameshScope",
    platforms: [.macOS(.v13)],
    targets: [
        .executableTarget(
            name: "NelameshScope",
            path: "Sources/NelameshScope"
        ),
    ]
)
