// swift-tools-version: 5.9
import PackageDescription
let package = Package(name: "NoPaint", platforms: [.macOS(.v14)], targets: [
    .executableTarget(name: "NoPaint"),
    .testTarget(name: "NoPaintTests", dependencies: ["NoPaint"]),
])
