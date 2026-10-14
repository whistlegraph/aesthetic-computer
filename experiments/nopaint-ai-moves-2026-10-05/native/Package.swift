// swift-tools-version: 5.9
import PackageDescription
let package = Package(name: "NoPaint", platforms: [.macOS(.v14)], dependencies: [
    .package(path: "../../../system/public/aesthetic.computer/lib/ui"),
], targets: [
    .executableTarget(name: "NoPaint", dependencies: [.product(name: "ACUI", package: "ui")]),
    .testTarget(name: "NoPaintTests", dependencies: ["NoPaint"]),
])
