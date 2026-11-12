// swift-tools-version: 5.9
import PackageDescription

let package = Package(name: "Airbrush", platforms: [.macOS(.v13)], targets: [
    .target(name: "AirbrushCore"),
    .executableTarget(name: "Airbrush", dependencies: ["AirbrushCore"]),
    .testTarget(name: "AirbrushCoreTests", dependencies: ["AirbrushCore"]),
])
