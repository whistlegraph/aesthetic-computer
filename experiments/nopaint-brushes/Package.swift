// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "NoPaintBrushes",
    platforms: [.macOS(.v14)],
    products: [
        .library(name: "BrushCore", targets: ["BrushCore"]),
        .executable(name: "brush-runner", targets: ["BrushRunner"]),
        .executable(name: "brush-app", targets: ["BrushApp"]),
    ],
    targets: [
        .target(name: "BrushCore"),
        .executableTarget(name: "BrushRunner", dependencies: ["BrushCore"]),
        .executableTarget(name: "BrushApp", dependencies: ["BrushCore"]),
    ]
)
