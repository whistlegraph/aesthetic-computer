// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MenuBandRaytracer",
    platforms: [.macOS(.v14)],
    targets: [
        .executableTarget(
            name: "MenuBandRaytracer",
            resources: [.copy("MenuBand.metal")]
        )
    ]
)
