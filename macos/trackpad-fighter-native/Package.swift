// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "TrackpadFighter",
  platforms: [.macOS(.v14)],
  products: [.executable(name: "trackpad-fighter", targets: ["MenuFighterNative"])],
  targets: [
    .target(name: "TrackpadBridge", publicHeadersPath: "include"),
    .executableTarget(name: "MenuFighterNative", dependencies: ["TrackpadBridge"], linkerSettings: [.linkedFramework("Security")])
  ]
)
