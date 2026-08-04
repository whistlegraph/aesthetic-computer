// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "MenuFighterNative",
  platforms: [.macOS(.v14)],
  products: [.executable(name: "menu-fighter", targets: ["MenuFighterNative"])],
  targets: [
    .target(name: "TrackpadBridge", publicHeadersPath: "include"),
    .executableTarget(name: "MenuFighterNative", dependencies: ["TrackpadBridge"], linkerSettings: [.linkedFramework("Security")])
  ]
)
