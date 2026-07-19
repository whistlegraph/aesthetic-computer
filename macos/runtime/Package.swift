// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "ACNativeRuntime",
  platforms: [.macOS(.v14)],
  products: [.library(name: "ACNativeRuntime", targets: ["ACNativeRuntime"])],
  targets: [
    .target(name: "ACNativeRuntime"),
    .testTarget(name: "ACNativeRuntimeTests", dependencies: ["ACNativeRuntime"]),
  ]
)
