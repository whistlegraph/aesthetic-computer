// swift-tools-version: 5.10
import PackageDescription

let package = Package(
  name: "ThespianJas",
  platforms: [.macOS(.v13)],
  dependencies: [.package(url: "https://github.com/warrenm/GLTFKit2.git", branch: "master")],
  targets: [.executableTarget(name: "ThespianJas", dependencies: ["GLTFKit2"])]
)
