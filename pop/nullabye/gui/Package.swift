// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "SineabyeLive",
  platforms: [.macOS(.v14)],
  products: [.executable(name: "sineabye-live", targets: ["SineabyeLive"])],
  targets: [.executableTarget(name: "SineabyeLive")]
)
