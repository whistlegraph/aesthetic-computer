// swift-tools-version: 5.9
import PackageDescription
let package = Package(name: "ACUI", platforms: [.macOS(.v14)], products: [.library(name: "ACUI", targets: ["ACUI"])], targets: [
    .target(name: "ACUI", resources: [.process("Resources")]),
])
