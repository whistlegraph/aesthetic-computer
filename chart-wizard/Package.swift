// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "ChartWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "ChartWizard",
            path: "Sources/ChartWizard",
            resources: [.copy("Assets")]
        ),
    ]
)
