// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "SyllaWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "SyllaWizard",
            path: "Sources/SyllaWizard"
        ),
    ]
)
