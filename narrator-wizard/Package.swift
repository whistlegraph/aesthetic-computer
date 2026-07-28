// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "NarratorWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "NarratorWizard",
            path: "Sources/NarratorWizard"
        ),
    ]
)
