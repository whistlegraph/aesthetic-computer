// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "DateWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "DateWizard",
            path: "Sources/DateWizard",
            resources: [
                .copy("Assets"),
            ]
        ),
    ]
)
