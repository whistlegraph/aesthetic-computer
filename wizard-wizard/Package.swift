// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "WizardWizard",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "WizardWizard",
            path: "Sources/WizardWizard"
        ),
    ]
)
