// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "ACFightRunner",
    platforms: [.macOS(.v12)],
    targets: [
        .executableTarget(
            name: "ac-fight-runner",
            linkerSettings: [
                .linkedFramework("AppKit"),
                .linkedFramework("WebKit"),
                .linkedFramework("GameController"),
            ]
        )
    ]
)
