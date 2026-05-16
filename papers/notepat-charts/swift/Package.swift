// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "NotepatCharts",
    platforms: [.macOS(.v13)],
    targets: [
        .executableTarget(
            name: "NotepatCharts",
            path: "Sources/NotepatCharts"
        ),
    ]
)
