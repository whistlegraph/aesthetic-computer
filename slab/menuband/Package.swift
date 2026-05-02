// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "MenuBand",
    platforms: [.macOS(.v11)],
    targets: [
        .executableTarget(
            name: "MenuBand",
            path: "Sources/MenuBand",
            resources: [
                .process("Resources"),
                // SwiftPM doesn't auto-compile .metal files in
                // executable targets — declaring it as a processed
                // resource makes SwiftPM emit a default.metallib into
                // the module bundle, which `device.makeDefaultLibrary(
                // bundle: .module)` then finds at runtime. Without
                // this the visualizer renders solid black: the Metal
                // pipeline fails with "no default library was found".
                .process("WaveformShaders.metal"),
            ]
        ),
    ]
)
