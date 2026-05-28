// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "MenuBand",
    platforms: [.macOS(.v11)],
    targets: [
        // Tiny always-running daemon whose only job is to watch for the
        // double-tap right-Command gesture and relaunch Menu Band.app
        // if its process isn't currently running. When MenuBand IS
        // running, the launcher no-ops — the main app's own
        // double-tap handler in AppDelegate fires instead.
        .executableTarget(
            name: "MenuBandLauncher",
            path: "Sources/MenuBandLauncher"
        ),
        .executableTarget(
            name: "MenuBand",
            path: "Sources/MenuBand",
            exclude: [
                // Docs that live alongside source modules for discoverability —
                // SwiftPM nags about "unhandled files" without an explicit exclude.
                "KidLisp/README.md",
            ],
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
            ],
            linkerSettings: [
                .linkedFramework("IOKit"),
                // Embed the package-root Info.plist into the dev
                // binary so macOS TCC has an
                // NSMicrophoneUsageDescription to show when
                // AVCaptureDevice triggers the mic permission
                // prompt. Without this the `swift run` debug
                // binary appears un-bundled and TCC silently
                // denies microphone access. install.sh copies
                // the same plist into the bundled .app so prod
                // and dev resolve permissions identically.
                .unsafeFlags([
                    "-Xlinker", "-sectcreate",
                    "-Xlinker", "__TEXT",
                    "-Xlinker", "__info_plist",
                    "-Xlinker", "Info.plist",
                ]),
            ]
        ),
        .testTarget(
            name: "MenuBandTests",
            dependencies: ["MenuBand"],
            path: "Tests/MenuBandTests"
        ),
    ]
)
