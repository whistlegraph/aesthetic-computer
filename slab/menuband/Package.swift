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
            ],
            linkerSettings: [
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
    ]
)
