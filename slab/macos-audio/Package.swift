// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "ACMacAudio",
    platforms: [.macOS(.v11)],
    products: [
        .library(name: "ACMacAudio", targets: ["ACMacAudio"]),
        .executable(name: "ac-audio-room", targets: ["ACAudioRoomCLI"]),
    ],
    targets: [
        .target(
            name: "ACMacAudio",
            linkerSettings: [
                .linkedFramework("AVFoundation"),
                .linkedFramework("AudioToolbox"),
                .linkedFramework("CoreAudio"),
                .linkedFramework("Network"),
            ]
        ),
        .executableTarget(
            name: "ACAudioRoomCLI",
            dependencies: ["ACMacAudio"]
        ),
        .testTarget(
            name: "ACMacAudioTests",
            dependencies: ["ACMacAudio"]
        ),
    ]
)
