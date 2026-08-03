// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "HitboxWIP",
    platforms: [.macOS(.v13)],
    targets: [
        .systemLibrary(
            name: "CLibUSB",
            pkgConfig: "libusb-1.0",
            providers: [.brew(["libusb"])]
        ),
        .executableTarget(
            name: "hitboxwip",
            dependencies: ["CLibUSB"],
            resources: [.process("controller-reference.png")],
            linkerSettings: [
                .linkedFramework("AppKit"),
                .linkedFramework("AVFoundation"),
                .linkedFramework("AudioToolbox"),
                .linkedFramework("GameController"),
            ]
        ),
    ]
)
