// swift-tools-version:5.9
import PackageDescription

// The scrolling output waveform that stands behind an AC piece preview, shared
// by every native host that shows one: the Slab card and the Aesel app.
// It lives beside slab/bin so the minis' rsync of slab carries it too.
let package = Package(
    name: "ACWaveform",
    platforms: [.macOS(.v11), .iOS(.v15)],
    products: [
        .library(name: "ACWaveform", targets: ["ACWaveform"]),
    ],
    targets: [
        .target(name: "ACWaveform"),
        .testTarget(name: "ACWaveformTests", dependencies: ["ACWaveform"]),
    ]
)
