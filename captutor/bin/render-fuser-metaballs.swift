// Bake the canonical twelve-node Fuser mark as a smooth 3D metaball surface.
// Spin delivery is rendered above display size and Lanczos-downsampled while
// encoding, providing full-scene antialiasing without runtime GPU cost.

import AppKit
import Foundation
import simd

guard CommandLine.arguments.count >= 2 else {
    fputs("usage: render-fuser-metaballs <output-directory> [--spin] [--frames 480] [--size 128]\n", stderr)
    exit(2)
}

private let output = URL(fileURLWithPath: CommandLine.arguments[1], isDirectory: true)
private let arguments = Array(CommandLine.arguments.dropFirst(2))
private let spinMode = arguments.contains("--spin")
private func integerArgument(_ name: String, fallback: Int) -> Int {
    guard let index = arguments.firstIndex(of: name), arguments.indices.contains(index + 1),
          let value = Int(arguments[index + 1]) else { return fallback }
    return value
}
// Delivery spin renders one unique ray-marched view for every encoded frame.
// Use --size 256 for the canonical 128px asset: its encoder downsamples 2:1,
// then Captutor downsamples once more to the actual side-mark size.
private let frameSize = spinMode ? integerArgument("--size", fallback:128) : 384
private let columns = 1
private let rows = 1
private let frameCount = spinMode ? integerArgument("--frames", fallback:480) : columns * rows
private let sheetWidth = columns * frameSize
private let sheetHeight = rows * frameSize

private struct Palette {
    let base: SIMD3<Float>
    let accent: SIMD3<Float>
    let edge: SIMD3<Float>
}

private let palettes = [
    // Obsidian, pearl, and graphite remain strictly neutral. Their different
    // values make overlapping marks legible without leaving Fuser's B/W system.
    Palette(base: SIMD3(repeating: 0.035), accent: SIMD3(repeating: 1.00), edge: SIMD3(repeating: 0.005)),
    Palette(base: SIMD3(repeating: 0.76), accent: SIMD3(repeating: 1.00), edge: SIMD3(repeating: 0.16)),
    Palette(base: SIMD3(repeating: 0.22), accent: SIMD3(repeating: 0.94), edge: SIMD3(repeating: 0.018)),
]

// Centers are taken directly from captutor/assets/fuser-mark.svg. Normalizing
// around its 24×24 viewBox preserves the production mark's proportions.
private let svgCenters: [(Float, Float)] = [
    (15.1507, 2.5838), (21.4201, 2.5838), (8.8818, 2.5838),
    (8.8818, 8.86312), (2.58145, 8.83282), (2.61171, 15.1369),
    (2.61171, 21.4162), (8.8818, 21.4162), (15.1507, 21.4162),
    (15.1507, 15.1369), (21.4201, 15.1369), (21.4201, 8.86312),
]
private let sourceCenters: [SIMD3<Float>] = svgCenters.enumerated().map { index, point in
    let x = (point.0 - 12) / 7.75
    let y = (12 - point.1) / 7.75
    // A tiny alternating depth keeps the front silhouette faithful while the
    // side view reveals that this is a living cluster, not a flat extrusion.
    let z = sin(Float(index) * 1.71) * 0.055
    return SIMD3(x, y, z)
}

@Sendable @inline(__always) private func rotate(_ point: SIMD3<Float>, yaw: Float, pitch: Float) -> SIMD3<Float> {
    let cy = cos(yaw), sy = sin(yaw)
    let cp = cos(pitch), sp = sin(pitch)
    let yTurned = SIMD3(point.x * cy + point.z * sy, point.y, -point.x * sy + point.z * cy)
    return SIMD3(yTurned.x, yTurned.y * cp - yTurned.z * sp,
                 yTurned.y * sp + yTurned.z * cp)
}

@Sendable @inline(__always) private func smoothMin(_ a: Float, _ b: Float, _ blend: Float) -> Float {
    let h = max(blend - abs(a - b), 0) / blend
    return min(a, b) - h * h * blend * 0.25
}

@Sendable @inline(__always) private func fieldDistance(_ point: SIMD3<Float>, centers: [SIMD3<Float>]) -> Float {
    let radius: Float = 0.355
    var distance = simd_length(point - centers[0]) - radius
    for center in centers.dropFirst() {
        distance = smoothMin(distance, simd_length(point - center) - radius, 0.30)
    }
    return distance
}

@Sendable @inline(__always) private func surfaceNormal(_ point: SIMD3<Float>, centers: [SIMD3<Float>]) -> SIMD3<Float> {
    let epsilon: Float = 0.005
    let x = fieldDistance(point + SIMD3(epsilon, 0, 0), centers: centers)
          - fieldDistance(point - SIMD3(epsilon, 0, 0), centers: centers)
    let y = fieldDistance(point + SIMD3(0, epsilon, 0), centers: centers)
          - fieldDistance(point - SIMD3(0, epsilon, 0), centers: centers)
    let z = fieldDistance(point + SIMD3(0, 0, epsilon), centers: centers)
          - fieldDistance(point - SIMD3(0, 0, epsilon), centers: centers)
    return simd_normalize(SIMD3(x, y, z))
}

@Sendable @inline(__always) private func clamp01(_ value: Float) -> Float { min(max(value, 0), 1) }
@Sendable @inline(__always) private func mix(_ a: SIMD3<Float>, _ b: SIMD3<Float>, _ t: Float) -> SIMD3<Float> {
    a + (b - a) * clamp01(t)
}

@Sendable private func shade(pixelX: Int, pixelY: Int, centers: [SIMD3<Float>], palette: Palette) -> SIMD4<UInt8> {
    let u = (Float(pixelX) + 0.5) / Float(frameSize)
    let v = (Float(pixelY) + 0.5) / Float(frameSize)
    let viewScale: Float = 3.45
    var point = SIMD3((u - 0.5) * viewScale, (0.5 - v) * viewScale, 3.0)
    let ray = SIMD3<Float>(0, 0, -1)
    var traveled: Float = 0
    var hit = false

    for _ in 0..<88 {
        let distance = fieldDistance(point, centers: centers)
        if distance < 0.0035 {
            hit = true
            break
        }
        let advance = max(distance * 0.72, 0.006)
        traveled += advance
        if traveled > 6.2 { break }
        point += ray * advance
    }
    guard hit else { return SIMD4(0, 0, 0, 0) }

    let normal = surfaceNormal(point, centers: centers)
    let light = simd_normalize(SIMD3<Float>(-0.48, 0.72, 0.62))
    let fill = simd_normalize(SIMD3<Float>(0.68, -0.22, 0.70))
    let view = SIMD3<Float>(0, 0, 1)
    let diffuse = max(simd_dot(normal, light), 0)
    let fillLight = max(simd_dot(normal, fill), 0)
    let halfVector = simd_normalize(light + view)
    let specular = pow(max(simd_dot(normal, halfVector), 0), 54)
    let rim = pow(1 - max(simd_dot(normal, view), 0), 2.35)
    let facing = clamp01(normal.z * 0.5 + 0.5)
    var color = mix(palette.edge, palette.base, 0.28 + facing * 0.72)
    color *= 0.24 + diffuse * 0.71 + fillLight * 0.18
    color += palette.accent * (rim * 0.42)
    color += SIMD3<Float>(repeating: 1) * (specular * 0.88)

    // Contact darkening where neighboring balls merge helps the lobes remain
    // legible without drawing seams into the implicit surface.
    let nearest = centers.map { simd_length(point - $0) }.sorted()
    if nearest.count > 1 {
        let junction = clamp01((0.53 - nearest[1]) * 2.6)
        color *= 1 - junction * 0.14
    }

    return SIMD4(UInt8(clamp01(color.x) * 255),
                 UInt8(clamp01(color.y) * 255),
                 UInt8(clamp01(color.z) * 255), 255)
}

try FileManager.default.createDirectory(at: output, withIntermediateDirectories: true)
let rotatedCenters: [[SIMD3<Float>]] = (0..<frameCount).map { frame in
    let yaw = Float(frame) / Float(frameCount) * Float.pi * 2
    let pitch = -0.13 + sin(yaw * 2) * 0.075
    return sourceCenters.map { rotate($0, yaw: yaw, pitch: pitch) }
}

if spinMode {
    let palette = palettes[0]
    for frame in 0..<frameCount {
        let byteCount = frameSize * frameSize * 4
        let pixels = UnsafeMutablePointer<UInt8>.allocate(capacity: byteCount)
        pixels.initialize(repeating: 0, count: byteCount)
        DispatchQueue.concurrentPerform(iterations: frameSize) { y in
            for x in 0..<frameSize {
                let rgba = shade(pixelX:x, pixelY:y, centers:rotatedCenters[frame], palette:palette)
                let offset = (y * frameSize + x) * 4
                pixels[offset] = rgba.x
                pixels[offset + 1] = rgba.y
                pixels[offset + 2] = rgba.z
                pixels[offset + 3] = rgba.w
            }
        }
        var planes: [UnsafeMutablePointer<UInt8>?] = [pixels]
        guard let bitmap = NSBitmapImageRep(bitmapDataPlanes:&planes,
                                            pixelsWide:frameSize, pixelsHigh:frameSize,
                                            bitsPerSample:8, samplesPerPixel:4,
                                            hasAlpha:true, isPlanar:false,
                                            colorSpaceName:.deviceRGB,
                                            bitmapFormat:.alphaNonpremultiplied,
                                            bytesPerRow:frameSize * 4, bitsPerPixel:32),
              let png = bitmap.representation(using:.png, properties:[.compressionFactor:0.82]) else {
            pixels.deallocate()
            fatalError("could not encode spin frame")
        }
        let name = String(format:"fuser-metaballs-spin-%03d.png", frame)
        try png.write(to:output.appendingPathComponent(name))
        pixels.deallocate()
        if frame.isMultiple(of:60) { print("  frame \(frame)/\(frameCount)") }
    }
    print("✓ \(frameCount) unique 3D frames · \(frameSize)px")
    exit(0)
}

for (variant, palette) in palettes.enumerated() {
    let byteCount = sheetWidth * sheetHeight * 4
    let pixels = UnsafeMutablePointer<UInt8>.allocate(capacity: byteCount)
    pixels.initialize(repeating: 0, count: byteCount)

    DispatchQueue.concurrentPerform(iterations: sheetHeight) { outputY in
        let cellYFromTop = outputY / frameSize
        let localY = outputY % frameSize
        // Frame zero occupies the bottom sheet row, matching CALayer contentsRect.
        let frameRow = rows - 1 - cellYFromTop
        for outputX in 0..<sheetWidth {
            let frameColumn = outputX / frameSize
            let localX = outputX % frameSize
            let frame = frameRow * columns + frameColumn
            let rgba = shade(pixelX: localX, pixelY: localY,
                             centers: rotatedCenters[frame], palette: palette)
            let offset = (outputY * sheetWidth + outputX) * 4
            pixels[offset] = rgba.x
            pixels[offset + 1] = rgba.y
            pixels[offset + 2] = rgba.z
            pixels[offset + 3] = rgba.w
        }
    }

    var planes: [UnsafeMutablePointer<UInt8>?] = [pixels]
    guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: &planes,
                                        pixelsWide: sheetWidth, pixelsHigh: sheetHeight,
                                        bitsPerSample: 8, samplesPerPixel: 4,
                                        hasAlpha: true, isPlanar: false,
                                        colorSpaceName: .deviceRGB,
                                        bitmapFormat: .alphaNonpremultiplied,
                                        bytesPerRow: sheetWidth * 4, bitsPerPixel: 32),
          let png = bitmap.representation(using: .png, properties: [.compressionFactor: 0.86]) else {
        pixels.deallocate()
        fatalError("could not encode metaball sheet")
    }
    let destination = output.appendingPathComponent("fuser-metaballs-\(variant).png")
    try png.write(to: destination)
    pixels.deallocate()
    print("✓ \(destination.lastPathComponent) · \(png.count / 1024) KB")
}
