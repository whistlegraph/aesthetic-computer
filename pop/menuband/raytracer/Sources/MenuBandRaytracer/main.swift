import Foundation
import Metal
import MetalKit
import CoreGraphics
import ImageIO
import UniformTypeIdentifiers

struct Uniforms {
    var width: UInt32
    var height: UInt32
    var samples: UInt32
    var frame: UInt32
    var time: Float
    var duration: Float
    var loopSeconds: Float
    var exposure: Float
}

struct Arguments {
    let values: [String: String]
    let flags: Set<String>

    init(_ raw: [String]) {
        var values: [String: String] = [:]
        var flags = Set<String>()
        var index = 1
        while index < raw.count {
            let key = raw[index]
            if key.hasPrefix("--"), index + 1 < raw.count, !raw[index + 1].hasPrefix("--") {
                values[key] = raw[index + 1]
                index += 2
            } else {
                flags.insert(key)
                index += 1
            }
        }
        self.values = values
        self.flags = flags
    }

    func string(_ key: String, _ fallback: String) -> String { values[key] ?? fallback }
    func int(_ key: String, _ fallback: Int) -> Int { Int(values[key] ?? "") ?? fallback }
    func float(_ key: String, _ fallback: Float) -> Float { Float(values[key] ?? "") ?? fallback }
}

let arguments = Arguments(CommandLine.arguments)
if arguments.flags.contains("--help") {
    print("""
    MenuBandRaytracer
      --score <score.png> --keyboard <keyboard.png>
      --out <frame.png> --time <seconds> --size <WxH> --spp <samples>
      --shape <wheel|strip>
      --frame-start <n> --frame-end <n> --fps <n> --out-dir <directory>
      --shard-index <n> --shard-count <n>
    """)
    exit(0)
}

let cwd = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
func absoluteURL(_ value: String) -> URL {
    if value.hasPrefix("/") { return URL(fileURLWithPath: value) }
    return cwd.appendingPathComponent(value)
}

let scoreURL = absoluteURL(arguments.string("--score", "assets/01-lantern/score.png"))
let keyboardURL = absoluteURL(arguments.string("--keyboard", "assets/01-lantern/keyboard.png"))
let shape = arguments.string("--shape", "wheel").lowercased()
guard shape == "wheel" || shape == "strip" else { fatalError("--shape must be wheel or strip") }
let sizeParts = arguments.string("--size", "1080x1920").lowercased().split(separator: "x")
guard sizeParts.count == 2, let width = Int(sizeParts[0]), let height = Int(sizeParts[1]), width > 0, height > 0 else {
    fatalError("--size must be WxH")
}
let samples = max(1, arguments.int("--spp", 8))
let duration = arguments.float("--duration", 60)
let loopSeconds = arguments.float("--loop-seconds", 9.6)
let exposure = arguments.float("--exposure", 0.80)
let fps = max(1, arguments.int("--fps", 60))
let shardCount = max(1, arguments.int("--shard-count", 1))
let shardIndex = min(shardCount - 1, max(0, arguments.int("--shard-index", 0)))

guard let device = MTLCreateSystemDefaultDevice() else { fatalError("Metal device unavailable") }
let loader = MTKTextureLoader(device: device)
let loadOptions: [MTKTextureLoader.Option: Any] = [
    .origin: MTKTextureLoader.Origin.topLeft,
    .generateMipmaps: true,
    .SRGB: true,
]
let scoreTexture = try loader.newTexture(URL: scoreURL, options: loadOptions)
let keyboardTexture = try loader.newTexture(URL: keyboardURL, options: loadOptions)
guard let shaderURL = Bundle.module.url(forResource: "MenuBand", withExtension: "metal") else {
    fatalError("bundled Metal shader missing")
}
let shaderSource = try String(contentsOf: shaderURL, encoding: .utf8)
let shapeDefine = shape == "strip" ? "#define MENU_BAND_STRIP 1\n" : "#define MENU_BAND_STRIP 0\n"
let library = try device.makeLibrary(source: shapeDefine + shaderSource, options: nil)
guard let function = library.makeFunction(name: "renderMenuBand") else { fatalError("renderMenuBand shader missing") }
let pipeline = try device.makeComputePipelineState(function: function)
guard let queue = device.makeCommandQueue() else { fatalError("Metal command queue unavailable") }

let outputDescriptor = MTLTextureDescriptor.texture2DDescriptor(
    pixelFormat: .rgba8Unorm,
    width: width,
    height: height,
    mipmapped: false
)
outputDescriptor.usage = [.shaderWrite]
outputDescriptor.storageMode = .shared
guard let outputTexture = device.makeTexture(descriptor: outputDescriptor) else { fatalError("output texture allocation failed") }

@MainActor
func render(frame: Int, time: Float, outputURL: URL) throws {
    var uniforms = Uniforms(
        width: UInt32(width), height: UInt32(height), samples: UInt32(samples), frame: UInt32(frame),
        time: time, duration: duration, loopSeconds: loopSeconds, exposure: exposure
    )
    guard let commandBuffer = queue.makeCommandBuffer(), let encoder = commandBuffer.makeComputeCommandEncoder() else {
        fatalError("Metal encoder unavailable")
    }
    encoder.setComputePipelineState(pipeline)
    encoder.setTexture(scoreTexture, index: 0)
    encoder.setTexture(keyboardTexture, index: 1)
    encoder.setTexture(outputTexture, index: 2)
    encoder.setBytes(&uniforms, length: MemoryLayout<Uniforms>.stride, index: 0)
    let threadWidth = pipeline.threadExecutionWidth
    let threadHeight = max(1, pipeline.maxTotalThreadsPerThreadgroup / threadWidth)
    encoder.dispatchThreads(
        MTLSize(width: width, height: height, depth: 1),
        threadsPerThreadgroup: MTLSize(width: threadWidth, height: threadHeight, depth: 1)
    )
    encoder.endEncoding()
    commandBuffer.commit()
    commandBuffer.waitUntilCompleted()
    if let error = commandBuffer.error { throw error }
    try writePNG(texture: outputTexture, to: outputURL)
}

let started = Date()
if let frameStartText = arguments.values["--frame-start"], let frameStart = Int(frameStartText) {
    let frameEnd = max(frameStart, arguments.int("--frame-end", frameStart))
    let outputDirectory = absoluteURL(arguments.string("--out-dir", "out/frames"))
    try FileManager.default.createDirectory(at: outputDirectory, withIntermediateDirectories: true)
    for frame in frameStart...frameEnd where frame % shardCount == shardIndex {
        let output = outputDirectory.appendingPathComponent(String(format: "%06d.png", frame))
        try render(frame: frame, time: Float(frame) / Float(fps), outputURL: output)
        print("✓ frame \(frame) → \(output.path)")
    }
} else {
    let time = arguments.float("--time", 2.05)
    let frame = Int((time * Float(fps)).rounded())
    let output = absoluteURL(arguments.string("--out", "out/frame.png"))
    try FileManager.default.createDirectory(at: output.deletingLastPathComponent(), withIntermediateDirectories: true)
    try render(frame: frame, time: time, outputURL: output)
    print("✓ \(width)×\(height) · \(samples) spp · \(String(format: "%.2f", Date().timeIntervalSince(started)))s → \(output.path)")
}

func writePNG(texture: MTLTexture, to url: URL) throws {
    let bytesPerRow = texture.width * 4
    var bytes = [UInt8](repeating: 0, count: bytesPerRow * texture.height)
    texture.getBytes(
        &bytes,
        bytesPerRow: bytesPerRow,
        from: MTLRegionMake2D(0, 0, texture.width, texture.height),
        mipmapLevel: 0
    )
    let data = Data(bytes)
    guard let provider = CGDataProvider(data: data as CFData),
          let image = CGImage(
            width: texture.width,
            height: texture.height,
            bitsPerComponent: 8,
            bitsPerPixel: 32,
            bytesPerRow: bytesPerRow,
            space: CGColorSpace(name: CGColorSpace.sRGB)!,
            bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedLast.rawValue),
            provider: provider,
            decode: nil,
            shouldInterpolate: true,
            intent: .defaultIntent
          ),
          let destination = CGImageDestinationCreateWithURL(url as CFURL, UTType.png.identifier as CFString, 1, nil) else {
        fatalError("PNG encoder creation failed")
    }
    CGImageDestinationAddImage(destination, image, nil)
    guard CGImageDestinationFinalize(destination) else { fatalError("PNG write failed: \(url.path)") }
}
