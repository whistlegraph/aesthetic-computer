import MetalKit
import UIKit

struct SceneVertex {
    var position: SIMD4<Float>
    var color: SIMD4<Float>
}

/// Depth-bearing Oskiewar scene surface. The JavaScript camera already emits
/// projected logical x/y and canonical z; this view performs only the shared
/// logical-to-clip transform also used by the web and Xbox adapters.
final class MetalSceneView: MTKView, MTKViewDelegate {
    static let maxTriangles = 8_192
    private var sceneVertices: [SceneVertex] = []
    private let commandQueue: MTLCommandQueue
    private let pipeline: MTLRenderPipelineState
    private let depthState: MTLDepthStencilState
    private let vertexBuffer: MTLBuffer
    private let sceneLock = NSLock()
    private var background = MTLClearColor(red: 7 / 255, green: 8 / 255,
                                            blue: 28 / 255, alpha: 1)

    init(frame: CGRect = .zero) {
        guard let device = MTLCreateSystemDefaultDevice(),
              let queue = device.makeCommandQueue() else {
            fatalError("Metal is unavailable")
        }
        commandQueue = queue
        guard let buffer = device.makeBuffer(length: Self.maxTriangles * 3 *
            MemoryLayout<SceneVertex>.stride, options: .storageModeShared) else {
            fatalError("Metal scene buffer allocation failed")
        }
        vertexBuffer = buffer

        let source = """
        #include <metal_stdlib>
        using namespace metal;
        struct Vertex { float4 position; float4 color; };
        struct Raster { float4 position [[position]]; float3 color; };
        vertex Raster scene_vertex(const device Vertex *vertices [[buffer(0)]],
                                   uint id [[vertex_id]]) {
          Raster out;
          out.position = vertices[id].position;
          out.color = vertices[id].color.rgb;
          return out;
        }
        fragment float4 scene_fragment(Raster in [[stage_in]]) {
          return float4(in.color, 1.0);
        }
        """
        do {
            let library = try device.makeLibrary(source: source, options: nil)
            let descriptor = MTLRenderPipelineDescriptor()
            descriptor.vertexFunction = library.makeFunction(name: "scene_vertex")
            descriptor.fragmentFunction = library.makeFunction(name: "scene_fragment")
            descriptor.colorAttachments[0].pixelFormat = .bgra8Unorm
            descriptor.depthAttachmentPixelFormat = .depth32Float
            pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        } catch {
            fatalError("Metal scene pipeline failed: \(error)")
        }
        let depth = MTLDepthStencilDescriptor()
        depth.isDepthWriteEnabled = true
        depth.depthCompareFunction = .lessEqual
        guard let state = device.makeDepthStencilState(descriptor: depth) else {
            fatalError("Metal depth state failed")
        }
        depthState = state
        super.init(frame: frame, device: device)
        colorPixelFormat = .bgra8Unorm
        depthStencilPixelFormat = .depth32Float
        clearDepth = 1
        framebufferOnly = true
        isPaused = true
        enableSetNeedsDisplay = true
        preferredFramesPerSecond = 60
        delegate = self
        isMultipleTouchEnabled = true
    }

    required init(coder: NSCoder) { fatalError("init(coder:) is unsupported") }

    func beginFrame(red: Double, green: Double, blue: Double) {
        sceneLock.lock()
        sceneVertices.removeAll(keepingCapacity: true)
        background = MTLClearColor(red: clamped(red), green: clamped(green),
            blue: clamped(blue), alpha: 1)
        sceneLock.unlock()
    }

    @discardableResult
    func triangle(_ values: [Double], logicalSize: CGSize) -> Bool {
        guard values.count >= 12, logicalSize.width > 0, logicalSize.height > 0,
              values.prefix(12).allSatisfy(\.isFinite) else { return false }
        sceneLock.lock()
        defer { sceneLock.unlock() }
        guard sceneVertices.count < Self.maxTriangles * 3 else { return false }
        let ink = SIMD4<Float>(Float(clamped255(values[9])),
                               Float(clamped255(values[10])),
                               Float(clamped255(values[11])), 1)
        for vertex in 0..<3 {
            let at = vertex * 3
            let x = Float(values[at] / Double(logicalSize.width / 2) - 1)
            let y = Float(1 - values[at + 1] / Double(logicalSize.height / 2))
            let z = Float(max(0, min(1, (values[at + 2] + 1.5) / 3)))
            sceneVertices.append(SceneVertex(position: SIMD4(x, y, z, 1), color: ink))
        }
        return true
    }

    func present() { setNeedsDisplay() }

    func draw(in view: MTKView) {
        sceneLock.lock()
        let vertices = sceneVertices
        let clear = background
        sceneLock.unlock()
        guard let pass = currentRenderPassDescriptor,
              let drawable = currentDrawable,
              let command = commandQueue.makeCommandBuffer() else { return }
        pass.colorAttachments[0].clearColor = clear
        pass.colorAttachments[0].loadAction = .clear
        pass.depthAttachment.clearDepth = 1
        pass.depthAttachment.loadAction = .clear
        guard let encoder = command.makeRenderCommandEncoder(descriptor: pass) else { return }
        if !vertices.isEmpty {
            _ = vertices.withUnsafeBytes { bytes in
                memcpy(vertexBuffer.contents(), bytes.baseAddress!, bytes.count)
            }
            encoder.setRenderPipelineState(pipeline)
            encoder.setDepthStencilState(depthState)
            encoder.setVertexBuffer(vertexBuffer, offset: 0, index: 0)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0,
                                   vertexCount: vertices.count)
        }
        encoder.endEncoding()
        command.present(drawable)
        command.commit()
    }

    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}

    private func clamped(_ channel: Double) -> Double {
        max(0, min(255, channel)) / 255
    }

    private func clamped255(_ channel: Double) -> Double {
        max(0, min(255, channel)) / 255
    }
}
