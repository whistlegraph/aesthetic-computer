// Renderer.swift — Metal pipeline + fixed-step physics + camera. Owns the
// shared Game state (player + input) so the GameView can write input and the
// draw loop can advance physics and render.

import MetalKit
import simd
import QuartzCore

// Shared game state. Input flags are written from the main thread (GameView)
// and read by the fixed-step sim in `update`.
final class Game {
    var player: PlayerState
    let cfg = MoveConfig()

    // Held keys / buttons.
    var forwardKey = false, backKey = false, leftKey = false, rightKey = false
    var jump = false, crouch = false

    var lookSensitivity: Float = 0.12
    var invertY = false

    private var accumulator: Double = 0
    private let fixedStep: Double = 1.0 / 120.0

    init() {
        player = PlayerState(x: 0, z: -10, yaw: 0, pitch: 0)
    }

    // Mouse-look: dx/dy are pointer deltas in points.
    func look(dx: Float, dy: Float) {
        player.yaw += Double(dx * lookSensitivity)
        let dyAdj = invertY ? dy : -dy
        player.pitch = max(-89, min(89, player.pitch + Double(dyAdj * lookSensitivity)))
        // Keep yaw in a sane range.
        if player.yaw > 360 { player.yaw -= 360 }
        if player.yaw < -360 { player.yaw += 360 }
    }

    func update(_ frameDt: Double) {
        accumulator += min(frameDt, 0.25)
        while accumulator >= fixedStep {
            let fwd = (forwardKey ? 1 : 0) - (backKey ? 1 : 0)
            let right = (rightKey ? 1 : 0) - (leftKey ? 1 : 0)
            var buttons = 0
            if jump { buttons |= BTN.jump }
            if crouch { buttons |= BTN.crouch }
            let cmd = UserCmd(fwd: fwd, right: right,
                              yaw: player.yaw, pitch: player.pitch,
                              buttons: buttons, dt: fixedStep)
            player = pmove(player, cmd, cfg)
            accumulator -= fixedStep
        }
    }

    var speed: Double { (player.vx * player.vx + player.vz * player.vz).squareRoot() }
}

final class Renderer: NSObject, MTKViewDelegate {
    let device: MTLDevice
    let queue: MTLCommandQueue
    let game: Game

    private var triPipeline: MTLRenderPipelineState!
    private var linePipeline: MTLRenderPipelineState!
    private var depthState: MTLDepthStencilState!

    private let triBuffer: MTLBuffer
    private let triCount: Int
    private let lineBuffer: MTLBuffer
    private let lineCount: Int

    private var lastTime: CFTimeInterval = CACurrentMediaTime()
    private var aspect: Float = 1

    init?(mtkView: MTKView, game: Game) {
        guard let dev = mtkView.device ?? MTLCreateSystemDefaultDevice(),
              let q = dev.makeCommandQueue() else { return nil }
        self.device = dev
        self.queue = q
        self.game = game

        // Build world geometry once.
        var mesh = WorldMesh()
        MeshBuilder.buildGround(into: &mesh)
        MeshBuilder.buildObstacles(into: &mesh)

        guard let tb = dev.makeBuffer(bytes: mesh.tris,
                                      length: mesh.tris.count * MemoryLayout<TriVertex>.stride,
                                      options: []),
              let lb = dev.makeBuffer(bytes: mesh.lines,
                                      length: mesh.lines.count * MemoryLayout<LineVertex>.stride,
                                      options: []) else { return nil }
        self.triBuffer = tb
        self.triCount = mesh.tris.count
        self.lineBuffer = lb
        self.lineCount = mesh.lines.count

        super.init()

        mtkView.depthStencilPixelFormat = .depth32Float
        mtkView.clearColor = MTLClearColor(red: 0.02, green: 0.02, blue: 0.04, alpha: 1)
        mtkView.colorPixelFormat = .bgra8Unorm

        do {
            try buildPipelines(view: mtkView)
        } catch {
            print("❌ pipeline build failed: \(error)")
            return nil
        }

        let depthDesc = MTLDepthStencilDescriptor()
        depthDesc.depthCompareFunction = .less
        depthDesc.isDepthWriteEnabled = true
        depthState = dev.makeDepthStencilState(descriptor: depthDesc)
    }

    private func buildPipelines(view: MTKView) throws {
        let library = try device.makeLibrary(source: metalShaderSource, options: nil)

        // Triangle vertex descriptor: position(0), normal(12), color(24); stride 40.
        let triVD = MTLVertexDescriptor()
        triVD.attributes[0].format = .float3
        triVD.attributes[0].offset = 0
        triVD.attributes[0].bufferIndex = 0
        triVD.attributes[1].format = .float3
        triVD.attributes[1].offset = MemoryLayout<Float>.stride * 3
        triVD.attributes[1].bufferIndex = 0
        triVD.attributes[2].format = .float4
        triVD.attributes[2].offset = MemoryLayout<Float>.stride * 6
        triVD.attributes[2].bufferIndex = 0
        triVD.layouts[0].stride = MemoryLayout<TriVertex>.stride

        let triDesc = MTLRenderPipelineDescriptor()
        triDesc.vertexFunction = library.makeFunction(name: "vertex_tri")
        triDesc.fragmentFunction = library.makeFunction(name: "fragment_tri")
        triDesc.vertexDescriptor = triVD
        triDesc.colorAttachments[0].pixelFormat = view.colorPixelFormat
        triDesc.depthAttachmentPixelFormat = view.depthStencilPixelFormat
        triPipeline = try device.makeRenderPipelineState(descriptor: triDesc)

        // Line vertex descriptor: position(0), color(12); stride 28.
        let lineVD = MTLVertexDescriptor()
        lineVD.attributes[0].format = .float3
        lineVD.attributes[0].offset = 0
        lineVD.attributes[0].bufferIndex = 0
        lineVD.attributes[1].format = .float4
        lineVD.attributes[1].offset = MemoryLayout<Float>.stride * 3
        lineVD.attributes[1].bufferIndex = 0
        lineVD.layouts[0].stride = MemoryLayout<LineVertex>.stride

        let lineDesc = MTLRenderPipelineDescriptor()
        lineDesc.vertexFunction = library.makeFunction(name: "vertex_line")
        lineDesc.fragmentFunction = library.makeFunction(name: "fragment_line")
        lineDesc.vertexDescriptor = lineVD
        lineDesc.colorAttachments[0].pixelFormat = view.colorPixelFormat
        lineDesc.depthAttachmentPixelFormat = view.depthStencilPixelFormat
        linePipeline = try device.makeRenderPipelineState(descriptor: lineDesc)
    }

    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {
        aspect = size.height > 0 ? Float(size.width / size.height) : 1
    }

    func draw(in view: MTKView) {
        let now = CACurrentMediaTime()
        let dt = now - lastTime
        lastTime = now
        game.update(dt)

        guard let drawable = view.currentDrawable,
              let rpd = view.currentRenderPassDescriptor,
              let cmd = queue.makeCommandBuffer(),
              let enc = cmd.makeRenderCommandEncoder(descriptor: rpd) else { return }

        // Camera from player eye + yaw/pitch.
        let p = game.player
        let eye = SIMD3<Float>(Float(p.x), Float(p.y), Float(p.z))
        let fwd = forwardVector(yawDeg: Float(p.yaw), pitchDeg: Float(p.pitch))
        let view4 = lookAtRH(eye: eye, center: eye + fwd, up: SIMD3<Float>(0, 1, 0))
        let proj = perspectiveRH(fovyRadians: radians(70), aspect: aspect, near: 0.05, far: 300)
        var uniforms = Uniforms(viewProj: proj * view4,
                                lightDir: SIMD4<Float>(simd_normalize(SIMD3<Float>(0.4, 1.0, 0.3)), 0))

        enc.setDepthStencilState(depthState)
        // Culling off for now — keeps geometry visible regardless of winding;
        // tighten to .back once faces are verified.
        enc.setCullMode(.none)

        // Triangles.
        enc.setRenderPipelineState(triPipeline)
        enc.setVertexBuffer(triBuffer, offset: 0, index: 0)
        enc.setVertexBytes(&uniforms, length: MemoryLayout<Uniforms>.stride, index: 1)
        enc.setFragmentBytes(&uniforms, length: MemoryLayout<Uniforms>.stride, index: 1)
        enc.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: triCount)

        // Lines.
        enc.setRenderPipelineState(linePipeline)
        enc.setVertexBuffer(lineBuffer, offset: 0, index: 0)
        enc.setVertexBytes(&uniforms, length: MemoryLayout<Uniforms>.stride, index: 1)
        enc.drawPrimitives(type: .line, vertexStart: 0, vertexCount: lineCount)

        enc.endEncoding()
        cmd.present(drawable)
        cmd.commit()
    }
}
