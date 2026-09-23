import AppKit
import MetalKit
import CoreVideo

/// Oskiewar Native's triangle-stream approach adapted to the face rig: CPU
/// geometry, GPU rasterization, one batch, display-clock scheduling. A ring of
/// buffers keeps the CPU from overwriting vertices still being read by Metal.
struct SingerFaceVertex {
    var position: SIMD2<Float>
    var color: SIMD4<Float>
}

final class SingerFaceCanvas {
    private let context: CGContext?
    private var transform = CGAffineTransform.identity
    private var clips: [[CGPoint]] = []
    private var stack: [(CGAffineTransform, [[CGPoint]])] = []
    var vertices: [SingerFaceVertex] = []
    var caption: (NSAttributedString, CGPoint)?
    init(context: CGContext? = nil) {
        self.context = context
        if context == nil { vertices.reserveCapacity(24_000) }
    }
    func saveGState() {
        if let context { context.saveGState() } else { stack.append((transform, clips)) }
    }
    func restoreGState() {
        if let context { context.restoreGState() }
        else if let state = stack.popLast() { transform = state.0; clips = state.1 }
    }
    func translateBy(x: CGFloat, y: CGFloat) {
        if let context { context.translateBy(x: x, y: y) }
        else { transform = transform.translatedBy(x: x, y: y) }
    }
    func rotate(by angle: CGFloat) {
        if let context { context.rotate(by: angle) } else { transform = transform.rotated(by: angle) }
    }
    func scaleBy(x: CGFloat, y: CGFloat) {
        if let context { context.scaleBy(x: x, y: y) } else { transform = transform.scaledBy(x: x, y: y) }
    }
    private func rgba(_ color: NSColor) -> SIMD4<Float> {
        let c = color.usingColorSpace(.sRGB) ?? .black
        return SIMD4(Float(c.redComponent), Float(c.greenComponent), Float(c.blueComponent), Float(c.alphaComponent))
    }
    // The face's fills and clipping outlines are convex. Sample Beziers into
    // subpixel-sized arcs, then use triangle fans instead of bitmap snapshots.
    private func contours(_ path: NSBezierPath) -> [(points: [CGPoint], closed: Bool)] {
        var result: [(points: [CGPoint], closed: Bool)] = [], points: [CGPoint] = []
        var control = [CGPoint](repeating: .zero, count: 3)
        for i in 0..<path.elementCount {
            switch path.element(at: i, associatedPoints: &control) {
            case .moveTo:
                if !points.isEmpty { result.append((points, false)) }
                points = [control[0]]
            case .lineTo: points.append(control[0])
            case .curveTo:
                guard let a = points.last else { continue }
                let b = control[0], c = control[1], d = control[2]
                let length = hypot(a.x-b.x,a.y-b.y)+hypot(b.x-c.x,b.y-c.y)+hypot(c.x-d.x,c.y-d.y)
                let steps = max(8,min(96,Int(ceil(length/12))))
                for j in 1...steps {
                    let t = CGFloat(j)/CGFloat(steps), u = 1-t
                    points.append(CGPoint(x: u*u*u*a.x+3*u*u*t*b.x+3*u*t*t*c.x+t*t*t*d.x,
                                          y: u*u*u*a.y+3*u*u*t*b.y+3*u*t*t*c.y+t*t*t*d.y))
                }
            case .closePath:
                if points.count > 1, points.first == points.last { points.removeLast() }
                result.append((points,true)); points = []
            @unknown default: break
            }
        }
        if !points.isEmpty { result.append((points,false)) }
        return result
    }
    func clip(_ path: NSBezierPath) {
        if context != nil { path.addClip(); return }
        if let points = contours(path).first?.points { clips.append(points.map { $0.applying(transform) }) }
    }
    private func cross(_ a: CGPoint, _ b: CGPoint, _ c: CGPoint) -> CGFloat {
        (b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x)
    }
    private func clipped(_ polygon: [CGPoint]) -> [CGPoint] {
        var output = polygon
        for clip in clips where clip.count >= 3 {
            let area = zip(clip, Array(clip.dropFirst())+[clip[0]]).reduce(CGFloat(0)) { $0+$1.0.x*$1.1.y-$1.1.x*$1.0.y }
            let sign: CGFloat = area >= 0 ? 1 : -1
            for i in clip.indices {
                guard !output.isEmpty else { break }
                let a = clip[i], b = clip[(i+1)%clip.count], input = output
                output.removeAll(keepingCapacity: true)
                var previous = input.last!, dp = sign*cross(a,b,previous)
                for point in input {
                    let dc = sign*cross(a,b,point)
                    if (dc >= 0) != (dp >= 0) {
                        let t = dp/(dp-dc)
                        output.append(CGPoint(x: previous.x+(point.x-previous.x)*t, y: previous.y+(point.y-previous.y)*t))
                    }
                    if dc >= 0 { output.append(point) }
                    previous = point; dp = dc
                }
            }
        }
        return output
    }
    private func vertex(_ point: CGPoint, _ color: SIMD4<Float>) -> SingerFaceVertex {
        SingerFaceVertex(position: SIMD2(Float(point.x),Float(point.y)), color: color)
    }
    private func polygon(_ points: [CGPoint], _ color: SIMD4<Float>) {
        let p = clipped(points.map { $0.applying(transform) })
        guard p.count >= 3 else { return }
        for i in 1..<(p.count-1) {
            vertices.append(vertex(p[0],color)); vertices.append(vertex(p[i],color)); vertices.append(vertex(p[i+1],color))
        }
    }
    func fill(_ path: NSBezierPath, color: NSColor) {
        if context != nil { color.setFill(); path.fill(); return }
        let color = rgba(color)
        for contour in contours(path) { polygon(contour.points,color) }
    }
    func ink(_ path: NSBezierPath, width: CGFloat, fill color: NSColor?) {
        if let color { fill(path,color: color) }
        if context != nil {
            path.lineWidth = width; path.lineJoinStyle = .round; path.lineCapStyle = .round
            NSColor.black.setStroke(); path.stroke(); return
        }
        let ink = SIMD4<Float>(0,0,0,1), radius = width/2
        for contour in contours(path) {
            let p = contour.points
            guard p.count > 1 else { continue }
            let count = contour.closed ? p.count : p.count-1
            // A strip plus round joins/caps. Filled on the GPU in the same
            // draw call as the eyes and mouth; no per-path render passes.
            for i in 0..<count {
                let a = p[i], b = p[(i+1)%p.count], dx = b.x-a.x, dy = b.y-a.y
                let length = max(0.0001,hypot(dx,dy)), nx = -dy/length*radius, ny = dx/length*radius
                polygon([CGPoint(x:a.x+nx,y:a.y+ny),CGPoint(x:b.x+nx,y:b.y+ny),CGPoint(x:b.x-nx,y:b.y-ny),CGPoint(x:a.x-nx,y:a.y-ny)],ink)
            }
            let segments = 12
            for point in p {
                polygon((0..<segments).map { i in
                    let angle = CGFloat(i)*2 * .pi/CGFloat(segments)
                    return CGPoint(x:point.x+cos(angle)*radius,y:point.y+sin(angle)*radius)
                },ink)
            }
        }
    }
    /// Soft rectangular vignette. Opaque color rings share one triangle batch;
    /// smoothstep spacing preserves the accent in the middle of the screen.
    func edgeLight(in rect: CGRect, center: NSColor, edge: NSColor) {
        let a = rgba(center), b = rgba(edge)
        func ring(_ t: CGFloat) -> [CGPoint] {
            let r = rect.insetBy(dx: rect.width*0.25*t, dy: rect.height*0.24*t)
            return [CGPoint(x:r.minX,y:r.minY),CGPoint(x:r.maxX,y:r.minY),
                    CGPoint(x:r.maxX,y:r.maxY),CGPoint(x:r.minX,y:r.maxY)]
        }
        func color(_ t: CGFloat) -> SIMD4<Float> {
            let u = Float(t*t*(3-2*t)); return b+(a-b)*u
        }
        for step in 0..<24 {
            let t = CGFloat(step)/24, u = CGFloat(step+1)/24
            let outer = ring(t), inner = ring(u), c = color(t), d = color(u)
            if context != nil {
                let path = NSBezierPath(rect: CGRect(x:outer[0].x,y:outer[0].y,
                    width:outer[1].x-outer[0].x,height:outer[2].y-outer[0].y))
                fill(path,color:NSColor(srgbRed:CGFloat(c.x),green:CGFloat(c.y),blue:CGFloat(c.z),alpha:1))
            } else {
                for i in 0..<4 {
                    let j = (i+1)%4
                    vertices += [vertex(outer[i],c),vertex(outer[j],c),vertex(inner[j],d),
                                 vertex(outer[i],c),vertex(inner[j],d),vertex(inner[i],d)]
                }
            }
        }
        if context != nil {
            fill(NSBezierPath(rect:rect.insetBy(dx:rect.width*0.25,dy:rect.height*0.24)),color:center)
        }
    }

    func gradient(in path: NSBezierPath, color: NSColor) {
        if context != nil {
            NSGradient(starting:color,ending:color.withAlphaComponent(0))?.draw(in:path,relativeCenterPosition:.zero)
            return
        }
        guard let contour = contours(path).first?.points, contour.count > 2 else { return }
        let rect = path.bounds, center = CGPoint(x:rect.midX,y:rect.midY).applying(transform)
        let inner = rgba(color), outer = SIMD4(inner.x,inner.y,inner.z,Float(0))
        for i in contour.indices {
            vertices.append(vertex(center,inner))
            vertices.append(vertex(contour[i].applying(transform),outer))
            vertices.append(vertex(contour[(i+1)%contour.count].applying(transform),outer))
        }
    }
    func text(_ text: NSAttributedString, at point: CGPoint) {
        if context != nil { text.draw(at:point) } else { caption = (text,point) }
    }
}

final class SingerFaceMetalView: MTKView, MTKViewDelegate {
    var animate: (() -> Void)?
    var paint: ((SingerFaceCanvas) -> Void)?
    private var queue: MTLCommandQueue!
    private var pipeline: MTLRenderPipelineState!
    private var buffers: [MTLBuffer] = []
    private var freeSlots = [0,1,2]
    private let slotLock = NSLock()
    private var link: CVDisplayLink?
    private var nativeLink: AnyObject?
    private let scheduleLock = NSLock()
    private var framePending = false, running = false
    private var generation = 0
    private let labelView = NSTextField(labelWithString: "")
    private var presented = 0, gpuSeconds = 0.0, completed = 0, lastPresented = 0.0
    private var intervals: [Double] = []
    private var submitted = 0, missed = 0, cpuSeconds = 0.0, reportAt = CACurrentMediaTime()
    private let statsLock = NSLock()
    private let capacity = 196_608

    init?(metalFrame frame: CGRect) {
        guard let device = MTLCreateSystemDefaultDevice(), let queue = device.makeCommandQueue() else { return nil }
        super.init(frame:frame,device:device)
        self.queue = queue
        colorPixelFormat = .bgra8Unorm
        sampleCount = device.supportsTextureSampleCount(4) ? 4 : 1
        framebufferOnly = true; isPaused = true; enableSetNeedsDisplay = false
        autoResizeDrawable = true
        (layer as? CAMetalLayer)?.maximumDrawableCount = 3
        (layer as? CAMetalLayer)?.allowsNextDrawableTimeout = true
        let source = """
        #include <metal_stdlib>
        using namespace metal;
        struct Vertex { float2 position; float4 color; };
        struct Raster { float4 position [[position]]; float4 color; };
        vertex Raster face_vertex(const device Vertex *v [[buffer(0)]],
                                  constant float2 &size [[buffer(1)]], uint id [[vertex_id]]) {
            Raster out; out.position=float4(v[id].position/size*2.0-1.0,0,1); out.color=v[id].color; return out;
        }
        fragment float4 face_fragment(Raster in [[stage_in]]) { return in.color; }
        """
        do {
            let library = try device.makeLibrary(source:source,options:nil)
            let p = MTLRenderPipelineDescriptor()
            p.vertexFunction = library.makeFunction(name:"face_vertex")
            p.fragmentFunction = library.makeFunction(name:"face_fragment")
            p.sampleCount = sampleCount
            let a = p.colorAttachments[0]!
            a.pixelFormat = colorPixelFormat; a.isBlendingEnabled = true
            a.sourceRGBBlendFactor = .sourceAlpha; a.destinationRGBBlendFactor = .oneMinusSourceAlpha
            a.sourceAlphaBlendFactor = .one; a.destinationAlphaBlendFactor = .oneMinusSourceAlpha
            pipeline = try device.makeRenderPipelineState(descriptor:p)
        } catch { NSLog("🎭 face: Metal pipeline failed: %@",String(describing:error)); return nil }
        for _ in 0..<3 {
            guard let buffer = device.makeBuffer(length:capacity*MemoryLayout<SingerFaceVertex>.stride,options:.storageModeShared) else { return nil }
            buffers.append(buffer)
        }
        labelView.isHidden = true; addSubview(labelView)
        delegate = self
    }
    required init(coder:NSCoder) { fatalError("Code-only face") }
    override var isOpaque: Bool { true }
    @discardableResult
    func start() -> Bool {
        guard !running else { return true }
        running = true; generation += 1; reportAt = CACurrentMediaTime()
        if #available(macOS 14.0, *) {
            let clock = displayLink(target:self,selector:#selector(nativeTick(_:)))
            let hz = Float(window?.screen?.maximumFramesPerSecond ?? 60)
            clock.preferredFrameRateRange = CAFrameRateRange(minimum:min(60,hz),maximum:hz,preferred:hz)
            clock.add(to:.main,forMode:.common); nativeLink = clock
            NSLog("🎭 Metal face: display target %.0f Hz · %dx MSAA",hz,sampleCount)
            return true
        }
        var newLink: CVDisplayLink?
        let display = (window?.screen?.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber)?.uint32Value ?? CGMainDisplayID()
        guard CVDisplayLinkCreateWithCGDisplay(display,&newLink) == kCVReturnSuccess, let newLink else { running = false; return false }
        link = newLink
        CVDisplayLinkSetOutputCallback(newLink, { _,_,_,_,_,context in
            guard let context else { return kCVReturnError }
            Unmanaged<SingerFaceMetalView>.fromOpaque(context).takeUnretainedValue().scheduleFrame()
            return kCVReturnSuccess
        },Unmanaged.passUnretained(self).toOpaque())
        if CVDisplayLinkStart(newLink) != kCVReturnSuccess { running = false; link = nil; return false }
        return true
    }
    @objc private func nativeTick(_ sender: Any) { if running { draw() } }
    func stop() {
        running = false; generation += 1
        if #available(macOS 14.0, *) { (nativeLink as? CADisplayLink)?.invalidate() }; nativeLink = nil
        if let link { CVDisplayLinkStop(link) }; link = nil
    }
    deinit { if let link { CVDisplayLinkStop(link) } }
    private func scheduleFrame() {
        scheduleLock.lock()
        guard !framePending else { scheduleLock.unlock(); return }
        framePending = true; scheduleLock.unlock()
        DispatchQueue.main.async { [weak self] in
            guard let self else { return }
            if self.running { self.draw() }
            self.scheduleLock.lock(); self.framePending = false; self.scheduleLock.unlock()
        }
    }
    // Explicit presentation avoids AppKit coalescing separate display ticks
    // into one dirty-rectangle redraw. All pending work is bounded to one frame.
    func draw(in view: MTKView) { renderFrame() }
    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}
    private func renderFrame() {
        guard bounds.width > 0, bounds.height > 0 else { return }
        let start = CACurrentMediaTime()
        animate?()
        slotLock.lock(); let slot = freeSlots.popLast(); slotLock.unlock()
        guard let slot else { missed += 1; return }
        var submittedSlot = false
        defer { if !submittedSlot { slotLock.lock(); freeSlots.append(slot); slotLock.unlock() } }
        let canvas = SingerFaceCanvas(); paint?(canvas)
        guard canvas.vertices.count <= capacity else { NSLog("🎭 face: triangle capacity exceeded"); return }
        if let (text,point) = canvas.caption {
            if labelView.attributedStringValue != text { labelView.attributedStringValue = text; labelView.sizeToFit() }
            labelView.setFrameOrigin(point); labelView.isHidden = false
        } else { labelView.isHidden = true }
        guard let pass = currentRenderPassDescriptor, let drawable = currentDrawable,
              let command = queue.makeCommandBuffer(), let encoder = command.makeRenderCommandEncoder(descriptor:pass) else { missed += 1; return }
        let buffer = buffers[slot]
        canvas.vertices.withUnsafeBytes { bytes in
            if let base = bytes.baseAddress { memcpy(buffer.contents(),base,bytes.count) }
        }
        var size = SIMD2<Float>(Float(bounds.width),Float(bounds.height))
        encoder.setRenderPipelineState(pipeline)
        encoder.setVertexBuffer(buffer,offset:0,index:0)
        encoder.setVertexBytes(&size,length:MemoryLayout<SIMD2<Float>>.stride,index:1)
        encoder.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:canvas.vertices.count)
        encoder.endEncoding()
        let run = generation
        drawable.addPresentedHandler { [weak self] drawable in
            guard let self else { return }
            self.statsLock.lock()
            let time = CACurrentMediaTime() // callback arrival; presentedTime is zero on some macOS drivers
            if self.lastPresented > 0 && time > self.lastPresented { self.intervals.append(time-self.lastPresented) }
            self.lastPresented = time; self.presented += 1
            self.statsLock.unlock()
        }
        command.addCompletedHandler { [weak self] command in
            guard let self else { return }
            self.slotLock.lock(); self.freeSlots.append(slot); self.slotLock.unlock()
            self.statsLock.lock()
            self.gpuSeconds += max(0,command.gpuEndTime-command.gpuStartTime); self.completed += 1
            self.statsLock.unlock()
            if command.status == .error { NSLog("🎭 face: Metal command failed (%d): %@",run,String(describing:command.error)) }
        }
        command.present(drawable); command.commit(); submittedSlot = true
        submitted += 1; cpuSeconds += CACurrentMediaTime()-start
        report()
    }
    /// Render the production triangle pipeline into an exportable GPU texture.
    /// Used by visual regression checks; live frames never read pixels back.
    func snapshot(_ canvas: SingerFaceCanvas, width: Int, height: Int) -> NSBitmapImageRep? {
        guard let device, canvas.vertices.count <= capacity else { return nil }
        let desc = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: colorPixelFormat,width: width,height: height,mipmapped: false)
        desc.usage = [.renderTarget]; desc.storageMode = .private
        guard let resolved = device.makeTexture(descriptor:desc) else { return nil }
        let pass = MTLRenderPassDescriptor()
        if sampleCount > 1 {
            desc.textureType = .type2DMultisample; desc.sampleCount = sampleCount
            guard let multisample = device.makeTexture(descriptor:desc) else { return nil }
            pass.colorAttachments[0].texture = multisample
            pass.colorAttachments[0].resolveTexture = resolved
            pass.colorAttachments[0].storeAction = .multisampleResolve
        } else {
            pass.colorAttachments[0].texture = resolved
            pass.colorAttachments[0].storeAction = .store
        }
        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].clearColor = MTLClearColor(red:0,green:0,blue:0,alpha:1)
        guard let buffer = device.makeBuffer(length:max(1,canvas.vertices.count)*MemoryLayout<SingerFaceVertex>.stride,options:.storageModeShared),
              let readback = device.makeBuffer(length:width*height*4,options:.storageModeShared),
              let command = queue.makeCommandBuffer(), let encoder = command.makeRenderCommandEncoder(descriptor:pass) else { return nil }
        canvas.vertices.withUnsafeBytes { bytes in if let p = bytes.baseAddress { memcpy(buffer.contents(),p,bytes.count) } }
        var size = SIMD2<Float>(Float(bounds.width),Float(bounds.height))
        encoder.setRenderPipelineState(pipeline); encoder.setVertexBuffer(buffer,offset:0,index:0)
        encoder.setVertexBytes(&size,length:MemoryLayout<SIMD2<Float>>.stride,index:1)
        encoder.drawPrimitives(type:.triangle,vertexStart:0,vertexCount:canvas.vertices.count); encoder.endEncoding()
        guard let blit = command.makeBlitCommandEncoder() else { return nil }
        blit.copy(from:resolved,sourceSlice:0,sourceLevel:0,sourceOrigin:MTLOrigin(x:0,y:0,z:0),sourceSize:MTLSize(width:width,height:height,depth:1),to:readback,destinationOffset:0,destinationBytesPerRow:width*4,destinationBytesPerImage:width*height*4)
        blit.endEncoding(); command.commit(); command.waitUntilCompleted()
        guard command.status == .completed, let bitmap = NSBitmapImageRep(bitmapDataPlanes:nil,pixelsWide:width,pixelsHigh:height,bitsPerSample:8,samplesPerPixel:4,hasAlpha:true,isPlanar:false,colorSpaceName:.deviceRGB,bytesPerRow:width*4,bitsPerPixel:32),let output = bitmap.bitmapData else { return nil }
        let input = readback.contents().assumingMemoryBound(to:UInt8.self)
        for i in stride(from:0,to:width*height*4,by:4) {
            output[i] = input[i+2]; output[i+1] = input[i+1]; output[i+2] = input[i]; output[i+3] = input[i+3]
        }
        return bitmap
    }

    private func report() {
        let now = CACurrentMediaTime(), elapsed = now-reportAt
        guard elapsed >= 5 else { return }
        statsLock.lock()
        let frames = presented, gpu = gpuSeconds/Double(max(1,completed)), sorted = intervals.sorted()
        let p95 = sorted.isEmpty ? 0 : sorted[min(sorted.count-1,Int(Double(sorted.count)*0.95))]
        presented = 0; gpuSeconds = 0; completed = 0; intervals.removeAll(keepingCapacity:true)
        statsLock.unlock()
        NSLog("🎭 Metal face: %.1f presented fps · %.2f ms CPU · %.2f ms GPU · %.2f ms p95 callback gap · %d skipped",
              Double(frames)/elapsed,cpuSeconds/Double(max(1,submitted))*1000,gpu*1000,p95*1000,missed)
        submitted = 0; missed = 0; cpuSeconds = 0; reportAt = now
    }
}
