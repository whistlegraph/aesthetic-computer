import CoreText
import Foundation
import Metal

/// One corner of a glyph quad. Position stays in the engine's logical space —
/// the same x/y JavaScript hands the host — and the vertex stage applies the
/// shared logical-to-clip transform, so the formula lives in one place instead
/// of being copied beside `MetalSceneView.triangle`.
struct GlyphVertex {
    var position: SIMD2<Float>
    var uv: SIMD2<Float>
    var color: SIMD4<Float>
}

/// Core Text glyph sheet for the Metal text pipeline.
///
/// Metal has no text primitive, so every character the HUD writes becomes a
/// textured quad cut from one alpha sheet: Core Text rasterizes a glyph the
/// first time it is seen, the shelf packer finds it a slot, and from then on
/// the character costs six vertices.
///
/// Not thread-safe — build text on whichever thread builds the rest of the
/// frame's draw commands.
final class GlyphAtlas {
    /// Alpha coverage in `r`. Bind at texture(0) of the glyph fragment stage.
    let texture: MTLTexture

    private let side: Int
    /// Glyphs rasterize at `size * scale` pixels while quads stay logical, so
    /// text is sharp on a retina drawable without the engine knowing about it.
    private let scale: CGFloat
    private var fonts: [FontKey: CTFont] = [:]
    private var cells: [CellKey: Cell] = [:]
    private var shelfX = 0
    private var shelfY = 0
    private var shelfHeight = 0
    private var askedForBundledFonts = false

    init(device: MTLDevice, side: Int = 1024, scale: CGFloat = 2) {
        self.side = side
        self.scale = max(1, scale)
        let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .r8Unorm,
            width: side, height: side, mipmapped: false)
        descriptor.usage = .shaderRead
        #if os(macOS)
        // A discrete-GPU Mac cannot back a texture with shared memory; managed
        // still takes replace() straight from the CPU.
        descriptor.storageMode = device.hasUnifiedMemory ? .shared : .managed
        #else
        descriptor.storageMode = .shared
        #endif
        guard let sheet = device.makeTexture(descriptor: descriptor) else {
            fatalError("Metal glyph atlas allocation failed")
        }
        texture = sheet
        clear()
    }

    /// Appends `text` as textured quads, six vertices per drawn glyph.
    ///
    /// x/y/size are logical units and r/g/b are 0–255, matching the host's
    /// `comicWrite`/`systemWrite`/`write` bindings. y is the TOP of the line,
    /// not a baseline: the web shell writes with `textBaseline = "top"` and the
    /// AppKit host draws with `NSString.draw(at:)`, both of which treat the
    /// point as the top-left of the line box. So the pen drops by the font's
    /// ascent before the first glyph, and "\n" returns to the original x one
    /// `size * 1.15` step down — comicWrite's line height.
    func write(_ text: String, family: String, x: Double, y: Double, size: Double,
               red: Double, green: Double, blue: Double,
               into vertices: inout [GlyphVertex]) {
        guard !text.isEmpty, x.isFinite, y.isFinite, size.isFinite, size > 0 else { return }
        // Whole points only. The cache would otherwise hold a separate sheet of
        // cells for every fractional size a resize walks through.
        let points = max(1, size.rounded())
        let ascent = Double(CTFontGetAscent(font(family: family, points: points))) / Double(scale)
        let ink = SIMD4<Float>(Float(channel(red)), Float(channel(green)),
                               Float(channel(blue)), 1)
        var pen = x
        var baseline = y + ascent
        for character in text {
            if character == "\n" {
                pen = x
                baseline += points * 1.15
                continue
            }
            guard let cell = cell(for: character, family: family, points: points) else { continue }
            if cell.width > 0, cell.height > 0 {
                let left = Float(pen + Double(cell.left))
                let top = Float(baseline - Double(cell.top))
                let right = left + Float(cell.width)
                let bottom = top + Float(cell.height)
                let topLeft = GlyphVertex(position: SIMD2(left, top),
                                          uv: SIMD2(cell.u0, cell.vTop), color: ink)
                let topRight = GlyphVertex(position: SIMD2(right, top),
                                           uv: SIMD2(cell.u1, cell.vTop), color: ink)
                let bottomLeft = GlyphVertex(position: SIMD2(left, bottom),
                                             uv: SIMD2(cell.u0, cell.vBottom), color: ink)
                let bottomRight = GlyphVertex(position: SIMD2(right, bottom),
                                              uv: SIMD2(cell.u1, cell.vBottom), color: ink)
                vertices.append(contentsOf: [topLeft, topRight, bottomLeft,
                                             topRight, bottomRight, bottomLeft])
            }
            pen += Double(cell.advance)
        }
    }

    /// Forgets every cached cell and wipes the sheet. Safe between frames; mid
    /// frame it scrambles quads already emitted against the old packing.
    func reset() {
        cells.removeAll(keepingCapacity: true)
        shelfX = 0
        shelfY = 0
        shelfHeight = 0
        clear()
    }

    /// Vertex stage wants the quads at buffer(0) and the frame's logical size
    /// as a `float2` at buffer(1):
    ///
    ///     var logical = SIMD2<Float>(Float(width), Float(height))
    ///     encoder.setVertexBytes(&logical, length: MemoryLayout<SIMD2<Float>>.stride, index: 1)
    ///     encoder.setFragmentTexture(atlas.texture, index: 0)
    ///
    /// The clip transform is character-for-character the one MetalSceneView
    /// applies on the CPU, which is the whole point — glyphs and triangles have
    /// to agree on where logical (0, 0) is.
    static let shaderSource = """
    #include <metal_stdlib>
    using namespace metal;
    struct Vertex { float2 position; float2 uv; float4 color; };
    struct Raster { float4 position [[position]]; float2 uv; float4 color; };
    constexpr sampler sheet(filter::linear, address::clamp_to_edge, coord::normalized);
    vertex Raster glyph_vertex(const device Vertex *vertices [[buffer(0)]],
                               constant float2 &logical [[buffer(1)]],
                               uint id [[vertex_id]]) {
      Raster out;
      float2 at = vertices[id].position;
      out.position = float4(at.x / (logical.x * 0.5) - 1.0,
                            1.0 - at.y / (logical.y * 0.5), 0.0, 1.0);
      out.uv = vertices[id].uv;
      out.color = vertices[id].color;
      return out;
    }
    fragment float4 glyph_fragment(Raster in [[stage_in]],
                                   texture2d<float> atlas [[texture(0)]]) {
      float coverage = atlas.sample(sheet, in.uv).r;
      return float4(in.color.rgb, in.color.a * coverage);
    }
    """

    /// Straight (non-premultiplied) alpha blending, and deliberately no depth
    /// state: text is a HUD layer drawn last, so it wants the default
    /// always-pass/never-write depth behavior rather than the scene's.
    static func pipeline(device: MTLDevice, colorFormat: MTLPixelFormat = .bgra8Unorm,
                         depthFormat: MTLPixelFormat = .depth32Float) throws
        -> MTLRenderPipelineState {
        let library = try device.makeLibrary(source: shaderSource, options: nil)
        let descriptor = MTLRenderPipelineDescriptor()
        descriptor.vertexFunction = library.makeFunction(name: "glyph_vertex")
        descriptor.fragmentFunction = library.makeFunction(name: "glyph_fragment")
        descriptor.depthAttachmentPixelFormat = depthFormat
        let color = descriptor.colorAttachments[0]!
        color.pixelFormat = colorFormat
        color.isBlendingEnabled = true
        color.sourceRGBBlendFactor = .sourceAlpha
        color.destinationRGBBlendFactor = .oneMinusSourceAlpha
        color.sourceAlphaBlendFactor = .sourceAlpha
        color.destinationAlphaBlendFactor = .oneMinusSourceAlpha
        return try device.makeRenderPipelineState(descriptor: descriptor)
    }

    private struct CellKey: Hashable {
        let family: String
        let points: Int
        let character: Character
    }

    private struct Cell {
        var u0: Float = 0
        var u1: Float = 0
        /// A CGBitmapContext's user space is y-up but its buffer still starts
        /// at the image's top row, and `replace` lays that first row at the
        /// region's low v — so a cell arrives upright and V runs the same way
        /// down the sheet as it does down the quad.
        var vTop: Float = 0
        var vBottom: Float = 0
        var left: CGFloat = 0
        /// Baseline to the bitmap's top edge, positive upward.
        var top: CGFloat = 0
        var width: CGFloat = 0
        var height: CGFloat = 0
        var advance: CGFloat = 0
    }

    private func cell(for character: Character, family: String, points: Double) -> Cell? {
        let key = CellKey(family: family, points: Int(points), character: character)
        if let cached = cells[key] { return cached }
        let face = font(family: family, points: points)
        let utf16 = Array(character.utf16)
        var glyphs = [CGGlyph](repeating: 0, count: utf16.count)
        // An unmapped character (emoji in a text face) draws nothing rather
        // than pulling in a Core Text fallback cascade; the HUD writes ASCII.
        guard CTFontGetGlyphsForCharacters(face, utf16, &glyphs, utf16.count),
              var glyph = glyphs.first, glyph != 0 else { return nil }

        var advance = CGSize.zero
        CTFontGetAdvancesForGlyphs(face, .horizontal, &glyph, &advance, 1)
        var bounds = CGRect.zero
        CTFontGetBoundingRectsForGlyphs(face, .horizontal, &glyph, &bounds, 1)
        guard bounds.width.isFinite, bounds.height.isFinite else { return nil }

        var cell = Cell(advance: advance.width / scale)
        guard bounds.width > 0, bounds.height > 0 else {
            // A space: no ink, still moves the pen.
            cells[key] = cell
            return cell
        }

        // One transparent pixel around every glyph so linear filtering at the
        // quad's edge samples emptiness instead of the neighbour.
        let pad: CGFloat = 1
        let originX = floor(bounds.minX) - pad
        let originY = floor(bounds.minY) - pad
        let width = Int(ceil(bounds.maxX + pad) - originX)
        let height = Int(ceil(bounds.maxY + pad) - originY)
        guard let slot = allocate(width: width, height: height) else { return nil }
        guard let raster = CGContext(data: nil, width: width, height: height,
            bitsPerComponent: 8, bytesPerRow: width,
            space: CGColorSpaceCreateDeviceGray(),
            bitmapInfo: CGImageAlphaInfo.none.rawValue), let pixels = raster.data else {
            return nil
        }
        raster.setShouldAntialias(true)
        raster.setFillColor(gray: 1, alpha: 1)
        var position = CGPoint(x: -originX, y: -originY)
        CTFontDrawGlyphs(face, &glyph, &position, 1, raster)
        texture.replace(region: MTLRegionMake2D(slot.x, slot.y, width, height),
                        mipmapLevel: 0, withBytes: pixels,
                        bytesPerRow: raster.bytesPerRow)

        let sheet = Float(side)
        cell.u0 = Float(slot.x) / sheet
        cell.u1 = Float(slot.x + width) / sheet
        cell.vTop = Float(slot.y) / sheet
        cell.vBottom = Float(slot.y + height) / sheet
        cell.left = originX / scale
        cell.top = (originY + CGFloat(height)) / scale
        cell.width = CGFloat(width) / scale
        cell.height = CGFloat(height) / scale
        cells[key] = cell
        return cell
    }

    /// Shelf packing: cells fill a row left to right, then the row's tallest
    /// glyph sets the next shelf's floor. Cheap, and glyph cells of one size
    /// are near enough the same height that the waste is a few percent.
    ///
    /// The sheet never grows. 1024² holds several hundred cells at HUD sizes —
    /// far more than oskiewar's working set — and growing would mean
    /// re-rasterizing everything while the renderer holds the old texture.
    /// Overflow instead wipes and refills: the engine rebuilds every draw
    /// command each frame, so the cost is at most one frame of scrambled cells
    /// and the refilled sheet holds exactly the glyphs still in use.
    private func allocate(width: Int, height: Int) -> (x: Int, y: Int)? {
        guard width <= side, height <= side else { return nil }
        if let slot = shelve(width: width, height: height) { return slot }
        reset()
        return shelve(width: width, height: height)
    }

    private func shelve(width: Int, height: Int) -> (x: Int, y: Int)? {
        if shelfX + width > side {
            shelfY += shelfHeight
            shelfX = 0
            shelfHeight = 0
        }
        guard shelfY + height <= side else { return nil }
        let slot = (x: shelfX, y: shelfY)
        shelfX += width
        shelfHeight = max(shelfHeight, height)
        return slot
    }

    private func clear() {
        let empty = [UInt8](repeating: 0, count: side * side)
        empty.withUnsafeBytes { bytes in
            texture.replace(region: MTLRegionMake2D(0, 0, side, side), mipmapLevel: 0,
                            withBytes: bytes.baseAddress!, bytesPerRow: side)
        }
    }

    private struct FontKey: Hashable {
        let family: String
        let points: Int
    }

    /// One face per family and point size, built at raster resolution so the
    /// metrics read back in the same units the bitmap was drawn in.
    private func font(family: String, points: Double) -> CTFont {
        let key = FontKey(family: family, points: Int(points))
        if let cached = fonts[key] { return cached }
        let pixels = CGFloat(points) * scale
        var face = CTFontCreateWithName(family as CFString, pixels, nil)
        if !named(face, family), !askedForBundledFonts {
            registerBundledFonts()
            face = CTFontCreateWithName(family as CFString, pixels, nil)
        }
        // Still no match means the family is genuinely absent — Core Text has
        // already substituted the system face, which is the fallback we want.
        fonts[key] = face
        return face
    }

    private func named(_ face: CTFont, _ family: String) -> Bool {
        (CTFontCopyFamilyName(face) as String).caseInsensitiveCompare(family) == .orderedSame
    }

    /// Comic Relief rides in the app bundle as a raw TTF (the web shell serves
    /// the same file). iOS only auto-registers faces listed under UIAppFonts,
    /// so ask Core Text directly; on macOS the app already registered it at
    /// launch and the duplicate is a no-op.
    private func registerBundledFonts() {
        askedForBundledFonts = true
        guard let url = Bundle.main.url(forResource: "ComicRelief-Regular",
                                        withExtension: "ttf") else { return }
        CTFontManagerRegisterFontsForURL(url as CFURL, .process, nil)
    }

    private func channel(_ value: Double) -> Double {
        max(0, min(255, value)) / 255
    }
}
