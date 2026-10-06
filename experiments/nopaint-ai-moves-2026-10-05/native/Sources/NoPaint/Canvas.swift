import AppKit
import SwiftUI

// Image coordinates always stay 256×256; zoom changes only the viewport.
struct CanvasViewport {
    var zoom: CGFloat = 1
    var offset = CGPoint.zero
    func rect(_ size: CGSize) -> CGRect { CGRect(origin: offset, size: CGSize(width: size.width*zoom, height: size.height*zoom)) }
    func pixel(_ point: CGPoint, size: CGSize) -> CGPoint {
        CGPoint(x: (point.x-offset.x)*256/(size.width*zoom), y: (point.y-offset.y)*256/(size.height*zoom))
    }
    mutating func clamp(_ size: CGSize) {
        offset.x = min(0, max(size.width*(1-zoom), offset.x))
        offset.y = min(0, max(size.height*(1-zoom), offset.y))
    }
    mutating func scale(_ value: CGFloat, at point: CGPoint, size: CGSize) {
        let next = min(8, max(1, value)), ratio = next/zoom
        offset = CGPoint(x: point.x-(point.x-offset.x)*ratio, y: point.y-(point.y-offset.y)*ratio)
        zoom = next; clamp(size)
    }
}

final class PixelView: NSView {
    private var current: NSImage?
    private var previous: NSImage?
    private var began: TimeInterval = 0
    private var transitionDuration: TimeInterval = 0.22
    private var sequence = ""
    var clock: () -> TimeInterval = { ProcessInfo.processInfo.systemUptime }
    private var timer: Timer?
    private var tracking: NSTrackingArea?
    private var pointer: CGPoint?
    private var start: CGPoint?
    private var last: CGPoint?
    private var marquee: CGRect?
    private var panning = false
    private var painting = false
    private var bits = [UInt8](repeating: 0, count: 8192)
    private var overlay: NSImage?
    private var strokeBase = ""
    private var fitVersion = 0
    private var viewport = CanvasViewport()
    var duration: Double = 0.22
    var mode = "zoom"
    var brushSize: CGFloat = 16
    var canBrush = false
    var generating = false
    var accepted = ""
    var onBegin: (() -> Void)?
    var onMask: ((String, String) -> Void)?
    var onCrop: (([Int], String) -> Void)?
    var onEnd: (() -> Void)?
    var onSettled: (() -> Void)?
    override var isFlipped: Bool { true }
    override var intrinsicContentSize: NSSize { NSSize(width: NSView.noIntrinsicMetric, height: NSView.noIntrinsicMetric) }
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    func update(_ image: NSImage?, immediate: Bool, mask: String?, fit: Int, sequence nextSequence: String = "", blendBoundary: Bool = false) {
        if fit != fitVersion { viewport = CanvasViewport(); fitVersion = fit }
        if !painting {
            let next = mask.flatMap { Data(base64Encoded: $0) }.map(Array.init) ?? [UInt8](repeating: 0, count: 8192)
            if next.count == 8192 && next != bits { bits = next; makeOverlay() }
        }
        viewport.clamp(bounds.size); needsDisplay = true
        setAccessibilityLabel(generating ? "Image forming" : "Current painting")
        setAccessibilityRole(.image)
        let boundary = sequence != nextSequence
        sequence = nextSequence
        let snap = immediate || (boundary && !blendBoundary) || NSWorkspace.shared.accessibilityDisplayShouldReduceMotion
        if current === image {
            if snap { previous = nil; timer?.invalidate() }
            if previous == nil { onSettled?() }
            return
        }
        // Freeze the visible composite, not the unfinished target of the last
        // transition. A new preview then continues without jumping forward.
        previous = snap ? nil : presentation(at: clock())
        current = image; began = clock(); transitionDuration = duration; timer?.invalidate()
        if previous != nil {
            let next = Timer(timeInterval: 1/60, repeats: true) { [weak self] timer in
                guard let self else { timer.invalidate(); return }
                self.needsDisplay = true
                if self.clock() - self.began >= self.transitionDuration { self.previous = nil; timer.invalidate(); self.onSettled?() }
            }
            timer = next
            RunLoop.main.add(next, forMode: .common)
        } else { onSettled?() }
        setAccessibilityHelp(mode == "zoom" ? "Drag a box to crop the painting. Scroll or pinch to inspect; double-click to fit." : "Drag to mark the area to repaint. Option-drag erases the mask.")
    }

    private func blend(_ time: TimeInterval) -> CGFloat {
        let t = min(1, max(0, (time - began) / max(0.001, transitionDuration)))
        return CGFloat(t * t * (3 - 2 * t))
    }

    // Rasterize just the image buffer; viewport, mask, and pointer overlays
    // never become part of a later model frame.
    func presentation(at time: TimeInterval) -> NSImage? {
        guard let current, let previous else { return current }
        guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: 256, pixelsHigh: 256,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 1024, bitsPerPixel: 32),
              let context = NSGraphicsContext(bitmapImageRep: bitmap) else { return current }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = context
        context.imageInterpolation = .none
        let rect = NSRect(x: 0, y: 0, width: 256, height: 256)
        current.draw(in: rect, from: .zero, operation: .copy, fraction: 1)
        previous.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1 - blend(time))
        NSGraphicsContext.restoreGraphicsState()
        let snapshot = NSImage(size: rect.size); snapshot.addRepresentation(bitmap)
        return snapshot
    }

    private func makeOverlay() {
        guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: 256, pixelsHigh: 256,
                bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                colorSpaceName: .deviceRGB, bytesPerRow: 1024, bitsPerPixel: 32), let data = bitmap.bitmapData else { return }
        for i in 0..<65536 {
            data[i*4] = 218; data[i*4+1] = 61; data[i*4+2] = 112
            data[i*4+3] = bits[i/8] & (1 << (7-i%8)) == 0 ? 0 : 95
        }
        let image = NSImage(size: NSSize(width: 256, height: 256)); image.addRepresentation(bitmap); overlay = image
    }

    private func stamp(_ point: CGPoint, erase: Bool) {
        let radius = brushSize/2
        let left = max(0, Int(floor(point.x-radius))), right = min(255, Int(ceil(point.x+radius)))
        let top = max(0, Int(floor(point.y-radius))), bottom = min(255, Int(ceil(point.y+radius)))
        guard left <= right, top <= bottom else { return }
        for y in top...bottom { for x in left...right {
            let dx = CGFloat(x)+0.5-point.x, dy = CGFloat(y)+0.5-point.y
            if dx*dx+dy*dy <= radius*radius {
                let i = y*256+x, bit = UInt8(1 << (7-i%8))
                if erase { bits[i/8] &= ~bit } else { bits[i/8] |= bit }
            }
        } }
    }
    private func stroke(to point: CGPoint, erase: Bool) {
        let previous = last ?? point
        let count = max(1, Int(ceil(hypot(point.x-previous.x, point.y-previous.y)/max(1, brushSize/4))))
        for i in 0...count {
            let t = CGFloat(i)/CGFloat(count)
            stamp(CGPoint(x: previous.x+(point.x-previous.x)*t, y: previous.y+(point.y-previous.y)*t), erase: erase)
        }
        last = point; makeOverlay(); needsDisplay = true
    }

    override func updateTrackingAreas() {
        if let tracking { removeTrackingArea(tracking) }
        tracking = NSTrackingArea(rect: .zero, options: [.mouseMoved, .mouseEnteredAndExited, .activeInKeyWindow, .inVisibleRect], owner: self)
        addTrackingArea(tracking!); super.updateTrackingAreas()
    }
    override func mouseEntered(with event: NSEvent) { mouseMoved(with: event) }
    override func mouseExited(with event: NSEvent) { pointer = nil; needsDisplay = true; NSCursor.arrow.set() }
    override func mouseMoved(with event: NSEvent) {
        pointer = convert(event.locationInWindow, from: nil); needsDisplay = true
        if mode == "inpaint" || viewport.zoom == 1 { NSCursor.crosshair.set() } else { NSCursor.openHand.set() }
    }
    override func mouseDown(with event: NSEvent) {
        let point = convert(event.locationInWindow, from: nil)
        if mode == "inpaint" {
            guard canBrush else { return }
            painting = true; strokeBase = accepted; last = nil; onBegin?()
            stroke(to: viewport.pixel(point, size: bounds.size), erase: event.modifierFlags.contains(.option))
        } else {
            guard canBrush else { return }
            if event.clickCount == 2 { viewport = CanvasViewport(); needsDisplay = true; return }
            strokeBase = accepted; onBegin?()
            start = point; last = point; panning = false
        }
    }
    override func mouseDragged(with event: NSEvent) {
        let point = convert(event.locationInWindow, from: nil); pointer = point
        if painting {
            stroke(to: viewport.pixel(point, size: bounds.size), erase: event.modifierFlags.contains(.option))
        } else if let start, let last {
            if panning {
                viewport.offset.x += point.x-last.x; viewport.offset.y += point.y-last.y
                viewport.clamp(bounds.size); self.last = point
            } else {
                marquee = CGRect(x: min(point.x,start.x), y: min(point.y,start.y), width: abs(point.x-start.x), height: abs(point.y-start.y)).intersection(bounds)
            }
            needsDisplay = true
        }
    }
    override func mouseUp(with event: NSEvent) {
        if painting {
            painting = false; last = nil
            onMask?(Data(bits).base64EncodedString(), strokeBase)
        } else if let box = marquee, box.width > 8, box.height > 8 {
            let a = viewport.pixel(box.origin, size: bounds.size)
            let b = viewport.pixel(CGPoint(x: box.maxX, y: box.maxY), size: bounds.size)
            let crop = [max(0, Int(floor(a.x))), max(0, Int(floor(a.y))), min(256, Int(ceil(b.x))), min(256, Int(ceil(b.y)))]
            viewport = CanvasViewport(); onCrop?(crop, strokeBase)
        } else { onEnd?() }
        start = nil; last = nil; marquee = nil; needsDisplay = true
    }
    override func magnify(with event: NSEvent) {
        guard !painting else { return }
        viewport.scale(viewport.zoom*(1+event.magnification), at: convert(event.locationInWindow, from: nil), size: bounds.size); needsDisplay = true
    }
    override func scrollWheel(with event: NSEvent) {
        guard !painting else { return }
        viewport.scale(viewport.zoom*exp(-event.scrollingDeltaY*0.012), at: convert(event.locationInWindow, from: nil), size: bounds.size); needsDisplay = true
    }
    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        needsDisplay = true
    }
    override func draw(_ dirtyRect: NSRect) {
        NoPaintPalette.canvas.setFill(); bounds.fill()
        NSGraphicsContext.saveGraphicsState(); defer { NSGraphicsContext.restoreGraphicsState() }
        NSBezierPath(rect: bounds).addClip(); NSGraphicsContext.current?.imageInterpolation = .none
        let rect = viewport.rect(bounds.size)
        current?.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true, hints: nil)
        if let previous { previous.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1-blend(clock()), respectFlipped: true, hints: nil) }
        if mode == "inpaint" && (painting || (!generating && pointer != nil)) {
            overlay?.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1, respectFlipped: true, hints: nil)
        }
        if let marquee {
            NSColor.white.withAlphaComponent(0.2).setFill(); marquee.fill()
            NSColor.white.setStroke(); let path = NSBezierPath(rect: marquee); path.lineWidth = 1; path.stroke()
        }
        if mode == "inpaint", let pointer {
            let diameter = brushSize*bounds.width*viewport.zoom/256
            let ring = NSBezierPath(ovalIn: CGRect(x: pointer.x-diameter/2, y: pointer.y-diameter/2, width: diameter, height: diameter))
            NSColor.black.withAlphaComponent(0.7).setStroke(); ring.lineWidth = 3; ring.stroke()
            NSColor.white.setStroke(); ring.lineWidth = 1; ring.stroke()
        }
    }
}

struct PixelCanvas: NSViewRepresentable {
    @ObservedObject var game: GameStore
    func makeNSView(context: Context) -> PixelView { PixelView() }
    func sizeThatFits(_ proposal: ProposedViewSize, nsView: PixelView, context: Context) -> CGSize? {
        CGSize(width: proposal.width ?? 256, height: proposal.height ?? 256)
    }
    func updateNSView(_ view: PixelView, context: Context) {
        view.mode = game.dragMode; view.brushSize = game.brushSize
        view.canBrush = game.state != nil && !game.sending && !game.pendingDone && game.connectionError == nil
        view.generating = game.busy; view.accepted = game.state?.accepted ?? ""
        view.duration = game.busy ? 0.22 : 0.4
        view.onBegin = { game.beginMask() }
        view.onMask = { bits, base in game.finishMask(bits, base: base) }
        view.onCrop = { box, base in game.finishCrop(box, base: base) }
        view.onEnd = { game.endMarking() }
        let path = game.imagePath, sequence = game.imageSequence
        view.onSettled = { [weak game] in
            Task { @MainActor in
                if let game, game.imagePath == path, game.imageSequence == sequence, !game.imageSettled { game.imageSettled = true }
            }
        }
        view.update(game.image, immediate: game.imageImmediate || game.peeking || game.marking,
                    mask: game.state?.mask, fit: game.fitVersion, sequence: game.imageSequence, blendBoundary: game.imageBlendBoundary)
    }
}
