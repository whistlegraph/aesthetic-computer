import AppKit
import XCTest
@testable import NoPaint

@MainActor final class CanvasTests: XCTestCase {
    func solid(_ color: NSColor) -> NSImage {
        let image = NSImage(size: NSSize(width: 256, height: 256))
        image.lockFocus(); color.setFill(); NSRect(x: 0, y: 0, width: 256, height: 256).fill(); image.unlockFocus()
        return image
    }
    func rgb(_ image: NSImage?) -> [CGFloat] {
        let bitmap = NSBitmapImageRep(data: image!.tiffRepresentation!)!
        let color = bitmap.colorAt(x: 128, y: 128)!.usingColorSpace(.deviceRGB)!
        return [color.redComponent, color.greenComponent, color.blueComponent]
    }
    func assertSame(_ a: [CGFloat], _ b: [CGFloat], file: StaticString = #filePath, line: UInt = #line) {
        for i in 0..<3 { XCTAssertEqual(a[i], b[i], accuracy: 2.0/255, file: file, line: line) }
    }
    func testInterruptedBlendContinuesFromVisiblePixels() async {
        let view = PixelView(); var time = 0.0; view.clock = { time }; view.duration = 1
        view.update(solid(.red), immediate: true, mask: nil, fit: 0, sequence: "move")
        view.update(solid(.blue), immediate: false, mask: nil, fit: 0, sequence: "move")
        time = 0.3
        let before = rgb(view.presentation(at: time))
        view.update(solid(.green), immediate: false, mask: nil, fit: 0, sequence: "move")
        assertSame(before, rgb(view.presentation(at: time)))
        XCTAssertGreaterThan(before[0], 0.5)
        time = 1.3
        assertSame(rgb(solid(.green)), rgb(view.presentation(at: time)))
    }
    func testPreviewToFinalStatusDoesNotRetimeExistingBlend() async {
        let view = PixelView(); var time = 0.0; view.clock = { time }; view.duration = 1
        let blue = solid(.blue)
        view.update(solid(.red), immediate: true, mask: nil, fit: 0)
        view.update(blue, immediate: false, mask: nil, fit: 0)
        time = 0.5
        let before = rgb(view.presentation(at: time))
        view.duration = 2
        view.update(blue, immediate: false, mask: nil, fit: 0)
        assertSame(before, rgb(view.presentation(at: time)))
    }
    func testModelBoundaryDoesNotBlendRejectedPreviews() async {
        let view = PixelView(); var time = 0.0; view.clock = { time }; view.duration = 1
        let accepted = solid(.green)
        view.update(solid(.red), immediate: true, mask: nil, fit: 0, sequence: "old-model")
        view.update(solid(.blue), immediate: false, mask: nil, fit: 0, sequence: "old-model")
        time = 0.3
        view.update(accepted, immediate: false, mask: nil, fit: 0, sequence: "new-model")
        assertSame(rgb(accepted), rgb(view.presentation(at: time)))
    }
    func testHoverPreviewFadesAcrossHistoryAndCanReverseWithoutJumping() async {
        let view=PixelView();var time=0.0;view.clock={time};view.duration=1
        let current=solid(.red), previous=solid(.blue)
        view.update(current,immediate:true,mask:nil,fit:0,sequence:"canvas")
        view.update(previous,immediate:false,mask:nil,fit:0,sequence:"before",blendBoundary:true)
        time=0.5
        let midway=rgb(view.presentation(at:time))
        XCTAssertGreaterThan(midway[0],0.4);XCTAssertGreaterThan(midway[2],0.4)
        view.update(current,immediate:false,mask:nil,fit:0,sequence:"canvas",blendBoundary:true)
        assertSame(midway,rgb(view.presentation(at:time)))
        time=1.5;assertSame(rgb(current),rgb(view.presentation(at:time)))
    }
    func testImmediateBeforeFinishesAnInProgressBlendEvenWithSameTarget() async {
        let view = PixelView(); var time = 0.0; view.clock = { time }; view.duration = 1
        let accepted = solid(.blue)
        view.update(solid(.red), immediate: true, mask: nil, fit: 0)
        view.update(accepted, immediate: false, mask: nil, fit: 0)
        time = 0.2
        view.update(accepted, immediate: true, mask: nil, fit: 0)
        assertSame(rgb(accepted), rgb(view.presentation(at: time)))
    }
    func testInterruptedBlendPreservesImageOrientation() async {
        let image = solid(.red)
        image.lockFocus(); NSColor.blue.setFill(); NSRect(x: 0, y: 0, width: 128, height: 96).fill(); image.unlockFocus()
        let view = PixelView(); var time = 0.0; view.clock = { time }; view.duration = 1
        view.update(image, immediate: true, mask: nil, fit: 0)
        view.update(solid(.green), immediate: false, mask: nil, fit: 0)
        time = 0.3
        let before = NSBitmapImageRep(data: view.presentation(at: time)!.tiffRepresentation!)!
        view.update(solid(.white), immediate: false, mask: nil, fit: 0)
        let after = NSBitmapImageRep(data: view.presentation(at: time)!.tiffRepresentation!)!
        for (x,y) in [(32,32),(32,224),(224,32),(224,224)] {
            let a=before.colorAt(x:x,y:y)!.usingColorSpace(.deviceRGB)!
            let b=after.colorAt(x:x,y:y)!.usingColorSpace(.deviceRGB)!
            assertSame([a.redComponent,a.greenComponent,a.blueComponent], [b.redComponent,b.greenComponent,b.blueComponent])
        }
        XCTAssertGreaterThan(abs(before.colorAt(x:32,y:32)!.blueComponent - before.colorAt(x:32,y:224)!.blueComponent), 0.5)
    }
}
