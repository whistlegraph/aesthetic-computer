import AppKit
import XCTest
@testable import MenuBand

final class ResponsiveKeyboardLayoutTests: XCTestCase {
    func testFittedKeyWidthDrivesGeometryAndHitTesting() {
        let oldLayout = KeyboardIconRenderer.displayLayout
        let oldKeymap = KeyboardIconRenderer.activeKeymap
        let oldSlimKeys = KeyboardIconRenderer.slimKeys
        let oldFittedWhiteW = KeyboardIconRenderer.fittedWhiteW
        defer {
            KeyboardIconRenderer.displayLayout = oldLayout
            KeyboardIconRenderer.activeKeymap = oldKeymap
            KeyboardIconRenderer.slimKeys = oldSlimKeys
            KeyboardIconRenderer.fittedWhiteW = oldFittedWhiteW
        }

        KeyboardIconRenderer.displayLayout = .full
        KeyboardIconRenderer.activeKeymap = .notepat
        KeyboardIconRenderer.slimKeys = true
        KeyboardIconRenderer.fittedWhiteW = 20

        let c = try! XCTUnwrap(KeyboardIconRenderer.keyRect(for: 60))
        let d = try! XCTUnwrap(KeyboardIconRenderer.keyRect(for: 62))
        XCTAssertEqual(d.minX - c.minX, 20, accuracy: 0.001)
        XCTAssertEqual(
            KeyboardIconRenderer.noteAt(NSPoint(x: c.midX, y: c.minY + 1)),
            60
        )
    }

    func testFloatingKeyboardIgnoresAndRestoresMenuBarFitWidth() {
        let oldLayout = KeyboardIconRenderer.displayLayout
        let oldSlimKeys = KeyboardIconRenderer.slimKeys
        let oldFittedWhiteW = KeyboardIconRenderer.fittedWhiteW
        defer {
            KeyboardIconRenderer.displayLayout = oldLayout
            KeyboardIconRenderer.slimKeys = oldSlimKeys
            KeyboardIconRenderer.fittedWhiteW = oldFittedWhiteW
        }

        KeyboardIconRenderer.displayLayout = .full
        KeyboardIconRenderer.slimKeys = true
        KeyboardIconRenderer.fittedWhiteW = 19

        let floatingWidth = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: nil) {
            KeyboardIconRenderer.whiteW
        }
        XCTAssertEqual(floatingWidth, KeyboardIconRenderer.regularWhiteW)
        XCTAssertEqual(KeyboardIconRenderer.whiteW, 19)
    }
}
