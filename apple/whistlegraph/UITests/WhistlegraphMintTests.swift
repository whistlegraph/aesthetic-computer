import XCTest
import UIKit

// Explicit physical-phone test. Pins only the selected owner-authenticated
// version and stops before wallet connection, signing, or minting.
final class WhistlegraphMintTests: XCTestCase {
    func testPhoneSpinningTreePackPreview() {
        _ = openTreePreview()
    }

    // Opens Temple only. The user reviews and approves any wallet request.
    func testPhoneSpinningTreeTempleHandoff() {
        let app = openTreePreview()
        app.webViews.buttons["Connect Tezos wallet"].tap()
        let temple = app.staticTexts["Temple"]
        XCTAssertTrue(temple.waitForExistence(timeout: 30))
        let picker = XCTAttachment(screenshot: app.screenshot())
        picker.name = "Mint wallet picker"; picker.lifetime = .keepAlways; add(picker)
        temple.tap()
        let open = app.buttons["Open"]
        if open.waitForExistence(timeout: 3) { open.tap() }
        let wallet = XCUIApplication(bundleIdentifier: "com.madfish.temple-wallet")
        XCTAssertTrue(wallet.wait(for: .runningForeground, timeout: 25))
        let handoff = XCTAttachment(screenshot: wallet.screenshot())
        handoff.name = "Spinning tree Temple handoff"; handoff.lifetime = .keepAlways; add(handoff)
    }

    private func openTreePreview() -> XCUIApplication {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launch()
        XCTAssertTrue(app.buttons["workspace-settings"].waitForExistence(timeout: 30))
        app.buttons["workspace-settings"].tap()
        let tree = app.buttons["piece-wgDefen"]
        if !tree.waitForExistence(timeout: 5) { app.swipeUp() }
        XCTAssertTrue(tree.waitForExistence(timeout: 10), "wgDefen is saved on this phone")
        tree.tap()
        XCTAssertTrue(app.buttons["workspace-settings"].waitForExistence(timeout: 30))
        app.buttons["workspace-settings"].tap()
        let mint = app.buttons["pieces-mint"]
        XCTAssertTrue(mint.waitForExistence(timeout: 10))
        mint.tap()
        app.swipeUp()
        let title = app.textFields["mint-title"]
        XCTAssertTrue(title.waitForExistence(timeout: 10))
        if title.isEnabled {
            title.tap()
            if let value = title.value as? String { title.typeText(String(repeating: XCUIKeyboardKey.delete.rawValue, count: value.count)) }
            title.typeText("Spinning tree\n")
        }
        let form = XCTAttachment(screenshot: app.screenshot()); form.name = "wgDefen native mint settings"; form.lifetime = .keepAlways; add(form)
        app.swipeUp()
        let prepare = app.buttons["mint-prepare"]
        XCTAssertTrue(prepare.waitForExistence(timeout: 10))
        let enabled = expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: prepare)
        XCTAssertEqual(XCTWaiter.wait(for: [enabled], timeout: 20), .completed)
        prepare.tap()
        let connect = app.webViews.buttons["Connect Tezos wallet"]
        XCTAssertTrue(connect.waitForExistence(timeout: 180), "packed preview is ready inside Whistlegraph")
        // The controls can load while an IPFS gateway fails. Require the
        // actual wgDefen green foliage and purple sky in the browser image.
        let painted = expectation(for: NSPredicate { _, _ in Self.treeIsPainted(app.screenshot().image) }, evaluatedWith: nil)
        XCTAssertEqual(XCTWaiter.wait(for: [painted], timeout: 60), .completed, "packed tree renders on the phone")
        let preview = XCTAttachment(screenshot: app.screenshot()); preview.name = "wgDefen packed HTML in app"; preview.lifetime = .keepAlways; add(preview)
        XCTAssertFalse(app.webViews.buttons["Mint in wallet"].exists, "mint requires a connected wallet")
        return app
    }

    private static func treeIsPainted(_ image: UIImage) -> Bool {
        guard let cg = image.cgImage else { return false }
        var pixels = [UInt8](repeating: 0, count: 120 * 260 * 4)
        return pixels.withUnsafeMutableBytes { bytes in
            guard let context = CGContext(data: bytes.baseAddress, width: 120, height: 260, bitsPerComponent: 8,
                bytesPerRow: 120 * 4, space: CGColorSpaceCreateDeviceRGB(),
                bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue | CGBitmapInfo.byteOrder32Big.rawValue) else { return false }
            context.draw(cg, in: CGRect(x: 0, y: 0, width: 120, height: 260))
            var green = 0, purple = 0
            for i in stride(from: 0, to: bytes.count, by: 4) {
                let r = Int(bytes[i]), g = Int(bytes[i + 1]), b = Int(bytes[i + 2])
                if g > r + 20 && g > b + 20 { green += 1 }
                if b > r + 30 && r > g + 20 { purple += 1 }
            }
            return green > 500 && purple > 500
        }
    }
}
