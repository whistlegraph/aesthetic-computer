import XCTest

final class MimeLayoutTests: XCTestCase {
    func testNativeIdentityCorners() {
        let app = XCUIApplication()
        app.launch()
        let code = app.buttons["mime-global-identifier"].firstMatch
        XCTAssertTrue(code.waitForExistence(timeout: 45), "first piece opens without a swipe")
        XCTAssertFalse(app.staticTexts["Swipe up for the feed"].exists)
        let author = app.descendants(matching: .any).matching(identifier: "mime-author").firstMatch
        let type = app.descendants(matching: .any).matching(identifier: "mime-type").firstMatch
        XCTAssertTrue(author.exists)
        XCTAssertTrue(type.exists)
        XCTAssertFalse(code.label.contains("@"))
        XCTAssertTrue(type.label.contains("/"))
        XCTAssertLessThan(code.frame.maxY, author.frame.minY)
        XCTAssertLessThan(author.frame.maxX, type.frame.minX)
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = "MIME native identity layout"; image.lifetime = .keepAlways; add(image)
    }
}
