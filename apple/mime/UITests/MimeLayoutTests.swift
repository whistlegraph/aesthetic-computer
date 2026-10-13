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
    func testTypeStartsFreshPaintingAndPieceInSharedEditor() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "signed-in"
        app.launch()
        let type = app.buttons["mime-type"].firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 30)); XCTAssertEqual(type.label, "image/png")
        openType(type, in: app)
        let opened = app.buttons["painting-actions"].waitForExistence(timeout: 40)
        if !opened { print(app.debugDescription) }
        XCTAssertTrue(opened)
        XCTAssertTrue(app.staticTexts["painting-resolution"].waitForExistence(timeout: 10))
        XCTAssertEqual(app.staticTexts["painting-resolution"].label, "768 by 768 pixels")
        app.buttons["workspace-settings"].tap()
        XCTAssertTrue(app.buttons["piece-legacy"].waitForExistence(timeout: 10), "Starting from the type preserves the prior project")
        app.buttons["Done"].tap()
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "MIME creates PNG with shared Whistlegraph editor"
        screenshot.lifetime = .keepAlways; add(screenshot)
        app.buttons["mime-creation-back"].tap()
        XCTAssertTrue(type.waitForExistence(timeout: 10))
        app.swipeUp()
        let ac = app.buttons.matching(identifier: "mime-type").matching(NSPredicate(format: "label == %@", "text/javascript")).firstMatch
        XCTAssertTrue(ac.waitForExistence(timeout: 10)); ac.tap()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        XCTAssertTrue(picker.label.contains("Aesthetic.Computer Piece"))
        XCTAssertTrue(app.buttons["type-control"].waitForExistence(timeout: 10))
        let piece = XCTAttachment(screenshot: app.screenshot())
        piece.name = "MIME creates AC piece directly"
        piece.lifetime = .keepAlways; add(piece)
    }

    func testTypeCreationRequiresSignIn() {
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "signed-out"
        app.launch()
        let type = app.buttons["mime-type"].firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 30)); openType(type, in: app)
        XCTAssertTrue(app.buttons["account-entry-login"].waitForExistence(timeout: 30))
        XCTAssertFalse(app.buttons["type-control"].isHittable, "Creation controls remain inaccessible until sign-in")
        app.buttons["mime-creation-back"].tap()
        XCTAssertTrue(type.waitForExistence(timeout: 10))
    }

    private func openType(_ type: XCUIElement, in app: XCUIApplication) {
        let spring = XCUIApplication(bundleIdentifier: "com.apple.springboard")
        let banner = spring.otherElements.matching(identifier: "NotificationShortLookView").firstMatch
        if banner.exists { banner.swipeUp() }
        type.tap()
        if !app.buttons["mime-creation-back"].waitForExistence(timeout: 3), type.isHittable {
            if banner.exists { banner.swipeUp() }
            type.tap()
        }
    }

}
