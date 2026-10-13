import XCTest

final class MimeLayoutTests: XCTestCase {
    func testNativeIdentityCorners() {
        let app = XCUIApplication()
        app.launch()
        let code = app.buttons["mime-global-identifier"].firstMatch
        XCTAssertTrue(code.waitForExistence(timeout: 45), "first piece opens without a swipe")
        waitForReveal(in: app)
        XCTAssertFalse(app.staticTexts["Swipe up for the feed"].exists)
        let author = app.descendants(matching: .any).matching(identifier: "mime-author").firstMatch
        let type = app.descendants(matching: .any).matching(identifier: "mime-type").firstMatch
        XCTAssertTrue(author.exists)
        XCTAssertTrue(type.exists)
        XCTAssertFalse(code.label.contains("@"))
        XCTAssertTrue(type.label.contains("/"))
        XCTAssertLessThan(code.frame.maxY, author.frame.minY)
        XCTAssertLessThan(author.frame.maxX, type.frame.minX)
        XCTAssertEqual(author.frame.maxY, type.frame.maxY, accuracy: 3)
        XCTAssertTrue(app.descendants(matching: .any)["mime-comic-logo"].firstMatch.exists)
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
        waitForEditor(in: app)
        XCTAssertFalse(app.staticTexts["Checking your account…"].exists)
        XCTAssertFalse(app.descendants(matching: .any)["account-entry"].firstMatch.exists)
        let card = app.descendants(matching: .any)["mime-creation"].firstMatch.frame
        XCTAssertGreaterThan(card.minX, 8); XCTAssertGreaterThan(card.minY, 80)
        XCTAssertLessThan(card.width, app.frame.width)
        let chalk = app.buttons["draw-control"], typeButton = app.buttons["type-control"], talk = app.descendants(matching: .any)["talk-control"].firstMatch
        XCTAssertGreaterThan(chalk.frame.midX, typeButton.frame.midX)
        XCTAssertLessThan(chalk.frame.midX, talk.frame.midX)
        XCTAssertEqual(chalk.frame.midY, typeButton.frame.midY, accuracy: 4)
        XCTAssertFalse(app.descendants(matching: .any)["painting-resolution"].firstMatch.exists)
        app.buttons["brain-settings"].tap()
        app.buttons["brain-canvas"].tap()
        let resolution = app.descendants(matching: .any)["painting-resolution"].firstMatch
        XCTAssertTrue(resolution.waitForExistence(timeout: 10))
        XCTAssertEqual(resolution.label, "768 by 768 pixels")
        app.navigationBars["Canvas"].buttons.firstMatch.tap()
        app.buttons["Done"].tap()
        app.buttons["workspace-settings"].tap()
        XCTAssertTrue(app.buttons["piece-legacy"].waitForExistence(timeout: 10), "Starting from the type preserves the prior project")
        app.buttons["Done"].tap()
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "MIME creates PNG with shared Whistlegraph editor"
        screenshot.lifetime = .keepAlways; add(screenshot)
        app.buttons["workspace-settings"].tap()
        let explore = app.buttons["pieces-explore"]
        XCTAssertTrue(explore.waitForExistence(timeout: 10)); explore.tap()
        XCTAssertTrue(type.waitForExistence(timeout: 10))
        XCTAssertEqual(type.label, "image/png", "Explore returns to the existing feed position")
        XCTAssertFalse(app.buttons["mime-creation-back"].isHittable)
        app.swipeUp()
        let ac = app.buttons.matching(identifier: "mime-type").matching(NSPredicate(format: "label == %@", "text/javascript")).firstMatch
        XCTAssertTrue(ac.waitForExistence(timeout: 10)); ac.tap()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        waitForEditor(in: app)
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
        let handle = app.buttons["mime-creation-back"]
        let start = handle.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.5))
        start.press(forDuration: 0.1, thenDragTo: start.withOffset(CGVector(dx: 0, dy: 180)))
        let dismissed = expectation(for: NSPredicate(format: "isHittable == false"), evaluatedWith: handle)
        XCTAssertEqual(XCTWaiter().wait(for: [dismissed], timeout: 10), .completed)
        XCTAssertTrue(type.waitForExistence(timeout: 10))
    }

    private func openType(_ type: XCUIElement, in app: XCUIApplication) {
        let spring = XCUIApplication(bundleIdentifier: "com.apple.springboard")
        let banner = spring.otherElements.matching(identifier: "NotificationShortLookView").firstMatch
        if banner.exists { banner.swipeUp() }
        waitForReveal(in: app)
        let visible = expectation(for: NSPredicate(format: "isHittable == true"), evaluatedWith: type)
        XCTAssertEqual(XCTWaiter().wait(for: [visible], timeout: 90), .completed)
        type.tap()
        if !app.buttons["mime-creation-back"].waitForExistence(timeout: 3), type.isHittable {
            if banner.exists { banner.swipeUp() }
            type.tap()
        }
    }

    private func waitForEditor(in app: XCUIApplication) {
        let ready = expectation(for: NSPredicate { _, _ in
            !app.descendants(matching: .any)["mime-creation-loading"].firstMatch.exists
        }, evaluatedWith: app)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 60), .completed)
    }

    private func waitForReveal(in app: XCUIApplication) {
        let revealed = expectation(for: NSPredicate { _, _ in
            !app.descendants(matching: .any)["mime-loading"].firstMatch.exists
        }, evaluatedWith: app)
        XCTAssertEqual(XCTWaiter().wait(for: [revealed], timeout: 90), .completed)
    }

    func testDoomFeedPlaysAndItsTypeCreatesWad() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "doom"
        app.launch()
        let type = app.buttons["mime-type"].firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 90))
        XCTAssertEqual(type.label, "application/x-doom-wad")
        XCTAssertEqual(app.buttons["mime-global-identifier"].firstMatch.label, "doom~fixture-wad")
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 90), "The WAD feed reaches its first game frame")
        waitForReveal(in: app)
        XCTAssertFalse(app.webViews.buttons["Play"].exists)
        XCTAssertFalse(app.webViews.buttons["Fire"].exists)
        let reel = XCTAttachment(screenshot: app.screenshot())
        reel.name = "MIME native playable WAD reel"; reel.lifetime = .keepAlways; add(reel)
        openType(type, in: app)
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 90))
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 90))
        let editor = XCTAttachment(screenshot: app.screenshot())
        editor.name = "MIME WAD editor overlay"; editor.lifetime = .keepAlways; add(editor)
    }

}
