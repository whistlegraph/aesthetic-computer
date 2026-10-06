import XCTest

final class SwipeToTypeTests: XCTestCase {
    func testScrollingVersionsSelectsSilently() {
        let app = XCUIApplication()
        app.launchEnvironment["WHISTLEGRAPH_NATIVE_SCREEN_FIXTURE"] = "history"
        app.launch()
        let current = app.buttons["version-3"]
        XCTAssertTrue(current.waitForExistence(timeout: 20))
        let history = app.scrollViews["version-rolodex"]
        XCTAssertTrue(history.waitForExistence(timeout: 5))
        history.swipeUp(velocity: .slow)
        let changed = NSPredicate(format: "value != %@", "Current version")
        expectation(for: changed, evaluatedWith: current)
        waitForExpectations(timeout: 10)
        XCTAssertFalse(app.staticTexts["spoken-word"].exists)
        XCTAssertTrue(app.buttons["workspace-settings"].exists)
        let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Rolodex selection"; image.lifetime = .keepAlways; add(image)
        app.terminate()
    }

    func testVersionStoryRestoresSelectedVersion() {
        let app = XCUIApplication()
        app.launchEnvironment["WHISTLEGRAPH_NATIVE_SCREEN_FIXTURE"] = "history"
        app.launch()
        let version = app.buttons["version-3"]
        XCTAssertTrue(version.waitForExistence(timeout: 20))
        let play = app.buttons["play-versions"]
        XCTAssertTrue(play.waitForExistence(timeout: 10))
        play.tap()
        let close = app.buttons["Close version story"]
        XCTAssertTrue(close.waitForExistence(timeout: 10))
        XCTAssertFalse(app.buttons["type-control"].exists)
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = "Fullscreen version story"; image.lifetime = .keepAlways; add(image)
        close.tap()
        XCTAssertTrue(version.waitForExistence(timeout: 10))
        XCTAssertEqual(version.value as? String, "Current version")
        XCTAssertTrue(app.buttons["type-control"].exists)
        app.terminate()
    }

    func testInlineTypingInBothAppearances() {
        let app = XCUIApplication()
        app.launchEnvironment["WHISTLEGRAPH_NATIVE_SCREEN_FIXTURE"] = "history"
        for appearance in ["light", "dark"] {
            app.launchArguments = ["-whistlegraph-appearance", appearance]
            app.launch()
            let type = app.buttons["type-control"], talk = app.buttons["talk-control"]
            XCTAssertTrue(type.waitForExistence(timeout: 20))
            let version = app.buttons["version-3"]
            XCTAssertTrue(version.waitForExistence(timeout: 20))
            XCTAssertLessThan(type.frame.midX, talk.frame.midX)
            let ready = XCTAttachment(screenshot: app.screenshot()); ready.name = "Ready \(appearance)"; ready.lifetime = .keepAlways; add(ready)
            type.tap()
            let editor = app.descendants(matching: .any)["typed-request"].firstMatch
            XCTAssertTrue(editor.waitForExistence(timeout: 5))
            XCTAssertTrue(app.keyboards.firstMatch.waitForExistence(timeout: 5))
            let preview = app.webViews.firstMatch
            XCTAssertTrue(preview.exists)
            XCTAssertLessThanOrEqual(preview.frame.maxY, app.keyboards.firstMatch.frame.minY)
            editor.tap()
            editor.typeText(String(repeating: "a", count: 110))
            XCTAssertEqual((editor.value as? String)?.count, 96)
            XCTAssertEqual(app.staticTexts["request-count"].label, "96 / 96")
            let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Inline typing \(appearance)"; image.lifetime = .keepAlways; add(image)
            app.buttons["Cancel"].tap()
            XCTAssertEqual(version.value as? String, "Current version")
            app.terminate()
        }
    }
}

extension SwipeToTypeTests {
    func testSwipeTalkLatchesDrawingAndAudio() {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "gestures"
        app.launch()
        let talk = app.buttons["talk-control"]
        XCTAssertTrue(talk.waitForExistence(timeout: 25))
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: talk)
        waitForExpectations(timeout: 30)
        let original = talk.frame.width
        let start = talk.coordinate(withNormalizedOffset: CGVector(dx: 0.7, dy: 0.5))
        start.press(forDuration: 0.5, thenDragTo: start.withOffset(CGVector(dx: -100, dy: 0)))
        expectation(for: NSPredicate(format: "label == %@", "Send performance"), evaluatedWith: talk)
        waitForExpectations(timeout: 10)
        XCTAssertFalse(app.buttons["type-control"].isHittable)
        XCTAssertGreaterThan(talk.frame.width, original * 1.5)
        let pad = app.otherElements.matching(NSPredicate(format: "label == %@", "Chalk over the piece")).firstMatch
        XCTAssertTrue(pad.waitForExistence(timeout: 5))
        pad.coordinate(withNormalizedOffset: CGVector(dx: 0.2, dy: 0.3)).press(forDuration: 0.1, thenDragTo: pad.coordinate(withNormalizedOffset: CGVector(dx: 0.8, dy: 0.6)))
        Thread.sleep(forTimeInterval: 9)
        XCTAssertEqual(talk.label, "Send performance", "Lifting from a stroke and passing the ordinary hold limit must not submit")
        let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Chalk and audio performance"; image.lifetime = .keepAlways; add(image)
        app.buttons["Cancel recording, keep drawing"].tap()
        expectation(for: NSPredicate(format: "hittable == true"), evaluatedWith: app.buttons["type-control"])
        waitForExpectations(timeout: 5)
        XCTAssertTrue(pad.exists, "Cancelling keeps the local drawing")
        app.terminate()
    }
}
