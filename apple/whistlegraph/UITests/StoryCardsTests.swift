import XCTest

final class StoryCardsTests: XCTestCase {
    func testExportStoryMP4OnPhone() throws {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "story"
        app.launchEnvironment["WALKIE_RESET_STORY_CACHE"] = "1"
        app.launch()
        let cards = app.buttons["play-versions"]
        XCTAssertTrue(cards.waitForExistence(timeout: 30))
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: cards)
        waitForExpectations(timeout: 60)
        cards.tap()
        let export = app.buttons["story-export"]
        XCTAssertTrue(export.waitForExistence(timeout: 15))
        let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Phone story card"; image.lifetime = .keepAlways; add(image)
        export.tap()
        let progress = app.otherElements["story-export-progress"]
        XCTAssertTrue(progress.waitForExistence(timeout: 10), "Export shows progress immediately")
        XCTAssertTrue(app.buttons["story-export-cancel"].exists)
        let rendering = XCTAttachment(screenshot: app.screenshot()); rendering.name = "MP4 rendering progress"; rendering.lifetime = .keepAlways; add(rendering)
        let copy = app.buttons["story-save-video"]
        XCTAssertTrue(copy.waitForExistence(timeout: 90), app.debugDescription)
        let popover = app.otherElements["story-movie-popover"]
        XCTAssertTrue(popover.exists)
        XCTAssertLessThan(popover.frame.height, app.frame.height / 2, "Save actions stay in a compact popover")
        let shared = XCTAttachment(screenshot: app.screenshot()); shared.name = "MP4 ready to save"; shared.lifetime = .keepAlways; add(shared)
        app.terminate()
    }

    func testCancelExportRestoresStoryControls() {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "story"
        app.launchEnvironment["WALKIE_RESET_STORY_CACHE"] = "1"
        app.launch()
        let cards = app.buttons["play-versions"]
        XCTAssertTrue(cards.waitForExistence(timeout: 30))
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: cards)
        waitForExpectations(timeout: 60)
        cards.tap()
        let export = app.buttons["story-export"]
        XCTAssertTrue(export.waitForExistence(timeout: 15)); export.tap()
        let cancel = app.buttons["story-export-cancel"]
        XCTAssertTrue(cancel.waitForExistence(timeout: 10)); cancel.tap()
        XCTAssertTrue(app.buttons["story-pause"].waitForExistence(timeout: 10))
        XCTAssertTrue(export.isEnabled)
        XCTAssertFalse(app.buttons["story-save-video"].exists)
        app.terminate()
    }

    func testCardsPrepareVideoWithoutExportAndReuseItAfterRelaunch() {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "story"
        app.launchEnvironment["WALKIE_RESET_STORY_CACHE"] = "1"
        app.launch()
        let cards = app.buttons["play-versions"]
        XCTAssertTrue(cards.waitForExistence(timeout: 30))
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: cards)
        waitForExpectations(timeout: 60); cards.tap()
        let status = app.staticTexts["story-video-status"]
        expectation(for: NSPredicate(format: "label == %@", "Video ready"), evaluatedWith: status)
        waitForExpectations(timeout: 90)
        XCTAssertFalse(app.buttons["story-save-video"].exists, "Background preparation never interrupts with a popover")
        app.terminate(); app.launchEnvironment["WALKIE_RESET_STORY_CACHE"] = "0"; app.launch()
        XCTAssertTrue(cards.waitForExistence(timeout: 30))
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: cards)
        waitForExpectations(timeout: 60); cards.tap()
        expectation(for: NSPredicate(format: "label == %@", "Video ready"), evaluatedWith: status)
        waitForExpectations(timeout: 3)
        app.buttons["story-export"].tap()
        XCTAssertTrue(app.buttons["story-save-video"].waitForExistence(timeout: 3))
        XCTAssertFalse(app.otherElements["story-export-progress"].exists)
        let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Cached MP4 popover"; image.lifetime = .keepAlways; add(image)
        app.terminate()
    }

    func testCardsKeepCaptionsBelowPictureAndRestoreSelection() {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "history"
        app.launch()
        let cards = app.buttons["play-versions"]
        XCTAssertTrue(cards.waitForExistence(timeout: 20))
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: cards)
        waitForExpectations(timeout: 60)
        XCTAssertEqual(cards.label, "Open story cards")
        cards.tap()
        let pause = app.buttons["story-pause"]
        XCTAssertTrue(pause.waitForExistence(timeout: 10)); pause.tap()
        XCTAssertEqual(pause.label, "Resume story")
        XCTAssertTrue(app.buttons["story-export"].exists)
        XCTAssertTrue(app.descendants(matching: .any).matching(identifier: "story-version").firstMatch.waitForExistence(timeout: 10))
        let caption = app.staticTexts["spoken-word"]
        XCTAssertTrue(caption.exists)
        let picture = app.webViews.matching(identifier: "story-picture").firstMatch
        if picture.exists { XCTAssertGreaterThanOrEqual(app.descendants(matching: .any).matching(identifier: "story-version").firstMatch.frame.minY, picture.frame.maxY) }
        XCTAssertLessThan(caption.frame.maxY, app.frame.height * 0.8)
        app.buttons["Next card"].tap()
        let version = app.descendants(matching: .any).matching(identifier: "story-version").firstMatch
        expectation(for: NSPredicate(format: "label == %@", "Running version 3"), evaluatedWith: version)
        waitForExpectations(timeout: 10)
        // v2 is a sibling branch and must not appear in this story.
        XCTAssertTrue(app.otherElements.matching(NSPredicate(format: "label == %@", "Card 2 of 2")).firstMatch.exists)
        let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Story card with caption"; image.lifetime = .keepAlways; add(image)
        app.buttons["Previous card"].tap()
        expectation(for: NSPredicate(format: "label == %@", "Running version 1"), evaluatedWith: version)
        waitForExpectations(timeout: 10)
        app.buttons["Close version story"].tap()
        let selected = app.buttons["version-3"]
        XCTAssertTrue(selected.waitForExistence(timeout: 10)); XCTAssertEqual(selected.value as? String, "Current version")
    }
}
