import XCTest

final class WaresTests: XCTestCase {
    // Explicitly opt in: this sends one real request using the phone's account.
    func testGenerateWadThroughLiveAccount() throws {
        guard ProcessInfo.processInfo.environment["WHISTLEGRAPH_LIVE_WAD_AUTOMATION"] == "1" else {
            throw XCTSkip("Live account generation requires explicit opt-in")
        }
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        picker.tap()
        app.buttons["ware-doom"].tap()
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 20))
        let current = NSPredicate(format: "identifier BEGINSWITH %@ AND value == %@", "version-", "Current version")
        let previous = app.buttons.matching(current).firstMatch
        let oldVersion = previous.exists ? previous.identifier : ""
        let type = app.buttons["type-control"]
        XCTAssertTrue(type.waitForExistence(timeout: 20))
        let connected = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: type)
        XCTAssertEqual(XCTWaiter().wait(for: [connected], timeout: 30), .completed)
        type.tap()
        let editor = app.descendants(matching: .any)["typed-request"].firstMatch
        XCTAssertTrue(editor.waitForExistence(timeout: 10))
        editor.tap()
        editor.typeText("Make a lollipop world with custom candy textures and sprites, connected rooms and stairs.")
        app.buttons["request-send"].tap()
        let generated = expectation(for: NSPredicate { _, _ in
            let version = app.buttons.matching(current).firstMatch
            return version.exists && version.identifier != oldVersion
        }, evaluatedWith: nil)
        XCTAssertEqual(XCTWaiter().wait(for: [generated], timeout: 180), .completed,
                       "A real inference turn must save a new WAD version")
        XCTAssertTrue(app.webViews.otherElements["Doom room"].waitForExistence(timeout: 60))
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "WAD generated through Whistlegraph prompt"
        screenshot.lifetime = .keepAlways
        add(screenshot)
        app.buttons["doom-actions"].tap()
        app.buttons["Export WAD"].tap()
        XCTAssertTrue(app.buttons["Close"].waitForExistence(timeout: 15), "A full WAD must reach the native share sheet")
        app.buttons["Close"].tap()
    }

    func testDoomWarePlaysRealWad() {
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        picker.tap()
        app.buttons["ware-doom"].tap()
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 20))
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 60))
        XCTAssertFalse(app.webViews.buttons["Play"].exists)
        XCTAssertFalse(app.webViews.buttons["Fire"].exists)
        XCTAssertFalse(app.webViews.staticTexts["First arena"].exists)
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "Whistlegraph playable Doom WAD"
        screenshot.lifetime = .keepAlways
        add(screenshot)
    }

    func testRoomPickerPreservesPieceWorkspace() {
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        picker.tap()
        app.buttons["ware-roblox"].tap()
        XCTAssertTrue(app.buttons["play-roblox"].waitForExistence(timeout: 20))
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "Whistlegraph Roblox Room"
        screenshot.lifetime = .keepAlways
        add(screenshot)
        picker.tap()
        app.buttons["ware-piece"].tap()
        XCTAssertTrue(app.buttons["workspace-settings"].waitForExistence(timeout: 20))
        XCTAssertFalse(app.buttons["play-roblox"].exists)
    }
}
