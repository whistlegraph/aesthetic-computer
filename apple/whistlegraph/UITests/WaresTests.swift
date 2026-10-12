import XCTest

final class WaresTests: XCTestCase {
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
