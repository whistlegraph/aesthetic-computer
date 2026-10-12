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
        XCTAssertTrue(app.buttons["play-doom"].waitForExistence(timeout: 20))
        let play = app.webViews.buttons["Play"]
        XCTAssertTrue(play.waitForExistence(timeout: 60))
        let loaded = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: play)
        XCTAssertEqual(XCTWaiter().wait(for: [loaded], timeout: 60), .completed)
        play.tap()
        XCTAssertTrue(app.webViews.buttons["Fire"].waitForExistence(timeout: 20))
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
