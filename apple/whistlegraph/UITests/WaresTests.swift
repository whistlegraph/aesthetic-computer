import XCTest

final class WaresTests: XCTestCase {
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
