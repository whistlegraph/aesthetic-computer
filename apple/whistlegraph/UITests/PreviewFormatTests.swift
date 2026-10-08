import XCTest

final class PreviewFormatTests: XCTestCase {
    // Changes only presentation preferences on the paired phone. No inference,
    // piece edits, wallet calls or purchases.
    func testPhonePreviewFormats() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launch()
        let brain = app.buttons["brain-settings"]
        XCTAssertTrue(brain.waitForExistence(timeout: 30))
        for format in ["9:16", "1:1", "4:3", "16:9", "2:3"] {
            brain.tap()
            XCTAssertTrue(app.buttons["brain-canvas"].waitForExistence(timeout: 10)); app.buttons["brain-canvas"].tap()
            let choices = app.segmentedControls["brain-preview-format"]
            XCTAssertTrue(choices.waitForExistence(timeout: 10))
            XCTAssertTrue(app.segmentedControls["brain-pixel-size"].exists)
            choices.buttons[format].tap()
            XCTAssertTrue(choices.buttons[format].isSelected)
            if format == "2:3" {
                let settings = XCTAttachment(screenshot: app.screenshot()); settings.name = "Brain preview controls"; settings.lifetime = .keepAlways; add(settings)
            }
            app.navigationBars.buttons["Brain"].tap()
            app.buttons["Done"].tap()
            let picture = app.otherElements["story-picture"]
            XCTAssertTrue(picture.waitForExistence(timeout: 10))
            let parts = format.split(separator: ":").compactMap { Double($0) }
            // SwiftUI exposes the canvas bounds here, excluding the wood padding.
            XCTAssertEqual(picture.frame.width / picture.frame.height, parts[0] / parts[1], accuracy: 0.01)
            XCTAssertLessThanOrEqual(picture.frame.height, 394)
            XCTAssertTrue(brain.isHittable)
            XCTAssertTrue(app.buttons["type-control"].isHittable)
            let image = XCTAttachment(screenshot: app.screenshot()); image.name = "Preview " + format; image.lifetime = .keepAlways; add(image)
        }
        app.terminate(); app.launch()
        XCTAssertTrue(brain.waitForExistence(timeout: 30)); brain.tap()
        XCTAssertTrue(app.buttons["brain-canvas"].waitForExistence(timeout: 10)); app.buttons["brain-canvas"].tap()
        XCTAssertTrue(app.segmentedControls["brain-preview-format"].buttons["2:3"].isSelected)
        app.navigationBars.buttons["Brain"].tap()
        app.buttons["Done"].tap()
    }
}
