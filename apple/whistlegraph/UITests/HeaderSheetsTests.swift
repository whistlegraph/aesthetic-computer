import XCTest

// The top-left identity: the handle opens the account sheet, the /code opens
// the pieces sheet with a way to start a new piece. Runs on the simulator
// against the "history" fixture, which signs nothing in and reaches no cloud.
final class HeaderSheetsTests: XCTestCase {
    /// Sheets dismiss with an animation; poll until the element is really gone.
    private func waitForDisappearance(of element: XCUIElement, timeout: TimeInterval = 6) -> Bool {
        let gone = expectation(for: NSPredicate(format: "exists == false"), evaluatedWith: element)
        return XCTWaiter().wait(for: [gone], timeout: timeout) == .completed
    }

    private func launch() -> XCUIApplication {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "history"
        app.launch()
        return app
    }

    // Read-only inspection of the installed phone account and its current piece.
    func testPhoneBrainPanelAndPieceMenu() {
        let app = XCUIApplication()
        app.launch()
        let brain = app.buttons["brain-settings"]
        XCTAssertTrue(brain.waitForExistence(timeout: 30))
        func capture(_ name: String) {
            let image = XCTAttachment(screenshot: app.screenshot())
            image.name = name; image.lifetime = .keepAlways; add(image)
        }
        capture("Build 98 phone workspace")
        brain.tap()
        XCTAssertTrue(app.segmentedControls["brain-pixel-size"].waitForExistence(timeout: 10))
        XCTAssertTrue(app.staticTexts["OpenRouter"].waitForExistence(timeout: 15))
        capture("Build 98 phone Brain settings")
        // Expand the sheet to inspect the allowance and request usage below it.
        app.swipeUp()
        XCTAssertTrue(app.descendants(matching: .any).matching(identifier: "brain-balance").firstMatch.waitForExistence(timeout: 15), app.debugDescription)
        capture("Build 98 phone braincells and usage")
        app.buttons["Done"].tap()
        XCTAssertTrue(waitForDisappearance(of: app.segmentedControls["brain-pixel-size"]))
        app.buttons["workspace-settings"].tap()
        XCTAssertTrue(app.buttons["pieces-new"].waitForExistence(timeout: 10))
        XCTAssertTrue(app.staticTexts["OpenRouter"].exists)
        XCTAssertFalse(app.segmentedControls["brain-pixel-size"].exists)
        capture("Build 98 phone piece menu")
        app.buttons["Done"].tap()
        XCTAssertTrue(waitForDisappearance(of: app.buttons["pieces-new"]))
    }

    func testPieceCodeOpensPiecesSheetWithNewPiece() {
        let app = launch()
        let code = app.buttons["workspace-settings"]
        XCTAssertTrue(code.waitForExistence(timeout: 20), "the /code title should be tappable")
        code.tap()
        XCTAssertTrue(app.buttons["pieces-new"].waitForExistence(timeout: 10), "the pieces sheet offers New piece")
        XCTAssertTrue(app.buttons["piece-wwDemo"].exists, "the open piece is listed by its code")
        XCTAssertFalse(app.segmentedControls["brain-pixel-size"].exists, "pixel size belongs to Brain settings")
        app.buttons["Done"].tap()
        XCTAssertTrue(waitForDisappearance(of: app.buttons["pieces-new"]), "Done closes the pieces sheet")
    }

    func testHandleOpensAccountSheetWithSignOut() {
        let app = launch()
        let handle = app.buttons["workspace-account"]
        XCTAssertTrue(handle.waitForExistence(timeout: 20), "the handle should be tappable")
        handle.tap()
        XCTAssertTrue(app.buttons["account-sign-out"].waitForExistence(timeout: 10), "a signed-in fixture handle shows Sign out")
        XCTAssertTrue(app.segmentedControls.firstMatch.exists, "appearance moved into the account sheet")
        app.buttons["Done"].tap()
        XCTAssertTrue(waitForDisappearance(of: app.buttons["account-sign-out"]), "Done closes the account sheet")
    }

    func testPixelSizePersistsAcrossLaunches() {
        let app = launch()
        let code = app.buttons["brain-settings"]
        XCTAssertTrue(code.waitForExistence(timeout: 20))
        code.tap()
        let picker = app.segmentedControls["brain-pixel-size"]
        XCTAssertTrue(picker.waitForExistence(timeout: 10))
        for size in [1, 2, 3, 4] {
            picker.buttons["\(size)×"].tap()
            XCTAssertTrue(picker.buttons["\(size)×"].isSelected)
        }
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = "Pixel size in Brain settings"; image.lifetime = .keepAlways; add(image)
        app.terminate()
        app.launch()
        XCTAssertTrue(code.waitForExistence(timeout: 20))
        code.tap()
        XCTAssertTrue(picker.waitForExistence(timeout: 10))
        XCTAssertTrue(picker.buttons["4×"].isSelected)
        picker.buttons["2×"].tap()
    }
}
