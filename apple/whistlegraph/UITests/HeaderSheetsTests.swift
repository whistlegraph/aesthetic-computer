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
        app.launchEnvironment["WHISTLEGRAPH_NATIVE_SCREEN_FIXTURE"] = "history"
        app.launch()
        return app
    }

    func testPieceCodeOpensPiecesSheetWithNewPiece() {
        let app = launch()
        let code = app.buttons["workspace-settings"]
        XCTAssertTrue(code.waitForExistence(timeout: 20), "the /code title should be tappable")
        code.tap()
        XCTAssertTrue(app.buttons["pieces-new"].waitForExistence(timeout: 10), "the pieces sheet offers New piece")
        XCTAssertTrue(app.buttons["piece-wwDemo"].exists, "the open piece is listed by its code")
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
}
