import XCTest

// The top-left identity: the handle opens the account sheet, the /code opens
// the pieces sheet with a way to start a new piece. Runs on the simulator
// against the "history" fixture, which signs nothing in and reaches no cloud.
final class HeaderSheetsTests: XCTestCase {
    private func launch() -> XCUIApplication {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "history"
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
        XCTAssertFalse(app.buttons["pieces-new"].waitForExistence(timeout: 2))
    }

    func testHandleOpensAccountSheetWithSignOut() {
        let app = launch()
        let handle = app.buttons["workspace-account"]
        XCTAssertTrue(handle.waitForExistence(timeout: 20), "the handle should be tappable")
        handle.tap()
        XCTAssertTrue(app.buttons["account-sign-out"].waitForExistence(timeout: 10), "a signed-in fixture handle shows Sign out")
        XCTAssertTrue(app.segmentedControls.firstMatch.exists, "appearance moved into the account sheet")
        app.buttons["Done"].tap()
        XCTAssertFalse(app.buttons["account-sign-out"].waitForExistence(timeout: 2))
    }
}
