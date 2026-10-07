import XCTest

final class AccountEntryTests: XCTestCase {
    func testLoginAndSignupEntry() {
        let app = XCUIApplication()
        // Use an ephemeral workspace and omit Keychain restoration. The phone's
        // account and local pieces stay intact; hosted forms use the real service.
        app.launchEnvironment["WHISTLEGRAPH_ACCOUNT_ENTRY_TEST"] = "1"
        app.launch()
        let login = app.buttons["account-entry-login"]
        XCTAssertTrue(login.waitForExistence(timeout: 30), "bundled workspace must pass the navigation guard")
        XCTAssertFalse(app.buttons["ware-picker"].exists, "signed-out users see account entry")
        XCTAssertTrue(app.buttons["account-entry-signup"].isHittable)
        capture(app, "Whistlegraph account entry")
        login.tap()
        XCTAssertTrue(app.navigationBars["Log in to Aesthetic Computer"].waitForExistence(timeout: 10))
        let email = app.webViews.textFields.firstMatch
        XCTAssertTrue(email.waitForExistence(timeout: 30), "real login form loaded")
        capture(app, "Whistlegraph login form")
        app.navigationBars.buttons["Cancel"].tap()
        XCTAssertTrue(login.waitForExistence(timeout: 10))
        app.buttons["account-entry-signup"].tap()
        XCTAssertTrue(app.navigationBars["Join Aesthetic Computer"].waitForExistence(timeout: 10))
        XCTAssertTrue(email.waitForExistence(timeout: 30), "real signup form loaded")
        capture(app, "Whistlegraph signup form")
        app.navigationBars.buttons["Cancel"].tap()
        XCTAssertTrue(login.waitForExistence(timeout: 10))
        XCTAssertTrue(app.buttons["account-entry-signup"].isHittable)
    }

    func testInstalledWorkspaceStarts() {
        let app = XCUIApplication()
        app.launch()
        let outcome = expectation(for: NSPredicate { _, _ in
            app.buttons["ware-picker"].exists || app.buttons["account-entry-login"].exists || app.buttons["account-entry-retry"].exists
        }, evaluatedWith: nil)
        XCTAssertEqual(XCTWaiter.wait(for: [outcome], timeout: 45), .completed)
        XCTAssertFalse(app.buttons["Reload"].exists)
        capture(app, "Whistlegraph restored launch")
    }

    private func capture(_ app: XCUIApplication, _ name: String) {
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = name; image.lifetime = .keepAlways; add(image)
    }
}
