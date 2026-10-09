import XCTest

// Uses a separate fixture identity, source and consent record. No real account,
// recording or model request is used. Logging in is the AI permission: there is
// no sheet, the signed-out screen says so, and the switch in AI & privacy is the
// only way off.
final class AIConsentTests: XCTestCase {
    override func setUp() { continueAfterFailure = false }
    private func launch(identityFailure: Bool = false) -> XCUIApplication {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "consent"
        app.launchEnvironment["WALKIE_RESET_AI_CONSENT"] = "1"
        app.launchEnvironment["WALKIE_IDENTITY_FAILURE"] = identityFailure ? "1" : "0"
        app.launch()
        let account = app.buttons.matching(identifier: "workspace-account")
            .matching(NSPredicate(format: "label == %@", "@preview, account")).firstMatch
        XCTAssertTrue(account.waitForExistence(timeout: 40))
        XCTAssertFalse(app.buttons["ai-consent-allow"].exists, "No sheet, ever")
        return app
    }
    private func type(_ words: String, in app: XCUIApplication) {
        XCTAssertEqual(app.state, .runningForeground, "The phone must be left in the test app")
        app.buttons["type-control"].tap()
        let field = app.textFields["typed-request"]
        XCTAssertTrue(field.waitForExistence(timeout: 10))
        field.tap(); field.typeText(words)
        app.buttons["request-send"].tap()
    }
    private func count(_ number: Int, in app: XCUIApplication) {
        let reached = expectation(for: NSPredicate(format: "label == %@", "Requests: \(number)"),
            evaluatedWith: app.staticTexts["consent-fixture-requests"])
        XCTAssertEqual(XCTWaiter.wait(for: [reached], timeout: 10), .completed)
    }
    func testLoggedInAccountCreatesWithoutAPromptAndSurvivesRelaunch() {
        let app = launch()
        type("Make a dancing tree", in: app)
        count(1, in: app)
        type("Make it purple", in: app)
        count(2, in: app)
        app.terminate()
        app.launchEnvironment["WALKIE_RESET_AI_CONSENT"] = "0"
        app.launch()
        XCTAssertTrue(app.buttons["type-control"].waitForExistence(timeout: 40))
        type("Make it spin", in: app)
        count(1, in: app)
    }
    func testSwitchingOffBlocksWithAnExplanationAndKeepsTheDraft() {
        let app = launch()
        app.buttons["workspace-account"].tap()
        let privacy = app.buttons["AI & privacy"]
        XCTAssertTrue(privacy.waitForExistence(timeout: 10)); privacy.tap()
        let toggle = app.switches["privacy-ai-creation"]
        XCTAssertTrue(toggle.waitForExistence(timeout: 10))
        XCTAssertEqual(toggle.value as? String, "1", "Logging in switched it on")
        toggle.tap()
        app.buttons["Done"].tap()
        type("Keep this draft", in: app)
        let alert = app.alerts["Could not continue"]
        XCTAssertTrue(alert.waitForExistence(timeout: 10))
        XCTAssertTrue(alert.staticTexts["Create with AI is switched off for this account. Turn it on in Brain → AI & privacy."].exists)
        alert.buttons["OK"].tap()
        XCTAssertEqual(app.textFields["typed-request"].value as? String, "Keep this draft")
        count(0, in: app)
    }
    func testIdentityFailureExplainsBlockAndKeepsDraft() {
        let app = launch(identityFailure: true)
        type("Keep this draft", in: app)
        let alert = app.alerts["Could not continue"]
        XCTAssertTrue(alert.waitForExistence(timeout: 10))
        XCTAssertTrue(alert.staticTexts["Could not verify your account. Check your connection and try again. Your draft is still here."].exists)
        alert.buttons["OK"].tap()
        XCTAssertEqual(app.textFields["typed-request"].value as? String, "Keep this draft")
        count(0, in: app)
    }
    func testDeviceLogShowsActionsAndExportsWithoutPromptText() {
        let app = launch()
        type("Secret test drawing", in: app)
        count(1, in: app)
        app.buttons["workspace-account"].tap()
        let log = app.buttons["account-debug-log"]
        XCTAssertTrue(log.waitForExistence(timeout: 10)); log.tap()
        let contents = app.staticTexts["debug-log-contents"]
        XCTAssertTrue(contents.waitForExistence(timeout: 10))
        XCTAssertTrue(contents.label.contains("typeSend"))
        XCTAssertTrue(contents.label.contains("accountIdentity"))
        XCTAssertTrue(contents.label.contains("touch"), "The passive observer sees taps without blocking them")
        XCTAssertFalse(contents.label.contains("Secret test drawing"))
        app.buttons["debug-log-export"].tap()
        XCTAssertTrue(app.buttons["Share log…"].waitForExistence(timeout: 10))
    }
}
