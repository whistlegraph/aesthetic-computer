import XCTest

// Uses a separate fixture identity, source and consent record. No real account,
// recording or model request is used. The production native consent flow runs.
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
        XCTAssertFalse(app.buttons["ai-consent-allow"].exists, "No prompt at launch or sign-in")
        return app
    }
    private func type(_ words: String, in app: XCUIApplication) {
        XCTAssertEqual(app.state, .runningForeground, "The phone must be left in the test app")
        app.buttons["type-control"].tap()
        let field = app.textFields["typed-request"]
        XCTAssertTrue(field.waitForExistence(timeout: 10))
        field.tap(); field.typeText(words)
        XCTAssertFalse(app.buttons["ai-consent-allow"].exists, "Typing stays local")
        app.buttons["request-send"].tap()
    }
    private func count(_ number: Int, in app: XCUIApplication) {
        let reached = expectation(for: NSPredicate(format: "label == %@", "Requests: \(number)"),
            evaluatedWith: app.staticTexts["consent-fixture-requests"])
        XCTAssertEqual(XCTWaiter.wait(for: [reached], timeout: 10), .completed)
    }
    func testFirstSendDeclineAllowAndRelaunch() {
        let app = launch()
        type("Make a dancing tree", in: app)
        let allow = app.buttons["ai-consent-allow"]
        XCTAssertTrue(allow.waitForExistence(timeout: 10))
        XCTAssertTrue(allow.isHittable, "Primary action fits without scrolling")
        XCTAssertTrue(app.buttons["ai-consent-not-now"].isHittable)
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = "First AI use"; image.lifetime = .keepAlways; add(image)
        app.buttons["ai-consent-not-now"].tap()
        let field = app.textFields["typed-request"]
        XCTAssertTrue(field.waitForExistence(timeout: 10))
        XCTAssertEqual(field.value as? String, "Make a dancing tree")
        count(0, in: app)
        app.buttons["request-send"].tap()
        XCTAssertTrue(allow.waitForExistence(timeout: 10)); allow.tap()
        count(1, in: app)
        XCTAssertFalse(field.exists, "Allow continues the original typed request")
        type("Make it purple", in: app)
        count(2, in: app)
        XCTAssertFalse(allow.exists, "No repeated prompt")
        app.terminate()
        app.launchEnvironment["WALKIE_RESET_AI_CONSENT"] = "0"
        app.launch()
        XCTAssertTrue(app.buttons["type-control"].waitForExistence(timeout: 40))
        type("Make it spin", in: app)
        count(1, in: app)
        XCTAssertFalse(allow.exists, "Permission survives relaunch")
    }
    func testFirstTalkDoesNotStartMicrophoneAfterAllow() {
        let app = launch()
        app.buttons["talk-control"].press(forDuration: 0.2)
        let allow = app.buttons["ai-consent-allow"]
        XCTAssertTrue(allow.waitForExistence(timeout: 10)); allow.tap()
        XCTAssertTrue(app.buttons["type-control"].waitForExistence(timeout: 10))
        XCTAssertFalse(app.staticTexts["Listening…"].exists)
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
        XCTAssertFalse(app.buttons["ai-consent-allow"].exists)
        count(0, in: app)
    }
    func testDeviceLogShowsActionsAndExportsWithoutPromptText() {
        let app = launch()
        type("Secret test drawing", in: app)
        XCTAssertTrue(app.buttons["ai-consent-not-now"].waitForExistence(timeout: 10))
        app.buttons["ai-consent-not-now"].tap()
        app.buttons["request-cancel"].tap()
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
