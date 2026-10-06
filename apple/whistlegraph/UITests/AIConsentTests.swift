import XCTest

// Uses a separate fixture identity, source and consent record. No real account,
// recording or model request is used. The production native consent flow runs.
final class AIConsentTests: XCTestCase {
    override func setUp() { continueAfterFailure = false }
    private func launch() -> XCUIApplication {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "consent"
        app.launchEnvironment["WALKIE_RESET_AI_CONSENT"] = "1"
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
        app.buttons["Send"].tap()
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
        app.buttons["Send"].tap()
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
}
