import XCTest

// The top-left identity: the handle opens the account sheet, the /code opens
// the pieces sheet with a way to start a new piece. Runs on the simulator
// against the "history" fixture, which signs nothing in and reaches no cloud.
final class HeaderSheetsTests: XCTestCase {
    // Uses the paired phone's current workspace; only changes display units.
    func testPhoneCostUnitToggle() {
        let app = XCUIApplication()
        app.launch()
        let brain = app.buttons["brain-settings"]
        XCTAssertTrue(brain.waitForExistence(timeout: 30))
        brain.tap()
        app.swipeUp()
        let units = app.segmentedControls["brain-cost-unit"]
        XCTAssertTrue(units.waitForExistence(timeout: 15))
        for unit in ["Braincells", "USD", "Tezos"] {
            units.buttons[unit].tap()
            XCTAssertTrue(units.buttons[unit].isSelected)
            let image = XCTAttachment(screenshot: app.screenshot())
            image.name = "Brain cost in " + unit; image.lifetime = .keepAlways; add(image)
        }
        XCTAssertTrue(app.buttons["brain-buy-tezos"].exists)
        app.buttons["Done"].tap()
        app.terminate(); app.launch()
        XCTAssertTrue(brain.waitForExistence(timeout: 30))
        brain.tap(); app.swipeUp()
        XCTAssertTrue(units.waitForExistence(timeout: 15))
        XCTAssertTrue(units.buttons["Tezos"].isSelected, "unit survives relaunch")
        app.buttons["Done"].tap()
    }

    // Opens Temple; never approves a wallet connection, signature or payment.
    func testPhoneLiveTezosCheckout() {
        let app = XCUIApplication()
        app.launch()
        let brain = app.buttons["brain-settings"]
        XCTAssertTrue(brain.waitForExistence(timeout: 30))
        brain.tap(); app.swipeUp()
        let units = app.segmentedControls["brain-cost-unit"]
        XCTAssertTrue(units.waitForExistence(timeout: 15))
        units.buttons["Tezos"].tap()
        let rate = app.staticTexts.matching(NSPredicate(format: "label BEGINSWITH %@", "1 tez = ")).firstMatch
        XCTAssertTrue(rate.waitForExistence(timeout: 20), "live timestamped Tezos rate")
        let balance = XCTAttachment(screenshot: app.screenshot())
        balance.name = "Live Tezos balance"; balance.lifetime = .keepAlways; add(balance)
        app.buttons["brain-buy-tezos"].tap()
        let browsers = ["com.apple.mobilesafari", "com.google.chrome.ios"].map { XCUIApplication(bundleIdentifier: $0) }
        let opened = expectation(for: NSPredicate { _, _ in browsers.contains { $0.state == .runningForeground } }, evaluatedWith: nil)
        guard XCTWaiter.wait(for: [opened], timeout: 20) == .completed,
              let browser = browsers.first(where: { $0.state == .runningForeground }) else {
            XCTFail("default browser did not open"); return
        }
        let connect = browser.buttons["Connect wallet"]
        guard connect.waitForExistence(timeout: 30) else {
            let failure = XCTAttachment(screenshot: app.screenshot()); failure.name = "Checkout did not open"; failure.lifetime = .keepAlways; add(failure)
            XCTFail("authenticated checkout did not open in the default browser"); return
        }
        connect.tap()
        let temple = browser.staticTexts["Temple"]
        XCTAssertTrue(temple.waitForExistence(timeout: 30), "Temple is offered on iOS")
        let wallet = XCTAttachment(screenshot: browser.screenshot())
        wallet.name = "Temple wallet picker on iPhone"; wallet.lifetime = .keepAlways; add(wallet)
        temple.tap()
        let open = browser.alerts.buttons["Open"]
        if open.waitForExistence(timeout: 3) { open.tap() }
        let walletApp = XCUIApplication(bundleIdentifier: "com.madfish.temple-wallet")
        XCTAssertTrue(walletApp.wait(for: .runningForeground, timeout: 25), "checkout hands off to installed Temple")
    }

    func testPieceAudioStartsWithoutTouch() {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "audio"
        app.launch()
        let result = app.staticTexts["audio-autoplay-result"]
        let playing = expectation(for: NSPredicate(format: "label == %@", "Piece audio without a tap"), evaluatedWith: result)
        XCTAssertEqual(XCTWaiter.wait(for: [playing], timeout: 65), .completed, "The real AC synth must produce a waveform before any tap")
    }
    func testTVSheetAndStoryDuringGeneration() {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "working"
        app.launch()
        let tv = app.buttons["project-tv"]
        XCTAssertTrue(tv.waitForExistence(timeout: 25))
        tv.tap()
        XCTAssertTrue(app.navigationBars["TV"].waitForExistence(timeout: 10))
        app.buttons["Done"].tap()
        let cards = app.buttons["play-versions"]
        expectation(for: NSPredicate(format: "enabled == true"), evaluatedWith: cards)
        waitForExpectations(timeout: 40)
        let workspace = XCTAttachment(screenshot: app.screenshot()); workspace.name = "Wood frame and TV while generating"; workspace.lifetime = .keepAlways; add(workspace)
        cards.tap()
        XCTAssertTrue(app.buttons["story-pause"].waitForExistence(timeout: 15))
        let ready = app.staticTexts["story-video-status"]
        expectation(for: NSPredicate(format: "label CONTAINS %@", "Video ready"), evaluatedWith: ready)
        waitForExpectations(timeout: 60)
        app.buttons["Close version story"].tap()
        XCTAssertTrue(app.buttons["play-versions"].waitForExistence(timeout: 10))
        XCTAssertFalse(app.buttons["version-3"].isEnabled, "Generation still locks editing after story playback")
    }

    /// Sheets dismiss with an animation; poll until the element is really gone.
    private func waitForDisappearance(of element: XCUIElement, timeout: TimeInterval = 6) -> Bool {
        let gone = expectation(for: NSPredicate(format: "exists == false"), evaluatedWith: element)
        return XCTWaiter().wait(for: [gone], timeout: timeout) == .completed
    }

    private func launch() -> XCUIApplication {
        let app = XCUIApplication()
        app.launchEnvironment["WALKIE_NATIVE_SCREEN_FIXTURE"] = "history"
        app.launch()
        // The button exists as "Log in" before the engine has loaded the fixture.
        let account = app.buttons.matching(identifier: "workspace-account")
            .matching(NSPredicate(format: "label == %@", "@preview, account")).firstMatch
        XCTAssertTrue(account.waitForExistence(timeout: 25), "the signed-in fixture is ready")
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

    func testThreadCostAppearsLeftOfBrain() {
        let app = launch()
        let cost = app.staticTexts["thread-cost"]
        XCTAssertTrue(cost.waitForExistence(timeout: 20))
        XCTAssertEqual(cost.value as? String, "$0.42")
        let brain = app.buttons["brain-settings"]
        XCTAssertTrue(brain.exists)
        XCTAssertLessThanOrEqual(cost.frame.maxX, brain.frame.minX)
        XCTAssertLessThan(abs(cost.frame.midY - brain.frame.midY), 3)
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = "Thread cost left of brain"; image.lifetime = .keepAlways; add(image)
    }

    func testUnsentDrawingSurvivesRelaunch() {
        let app = launch()
        if app.buttons["drawing-clear"].exists { app.buttons["drawing-clear"].tap() }
        app.buttons["draw-control"].tap()
        let start = app.coordinate(withNormalizedOffset: CGVector(dx: 0.4, dy: 0.35))
        let end = app.coordinate(withNormalizedOffset: CGVector(dx: 0.65, dy: 0.45))
        start.press(forDuration: 0.15, thenDragTo: end)
        XCTAssertTrue(app.buttons["drawing-undo"].waitForExistence(timeout: 5))
        app.terminate(); app.launch()
        XCTAssertTrue(app.buttons["drawing-undo"].waitForExistence(timeout: 25))
        app.buttons["drawing-clear"].tap()
        XCTAssertFalse(app.buttons["drawing-undo"].exists)
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
