import XCTest

final class WaresTests: XCTestCase {
    // Explicitly opt in: this sends one real request using the phone's account.
    func testGenerateWadThroughLiveAccount() throws {
        guard ProcessInfo.processInfo.environment["WHISTLEGRAPH_LIVE_WAD_AUTOMATION"] == "1" else {
            throw XCTSkip("Live account generation requires explicit opt-in")
        }
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        picker.tap()
        app.buttons["ware-doom"].tap()
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 20))
        let current = NSPredicate(format: "identifier BEGINSWITH %@ AND value == %@", "version-", "Current version")
        let previous = app.buttons.matching(current).firstMatch
        let oldVersion = previous.exists ? previous.identifier : ""
        let type = app.buttons["type-control"]
        XCTAssertTrue(type.waitForExistence(timeout: 20))
        let connected = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: type)
        XCTAssertEqual(XCTWaiter().wait(for: [connected], timeout: 30), .completed)
        type.tap()
        let editor = app.descendants(matching: .any)["typed-request"].firstMatch
        XCTAssertTrue(editor.waitForExistence(timeout: 10))
        editor.tap()
        let prompt = ProcessInfo.processInfo.environment["WHISTLEGRAPH_LIVE_WAD_PROMPT"] ?? "Make a lollipop world with custom candy textures and sprites, connected rooms and stairs."
        XCTAssertLessThanOrEqual(prompt.count, 96)
        editor.typeText(prompt)
        app.buttons["request-send"].tap()
        var failureReason: String?
        let generated = expectation(for: NSPredicate { _, _ in
            if app.buttons["attempt-retry"].exists {
                let row = app.staticTexts.matching(NSPredicate(format: "label BEGINSWITH %@", prompt)).firstMatch
                failureReason = (row.exists ? row.value as? String : nil) ?? "The app reported a failed WAD request"
                return true
            }
            let version = app.buttons.matching(current).firstMatch
            return version.exists && version.identifier != oldVersion
        }, evaluatedWith: nil)
        XCTAssertEqual(XCTWaiter().wait(for: [generated], timeout: 180), .completed,
                       "A real inference turn must save a new WAD version")
        if let failureReason { XCTFail("WAD generation: \(failureReason)") }
        XCTAssertTrue(app.webViews.otherElements["Doom room"].waitForExistence(timeout: 60))
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "WAD generated through Whistlegraph prompt"
        screenshot.lifetime = .keepAlways
        add(screenshot)
        app.buttons["doom-actions"].tap()
        app.buttons["Export WAD"].tap()
        XCTAssertTrue(app.descendants(matching: .any)["Copy"].firstMatch.waitForExistence(timeout: 15), "A full WAD must reach the native share sheet")
        if app.buttons["Close"].exists {
            app.buttons["Close"].tap()
        } else {
            app.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.45))
                .press(forDuration: 0.1, thenDragTo: app.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.95)))
        }
    }

    func testExistingDoomExportShowsShareSheet() {
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        let banner = XCUIApplication(bundleIdentifier: "com.apple.springboard")
            .descendants(matching: .any)["NotificationShortLookView"].firstMatch
        if banner.exists { banner.swipeUp() }
        picker.tap()
        let doom = app.buttons["ware-doom"]
        if !doom.waitForExistence(timeout: 2) {
            if banner.exists { banner.swipeUp() }
            picker.tap()
        }
        XCTAssertTrue(doom.waitForExistence(timeout: 5))
        doom.tap()
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 30))
        let compiled = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: app.buttons["doom-actions"])
        XCTAssertEqual(XCTWaiter().wait(for: [compiled], timeout: 60), .completed)
        app.buttons["doom-actions"].tap()
        app.buttons["Export WAD"].tap()
        let copy = app.descendants(matching: .any)["Copy"].firstMatch
        XCTAssertTrue(copy.waitForExistence(timeout: 15))
        app.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.45))
            .press(forDuration: 0.1, thenDragTo: app.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.95)))
    }

    func testDoomWarePlaysRealWad() {
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        picker.tap()
        app.buttons["ware-doom"].tap()
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 20))
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 60))
        XCTAssertFalse(app.webViews.buttons["Play"].exists)
        XCTAssertFalse(app.webViews.buttons["Fire"].exists)
        XCTAssertFalse(app.webViews.staticTexts["First arena"].exists)
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "Whistlegraph playable Doom WAD"
        screenshot.lifetime = .keepAlways
        add(screenshot)
    }

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

extension WaresTests {
    func testNewWadHasItsOwnHistoryAndKeepsThePreviousProject() {
        let app = XCUIApplication()
        app.launch()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        let ready = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: picker)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 30), .completed)
        let banner = XCUIApplication(bundleIdentifier: "com.apple.springboard")
            .descendants(matching: .any)["NotificationShortLookView"].firstMatch
        if banner.exists { banner.swipeUp() }
        picker.tap()
        let doom = app.buttons["ware-doom"]
        if !doom.waitForExistence(timeout: 2) { if banner.exists { banner.swipeUp() }; picker.tap() }
        XCTAssertTrue(doom.waitForExistence(timeout: 5)); doom.tap()
        let compiled = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: app.buttons["doom-actions"])
        XCTAssertEqual(XCTWaiter().wait(for: [compiled], timeout: 60), .completed)
        let current = NSPredicate(format: "identifier BEGINSWITH %@ AND value == %@", "version-", "Current version")
        let old = app.buttons.matching(current).firstMatch
        let oldVersion = old.exists ? old.identifier : ""
        app.buttons["workspace-settings"].tap()
        XCTAssertTrue(app.buttons["pieces-new"].waitForExistence(timeout: 10))
        app.buttons["pieces-new"].tap()
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 60))
        XCTAssertFalse(app.buttons.matching(current).firstMatch.exists, "A fresh v0 project has no made versions")
        app.buttons["workspace-settings"].tap()
        let previous = app.buttons["piece-legacy"]
        XCTAssertTrue(previous.waitForExistence(timeout: 10), "The original WAD remains in the project picker")
        previous.tap()
        if !oldVersion.isEmpty { XCTAssertTrue(app.buttons[oldVersion].waitForExistence(timeout: 60)) }
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "Original WAD restored after New empty WAD"
        screenshot.lifetime = .keepAlways
        add(screenshot)
    }
}
