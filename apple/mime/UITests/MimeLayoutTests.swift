import XCTest

final class MimeLayoutTests: XCTestCase {
    func testVersionRemixesPiecePaintingAndWadAsNewV0() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "remix"
        app.launch()
        let badge = app.buttons["mime-remix-version"].firstMatch
        XCTAssertTrue(badge.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        for (version, author, ware) in [(7, "original", "Aesthetic.Computer Piece"), (3, "painter", "Painting"), (2, "builder", "Doom")] {
            XCTAssertEqual(badge.label, "v\(version)")
            let identifier = app.buttons["mime-global-identifier"].firstMatch
            XCTAssertGreaterThan(badge.frame.minX, identifier.frame.maxX)
            let feed = XCTAttachment(screenshot: app.screenshot())
            feed.name = "MIME remix feed v\(version)"; feed.lifetime = .keepAlways; add(feed)
            badge.tap()
            XCTAssertTrue(app.buttons["ware-picker"].waitForExistence(timeout: 60))
            waitForEditor(in: app)
            XCTAssertTrue(app.buttons["ware-picker"].label.contains(ware), app.buttons["ware-picker"].label)
            let v0 = app.buttons["version-0"]
            XCTAssertTrue(v0.waitForExistence(timeout: 20))
            XCTAssertTrue(v0.label.contains("Stole from @\(author)"), v0.label)
            XCTAssertTrue(v0.label.contains("v\(version)"), v0.label)
            XCTAssertFalse(app.buttons["version-1"].exists)
            if version == 3 {
                app.buttons["brain-settings"].tap(); app.buttons["brain-canvas"].tap()
                let resolution = app.descendants(matching: .any)["painting-resolution"].firstMatch
                XCTAssertTrue(resolution.waitForExistence(timeout: 10))
                XCTAssertEqual(resolution.label, "32 by 48 pixels")
                app.navigationBars["Canvas"].buttons.firstMatch.tap(); app.buttons["Done"].tap()
            }
            let editor = XCTAttachment(screenshot: app.screenshot())
            editor.name = "MIME remixed \(ware) v0"; editor.lifetime = .keepAlways; add(editor)
            app.buttons["mime-creation-back"].tap()
            XCTAssertTrue(badge.waitForExistence(timeout: 10))
            app.buttons["mime-resume-editor"].tap(); waitForEditor(in: app)
            XCTAssertTrue(v0.waitForExistence(timeout: 10), "Resuming keeps the remix")
            app.buttons["mime-creation-back"].tap()
            if version != 2 { app.swipeUp() }
        }
    }
    func testNativeIdentityCorners() {
        let app = XCUIApplication()
        app.launch()
        let code = app.buttons["mime-global-identifier"].firstMatch
        XCTAssertTrue(code.waitForExistence(timeout: 45), "first piece opens without a swipe")
        waitForReveal(in: app)
        XCTAssertFalse(app.staticTexts["Swipe up for the feed"].exists)
        let author = app.descendants(matching: .any).matching(identifier: "mime-author").firstMatch
        let type = app.descendants(matching: .any).matching(identifier: "mime-type").firstMatch
        XCTAssertTrue(author.exists)
        XCTAssertTrue(type.exists)
        XCTAssertFalse(code.label.contains("@"))
        XCTAssertTrue(type.label.contains("/"))
        XCTAssertLessThan(code.frame.maxY, author.frame.minY)
        XCTAssertLessThan(author.frame.maxX, type.frame.minX)
        XCTAssertEqual(author.frame.maxY, type.frame.maxY, accuracy: 3)
        XCTAssertTrue(app.descendants(matching: .any)["mime-comic-logo"].firstMatch.exists)
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = "MIME native identity layout"; image.lifetime = .keepAlways; add(image)
    }
    func testTypeStartsFreshPaintingAndPieceInSharedEditor() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "signed-in"
        app.launch()
        let type = app.buttons["mime-type"].firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 30)); XCTAssertEqual(type.label, "image/png")
        openType(type, in: app)
        let opened = app.buttons["painting-actions"].waitForExistence(timeout: 40)
        if !opened { print(app.debugDescription) }
        XCTAssertTrue(opened)
        waitForEditor(in: app)
        XCTAssertFalse(app.staticTexts["Checking your account…"].exists)
        XCTAssertFalse(app.descendants(matching: .any)["account-entry"].firstMatch.exists)
        let editor = app.descendants(matching: .any)["mime-creation"].firstMatch.frame
        XCTAssertEqual(editor.minX, 0, accuracy: 1)
        XCTAssertEqual(editor.width, app.frame.width, accuracy: 1)
        XCTAssertGreaterThan(editor.height, app.frame.height * 0.85)
        let chalk = app.buttons["draw-control"], typeButton = app.buttons["type-control"], talk = app.descendants(matching: .any)["talk-control"].firstMatch
        XCTAssertGreaterThan(chalk.frame.midX, typeButton.frame.midX)
        XCTAssertLessThan(chalk.frame.midX, talk.frame.midX)
        XCTAssertEqual(chalk.frame.midY, typeButton.frame.midY, accuracy: 4)
        XCTAssertFalse(app.descendants(matching: .any)["painting-resolution"].firstMatch.exists)
        app.buttons["brain-settings"].tap()
        app.buttons["brain-canvas"].tap()
        let resolution = app.descendants(matching: .any)["painting-resolution"].firstMatch
        XCTAssertTrue(resolution.waitForExistence(timeout: 10))
        XCTAssertEqual(resolution.label, "768 by 768 pixels")
        app.navigationBars["Canvas"].buttons.firstMatch.tap()
        app.buttons["Done"].tap()
        app.buttons["workspace-settings"].tap()
        XCTAssertTrue(app.buttons["piece-legacy"].waitForExistence(timeout: 10), "Starting from the type preserves the prior project")
        app.buttons["Done"].tap()
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "MIME creates PNG with shared Whistlegraph editor"
        screenshot.lifetime = .keepAlways; add(screenshot)
        app.buttons["workspace-settings"].tap()
        let explore = app.buttons["pieces-explore"]
        XCTAssertTrue(explore.waitForExistence(timeout: 10)); explore.tap()
        XCTAssertTrue(type.waitForExistence(timeout: 10))
        XCTAssertEqual(type.label, "image/png", "Explore returns to the existing feed position")
        XCTAssertFalse(app.buttons["mime-creation-back"].isHittable)
        app.swipeUp()
        let ac = app.buttons.matching(identifier: "mime-type").matching(NSPredicate(format: "label == %@", "text/javascript")).firstMatch
        XCTAssertTrue(ac.waitForExistence(timeout: 10)); ac.tap()
        let picker = app.buttons["ware-picker"]
        XCTAssertTrue(picker.waitForExistence(timeout: 30))
        waitForEditor(in: app)
        XCTAssertTrue(picker.label.contains("Aesthetic.Computer Piece"))
        XCTAssertTrue(app.buttons["type-control"].waitForExistence(timeout: 10))
        let piece = XCTAttachment(screenshot: app.screenshot())
        piece.name = "MIME creates AC piece directly"
        piece.lifetime = .keepAlways; add(piece)
    }

    func testEmptyPieceOpensWithoutWaitingForFrame() {
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "empty-piece"
        app.launch()
        XCTAssertTrue(app.buttons["mime-type"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        app.swipeUp()
        let ac = app.buttons.matching(identifier: "mime-type").matching(NSPredicate(format: "label == %@", "text/javascript")).firstMatch
        XCTAssertTrue(ac.waitForExistence(timeout: 10)); ac.tap()
        XCTAssertTrue(app.buttons["ware-picker"].waitForExistence(timeout: 30))
        waitForEditor(in: app)
        XCTAssertTrue(app.buttons["type-control"].isHittable)
    }

    func testTypeCreationRequiresSignIn() {
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "signed-out"
        app.launch()
        let type = app.buttons["mime-type"].firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 30)); openType(type, in: app)
        XCTAssertTrue(app.buttons["account-entry-login"].waitForExistence(timeout: 30))
        XCTAssertFalse(app.buttons["type-control"].isHittable, "Creation controls remain inaccessible until sign-in")
        let handle = app.buttons["mime-creation-back"]
        let header = app.coordinate(withNormalizedOffset: CGVector(dx: 0.8, dy: 0)).withOffset(CGVector(dx: 0, dy: handle.frame.midY))
        header.press(forDuration: 0.1, thenDragTo: header.withOffset(CGVector(dx: -200, dy: 0)))
        let dismissed = expectation(for: NSPredicate(format: "isHittable == false"), evaluatedWith: handle)
        XCTAssertEqual(XCTWaiter().wait(for: [dismissed], timeout: 10), .completed)
        XCTAssertTrue(type.waitForExistence(timeout: 10))
    }

    func testWorkingPieceCanReturnToFeedAndResume() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "busy"
        app.launch()
        XCTAssertTrue(app.buttons["mime-type"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        app.buttons["mime-resume-editor"].tap()
        XCTAssertTrue(app.buttons["ware-picker"].waitForExistence(timeout: 30))
        waitForEditor(in: app)
        XCTAssertFalse(app.buttons["type-control"].isEnabled, "The fixture is still generating")
        app.buttons["mime-creation-back"].tap()
        XCTAssertTrue(app.buttons["mime-resume-editor"].waitForExistence(timeout: 10))
        app.buttons["mime-resume-editor"].tap()
        waitForEditor(in: app)
        XCTAssertFalse(app.buttons["type-control"].isEnabled, "Returning did not stop the turn")
        let editor = XCTAttachment(screenshot: app.screenshot())
        editor.name = "MIME full screen editor while generating"; editor.lifetime = .keepAlways; add(editor)
        let edge = app.coordinate(withNormalizedOffset: CGVector(dx: 0.995, dy: 0.5))
        edge.press(forDuration: 0.1, thenDragTo: edge.withOffset(CGVector(dx: -260, dy: 0)))
        let returned = expectation(for: NSPredicate(format: "isHittable == true"), evaluatedWith: app.buttons["mime-resume-editor"])
        XCTAssertEqual(XCTWaiter().wait(for: [returned], timeout: 10), .completed)
        // Tapping a type during work resumes that work; it must not queue a
        // project switch behind a loading cover or overwrite the active piece.
        app.buttons["mime-type"].firstMatch.tap()
        waitForEditor(in: app)
        XCTAssertTrue(app.buttons["ware-picker"].label.contains("Aesthetic.Computer Piece"))
        XCTAssertFalse(app.buttons["type-control"].isEnabled)
        app.buttons["mime-creation-back"].tap()
    }

    func testPreviewAudioStopsInFeedAndResumesInEditor() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "audio"
        app.launch()
        XCTAssertTrue(app.buttons["mime-type"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        let state = app.staticTexts["mime-preview-audio-state"]
        func waitForAudio(_ label: String) {
            let expected = expectation(for: NSPredicate(format: "label == %@", label), evaluatedWith: state)
            XCTAssertEqual(XCTWaiter().wait(for: [expected], timeout: 45), .completed)
        }
        waitForAudio("Preview suspended") // Warm editor behind the feed is silent.
        app.buttons["mime-resume-editor"].tap()
        waitForEditor(in: app)
        waitForAudio("Preview audible")
        app.buttons["mime-creation-back"].tap()
        waitForAudio("Preview suspended")
        XCUIDevice.shared.press(.home)
        app.activate()
        waitForAudio("Preview suspended") // Foregrounding the feed must not resume it.
        app.buttons["mime-resume-editor"].tap()
        waitForEditor(in: app)
        waitForAudio("Preview audible")
        let edge = app.coordinate(withNormalizedOffset: CGVector(dx: 0.995, dy: 0.5))
        edge.press(forDuration: 0.1, thenDragTo: edge.withOffset(CGVector(dx: -260, dy: 0)))
        waitForAudio("Preview suspended")
    }

    private func openType(_ type: XCUIElement, in app: XCUIApplication) {
        let spring = XCUIApplication(bundleIdentifier: "com.apple.springboard")
        let banner = spring.otherElements.matching(identifier: "NotificationShortLookView").firstMatch
        if banner.exists { banner.swipeUp() }
        waitForReveal(in: app)
        let visible = expectation(for: NSPredicate(format: "isHittable == true"), evaluatedWith: type)
        XCTAssertEqual(XCTWaiter().wait(for: [visible], timeout: 90), .completed)
        type.tap()
        if !app.buttons["mime-creation-back"].waitForExistence(timeout: 3), type.isHittable {
            if banner.exists { banner.swipeUp() }
            type.tap()
        }
    }

    private func waitForEditor(in app: XCUIApplication) {
        let ready = expectation(for: NSPredicate { _, _ in
            !app.descendants(matching: .any)["mime-creation-loading"].firstMatch.exists
        }, evaluatedWith: app)
        XCTAssertEqual(XCTWaiter().wait(for: [ready], timeout: 60), .completed)
    }

    private func waitForReveal(in app: XCUIApplication) {
        let revealed = expectation(for: NSPredicate { _, _ in
            !app.descendants(matching: .any)["mime-loading"].firstMatch.exists
        }, evaluatedWith: app)
        XCTAssertEqual(XCTWaiter().wait(for: [revealed], timeout: 90), .completed)
    }

    func testDoomFeedPlaysAndItsTypeCreatesWad() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "doom"
        app.launch()
        let type = app.buttons["mime-type"].firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 90))
        XCTAssertEqual(type.label, "application/x-doom-wad")
        XCTAssertEqual(app.buttons["mime-global-identifier"].firstMatch.label, "doom~fixture-wad")
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 90), "The WAD feed reaches its first game frame")
        waitForReveal(in: app)
        XCTAssertFalse(app.webViews.buttons["Play"].exists)
        XCTAssertFalse(app.webViews.buttons["Fire"].exists)
        let reel = XCTAttachment(screenshot: app.screenshot())
        reel.name = "MIME native playable WAD reel"; reel.lifetime = .keepAlways; add(reel)
        openType(type, in: app)
        XCTAssertTrue(app.buttons["doom-actions"].waitForExistence(timeout: 90))
        XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 90))
        let editor = XCTAttachment(screenshot: app.screenshot())
        editor.name = "MIME WAD editor overlay"; editor.lifetime = .keepAlways; add(editor)
    }

}
