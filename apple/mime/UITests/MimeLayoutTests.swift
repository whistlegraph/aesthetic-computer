import XCTest

final class MimeLayoutTests: XCTestCase {
    func testMoodLabelOpensSharedEditor() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_MOOD_FIXTURE"] = "signed-in"
        app.launchEnvironment["WHISTLEGRAPH_DEVICE_REPORTS"] = "0"
        app.launch()
        waitForReveal(in: app)
        let type = app.buttons.matching(identifier: "mime-type").matching(NSPredicate(format: "label == %@", "mood")).firstMatch
        XCTAssertTrue(type.waitForExistence(timeout: 10)); type.tap()
        if app.alerts["Before you make something"].waitForExistence(timeout: 2) { app.alerts.buttons["Sounds good"].tap() }
        XCTAssertTrue(app.otherElements["mime-creation"].waitForExistence(timeout: 15))
        XCTAssertFalse(app.textViews["mime-mood-text"].exists)
    }

    func testLiveCenterAndEdgeEscape() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_INTERACTION_FIXTURE"] = "1"
        app.launch()
        waitForReveal(in: app)
        let button = app.webViews.buttons["Touch me"].firstMatch
        XCTAssertTrue(button.waitForExistence(timeout: 20))
        button.tap()
        XCTAssertTrue(app.webViews.buttons["Touched 1"].firstMatch.waitForExistence(timeout: 3), "The first tap reaches the content")
        let first = XCTAttachment(screenshot: app.screenshot())
        first.name = "MIME center interaction"; first.lifetime = .keepAlways; add(first)
        let bounds = app.frame
        let edge = app.coordinate(withNormalizedOffset: .zero).withOffset(CGVector(dx: 3, dy: bounds.height * 0.6))
        edge.press(forDuration: 0.05, thenDragTo: edge.withOffset(CGVector(dx: 0, dy: -180)), withVelocity: .fast, thenHoldForDuration: 0)
        let next = app.buttons.matching(identifier: "mime-global-identifier").matching(NSPredicate(format: "label == %@", "wgSecond")).firstMatch
        XCTAssertTrue(next.waitForExistence(timeout: 5))
        XCTAssertLessThan(next.frame.midY, 150, "The outer edge advances the feed")
        let second = app.webViews.buttons["Touch me"].firstMatch
        XCTAssertTrue(second.waitForExistence(timeout: 5)); second.tap()
        XCTAssertTrue(app.webViews.buttons["Touched 1"].firstMatch.waitForExistence(timeout: 3))
        let returned = expectation(for: NSPredicate(format: "isHittable == true"), evaluatedWith: next)
        XCTAssertEqual(XCTWaiter().wait(for: [returned], timeout: 4), .completed)
        let shot = XCTAttachment(screenshot: app.screenshot())
        shot.name = "MIME corners returned"; shot.lifetime = .keepAlways; add(shot)
        let top = app.coordinate(withNormalizedOffset: .zero).withOffset(CGVector(dx: bounds.width / 2, dy: 85))
        top.press(forDuration: 0.05, thenDragTo: top.withOffset(CGVector(dx: 0, dy: 180)), withVelocity: .fast, thenHoldForDuration: 0)
        let previous = app.buttons.matching(identifier: "mime-global-identifier").matching(NSPredicate(format: "label == %@", "wgFirst")).firstMatch
        XCTAssertTrue(previous.waitForExistence(timeout: 5)); XCTAssertLessThan(previous.frame.midY, 150)
    }

    func testSavedOskiewarReplayInProductionFeed() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launch()
        XCTAssertTrue(app.buttons["mime-global-identifier"].firstMatch.waitForExistence(timeout: 45))
        waitForReveal(in: app)
        let replay = app.buttons.matching(identifier: "mime-global-identifier")
            .matching(NSPredicate(format: "label BEGINSWITH %@", "ow-")).firstMatch
        for _ in 0..<3 {
            if replay.exists && replay.isHittable && replay.frame.midY > 0 && replay.frame.midY < 150 { break }
            app.swipeUp()
            let settled = expectation(for: NSPredicate { _, _ in
                replay.exists && replay.isHittable && replay.frame.midY > 0 && replay.frame.midY < 150
            }, evaluatedWith: app)
            if XCTWaiter().wait(for: [settled], timeout: 5) == .completed { break }
        }
        XCTAssertTrue(replay.exists)
        XCTAssertLessThan(replay.frame.midY, 150)
        replay.tap()
        XCTAssertTrue(app.webViews.otherElements["oskiewar stick fight and lava climb"].firstMatch.waitForExistence(timeout: 45))
        let shot = XCTAttachment(screenshot: app.screenshot())
        shot.name = "MIME saved Oskiewar replay"; shot.lifetime = .keepAlways; add(shot)
        replay.tap()
        app.swipeUp()
        XCTAssertTrue(app.buttons["mime-global-identifier"].firstMatch.exists, "The feed remains scrollable after playback")
    }
    func testConstructingVersionStagesAndLiveSourceShareOneLine() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "progress"
        app.launch()
        XCTAssertTrue(app.buttons["mime-type"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        openEditorFromAccount(in: app); waitForEditor(in: app)
        let preview = app.descendants(matching: .any)["story-picture"].firstMatch
        let picture = preview.frame
        app.buttons["mime-fixture-voice"].tap()
        let strip = app.descendants(matching: .any)["generation-strip"].firstMatch
        XCTAssertTrue(strip.waitForExistence(timeout: 10))
        let status = app.buttons["generation-status"]
        let version = app.staticTexts["generation-version"]
        let starting = expectation(for: NSPredicate(format: "value == %@", "Starting"), evaluatedWith: status)
        XCTAssertEqual(XCTWaiter().wait(for: [starting], timeout: 12), .completed)
        XCTAssertEqual(version.label, "v1")
        XCTAssertEqual(version.value as? String, "Under construction")
        XCTAssertLessThanOrEqual(strip.frame.height, 45)
        XCTAssertEqual(preview.frame, picture, "Generation leaves the preview in place")
        XCTAssertFalse(app.descendants(matching: .any)["generation-request"].firstMatch.exists)
        XCTAssertFalse(app.descendants(matching: .any)["turn-progress"].firstMatch.exists)
        XCTAssertFalse(app.staticTexts["Waiting for model…"].exists)
        let statusShot = XCTAttachment(screenshot: app.screenshot())
        statusShot.name = "MIME ghost version and generation stages"; statusShot.lifetime = .keepAlways; add(statusShot)
        let buffer = app.descendants(matching: .any)["code-ticker"].firstMatch
        if !buffer.exists { status.tap() }
        XCTAssertTrue(buffer.waitForExistence(timeout: 5))
        let before = buffer.value as? String ?? ""
        let live = expectation(for: NSPredicate { _, _ in (buffer.value as? String ?? "") != before }, evaluatedWith: app)
        XCTAssertEqual(XCTWaiter().wait(for: [live], timeout: 12), .completed)
        XCTAssertFalse((buffer.value as? String ?? "").contains("\n"))
        XCTAssertLessThanOrEqual(strip.frame.height, 45)
        XCTAssertLessThanOrEqual(buffer.frame.height, 24)
        XCTAssertLessThanOrEqual(version.frame.maxX, status.frame.minX)
        XCTAssertLessThanOrEqual(status.frame.maxX, buffer.frame.minX)
        XCTAssertEqual(preview.frame, picture)
        let bufferShot = XCTAttachment(screenshot: app.screenshot())
        bufferShot.name = "MIME constructing version with flying source"; bufferShot.lifetime = .keepAlways; add(bufferShot)
        let checking = expectation(for: NSPredicate(format: "value == %@", "Checking"), evaluatedWith: status)
        XCTAssertEqual(XCTWaiter().wait(for: [checking], timeout: 25), .completed)
        XCTAssertEqual(version.label, "v1", "Checking keeps the same provisional version")
        status.tap()
        XCTAssertFalse(buffer.exists)
        XCTAssertTrue(app.staticTexts["Checking"].exists)
        status.tap()
        XCTAssertTrue(buffer.exists)
        app.buttons["generation-stop"].tap()
        let stopped = expectation(for: NSPredicate(format: "exists == false"), evaluatedWith: strip)
        XCTAssertEqual(XCTWaiter().wait(for: [stopped], timeout: 10), .completed)
        XCTAssertTrue(app.buttons["type-control"].isEnabled)
        XCTAssertTrue(app.buttons["attempt-retry"].exists)
        app.buttons["attempt-retry"].tap()
        XCTAssertTrue(strip.waitForExistence(timeout: 10))
        XCTAssertEqual(version.label, "v1", "A stopped try does not consume a version")
        app.buttons["generation-stop"].tap()
    }
    func testRemoteVoiceUnlocksAndReturnsFromPreviewGutter() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "remote-voice"
        app.launch()
        XCTAssertTrue(app.buttons["mime-type"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        openEditorFromAccount(in: app); waitForEditor(in: app)
        for version in 1...2 {
            app.buttons["mime-fixture-voice"].tap()
            let constructing = app.staticTexts["generation-version"]
            XCTAssertTrue(constructing.waitForExistence(timeout: 5))
            XCTAssertEqual(constructing.label, "v\(version)")
            let row = app.buttons["version-\(version)"]
            XCTAssertTrue(row.waitForExistence(timeout: 30))
            let usable = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: app.buttons["type-control"])
            XCTAssertEqual(XCTWaiter().wait(for: [usable], timeout: 10), .completed)
            XCTAssertTrue(row.isEnabled, "Voice completion also releases version selection")
        }
        app.buttons["type-control"].tap()
        XCTAssertTrue(app.textFields["typed-request"].waitForExistence(timeout: 5))
        app.buttons["request-cancel"].tap()
        let keyboardGone = expectation(for: NSPredicate(format: "exists == false"), evaluatedWith: app.keyboards.firstMatch)
        XCTAssertEqual(XCTWaiter().wait(for: [keyboardGone], timeout: 10), .completed)
        let arrow = app.buttons["mime-creation-back"], preview = app.descendants(matching: .any)["story-picture"].firstMatch
        let aligned = expectation(for: NSPredicate { _, _ in abs(arrow.frame.midY - preview.frame.midY) < 2 }, evaluatedWith: app)
        XCTAssertEqual(XCTWaiter().wait(for: [aligned], timeout: 5), .completed)
        XCTAssertEqual(arrow.frame.midY, preview.frame.midY, accuracy: 2)
        XCTAssertLessThanOrEqual(arrow.frame.maxX, preview.frame.minX + 1)
        XCTAssertLessThan(app.buttons["workspace-settings"].frame.minY, 110, "No extra navigation row")
        let screenshot = XCTAttachment(screenshot: app.screenshot())
        screenshot.name = "MIME voice request complete with preview return arrow"; screenshot.lifetime = .keepAlways; add(screenshot)
        let gutter = app.coordinate(withNormalizedOffset: .zero).withOffset(CGVector(dx: arrow.frame.midX, dy: arrow.frame.midY))
        gutter.press(forDuration: 0.1, thenDragTo: gutter.withOffset(CGVector(dx: -35, dy: 0)), withVelocity: .fast, thenHoldForDuration: 0)
        XCTAssertTrue(app.buttons["mime-account"].waitForExistence(timeout: 10))
        openEditorFromAccount(in: app); waitForEditor(in: app)
        XCTAssertTrue(app.buttons["version-2"].isEnabled)
        arrow.tap()
        XCTAssertTrue(app.buttons["mime-account"].waitForExistence(timeout: 10))
    }
    func testVersionRemixesPiecePaintingAndWadAsNewV0() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "remix"
        app.launch()
        XCTAssertTrue(app.buttons["mime-remix-version"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        for (version, author, ware) in [(7, "original", "Aesthetic.Computer Piece"), (3, "painter", "Painting"), (2, "builder", "Doom")] {
            let badge = app.buttons.matching(identifier: "mime-remix-version").matching(NSPredicate(format: "label == %@", "v\(version)")).firstMatch
            let settled = expectation(for: NSPredicate { _, _ in badge.exists && badge.isHittable && badge.frame.midY < 150 }, evaluatedWith: app)
            XCTAssertEqual(XCTWaiter().wait(for: [settled], timeout: 15), .completed)
            XCTAssertEqual(badge.label, "v\(version)")
            let code = version == 7 ? "fixture-remix-piece" : version == 3 ? "fixture-remix-png" : "doom~fixture-remix-wad"
            let identifier = app.buttons.matching(identifier: "mime-global-identifier").matching(NSPredicate(format: "label == %@", code)).firstMatch
            XCTAssertGreaterThan(badge.frame.minX, identifier.frame.maxX)
            let feed = XCTAttachment(screenshot: app.screenshot())
            feed.name = "MIME remix feed v\(version)"; feed.lifetime = .keepAlways; add(feed)
            openType(badge, in: app)
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
            if version == 2 { XCTAssertTrue(app.webViews.otherElements["Empty Doom room"].waitForExistence(timeout: 90), "The remixed WAD reaches its first game frame") }
            let editor = XCTAttachment(screenshot: app.screenshot())
            editor.name = "MIME remixed \(ware) v0"; editor.lifetime = .keepAlways; add(editor)
            app.buttons["mime-creation-back"].tap()
            XCTAssertTrue(badge.waitForExistence(timeout: 10))
            openEditorFromAccount(in: app); waitForEditor(in: app)
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
        handle.tap()
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
        openEditorFromAccount(in: app)
        XCTAssertTrue(app.buttons["ware-picker"].waitForExistence(timeout: 30))
        waitForEditor(in: app)
        XCTAssertFalse(app.buttons["type-control"].isEnabled, "The fixture is still generating")
        app.buttons["mime-creation-back"].tap()
        XCTAssertTrue(app.buttons["mime-account"].waitForExistence(timeout: 10))
        openEditorFromAccount(in: app)
        waitForEditor(in: app)
        XCTAssertFalse(app.buttons["type-control"].isEnabled, "Returning did not stop the turn")
        let editor = XCTAttachment(screenshot: app.screenshot())
        editor.name = "MIME full screen editor while generating"; editor.lifetime = .keepAlways; add(editor)
        let edge = app.coordinate(withNormalizedOffset: CGVector(dx: 0.995, dy: 0.5))
        edge.press(forDuration: 0.1, thenDragTo: edge.withOffset(CGVector(dx: -260, dy: 0)))
        let returned = expectation(for: NSPredicate(format: "isHittable == true"), evaluatedWith: app.buttons["mime-account"])
        XCTAssertEqual(XCTWaiter().wait(for: [returned], timeout: 10), .completed)
        // A local turn keeps its engine; a new-work tap waits in a visible queue.
        app.buttons["mime-type"].firstMatch.tap()
        XCTAssertTrue(app.buttons["mime-work-queue"].waitForExistence(timeout: 5))
        XCTAssertFalse(app.descendants(matching: .any)["mime-creation-loading"].firstMatch.exists)
        app.buttons["mime-work-queue"].tap()
        XCTAssertTrue(app.buttons["queued-work-painting"].waitForExistence(timeout: 5))
        app.buttons["Done"].tap()
        openEditorFromAccount(in: app)
        // A system banner can cover the top-right wordmark during the tap.
        if !app.buttons["mime-creation-back"].isHittable {
            let available = expectation(for: NSPredicate(format: "isHittable == true"), evaluatedWith: app.buttons["mime-account"])
            XCTAssertEqual(XCTWaiter().wait(for: [available], timeout: 10), .completed)
            openEditorFromAccount(in: app)
        }
        waitForEditor(in: app)
        XCTAssertTrue(app.buttons["ware-picker"].label.contains("Aesthetic.Computer Piece"))
        XCTAssertFalse(app.buttons["type-control"].isEnabled)
        app.buttons["mime-creation-back"].tap()
    }

    func testRemixCanLeaveServerTurnAndReturnToItsProject() {
        continueAfterFailure = false
        let app = XCUIApplication()
        app.launchEnvironment["MIME_CREATION_FIXTURE"] = "progress"
        app.launch()
        XCTAssertTrue(app.buttons["mime-remix-version"].firstMatch.waitForExistence(timeout: 30))
        waitForReveal(in: app)
        openEditorFromAccount(in: app); waitForEditor(in: app)
        app.buttons["mime-fixture-voice"].tap()
        let readyToLeave = expectation(for: NSPredicate(format: "isEnabled == true"), evaluatedWith: app.buttons["ware-picker"])
        XCTAssertEqual(XCTWaiter().wait(for: [readyToLeave], timeout: 15), .completed)
        XCTAssertTrue(app.buttons["generation-stop"].exists)
        app.buttons["mime-creation-back"].tap()
        XCTAssertTrue(app.buttons["mime-remix-version"].firstMatch.waitForExistence(timeout: 5))
        app.buttons["mime-remix-version"].firstMatch.tap()
        XCTAssertTrue(app.buttons["version-0"].waitForExistence(timeout: 30))
        waitForEditor(in: app)
        XCTAssertTrue(app.buttons["version-0"].label.contains("Stole from"))
        XCTAssertTrue(app.buttons["type-control"].isEnabled)
        XCTAssertFalse(app.alerts.firstMatch.exists)
        app.buttons["workspace-settings"].tap()
        let original = app.buttons["piece-wgFixture"]
        if !original.exists { app.collectionViews.firstMatch.swipeUp() }
        XCTAssertTrue(original.waitForExistence(timeout: 10))
        original.tap(); waitForEditor(in: app)
        XCTAssertTrue(app.buttons["generation-stop"].waitForExistence(timeout: 15), "The archived turn resumes watching the server")
        app.buttons["generation-stop"].tap()
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
        openEditorFromAccount(in: app)
        waitForEditor(in: app)
        waitForAudio("Preview audible")
        app.buttons["mime-creation-back"].tap()
        waitForAudio("Preview suspended")
        XCUIDevice.shared.press(.home)
        app.activate()
        waitForAudio("Preview suspended") // Foregrounding the feed must not resume it.
        openEditorFromAccount(in: app)
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
            !app.descendants(matching: .any)["mime-creation-loading"].firstMatch.exists && app.buttons["mime-creation-back"].isHittable
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

    private func openEditorFromAccount(in app: XCUIApplication) {
        app.buttons["mime-account"].tap()
        let editor = app.buttons["mime-editor.open"]
        XCTAssertTrue(editor.waitForExistence(timeout: 30))
        editor.tap()
    }

}
