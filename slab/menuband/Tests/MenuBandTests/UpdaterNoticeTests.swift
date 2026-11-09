import AppKit
import XCTest
@testable import MenuBand

@MainActor
final class UpdaterNoticeTests: XCTestCase {
    override func tearDown() {
        MenuBandUpdater.dismissNotice()
        MenuBandUpdater.onWillPresent = nil
        super.tearDown()
    }

    private func alert() -> NSAlert {
        _ = NSApplication.shared
        let alert = NSAlert()
        alert.messageText = "Menu Band update test"
        alert.addButton(withTitle: "Update")
        alert.addButton(withTitle: "Later")
        return alert
    }

    func testPresentationReleasesInputBeforeShowingANonmodalWindow() {
        var released = false
        var replies: [Bool] = []
        MenuBandUpdater.onWillPresent = { released = true }
        MenuBandUpdater.present(alert()) { replies.append($0) }
        XCTAssertTrue(released)
        XCTAssertTrue(MenuBandUpdater.notice?.alert.window.isVisible == true)
        XCTAssertNil(NSApp.modalWindow)
        MenuBandUpdater.dismissNotice()
        MenuBandUpdater.dismissNotice()
        XCTAssertEqual(replies, [false])
        XCTAssertNil(MenuBandUpdater.notice)
    }

    func testNoticeReleasesCaptureAndPlayingAgainDismissesIt() {
        _ = NSApplication.shared
        let capture = LocalKeyCapture()
        defer { capture.disarm(reason: .resignedKey) }
        capture.onWillArm = { MenuBandUpdater.dismissNotice() }
        capture.arm()
        XCTAssertTrue(capture.isArmed)
        MenuBandUpdater.onWillPresent = { capture.disarm(reason: .resignedKey) }
        var accepted: Bool?
        MenuBandUpdater.present(alert()) { accepted = $0 }
        XCTAssertFalse(capture.isArmed)
        XCTAssertNil(accepted)
        capture.arm()
        XCTAssertTrue(capture.isArmed)
        XCTAssertEqual(accepted, false)
        XCTAssertNil(MenuBandUpdater.notice)
    }

    func testButtonsAndCloseCompleteExactlyOnce() {
        for choice in 0...2 {
            let alert = alert()
            var replies: [Bool] = []
            let notice = MenuBandUpdateNotice(alert: alert) { replies.append($0) }
            notice.show()
            if choice < 2 { alert.buttons[choice].performClick(nil) }
            else { alert.window.performClose(nil) }
            notice.dismiss()
            XCTAssertEqual(replies, [choice == 0])
            XCTAssertFalse(alert.window.isVisible)
        }
    }

    func testEscapeDeclinesWithoutStartingAnUpdate() {
        let alert = alert()
        var accepted: Bool?
        MenuBandUpdater.present(alert) { accepted = $0 }
        let escape = NSEvent.keyEvent(with: .keyDown, location: .zero,
            modifierFlags: [], timestamp: 0, windowNumber: alert.window.windowNumber,
            context: nil, characters: "\u{1b}", charactersIgnoringModifiers: "\u{1b}",
            isARepeat: false, keyCode: 53)!
        XCTAssertTrue(alert.window.performKeyEquivalent(with: escape))
        XCTAssertEqual(accepted, false)
        XCTAssertNil(MenuBandUpdater.notice)
    }
}
