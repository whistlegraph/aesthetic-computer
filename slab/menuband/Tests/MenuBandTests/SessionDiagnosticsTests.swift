import Foundation
import XCTest
@testable import MenuBand

final class SessionDiagnosticsTests: XCTestCase {
    private var directory: URL!
    override func setUp() {
        directory = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
    }
    override func tearDown() { try? FileManager.default.removeItem(at: directory) }

    private func rows(_ url: URL) throws -> [SessionDiagnostics.Row] {
        try String(contentsOf: url).split(separator: "\n").map {
            try JSONDecoder().decode(SessionDiagnostics.Row.self, from: Data($0.utf8))
        }
    }

    func testDisabledDoesNotCreateEvidence() {
        let log = SessionDiagnostics(enabled: false, directory: directory)
        log.begin(); log.record(.captureArm); log.end(); log.flush()
        XCTAssertFalse(FileManager.default.fileExists(atPath: directory.path))
    }

    func testUncleanAndCleanRelaunchAreDistinguished() throws {
        let first = SessionDiagnostics(enabled: true, directory: directory)
        first.begin(); first.record(.captureDisarmed, reason: .resignedKey); first.flush()
        let second = SessionDiagnostics(enabled: true, directory: directory)
        second.begin(); second.end()
        let third = SessionDiagnostics(enabled: true, directory: directory)
        third.begin(); third.end()
        let events = try rows(third.currentURL).map(\.event)
        XCTAssertEqual(events.filter { $0 == .previousSessionUnclean }.count, 1)
        XCTAssertEqual(events.filter { $0 == .sessionStarted }.count, 3)
        XCTAssertEqual(events.filter { $0 == .sessionEnded }.count, 2)
        XCTAssertEqual(events.last, .sessionEnded)
    }

    func testRotationIsBoundedAndRowsHaveOnlyAllowedFields() throws {
        let log = SessionDiagnostics(enabled: true, directory: directory, maxBytes: 1500)
        log.begin()
        for _ in 0..<100 {
            log.record(.captureDisarmed, reason: .resignedKey, source: .localCapture)
            log.flush()
        }
        log.end()
        for url in [log.currentURL, log.previousURL] {
            let data = try Data(contentsOf: url)
            XCTAssertLessThanOrEqual(data.count, 1500)
            XCTAssertFalse(try rows(url).isEmpty)
            for line in data.split(separator: 0x0a) {
                let json = try XCTUnwrap(JSONSerialization.jsonObject(with: Data(line)) as? [String: Any])
                XCTAssertTrue(Set(json.keys).isSubset(of: ["schema", "session", "timestamp", "uptime", "event", "reason", "source", "state"]))
            }
            let mode = try FileManager.default.attributesOfItem(atPath: url.path)[.posixPermissions] as? Int
            XCTAssertEqual(mode, 0o600)
        }
        XCTAssertEqual(try FileManager.default.contentsOfDirectory(atPath: directory.path).count, 3)
        XCTAssertEqual(try FileManager.default.attributesOfItem(atPath: directory.path)[.posixPermissions] as? Int, 0o700)
    }

    func testConcurrentProducersAndShutdownLeaveValidJSON() throws {
        let log = SessionDiagnostics(enabled: true, directory: directory, maxBytes: 4096)
        log.begin()
        DispatchQueue.concurrentPerform(iterations: 1000) { _ in log.record(.tapDisabled, reason: .timeout) }
        log.end()
        log.record(.captureArm); log.flush()
        XCTAssertEqual(try rows(log.currentURL).last?.event, .sessionEnded)
        let marker = try XCTUnwrap(JSONSerialization.jsonObject(with: Data(contentsOf: log.markerURL)) as? [String: Any])
        XCTAssertEqual(marker["clean"] as? Bool, true)
    }

    func testUnwritableDestinationDoesNotThrowOrBlockProducers() throws {
        try Data("not a directory".utf8).write(to: directory)
        let log = SessionDiagnostics(enabled: true, directory: directory)
        log.begin(); log.record(.captureArm); log.end(); log.flush()
        XCTAssertEqual(try String(contentsOf: directory), "not a directory")
    }
}
