import XCTest
@testable import MenuBand

final class InterfaceResetCoordinatorTests: XCTestCase {
    func testOverlappingRequestsJoinOneResetAndKeepBusyUntilCompletion() {
        var devices: [UInt32] = [], busy: [Bool] = [], results: [Bool] = []
        var finish: ((Bool) -> Void)?
        let coordinator = InterfaceResetCoordinator(perform: { device, completion in
            devices.append(device); finish = completion
        }, busyChanged: { busy.append($0) })
        for _ in 0..<20 { coordinator.request(87) { results.append($0) } }
        XCTAssertEqual(devices, [87])
        XCTAssertEqual(busy, [true])
        XCTAssertTrue(results.isEmpty)
        finish?(true)
        XCTAssertEqual(results, Array(repeating: true, count: 20))
        XCTAssertEqual(busy, [true, false])
    }

    func testDifferentDevicesAreSerializedAndFailureDoesNotStrandTheQueue() {
        var devices: [UInt32] = [], busy: [Bool] = [], results: [Bool] = []
        var finishes: [(Bool) -> Void] = []
        let coordinator = InterfaceResetCoordinator(perform: { device, completion in
            devices.append(device); finishes.append(completion)
        }, busyChanged: { busy.append($0) })
        coordinator.request(87) { results.append($0) }
        coordinator.request(88) { results.append($0) }
        coordinator.request(88) { results.append($0) }
        XCTAssertEqual(devices, [87])
        finishes[0](false)
        XCTAssertEqual(devices, [87, 88])
        XCTAssertEqual(busy, [true])
        finishes[1](true)
        XCTAssertEqual(results, [false, true, true])
        XCTAssertEqual(busy, [true, false])
    }
}
