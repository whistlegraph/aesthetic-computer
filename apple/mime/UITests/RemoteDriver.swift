import XCTest
import Network

/// Remote control for MIME on a real phone: a UI test that never ends, serving
/// HTTP on the phone so the Mac can tap, swipe, type and screenshot through
/// XCUITest. Runs only when the driver is asked for (MIME_DRIVER=1), and attaches
/// to the running app instead of relaunching it. See tools/drive.sh.
///
///   GET /shot                       PNG of the screen
///   GET /tree                       the accessibility hierarchy
///   GET /tap?x=&y=                  tap at screen points
///   GET /tap?id=                    tap an element by accessibility identifier or label
///   GET /swipe?x1=&y1=&x2=&y2=&t=   drag between points, holding t seconds first
///   GET /doubletap?x=&y=            double-tap at screen points
///   GET /pinch?scale=&v=            pinch the window (scale > 1 zooms in)
///   GET /type?text=                 type into the focused field
///   GET /launch  /activate  /home  /stop
final class RemoteDriver: XCTestCase {
    private struct Job { let path: String; let query: [String: String]; let reply: (Int, String, Data) -> Void }
    private let lock = NSLock()
    private var jobs: [Job] = []

    func testRemote() throws {
        let env = ProcessInfo.processInfo.environment
        guard env["MIME_DRIVER"] == "1" else { throw XCTSkip("Remote driver runs only with TEST_RUNNER_MIME_DRIVER=1") }
        executionTimeAllowance = 24 * 3600
        let app = XCUIApplication()
        app.activate()

        let port = NWEndpoint.Port(rawValue: UInt16(env["MIME_DRIVER_PORT"] ?? "") ?? 8765)!
        let listener = try NWListener(using: .tcp, on: port)
        listener.newConnectionHandler = { [weak self] connection in self?.accept(connection) }
        listener.start(queue: .global())
        print("[driver] listening on \(port)")

        var running = true
        while running {
            lock.lock(); let job = jobs.isEmpty ? nil : jobs.removeFirst(); lock.unlock()
            guard let job else { RunLoop.current.run(until: Date(timeIntervalSinceNow: 0.03)); continue }
            if job.path == "/stop" { running = false }
            let (status, type, body) = handle(job, app: app)
            job.reply(status, type, body)
        }
        listener.cancel()
    }

    private func handle(_ job: Job, app: XCUIApplication) -> (Int, String, Data) {
        let q = job.query
        func number(_ key: String) -> CGFloat? { q[key].flatMap(Double.init).map { CGFloat($0) } }
        func point(_ x: CGFloat, _ y: CGFloat) -> XCUICoordinate {
            app.coordinate(withNormalizedOffset: .zero).withOffset(CGVector(dx: x, dy: y))
        }
        let ok = (200, "text/plain", Data("ok\n".utf8))
        switch job.path {
        case "/shot":
            return (200, "image/png", XCUIScreen.main.screenshot().pngRepresentation)
        case "/tree":
            return (200, "text/plain", Data(app.debugDescription.utf8))
        case "/tap":
            if let id = q["id"] {
                let element = app.descendants(matching: .any).matching(NSPredicate(format: "identifier == %@ OR label == %@", id, id)).firstMatch
                guard element.waitForExistence(timeout: 3) else { return (404, "text/plain", Data("no element \(id)\n".utf8)) }
                element.tap(); return ok
            }
            guard let x = number("x"), let y = number("y") else { return (400, "text/plain", Data("tap needs x,y or id\n".utf8)) }
            point(x, y).tap(); return ok
        case "/swipe":
            guard let x1 = number("x1"), let y1 = number("y1"), let x2 = number("x2"), let y2 = number("y2")
            else { return (400, "text/plain", Data("swipe needs x1,y1,x2,y2\n".utf8)) }
            point(x1, y1).press(forDuration: Double(number("t") ?? 0.05), thenDragTo: point(x2, y2)); return ok
        case "/doubletap":
            guard let x = number("x"), let y = number("y") else { return (400, "text/plain", Data("doubletap needs x,y\n".utf8)) }
            point(x, y).doubleTap(); return ok
        case "/pinch":
            app.windows.firstMatch.pinch(withScale: number("scale") ?? 3, velocity: number("v") ?? 2); return ok
        case "/type":
            app.typeText(q["text"] ?? ""); return ok
        case "/launch": app.launch(); return ok
        case "/activate": app.activate(); return ok
        case "/home": XCUIDevice.shared.press(.home); return ok
        case "/stop": return ok
        default: return (404, "text/plain", Data("unknown \(job.path)\n".utf8))
        }
    }

    /// One request per connection: read the request line, queue it for the test's
    /// main thread (XCUITest calls must happen there), answer, close.
    private func accept(_ connection: NWConnection) {
        connection.start(queue: .global())
        connection.receive(minimumIncompleteLength: 1, maximumLength: 65536) { [weak self] data, _, _, _ in
            guard let self, let data, let head = String(data: data, encoding: .utf8)?.split(separator: "\r\n").first else { connection.cancel(); return }
            let target = head.split(separator: " ").dropFirst().first.map(String.init) ?? "/"
            let parts = URLComponents(string: target)
            var query: [String: String] = [:]
            for item in parts?.queryItems ?? [] { query[item.name] = item.value ?? "" }
            let job = Job(path: parts?.path ?? target, query: query) { status, type, body in
                var response = Data("HTTP/1.1 \(status) \(status == 200 ? "OK" : "Error")\r\nContent-Type: \(type)\r\nContent-Length: \(body.count)\r\nConnection: close\r\n\r\n".utf8)
                response.append(body)
                connection.send(content: response, completion: .contentProcessed { _ in connection.cancel() })
            }
            self.lock.lock(); self.jobs.append(job); self.lock.unlock()
        }
    }
}
