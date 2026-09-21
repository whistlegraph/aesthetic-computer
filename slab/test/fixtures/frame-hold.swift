// Disposable real AppKit button, with independent event/count receipts on stdout.
import AppKit

final class HoldFixture: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    let counter = NSTextField(labelWithString: "Count: 0")
    var count = 0
    var downAt: TimeInterval?
    var monitor: Any?

    func applicationDidFinishLaunching(_ notification: Notification) {
        window = NSWindow(contentRect: NSRect(x: 180, y: 260, width: 520, height: 240),
                          styleMask: [.titled, .closable], backing: .buffered, defer: false)
        window.title = "Frame hold check"
        let button = NSButton(title: "Add one", target: self, action: #selector(add))
        button.frame = NSRect(x: 50, y: 110, width: 160, height: 60)
        button.bezelStyle = .rounded
        button.font = .systemFont(ofSize: 22)
        counter.frame = NSRect(x: 270, y: 115, width: 220, height: 50)
        counter.font = .systemFont(ofSize: 24)
        window.contentView?.addSubview(button)
        window.contentView?.addSubview(counter)
        monitor = NSEvent.addLocalMonitorForEvents(matching: [.leftMouseDown, .leftMouseUp]) { [self] event in
            if event.window === window {
                if event.type == .leftMouseDown { downAt = ProcessInfo.processInfo.systemUptime }
            }
            return event
        }
        window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
        emit(["ready": true])
    }

    @objc func add() {
        count += 1
        counter.stringValue = "Count: \(count)"
        emit(["count": count, "eventClickCount": NSApp.currentEvent?.clickCount ?? 0,
              "downToActionMs": downAt.map { (ProcessInfo.processInfo.systemUptime - $0) * 1000 } ?? -1])
    }

    func emit(_ value: [String: Any]) {
        let data = try! JSONSerialization.data(withJSONObject: value)
        FileHandle.standardOutput.write(data + Data([10]))
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}
let app = NSApplication.shared
let fixture = HoldFixture()
app.setActivationPolicy(.regular)
app.delegate = fixture
app.run()
