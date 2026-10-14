// keybot.swift — native key events into the oskiewar app by pid. stdin: hold <code> | release <code> | press <code> <ms> | releaseall | quit
import Foundation
import CoreGraphics
func findPid() -> pid_t? {
    let p = Process(); p.executableURL = URL(fileURLWithPath: "/usr/bin/pgrep"); p.arguments = ["-x", "oskiewar"]
    let pipe = Pipe(); p.standardOutput = pipe; try? p.run(); p.waitUntilExit()
    let out = String(data: pipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8) ?? ""
    return out.split(separator: "\n").first.flatMap { pid_t($0) }
}
var pid: pid_t? = nil
var held = Set<CGKeyCode>()
let lock = NSLock()
func key(_ code: CGKeyCode, _ down: Bool) {
    if pid == nil { pid = findPid() }
    guard let pid else { print("no app"); fflush(stdout); return }
    // Stamped, so an app in bot mode (OSKIEWAR_BOT=1) knows the press is the
    // harness's and not someone at the keyboard. Same value as main.swift.
    guard let event = CGEvent(keyboardEventSource: nil, virtualKey: code, keyDown: down) else { return }
    event.setIntegerValueField(.eventSourceUserData, value: 0x05B07B07)
    event.postToPid(pid)
}
print("keybot ready pid \(findPid().map(String.init) ?? "none")"); fflush(stdout)
while let line = readLine() {
    let parts = line.split(separator: " ").map(String.init)
    guard let cmd = parts.first else { continue }
    if cmd == "quit" { break }
    switch cmd {
    case "hold": if let c = parts.dropFirst().first.flatMap({ CGKeyCode($0) }) { lock.lock(); held.insert(c); lock.unlock(); key(c, true) }
    case "release": if let c = parts.dropFirst().first.flatMap({ CGKeyCode($0) }) { lock.lock(); held.remove(c); lock.unlock(); key(c, false) }
    case "press":
        if parts.count >= 3, let c = CGKeyCode(parts[1]), let ms = Double(parts[2]) {
            key(c, true); lock.lock(); held.insert(c); lock.unlock()
            DispatchQueue.global().asyncAfter(deadline: .now() + ms / 1000) { key(c, false); lock.lock(); held.remove(c); lock.unlock() }
        }
    case "releaseall": lock.lock(); let all = held; held.removeAll(); lock.unlock(); for c in all { key(c, false) }
    default: break
    }
    print("ok \(line)"); fflush(stdout)
}
lock.lock(); for c in held { key(c, false) }; lock.unlock()
