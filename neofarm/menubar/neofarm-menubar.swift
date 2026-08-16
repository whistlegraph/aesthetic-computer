// neofarm-menubar.swift — the farm's pulse in the macOS menu bar.
//
//   swiftc -O -o neofarm-menubar neofarm-menubar.swift
//
// Reads ~/.local/share/neofarm/status.json (written by the daemon each tick)
// every five seconds. Title shows life: 🌾 population · queue. Grey sprout
// means the daemon has gone quiet (no tick in 10 minutes). The menu is
// read-only telemetry plus a jump to the latest digest — control stays with
// the daemon and the knobs file, not this widget.

import AppKit

let statusPath = FileManager.default.homeDirectoryForCurrentUser
  .appendingPathComponent(".local/share/neofarm/status.json")

struct FarmStatus {
  var host = "?", population = 0, queue = 0, tick = 0
  var tried = 0, kept = 0, births = 0
  var lastTick: Date? = nil
  var digest: String? = nil
}

func readStatus() -> FarmStatus? {
  guard let data = try? Data(contentsOf: statusPath),
        let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
  else { return nil }
  var status = FarmStatus()
  status.host = json["host"] as? String ?? "?"
  status.population = json["population"] as? Int ?? 0
  status.queue = json["queue"] as? Int ?? 0
  status.tick = json["tick"] as? Int ?? 0
  status.tried = json["tried"] as? Int ?? 0
  status.kept = json["kept"] as? Int ?? 0
  status.births = json["births"] as? Int ?? 0
  status.digest = json["digest"] as? String
  if let stamp = json["lastTick"] as? Double {
    status.lastTick = Date(timeIntervalSince1970: stamp)
  }
  return status
}

class Farmhand: NSObject {
  let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
  var timer: Timer?

  func start() {
    refresh()
    timer = Timer.scheduledTimer(withTimeInterval: 5, repeats: true) { _ in self.refresh() }
  }

  func refresh() {
    let menu = NSMenu()
    guard let status = readStatus() else {
      item.button?.title = "🌾 –"
      menu.addItem(withTitle: "no farm state yet", action: nil, keyEquivalent: "")
      finish(menu)
      return
    }
    let age = status.lastTick.map { Date().timeIntervalSince($0) } ?? .infinity
    let alive = age < 600
    item.button?.title = alive ? "🌾 \(status.population)·\(status.queue)" : "🥀 \(status.population)"
    item.button?.alphaValue = alive ? 1.0 : 0.55

    menu.addItem(withTitle: "neofarm on \(status.host)", action: nil, keyEquivalent: "")
    menu.addItem(withTitle: alive ? "tick \(status.tick), \(Int(age))s ago" : "daemon quiet (\(Int(age / 60))m)",
                 action: nil, keyEquivalent: "")
    menu.addItem(NSMenuItem.separator())
    menu.addItem(withTitle: "population \(status.population) · queue \(status.queue)", action: nil, keyEquivalent: "")
    menu.addItem(withTitle: "lifetime \(status.kept)/\(status.tried) kept · \(status.births) births", action: nil, keyEquivalent: "")
    if status.digest != nil {
      menu.addItem(NSMenuItem.separator())
      let digest = NSMenuItem(title: "Open Latest Digest", action: #selector(openDigest), keyEquivalent: "d")
      digest.target = self
      menu.addItem(digest)
    }
    finish(menu)
  }

  func finish(_ menu: NSMenu) {
    menu.addItem(NSMenuItem.separator())
    let quit = NSMenuItem(title: "Quit Neofarm Menubar", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
    menu.addItem(quit)
    item.menu = menu
  }

  @objc func openDigest() {
    if let digest = readStatus()?.digest {
      NSWorkspace.shared.open(URL(fileURLWithPath: digest))
    }
  }
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let farmhand = Farmhand()
farmhand.start()
app.run()
