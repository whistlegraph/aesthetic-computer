// Native display inventory, identification, and session layout transactions.
import AppKit
import CoreGraphics
import Foundation
import Darwin
import SystemConfiguration

struct DisplayError: Error, CustomStringConvertible {
    let description: String
    init(_ message: String) { description = message }
}
func check(_ result: CGError) throws {
    if result != .success { throw DisplayError("CoreGraphics error \(result.rawValue)") }
}
let encoder = JSONEncoder()
encoder.outputFormatting = [.sortedKeys]
func output<T: Encodable>(_ value: T) throws {
    FileHandle.standardOutput.write(try encoder.encode(value))
    FileHandle.standardOutput.write(Data("\n".utf8))
}
struct Rect: Codable, Equatable {
    var x: Int; var y: Int; var width: Int; var height: Int
    init(_ r: CGRect) { x = Int(r.minX); y = Int(r.minY); width = Int(r.width); height = Int(r.height) }
}
struct Mode: Codable, Equatable {
    let id: Int32; let width: Int; let height: Int
    let pixelWidth: Int; let pixelHeight: Int; let hz: Double
    init(_ m: CGDisplayMode) {
        id = m.ioDisplayModeID; width = m.width; height = m.height
        pixelWidth = m.pixelWidth; pixelHeight = m.pixelHeight; hz = m.refreshRate
    }
}
struct Display: Codable {
    let number: Int; let uuid: String; let id: UInt32; let name: String
    let builtin: Bool; let main: Bool; let active: Bool; let asleep: Bool
    let mirrored: Bool; let rotation: Double; let bounds: Rect
    let millimeters: [String: Double]; let mode: Mode?
}
struct Inventory: Encodable {
    let version = 1
    let coordinateSystem = "quartz-top-left-points"
    let displays: [Display]
}
let stateDir = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent(".config/slab/displays")

func onlineIDs() throws -> [CGDirectDisplayID] {
    var count: UInt32 = 0
    try check(CGGetOnlineDisplayList(0, nil, &count))
    var ids = [CGDirectDisplayID](repeating: 0, count: Int(count))
    if count > 0 { try check(CGGetOnlineDisplayList(count, &ids, &count)) }
    return Array(ids.prefix(Int(count)))
}
func uuidFor(_ id: CGDirectDisplayID) throws -> String {
    guard let uuid = CGDisplayCreateUUIDFromDisplayID(id)?.takeRetainedValue() else {
        throw DisplayError("No persistent UUID for display \(id); retry after the display settles")
    }
    return CFUUIDCreateString(nil, uuid) as String
}
// Numbers belong to this host and are never recycled when a panel disconnects.
// A lock and atomic write keep simultaneous CLI/MCP calls from renumbering it.
func numbersFor(_ uuids: [String]) throws -> [String: Int] {
    try FileManager.default.createDirectory(at: stateDir, withIntermediateDirectories: true)
    let fd = open(stateDir.appendingPathComponent("numbers.lock").path, O_CREAT | O_RDWR, 0o600)
    guard fd >= 0 else { throw DisplayError("Cannot open display number lock") }
    defer { close(fd) }
    guard flock(fd, LOCK_EX) == 0 else { throw DisplayError("Cannot lock display numbers") }
    defer { flock(fd, LOCK_UN) }
    let path = stateDir.appendingPathComponent("numbers.json")
    var numbers: [String: Int] = [:]
    if FileManager.default.fileExists(atPath: path.path) {
        numbers = try JSONDecoder().decode([String: Int].self, from: Data(contentsOf: path))
        guard numbers.values.allSatisfy({ $0 > 0 }), Set(numbers.values).count == numbers.count else {
            throw DisplayError("Invalid numbers.json; refusing to renumber displays")
        }
    }
    var changed = false
    for uuid in uuids where numbers[uuid] == nil {
        numbers[uuid] = (numbers.values.max() ?? 0) + 1; changed = true
    }
    if changed { try encoder.encode(numbers).write(to: path, options: .atomic) }
    return numbers
}
func inventory() throws -> Inventory {
    let ids = try onlineIDs().sorted {
        if CGDisplayIsBuiltin($0) != CGDisplayIsBuiltin($1) { return CGDisplayIsBuiltin($0) != 0 }
        return $0 < $1
    }
    let uuids = try ids.map(uuidFor)
    guard Set(uuids).count == uuids.count else { throw DisplayError("Duplicate display UUIDs; cannot safely address these panels") }
    let numbers = try numbersFor(uuids)
    let screens = NSScreen.screens
    return Inventory(displays: try ids.map { id in
        let uuid = try uuidFor(id)
        let screen = screens.first { ($0.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber)?.uint32Value == id }
        let mm = CGDisplayScreenSize(id)
        return Display(number: numbers[uuid]!, uuid: uuid, id: id,
                       name: screen?.localizedName ?? "Display", builtin: CGDisplayIsBuiltin(id) != 0,
                       main: CGDisplayIsMain(id) != 0, active: CGDisplayIsActive(id) != 0,
                       asleep: CGDisplayIsAsleep(id) != 0, mirrored: CGDisplayIsInMirrorSet(id) != 0,
                       rotation: CGDisplayRotation(id), bounds: Rect(CGDisplayBounds(id)),
                       millimeters: ["width": mm.width, "height": mm.height],
                       mode: CGDisplayCopyDisplayMode(id).map(Mode.init))
    }.sorted { $0.number < $1.number })
}
func modesFor(_ id: CGDirectDisplayID) -> [CGDisplayMode] {
    CGDisplayCopyAllDisplayModes(id, [kCGDisplayShowDuplicateLowResolutionModes: true] as CFDictionary) as? [CGDisplayMode] ?? []
}
struct Placement: Codable, Equatable {
    let uuid: String; let x: Int32; let y: Int32; let modeID: Int32; let rotation: Double
}
func placements(_ inv: Inventory) throws -> [Placement] {
    try inv.displays.filter { $0.active }.map {
        guard let mode = $0.mode else { throw DisplayError("Missing display mode") }
        return Placement(uuid: $0.uuid, x: Int32($0.bounds.x), y: Int32($0.bounds.y), modeID: mode.id, rotation: $0.rotation)
    }.sorted { $0.uuid < $1.uuid }
}
struct Request: Codable { let expected: [Placement]; let layout: [Placement] }
func apply(_ request: Request) throws {
    try FileManager.default.createDirectory(at: stateDir, withIntermediateDirectories: true)
    let lock = open(stateDir.appendingPathComponent("layout.lock").path, O_CREAT | O_RDWR, 0o600)
    guard lock >= 0 else { throw DisplayError("Cannot open layout lock") }
    defer { close(lock) }
    guard flock(lock, LOCK_EX) == 0 else { throw DisplayError("Cannot lock display layout") }
    defer { flock(lock, LOCK_UN) }
    let before = try inventory()
    guard !before.displays.contains(where: { $0.mirrored }) else { throw DisplayError("Mirrored layouts are read-only") }
    let current = try placements(before)
    guard current == request.expected.sorted(by: { $0.uuid < $1.uuid }) else {
        throw DisplayError("Displays changed since preview; read the geometry again")
    }
    guard request.layout.count == current.count,
          Set(request.layout.map { $0.uuid }) == Set(current.map { $0.uuid }) else {
        throw DisplayError("Layout must contain every active display exactly once")
    }
    var work: [(Display, Placement, CGDisplayMode)] = []
    var rects: [CGRect] = []
    for p in request.layout {
        guard let d = before.displays.first(where: { $0.uuid == p.uuid }), p.rotation == d.rotation,
              let mode = modesFor(d.id).first(where: { $0.ioDisplayModeID == p.modeID }) else {
            throw DisplayError("Unavailable mode or rotation change; list modes again")
        }
        guard abs(Int64(p.x)) <= 100000, abs(Int64(p.y)) <= 100000 else { throw DisplayError("Origin outside supported range") }
        work.append((d, p, mode))
        let portrait = Int(d.rotation) % 180 != 0
        rects.append(CGRect(x: Int(p.x), y: Int(p.y), width: portrait ? mode.height : mode.width, height: portrait ? mode.width : mode.height))
    }
    guard rects.contains(where: { $0.origin == .zero }) else { throw DisplayError("One display must start at (0,0)") }
    for i in rects.indices {
        for j in rects.indices where j > i {
            let intersection = rects[i].intersection(rects[j])
            if !intersection.isNull && intersection.width > 0 && intersection.height > 0 { throw DisplayError("Displays overlap") }
        }
    }
    func touches(_ a: CGRect, _ b: CGRect) -> Bool {
        ((a.maxX == b.minX || b.maxX == a.minX) && min(a.maxY, b.maxY) > max(a.minY, b.minY)) ||
        ((a.maxY == b.minY || b.maxY == a.minY) && min(a.maxX, b.maxX) > max(a.minX, b.minX))
    }
    var reached: Set<Int> = rects.isEmpty ? [] : [0]
    while true {
        let old = reached
        for i in rects.indices where !reached.contains(i) {
            if reached.contains(where: { touches(rects[i], rects[$0]) }) { reached.insert(i) }
        }
        if reached == old { break }
    }
    guard reached.count == rects.count else { throw DisplayError("Displays must share edges; gaps cannot be applied exactly") }
    let backup = stateDir.appendingPathComponent("layout-\(UUID().uuidString).json")
    try encoder.encode(current).write(to: backup, options: .atomic)
    fputs("Restore file: \(backup.path)\n", stderr)
    var config: CGDisplayConfigRef?
    try check(CGBeginDisplayConfiguration(&config))
    guard let config else { throw DisplayError("Cannot begin display transaction") }
    var completed = false
    defer { if !completed { CGCancelDisplayConfiguration(config) } }
    for (d, p, mode) in work {
        if d.mode?.id != p.modeID { try check(CGConfigureDisplayWithDisplayMode(config, d.id, mode, nil)) }
        try check(CGConfigureDisplayOrigin(config, d.id, p.x, p.y))
    }
    try check(CGCompleteDisplayConfiguration(config, .forSession)); completed = true
    // Quartz may normalize geometry; always return the observed result.
    Thread.sleep(forTimeInterval: 0.5)
    struct Result: Encodable { let backup: String; let matchesRequested: Bool; let inventory: Inventory }
    let after = try inventory()
    try output(Result(backup: backup.path,
                      matchesRequested: try placements(after) == request.layout.sorted { $0.uuid < $1.uuid }, inventory: after))
}

var labelWindows: [NSWindow] = []
func identify(_ inv: Inventory, seconds: Double, number: Int?, seatNumber: Int? = nil) throws {
    let app = NSApplication.shared
    app.setActivationPolicy(.accessory)
    let host = SCDynamicStoreCopyLocalHostName(nil) as String? ?? ProcessInfo.processInfo.hostName.components(separatedBy: ".")[0]
    for d in inv.displays where number == nil || number == d.number {
        guard let screen = NSScreen.screens.first(where: { ($0.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber)?.uint32Value == d.id }) else { continue }
        let width = min(520.0, screen.frame.width * 0.7), height = min(240.0, screen.frame.height * 0.5)
        let rect = NSRect(x: screen.frame.midX - width / 2, y: screen.frame.midY - height / 2, width: width, height: height)
        let window = NSWindow(contentRect: rect, styleMask: [.borderless], backing: .buffered, defer: false)
        window.level = .screenSaver; window.isOpaque = true; window.backgroundColor = NSColor(calibratedWhite: 0.08, alpha: 1)
        window.ignoresMouseEvents = true; window.hasShadow = true
        window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
        let numberLabel = NSTextField(labelWithString: "\(seatNumber ?? d.number)")
        numberLabel.font = .monospacedDigitSystemFont(ofSize: 132, weight: .bold)
        numberLabel.textColor = .white; numberLabel.alignment = .center
        numberLabel.frame = NSRect(x: 0, y: 52, width: width, height: 168)
        let hostLabel = NSTextField(labelWithString: seatNumber == nil ? host : "\(host):\(d.number)")
        hostLabel.font = .systemFont(ofSize: 30, weight: .medium); hostLabel.textColor = .white; hostLabel.alignment = .center
        hostLabel.frame = NSRect(x: 0, y: 16, width: width, height: 40)
        window.contentView?.addSubview(numberLabel); window.contentView?.addSubview(hostLabel)
        window.orderFrontRegardless(); labelWindows.append(window)
    }
    guard !labelWindows.isEmpty else { throw DisplayError("No drawable display for that number") }
    Timer.scheduledTimer(withTimeInterval: seconds, repeats: false) { _ in
        labelWindows.forEach { $0.orderOut(nil) }; app.stop(nil)
        app.postEvent(NSEvent.otherEvent(with: .applicationDefined, location: .zero, modifierFlags: [], timestamp: 0, windowNumber: 0, context: nil, subtype: 0, data1: 0, data2: 0)!, atStart: false)
    }
    app.run()
    try output(["identified": labelWindows.count])
}

do {
    let args = Array(CommandLine.arguments.dropFirst())
    switch args.first ?? "list" {
    case "list": try output(inventory())
    case "modes":
        guard args.count == 2, let number = Int(args[1]), let d = try inventory().displays.first(where: { $0.number == number }) else { throw DisplayError("usage: modes NUMBER") }
        try output(modesFor(d.id).map(Mode.init))
    case "identify":
        guard args.count >= 2, args.count <= 4, let seconds = Double(args[1]), seconds.isFinite, seconds >= 1, seconds <= 30 else { throw DisplayError("usage: identify SECONDS [NUMBER [SEAT_NUMBER]] (1–30 seconds)") }
        let number = args.count >= 3 ? Int(args[2]) : nil
        let seatNumber = args.count == 4 ? Int(args[3]) : nil
        if args.count >= 3 && (number == nil || number! < 1) { throw DisplayError("Invalid display number") }
        if args.count == 4 && (seatNumber == nil || seatNumber! < 1) { throw DisplayError("Invalid seat number") }
        try identify(inventory(), seconds: seconds, number: number, seatNumber: seatNumber)
    case "apply":
        try apply(JSONDecoder().decode(Request.self, from: FileHandle.standardInput.readDataToEndOfFile()))
    case "saved":
        guard args.count == 2, args[1].range(of: "^layout-[A-Fa-f0-9-]{36}\\.json$", options: .regularExpression) != nil else { throw DisplayError("Invalid backup name") }
        try output(JSONDecoder().decode([Placement].self, from: Data(contentsOf: stateDir.appendingPathComponent(args[1]))))
    default: throw DisplayError("usage: slab-displays-native list | modes NUMBER | identify SECONDS [NUMBER] | apply < request.json")
    }
} catch {
    fputs("displays: \(error)\n", stderr); exit(1)
}
