// host.swift — the development host: a small program that never changes while
// you work. It makes the application, loads the app's code from a dynamic
// library (build/dev/libNCCd.dylib), and watches that file. When dev.sh has
// built it again, the host asks the old code to leave (nccd_teardown) and
// starts the new code in its place (nccd_boot), in the same process: the
// window comes back where it was, on the same tab, with the same draft.
//
// A library that has been loaded is never unloaded (its views may still be
// being torn down); each build is loaded from its own copy, under a new name,
// so the loader takes it for a new library.

import AppKit

let library = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "build/dev/libNCCd.dylib"
var loaded: UnsafeMutableRawPointer?
var seen: Date?
var count = 0

func say(_ s: String) { FileHandle.standardError.write(Data(("nccd dev: " + s + "\n").utf8)) }

func stamp() -> Date? {
    (try? FileManager.default.attributesOfItem(atPath: library))?[.modificationDate] as? Date
}

func load() {
    count += 1
    let copy = NSTemporaryDirectory() + "libNCCd-\(ProcessInfo.processInfo.processIdentifier)-\(count).dylib"
    try? FileManager.default.removeItem(atPath: copy)
    do { try FileManager.default.copyItem(atPath: library, toPath: copy) } catch { return say("cannot copy \(library): \(error.localizedDescription)") }
    guard let handle = dlopen(copy, RTLD_NOW | RTLD_LOCAL) else { return say("cannot load: " + String(cString: dlerror())) }
    typealias Entry = @convention(c) () -> Void
    if let old = loaded, let leave = dlsym(old, "nccd_teardown") {
        unsafeBitCast(leave, to: Entry.self)()
    }
    guard let boot = dlsym(handle, "nccd_boot") else { return say("the library has no nccd_boot") }
    loaded = handle
    unsafeBitCast(boot, to: Entry.self)()
    say(count == 1 ? "loaded" : "reloaded (\(count - 1))")
}

let app = NSApplication.shared
app.setActivationPolicy(ProcessInfo.processInfo.environment["NCC_SNAPSHOT"] != nil ? .accessory : .regular)
DispatchQueue.main.async {
    seen = stamp()
    load()
}
// Look at the library twice a second; load it again when it is newer and has stopped changing.
var pending: Date?
Timer.scheduledTimer(withTimeInterval: 0.5, repeats: true) { _ in
    guard let now = stamp(), let before = seen, now > before else { return }
    if pending == now {                 // the same stamp twice running: the linker is done with it
        seen = now
        pending = nil
        load()
    } else {
        pending = now
    }
}
app.run()
