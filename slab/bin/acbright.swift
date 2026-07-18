// acbright.swift — absolute brightness get/set for a Mac's BUILT-IN display,
// via Apple's private DisplayServices framework. This is the one path that
// actually works on Apple Silicon laptops: the Homebrew `brightness` CLI fails
// on built-in panels with `error -536870201`, and AppleScript has no brightness
// verb at all. External monitors are a different world (DDC/CI via `m1ddc`);
// this binary is only for neo/blueberry-style built-in Retina displays.
//
// Usage:
//   acbright            → print current brightness as an integer percent (0–100)
//   acbright <0–100>    → set brightness to that percent; prints the value set
//   acbright +N / -N    → nudge by N points relative to current (clamped 0–100).
//                         The leading sign is what makes it relative — used by
//                         the menubar's ⌃⌥⌘↑/↓ hotkey so a nudge is one token
//                         over ssh (no shell arithmetic, works under any shell).
//
// Built for the `fleet-mcp` fan-out, but standalone and dependency-free —
// compile with `swiftc acbright.swift -o acbright` (Command Line Tools suffice).
import Foundation
import CoreGraphics

typealias GetFn = @convention(c) (UInt32, UnsafeMutablePointer<Float>) -> Int32
typealias SetFn = @convention(c) (UInt32, Float) -> Int32

let path = "/System/Library/PrivateFrameworks/DisplayServices.framework/DisplayServices"
guard let handle = dlopen(path, RTLD_NOW) else {
  FileHandle.standardError.write(Data("acbright: DisplayServices unavailable\n".utf8))
  exit(1)
}
let display = CGMainDisplayID()

func fail(_ msg: String) -> Never {
  FileHandle.standardError.write(Data("acbright: \(msg)\n".utf8))
  exit(1)
}

func readBrightness() -> Double {
  guard let sym = dlsym(handle, "DisplayServicesGetBrightness") else { fail("no get symbol") }
  let getBrightness = unsafeBitCast(sym, to: GetFn.self)
  var value: Float = -1
  let rc = getBrightness(display, &value)
  if rc != 0 { fail("get returned rc=\(rc)") }
  return Double(value) * 100
}

if CommandLine.arguments.count >= 2 {
  let arg = CommandLine.arguments[1]
  guard let n = Double(arg) else { fail("bad percent argument") }
  // A leading + or - means relative: nudge from the current level.
  let relative = arg.hasPrefix("+") || arg.hasPrefix("-")
  let target = relative ? readBrightness() + n : n
  let clamped = max(0, min(100, target))
  guard let sym = dlsym(handle, "DisplayServicesSetBrightness") else { fail("no set symbol") }
  let setBrightness = unsafeBitCast(sym, to: SetFn.self)
  let rc = setBrightness(display, Float(clamped / 100.0))
  if rc != 0 { fail("set returned rc=\(rc)") }
  print(Int(clamped.rounded()))
} else {
  print(Int(readBrightness().rounded()))
}
