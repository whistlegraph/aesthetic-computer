import AppKit
import Foundation

let identifier = "unipointer"
let stateKind = "unipointer-state"

switch CommandLine.arguments.dropFirst().first {
case "id", "--id", "--identifier":
    print(identifier)
    exit(0)
case "help", "--help", "-h":
    print("usage: unipointer [state|--identifier]")
    print("  state          print the versioned unipointer-state JSON (default)")
    print("  --identifier   print the canonical identifier: unipointer")
    exit(0)
case nil, "state", "--state":
    break
default:
    fputs("unipointer: unknown command\n", stderr)
    exit(64)
}

// `unipointer` is the fleet cursor-state front door. Keep stdout machine-readable
// so host tooling and agents can use it without scraping Deskflow logs.
let point = NSEvent.mouseLocation
guard let screen = NSScreen.screens.first(where: { NSMouseInRect(point, $0.frame, false) })
        ?? NSScreen.main else {
    fputs("unipointer: no active screen\n", stderr)
    exit(1)
}
let frame = screen.frame
let payload: [String: Any] = [
    "identifier": identifier,
    "kind": stateKind,
    "version": 1,
    "x": point.x,
    "y": point.y,
    "frame": [
        "x": frame.minX,
        "y": frame.minY,
        "width": frame.width,
        "height": frame.height,
    ],
    "normalized": [
        "x": (point.x - frame.minX) / frame.width,
        "y": (point.y - frame.minY) / frame.height,
    ],
]
let data = try JSONSerialization.data(withJSONObject: payload, options: [.sortedKeys])
FileHandle.standardOutput.write(data)
FileHandle.standardOutput.write(Data("\n".utf8))
