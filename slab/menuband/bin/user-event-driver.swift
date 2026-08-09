import CoreGraphics
import Foundation

private let leftCommandKeyCode: CGKeyCode = 55
private let cKeyCode: CGKeyCode = 8
private let leftCommandFlags = CGEventFlags(rawValue:
    CGEventFlags.maskCommand.rawValue | 0x0000_0008
)

private func post(_ type: CGEventType, keyCode: CGKeyCode, flags: CGEventFlags) {
    guard let event = CGEvent(
        keyboardEventSource: nil,
        virtualKey: keyCode,
        keyDown: type != .keyUp
    ) else { exit(2) }
    event.type = type
    event.flags = flags
    event.post(tap: .cghidEventTap)
    usleep(40_000)
}

private func commandTap() {
    post(.flagsChanged, keyCode: leftCommandKeyCode, flags: leftCommandFlags)
    post(.flagsChanged, keyCode: leftCommandKeyCode, flags: [])
}

switch CommandLine.arguments.dropFirst().first {
case "command-tap":
    commandTap()
case "command-c":
    post(.flagsChanged, keyCode: leftCommandKeyCode, flags: leftCommandFlags)
    post(.keyDown, keyCode: cKeyCode, flags: leftCommandFlags)
    post(.keyUp, keyCode: cKeyCode, flags: leftCommandFlags)
    post(.flagsChanged, keyCode: leftCommandKeyCode, flags: [])
default:
    fputs("usage: user-event-driver command-tap|command-c\n", stderr)
    exit(64)
}
