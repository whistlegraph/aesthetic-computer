import CoreGraphics
import Foundation

guard CommandLine.arguments.count >= 4,
      let x = Double(CommandLine.arguments[1]),
      let y = Double(CommandLine.arguments[2]),
      let durationMs = Double(CommandLine.arguments[3]) else {
    fputs("usage: captutor-pointer x y duration-ms\n", stderr)
    exit(2)
}

let destination = CGPoint(x: x, y: y)
let start = CGEvent(source: nil)?.location ?? destination
let frames = max(1, Int(durationMs / (1000.0 / 120.0)))

for index in 1...frames {
    let t = Double(index) / Double(frames)
    let eased = t < 0.5
        ? 4.0 * t * t * t
        : 1.0 - pow(-2.0 * t + 2.0, 3.0) / 2.0
    let point = CGPoint(
        x: start.x + (destination.x - start.x) * eased,
        y: start.y + (destination.y - start.y) * eased
    )
    CGWarpMouseCursorPosition(point)
    usleep(8_333)
}

CGWarpMouseCursorPosition(destination)
CGAssociateMouseAndMouseCursorPosition(1)
