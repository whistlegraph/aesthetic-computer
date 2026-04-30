import AppKit

enum NoteExpression {
    static func values(
        for midiNote: UInt8,
        at point: NSPoint,
        layout: KeyboardIconRenderer.Layout = .fixedCanvas
    ) -> (velocity: UInt8, pan: UInt8) {
        guard let rect = KeyboardIconRenderer.keyRect(for: midiNote, layout: layout) else {
            return (100, 64)
        }
        return values(in: rect, at: point)
    }

    private static func values(in rect: NSRect, at point: NSPoint) -> (velocity: UInt8, pan: UInt8) {
        let xRel = max(0, min(1, (point.x - rect.minX) / rect.width))
        let yRel = max(0, min(1, (point.y - rect.minY) / rect.height))
        let pan = UInt8(max(0, min(127, Int(round(xRel * 127)))))
        let yDist = abs(yRel - 0.5) * 2.0
        let vMin: Double = 60, vMax: Double = 120
        let vel = vMax - (vMax - vMin) * yDist
        let velocity = UInt8(max(1, min(127, Int(round(vel)))))
        return (velocity, pan)
    }
}
