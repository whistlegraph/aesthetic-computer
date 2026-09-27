import Foundation
import BrushCore

// Host input generator. The brush sees only its recorded samples.
func makeGesture(seed: Int, width: Int, height: Int, mode: Int) -> [GestureSample] {
    var state = UInt32(seed)
    func random(_ n: Int) -> Int { state = state &* 1664525 &+ 1013904223; return Int(state >> 8) % n }
    let count = 96, spacing = 3 + random(5)
    var x = random(width), y = random(height), vx = random(7) - 3, vy = random(7) - 3
    let startX = x, startY = y, endX = random(width), endY = random(height)
    let phase = Double(random(628)) / 100
    let turns = Double(1 + random(3))
    return (0..<count).map { index in
        let t = Double(index) / Double(count - 1)
        switch mode {
        case 1:
            x = Int(Double(startX) * (1-t) + Double(endX) * t)
            y = Int(Double(startY) * (1-t) + Double(endY) * t)
        case 2:
            let angle = phase + t * .pi * 2 * turns
            x = Int(Double(width - 1) * (0.5 + 0.42 * cos(angle)))
            y = Int(Double(height - 1) * (0.5 + 0.42 * sin(angle * 1.5)))
        case 3:
            x = Int(t * Double(width - 1))
            y = Int(Double(height - 1) * (0.15 + 0.7 * abs(2 * ((t * turns).truncatingRemainder(dividingBy: 1)) - 1)))
        default:
            if index > 0 {
                vx = max(-4, min(4, vx + random(3) - 1)); vy = max(-4, min(4, vy + random(3) - 1))
                if vx == 0 && vy == 0 { vx = 1 }
                if x + vx < 0 || x + vx >= width { vx = -vx }
                if y + vy < 0 || y + vy >= height { vy = -vy }
                x = max(0, min(width-1, x + vx)); y = max(0, min(height-1, y + vy))
            }
        }
        return GestureSample(x: x, y: y, tick: index * spacing)
    }
}
