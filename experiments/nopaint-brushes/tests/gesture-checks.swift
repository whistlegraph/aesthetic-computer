import Foundation
import BrushCore

@main struct GestureChecks {
    static func main() throws {
        let brush = try JSONDecoder().decode(Brush.self, from: Data(contentsOf: URL(fileURLWithPath: "experiments/nopaint-brushes/fixtures/line.brush.json")))
        for mode in 0..<4 {
            for size in [1, 8, 256] {
                for seed in [0, 42, 4294967295] {
                    let samples = makeGesture(seed: seed, width: size, height: size, mode: mode)
                    precondition(samples.allSatisfy { $0.x >= 0 && $0.x < size && $0.y >= 0 && $0.y < size })
                    let call = Invocation(brush: brush, seed: seed, tick: samples.last!.tick, gesture: samples)
                    _ = try renderBrush(Array(repeating: 255, count: size * size * 4), width: size, height: size, invocation: call)
                }
            }
        }
        print("36 generated gesture cases passed")
    }
}
