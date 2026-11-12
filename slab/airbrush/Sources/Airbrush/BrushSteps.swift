import AirbrushCore

/// One airbrush, with the same discrete settings for the bar and gestures.
enum BrushSteps {
    static let values: [[Double]] = [
        [1, 2, 4, 8, 16, 32, 64, 128, 256, 512], [0, 0.5, 1],
        [0.1, 0.25, 0.5, 1], [0, 0.25, 0.5, 1]
    ]
    static let names = ["Radius", "Edge", "Flow", "Scatter"]
    static func value(_ brush: Brush, _ parameter: Int) -> Double {
        [brush.radius, brush.hardness, brush.flow, brush.scatter][parameter]
    }
    static func index(_ brush: Brush, _ parameter: Int) -> Int {
        values[parameter].indices.min { abs(values[parameter][$0] - value(brush, parameter)) < abs(values[parameter][$1] - value(brush, parameter)) }!
    }
    static func set(_ brush: inout Brush, _ parameter: Int, _ index: Int) {
        let v = values[parameter][max(0, min(values[parameter].count - 1, index))]
        switch parameter {
        case 0: brush.radius = v
        case 1: brush.hardness = v
        case 2: brush.flow = v
        default: brush.scatter = v
        }
    }
    static func step(_ brush: inout Brush, _ parameter: Int, _ amount: Int) {
        set(&brush, parameter, index(brush, parameter) + amount)
    }
    static func title(_ parameter: Int, _ index: Int) -> String {
        let v = values[parameter][index]
        switch parameter {
        case 0: return "\(Int(v)) px"
        case 1: return ["Soft", "Mid", "Hard"][index]
        case 2: return "F \(Int(v * 100))"
        default: return "S \(Int(v * 100))"
        }
    }
}
