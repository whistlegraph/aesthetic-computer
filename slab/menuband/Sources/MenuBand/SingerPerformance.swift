import Foundation

/// Score curves use the same axes as the slide: negative X = space,
/// positive X = echo, Y = pitch (one unit = 12 semitones).
struct SingerPerformance: Decodable {
    struct Key: Decodable {
        var beat: Double
        var space: Float
        var pitch: Float
        var echo: Float? = nil
    }
    var keys: [Key]
    var expression: Double = 0.75

    static func decode(_ text: String?) -> Self? {
        guard let text, let data = Data(base64Encoded: text),
              var p = try? JSONDecoder().decode(Self.self, from: data),
              !p.keys.isEmpty, p.expression.isFinite,
              p.keys.allSatisfy({ $0.beat.isFinite && $0.beat >= 0 && $0.space.isFinite && $0.pitch.isFinite && ($0.echo?.isFinite ?? true) }) else { return nil }
        p.keys.sort { $0.beat < $1.beat }
        p.expression = min(1, max(0, p.expression))
        return p
    }

    func axes(at beat: Double) -> (x: Float, y: Float) {
        guard let first = keys.first, let last = keys.last else { return (0,0) }
        // Echo and space are opposite halves of the physical slide. A scored
        // echo throw takes the positive side; absent echo preserves old scores.
        func axes(_ k: Key) -> (Float, Float) {
            let echo = min(1,max(0,k.echo ?? 0))
            return (echo > 0 ? echo : -min(1,max(0,k.space)), min(24,max(-24,k.pitch))/12)
        }
        if beat <= first.beat { return axes(first) }
        if beat >= last.beat { return axes(last) }
        let i = keys.lastIndex { $0.beat <= beat }!
        let a = keys[i], b = keys[i+1]
        let t = min(1,max(0,(beat-a.beat)/max(0.001,b.beat-a.beat)))
        let u = Float(t*t*(3-2*t)), aa = axes(a), bb = axes(b)
        return (aa.0+(bb.0-aa.0)*u, aa.1+(bb.1-aa.1)*u)
    }
}

final class SingerPerformancePlayer {
    private var timer: Timer?
    func stop() { timer?.invalidate(); timer = nil }
    func play(_ score: SingerPerformance, epoch: Double, bpm: Double, apply: @escaping (Float,Float) -> Void) {
        stop()
        guard bpm > 0 && bpm.isFinite else { return }
        let timer = Timer(timeInterval: 1.0/60, repeats: true) { [weak self] timer in
            let beat = (Date().timeIntervalSince1970-epoch)*bpm/60
            let a = score.axes(at: beat)
            apply(a.x,a.y)
            if beat >= (score.keys.last?.beat ?? 0) { timer.invalidate(); self?.timer = nil }
        }
        self.timer = timer
        RunLoop.main.add(timer, forMode: .common)
        timer.fire()
    }
}

/// Post-normalization phrasing: relative syllable accents and long-note
/// diminuendos. Twelve-millisecond smoothstep ramps avoid gain discontinuities.
struct SingerDynamics {
    struct Note { let start: Double; let duration: Double; let gain: Float }
    let notes: [Note]
    init(notation: String, bpm: Double, gains: [Float], offset: Double) {
        var beat = 0.0, index = 0, result: [Note] = []
        for token in notation.split(separator: ",") {
            let pair = token.split(separator: ":")
            let duration = pair.count > 1 ? Double(pair[1]) ?? 1 : 1
            if pair.first != "r" {
                let gain = index < gains.count && gains[index].isFinite ? min(1,max(0,gains[index])) : 1
                result.append(Note(start:beat*60/bpm-offset,duration:duration*60/bpm,gain:gain))
                index += 1
            }
            beat += duration
        }
        notes = result
    }
    func gain(at time: Double) -> Float {
        guard let first = notes.first else { return 1 }
        guard let index = notes.lastIndex(where: { $0.start <= time }) else { return first.gain }
        let note = notes[index], age = max(0,time-note.start)
        func held(_ note: Note, _ age: Double) -> Float {
            guard note.duration > 1.2 else { return note.gain }
            let t = min(1,max(0,(age/note.duration-0.22)/0.78))
            return note.gain * Float(1-0.55*t*t*(3-2*t))
        }
        let target = held(note,age)
        guard index > 0 && age < 0.012 else { return target }
        let previous = notes[index-1]
        let before = held(previous,note.start-previous.start)
        let t = Float(age/0.012), u = t*t*(3-2*t)
        return before+(target-before)*u
    }
}
