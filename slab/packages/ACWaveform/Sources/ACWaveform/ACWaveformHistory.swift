import CoreGraphics
import Foundation

/// The strip's memory: timestamped min/max slices, oldest first. Pure, so the
/// geometry can be tested without a window.
public struct ACWaveformHistory {
    public struct Slice: Equatable {
        public var at: TimeInterval
        public var low: CGFloat
        public var high: CGFloat
        public var active: Bool { low != 0 || high != 0 }
    }

    public private(set) var slices: [Slice] = []

    public init() {}

    /// How long the strip remembers: eight beats — two bars of 4/4 — at `bpm`.
    /// At the default 120 BPM that is the desktop's four seconds.
    public static func span(bpm: Double, beats: Double = 8) -> TimeInterval {
        guard bpm.isFinite, bpm > 0 else { return 4 }
        return beats * 60 / bpm
    }

    public mutating func append(low: Double, high: Double, at: TimeInterval) {
        let clamp = { (v: Double) in CGFloat(v.isFinite ? max(-1, min(1, v)) : 0) }
        slices.append(Slice(at: at, low: clamp(low), high: clamp(high)))
    }

    public mutating func appendSilence(at: TimeInterval) {
        slices.append(Slice(at: at, low: 0, high: 0))
    }

    public mutating func prune(now: TimeInterval, span: TimeInterval) {
        if let first = slices.firstIndex(where: { now - $0.at < span }) {
            if first > 0 { slices.removeFirst(first) }
        } else {
            slices.removeAll()
        }
    }

    public mutating func removeAll() { slices.removeAll() }

    public var isSounding: Bool { slices.contains(where: \.active) }

    /// A closed outline in a top-left-origin space: time runs down the strip
    /// with the newest slice at the bottom edge, amplitude runs across it
    /// about the centre line. Down the low edges, back up the high ones.
    public func path(in size: CGSize, now: TimeInterval, span: TimeInterval) -> CGPath? {
        guard isSounding, size.width > 0, size.height > 0, span > 0, let first = slices.first else { return nil }
        let center = size.width / 2
        let reach = size.width / 2 * 13 / 16
        let y = { (slice: Slice) in size.height * CGFloat(1 - (now - slice.at) / span) }
        let path = CGMutablePath()
        path.move(to: CGPoint(x: center, y: y(first)))
        for slice in slices { path.addLine(to: CGPoint(x: center + slice.low * reach, y: y(slice))) }
        path.addLine(to: CGPoint(x: center, y: size.height))
        for slice in slices.reversed() { path.addLine(to: CGPoint(x: center + slice.high * reach, y: y(slice))) }
        path.closeSubpath()
        return path
    }
}
