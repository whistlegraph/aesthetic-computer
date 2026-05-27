// Seedable PCG32 — gives deterministic conformance frames.

import Foundation

struct KLRNG {
    private var state: UInt64
    private let inc: UInt64

    init(seed: UInt64 = 0x4d595df4d0f33173) {
        self.state = 0
        self.inc = (seed << 1) | 1
        _ = nextUInt32()
        self.state &+= seed
        _ = nextUInt32()
    }

    mutating func nextUInt32() -> UInt32 {
        let old = state
        state = old &* 6364136223846793005 &+ inc
        let xorshifted = UInt32(truncatingIfNeeded: ((old >> 18) ^ old) >> 27)
        let rot = UInt32(truncatingIfNeeded: old >> 59)
        return (xorshifted >> rot) | (xorshifted << ((0 &- rot) & 31))
    }

    mutating func nextDouble() -> Double {
        return Double(nextUInt32()) / Double(UInt32.max)
    }

    /// Integer in [low, high) — matches kidlisp.mjs `randIntRange(low, high)`.
    mutating func intRange(_ low: Int, _ high: Int) -> Int {
        guard high > low else { return low }
        return low + Int(nextUInt32() % UInt32(high - low))
    }
}
