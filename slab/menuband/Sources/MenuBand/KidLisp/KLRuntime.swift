// Top-level facade. Two call shapes:
//
//   • One-shot (CLI / conformance):
//         let fb = KLRuntime.render(source: "(wipe blue)", width: 64, height: 32)
//
//   • Persistent (live preview surfaces like the popover TV):
//         let ev = KLEvaluator(fb: persistentFB)
//         ev.runFrame(source)  // every tick
//
// `seed` lets conformance tests pin the RNG so frames are reproducible.

import Foundation

enum KLRuntime {
    static func render(source: String,
                       width: Int,
                       height: Int,
                       frame: Int = 0,
                       seed: UInt64 = 0xC0FFEE_BABE_F00D) -> KLFramebuffer {
        let fb = KLFramebuffer(width: width, height: height)
        let ev = KLEvaluator(fb: fb, seed: seed)
        // Catch up the frame counter for callers that want a specific
        // frame value visible via the `frame` magic var. Runs no work —
        // just advances the counter without re-evaluating the source.
        for _ in 0..<frame { ev.frame &+= 1 }
        ev.runFrame(source)
        return fb
    }
}
