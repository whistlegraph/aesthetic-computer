// Headless renderer for conformance testing.
//
// Usage (after `swift build -c release`):
//
//   ./MenuBand --kidlisp-render "<source>" <width> <height> <out.ppm> [seed]
//
// Writes a binary PPM (P6) at the given path. Designed to be called from
// kidlisp/conformance/ harness scripts to diff against reference frames
// rendered by kidlisp.mjs.

import Foundation

enum KLCLI {
    /// Returns true if the args contained --kidlisp-render and the run
    /// completed (caller should exit). Returns false to let normal app
    /// startup proceed.
    ///
    /// Single frame:
    ///   --kidlisp-render <source> <w> <h> <out.ppm> [seed]
    ///
    /// Multi-frame dump (persistent evaluator across frames — accumulates
    /// blur, advances timing tokens, etc.):
    ///   --kidlisp-render-frames <source> <w> <h> <frame_count> <out_dir>
    ///                           [seed] [frame_interval_ms]
    static func runIfRequested(_ args: [String]) -> Bool {
        if let idx = args.firstIndex(of: "--kidlisp-render-frames") {
            return runFrames(Array(args.dropFirst(idx + 1)))
        }
        guard let idx = args.firstIndex(of: "--kidlisp-render") else { return false }
        let rest = Array(args.dropFirst(idx + 1))
        guard rest.count >= 4 else {
            FileHandle.standardError.write(Data(
                "usage: --kidlisp-render <source> <width> <height> <out.ppm> [seed]\n".utf8))
            exit(2)
        }
        let source = rest[0]
        let width = Int(rest[1]) ?? 0
        let height = Int(rest[2]) ?? 0
        let outPath = rest[3]
        let seed: UInt64 = rest.count > 4 ? (UInt64(rest[4]) ?? 0xC0FFEE_BABE_F00D)
                                          : 0xC0FFEE_BABE_F00D
        guard width > 0, height > 0 else {
            FileHandle.standardError.write(Data("invalid width/height\n".utf8))
            exit(2)
        }
        let fb = KLRuntime.render(source: source,
                                  width: width,
                                  height: height,
                                  frame: 0,
                                  seed: seed)
        do {
            try fb.writePPM(to: URL(fileURLWithPath: outPath))
            FileHandle.standardOutput.write(Data(
                "wrote \(width)x\(height) → \(outPath)\n".utf8))
        } catch {
            FileHandle.standardError.write(Data(
                "ppm write failed: \(error)\n".utf8))
            exit(1)
        }
        return true
    }

    private static func runFrames(_ rest: [String]) -> Bool {
        guard rest.count >= 5 else {
            FileHandle.standardError.write(Data(
                "usage: --kidlisp-render-frames <source> <w> <h> <frames> <outDir> [seed] [interval_ms]\n".utf8))
            exit(2)
        }
        let source = rest[0]
        let width = Int(rest[1]) ?? 0
        let height = Int(rest[2]) ?? 0
        let frames = Int(rest[3]) ?? 0
        let outDir = rest[4]
        let seed: UInt64 = rest.count > 5 ? (UInt64(rest[5]) ?? 0xC0FFEE_BABE_F00D)
                                          : 0xC0FFEE_BABE_F00D
        // Each "frame" advances wall-clock time by this many ms — timing
        // tokens (1s..., 2s..., 0.5s) anchor on wall-clock, so we need to
        // simulate elapsed time between frames for deterministic dumps.
        // Default 67ms matches oven's production WebP frame interval.
        let intervalMs = rest.count > 6 ? (Double(rest[6]) ?? 67.0) : 67.0

        guard width > 0, height > 0, frames > 0 else {
            FileHandle.standardError.write(Data("invalid args\n".utf8))
            exit(2)
        }
        let fm = FileManager.default
        try? fm.createDirectory(atPath: outDir,
                                withIntermediateDirectories: true)

        let fb = KLFramebuffer(width: width, height: height)
        let ev = KLEvaluator(fb: fb, seed: seed)
        ev.simulatedClock = Date().timeIntervalSinceReferenceDate
        let step = intervalMs / 1000.0

        for i in 0..<frames {
            ev.runFrame(source)
            let url = URL(fileURLWithPath: outDir)
                .appendingPathComponent(String(format: "%04d.ppm", i + 1))
            do {
                try fb.writePPM(to: url)
            } catch {
                FileHandle.standardError.write(Data(
                    "ppm write failed at frame \(i + 1): \(error)\n".utf8))
                exit(1)
            }
            ev.simulatedClock! += step
        }
        FileHandle.standardOutput.write(Data(
            "wrote \(frames) frames @ \(width)x\(height) to \(outDir)\n".utf8))
        return true
    }
}
