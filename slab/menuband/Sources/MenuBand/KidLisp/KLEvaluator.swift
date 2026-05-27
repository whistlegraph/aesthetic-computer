// Tree-walking evaluator for KidLisp.
//
// Conformance target: `KidLisp Decree '26: Core + RBP-26` (in progress).
// Heads: wipe, ink, line, blur, circle, scroll, spin, zoom, contrast,
// `?` random pick, timing tokens (`Ns`, `Ns...`, `Ns!`), `fade:` gradient.
// Magic vars: width/height/w/h/w/2/h/2/frame. Magic colors: rainbow.

import Foundation

final class KLEvaluator {
    let fb: KLFramebuffer
    var rng: KLRNG
    var frame: Int = 0

    /// Audio amplitude in (roughly) 0–10 scale, matching kidlisp.mjs's
    /// `globalDef.amp`. Owner pushes this in each tick from whichever
    /// source feeds the visualizer (synth output, mic, etc.). Pieces
    /// access via the magic var `amp` (or `mic`).
    var amp: Double = 0

    private var inkSet: Bool = false
    private var ink: KLInkSpec = .rgba(.white)

    // Timing state — persists across frames so `1s...` knows when it last
    // fired. Keyed by `head|args.description` (stable for a fixed source).
    private var timingLast: [String: TimeInterval] = [:]
    private var timingSeq: [String: Int] = [:]

    // Backdrop / `(once ...)` gating. A bare color at the top of a program
    // (e.g. `purple` in $bop) is shorthand for `(once (wipe purple))` per
    // kidlisp.mjs — it wipes the first frame only, then subsequent frames
    // accumulate on top. Keyed by the backdrop's color/expression.
    private var oncedBackdrops: Set<String> = []

    // Progressive blur accumulator — matches graph.mjs §blur. Each call to
    // `blur N` adds N/3 to the accumulator; the kernel only fires when
    // |accumulator| ≥ 0.5, then resets. Keeps `blur 5` from washing the
    // whole framebuffer flat after a few frames.
    private var blurAccumulator: Double = 0.0
    private let blurThreshold: Double = 0.5

    // Spin: fractional `steps` accumulate until we have a whole step, then
    // the vortex kernel fires (matches graph.mjs §spinBlockBased).
    private var spinAccumulator: Double = 0.0

    // Zoom: small zoom calls (|level - 1| < 0.1) accumulate; only fire
    // every ~0.05 of total delta. Large zooms apply immediately. Matches
    // graph.mjs §zoom.
    private var zoomAccumulator: Double = 0.0

    // For deterministic CLI frame dumps. When non-nil, timing tokens read
    // from this instead of Date() — the harness advances it by the
    // expected per-frame interval between runFrame() calls.
    var simulatedClock: TimeInterval? = nil

    init(fb: KLFramebuffer, seed: UInt64 = 0xC0FFEE_BABE_F00D) {
        self.fb = fb
        self.rng = KLRNG(seed: seed)
    }

    /// Run `source` once as a single frame. Increments the frame counter
    /// and clears per-frame state (ink) at entry.
    func runFrame(_ source: String) {
        inkSet = false
        ink = .rgba(.white)
        let tokens = KLLexer(source).tokenize()
        let ast = KLParser(tokens).parseProgram()
        _ = eval(ast)
        frame &+= 1
    }

    @discardableResult
    func run(_ program: KLExpr) -> Any? {
        return eval(program)
    }

    // MARK: - Evaluation

    @discardableResult
    private func eval(_ expr: KLExpr) -> Any? {
        switch expr {
        case .number(let d): return d
        case .symbol(let s):
            if let v = magicVar(s) { return v }
            return s
        case .program(let stmts):
            var last: Any? = nil
            for s in stmts { last = eval(s) }
            return last
        case .atom(let inner):
            if case .symbol(let name) = inner {
                if let c = resolveSymbolAsColor(name) {
                    onceBackdrop("color:\(name)") { fb.wipe(flattenColor(c)) }
                    return nil
                }
                if name.hasPrefix("fade:") {
                    onceBackdrop(name) { fb.wipeFade(parseFadeColors(name)) }
                    return nil
                }
            }
            return eval(inner)
        case .list(let items):
            guard let first = items.first, case .symbol(let head) = first else {
                return nil
            }
            return apply(head, Array(items.dropFirst()))
        case .bareForm(let head, let args):
            return apply(head, args)
        }
    }

    // MARK: - Magic vars

    private func magicVar(_ name: String) -> Double? {
        switch name {
        case "width", "w": return Double(fb.width)
        case "height", "h": return Double(fb.height)
        case "w/2": return Double(fb.width) / 2.0
        case "h/2": return Double(fb.height) / 2.0
        case "frame": return Double(frame)
        // Audio reactivity — `amp`/`mic`/`amplitude` all return the
        // same owner-supplied value. Mirrors kidlisp.mjs which
        // populates `amp`, `mic`, `amplitude` separately but in this
        // host we feed a single synth-output channel.
        case "amp", "mic", "amplitude": return amp
        default: return nil
        }
    }

    // MARK: - Dispatch

    private func apply(_ head: String, _ args: [KLExpr]) -> Any? {
        // `?` random pick: evaluate one of the args at random.
        if head == "?" {
            guard !args.isEmpty else { return nil }
            let idx = rng.intRange(0, args.count)
            return eval(args[idx])
        }
        // Timing tokens: `Ns`, `Ns...`, `Ns!`
        if let timing = parseTiming(head) {
            return applyTiming(timing, args: args)
        }
        // Fade: `fade:r-b-c-...` as a top-level bare command = implicit
        // fade backdrop (one-shot, like a bare color).
        if head.hasPrefix("fade:") {
            onceBackdrop(head) { self.fb.wipeFade(self.parseFadeColors(head)) }
            return nil
        }
        // Basic arithmetic — needed for `amp`-driven pieces to scale
        // the 0–10 value into framebuffer coordinates. Variadic to
        // match the lisp shape: `(+ a b c)`, `(* amp 6)`, `(- h/2 4)`.
        if let v = applyArith(head, args) { return v }

        switch head {
        case "wipe":
            if args.count == 1, case .symbol(let s) = args[0], s.hasPrefix("fade:") {
                fb.wipeFade(parseFadeColors(s))
                return nil
            }
            if let c = resolveColor(args) {
                fb.wipe(flattenColor(c))
            }
            return nil

        case "ink":
            if args.isEmpty {
                inkSet = false
            } else if let c = resolveColor(args) {
                ink = c
                inkSet = true
            }
            return nil

        case "line":
            applyInkIfNeeded()
            let (x0, y0, x1, y1) = lineEndpoints(args)
            fb.line(x0, y0, x1, y1, flattenColor(ink))
            return nil

        case "circle":
            applyInkIfNeeded()
            let x = Int(numericArg(args, 0) ?? Double(fb.width / 2))
            let y = Int(numericArg(args, 1) ?? Double(fb.height / 2))
            let r = Int(numericArg(args, 2) ?? 4)
            fb.circle(x, y, r, flattenColor(ink))
            return nil

        case "blur":
            // Strength scale is gentler than kidlisp.mjs's /3 — at the TV's
            // 192×120 res each blur radius covers less screen area, so we
            // can spend more accumulator per call without smearing flat.
            let strength = numericArg(args, 0) ?? 1
            blurAccumulator += strength / 1.5
            if abs(blurAccumulator) >= blurThreshold {
                let r = max(1, Int(abs(blurAccumulator)))
                fb.blur(radius: r)
                blurAccumulator = 0.0
            }
            return nil

        case "scroll":
            let dx = numericArg(args, 0) ?? 0
            let dy = numericArg(args, 1) ?? 0
            fb.scroll(dx: dx, dy: dy)
            return nil

        case "contrast":
            let f = numericArg(args, 0) ?? 1.0
            fb.contrast(factor: f)
            return nil

        case "spin":
            let steps = numericArg(args, 0) ?? 0
            if steps != 0 {
                spinAccumulator += steps
                let integerSteps = floor(spinAccumulator)
                spinAccumulator -= integerSteps
                if integerSteps != 0 {
                    fb.spinVortex(integerSteps: integerSteps)
                }
            }
            return nil

        case "zoom":
            let level = numericArg(args, 0) ?? 1.0
            if level != 1.0 {
                if abs(level - 1.0) >= 0.1 {
                    // Large change — apply immediately, reset accumulator.
                    zoomAccumulator = 0
                    fb.zoom(scale: level)
                } else {
                    // Small change — accumulate, fire on 0.05-step crossings.
                    zoomAccumulator += (level - 1.0)
                    let threshold = 0.05
                    if abs(zoomAccumulator) >= threshold {
                        let zoomDelta = (zoomAccumulator >= 0 ? 1.0 : -1.0) * threshold
                        let actual = 1.0 + zoomDelta
                        zoomAccumulator -= zoomDelta
                        fb.zoom(scale: actual)
                    }
                }
            }
            return nil

        default:
            // Implicit backdrop via a bare color name with no args — runs
            // once per evaluator (per kidlisp.mjs §backdrop semantics).
            if args.isEmpty, let c = KLColors.named(head) {
                onceBackdrop("color:\(head)") { self.fb.wipe(c) }
            }
            return nil
        }
    }

    private func onceBackdrop(_ key: String, _ body: () -> Void) {
        let full = "backdrop:" + key
        if oncedBackdrops.contains(full) { return }
        oncedBackdrops.insert(full)
        body()
    }

    /// Called when the playing source changes — clear once-set so the new
    /// program's backdrops fire on its first frame.
    func resetBackdrops() {
        oncedBackdrops.removeAll(keepingCapacity: true)
    }

    // MARK: - Timing

    private struct Timing {
        let seconds: Double
        let iterating: Bool   // `Ns...` cycles through args
        let bang: Bool        // `Ns!` (reserved)
    }

    private func parseTiming(_ head: String) -> Timing? {
        // Match `<seconds>s` optionally followed by `...` or `!`.
        guard head.last != ":" else { return nil }
        var idx = head.startIndex
        var numStr = ""
        while idx < head.endIndex, head[idx].isNumber || head[idx] == "." {
            numStr.append(head[idx]); idx = head.index(after: idx)
        }
        guard !numStr.isEmpty, idx < head.endIndex, head[idx] == "s" else { return nil }
        guard let secs = Double(numStr) else { return nil }
        let tail = head[head.index(after: idx)...]
        let iterating = tail == "..."
        let bang = tail == "!"
        if !tail.isEmpty && !iterating && !bang { return nil }
        return Timing(seconds: secs, iterating: iterating, bang: bang)
    }

    private func applyTiming(_ t: Timing, args: [KLExpr]) -> Any? {
        guard !args.isEmpty else { return nil }
        let now = simulatedClock ?? Date().timeIntervalSinceReferenceDate
        let key = "\(t.seconds)|\(t.iterating)|\(args.count)|\(stableArgsKey(args))"
        let last = timingLast[key] ?? (now - t.seconds)  // first call fires immediately
        let due = (now - last) >= t.seconds

        if t.iterating {
            if due {
                timingLast[key] = now
                timingSeq[key] = (timingSeq[key] ?? 0) &+ 1
            }
            let idx = (timingSeq[key] ?? 0) % args.count
            return eval(args[idx])
        } else {
            // One-shot: only eval body on a fresh tick. Between fires, do
            // nothing. Returns the last evaluated expr's value (matches
            // kidlisp.mjs `do { result = eval(a) } while args`).
            if due {
                timingLast[key] = now
                var result: Any? = nil
                for a in args { result = eval(a) }
                return result
            }
            return nil
        }
    }

    private func stableArgsKey(_ args: [KLExpr]) -> String {
        // Cheap structural hash — good enough for keying timing state
        // when the source string is stable across frames.
        var s = ""
        for a in args { s += "|" + shortDesc(a) }
        return s
    }
    private func shortDesc(_ e: KLExpr) -> String {
        switch e {
        case .number(let d): return "n\(d)"
        case .symbol(let s): return "s\(s)"
        case .list(let xs): return "(" + xs.map(shortDesc).joined(separator: " ") + ")"
        case .bareForm(let h, let xs): return "b\(h){" + xs.map(shortDesc).joined() + "}"
        case .atom(let i): return "a" + shortDesc(i)
        case .program(let xs): return "p[" + xs.map(shortDesc).joined() + "]"
        }
    }

    // MARK: - Ink + line

    private func applyInkIfNeeded() {
        if !inkSet {
            let r = UInt8(rng.intRange(0, 256))
            let g = UInt8(rng.intRange(0, 256))
            let b = UInt8(rng.intRange(0, 256))
            ink = .rgba(KLColor(r, g, b))
        }
    }

    private func lineEndpoints(_ args: [KLExpr]) -> (Int, Int, Int, Int) {
        if args.count >= 4 {
            let x0 = Int(numericArg(args, 0) ?? 0)
            let y0 = Int(numericArg(args, 1) ?? 0)
            let x1 = Int(numericArg(args, 2) ?? 0)
            let y1 = Int(numericArg(args, 3) ?? 0)
            return (x0, y0, x1, y1)
        }
        return (
            rng.intRange(0, fb.width),
            rng.intRange(0, fb.height),
            rng.intRange(0, fb.width),
            rng.intRange(0, fb.height)
        )
    }

    // MARK: - Numerics

    private func numericArg(_ args: [KLExpr], _ idx: Int) -> Double? {
        guard idx < args.count else { return nil }
        let v = eval(args[idx])
        if let d = v as? Double { return d }
        if let s = v as? String, let d = Double(s) { return d }
        return nil
    }

    /// Evaluate +, -, *, / as variadic numeric folds. Returns nil for
    /// any other head so the main dispatch falls through unchanged.
    /// Single-arg `-` negates (`(- amp)` = `-amp`); single-arg `/`
    /// reciprocates (`(/ 2)` = `0.5`).
    private func applyArith(_ head: String, _ args: [KLExpr]) -> Double? {
        switch head {
        case "+":
            var s: Double = 0
            for i in 0..<args.count { s += numericArg(args, i) ?? 0 }
            return s
        case "-":
            if args.isEmpty { return 0 }
            if args.count == 1 { return -(numericArg(args, 0) ?? 0) }
            var v = numericArg(args, 0) ?? 0
            for i in 1..<args.count { v -= numericArg(args, i) ?? 0 }
            return v
        case "*":
            var p: Double = 1
            for i in 0..<args.count { p *= numericArg(args, i) ?? 1 }
            return p
        case "/":
            if args.isEmpty { return 0 }
            if args.count == 1 {
                let d = numericArg(args, 0) ?? 0
                return d == 0 ? 0 : 1.0 / d
            }
            var v = numericArg(args, 0) ?? 0
            for i in 1..<args.count {
                let d = numericArg(args, i) ?? 1
                v = d == 0 ? 0 : v / d
            }
            return v
        default:
            return nil
        }
    }

    // MARK: - Color resolution

    /// Resolve args list to an ink spec. Recognized forms:
    ///   • named color: `red` / `purple` / `cyan` ...
    ///   • named color + alpha: `red 64` / `(? rainbow white 0) 8`
    ///   • r g b [a]: `255 0 128` / `255 0 128 64`
    ///   • rainbow magic color (frame-driven)
    ///   • `fade:r-b-c-...` gradient
    ///   • a number alone: treated as grayscale 0..255 (`0` = black)
    private func resolveColor(_ args: [KLExpr]) -> KLInkSpec? {
        if args.count >= 3 {
            let r = UInt8(clamping: Int(numericArg(args, 0) ?? 0))
            let g = UInt8(clamping: Int(numericArg(args, 1) ?? 0))
            let b = UInt8(clamping: Int(numericArg(args, 2) ?? 0))
            let a = UInt8(clamping: Int(numericArg(args, 3) ?? 255))
            return .rgba(KLColor(r, g, b, a))
        }
        guard !args.isEmpty else { return nil }

        // Evaluate the first arg — may be a list/`?`/timing token resolving
        // to a symbol or number.
        let v0 = eval(args[0])
        let alphaArg: UInt8? = args.count >= 2
            ? UInt8(clamping: Int(numericArg(args, 1) ?? 255))
            : nil

        if let name = v0 as? String {
            if let c = resolveSymbolAsColor(name) {
                if let a = alphaArg { return withAlpha(c, a) }
                return c
            }
            if name.hasPrefix("fade:") {
                return .fade(parseFadeColors(name))
            }
        }
        if let d = v0 as? Double {
            let g = UInt8(clamping: Int(d))
            let a = alphaArg ?? 255
            return .rgba(KLColor(g, g, g, a))
        }
        return nil
    }

    private func resolveSymbolAsColor(_ name: String) -> KLInkSpec? {
        if name == "rainbow" {
            return .rgba(rainbowAt(frame: frame))
        }
        if let c = KLColors.named(name) {
            return .rgba(c)
        }
        return nil
    }

    private func withAlpha(_ spec: KLInkSpec, _ a: UInt8) -> KLInkSpec {
        switch spec {
        case .rgba(let c): return .rgba(KLColor(c.r, c.g, c.b, a))
        case .fade(let cs):
            return .fade(cs.map { KLColor($0.r, $0.g, $0.b, a) })
        }
    }

    private func flattenColor(_ spec: KLInkSpec) -> KLColor {
        // Strokes use the first stop of fade as a fallback — proper
        // gradient strokes land when we wire fade into draw primitives.
        switch spec {
        case .rgba(let c): return c
        case .fade(let cs): return cs.first ?? .white
        }
    }

    private func parseFadeColors(_ raw: String) -> [KLColor] {
        // raw is `fade:red-blue-black-blue-red` or `fade:red-blue:frame` —
        // we ignore the trailing `:frame` (animation suffix) for now and
        // just take the first two segments delimited by `:`.
        var s = raw
        if s.hasPrefix("fade:") { s.removeFirst("fade:".count) }
        if let colon = s.firstIndex(of: ":") { s = String(s[..<colon]) }
        let names = s.split(separator: "-").map { String($0) }
        var out: [KLColor] = []
        for n in names {
            if n == "rainbow" {
                out.append(rainbowAt(frame: frame))
            } else if let c = KLColors.named(n) {
                out.append(c)
            } else if let d = Double(n) {
                let g = UInt8(clamping: Int(d))
                out.append(KLColor(g, g, g, 255))
            }
        }
        if out.isEmpty { out = [.black, .white] }
        return out
    }

    private func rainbowAt(frame f: Int) -> KLColor {
        // Slow hue sweep — full cycle every ~180 frames (≈6 sec @30fps).
        let hue = (Double(f).truncatingRemainder(dividingBy: 180.0)) / 180.0
        let (r, g, b) = hsvToRgb(h: hue, s: 1.0, v: 1.0)
        return KLColor(UInt8(r * 255), UInt8(g * 255), UInt8(b * 255))
    }

    private func hsvToRgb(h: Double, s: Double, v: Double) -> (Double, Double, Double) {
        let i = floor(h * 6)
        let f = h * 6 - i
        let p = v * (1 - s)
        let q = v * (1 - f * s)
        let t = v * (1 - (1 - f) * s)
        switch Int(i.truncatingRemainder(dividingBy: 6)) {
        case 0: return (v, t, p)
        case 1: return (q, v, p)
        case 2: return (p, v, t)
        case 3: return (p, q, v)
        case 4: return (t, p, v)
        default: return (v, p, q)
        }
    }
}

/// Ink can be either a flat RGBA or a fade gradient. Fade applies to
/// wipes today; future work threads it through draw primitives too.
enum KLInkSpec {
    case rgba(KLColor)
    case fade([KLColor])
}
