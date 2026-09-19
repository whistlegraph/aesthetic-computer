import Foundation

/// USB DMX stage-light lane — the room plays along with the synth.
///
/// Two sources, routed to different fixtures:
///   • KEYS hold. A note lights its pitch-class colour the frame it lands
///     and stays lit until the key lifts (short release tail); chords blend.
///   • DRUMS flash. A TrackDrum hit is a strobe-shaped pulse — full for a
///     beat, then a fast fall — in a colour per drum. Low drums and high
///     drums go to different pars.
/// Weighted average picks each group's colour, summed activity its level.
///
/// The house rig is four 6-channel pars (from the start address: red,
/// green, blue, amber, white, uv — found with `ac-dmx-cli sweep`). Silent
/// no-op until a USB DMX serial widget appears; unplug/replug self-heals.
/// Bench the widget and fixtures from the shell with `ac-dmx-cli`
/// (slab/bin) — same framing, same port.
final class DMXOut {
    static let shared = DMXOut()

    // MARK: - Rig

    /// What a fixture's channels mean, from its start address.
    enum Layout {
        case rgb       // 3ch: r g b
        case rgbawuv   // 6ch: r g b amber white uv (the house LED pars)
    }

    /// Which source a fixture listens to.
    enum Role { case keys, drumsLow, drumsHigh }

    struct Fixture { let address: Int; let layout: Layout; let role: Role }  // 1-based DMX address

    /// The rig. Two pars share address 1 (they mirror each other until one
    /// is re-addressed on its display); one each at 7 and 13. Change a
    /// role here to re-route. (The older rgb fixture at d.510 is off this
    /// chain; adding it back means 518-byte frames — see `slotCount`.)
    static let fixtures = [
        Fixture(address: 1, layout: .rgbawuv, role: .keys),
        Fixture(address: 7, layout: .rgbawuv, role: .drumsLow),
        Fixture(address: 13, layout: .rgbawuv, role: .drumsHigh),
    ]

    /// Slots per frame: just far enough to cover the fixtures, never fewer
    /// than DMX's 24. Frame SIZE is what the widget's serial link can't
    /// keep up with (~9.5 KB/s, measured): 518-byte full-universe frames at
    /// 40 Hz back up in the kernel until a close() or open() of the port
    /// hangs for minutes (the 2026-09-18 wedge). This rig needs 30 bytes.
    private static let slotCount: Int = max(24, fixtures.map { f in
        f.address - 1 + (f.layout == .rgb ? 3 : 6)
    }.max() ?? 24)

    // MARK: - Colour

    /// A roomward colour in the par's own terms. Amber, white and uv are
    /// real LEDs on the house pars — far brighter than mixing them from
    /// rgb — and an rgb-only fixture folds them back in.
    struct Colour {
        var r = 0.0, g = 0.0, b = 0.0, a = 0.0, w = 0.0, uv = 0.0
        static let off = Colour()
    }

    // Notepat ROYGBIV by pitch class (NoteColors.base, 0=C..11=B), every
    // hue pushed so its brightest LED is a full 255. D is the amber LED
    // (its orange, only brighter). Sharps are black on the strip; roomward
    // they hit the WHITE LED, so the strip's gaps are the room's hardest
    // hits instead of holes in the blend.
    private static let keyPalette: [Colour] = [
        Colour(r: 255, g: 50, b: 50),   // C
        Colour(w: 255),                 // C#
        Colour(a: 255),                 // D
        Colour(w: 255),                 // D#
        Colour(r: 255, g: 230),         // E
        Colour(r: 64, g: 255, b: 64),   // F
        Colour(w: 255),                 // F#
        Colour(r: 50, g: 120, b: 255),  // G
        Colour(w: 255),                 // G#
        Colour(r: 166, g: 64, b: 255),  // A
        Colour(w: 255),                 // A#
        Colour(r: 180, g: 80, b: 255),  // B
    ]

    /// One colour per TrackDrum voice, and which par it lands on. Kick is
    /// the white slam; skins are warm; metal is uv and cool.
    private static func drumColour(_ d: MenuBandPercussion.Drum) -> (Colour, Role) {
        switch d {
        case .kick:      return (Colour(w: 255), .drumsLow)
        case .snare:     return (Colour(r: 255), .drumsLow)
        case .clap:      return (Colour(a: 255), .drumsLow)
        case .snap:      return (Colour(r: 255, g: 255), .drumsLow)
        case .cowbell:   return (Colour(r: 128, a: 255), .drumsLow)
        case .block:     return (Colour(g: 255), .drumsLow)
        case .crash:     return (Colour(w: 255, uv: 255), .drumsHigh)
        case .splash:    return (Colour(b: 255, w: 128), .drumsHigh)
        case .ride:      return (Colour(b: 128, uv: 255), .drumsHigh)
        case .hatClosed: return (Colour(uv: 255), .drumsHigh)
        case .hatOpen:   return (Colour(w: 100, uv: 255), .drumsHigh)
        case .tambo:     return (Colour(g: 255, b: 255), .drumsHigh)
        }
    }

    // MARK: - Envelopes (seconds)

    /// Key release tail after the lift — short, so a staccato line reads
    /// as separate lights, but not a hard cut.
    private static let keyRelease = 0.08

    /// Drum flash: full for `hold`, then e^-(t/tau), cut at `window`
    /// (~3% of full). Velocity never scales the strike — a half-bright
    /// flash reads as nothing on a small par.
    private static let flashHold = 0.05
    private static let flashTau = 0.07
    private static let flashWindow = 0.30

    /// Tick period. The widget refreshes the line at 40 Hz (see
    /// `ac-dmx-cli info`), so frames faster than that are thrown away.
    private static let tickMs = 25

    /// Seconds of silence after the last light dies before the lane lets go
    /// of the widget. The widget keeps transmitting its last frame (a black
    /// one — the port is left with DTR up), and a released port means
    /// `ac-dmx-cli` or anything else can drive the chain with nothing of
    /// ours blinking through. The next note reopens it.
    private static let idleRelease = 2.0

    // MARK: - State (all on q)

    private struct Held {          // a key
        let colour: Colour
        let gain: Double           // velocity / 127 — weights the colour mix only
        var liftedAt: Double?      // nil while the key is down
    }
    private struct Flash {         // a drum hit
        let colour: Colour
        let gain: Double
        let start: Double
        let role: Role
    }

    private let q = DispatchQueue(label: "menuband.dmx")
    private var timer: DispatchSourceTimer?
    private var held: [Int: Held] = [:]
    private var flashes: [Flash] = []
    private var fd: Int32 = -1
    private var lastSlots: [UInt8] = []
    private var lastSendAt = 0.0
    private var lastOpenAttempt = 0.0
    private var lastEventAt = 0.0

    // MARK: - Inputs

    func noteOn(midi: Int, velocity: Int) {
        q.async {
            let now = self.now()
            self.lastEventAt = now
            self.held[midi] = Held(
                colour: Self.keyPalette[((midi % 12) + 12) % 12],
                gain: Double(max(1, min(127, velocity))) / 127.0,
                liftedAt: nil)
            self.ensureRunning()
            self.tick()   // the attack goes out now, not up to a tick later
        }
    }

    func noteOff(midi: Int) {
        q.async {
            guard self.held[midi] != nil else { return }
            self.held[midi]?.liftedAt = self.now()
            self.lastEventAt = self.now()
        }
    }

    func drum(_ d: MenuBandPercussion.Drum, velocity: Int) {
        q.async {
            let now = self.now()
            self.lastEventAt = now
            let (colour, role) = Self.drumColour(d)
            self.flashes.append(Flash(
                colour: colour,
                gain: Double(max(1, min(127, velocity))) / 127.0,
                start: now, role: role))
            self.ensureRunning()
            self.tick()
        }
    }

    func blackout() {
        q.async {
            self.held.removeAll()
            self.flashes.removeAll()
            if self.fd >= 0 { self.send(Self.blackSlots()) }
        }
    }

    // MARK: - Tick (on q)

    private func now() -> Double { ProcessInfo.processInfo.systemUptime }

    /// 40 Hz to match the widget's line refresh — cheap enough to leave
    /// running while anything is lit.
    private func ensureRunning() {
        guard timer == nil else { return }
        let t = DispatchSource.makeTimerSource(queue: q)
        t.schedule(deadline: .now(), repeating: .milliseconds(Self.tickMs))
        t.setEventHandler { [weak self] in self?.tick() }
        t.resume()
        timer = t
    }

    private func tick() {
        let now = now()
        // Prune what has died.
        held = held.filter { _, h in
            guard let off = h.liftedAt else { return true }
            return now - off < Self.keyRelease * 4
        }
        flashes.removeAll { now - $0.start >= Self.flashWindow }
        if held.isEmpty && flashes.isEmpty && now - lastEventAt > Self.idleRelease {
            release()   // before any open — idle never wants the port
            return
        }
        if fd < 0 {
            // Quiet hot-plug retry — no widget is the normal case.
            guard now - lastOpenAttempt > 3 else { return }
            lastOpenAttempt = now
            openWidget()
            guard fd >= 0 else { return }
        }

        // Keys: gate while down, short exponential tail after the lift.
        var keys = Blend()
        for h in held.values {
            let k = h.liftedAt.map { exp(-(now - $0) / Self.keyRelease) } ?? 1.0
            keys.add(h.colour, weight: h.gain * k, level: k)
        }
        // Drums: strobe shape, per role.
        var low = Blend(), high = Blend()
        for f in flashes {
            let t = now - f.start
            let k = t < Self.flashHold ? 1.0 : exp(-(t - Self.flashHold) / Self.flashTau)
            if f.role == .drumsLow { low.add(f.colour, weight: f.gain * k, level: k) }
            else { high.add(f.colour, weight: f.gain * k, level: k) }
        }

        var slots = [UInt8](repeating: 0, count: Self.slotCount)
        for f in Self.fixtures {
            let c: Colour
            switch f.role {
            case .keys: c = keys.colour
            case .drumsLow: c = low.colour
            case .drumsHigh: c = high.colour
            }
            Self.paint(c, layout: f.layout, into: &slots, at: f.address - 1)
        }
        if slots != lastSlots || now - lastSendAt > 1 { send(slots) }
    }

    /// Weighted average picks the COLOUR; summed activity sets the LEVEL.
    /// Normalizing alone would cancel the envelopes (one dying note would
    /// still render full-bright) — scaling by min(1, level) is what lets
    /// rests and releases actually reach black.
    private struct Blend {
        private var sum = Colour(), w = 0.0, level = 0.0
        mutating func add(_ c: Colour, weight: Double, level l: Double) {
            sum.r += c.r * weight; sum.g += c.g * weight; sum.b += c.b * weight
            sum.a += c.a * weight; sum.w += c.w * weight; sum.uv += c.uv * weight
            w += weight; level += l
        }
        var colour: Colour {
            guard w > 0 else { return .off }
            let s = min(1, level) / w
            return Colour(r: sum.r * s, g: sum.g * s, b: sum.b * s,
                          a: sum.a * s, w: sum.w * s, uv: sum.uv * s)
        }
    }

    private static func paint(_ c: Colour, layout: Layout, into slots: inout [UInt8], at i: Int) {
        let u = { (v: Double) in UInt8(max(0, min(255, v.rounded()))) }
        switch layout {
        case .rgb:
            // No amber/white/uv LEDs: fold them into the three we have.
            slots[i] = u(max(c.r, c.w, c.a))
            slots[i + 1] = u(max(c.g, c.w, c.a * 0.6))
            slots[i + 2] = u(max(c.b, c.w, c.uv))
        case .rgbawuv:
            slots[i] = u(c.r); slots[i + 1] = u(c.g); slots[i + 2] = u(c.b)
            slots[i + 3] = u(c.a); slots[i + 4] = u(c.w); slots[i + 5] = u(c.uv)
        }
    }

    private static func blackSlots() -> [UInt8] { [UInt8](repeating: 0, count: slotCount) }

    /// Black frame, port closed, timer stopped — see `idleRelease`. The
    /// open-retry throttle is cleared so the next note reopens at once.
    private func release() {
        if lastSlots != Self.blackSlots() { send(Self.blackSlots()) }
        dropPort()
        lastOpenAttempt = 0
        timer?.cancel()
        timer = nil
    }

    /// Close without draining. close() on a tty waits for pending output,
    /// and if the widget's link is behind that wait can run for minutes —
    /// the dmx queue hangs and, worse, every later open() of the device in
    /// ANY process blocks in the kernel until it drains (seen 2026-09-18).
    /// Flushing first makes the close immediate.
    private func dropPort() {
        guard fd >= 0 else { return }
        tcflush(fd, TCIOFLUSH)
        close(fd)
        fd = -1
    }

    // MARK: - Wire (on q)

    private func send(_ slots: [UInt8]) {
        guard fd >= 0 else { return }
        let pkt = Self.frame(slots)
        let n = pkt.withUnsafeBytes { write(fd, $0.baseAddress, $0.count) }
        if n == pkt.count {
            lastSlots = slots
            lastSendAt = now()
        } else {
            // Unplugged mid-frame — drop so the tick's retry reopens.
            dropPort()
        }
    }

    /// Enttec DMX USB Pro "send DMX" packet — the one place the wire framing
    /// lives (matches the house DMXking DMX USB PRO), so a different cable's
    /// protocol can swap in here. Any slot count from 24 to 512 is legal.
    private static func frame(_ slots: [UInt8]) -> [UInt8] {
        // 7E 06 lenLo lenHi | 00 start code + slots | E7
        let payload = UInt16(slots.count + 1)
        var pkt: [UInt8] = [0x7E, 0x06, UInt8(payload & 0xFF), UInt8(payload >> 8), 0x00]
        pkt.append(contentsOf: slots)
        pkt.append(0xE7)
        return pkt
    }

    private func openWidget() {
        guard let path = Self.discoverWidget() else { return }
        let f = open(path, O_RDWR | O_NOCTTY | O_NONBLOCK)
        guard f >= 0 else { return }
        var t = termios()
        if tcgetattr(f, &t) == 0 {
            cfmakeraw(&t)                     // frame bytes must pass untouched
            cfsetspeed(&t, speed_t(B115200))  // Pro widgets ignore baud; set anyway
            t.c_cflag |= tcflag_t(CSTOPB)     // 8N2
            t.c_cflag &= ~tcflag_t(HUPCL)     // keep DTR up on close: the
                                              // widget stops transmitting
                                              // when it drops, and the pars
                                              // fall dark on lost signal
            tcsetattr(f, TCSANOW, &t)
        }
        fd = f
        lastSlots = []  // force a fresh frame to the new widget
        NSLog("MenuBand DMX: opened \(path)")
    }

    /// First USB serial widget wins — covers FTDI (usbserial), Silicon Labs
    /// (SLAB), and WCH (wchusbserial) bridges.
    private static func discoverWidget() -> String? {
        #if MAC_APP_STORE
        // The sandboxed store build has no serial-device entitlement, so the
        // room lane is a silent no-op there: never look for a widget.
        return nil
        #else
        for pattern in ["/dev/cu.usbserial*", "/dev/cu.SLAB_USBtoUART*", "/dev/cu.wchusbserial*"] {
            var g = glob_t()
            defer { globfree(&g) }
            if glob(pattern, 0, nil, &g) == 0, g.gl_pathc > 0, let c = g.gl_pathv[0] {
                return String(cString: c)
            }
        }
        return nil
        #endif
    }
}
