import Foundation

/// USB DMX stage-light lane — mirrors whatever the synth sounds onto a room
/// fixture, notepat's pitch-class palette roomward. The light model is the
/// AC OS npscore one (fedac/native/pieces/npscore.mjs): each note-on is a
/// pulse that decays from the strike, weighted average picks the color,
/// summed activity sets the level — the room carries the music's dynamic,
/// not a held average. Silent no-op until a USB DMX serial widget appears;
/// unplug/replug self-heals.
final class DMXOut {
    static let shared = DMXOut()

    /// One pulse per note-on, like an npscore light cue.
    private struct Pulse {
        let r: Double, g: Double, b: Double
        let gain: Double   // velocity / 127
        let start: Double  // systemUptime at note-on
    }

    // Notepat ROYGBIV by pitch class (NoteColors.base, 0=C..11=B). Sharps
    // are deliberately black — naturals carry the chromatic identity, so a
    // sharp dims the blend the way it reads as a gap on the strip.
    private static let palette: [(Double, Double, Double)] = [
        (255, 50, 50), (0, 0, 0), (255, 160, 0), (0, 0, 0),
        (255, 230, 0), (50, 200, 50), (0, 0, 0), (50, 120, 255),
        (0, 0, 0), (130, 50, 200), (0, 0, 0), (180, 80, 255),
    ]

    /// Pulse window in seconds — the e^-3 envelope dies to ~5% here, where
    /// npscore cuts the cue.
    private static let window = 0.8

    // House fixture: rgb on DMX channels 510/511/512 → slot indices 509–511
    // of the 512-byte universe (display d.510, 3-channel mode).
    private static let rgbSlot = 509

    private let q = DispatchQueue(label: "menuband.dmx")
    private var timer: DispatchSourceTimer?
    private var pulses: [Pulse] = []
    private var fd: Int32 = -1
    private var lastSent = (-1, -1, -1)
    private var lastSendAt = 0.0
    private var lastOpenAttempt = 0.0

    func noteOn(midi: Int, velocity: Int) {
        q.async {
            self.ensureRunning()
            let c = Self.palette[((midi % 12) + 12) % 12]
            self.pulses.append(Pulse(
                r: c.0, g: c.1, b: c.2,
                gain: Double(max(0, min(127, velocity))) / 127.0,
                start: ProcessInfo.processInfo.systemUptime))
        }
    }

    /// Release is silent roomward: the pulse model fades from note-on, so a
    /// lift never cuts the light. Kept as the paired hook so call sites read
    /// on/off and a hold-style fixture model can slot in later.
    func noteOff(midi: Int) {}

    func blackout() {
        q.async {
            self.pulses.removeAll()
            if self.fd >= 0 { self.send(0, 0, 0) }
        }
    }

    // MARK: - Tick (on q)

    /// ~30Hz — cheap enough to leave running once the first note arrives;
    /// the widget wants a periodic refresh anyway (1s keepalive).
    private func ensureRunning() {
        guard timer == nil else { return }
        let t = DispatchSource.makeTimerSource(queue: q)
        t.schedule(deadline: .now(), repeating: .milliseconds(33))
        t.setEventHandler { [weak self] in self?.tick() }
        t.resume()
        timer = t
    }

    private func tick() {
        let now = ProcessInfo.processInfo.systemUptime
        if fd < 0 {
            // Quiet hot-plug retry — no widget is the normal case.
            guard now - lastOpenAttempt > 3 else { return }
            lastOpenAttempt = now
            openWidget()
            guard fd >= 0 else { return }
        }
        pulses.removeAll { now - $0.start >= Self.window }
        var r = 0.0, g = 0.0, b = 0.0, w = 0.0
        for p in pulses {
            let k = p.gain * exp(-3 * (now - p.start) / Self.window)
            r += p.r * k; g += p.g * k; b += p.b * k; w += k
        }
        // Weighted average picks the COLOR; summed activity sets the LEVEL.
        // Normalizing alone would cancel the envelopes (one dying note would
        // still render full-bright) — scaling by min(1, w) is what lets
        // rests and decays actually reach black.
        if w > 0 {
            let lvl = min(1, w)
            r = r / w * lvl; g = g / w * lvl; b = b / w * lvl
        }
        let R = min(255, Int(r.rounded())), G = min(255, Int(g.rounded())),
            B = min(255, Int(b.rounded()))
        if (R, G, B) != lastSent || now - lastSendAt > 1 { send(R, G, B) }
    }

    // MARK: - Wire (on q)

    private func send(_ r: Int, _ g: Int, _ b: Int) {
        guard fd >= 0 else { return }
        var slots = [UInt8](repeating: 0, count: 512)
        slots[Self.rgbSlot] = UInt8(r)
        slots[Self.rgbSlot + 1] = UInt8(g)
        slots[Self.rgbSlot + 2] = UInt8(b)
        let pkt = Self.frame(slots)
        let n = pkt.withUnsafeBytes { write(fd, $0.baseAddress, $0.count) }
        if n == pkt.count {
            lastSent = (r, g, b)
            lastSendAt = ProcessInfo.processInfo.systemUptime
        } else {
            // Unplugged mid-frame — close so the tick's retry reopens.
            close(fd)
            fd = -1
        }
    }

    /// Enttec DMX USB Pro "send DMX" packet — the one place the wire framing
    /// lives (matches the house DMXking DMX USB PRO), so a different cable's
    /// protocol can swap in here.
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
            tcsetattr(f, TCSANOW, &t)
        }
        fd = f
        lastSent = (-1, -1, -1)  // force a fresh frame to the new widget
        NSLog("MenuBand DMX: opened \(path)")
    }

    /// First USB serial widget wins — covers FTDI (usbserial), Silicon Labs
    /// (SLAB), and WCH (wchusbserial) bridges.
    private static func discoverWidget() -> String? {
        for pattern in ["/dev/cu.usbserial*", "/dev/cu.SLAB_USBtoUART*", "/dev/cu.wchusbserial*"] {
            var g = glob_t()
            defer { globfree(&g) }
            if glob(pattern, 0, nil, &g) == 0, g.gl_pathc > 0, let c = g.gl_pathv[0] {
                return String(cString: c)
            }
        }
        return nil
    }
}
