import QuartzCore

/// Single source of truth for one recorded "take" — a flat list of
/// note events with wall-clock onset / release times. This replaces
/// the old arrangement where the authoritative buffer lived inside
/// the Verovio WKWebView (`notes[]` in sheet.html). Swift now owns
/// the recording; Verovio is only spun up on demand to engrave a PDF.
///
/// Times are in `CACurrentMediaTime()` base. `startTime` is captured
/// at the first onset so every consumer (the live staff, the PDF
/// exporter) can express positions relative to the take's beginning.
final class TapeScore {
    /// One note. `release == nil` means the key is still held — the
    /// live staff draws it growing to "now" until the off arrives.
    struct Event {
        let midi: UInt8
        let onset: TimeInterval
        var release: TimeInterval?
    }

    private(set) var events: [Event] = []
    /// midi → index into `events` for the currently-held note of that
    /// pitch, so a release lands O(1) on the right event.
    private var active: [UInt8: Int] = [:]

    /// Wall-clock time of the first onset in this take. 0 when empty.
    private(set) var startTime: TimeInterval = 0

    /// Fired on every mutation so the view can mark `needsDisplay`.
    var onChange: (() -> Void)?

    var isEmpty: Bool { events.isEmpty }

    /// True while at least one note is still held (release == nil) —
    /// the live staff only needs to repaint/auto-follow while this is
    /// true, so the 30 Hz timer can idle otherwise.
    var hasHeldNote: Bool { !active.isEmpty }

    // MARK: - Recording

    func noteOn(_ midi: UInt8, at t: TimeInterval = CACurrentMediaTime()) {
        if events.isEmpty { startTime = t }
        // Defensive: if the same pitch is somehow already held (a
        // missed off), close the prior one at this onset so we never
        // leak a never-released event.
        if let prior = active[midi], prior < events.count,
           events[prior].release == nil {
            events[prior].release = t
        }
        events.append(Event(midi: midi, onset: t, release: nil))
        active[midi] = events.count - 1
        onChange?()
    }

    func noteOff(_ midi: UInt8, at t: TimeInterval = CACurrentMediaTime()) {
        guard let idx = active[midi], idx < events.count else { return }
        events[idx].release = t
        active[midi] = nil
        onChange?()
    }

    func clear() {
        events.removeAll()
        active.removeAll()
        startTime = 0
        onChange?()
    }

    // MARK: - Read models

    /// Seconds from `startTime` to the most recent note ONSET. Drives
    /// the staff's auto-scroll so a fresh attack pulls the viewport
    /// along while a sustained note leaves it still.
    var lastOnsetSeconds: TimeInterval {
        guard let last = events.last else { return 0 }
        return max(0, last.onset - startTime)
    }

    /// Seconds from `startTime` to the end of the take. Held notes
    /// (release == nil) extend to `now`. Returns 0 for an empty take.
    func endSeconds(now: TimeInterval = CACurrentMediaTime()) -> TimeInterval {
        guard !events.isEmpty else { return 0 }
        var end: TimeInterval = 0
        for e in events {
            let off = e.release ?? now
            end = max(end, off - startTime)
        }
        return max(0, end)
    }

    /// Onset/offset of every event in absolute `CACurrentMediaTime()`
    /// base (held notes use `now` for their offset). The beat-grid
    /// renderer converts these to metronome-relative beats so notes
    /// land on a real bar/beat grid instead of guessed wall-clock x.
    func absoluteEvents(now: TimeInterval = CACurrentMediaTime())
        -> [(midi: UInt8, on: TimeInterval, off: TimeInterval)] {
        events.map { e in
            (midi: e.midi, on: e.onset, off: e.release ?? now)
        }
    }

    /// Onset/offset of every event relative to `startTime`. Held
    /// notes use `now` for their offset so the live staff can draw
    /// the growing bar. Used by `TapeStaffView`.
    func relativeEvents(now: TimeInterval = CACurrentMediaTime())
        -> [(midi: UInt8, on: TimeInterval, off: TimeInterval)] {
        guard !events.isEmpty else { return [] }
        return events.map { e in
            let on = e.onset - startTime
            let off = (e.release ?? now) - startTime
            return (midi: e.midi, on: max(0, on), off: max(on, off))
        }
    }

    /// JSON payload shaped for `Sheet.loadNotes(...)` in sheet.html:
    /// `[{pitch, onset, release}]` with seconds relative to the take
    /// start and `release: null` for notes still held at export time.
    /// Used by the on-demand PDF exporter.
    func loadNotesPayload(now: TimeInterval = CACurrentMediaTime()) -> String {
        guard !events.isEmpty else { return "[]" }
        let items: [[String: Any]] = events.map { e in
            var dict: [String: Any] = [
                "pitch": Int(e.midi),
                "onset": (e.onset - startTime),
            ]
            if let r = e.release {
                dict["release"] = (r - startTime)
            } else {
                dict["release"] = NSNull()
            }
            return dict
        }
        guard let data = try? JSONSerialization.data(withJSONObject: items),
              let str = String(data: data, encoding: .utf8) else { return "[]" }
        return str
    }
}
