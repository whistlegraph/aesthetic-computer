/// A reader may consume only frames the capture callback has published.
/// After an underrun, wait for a fresh cushion instead of moving the writer
/// into unwritten ring memory (which replays audio from the previous wrap).
struct MonitorReadCursor {
    struct Plan {
        let start: Int
        let count: Int
        let underrun: Bool
        let dropped: Bool
    }
    private(set) var recovering = false

    mutating func plan(read: Int, written: Int, frames: Int, lead: Int) -> Plan {
        let available = max(0, written - read)
        if recovering {
            guard available >= lead + frames else {
                return Plan(start: read, count: 0, underrun: false, dropped: false)
            }
            recovering = false
            // Shed any accumulated backlog once, returning to the target
            // cushion rather than preserving the whole recovery delay.
            return Plan(start: written - lead - frames, count: frames,
                        underrun: false, dropped: false)
        }
        if available < frames {
            recovering = true
            return Plan(start: read, count: available, underrun: true, dropped: false)
        }
        if available > lead * 3 + frames / 2 {
            // Keep a full output block even when the device changed to a
            // larger callback size than the requested lead.
            return Plan(start: written - max(lead, frames), count: frames,
                        underrun: false, dropped: true)
        }
        return Plan(start: read, count: frames, underrun: false, dropped: false)
    }
}
