import Foundation
import AudioToolbox
import AppKit

/// Loads a Standard MIDI File (.mid / .midi) and replays its note
/// events through the host's note-routing callbacks at the file's
/// authored tempo. Intended for the "drag a MIDI file onto the
/// menubar piano and watch it play itself" workflow — events route
/// through the same `startTapNote` / `stopTapNote` pipeline the
/// physical piano uses, so the popover staff + the menubar icon
/// all light up as the song plays.
final class MidiFilePlayer {

    typealias NoteOnHandler = (_ midi: UInt8, _ velocity: UInt8) -> Void
    typealias NoteOffHandler = (_ midi: UInt8) -> Void
    typealias FinishHandler = () -> Void

    /// Single in-flight playback so dropping a second file
    /// supersedes the first instead of stacking events.
    private static var current: MidiFilePlayer?

    private struct Event {
        let time: TimeInterval   // seconds from start
        let midi: UInt8
        let velocity: UInt8
        let isOn: Bool
    }

    private var pending: [DispatchWorkItem] = []
    private var heldNotes: Set<UInt8> = []
    private weak var onNoteOn: AnyObject?  // satisfy ARC; kept indirectly via closures
    private var onFinish: FinishHandler?

    /// Stop any in-flight playback and start the supplied file.
    /// `onNoteOn` and `onNoteOff` are called on the main queue at
    /// each scheduled event boundary.
    static func play(url: URL,
                     onNoteOn: @escaping NoteOnHandler,
                     onNoteOff: @escaping NoteOffHandler,
                     onFinish: @escaping FinishHandler) -> Bool {
        current?.cancel()
        let player = MidiFilePlayer()
        guard player.start(url: url, onNoteOn: onNoteOn,
                            onNoteOff: onNoteOff, onFinish: onFinish) else {
            return false
        }
        current = player
        return true
    }

    static func stop() {
        current?.cancel()
        current = nil
    }

    private func start(url: URL,
                       onNoteOn: @escaping NoteOnHandler,
                       onNoteOff: @escaping NoteOffHandler,
                       onFinish: @escaping FinishHandler) -> Bool {
        guard let events = Self.loadEvents(url: url), !events.isEmpty else {
            NSLog("MidiFilePlayer: no playable events in \(url.lastPathComponent)")
            return false
        }
        self.onFinish = onFinish
        // Schedule each event independently — small file sizes
        // (few thousand events) keep DispatchQueue.main per-event
        // overhead cheap, and main-queue precision is fine for the
        // ~10ms accuracy a player-piano feel needs.
        let lastTime = events.last?.time ?? 0
        for event in events {
            let work = DispatchWorkItem { [weak self] in
                guard let self = self else { return }
                if event.isOn {
                    self.heldNotes.insert(event.midi)
                    onNoteOn(event.midi, event.velocity)
                } else {
                    self.heldNotes.remove(event.midi)
                    onNoteOff(event.midi)
                }
            }
            pending.append(work)
            DispatchQueue.main.asyncAfter(deadline: .now() + event.time,
                                          execute: work)
        }
        // Finish marker fires shortly after the last note-off so
        // any UI tied to "playback running" can clear itself.
        let finishWork = DispatchWorkItem { [weak self] in
            self?.releaseStuckNotes(onNoteOff: onNoteOff)
            self?.onFinish?()
            if MidiFilePlayer.current === self {
                MidiFilePlayer.current = nil
            }
        }
        pending.append(finishWork)
        DispatchQueue.main.asyncAfter(deadline: .now() + lastTime + 0.05,
                                      execute: finishWork)
        NSLog("MidiFilePlayer: scheduled \(events.count) events over \(lastTime)s from \(url.lastPathComponent)")
        return true
    }

    private func cancel() {
        for w in pending { w.cancel() }
        pending.removeAll()
        // Best-effort note-off for anything still held — caller
        // captured the off-handler in their per-event closures, but
        // those have been cancelled, so we can't reach the handler
        // here. The host should call its own panic() after stop()
        // when superseding playback.
    }

    private func releaseStuckNotes(onNoteOff: NoteOffHandler) {
        for midi in heldNotes {
            onNoteOff(midi)
        }
        heldNotes.removeAll()
    }

    // MARK: - File loading via MusicSequence

    private static func loadEvents(url: URL) -> [Event]? {
        var seq: MusicSequence?
        let newStatus = NewMusicSequence(&seq)
        guard newStatus == noErr, let sequence = seq else {
            NSLog("MidiFilePlayer: NewMusicSequence failed (\(newStatus))")
            return nil
        }
        defer { DisposeMusicSequence(sequence) }
        let loadStatus = MusicSequenceFileLoad(sequence, url as CFURL, .midiType, [])
        guard loadStatus == noErr else {
            NSLog("MidiFilePlayer: MusicSequenceFileLoad failed (\(loadStatus)) for \(url.path)")
            return nil
        }

        var trackCount: UInt32 = 0
        guard MusicSequenceGetTrackCount(sequence, &trackCount) == noErr,
              trackCount > 0 else {
            return nil
        }

        var events: [Event] = []
        for trackIdx in 0..<trackCount {
            var track: MusicTrack?
            guard MusicSequenceGetIndTrack(sequence, trackIdx, &track) == noErr,
                  let t = track else { continue }
            var iterator: MusicEventIterator?
            guard NewMusicEventIterator(t, &iterator) == noErr,
                  let it = iterator else { continue }
            defer { DisposeMusicEventIterator(it) }

            var hasEvent: DarwinBoolean = false
            MusicEventIteratorHasCurrentEvent(it, &hasEvent)
            while hasEvent.boolValue {
                var beat: MusicTimeStamp = 0
                var eventType: MusicEventType = 0
                var rawData: UnsafeRawPointer?
                var dataSize: UInt32 = 0
                MusicEventIteratorGetEventInfo(it, &beat, &eventType,
                                                &rawData, &dataSize)
                if eventType == kMusicEventType_MIDINoteMessage,
                   let raw = rawData {
                    let msg = raw.bindMemory(to: MIDINoteMessage.self,
                                              capacity: 1).pointee
                    var onSeconds: TimeInterval = 0
                    var offSeconds: TimeInterval = 0
                    MusicSequenceGetSecondsForBeats(sequence, beat,
                                                     &onSeconds)
                    MusicSequenceGetSecondsForBeats(sequence,
                                                     beat + Float64(msg.duration),
                                                     &offSeconds)
                    events.append(Event(time: onSeconds, midi: msg.note,
                                          velocity: msg.velocity, isOn: true))
                    events.append(Event(time: offSeconds, midi: msg.note,
                                          velocity: 0, isOn: false))
                }
                MusicEventIteratorNextEvent(it)
                MusicEventIteratorHasCurrentEvent(it, &hasEvent)
            }
        }

        // Sort by time so simultaneous off+on ordering is stable
        // (offs first when ties, so retriggers don't immediately
        // chop themselves).
        events.sort { lhs, rhs in
            if lhs.time != rhs.time { return lhs.time < rhs.time }
            return !lhs.isOn && rhs.isOn
        }
        // Dump first few events so a noteOn-only summary lands in
        // the log — invaluable for diagnosing PDF-drag playback
        // (octaves wrong / tempo off / etc) without hooking up a
        // MIDI monitor. Only the first 8 ons.
        let firstOns = events.filter(\.isOn).prefix(8)
        let summary = firstOns
            .map { String(format: "%.2fs:%d", $0.time, Int($0.midi)) }
            .joined(separator: ", ")
        NSLog("MidiFilePlayer: first onsets — \(summary)")
        return events
    }
}
