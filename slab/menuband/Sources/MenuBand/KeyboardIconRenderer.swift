import AppKit

// Tiny piano rendered into a single menubar status item with a flat
// text-only header. Layout (left to right):
//
//   [pad][picker][gap][piano keys (A3..D6, A3 hidden)][gap][MIDI][pad]
//
// Hit-testing returns .toggleMIDI / .pickInstrument / .note(midi) so the
// drag-and-tap interaction in AppDelegate can route accordingly. Header
// buttons are flat (no fill, no border) so they read like native menubar
// text. Keys are skeuomorphic: white gradient + dark-accent black gradient.
enum KeyboardIconRenderer {
    private static let pianoWaveformKeyHeightScale: CGFloat = 2.4

    enum Layout {
        case fixedCanvas
        case tightActiveRange
    }

    /// ROYGBIV note colors for the natural notes (C → red ... B → violet),
    /// keyed by MIDI pitch class. Mirrors `getNoteColorForOctave` in
    /// system/public/aesthetic.computer/lib/note-colors.mjs so the menu
    /// band's chromatic stripe reads the same as notepat's mini-piano.
    /// Sharps/flats return nil — the stripe only paints under naturals.
    private static let chromaticColorByPitchClass: [Int: NSColor] = [
        0:  NSColor(srgbRed: 255/255, green:  50/255, blue:  50/255, alpha: 1),  // C
        2:  NSColor(srgbRed: 255/255, green: 160/255, blue:   0/255, alpha: 1),  // D
        4:  NSColor(srgbRed: 255/255, green: 230/255, blue:   0/255, alpha: 1),  // E
        5:  NSColor(srgbRed:  50/255, green: 200/255, blue:  50/255, alpha: 1),  // F
        7:  NSColor(srgbRed:  50/255, green: 120/255, blue: 255/255, alpha: 1),  // G
        9:  NSColor(srgbRed: 130/255, green:  50/255, blue: 200/255, alpha: 1),  // A
        11: NSColor(srgbRed: 180/255, green:  80/255, blue: 255/255, alpha: 1),  // B
    ]

    /// Drum-pad colors (port of PERCUSSION_COLORS in lib/percussion.mjs),
    /// indexed by pitch class. Painted over the right-hand keys while the
    /// percussion split is active so the drum zone reads at a glance.
    private static let percussionColorByPitchClass: [Int: NSColor] = [
        0:  NSColor(srgbRed: 220/255, green:  90/255, blue:  40/255, alpha: 1),  // kick
        1:  NSColor(srgbRed: 220/255, green: 150/255, blue: 240/255, alpha: 1),  // crash
        2:  NSColor(srgbRed: 220/255, green: 180/255, blue: 110/255, alpha: 1),  // snare
        3:  NSColor(srgbRed: 240/255, green: 160/255, blue: 220/255, alpha: 1),  // splash
        4:  NSColor(srgbRed: 240/255, green: 220/255, blue: 130/255, alpha: 1),  // clap
        5:  NSColor(srgbRed: 220/255, green: 240/255, blue: 140/255, alpha: 1),  // snap
        6:  NSColor(srgbRed: 200/255, green: 150/255, blue:  80/255, alpha: 1),  // cowbell
        7:  NSColor(srgbRed: 120/255, green: 220/255, blue: 180/255, alpha: 1),  // closed hat
        8:  NSColor(srgbRed: 190/255, green: 120/255, blue:  70/255, alpha: 1),  // block
        9:  NSColor(srgbRed: 120/255, green: 200/255, blue: 240/255, alpha: 1),  // open hat
        10: NSColor(srgbRed: 230/255, green: 210/255, blue: 170/255, alpha: 1),  // tambo
        11: NSColor(srgbRed: 180/255, green: 180/255, blue: 230/255, alpha: 1),  // ride
    ]

    /// Set by AppDelegate when either percussion side toggles. Each half of
    /// the board latches to drums independently (tap left-⌥ / right-⌥).
    static var percussionLeftActive: Bool = false
    static var percussionRightActive: Bool = false

    /// Whether the key at display-MIDI `m` is currently a drum — its half of
    /// the board has to be latched. Left half is `< lingerSplitMidi`.
    static func percussionActive(forMidi m: Int) -> Bool {
        m < MenuBandLayout.lingerSplitMidi ? percussionLeftActive : percussionRightActive
    }

    /// The drum-pad color to wash over a key, or nil when that key's half
    /// isn't latched to drums (so it's still a melodic key).
    static func percussionDrumColor(forMidi m: Int) -> NSColor? {
        guard percussionActive(forMidi: m) else { return nil }
        return percussionColorByPitchClass[((m % 12) + 12) % 12]
    }

    /// Per-pitch-class drum hit pulses (time + 0…1 level), pushed by the
    /// AppDelegate visualizer tick straight from the live percussion data.
    /// Drives the per-key shake + blink so the menubar keys physically
    /// "vibe" with each drum hit.
    static var drumPulses: [MenuBandPercussion.DrumPulse] = []
    static var drumPulseNow: Double = 0

    /// Shake offset + blink (0…1) for a right-hand drum key, derived from
    /// its most recent hit: a velocity-scaled damped buzz that rings down
    /// over ~0.4 s. Returns zeros for melodic keys / when the split is off.
    static func percussionVibe(forMidi m: Int) -> (dx: CGFloat, dy: CGFloat, blink: CGFloat) {
        guard percussionActive(forMidi: m),
              !drumPulses.isEmpty else { return (0, 0, 0) }
        let pc = ((m % 12) + 12) % 12
        guard pc < drumPulses.count else { return (0, 0, 0) }
        let p = drumPulses[pc]
        guard p.at > 0 else { return (0, 0, 0) }
        let elapsed = drumPulseNow - p.at
        guard elapsed >= 0, elapsed < 0.5 else { return (0, 0, 0) }
        let env = p.level * exp(-elapsed * 9.0)
        if env < 0.02 { return (0, 0, 0) }
        // Frequencies kept under the 24 fps repaint Nyquist (12 Hz) so the
        // buzz reads as a fast jitter, not an aliased slow wobble.
        let amp = CGFloat(env) * 2.4
        let dx = amp * CGFloat(sin(elapsed * 2 * .pi * 9))
        let dy = amp * 0.5 * CGFloat(cos(elapsed * 2 * .pi * 7))
        let blink = CGFloat(min(1.0, env * 1.35))
        return (dx, dy, blink)
    }

    /// Rotation (radians) for the menubar music-note glyph, derived from
    /// the most recent percussion hit across ALL drum pads — a damped
    /// left/right wobble that rings down over ~0.4 s so the icon visibly
    /// shimmies each time a drum is struck. Independent of the split
    /// being active (drums can be hit via the percussion split keys).
    static func percussionIconWobble() -> CGFloat {
        guard !drumPulses.isEmpty else { return 0 }
        // Strongest still-ringing pulse drives the wobble.
        var best: (env: Double, elapsed: Double)? = nil
        for p in drumPulses where p.at > 0 {
            let elapsed = drumPulseNow - p.at
            guard elapsed >= 0, elapsed < 0.5 else { continue }
            let env = p.level * exp(-elapsed * 8.0)
            if env > (best?.env ?? 0) { best = (env, elapsed) }
        }
        guard let b = best, b.env > 0.02 else { return 0 }
        // ~9° peak swing, oscillating a couple of times as it decays.
        let amp = CGFloat(b.env) * (9.0 * .pi / 180.0)
        return amp * CGFloat(sin(b.elapsed * 2 * .pi * 6))
    }

    /// Updated by AppDelegate.updateIcon() before each render so the renderer
    /// can pick the right letter labels and active-range without threading
    /// the keymap through every static method's signature.
    static var activeKeymap: Keymap = .notepat

    /// Adaptive sizing for menubar overflow. AppDelegate progressively
    /// shrinks this when the status item can't fit, and tries to expand
    /// back when there's room. Render math (pianoWidth, imageSize, hit
    /// rects, slot positions) all derive from `lastMidi`.
    enum DisplayLayout: String {
        case full        // C4..B5 — 2 octaves, normal-width keys (default)
        case fullSlim    // Legacy/dev override — 2 octaves, skinnier keys
        case thirteenKeys
        case twelveKeys
        case elevenKeys
        case tenKeys
        case nineKeys
        case eightKeys
        case oneOctave   // C4..B4 — 1 octave, normal-width keys
        case sixKeys
        case fiveKeys
        case fourKeys
        case threeKeys
        case twoKeys
        case oneKey
        case compact     // chip only, no piano keys

        private static let adaptiveOrder: [DisplayLayout] = [
            .compact,
            .oneKey,
            .twoKeys,
            .threeKeys,
            .fourKeys,
            .fiveKeys,
            .sixKeys,
            .oneOctave,
            .eightKeys,
            .nineKeys,
            .tenKeys,
            .elevenKeys,
            .twelveKeys,
            .thirteenKeys,
            .full,
        ]

        var visibleWhiteKeyCount: Int {
            switch self {
            case .full, .fullSlim: return 14
            case .thirteenKeys: return 13
            case .twelveKeys: return 12
            case .elevenKeys: return 11
            case .tenKeys: return 10
            case .nineKeys: return 9
            case .eightKeys: return 8
            case .oneOctave: return 7
            case .sixKeys: return 6
            case .fiveKeys: return 5
            case .fourKeys: return 4
            case .threeKeys: return 3
            case .twoKeys: return 2
            case .oneKey: return 1
            case .compact: return 0
            }
        }

        var adaptiveRank: Int {
            Self.adaptiveOrder.firstIndex(of: self) ?? Self.adaptiveOrder.count - 1
        }

        // Shrink by cropping high keys from the right edge. This
        // preserves the left-hand C anchor and keeps as much of the
        // playable keyboard visible as the menu bar will accept.
        var smaller: DisplayLayout? {
            let order = Self.adaptiveOrder
            guard let index = order.firstIndex(of: self), index > 0 else { return nil }
            return order[index - 1]
        }
        var larger: DisplayLayout? {
            let order = Self.adaptiveOrder
            guard let index = order.firstIndex(of: self), index < order.count - 1 else { return nil }
            return order[index + 1]
        }
    }
    static var displayLayout: DisplayLayout = .full

    /// When non-nil, AppDelegate's adaptive resize is a no-op and the
    /// renderer stays pinned at this value. Driven by the
    /// `forceLayout` UserDefaults key so users (and we) can verify the
    /// slim render without needing to actually squeeze the menubar.
    static var forceLayout: DisplayLayout? = nil

    /// Which half of the keyboard renders uppercase labels — the visual
    /// cue for "linger / bell-ring mode," now sided to match the sided
    /// linger: left shift uppercases the left (lower) half, right shift
    /// the right (upper) half, caps lock / both shifts the whole board.
    /// AppDelegate sets this on .flagsChanged and re-issues updateIcon().
    enum UppercaseSide { case none, left, right, all }
    static var labelsUppercaseSide: UppercaseSide = .none

    /// True if any uppercase cue is active. Kept for the non-per-key
    /// consumers (linger fermata gating, label baseline nudge) that only
    /// care whether the mode is armed at all, not which half.
    static var labelsUppercase: Bool { labelsUppercaseSide != .none }

    /// Whether the label for a given display MIDI note should render
    /// uppercase, honoring the active side. Notes are the menubar piano's
    /// C4–C5 window; the split matches the audio linger split.
    static func uppercaseForMidi(_ midi: Int) -> Bool {
        switch labelsUppercaseSide {
        case .none: return false
        case .all: return true
        case .left: return midi < MenuBandLayout.lingerSplitMidi
        case .right: return midi >= MenuBandLayout.lingerSplitMidi
        }
    }

    /// Caps-lock latched (as opposed to a momentary shift). Drawn
    /// state in the chip is gated differently for the two: caps
    /// always paints the linger fermata mark (the user has
    /// committed to the mode), shift only paints it while at least
    /// one note is held (so resting on shift doesn't add chrome).
    static var lingerCapsLatched: Bool = false

    /// True while at least one menubar piano note is currently held.
    /// Used to gate the shift-momentary linger fermata so it appears
    /// only during active play, not while the user is just resting
    /// on shift between phrases.
    static var playingActive: Bool = false

    /// True while the sample-voice backend is capturing audio from
    /// the input device (user holding `). When set, the settings
    /// chip's music-note glyph + VU bars switch to a red tint so the
    /// menubar reads as "REC ON" at a glance. AppDelegate flips this
    /// in `updateIcon()` based on the controller's
    /// `sampleRecordingActive` proxy.
    static var recordingActive: Bool = false

    // Render area shrinks with the layout. Compact has no piano keys at
    // all — `lastMidi < firstMidi` makes whiteList() empty.
    static let firstMidi: Int = 60                 // C4 (middle C)
    private static let visibleWhiteMidiUpperBounds = [
        60, 62, 64, 65, 67, 69, 71,
        72, 74, 76, 77, 79, 81, 83,
    ]

    static var lastMidi: Int {
        let count = displayLayout.visibleWhiteKeyCount
        guard count > 0 else { return firstMidi - 1 }
        return visibleWhiteMidiUpperBounds[count - 1]
    }

    /// Active subset of the visible range that's drawn + interactive.
    /// Capped by both the keymap range (Ableton tops out at E5/76) and the
    /// current display layout. Returns nil when no keys are displayable.
    static var activeRange: ClosedRange<Int>? {
        let keymapMax = activeKeymap == .ableton ? 76 : 83
        let upper = Swift.min(keymapMax, lastMidi)
        return upper >= firstMidi ? firstMidi...upper : nil
    }

    @inline(__always)
    private static func isActive(_ midi: Int) -> Bool {
        activeRange?.contains(midi) ?? false
    }

    /// Right-align the active keys in Ableton mode so the negative space
    /// (the unmapped 4 whites past E5) sits on the *left* — visually it
    /// reads as "the rightmost portion of the 2-octave area is the playable
    /// keymap." `imageSize` doesn't change, so the popover anchor stays put.
    static var activeSlotOffset: Int {
        let allWhites = whiteList()
        guard let range = activeRange else { return 0 }
        let activeCount = allWhites.filter { range.contains($0) }.count
        return allWhites.count - activeCount
    }

    // Piano key sizes — wide whites give a generous hit area between black
    // keys (the exposed-white-between-blacks zone is whiteW - blackW).
    // Heights stay constant (menubar height is fixed); widths flex with
    // the display layout so .fullSlim can keep all 14 whites visible
    // when the menubar is squeezed instead of dropping an octave.
    static var whiteW: CGFloat {
        switch displayLayout {
        case .fullSlim: return 17.0   // ~74% of full — saves 84px on 14 whites
        default:        return 23.0
        }
    }
    static let baseWhiteH: CGFloat = 21.0
    static let baseBlackH: CGFloat = 12.0
    /// Multiplier applied to white/black key HEIGHTS only (widths stay
    /// fixed). The menubar piano stays the compact icon-shape it has
    /// always been; the floating play palette swaps this to a larger
    /// value inside `withFloatingPaletteKeyboard` so the overlay reads
    /// like a traditional piano with tall white keys + properly
    /// proportioned blacks on top.
    static var keyHeightScale: CGFloat = 1.0
    static var whiteH: CGFloat { baseWhiteH * keyHeightScale }
    static var blackH: CGFloat { baseBlackH * keyHeightScale }
    static var blackW: CGFloat {
        switch displayLayout {
        case .fullSlim: return 10.0   // proportional to slim white
        default:        return 13.5
        }
    }
    static let pad: CGFloat = 0.5

    // Mini visualizer — three vertical LED bars that REPLACE the three
    // horizontal "staff lines" of the SF Symbol `music.note.list`
    // inside the settings chip. So the chip reads as a music-note +
    // mini-meter pair: glyph on the left for settings, bars on the
    // right that pulse with note activity and are the click target for
    // the floating play palette (the "big overlay"). Single icon on
    // the menu, single visualizer affordance. Replaces both the
    // previous slide-down strip AND the leftmost-of-piano slot we
    // experimented with.
    /// Width of the visualizer slot inside the chip — set to match the
    /// natural left-half extent of the SF Symbol music.note.list at
    /// pointSize 13 (measured: lines occupy x ∈ [2.0, 8.5] of a 17pt
    /// symbol, ~6.5pt wide). The slot is slightly wider than the
    /// strict bounding box so the bars have a tiny breathing margin.
    static let miniVisualizerW: CGFloat = 7.5
    /// Hidden during popover/palette display so we don't render two
    /// visualizers at once.
    static var miniVisualizerVisible: Bool = true
    /// 0..1 amplitude derived from current note activity, smoothed at
    /// the AppDelegate's animation tick so bars move continuously
    /// instead of stepping.
    static var miniVisualizerLevel: CGFloat = 0.0
    /// Real per-bar levels (0..1) — bass / mid / treble RMS of the live
    /// waveform tap, updated every animation tick with a fast response so
    /// the three bars are an honest, time-accurate spectrum meter rather
    /// than a decorative wiggle. Empty until the first tick populates it.
    static var miniVisualizerBars: [CGFloat] = [0, 0, 0]
    /// Wall-clock seconds, updated once per animation tick. Drives the
    /// per-bar phase offset so bars wiggle independently — reads as
    /// live audio metering rather than three identical pulses.
    static var miniVisualizerPhase: CFTimeInterval = 0
    /// 0..1 fill brightness of the MIDI activity square. While
    /// MIDI mode is on the bars slot becomes an empty square that
    /// briefly fills on every outbound MIDI event (Ableton-style
    /// activity indicator). AppDelegate spikes this to 1 on each
    /// noteOn and decays it at ~80% per animation tick.
    static var midiActivityFlash: CGFloat = 0
    /// 0..1 metronome-tick flash — driven by the popover metronome
    /// each audible beat. Tints the music-note glyph yellow for a
    /// fraction of a second, decaying smoothly so the menubar
    /// "blinks" in time with the tempo.
    static var metronomeFlash: CGFloat = 0
    /// When true the visualizer slot inside the music-note chip
    /// renders a continuous sine wave instead of the 3 VU bars —
    /// reads as the metronome's "carrier" running, with the
    /// per-tick yellow flash riding on top.
    static var metronomeOn: Bool = false
    /// BPM + swing-start timestamp pushed by the popover's
    /// metronome on every restart. The chip's needle uses these
    /// to compute its swing phase from CACurrentMediaTime so the
    /// chip animation stays in lockstep with the trapezoid in
    /// the popover (instead of running at a fixed wall-clock
    /// rate that drifts apart from the actual BPM).
    static var metronomeBPM: Int = 60
    static var metronomeStartTime: CFTimeInterval = 0

    static func withPianoWaveformKeyboard<T>(keymap: Keymap?, _ body: () -> T) -> T {
        let oldLayout = displayLayout
        let oldKeymap = activeKeymap
        let oldScale = keyHeightScale
        displayLayout = .full
        keyHeightScale = pianoWaveformKeyHeightScale
        if let keymap {
            activeKeymap = keymap
        }
        defer {
            displayLayout = oldLayout
            activeKeymap = oldKeymap
            keyHeightScale = oldScale
        }
        return body()
    }

    // Settings — simple monochrome music note that reads like a native
    // status-bar icon. Click → popup menu with TYPE / MIDI / Instrument /
    // About.
    static let settingsW: CGFloat = 18.0
    static let settingsH: CGFloat = 21.0
    static let settingsGap: CGFloat = 12.0
    private static var settingsLeadGap: CGFloat {
        displayLayout == .compact ? 0 : settingsGap
    }
    /// Reserved space on the right of the icon for the voice-number
    /// badge to flow into when the program has 2+ digits. The music
    /// note + settingsHitRect stay anchored where they were; this
    /// pad just gives multi-digit numbers somewhere to grow without
    /// clipping at the image edge.
    /// Reserved space to the right of the music-note glyph for the
    /// voice-number badge digits. Sized so a 2-digit voice (1-99,
    /// covers the entire GM lineup except the top 28) sits cleanly
    /// inside the slot. 3-digit voices may extend a hair past the
    /// edge but still read clearly. Keeps the icon's right margin
    /// close to standard macOS menubar spacing.
    /// Character count of the current voice subscript, set by the
    /// AppDelegate each `updateIcon`. Lets the badge pad grow for 3-digit
    /// GM numbers (100–128) so the rightmost digit isn't cropped.
    static var voiceBadgeDigits: Int = 1
    /// Right pad reserved for the voice-number badge. Base covers the
    /// common 1–2 character case; each extra character adds ~one
    /// monospaced 7pt digit width so the slot widens only when needed.
    static var voiceBadgeRightPad: CGFloat {
        5.0 + CGFloat(max(0, voiceBadgeDigits - 2)) * 5.0
    }

    enum HitResult: Equatable {
        case openSettings
        case openVisualizer
        case note(UInt8)
        /// Red REC dot at the far left — toggles tape recording.
        case recDot
        /// Cassette body — pure drag-source now (REC moved to its own
        /// transport button). Clicking the cassette is a no-op; pulling
        /// it sideways ejects the WAV to the Desktop.
        case tape
        /// Transport buttons live between the cassette and the piano,
        /// in classic Walkman order: REW · STOP · PLAY · FFWD · REC · EJECT.
        case tapeRew
        case tapeStop
        case tapePlay
        case tapeFfwd
        case tapeRec
        case tapeEject
    }

    // MARK: - Tape (inline cassette)
    //
    // A miniature Walkman-style cassette sits to the LEFT of the piano
    // keys. Single-click toggles REC; mouse-drag starts a drag session
    // that ejects the WAV file onto the desktop. Visual cues:
    //
    //   • Two hexagonal-hub reels rotate during REC (red tint) or PLAY
    //     (steel tint).
    //   • A small red "● REC" LED inside the label card glows while
    //     recording.
    //   • A mic-permission LED to the right of the cassette glows
    //     orange while the hot mic is delivering frames (i.e. while
    //     REC is engaged AND mic access was granted).
    //
    // All state is pushed in via the static properties below before
    // each `image(...)` render — AppDelegate's `updateIcon()` reads
    // the controller's tape and refreshes these.
    static var tapeRecording: Bool = false
    static var tapePlaying: Bool = false
    /// 0..1 fraction of the recording capacity used. Drives the
    /// "fill" of the right reel and the duration tick mark.
    static var tapeFillFraction: CGFloat = 0
    /// 0..1 playhead position. Identical to fill while no recording
    /// happened yet; tracks playback otherwise.
    static var tapePlayheadFraction: CGFloat = 0
    /// Orange "mic hot" LED. True while the synth is delivering mic
    /// frames AND the tape is recording.
    static var tapeMicHot: Bool = false
    /// Continuous wall-clock seconds; drives reel rotation so the
    /// reels keep turning smoothly between hover events.
    static var tapePhase: CFTimeInterval = 0

    // Feature flag — the tape deck is still WIP. AppDelegate mirrors
    // `MenuBandTapeDeckEnabled` from UserDefaults into this flag, and
    // every tape-related layout/draw/hit-test branch is gated on it.
    // Defaults to OFF so a fresh install doesn't ship the unfinished
    // surface in the menubar.
    static var tapeFeatureEnabled: Bool = false

    /// UserDefaults key for the tape-deck feature flag.
    static let tapeFeatureDefaultsKey = "MenuBandTapeDeckEnabled"

    /// UserDefaults key for the right-hand percussion split toggle. Shared
    /// between MenuBandController (source of truth) and the About-window
    /// checkbox so the string never diverges.
    static let percussionSplitDefaultsKey = "notepat.percussionSplit"
    /// Per-side latched percussion (tap left-⌥ / right-⌥). Source of truth
    /// shared between MenuBandController and the About-window checkbox.
    static let percussionLeftDefaultsKey = "notepat.percussionLeft"
    static let percussionRightDefaultsKey = "notepat.percussionRight"

    // Compact Cassette physical aspect: 100.4mm × 63.8mm ≈ 1.574:1.
    // Cassette fills the full piano height (whiteH = 21pt); width
    // tracks the real-world ratio: 21 × 33 ≈ 1.571:1.
    static let tapeBodyW: CGFloat = 33.0
    static var tapeBodyH: CGFloat { whiteH }
    static let tapeBodyGap: CGFloat = 6.0
    /// Horizontal pad before the tape — keeps the cassette flush with
    /// the menubar's leading edge of the status item.
    static let tapeLeadPad: CGFloat = 0.5

    // MARK: - REC dot (tape recording)
    /// A red record dot in a fixed slot at the far LEFT of the menubar icon,
    /// before the piano. Click to start recording a tape; while recording it
    /// shows an elapsed timer; click again to stop + drop the take. State is
    /// pushed in by AppDelegate before each icon redraw.
    static let recDotDiameter: CGFloat = 9
    static let recDotLeadPad: CGFloat = 1
    static let recDotTrailGap: CGFloat = 3
    /// While recording the dot morphs into a capsule "bubble" holding the
    /// running m:ss timer.
    static let recPillWidth: CGFloat = 30
    static let recPillHeight: CGFloat = 12
    static var recActive: Bool = false        // true → recording (show timer)
    static var recElapsed: Double = 0          // seconds, drives timer + morph
    /// Slot width reserved at the left — full pill width while recording (so
    /// the keys don't jiggle as the bubble morphs open within the slot).
    static var recReservedWidth: CGFloat {
        recDotLeadPad + (recActive ? recPillWidth : recDotDiameter) + recDotTrailGap
    }

    // ── Deck panel (transport buttons only — cassette is mounted to
    // the LEFT of the deck on visible drive spokes that stick out of
    // the deck's left edge, like a portable cassette mechanism where
    // the transport face is one slab and the tape rides on spindles).
    static let deckPadInsetX: CGFloat = 2.5
    /// Visible run of the drive spokes between cassette and deck face.
    static let deckSpokesWidth: CGFloat = 6.0
    /// Horizontal pad between the deck's trailing edge and the piano.
    static let deckTrailGap: CGFloat = 4.0

    // Transport buttons — tall narrow columns running the full deck
    // height. Square corners, separated by hairline gaps so the row
    // reads as a stack of piano-key levers, not pill chiclets.
    static let transportBtnW: CGFloat = 17.0
    static let transportBtnGap: CGFloat = 1.6
    /// Vertical inset between deck face edge and column top/bottom.
    static let transportColumnInset: CGFloat = 1.6
    static let transportButtonCount: Int = 6

    static var transportStripWidth: CGFloat {
        CGFloat(transportButtonCount) * transportBtnW
            + CGFloat(transportButtonCount - 1) * transportBtnGap
    }

    /// Width of the deck panel itself (transport buttons + padding).
    /// Cassette no longer lives inside the deck — it hangs off the
    /// deck's left side on visible drive spokes.
    static var deckPanelWidth: CGFloat {
        deckPadInsetX + transportStripWidth + deckPadInsetX
    }

    /// Total footprint added to the status-item width — cassette,
    /// drive spokes, deck panel, and trailing pad before the piano.
    /// Returns 0 when the tape feature is disabled so the piano +
    /// settings chip shift left into the freed space.
    static var tapeReservedWidth: CGFloat {
        guard tapeFeatureEnabled else { return 0 }
        return tapeLeadPad + tapeBodyW + deckSpokesWidth
            + deckPanelWidth + deckTrailGap
    }

    // Letter labels keyed by MIDI note, per layout. The renderer picks
    // between these via `activeKeymap` so the menubar piano shows the
    // correct QWERTY hint for whichever mode you're in.
    private static let labelByMidiNotepat: [Int: String] = [
        58: "z", 59: "x", 60: "c", 61: "v", 62: "d", 63: "s",
        64: "e", 65: "f", 66: "w", 67: "g", 68: "r", 69: "a",
        70: "q", 71: "b", 72: "h", 73: "t", 74: "i", 75: "y",
        76: "j", 77: "k", 78: "u", 79: "l", 80: "o", 81: "m",
        82: "p", 83: "n", 84: ";", 85: "'", 86: "]",
    ]

    // Ableton Live's M-mode QWERTY mapping: A=C, W=C#, S=D, E=D#, D=E,
    // F=F, T=F#, G=G, Y=G#, H=A, U=A#, J=B, K=C+1, O=C#+1, L=D+1,
    // P=D#+1, ;=E+1. Mirrors `MenuBandLayout.semitoneByKeyCodeAbleton`
    // with middle C anchored at MIDI 60.
    private static let labelByMidiAbleton: [Int: String] = [
        60: "a", 61: "w", 62: "s", 63: "e", 64: "d",
        65: "f", 66: "t", 67: "g", 68: "y", 69: "h",
        70: "u", 71: "j", 72: "k", 73: "o", 74: "l",
        75: "p", 76: ";",
    ]

    static var labelByMidi: [Int: String] {
        activeKeymap == .ableton ? labelByMidiAbleton : labelByMidiNotepat
    }

    @inline(__always)
    private static func isWhite(_ midi: Int) -> Bool {
        switch midi % 12 {
        case 0, 2, 4, 5, 7, 9, 11: return true
        default: return false
        }
    }

    private static func whiteList() -> [Int] {
        guard lastMidi >= firstMidi else { return [] }
        return (firstMidi...lastMidi).filter { isWhite($0) }
    }

    private static var pianoWidth: CGFloat {
        CGFloat(whiteList().count) * whiteW
    }

    /// Pixel distance for one octave of natural keys. AppDelegate uses this
    /// for octave-shift animation so the menubar piano moves like a long
    /// masked instrument, not like the whole status item is paging sideways.
    static var octaveSlideDistance: CGFloat {
        7 * whiteW
    }

    /// Global magnification of the menubar icon. The image is drawn at
    /// base coordinates then blown up by this factor so the whole thing
    /// (piano, chip, bars, tape) reads larger in the bar — same
    /// proportions, just bigger. Hit-testing divides incoming points by it
    /// so clicks still land. Set adaptively at launch to fill the bar's
    /// usable height without overflowing (the button centers — doesn't
    /// downscale — so overshooting would clip). 1.0 = original size.
    static var iconScale: CGFloat = 1.0

    /// Base (unscaled) menubar icon size — the coordinate space all drawing
    /// and hit rects are computed in.
    static var baseImageSize: NSSize {
        // imageSize is always queried in tape-included context (it's
        // the menubar status-item size). Compute as if the static
        // flag were set, regardless of the flag's transient state.
        let totalW = ceil(recReservedWidth + tapeReservedWidth + pad + pianoWidth + settingsLeadGap + settingsW + pad + voiceBadgeRightPad)
        let totalH = ceil(whiteH + pad * 2)
        return NSSize(width: totalW, height: totalH)
    }

    /// The displayed menubar image size — base blown up by `iconScale`.
    /// Drives the status-item length and the window→image hit mapping.
    static var imageSize: NSSize {
        NSSize(width: ceil(baseImageSize.width * iconScale),
               height: ceil(baseImageSize.height * iconScale))
    }

    /// Cassette body rect — hangs off the LEFT side of the deck on
    /// visible drive spokes. Same vertical extent as the piano keys
    /// so the cassette reads as the same "device height" as the deck.
    static var tapeRect: NSRect {
        let bodyY = (baseImageSize.height - tapeBodyH) / 2.0
        return NSRect(x: tapeLeadPad,
                      y: bodyY,
                      width: tapeBodyW,
                      height: tapeBodyH)
    }

    /// Outer rect of the deck panel — the white transport face. Sits
    /// to the RIGHT of the cassette, separated by `deckSpokesWidth`
    /// worth of visible drive-spindle metal. Matches piano height.
    static var deckRect: NSRect {
        let x = tapeRect.maxX + deckSpokesWidth
        return NSRect(x: x,
                      y: pad,
                      width: deckPanelWidth,
                      height: whiteH)
    }

    /// X origin of the transport strip inside the deck face.
    static var transportStripOriginX: CGFloat {
        deckRect.minX + deckPadInsetX
    }

    /// Rect for a single transport button by index (0 = REW … 5 = EJECT).
    /// Columns span the deck's full height (minus a small inset), so
    /// they read as vertical piano-key-style levers rather than caps.
    static func transportButtonRect(_ index: Int) -> NSRect {
        let x = transportStripOriginX
            + CGFloat(index) * (transportBtnW + transportBtnGap)
        let y = deckRect.minY + transportColumnInset
        let h = deckRect.height - transportColumnInset * 2
        return NSRect(x: x, y: y, width: transportBtnW, height: h)
    }

    /// Where the VU bars actually DRAW — the original 3 staff-line
    /// band only, AppKit y [chip.midY - 1.5, chip.midY + 4.25] (height
    /// 5.75pt). Bars never grow past these bounds; this rect is also
    /// the click hit-test area.
    static var miniVisualizerRect: NSRect {
        let chip = settingsIconRect
        return NSRect(x: chip.midX - 6.5,
                      y: chip.midY - 1.5,
                      width: 6.5,
                      height: 5.75)
    }

    /// Punch-out region for the visualizer overlay. Slightly bigger
    /// than the bars draw rect on the LEFT and BOTTOM only — those
    /// are the edges where SF Symbol anti-aliasing leaves stray
    /// pixels of the staff lines after a tight destinationOut, and
    /// the menubar background (which can be a saturated theme color
    /// like green on the user's setup) bleeds through them. Right +
    /// top stay tight so the music-note's stem fragment (which sits
    /// just below this rect) isn't accidentally erased.
    private static var miniVisualizerPunchRect: NSRect {
        let r = miniVisualizerRect
        return NSRect(x: r.minX - 1.0,           // +1pt to the left
                      y: r.minY - 1.0,           // +1pt down
                      width: r.width + 1.0,
                      height: r.height + 1.0)
    }

    static var pianoImageSize: NSSize {
        pianoImageSize(layout: .fixedCanvas)
    }

    static func pianoImageSize(layout: Layout) -> NSSize {
        // pianoImageSize is used by callers that draw the keyboard
        // WITHOUT the tape strip (the floating play palette, e.g.).
        // Don't reserve tape width here.
        let totalW = ceil(pad + pianoWidth(layout: layout) + pad)
        let totalH = ceil(whiteH + pad * 2)
        return NSSize(width: totalW, height: totalH)
    }

    /// Whether the current render is including the tape strip (true
    /// for the full menubar icon; false for the standalone keyboard
    /// in the floating palette). Defaults to TRUE because the status
    /// item is the dominant caller — hit-testing + size queries fire
    /// outside any drawing closure and need the tape reservation in
    /// place. Flipped to FALSE only inside the palette's drawing
    /// handler (`includeSettings == false`).
    private static var tapeReservedInLayout: Bool = true
    private static var pianoOriginX: CGFloat {
        (tapeReservedInLayout ? recReservedWidth + tapeReservedWidth : 0) + pad
    }

    /// The REC dot's slot rect (and its hit rect) in base coords — the
    /// fixed left slot, only present in the menubar layout.
    static var recHitRect: NSRect {
        let h = baseImageSize.height
        let w = recReservedWidth
        return NSRect(x: 0, y: 0, width: w, height: h)
    }

    /// Draw the REC dot (idle) or dot + elapsed timer (recording) in the
    /// reserved left slot. Drawn in base coords inside the render closure.
    private static func drawRecIndicator(canvasHeight: CGFloat) {
        let x = recDotLeadPad
        // Idle: a steady red record dot.
        if !recActive {
            let d = recDotDiameter
            NSColor.systemRed.setFill()
            NSBezierPath(ovalIn: NSRect(x: x, y: (canvasHeight - d) / 2,
                                        width: d, height: d)).fill()
            return
        }
        // Recording: the dot MORPHS into a red capsule "bubble" that holds
        // the running timer — width + height ease from the dot to the pill
        // over the first ~0.25s, then the m:ss text fades in inside it.
        let prog = CGFloat(max(0, min(1, recElapsed / 0.25)))
        let w = recDotDiameter + (recPillWidth - recDotDiameter) * prog
        let h = recDotDiameter + (recPillHeight - recDotDiameter) * prog
        let rect = NSRect(x: x, y: (canvasHeight - h) / 2, width: w, height: h)
        let blink = 0.7 + 0.3 * abs(sin(recElapsed * .pi))   // gentle pulse
        NSColor.systemRed.withAlphaComponent(blink).setFill()
        NSBezierPath(roundedRect: rect, xRadius: h / 2, yRadius: h / 2).fill()
        // Timer text fades in once the bubble is mostly open.
        let textAlpha = max(0, (prog - 0.55) / 0.45)
        guard textAlpha > 0.01 else { return }
        let total = Int(recElapsed)
        let str = NSString(format: "%d:%02d", total / 60, total % 60)
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 7, weight: .bold),
            .foregroundColor: NSColor.white.withAlphaComponent(textAlpha),
        ]
        let sz = str.size(withAttributes: attrs)
        str.draw(at: NSPoint(x: rect.midX - sz.width / 2, y: rect.midY - sz.height / 2),
                 withAttributes: attrs)
    }

    /// Settings chip's visual rect IS its hit-test rect — they're identical
    /// so the user gets visual feedback wherever the click lands.
    private static var settingsRect: NSRect { settingsHitRect }

    static func image(litNotes: Set<UInt8>,
                      playbackLitNotes: Set<UInt8> = [],
                      enabled: Bool,
                      typeMode: Bool = false,
                      melodicProgram: UInt8 = 0,
                      voiceLabel: String? = nil,
                      hovered: HitResult? = nil,
                      letterAlpha: ((UInt8) -> CGFloat)? = nil,
                      slideOffsetX: CGFloat = 0,
                      settingsFlash: CGFloat = 0,
                      includeSettings: Bool = true,
                      layout: Layout = .fixedCanvas) -> NSImage {
        let whites = whiteList()
        var whiteIndex: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { whiteIndex[m] = i }
        // NSImage's drawing handler fires LAZILY — once AppKit needs
        // the bitmap, not at the moment we hand the closure over. So
        // any static state we'd want active "during drawing" has to be
        // set INSIDE the closure, not around it. `includeSettings`
        // captures into the closure; the closure flips
        // `tapeReservedInLayout` to match it for the duration of the
        // draw, then restores. Default static value (true) covers
        // hit-testing + size queries that happen outside any
        // drawing context.
        // `size` stays in BASE coordinates (all the drawing math below uses
        // it). The menubar canvas is blown up by iconScale and a matching
        // transform is applied so the whole icon renders larger; the
        // floating palette (includeSettings == false) is left at 1×.
        let size = includeSettings ? baseImageSize : pianoImageSize(layout: layout)
        let s: CGFloat = includeSettings ? iconScale : 1.0
        let canvasSize = NSSize(width: ceil(size.width * s), height: ceil(size.height * s))

        let img = NSImage(size: canvasSize, flipped: false) { _ in
            let savedTapeReserved = tapeReservedInLayout
            tapeReservedInLayout = includeSettings
            defer { tapeReservedInLayout = savedTapeReserved }
            if s != 1.0 {
                let scale = NSAffineTransform()
                scale.scale(by: s)
                scale.concat()
            }

            // 🔴 REC dot — fixed slot at the far left, before the piano.
            // Solid red when idle; a blinking dot + elapsed timer while
            // recording a tape. Only in the menubar layout (not the palette).
            if tapeReservedInLayout {
                drawRecIndicator(canvasHeight: size.height)
            }

            // Piano.
            NSGraphicsContext.saveGraphicsState()
            // Octave slide: scroll ONLY the piano keys behind a
            // fixed mask. The settings chip + visualizer stay
            // anchored, so changing octave reads as the piano
            // moving past a window cut into the menubar — physical
            // continuity (no whole-icon shift, no chip jiggle).
            // Clip first (in unmoved coords), then apply the
            // translation so keys slide behind the clip edges.
            // Piano area starts AFTER the tape strip when the tape is
            // drawn into the same image. The closure runs while
            // `tapeReservedInLayout == includeSettings`, so
            // `pianoOriginX` already reflects whether the tape is in
            // the layout — derive the clip start from it directly.
            let pianoMaskWidth = ceil(pad + pianoWidth(layout: layout) + pad)
            let pianoMaskRect = NSRect(x: pianoOriginX - pad, y: 0,
                                        width: pianoMaskWidth,
                                        height: size.height)
            NSBezierPath(rect: pianoMaskRect).addClip()
            if abs(slideOffsetX) > 0.01 {
                let xform = NSAffineTransform()
                xform.translateX(by: slideOffsetX, yBy: 0)
                xform.concat()
            }
            // Piano theme: notepat's cool off-white naturals in light
            // mode (RGB 215,225,230 → 195,205,210), dropped to a deep
            // slate in dark mode so the keys feel native against a
            // dark menubar instead of glowing white. Lit (active)
            // state always rides the accent palette so a pressed key
            // contrasts both backgrounds.
            let isDark = NSApp.effectiveAppearance.bestMatch(
                from: [.aqua, .darkAqua]) == .darkAqua
            // Active fill — light mode pops to a brighter accent
            // highlight; dark mode dampens slightly toward black so
            // the press feedback doesn't blast out of the slate
            // keyboard.
            let lit: NSColor = isDark
                ? (NSColor.controlAccentColor.blended(withFraction: 0.18, of: .black)
                    ?? NSColor.controlAccentColor)
                : (NSColor.controlAccentColor.highlight(withLevel: 0.30)
                    ?? NSColor.controlAccentColor)
            // Playback fill — red, distinct from the system-accent
            // press color so the user can tell "the score is playing
            // me back" from "I'm pressing keys right now."
            let playbackLit: NSColor = isDark
                ? NSColor(srgbRed: 230/255, green: 60/255, blue: 60/255, alpha: 1)
                : NSColor(srgbRed: 220/255, green: 35/255, blue: 35/255, alpha: 1)
            let groove: NSColor
            let whiteHi: NSColor
            let whiteLo: NSColor
            let blackHi: NSColor
            let blackLo: NSColor
            if isDark {
                groove = NSColor(srgbRed: 140/255, green: 155/255,
                                 blue: 165/255, alpha: 0.55)
                whiteHi = NSColor(srgbRed:  62/255, green:  72/255,
                                  blue:  82/255, alpha: 1)
                whiteLo = NSColor(srgbRed:  44/255, green:  54/255,
                                  blue:  62/255, alpha: 1)
                // Glowy sharps in dark mode — pump saturation +
                // brightness so the black keys feel like lit
                // accent gems above the slate naturals instead of
                // muddy shadows.
                blackHi = Self.boostedAccent(saturationBoost: 0.55,
                                             brightnessBoost: 0.45)
                blackLo = Self.boostedAccent(saturationBoost: 0.30,
                                             brightnessBoost: 0.20)
            } else {
                groove = NSColor(srgbRed: 50/255, green: 65/255,
                                 blue: 75/255, alpha: 0.75)
                whiteHi = NSColor(srgbRed: 215/255, green: 225/255,
                                  blue: 230/255, alpha: 1)
                whiteLo = NSColor(srgbRed: 195/255, green: 205/255,
                                  blue: 210/255, alpha: 1)
                blackHi = NSColor.controlAccentColor.shadow(withLevel: 0.30)
                    ?? NSColor.controlAccentColor
                blackLo = NSColor.controlAccentColor.shadow(withLevel: 0.55)
                    ?? NSColor.controlAccentColor
            }

            // "Edges" of the *active* range — used for the rounded outer
            // corners. Active range may be right-aligned (Ableton) so the
            // outer corners sit on the first/last active white in the slot
            // layout, not the geometric ends of the render area.
            let activeWhites = activeWhites(layout: layout)
            let leftmostMidi = activeWhites.first ?? firstMidi
            let rightmostMidi = activeWhites.last ?? lastMidi
            let slotOffset = slotOffset(layout: layout)
            let sliding = abs(slideOffsetX) > 0.01
            let tileWidth = pianoWidth(layout: layout)
            let tileOffsets: [CGFloat] = sliding
                ? [-tileWidth, 0, tileWidth]
                : [0]
            for tileOffset in tileOffsets {
                NSGraphicsContext.saveGraphicsState()
                if abs(tileOffset) > 0.01 {
                    let tileTransform = NSAffineTransform()
                    tileTransform.translateX(by: tileOffset, yBy: 0)
                    tileTransform.concat()
                }
                for (idx, m) in whites.enumerated() {
                    if !isActive(m) { continue }   // negative space — skip draw
                    // Percussion vibe: a fresh drum hit jolts only the
                    // LETTER with a damped buzz — the keycap + stripe stay
                    // put so just the label shakes (the blink still rides
                    // the same envelope on the drum-pad wash below).
                    let vibe = Self.percussionVibe(forMidi: m)
                    let rect = whiteRect(at: idx + slotOffset)
                    let isLit = tileOffset == 0 && litNotes.contains(UInt8(m))
                    let isPlaybackLit = tileOffset == 0 && playbackLitNotes.contains(UInt8(m))
                    let isHover = tileOffset == 0 && hovered == .note(UInt8(m))
                    let isLeftmost = !sliding && (m == leftmostMidi)
                    let isRightmost = !sliding && (m == rightmostMidi)
                    let path = roundedKeyPath(
                        rect: rect,
                        tl: isLeftmost ? 2.5 : 0,
                        tr: isRightmost ? 2.5 : 0,
                        br: isRightmost ? 2.5 : 0,
                        bl: isLeftmost ? 2.5 : 0
                    )
                    if isPlaybackLit {
                        // Auto-playback from a dragged-in PDF — red
                        // wins over the user's accent press color so
                        // the source of the sound stays unambiguous.
                        playbackLit.setFill()
                        path.fill()
                    } else if isLit {
                        // Pressed: whole keycap turns the system accent
                        // — the rainbow stripe stays hidden while the
                        // key's down so the press reads as a single
                        // saturated event, not a stripe-grow animation.
                        lit.setFill()
                        path.fill()
                    } else {
                        NSGradient(starting: whiteHi, ending: whiteLo)!.draw(in: path, angle: -90)
                    }
                    if isHover && !isLit {
                        NSColor.controlAccentColor.withAlphaComponent(0.50).setFill()
                        path.fill()
                    }
                    // Percussion split: wash the right-hand keys in their
                    // drum-pad color so the drum zone is unmistakable. Takes
                    // the place of the chromatic stripe for those keys.
                    let drumColor = Self.percussionDrumColor(forMidi: m)
                    if let drumColor, !isLit {
                        // Blink: a fresh hit flashes the pad brighter/whiter,
                        // riding the same envelope as the shake.
                        let b = vibe.blink
                        let c = b > 0.01
                            ? (drumColor.blended(withFraction: b * 0.85, of: .white) ?? drumColor)
                            : drumColor
                        let baseAlpha: CGFloat = isDark ? 0.50 : 0.62
                        c.withAlphaComponent(min(1.0, baseAlpha + b * 0.38)).setFill()
                        path.fill()
                    }
                    // Chromatic stripe — thin flat ROYGBIV band along
                    // the bottom of each natural key, idle only. Hidden
                    // on press so the lit accent fill reads cleanly.
                    // Dark mode dims the chroma toward black so it
                    // doesn't read as neon against dark slate keys.
                    let stripeH: CGFloat = keyHeightScale > 1.0 ? 3.0 : 2.0
                    if drumColor == nil, let chroma = Self.chromaticColorByPitchClass[m % 12], !isLit {
                        let stripeChroma: NSColor = isDark
                            ? (chroma.blended(withFraction: 0.18, of: .black) ?? chroma)
                            : chroma
                        NSGraphicsContext.saveGraphicsState()
                        path.addClip()
                        let stripeRect = NSRect(
                            x: rect.minX,
                            y: rect.minY,
                            width: rect.width,
                            height: stripeH
                        )
                        if isDark {
                            // Backlit-organ glow — clipped to the lower
                            // portion of the keycap so the halo radiates
                            // sideways + downward without bleeding up
                            // into the key's top edge.
                            NSGraphicsContext.saveGraphicsState()
                            let glowClipH = stripeH + 4
                            let glowBox = NSRect(
                                x: rect.minX - 8,
                                y: rect.minY - 8,
                                width: rect.width + 16,
                                height: glowClipH + 8
                            )
                            NSBezierPath(rect: glowBox).addClip()
                            Self.withGlow(color: chroma, blur: 4.5, alpha: 0.85) {
                                stripeChroma.setFill()
                                NSBezierPath(rect: stripeRect).fill()
                            }
                            NSGraphicsContext.restoreGraphicsState()
                        } else {
                            stripeChroma.setFill()
                            NSBezierPath(rect: stripeRect).fill()
                        }
                        // Top-edge faux lighting — light mode catches a
                        // soft white sheen from above (ambient lamp);
                        // dark mode flips to a thin dark vignette so
                        // the keycap top reads as a pulled-down crown
                        // rather than a glowy halo, which would fight
                        // with the chromatic glow at the bottom.
                        let topH: CGFloat = min(5, rect.height * 0.30)
                        let topRect = NSRect(
                            x: rect.minX,
                            y: rect.maxY - topH,
                            width: rect.width,
                            height: topH
                        )
                        let topGradient: NSGradient? = isDark
                            ? NSGradient(
                                starting: NSColor.black.withAlphaComponent(0.30),
                                ending: NSColor.black.withAlphaComponent(0)
                            )
                            : NSGradient(
                                starting: NSColor.white.withAlphaComponent(0.55),
                                ending: NSColor.white.withAlphaComponent(0)
                            )
                        topGradient?.draw(in: topRect, angle: -90)
                        NSGraphicsContext.restoreGraphicsState()
                    }
                    groove.setStroke()
                    path.lineWidth = 0.7
                    path.stroke()
                    // Lit keys always wear their letter at full opacity (a
                    // mouse-press tap reveals just that letter). For unlit
                    // keys, the per-key `letterAlpha` closure drives a wave
                    // fade-in / fade-out that ripples outward from whichever
                    // key the user is currently playing. Falls back to the
                    // legacy binary `typeMode` rendering when no closure is
                    // supplied (e.g., previews that don't drive animation).
                    if tileOffset == 0, let letter = labelByMidi[m] {
                        let display = Self.uppercaseForMidi(m) ? letter.uppercased() : letter
                        let a: CGFloat
                        if isLit {
                            a = 1.0
                        } else if let closure = letterAlpha {
                            a = closure(UInt8(m))
                        } else {
                            a = typeMode ? 1.0 : 0.0
                        }
                        if a > 0.01 {
                            // Labels stay anchored at the bottom of each
                            // key — the chromatic stripe paints behind
                            // them, so the letter reads on the colored
                            // band rather than floating above it. The
                            // percussion buzz shakes ONLY the letter rect.
                            let labelRect = rect.offsetBy(dx: vibe.dx, dy: vibe.dy)
                            drawWhiteLabel(display, in: labelRect, lit: isLit, alpha: a,
                                           chroma: Self.chromaticColorByPitchClass[m % 12],
                                           uppercase: Self.uppercaseForMidi(m))
                        }
                    }
                }
                for m in (firstMidi...max(firstMidi, lastMidi)) where lastMidi >= firstMidi && !isWhite(m) {
                    if !isActive(m) { continue }   // negative space
                    var leftWhite = m - 1
                    while !isWhite(leftWhite) { leftWhite -= 1 }
                    guard let leftIdx = whiteIndex[leftWhite] else { continue }
                    // Vibe shakes only the letter (see white-key note) —
                    // the keycap stays put.
                    let vibe = Self.percussionVibe(forMidi: m)
                    let rect = blackRect(rightOfWhiteIndex: leftIdx + slotOffset)
                    let isLit = tileOffset == 0 && litNotes.contains(UInt8(m))
                    let isPlaybackLit = tileOffset == 0 && playbackLitNotes.contains(UInt8(m))
                    let isHover = tileOffset == 0 && hovered == .note(UInt8(m))
                    let path = roundedKeyPath(rect: rect, tl: 0, tr: 0, br: 1.2, bl: 1.2)
                    if isPlaybackLit {
                        playbackLit.setFill()
                        path.fill()
                    } else if isLit {
                        if isDark {
                            // Invert in dark mode — the saturated bright
                            // sharp flips to a deep slate notch on press
                            // so the key feels recessed into the keybed
                            // instead of getting brighter on top of an
                            // already-glowing surface.
                            NSColor(srgbRed: 22/255, green: 30/255,
                                    blue: 36/255, alpha: 1).setFill()
                            path.fill()
                        } else {
                            lit.setFill()
                            path.fill()
                        }
                    } else if isDark {
                        // Sharps glow with the system color in dark
                        // mode — same backlit-organ feel as the
                        // chromatic stripe under the naturals.
                        Self.withGlow(color: NSColor.controlAccentColor,
                                      blur: 3.5,
                                      alpha: 0.55) {
                            NSGradient(starting: blackHi, ending: blackLo)!
                                .draw(in: path, angle: -90)
                        }
                    } else {
                        NSGradient(starting: blackHi, ending: blackLo)!.draw(in: path, angle: -90)
                    }
                    if isHover && !isLit {
                        NSColor.white.withAlphaComponent(0.20).setFill()
                        path.fill()
                    }
                    // Percussion split: tint the right-hand sharps (the
                    // accent drums — crash/splash/cowbell/block/tambo) with
                    // their pad color. Higher alpha than the naturals so it
                    // reads against the dark keycap.
                    if let drumColor = Self.percussionDrumColor(forMidi: m), !isLit {
                        let b = vibe.blink
                        let c = b > 0.01
                            ? (drumColor.blended(withFraction: b * 0.85, of: .white) ?? drumColor)
                            : drumColor
                        c.withAlphaComponent(min(1.0, 0.72 + b * 0.28)).setFill()
                        path.fill()
                    }
                    groove.setStroke()
                    path.lineWidth = 0.6
                    path.stroke()
                    if tileOffset == 0, let letter = labelByMidi[m] {
                        let display = Self.uppercaseForMidi(m) ? letter.uppercased() : letter
                        let a: CGFloat
                        if isLit {
                            a = 1.0
                        } else if let closure = letterAlpha {
                            a = closure(UInt8(m))
                        } else {
                            a = typeMode ? 1.0 : 0.0
                        }
                        if a > 0.01 {
                            // Percussion buzz shakes only the letter rect.
                            let labelRect = rect.offsetBy(dx: vibe.dx, dy: vibe.dy)
                            drawBlackLabel(display, in: labelRect, lit: isLit, alpha: a)
                        }
                    }
                }
                NSGraphicsContext.restoreGraphicsState()
            }
            NSGraphicsContext.restoreGraphicsState()

            if includeSettings && tapeFeatureEnabled {
                // Deck face — transport panel on the right.
                drawDeckPanel(in: deckRect)
                // Drive spokes — two horizontal metal rods extending
                // from the deck's left edge into the cassette's reel
                // centers. Drawn before the cassette so the cassette
                // body covers the inner tips of the spokes.
                drawDriveSpokes(deck: deckRect, cassette: tapeRect)
                // Cassette mounted on the spokes, off to the left of
                // the deck. Full piano height.
                drawTapeWidget(in: tapeRect,
                               hovered: hovered == .tape)
                // Transport buttons mounted on the deck face.
                drawTapeTransportStrip(hovered: hovered)
            }

            if includeSettings {
                // Settings chip — `music.note` glyph on the LEFT (click
                // = popover) + 3 LED visualizer bars on the RIGHT (click
                // = floating play palette). Together they replace the
                // old `music.note.list` SF Symbol — same footprint, but
                // the bars now actually pulse with note activity instead
                // of being decorative staff lines.
                let chipHovered = (hovered == .openSettings) || (hovered == .openVisualizer)
                // 1-based voice display — pressing the digit '1' picks
                // GM program 0 (Acoustic Grand) and the badge reads
                // "1" to match what the user typed. 0 is the MIDI
                // passthrough slot, surfaced as `midiOn` rather than
                // a digit, so there's no off-by-one ambiguity.
                drawSettingsChip(in: settingsRect, hoverRect: settingsHitRect,
                                 midiOn: enabled,
                                 hovered: chipHovered,
                                 flash: settingsFlash,
                                 voiceNumber: Int(melodicProgram) + 1,
                                 voiceLabel: voiceLabel,
                                 visualizerHovered: hovered == .openVisualizer,
                                 visualizerVisible: miniVisualizerVisible,
                                 visualizerLevel: miniVisualizerLevel)
            }
            return true
        }
        img.isTemplate = false
        return img
    }

    /// Public so the popover can anchor its arrow at the latch — point
    /// at the visible icon, not the wider hit area, so the arrow lines
    /// up directly below the music note glyph.
    static var settingsRectPublic: NSRect { settingsIconRect }

    // Settings button hit area extends from the piano's right edge
    // all the way to the image's right edge — including the voice-
    // badge pad — so clicking either the music-note glyph OR the
    // badge digits opens the popover. The icon is positioned via
    // `settingsIconRect` directly (not via this hit rect) so the
    // glyph stays put even though the hit zone now covers the pad.
    private static var settingsHitRect: NSRect {
        // Tape sits at the leading edge; the settings chip hit zone
        // starts AFTER the tape so the cassette stays clickable
        // (REC toggle) and draggable (eject) even in `.compact`.
        let leftX = displayLayout == .compact
            ? tapeReservedWidth
            : pianoOriginX + pianoWidth(layout: .fixedCanvas)
        let rightX = baseImageSize.width
        return NSRect(x: leftX, y: 0, width: rightX - leftX, height: baseImageSize.height)
    }

    /// Visible icon bounds — a pill-sized region centered on the music
    /// note glyph itself. Used both as the hover-highlight backdrop and
    /// as the popover anchor so the popover arrow points right at the
    /// icon. Width tracks `settingsW`; height tracks `settingsH`.
    private static var settingsIconRect: NSRect {
        let w = settingsW
        let h = settingsH
        // Anchor the icon to the image's "old" right edge — i.e.
        // before the voice-badge pad was added. Otherwise the
        // music-note glyph would drift right whenever the badge pad
        // claims its slice of the image.
        let oldRight = baseImageSize.width - voiceBadgeRightPad
        let cx = oldRight - w / 2 - pad
        // Nudge the chip slightly UP so the SF Symbol `music.note`
        // glyph — which is visually bottom-heavy (the stem extends
        // upward from a wide note head) — reads as centered in the
        // menubar slot. Without this the icon sits a touch low.
        let cy = baseImageSize.height / 2 + 1.2
        return NSRect(x: cx - w / 2, y: cy - h / 2, width: w, height: h)
    }

    // MARK: - Hit testing

    static func hit(at rawPoint: NSPoint) -> HitResult? {
        // The displayed icon is magnified by `iconScale`; map the click back
        // into the base coordinate space the hit rects live in.
        let point = NSPoint(x: rawPoint.x / iconScale, y: rawPoint.y / iconScale)
        // REC dot — fixed slot at the far leading edge, before everything.
        if recHitRect.contains(point) { return .recDot }
        if tapeFeatureEnabled {
            // Tape sits at the leading edge — checked first so the
            // cassette wins over any speculative key hit math that
            // might wander left of the piano origin.
            if tapeRect.contains(point) { return .tape }
            // Transport buttons immediately right of the cassette.
            // Order matches Walkman: REW · STOP · PLAY · FFWD · REC · EJECT.
            for i in 0..<transportButtonCount {
                if transportButtonRect(i).contains(point) {
                    switch i {
                    case 0: return .tapeRew
                    case 1: return .tapeStop
                    case 2: return .tapePlay
                    case 3: return .tapeFfwd
                    case 4: return .tapeRec
                    case 5: return .tapeEject
                    default: break
                    }
                }
            }
        }
        // Visualizer is a sub-rect inside the settings chip so it has
        // to be tested *before* the broader settings hit zone.
        if miniVisualizerVisible && miniVisualizerRect.contains(point) {
            return .openVisualizer
        }
        if settingsHitRect.contains(point) { return .openSettings }
        let whites = whiteList()
        var whiteIndex: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { whiteIndex[m] = i }
        let slotOffset = slotOffset(layout: .fixedCanvas)
        // Black-key hit area = the visual blackRect. 1:1 mapping with what
        // the user sees on screen — clicking on visible black triggers black,
        // clicking visible white triggers white. Inactive (negative-space)
        // keys are non-interactive.
        for m in (firstMidi...max(firstMidi, lastMidi)) where lastMidi >= firstMidi && !isWhite(m) {
            if !isActive(m) { continue }
            var leftWhite = m - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = whiteIndex[leftWhite] else { continue }
            if blackRect(rightOfWhiteIndex: leftIdx + slotOffset).contains(point) {
                return .note(UInt8(m))
            }
        }
        // White keys: y is unbounded so any cursor-y inside the button maps
        // to the white at that x — even when the menubar adds an extra pixel
        // or two of padding above/below the image, those edge clicks still
        // register as the underlying white. Black-band check above already
        // claims the black-key region; everything else falls through here.
        for (idx, m) in whites.enumerated() {
            if !isActive(m) { continue }
            let r = whiteRect(at: idx + slotOffset)
            let relaxed = NSRect(x: r.minX, y: -100,
                                 width: r.width, height: 200)
            if relaxed.contains(point) { return .note(UInt8(m)) }
        }
        return nil
    }

    /// Public lookup so callers (drag handler) can compute cursor-relative
    /// expression — e.g. y→velocity, x→pan within the active key's bounds.
    static func keyRect(for midi: UInt8, layout: Layout = .fixedCanvas) -> NSRect? {
        let m = Int(midi)
        let whites = whiteList()
        let slotOffset = slotOffset(layout: layout)
        if isWhite(m) {
            guard let idx = whites.firstIndex(of: m) else { return nil }
            return whiteRect(at: idx + slotOffset)
        } else {
            var leftWhite = m - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = whites.firstIndex(of: leftWhite) else { return nil }
            return blackRect(rightOfWhiteIndex: leftIdx + slotOffset)
        }
    }

    /// Drag-friendly hit test for piano keys only. Vertically forgiving (y
    /// can be anywhere); horizontally tolerates a small overshoot past the
    /// leftmost/rightmost white key so a drag rolling past the edge keeps
    /// the edge key sounding.
    static func noteAt(_ rawPoint: NSPoint, layout: Layout = .fixedCanvas) -> UInt8? {
        // Menubar canvas is magnified by iconScale — map clicks back to base
        // coords. The 1× floating palette passes its own layout, untouched.
        let point = layout == .fixedCanvas
            ? NSPoint(x: rawPoint.x / iconScale, y: rawPoint.y / iconScale)
            : rawPoint
        let whites = whiteList()
        let activeWhites = activeWhites(layout: layout)
        guard !activeWhites.isEmpty else { return nil }
        let slotOffset = slotOffset(layout: layout)
        // Active keys occupy slots [slotOffset, slotOffset + activeWhites.count - 1].
        let leftEdge  = pianoOriginX + CGFloat(slotOffset) * whiteW
        let rightEdge = pianoOriginX + CGFloat(slotOffset + activeWhites.count) * whiteW
        let edgeTolerance: CGFloat = whiteW * 0.6
        guard point.x >= leftEdge - edgeTolerance,
              point.x < rightEdge + edgeTolerance else { return nil }

        // Black-key band: matches the visual blackRect exactly. Inactive
        // black keys are negative space — skipped.
        let blackYMin = pad + (whiteH - blackH)
        if point.x >= leftEdge && point.x < rightEdge && point.y >= blackYMin {
            var whiteIndex: [Int: Int] = [:]
            for (i, m) in whites.enumerated() { whiteIndex[m] = i }
            for m in (firstMidi...max(firstMidi, lastMidi)) where lastMidi >= firstMidi && !isWhite(m) {
                if !isActive(m) { continue }
                var leftWhite = m - 1
                while !isWhite(leftWhite) { leftWhite -= 1 }
                guard let leftIdx = whiteIndex[leftWhite] else { continue }
                let rect = blackRect(rightOfWhiteIndex: leftIdx + slotOffset)
                if point.x >= rect.minX && point.x < rect.maxX { return UInt8(m) }
            }
        }
        // White by slot within the active range; overshoot clamps to the
        // outermost active white.
        let clampedX = max(leftEdge, min(rightEdge - 0.001, point.x))
        let localCol = Int((clampedX - leftEdge) / whiteW)
        let clamped = max(0, min(activeWhites.count - 1, localCol))
        return UInt8(activeWhites[clamped])
    }

    // MARK: - Layout helpers

    private static func activeWhites(layout: Layout) -> [Int] {
        let whites = whiteList()
        switch layout {
        case .fixedCanvas:
            return whites.filter { isActive($0) }
        case .tightActiveRange:
            return whites.filter { isActive($0) }
        }
    }

    private static func slotOffset(layout: Layout) -> Int {
        switch layout {
        case .fixedCanvas:
            return activeSlotOffset
        case .tightActiveRange:
            return 0
        }
    }

    private static func pianoWidth(layout: Layout) -> CGFloat {
        switch layout {
        case .fixedCanvas:
            return CGFloat(whiteList().count) * whiteW
        case .tightActiveRange:
            return CGFloat(activeWhites(layout: layout).count) * whiteW
        }
    }

    private static func whiteRect(at index: Int) -> NSRect {
        let x = pianoOriginX + CGFloat(index) * whiteW
        return NSRect(x: x, y: pad, width: whiteW, height: whiteH)
    }

    private static func blackRect(rightOfWhiteIndex idx: Int) -> NSRect {
        let xCenter = pianoOriginX + CGFloat(idx + 1) * whiteW
        let x = xCenter - blackW / 2.0
        let y = pad + (whiteH - blackH)
        return NSRect(x: x, y: y, width: blackW, height: blackH)
    }

    private static func roundedKeyPath(rect: NSRect, tl: CGFloat, tr: CGFloat,
                                       br: CGFloat, bl: CGFloat) -> NSBezierPath {
        let path = NSBezierPath()
        let minX = rect.minX, maxX = rect.maxX, minY = rect.minY, maxY = rect.maxY
        path.move(to: NSPoint(x: minX + bl, y: minY))
        path.line(to: NSPoint(x: maxX - br, y: minY))
        if br > 0 {
            path.appendArc(withCenter: NSPoint(x: maxX - br, y: minY + br),
                           radius: br, startAngle: 270, endAngle: 360)
        }
        path.line(to: NSPoint(x: maxX, y: maxY - tr))
        if tr > 0 {
            path.appendArc(withCenter: NSPoint(x: maxX - tr, y: maxY - tr),
                           radius: tr, startAngle: 0, endAngle: 90)
        }
        path.line(to: NSPoint(x: minX + tl, y: maxY))
        if tl > 0 {
            path.appendArc(withCenter: NSPoint(x: minX + tl, y: maxY - tl),
                           radius: tl, startAngle: 90, endAngle: 180)
        }
        path.line(to: NSPoint(x: minX, y: minY + bl))
        if bl > 0 {
            path.appendArc(withCenter: NSPoint(x: minX + bl, y: minY + bl),
                           radius: bl, startAngle: 180, endAngle: 270)
        }
        path.close()
        return path
    }

    // MARK: - Color helpers

    /// Pump HSB saturation + brightness on the system accent so the
    /// dark-mode sharps glow with a saturated version of the user's
    /// system color instead of a muddy shadow.
    private static func boostedAccent(saturationBoost: CGFloat,
                                      brightnessBoost: CGFloat) -> NSColor {
        let base = NSColor.controlAccentColor.usingColorSpace(.sRGB)
            ?? NSColor.controlAccentColor
        var h: CGFloat = 0, s: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        base.getHue(&h, saturation: &s, brightness: &b, alpha: &a)
        let s2 = min(1, s + (1 - s) * saturationBoost)
        let b2 = min(1, b + (1 - b) * brightnessBoost)
        return NSColor(hue: h, saturation: s2, brightness: b2, alpha: a)
    }

    /// Apply a soft NSShadow glow inside `body` — same hue radiating
    /// outward, no offset, decent blur. Reads like a backlit organ
    /// key with light leaking around its edges. The shadow state is
    /// scoped to one save/restore so it never leaks to later draws.
    private static func withGlow(color: NSColor,
                                 blur: CGFloat,
                                 alpha: CGFloat,
                                 _ body: () -> Void) {
        NSGraphicsContext.saveGraphicsState()
        let glow = NSShadow()
        glow.shadowColor = color.withAlphaComponent(alpha)
        glow.shadowBlurRadius = blur
        glow.shadowOffset = .zero
        glow.set()
        body()
        NSGraphicsContext.restoreGraphicsState()
    }

    // MARK: - Key labels

    private static func drawWhiteLabel(_ text: String, in rect: NSRect, lit: Bool, alpha: CGFloat = 1.0, bottomOffset: CGFloat = 0, chroma: NSColor? = nil, uppercase: Bool = false) {
        guard alpha > 0.01 else { return }
        // Lit keys fill with the system accent — label flips to a
        // dark on-color shade so the letter reads as ink stamped on
        // the colored keycap rather than glowing white. Idle keys
        // adapt to system theme: near-black on light off-white,
        // near-white on dark slate. (chroma is unused now but kept
        // for any future per-note label tinting.)
        _ = chroma
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let unlit: NSColor = isDark
            ? NSColor(white: 0.92, alpha: 1.0)
            : NSColor(white: 0.28, alpha: 1.0)
        let base: NSColor = lit ? NSColor(white: 0.12, alpha: 1.0) : unlit
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 9.0, weight: .heavy),
            .foregroundColor: base.withAlphaComponent(alpha),
        ]
        let str = NSAttributedString(string: text, attributes: attrs)
        let size = str.size()
        // White key labels sit a few pixels off the bottom — high
        // enough that the descender on `j` doesn't kiss the menubar
        // edge and that the letter floats clearly above the
        // chromatic stripe at the keycap's foot. Caps drop ~1pt
        // lower so the taller uppercase glyphs don't bump into the
        // black-key label band above. In fullSlim (thin-keys)
        // mode the whole keycap stack is squashed; the labels were
        // floating too high inside the cap, so push them down 2pt
        // so they sit grounded on the chromatic stripe.
        let slim = displayLayout == .fullSlim
        // Only the keys whose own label is uppercased drop lower — so a
        // one-sided shift nudges just that half, not the whole row.
        let baseY: CGFloat = (uppercase ? 2.0 : 3.0) - (slim ? 2.0 : 0)
        str.draw(at: NSPoint(x: rect.midX - size.width / 2,
                             y: rect.minY + baseY + bottomOffset))
    }

    private static func drawBlackLabel(_ text: String, in rect: NSRect, lit: Bool, alpha: CGFloat = 1.0) {
        guard alpha > 0.01 else { return }
        // Sharp body brightness depends on the *XOR* of lit + dark
        // — dark-mode unlit sharps glow saturated accent, dark-mode
        // lit sharps invert to deep slate; light-mode is the
        // opposite (unlit dark accent, lit bright accent). Pick the
        // label color from whichever surface the letter actually
        // lands on.
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let onBrightFill = lit != isDark
        let foreground: NSColor = onBrightFill
            ? NSColor(white: 0.10, alpha: 1.0)
            : NSColor.white
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 8.0, weight: .heavy),
            .foregroundColor: foreground.withAlphaComponent(0.96 * alpha),
        ]
        let str = NSAttributedString(string: text, attributes: attrs)
        let size = str.size()
        str.draw(at: NSPoint(x: rect.midX - size.width / 2,
                             y: rect.midY - size.height / 2))
    }

    /// Prefer Processing's bundled typeface for that Processing-IDE look. The
    /// font name varies across releases; try a few. Falls back to the system
    /// monospaced font (heavy) so the UI stays legible if it isn't installed.
    private static func processingFont(size: CGFloat) -> NSFont {
        let names = [
            "ProcessingSansPro-Bold",
            "Processing-Sans-Pro-Bold",
            "Processing Sans Pro Bold",
            "ProcessingSansPro-Regular",
            "Processing Sans Pro",
            "Processing",
        ]
        for n in names {
            if let f = NSFont(name: n, size: size) { return f }
        }
        return NSFont.monospacedSystemFont(ofSize: size, weight: .heavy)
    }

    // MARK: - Header buttons (flat, hover-aware)

    private static func drawHoverBackdrop(in rect: NSRect, hovered: Bool) {
        guard hovered else { return }
        let r = rect.insetBy(dx: 1.0, dy: 1.5)
        let path = NSBezierPath(roundedRect: r, xRadius: 3, yRadius: 3)
        NSColor.labelColor.withAlphaComponent(0.10).setFill()
        path.fill()
    }

    /// Music-notation chip — `music.note.list` reads like notes on a staff
    /// (a tiny scroll of music), more on-brand for a menubar instrument
    /// than a generic settings glyph. Tints accent + brightens when MIDI
    /// is sending to a DAW so you can see port status at a glance.
    ///
    /// Alternates considered: "music.quarternote.3", "pianokeys",
    /// "music.note", "scroll", "speaker.wave.2", "waveform".
    /// Three thin VERTICAL VU bars occupying the EXACT bounding box
    /// the staff lines of SF Symbol music.note.list occupied at
    /// pointSize 13 (measured by rendering the symbol and scanning the
    /// pixel buffer). Bar heights track the smoothed `level` plus a
    /// phase-shifted sine wiggle keyed to `miniVisualizerPhase` so the
    /// meter feels live even when the level is steady — reads as
    /// audio metering, not a binary-toggle indicator.
    /// Single amplitude square (replaces the prior 3-bar VU). Side
    /// scales with the smoothed `level` from 0 (invisible) to the
    /// band's height — at peak the square fills the entire staff-
    /// lines slot. Centered horizontally + vertically inside `rect`
    /// so it reads as a unified pulsing block, not a meter chasing
    /// a baseline.
    private static func drawChipAmplitudeSquare(in rect: NSRect, level: CGFloat,
                                                hovered: Bool, color: NSColor,
                                                baseAlpha: CGFloat) {
        let lvl = max(0, min(1, level))
        // Quarter-power compander stretches the low end so quiet
        // sustain plays still drive the square visibly.
        let dramatic = pow(lvl, 0.45)
        let alpha: CGFloat = hovered ? 1.0 : baseAlpha
        // Side caps at the smaller of width/height so the square
        // doesn't squish into a rectangle; the staff-lines band is
        // 5.75pt tall × 6.5pt wide so the cap is height-limited.
        let maxSide = min(rect.width, rect.height)
        let side = dramatic * maxSide
        if side < 0.6 { return }   // skip drawing when essentially zero
        let sq = NSRect(x: rect.midX - side / 2,
                        y: rect.midY - side / 2,
                        width: side, height: side)
        color.withAlphaComponent(alpha).setFill()
        NSBezierPath(roundedRect: sq, xRadius: 0.6, yRadius: 0.6).fill()
    }

    /// Linearly interpolates between the idle "3 horizontal staff
    /// lines" silhouette of music.note.list and the active "3 vertical
    /// VU bars" silhouette as `level` rises and falls. Audio attack
    /// pushes the lines outward into bars; audio decay slides them
    /// back into staff lines. Same drawer handles every t in [0, 1],
    /// so the transition is continuous and reversible.
    /// Empty square outline that pops a fill on each MIDI event.
    /// Used in place of the 3-bar VU when the chip is in MIDI
    /// mode — same visual language as Ableton's MIDI indicator
    /// dot.
    private static func drawChipMidiSquare(in rect: NSRect,
                                            flash: CGFloat,
                                            hovered: Bool,
                                            color: NSColor,
                                            baseAlpha: CGFloat) {
        // Square fits the band's smaller dimension, centered.
        let side = min(rect.width, rect.height)
        let frame = NSRect(
            x: rect.midX - side / 2,
            y: rect.midY - side / 2,
            width: side,
            height: side
        )
        let alpha: CGFloat = hovered ? 1.0 : baseAlpha
        // No outline at rest — the slot is empty until a MIDI event
        // hits, at which point a solid filled square pops in and
        // decays back to nothing. (Ableton-style activity blip.)
        let f = max(0, min(1, flash))
        if f > 0.02 {
            let fill = NSBezierPath(rect: frame)
            color.withAlphaComponent(alpha * f).setFill()
            fill.fill()
        }
    }

    /// Tiny rotating needle inside the chip's visualizer slot —
    /// pivots from the bottom-center, swings ±25° at ~1 Hz, just
    /// like the popover's metronome trapezoid. Reads as "metronome
    /// running" without competing with the music-note glyph.
    private static func drawChipMetronomeWave(in rect: NSRect,
                                                hovered: Bool,
                                                color: NSColor,
                                                baseAlpha: CGFloat) {
        let alpha: CGFloat = hovered ? 1.0 : baseAlpha
        // Sync to the popover's metronome by computing phase from
        // (now − metronomeStartTime) / period, where period =
        // 60/BPM × 2. Same formula the popover uses for its own
        // CAKeyframeAnimation duration, so the two needles stay
        // in lockstep regardless of BPM changes.
        let period = 60.0 / Double(max(1, metronomeBPM)) * 2.0
        let elapsed = CACurrentMediaTime() - metronomeStartTime
        let phase = CGFloat(elapsed.truncatingRemainder(dividingBy: period) / period) * .pi * 2
        let maxAngle: CGFloat = 25.0 * .pi / 180.0
        let angle = sin(phase) * maxAngle
        let pivotX = rect.midX
        let pivotY = rect.minY + 0.5
        let length = rect.height - 1.5
        // CALayer.transform.rotation.z rotates COUNTER-CLOCKWISE for
        // positive angles, so the popover needle tilts LEFT at +amp.
        // Mirror that here — without the sign flip, the chip needle
        // ends up swinging the exact opposite direction from the
        // popover's trapezoid.
        let tipX = pivotX - sin(angle) * length
        let tipY = pivotY + cos(angle) * length
        let path = NSBezierPath()
        path.lineWidth = 1.1
        path.lineCapStyle = .round
        path.move(to: NSPoint(x: pivotX, y: pivotY))
        path.line(to: NSPoint(x: tipX, y: tipY))
        color.withAlphaComponent(alpha).setStroke()
        path.stroke()
        // Tiny bob at the tip so the needle reads as a real
        // pendulum and not just a tick mark — matches the dot the
        // popover's metronome carries.
        let bobR: CGFloat = 0.9
        let bob = NSBezierPath(ovalIn: NSRect(
            x: tipX - bobR, y: tipY - bobR,
            width: bobR * 2, height: bobR * 2
        ))
        color.withAlphaComponent(alpha).setFill()
        bob.fill()
    }

    // MARK: - Tape widget

    /// Skeuomorphic cassette in the status item: brushed-silver body,
    /// label card across the top, two hexagonal reels below, REC dot
    /// + mic LED, a thin progress bar tracking 1:30 capacity.
    ///
    /// Reference notes: Walkman TPS-L2 (silver body + blue accent +
    /// "hotline mic" orange button), Maxell XL II (label-card stripe
    /// + hexagonal reel hubs), Tascam Portastudio (red REC dot tinted
    /// against a black chassis).
    private static func drawTapeWidget(in rect: NSRect, hovered: Bool) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua

        ctx.saveGState()
        defer { ctx.restoreGState() }

        // Body — slightly rounded rect with a 0.6pt bevel. In light
        // mode we tint toward navy (Walkman TPS-L2 face plate) so the
        // cassette reads as a distinct artifact against the white
        // menubar; dark mode goes to a deeper steel that doesn't
        // disappear into the bar.
        let bodyHi: NSColor
        let bodyLo: NSColor
        if isDark {
            bodyHi = NSColor(srgbRed:  78/255, green:  88/255,
                              blue: 100/255, alpha: 1)
            bodyLo = NSColor(srgbRed:  42/255, green:  50/255,
                              blue:  60/255, alpha: 1)
        } else {
            bodyHi = NSColor(srgbRed:  85/255, green: 102/255,
                              blue: 132/255, alpha: 1)   // navy steel
            bodyLo = NSColor(srgbRed:  48/255, green:  60/255,
                              blue:  85/255, alpha: 1)
        }
        let bodyPath = NSBezierPath(roundedRect: rect.insetBy(dx: 0.5, dy: 0.5),
                                    xRadius: 2.2, yRadius: 2.2)
        let bodyGradient = NSGradient(colors: [bodyHi, bodyLo],
                                       atLocations: [0.0, 1.0],
                                       colorSpace: .sRGB)
        bodyGradient?.draw(in: bodyPath, angle: -90)
        // Top bevel — single bright pixel row to catch light.
        NSColor.white.withAlphaComponent(isDark ? 0.18 : 0.28).setStroke()
        bodyPath.lineWidth = 0.6
        bodyPath.stroke()

        // Label card across the top half. Cream in light, warm beige
        // tilted dark in dark mode so the contrast against the body
        // stays readable but the cassette doesn't look like it has a
        // glaring white sticker.
        let labelInset: CGFloat = 1.6
        let labelH: CGFloat = rect.height * 0.36
        let labelRect = NSRect(x: rect.minX + labelInset,
                                y: rect.maxY - labelH - 1.2,
                                width:  rect.width  - labelInset * 2,
                                height: labelH)
        let labelHi: NSColor
        let labelLo: NSColor
        if isDark {
            labelHi = NSColor(srgbRed: 230/255, green: 220/255,
                               blue: 195/255, alpha: 1)
            labelLo = NSColor(srgbRed: 195/255, green: 180/255,
                               blue: 150/255, alpha: 1)
        } else {
            labelHi = NSColor(srgbRed: 250/255, green: 245/255,
                               blue: 225/255, alpha: 1)
            labelLo = NSColor(srgbRed: 230/255, green: 220/255,
                               blue: 195/255, alpha: 1)
        }
        let labelPath = NSBezierPath(roundedRect: labelRect,
                                     xRadius: 0.8, yRadius: 0.8)
        NSGradient(colors: [labelHi, labelLo],
                   atLocations: [0.0, 1.0],
                   colorSpace: .sRGB)?.draw(in: labelPath, angle: -90)
        // Thin stripe across the bottom of the label — TDK SA gold
        // stripe homage. Color shifts with state so the cassette
        // reads "armed/playing" at a glance even when the reels are
        // too small to see motion.
        let stripeColor: NSColor
        if tapeRecording {
            stripeColor = NSColor(srgbRed: 220/255, green: 60/255,
                                   blue: 60/255, alpha: 1)
        } else if tapePlaying {
            stripeColor = NSColor(srgbRed: 90/255, green: 180/255,
                                   blue: 90/255, alpha: 1)
        } else {
            stripeColor = NSColor(srgbRed: 210/255, green: 175/255,
                                   blue:  80/255, alpha: 1)   // TDK gold
        }
        let stripeRect = NSRect(x: labelRect.minX,
                                 y: labelRect.minY,
                                 width: labelRect.width,
                                 height: 1.0)
        stripeColor.setFill()
        NSBezierPath(rect: stripeRect).fill()

        // Progress mark — a tiny tick that slides across the stripe
        // tracking the fill (during REC) or playhead (during PLAY).
        let frac: CGFloat
        if tapeRecording {
            frac = min(1, max(0, tapeFillFraction))
        } else {
            frac = min(1, max(0, tapePlayheadFraction))
        }
        if frac > 0.001 {
            let tickW: CGFloat = 1.5
            let tickX = stripeRect.minX + frac * (stripeRect.width - tickW)
            let tickRect = NSRect(x: tickX, y: stripeRect.minY - 0.5,
                                   width: tickW, height: 2.0)
            NSColor.white.withAlphaComponent(0.85).setFill()
            NSBezierPath(rect: tickRect).fill()
        }

        // Two hexagonal reels. Hub-spoke geometry reads as "tape" even
        // at 16pt — circles alone look like a generic dial.
        let reelAreaY = rect.minY + 1.4
        let reelAreaH = labelRect.minY - reelAreaY - 1.0
        let reelR: CGFloat = min(reelAreaH * 0.45, rect.width * 0.18)
        let reelGap: CGFloat = rect.width * 0.22
        let reelCY = reelAreaY + reelAreaH / 2
        let leftCX = rect.midX - reelGap
        let rightCX = rect.midX + reelGap

        // Reel rotation speed — slow turn at idle (always-on lazy
        // shimmer), faster during PLAY, fastest during REC.
        let angularSpeed: CGFloat
        if tapeRecording { angularSpeed = 4.5 }
        else if tapePlaying { angularSpeed = 3.2 }
        else { angularSpeed = 0.0 }
        let phase = CGFloat(tapePhase) * angularSpeed
        drawTapeReel(centerX: leftCX, centerY: reelCY,
                     radius: reelR, rotation: phase, isDark: isDark)
        drawTapeReel(centerX: rightCX, centerY: reelCY,
                     radius: reelR, rotation: -phase + 0.5, isDark: isDark)

        // REC dot — sits in the label card, between the reels.
        // Pulses with a sine envelope while recording so the user's
        // peripheral vision registers "still rolling."
        if tapeRecording {
            let dotR: CGFloat = 1.7
            let pulseT = 0.5 + 0.5 * sin(CGFloat(tapePhase) * 6.0)
            let alpha: CGFloat = 0.7 + 0.3 * pulseT
            let dotRect = NSRect(x: labelRect.midX - dotR,
                                  y: labelRect.midY - dotR + 0.2,
                                  width: dotR * 2,
                                  height: dotR * 2)
            NSColor(srgbRed: 235/255, green: 50/255, blue: 50/255,
                    alpha: alpha).setFill()
            NSBezierPath(ovalIn: dotRect).fill()
        }

        // Mic LED — a tiny orange dot in the upper-right corner of
        // the label, lit while the hot mic is delivering frames
        // (i.e. while REC is engaged AND mic access is granted).
        // Walkman TPS-L2 "hotline" button orange.
        if tapeMicHot {
            let micR: CGFloat = 0.9
            let micRect = NSRect(x: labelRect.maxX - micR * 2 - 1.0,
                                  y: labelRect.maxY - micR * 2 - 0.6,
                                  width: micR * 2,
                                  height: micR * 2)
            NSColor(srgbRed: 255/255, green: 150/255, blue: 40/255,
                    alpha: 0.95).setFill()
            NSBezierPath(ovalIn: micRect).fill()
        }

        // Hover affordance — light edge so the user can tell the
        // cassette is interactive without us drawing a literal
        // button border.
        if hovered {
            NSColor.white.withAlphaComponent(0.35).setStroke()
            let hp = NSBezierPath(roundedRect: rect.insetBy(dx: 0.5, dy: 0.5),
                                  xRadius: 2.2, yRadius: 2.2)
            hp.lineWidth = 1.2
            hp.stroke()
        }
    }

    /// One reel: outer rim, hexagonal hub with 6 spokes radiating to
    /// the rim. Rotation is in radians.
    private static func drawTapeReel(centerX: CGFloat, centerY: CGFloat,
                                      radius: CGFloat, rotation: CGFloat,
                                      isDark: Bool) {
        // Reel window — a black disc cut into the cassette body that
        // the reel sits in. Reels themselves are slightly lighter so
        // they pop forward.
        let windowRect = NSRect(x: centerX - radius - 0.4,
                                 y: centerY - radius - 0.4,
                                 width: (radius + 0.4) * 2,
                                 height: (radius + 0.4) * 2)
        NSColor.black.withAlphaComponent(isDark ? 0.55 : 0.65).setFill()
        NSBezierPath(ovalIn: windowRect).fill()

        // Reel disc — the "tape spool" with a hexagonal hub. Color
        // edges toward warm tape brown so it reads as wound tape
        // even at 5pt radius.
        let reelRect = NSRect(x: centerX - radius,
                               y: centerY - radius,
                               width: radius * 2,
                               height: radius * 2)
        let reelColor = NSColor(srgbRed: 70/255, green: 55/255,
                                 blue: 45/255, alpha: 1.0)
        reelColor.setFill()
        NSBezierPath(ovalIn: reelRect).fill()

        // Hexagonal hub — 6 vertices around centerX/centerY, rotated
        // by `rotation`. The hub itself is brighter than the spool
        // edge so it reads as the plastic reel core, not part of the
        // wound tape.
        let hubR = radius * 0.55
        let hubPath = NSBezierPath()
        for i in 0..<6 {
            let theta = rotation + CGFloat(i) * (.pi / 3)
            let x = centerX + cos(theta) * hubR
            let y = centerY + sin(theta) * hubR
            if i == 0 { hubPath.move(to: NSPoint(x: x, y: y)) }
            else      { hubPath.line(to: NSPoint(x: x, y: y)) }
        }
        hubPath.close()
        NSColor(srgbRed: 200/255, green: 195/255, blue: 185/255,
                alpha: 1.0).setFill()
        hubPath.fill()

        // Center dot — captures the "drive spindle hole" that all
        // cassettes have. Same color as the body so it visually
        // recedes.
        let pinR: CGFloat = max(0.45, radius * 0.12)
        let pinRect = NSRect(x: centerX - pinR, y: centerY - pinR,
                              width: pinR * 2, height: pinR * 2)
        NSColor.black.withAlphaComponent(0.85).setFill()
        NSBezierPath(ovalIn: pinRect).fill()

        // One bright spoke — gives the rotation a clearer visual
        // anchor than the symmetric hexagon alone provides at this
        // size. Slim trapezoid from hub to rim, painted white at low
        // alpha so it reads as a wound-tape highlight catching light.
        let spokePath = NSBezierPath()
        let spokeWidthTheta: CGFloat = 0.20
        let spokeInner = hubR * 0.95
        let spokeOuter = radius * 0.92
        let cosL = cos(rotation - spokeWidthTheta)
        let sinL = sin(rotation - spokeWidthTheta)
        let cosR = cos(rotation + spokeWidthTheta)
        let sinR = sin(rotation + spokeWidthTheta)
        spokePath.move(to: NSPoint(x: centerX + cosL * spokeInner,
                                    y: centerY + sinL * spokeInner))
        spokePath.line(to: NSPoint(x: centerX + cosL * spokeOuter,
                                    y: centerY + sinL * spokeOuter))
        spokePath.line(to: NSPoint(x: centerX + cosR * spokeOuter,
                                    y: centerY + sinR * spokeOuter))
        spokePath.line(to: NSPoint(x: centerX + cosR * spokeInner,
                                    y: centerY + sinR * spokeInner))
        spokePath.close()
        NSColor.white.withAlphaComponent(0.45).setFill()
        spokePath.fill()
    }

    private static func drawChipVisualizer(in rect: NSRect, level: CGFloat,
                                           hovered: Bool, color: NSColor,
                                           baseAlpha: CGFloat) {
        let alpha: CGFloat = hovered ? 1.0 : baseAlpha
        color.withAlphaComponent(alpha).setFill()

        // Three real bars — bass / mid / treble RMS of the live waveform,
        // already fast-smoothed by the AppDelegate tick. No decorative
        // wiggle: the heights ARE the audio.
        let bars: [CGFloat] = miniVisualizerBars.count >= 3
            ? miniVisualizerBars : [level, level, level]
        let barCount = 3
        let barW: CGFloat = 1.6
        let barGap = (rect.width - CGFloat(barCount) * barW) / CGFloat(barCount - 1)

        // Silence → a single flat baseline line across the slot instead of
        // three idle nubs. The bars rise out of this line as sound returns.
        let peak = bars.prefix(3).max() ?? 0
        if peak < 0.05 {
            let lineH: CGFloat = 1.0
            let line = NSRect(x: rect.minX, y: rect.minY, width: rect.width, height: lineH)
            NSBezierPath(roundedRect: line, xRadius: 0.5, yRadius: 0.5).fill()
            return
        }

        for i in 0..<barCount {
            // Light gamma so quiet detail is visible; min 1px so an active
            // bar never fully vanishes mid-sound.
            let amp = min(1.0, pow(max(0, bars[i]), 0.7))
            let h = max(1.0, amp * rect.height)
            let x = rect.minX + CGFloat(i) * (barW + barGap)
            let bar = NSRect(x: x, y: rect.minY, width: barW, height: h)
            NSBezierPath(roundedRect: bar, xRadius: 0.3, yRadius: 0.3).fill()
        }
    }

    // MARK: - Drive spokes
    //
    // Two horizontal metal rods extend out of the deck's left face,
    // crossing the gap to the cassette and visually engaging with the
    // cassette's two reel centers. Reads as the mechanical drive
    // spindles of a portable deck. Drawn before the cassette so the
    // rod tips disappear behind the cassette body.
    private static func drawDriveSpokes(deck: NSRect, cassette: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        ctx.saveGState()
        defer { ctx.restoreGState() }

        // Reel-center math mirrors `drawTapeWidget` so the spokes
        // line up with the visible hubs no matter how the cassette
        // resizes.
        let reelGap = cassette.width * 0.22
        let reelCY = cassette.minY + cassette.height * 0.32 - 0.4
        let leftReelCY  = reelCY
        let rightReelCY = reelCY
        let _ = leftReelCY  // (reels share Y; vars kept for clarity)
        _ = rightReelCY

        // Rod endpoints: start at deck's left face, end ~1pt INSIDE
        // the cassette so the rod looks plugged into the hub.
        let xStart = deck.minX - 0.2
        let xEnd   = cassette.maxX + 1.0
        // Two rods — left rod aligns with cassette's LEFT reel,
        // right rod with cassette's RIGHT reel.
        let leftReelLine  = cassette.midX - reelGap
        let rightReelLine = cassette.midX + reelGap
        _ = leftReelLine
        _ = rightReelLine

        // Rod thickness ~ 1.8pt; centered on the reel Y.
        let rodH: CGFloat = 1.8
        let rodRect = { (cy: CGFloat) -> NSRect in
            NSRect(x: xEnd, y: cy - rodH / 2,
                   width: xStart - xEnd, height: rodH)
        }

        // The two reels share the same Y, so a single horizontal rod
        // would only need one Y. Real deck mechanisms have ONE driven
        // capstan + idler. Mimic that: one chunkier upper rod
        // (capstan) + one thinner lower rod (idler) to give the
        // illusion of mechanical complexity.
        let capstanY = reelCY + 1.4
        let idlerY   = reelCY - 2.4

        let metalHi: NSColor
        let metalLo: NSColor
        if isDark {
            metalHi = NSColor(srgbRed: 200/255, green: 205/255, blue: 215/255, alpha: 1)
            metalLo = NSColor(srgbRed: 110/255, green: 115/255, blue: 125/255, alpha: 1)
        } else {
            metalHi = NSColor(srgbRed: 220/255, green: 224/255, blue: 230/255, alpha: 1)
            metalLo = NSColor(srgbRed: 130/255, green: 138/255, blue: 152/255, alpha: 1)
        }

        for cy in [capstanY, idlerY] {
            let r = rodRect(cy)
            let path = NSBezierPath(roundedRect: r,
                                    xRadius: rodH / 2, yRadius: rodH / 2)
            NSGradient(colors: [metalHi, metalLo],
                       atLocations: [0.0, 1.0],
                       colorSpace: .sRGB)?.draw(in: path, angle: -90)
            // Thin dark line under the rod for a touch of dimensional
            // shadow against the menubar.
            NSColor.black.withAlphaComponent(0.30).setStroke()
            path.lineWidth = 0.4
            path.stroke()
        }
    }

    // MARK: - Deck panel (transport face)
    //
    // Bevelled pewter face that houses both the cassette window and the
    // transport row. Drawn first; everything else sits on top.
    private static func drawDeckPanel(in rect: NSRect) {
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        ctx.saveGState()
        defer { ctx.restoreGState() }

        // Outer face — bright porcelain white in light mode (Walkman
        // WM-EX series enamel), graphite in dark mode. Very subtle
        // vertical gradient so the face still reads as 3D plastic
        // rather than a flat paper rectangle.
        let faceHi: NSColor
        let faceLo: NSColor
        if isDark {
            faceHi = NSColor(srgbRed:  96/255, green: 100/255, blue: 110/255, alpha: 1)
            faceLo = NSColor(srgbRed:  46/255, green:  50/255, blue:  58/255, alpha: 1)
        } else {
            // Near-pure white with a 4% shading drop to the bottom.
            faceHi = NSColor(srgbRed: 254/255, green: 254/255, blue: 254/255, alpha: 1)
            faceLo = NSColor(srgbRed: 238/255, green: 240/255, blue: 244/255, alpha: 1)
        }
        let facePath = NSBezierPath(roundedRect: rect.insetBy(dx: 0.4, dy: 0.4),
                                    xRadius: 2.4, yRadius: 2.4)
        NSGradient(colors: [faceHi, faceLo],
                   atLocations: [0.0, 1.0],
                   colorSpace: .sRGB)?.draw(in: facePath, angle: -90)

        // Top bevel + bottom shadow — pencil-line accents that sell
        // the face as a milled plate, not a sticker. Toned down in
        // light mode so they don't fight the white enamel.
        ctx.saveGState()
        facePath.addClip()
        NSColor.white.withAlphaComponent(isDark ? 0.18 : 0.55).setStroke()
        let topBevel = NSBezierPath()
        topBevel.move(to: NSPoint(x: rect.minX + 1.5, y: rect.maxY - 0.8))
        topBevel.line(to: NSPoint(x: rect.maxX - 1.5, y: rect.maxY - 0.8))
        topBevel.lineWidth = 0.7
        topBevel.stroke()

        NSColor.black.withAlphaComponent(isDark ? 0.45 : 0.10).setStroke()
        let bottomShadow = NSBezierPath()
        bottomShadow.move(to: NSPoint(x: rect.minX + 1.5, y: rect.minY + 0.9))
        bottomShadow.line(to: NSPoint(x: rect.maxX - 1.5, y: rect.minY + 0.9))
        bottomShadow.lineWidth = 0.7
        bottomShadow.stroke()
        ctx.restoreGState()

        // Outer border — soft gray in light mode so the white deck has
        // a defined edge against the menubar.
        NSColor.black.withAlphaComponent(isDark ? 0.60 : 0.18).setStroke()
        facePath.lineWidth = 0.5
        facePath.stroke()
    }

    /// "Clear plastic door" over the cassette compartment — a thin
    /// glossy overlay with a recessed frame, so the cassette reads as
    /// sealed behind a flip-down window. Reference: the smoked-acrylic
    /// door on a Walkman WM-D6C / Nakamichi Dragon's cassette well.
    private static func drawCassetteDoor(over rect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        ctx.saveGState()
        defer { ctx.restoreGState() }

        let frame = rect.insetBy(dx: -1.0, dy: -1.0)
        let framePath = NSBezierPath(roundedRect: frame,
                                     xRadius: 2.2, yRadius: 2.2)

        // Recessed frame — dark inner ring around the cassette window.
        NSColor.black.withAlphaComponent(0.55).setStroke()
        framePath.lineWidth = 0.7
        framePath.stroke()

        // Inner clear pane — wider than the cassette body so the door
        // visibly frames it. Translucent gradient gives the smoked-acrylic
        // sheen without obscuring the reels.
        let panePath = NSBezierPath(roundedRect: frame.insetBy(dx: 0.6, dy: 0.6),
                                    xRadius: 1.6, yRadius: 1.6)
        ctx.saveGState()
        panePath.addClip()

        // Diagonal sheen — bright top-left → dim bottom-right.
        let sheenHi = NSColor.white.withAlphaComponent(0.22)
        let sheenLo = NSColor.white.withAlphaComponent(0.00)
        NSGradient(colors: [sheenHi, sheenLo],
                   atLocations: [0.0, 0.55],
                   colorSpace: .sRGB)?.draw(in: panePath, angle: -55)

        // A thin highlight streak across the top quarter — that classic
        // "glass catching the light" line. Stays under 0.18 alpha so
        // it never obscures the cassette label below.
        let streakRect = NSRect(x: frame.minX + 1.0,
                                 y: frame.maxY - frame.height * 0.30,
                                 width: frame.width - 2.0,
                                 height: frame.height * 0.16)
        let streakHi = NSColor.white.withAlphaComponent(0.18)
        let streakLo = NSColor.white.withAlphaComponent(0.04)
        NSGradient(colors: [streakHi, streakLo],
                   atLocations: [0.0, 1.0],
                   colorSpace: .sRGB)?.draw(in: NSBezierPath(rect: streakRect),
                                            angle: -90)
        ctx.restoreGState()

        // Inner highlight — bright top edge of the pane to sell the
        // door as a 3D piece.
        NSColor.white.withAlphaComponent(0.28).setStroke()
        let topGloss = NSBezierPath()
        topGloss.move(to: NSPoint(x: frame.minX + 1.2, y: frame.maxY - 0.9))
        topGloss.line(to: NSPoint(x: frame.maxX - 1.2, y: frame.maxY - 0.9))
        topGloss.lineWidth = 0.5
        topGloss.stroke()
    }

    // MARK: - Tape transport strip
    //
    // Six skeuomorph chiclets — REW · STOP · PLAY · FFWD · REC · EJECT —
    // styled after the bottom row of a Walkman / Nakamichi Dragon. Pewter
    // bodies with a top highlight bevel and bottom shadow, REC tinted red,
    // PLAY tinted green when engaged. Hover brightens the body; an active
    // state (REC armed, PLAY rolling) draws a pressed-in inverse gradient.
    private static func drawTapeTransportStrip(hovered: HitResult?) {
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let labels: [HitResult] = [
            .tapeRew, .tapeStop, .tapePlay, .tapeFfwd, .tapeRec, .tapeEject,
        ]
        for (i, role) in labels.enumerated() {
            let r = transportButtonRect(i)
            let isHover = hovered == role
            let isActive: Bool
            switch role {
            case .tapeRec:  isActive = tapeRecording
            case .tapePlay: isActive = tapePlaying
            default:        isActive = false
            }
            drawTransportButton(in: r,
                                role: role,
                                hovered: isHover,
                                active: isActive,
                                isDark: isDark)
        }
    }

    private static func drawTransportButton(in rect: NSRect,
                                            role: HitResult,
                                            hovered: Bool,
                                            active: Bool,
                                            isDark: Bool) {
        // Flat column — no body fill, no gradient. The deck face shows
        // straight through; only the glyph differentiates the column.
        // A hairline separator on the right edge keeps adjacent columns
        // visually distinct without forcing a chiclet silhouette.

        // Hover wash — gentle film so the user can tell which column
        // their cursor is over without painting in a hard hover box.
        // White film in dark mode, soft gray in light mode (so it
        // actually reads against the white deck).
        if hovered {
            if isDark {
                NSColor.white.withAlphaComponent(0.14).setFill()
            } else {
                NSColor.black.withAlphaComponent(0.08).setFill()
            }
            NSBezierPath(rect: rect).fill()
        }

        // Symbol color follows function — classic cassette-deck legend.
        let glyphColor = transportGlyphColor(for: role, active: active, isDark: isDark)
        drawTransportGlyph(in: rect, role: role, color: glyphColor)
    }

    /// Glyph color per transport role. Classic legend: REW/FFWD amber,
    /// STOP white/charcoal, PLAY green, REC red, EJECT orange.
    private static func transportGlyphColor(for role: HitResult,
                                            active: Bool,
                                            isDark: Bool) -> NSColor {
        switch role {
        case .tapeRew, .tapeFfwd:
            // Amber — Walkman tape-direction LEDs.
            return NSColor(srgbRed: 255/255, green: 190/255, blue:  70/255, alpha: 1)
        case .tapeStop:
            return isDark
                ? NSColor.white.withAlphaComponent(0.92)
                : NSColor(white: 0.10, alpha: 1.0)
        case .tapePlay:
            // Green — bright enough to glow when engaged, slightly
            // dimmer at rest so the deck doesn't look "running" all
            // the time.
            return active
                ? NSColor(srgbRed:  90/255, green: 220/255, blue:  90/255, alpha: 1)
                : NSColor(srgbRed:  70/255, green: 180/255, blue:  80/255, alpha: 1)
        case .tapeRec:
            // Saturated red; brightens when armed.
            return active
                ? NSColor(srgbRed: 255/255, green:  60/255, blue:  60/255, alpha: 1)
                : NSColor(srgbRed: 220/255, green:  50/255, blue:  50/255, alpha: 1)
        case .tapeEject:
            // Warm orange — same family as the cassette's mic-hot LED.
            return NSColor(srgbRed: 255/255, green: 145/255, blue:  35/255, alpha: 1)
        default:
            return isDark ? .white : NSColor(white: 0.10, alpha: 1.0)
        }
    }

    /// Hand-drawn vector glyphs sized for the wider full-height
    /// column buttons (≈ 17×17pt). Color-coded, centered in the rect.
    private static func drawTransportGlyph(in rect: NSRect,
                                           role: HitResult,
                                           color: NSColor) {
        color.setFill()
        color.setStroke()
        let cx = rect.midX
        let cy = rect.midY
        let s: CGFloat = 3.6   // half-glyph short axis
        let t: CGFloat = 4.6   // half-glyph long axis
        switch role {
        case .tapeRew:
            // Two left-pointing triangles, side by side.
            let g = NSBezierPath()
            let dx: CGFloat = 3.6
            for k in 0...1 {
                let ox = cx - dx * 0.5 + CGFloat(k) * dx
                g.move(to: NSPoint(x: ox + s * 0.6, y: cy + t * 0.85))
                g.line(to: NSPoint(x: ox - s * 0.6, y: cy))
                g.line(to: NSPoint(x: ox + s * 0.6, y: cy - t * 0.85))
                g.close()
            }
            g.fill()
        case .tapeStop:
            let side: CGFloat = 6.4
            let r = NSRect(x: cx - side / 2, y: cy - side / 2,
                            width: side, height: side)
            NSBezierPath(rect: r).fill()
        case .tapePlay:
            let g = NSBezierPath()
            g.move(to: NSPoint(x: cx - s, y: cy + t))
            g.line(to: NSPoint(x: cx + s + 0.8, y: cy))
            g.line(to: NSPoint(x: cx - s, y: cy - t))
            g.close()
            g.fill()
        case .tapeFfwd:
            let g = NSBezierPath()
            let dx: CGFloat = 3.6
            for k in 0...1 {
                let ox = cx - dx * 0.5 + CGFloat(k) * dx
                g.move(to: NSPoint(x: ox - s * 0.6, y: cy + t * 0.85))
                g.line(to: NSPoint(x: ox + s * 0.6, y: cy))
                g.line(to: NSPoint(x: ox - s * 0.6, y: cy - t * 0.85))
                g.close()
            }
            g.fill()
        case .tapeRec:
            let diam: CGFloat = 7.2
            let r = NSRect(x: cx - diam / 2, y: cy - diam / 2,
                            width: diam, height: diam)
            NSBezierPath(ovalIn: r).fill()
        case .tapeEject:
            // Up triangle over a thin bar.
            let g = NSBezierPath()
            g.move(to: NSPoint(x: cx - s, y: cy - t * 0.2))
            g.line(to: NSPoint(x: cx + s, y: cy - t * 0.2))
            g.line(to: NSPoint(x: cx, y: cy + t * 0.95))
            g.close()
            g.fill()
            let bar = NSRect(x: cx - s, y: cy - t * 0.9,
                              width: s * 2, height: 1.8)
            NSBezierPath(rect: bar).fill()
        default:
            break
        }
    }

    private static func drawSettingsChip(in _: NSRect, hoverRect _: NSRect,
                                         midiOn: Bool, hovered: Bool,
                                         flash: CGFloat = 0,
                                         voiceNumber: Int = 0,
                                         voiceLabel: String? = nil,
                                         visualizerHovered: Bool = false,
                                         visualizerVisible: Bool = true,
                                         visualizerLevel: CGFloat = 0) {
        // Standard systray pill: hover/click paints a soft rounded
        // backdrop centered on the icon glyph (NOT the full hit area, so
        // the piano-side empty space stays unhighlighted). Same look as
        // any other status-bar item.
        let iconBox = settingsIconRect
        if hovered {
            // Pill hugs the actual drawn content: just the music note
            // when voiceNumber is 0, expanding rightward only as far as
            // the voice-number digits actually flow when present. Avoids
            // a chunky 12pt of empty pad on the right when the user is
            // on the default Acoustic Grand (program 0).
            var pillRightExtra: CGFloat = 1
            // The active subscript: explicit label (e.g. "`" for the
            // sample backend) wins over the numeric voice slot.
            let activeLabel: String? = {
                // MIDI mode replaces the GM program number with a
                // bold "M" — the GM index is meaningless once the
                // route is going to a DAW, so the badge instead
                // signals the routing state.
                if midiOn { return "M" }
                return voiceLabel ?? (voiceNumber > 0 ? String(voiceNumber) : nil)
            }()
            if let activeLabel = activeLabel {
                // The sample backend's "`" badge renders bigger than the
                // numeric voice slots (see the subscript draw below), so
                // size the hover pill to the same larger glyph.
                let isBacktick = activeLabel == "`"
                let digitFont = NSFont.monospacedDigitSystemFont(
                    ofSize: isBacktick ? 12 : 7, weight: .heavy)
                let label = NSAttributedString(string: activeLabel, attributes: [
                    .font: digitFont, .kern: -0.4,
                ])
                let oneDigitW = NSAttributedString(string: "0", attributes: [
                    .font: NSFont.monospacedDigitSystemFont(ofSize: 7, weight: .heavy),
                ]).size().width
                // First digit hugs iconBox's right edge (see digit-draw
                // logic below); additional digits flow rightward into
                // the badge pad. Pad just enough to cover them + a 2pt
                // breathing margin past the rightmost digit.
                pillRightExtra = max(1, label.size().width - oneDigitW + 2)
            }
            let pill = NSRect(
                x: iconBox.minX - 1,
                y: iconBox.minY + 1,
                width: iconBox.width + pillRightExtra,
                height: iconBox.height - 2
            )
            let path = NSBezierPath(roundedRect: pill, xRadius: 4, yRadius: 4)
            NSColor.labelColor.withAlphaComponent(0.12).setFill()
            path.fill()
        }
        let alpha: CGFloat = hovered ? 1.0 : (midiOn ? 1.0 : 0.78)
        // Recording overrides everything else: the music-note glyph
        // and the VU bars both flip to a saturated red so the
        // menubar reads as "REC ON" the moment the user starts
        // holding the input key. Linger fermata + voice subscript
        // ride the same color so the chip stays visually unified.
        // MIDI mode tints the whole chip in the accent color so the
        // menubar reads as "routing OUT" at a glance.
        let recordingTint = NSColor.systemRed
        let baseColor: NSColor = recordingActive
            ? recordingTint
            : (midiOn
                ? NSColor.controlAccentColor
                : NSColor.labelColor.withAlphaComponent(alpha))
        // Blend toward the flash target based on `flash` (0..1).
        // Used to signal "activity" when the user taps an octave key
        // or plays a note — the music note icon briefly brightens
        // before settling back. Metronome ticks add a yellow blink
        // on top so the icon visibly pulses with the beat.
        //
        // Target depends on appearance: pure white reads great
        // against the bright dark-mode glyph, but in LIGHT mode the
        // glyph is already near-black on a pale bar, so blending
        // toward white barely registers. There, flash toward a
        // brightened system accent so the pulse actually pops.
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let flashTarget: NSColor = isDark
            ? .white
            : (NSColor.controlAccentColor.blended(withFraction: 0.4,
                                                  of: .white)
                ?? NSColor.controlAccentColor)
        let f = max(0, min(1, flash))
        var color = (f > 0.001)
            ? baseColor.blended(withFraction: f, of: flashTarget) ?? baseColor
            : baseColor
        let mF = max(0, min(1, metronomeFlash))
        // Beat pulse tints the glyph yellow on each tick. (The little side
        // "tick" blips that used to radiate from the chip are removed — they
        // read as clutter while the metronome runs.)
        if mF > 0.01, let yellowed = color.blended(withFraction: mF * 0.85, of: .systemYellow) {
            color = yellowed
        }
        // Draw the SF Symbol music.note.list at its original pointSize
        // (13) over the full chip. At rest the staff lines should
        // remain visible (the chip reads as the natural system glyph).
        // Only when there's audible signal do we punch out the staff
        // lines and overlay animated VU bars in the resulting hole.
        //
        // Percussion wobble: a fresh drum hit rotates the glyph
        // left/right about the icon center with a damped swing.
        let wobble = percussionIconWobble()
        if abs(wobble) > 0.0005 {
            NSGraphicsContext.saveGraphicsState()
            let c = NSPoint(x: iconBox.midX, y: iconBox.midY)
            let t = NSAffineTransform()
            t.translateX(by: c.x, yBy: c.y)
            t.rotate(byRadians: wobble)
            t.translateX(by: -c.x, yBy: -c.y)
            t.concat()
            drawTintedSymbol("music.note.list", in: iconBox, pointSize: 13.0, color: color)
            NSGraphicsContext.restoreGraphicsState()
        } else {
            drawTintedSymbol("music.note.list", in: iconBox, pointSize: 13.0, color: color)
        }
        // Always punch out the staff-lines slot and overlay 3 vertical
        // VU bars — the chip is "always live." When silent, the bars
        // fall to a short flat floor instead of vanishing back into
        // the SF Symbol's staff lines, so the menubar reads as the
        // app's own meter affordance at all times.
        if visualizerVisible, let ctx = NSGraphicsContext.current {
            ctx.saveGraphicsState()
            ctx.compositingOperation = .destinationOut
            NSColor.black.set()
            miniVisualizerPunchRect.fill()
            ctx.restoreGraphicsState()
            // The destination-out punch clears the hover backdrop in
            // the bars area too — leaving an oddly dark hole behind
            // the bars when the chip is hovered/clicked. Repaint the
            // same hover-backdrop color into the punched zone so the
            // bars sit on a uniform pill instead of a cut-out shadow.
            if hovered {
                NSColor.labelColor.withAlphaComponent(0.12).setFill()
                miniVisualizerPunchRect.fill()
            }
            // While MIDI mode is on, the bars slot becomes an
            // empty square that fills on every outbound MIDI
            // event (Ableton-style activity indicator). When
            // MIDI is off the slot keeps its 3-bar visualizer.
            if midiOn {
                drawChipMidiSquare(in: miniVisualizerRect,
                                    flash: midiActivityFlash,
                                    hovered: visualizerHovered,
                                    color: color,
                                    baseAlpha: alpha)
            } else if metronomeOn {
                drawChipMetronomeWave(in: miniVisualizerRect,
                                       hovered: visualizerHovered,
                                       color: color,
                                       baseAlpha: alpha)
            } else {
                drawChipVisualizer(in: miniVisualizerRect, level: visualizerLevel,
                                   hovered: visualizerHovered,
                                   color: color, baseAlpha: alpha)
            }
        }
        // Linger / bell-ring flourish — a fermata mark (the music
        // notation for "let ring / hold this note") drawn above the
        // music-note glyph. Caps lock latches the mode and always
        // paints the mark; momentary shift only paints while at
        // least one note is actively held, so resting on shift
        // between phrases doesn't add visual chrome.
        let lingerVisible = lingerCapsLatched
            || (labelsUppercase && playingActive)
        if lingerVisible, let ctx = NSGraphicsContext.current {
            ctx.saveGraphicsState()
            NSColor.white.set()
            // Anchor the fermata so it overlaps the upper-right of the
            // music-note glyph — pushed past the right edge and dropped
            // down a few points, so the arc reads as a fat round canopy
            // sitting on top of the note rather than floating above it.
            // Chunkier arc + bigger dot for a softer, cuter feel.
            let fW: CGFloat = 7.5
            let fH: CGFloat = 3.5
            let fX = iconBox.maxX - fW + 1.5
            let fY = iconBox.maxY - fH - 3.0
            let arc = NSBezierPath()
            arc.appendArc(
                withCenter: NSPoint(x: fX + fW / 2, y: fY),
                radius: fW / 2,
                startAngle: 0,
                endAngle: 180
            )
            arc.lineWidth = 1.4
            arc.lineCapStyle = .round
            arc.stroke()
            // Bigger dot under the arc — reads as a cartoony round
            // bead instead of a single stipple.
            let dot = NSBezierPath(ovalIn: NSRect(
                x: fX + fW / 2 - 0.95,
                y: fY - 0.4,
                width: 1.9,
                height: 1.9
            ))
            dot.fill()
            ctx.restoreGraphicsState()
        }
        // Voice-number subscript: tiny digits in the bottom-right
        // corner. The first digit sits where the single-digit case
        // looked good; additional digits FLOW RIGHT from there
        // (instead of growing leftward across the music-note glyph).
        // Single digit is the visual anchor; multi-digit values
        // extend toward / past the menubar slot's right edge.
        // Always rendered (badge is now 1-based; voice 1 is the
        // baseline GM Acoustic Grand and reads as "1"). When
        // `voiceLabel` is supplied (e.g. "`" while the sample backend
        // is active) we draw that string verbatim instead of the
        // numeric program slot.
        // Same priority as the hover-pill computation above: in
        // MIDI mode the subscript reads as a bold "M" because the
        // GM program slot is irrelevant once the synth route is
        // pointed at the DAW.
        let subscriptText: String? = midiOn
            ? "M"
            : voiceLabel ?? (voiceNumber > 0 ? String(voiceNumber) : nil)
        if let subscriptText = subscriptText {
            // The sample backend's "`" badge is drawn BIGGER than the
            // numeric voice slots: the backtick is a small high mark and
            // reads as a smudge at 7pt, so bump it to 12pt. Digits / "M"
            // keep the compact size.
            let isBacktick = subscriptText == "`"
            let fontSize: CGFloat = isBacktick ? 12.0 : 7.0
            // Negative kerning tightens the digits so multi-digit
            // values feel more like a tag than spaced text.
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedDigitSystemFont(ofSize: fontSize, weight: .heavy),
                .foregroundColor: color,
                .kern: -0.4,
            ]
            let str = NSAttributedString(string: subscriptText, attributes: attrs)
            // Width of a single "0" — anchor for the first digit.
            let oneDigit = NSAttributedString(string: "0", attributes: [
                .font: NSFont.monospacedDigitSystemFont(ofSize: 7, weight: .heavy),
            ]).size()
            if isBacktick {
                // Right-align the wider backtick to the icon's right edge
                // and drop its baseline so the high glyph hugs the note
                // instead of floating above the chip.
                str.draw(at: NSPoint(x: iconBox.maxX - str.size().width,
                                     y: iconBox.minY - 5))
            } else {
                // Nudge the first digit a small tad LEFT so it visually
                // hugs the music-note glyph; remaining digits flow
                // rightward into the reserved badge pad.
                let leftX = iconBox.maxX - oneDigit.width - 1
                str.draw(at: NSPoint(x: leftX, y: iconBox.minY - 1))
            }
        }
    }

    private static func drawInstrumentLabel(in rect: NSRect, hoverRect: NSRect,
                                            program: UInt8, hovered: Bool) {
        drawHoverBackdrop(in: hoverRect, hovered: hovered)
        let safeIdx = max(0, min(127, Int(program)))
        let abbrev = GeneralMIDI.familyAbbrev(for: program)
        // Display 1-based GM index (1-128). Slot 0 is reserved as
        // "MIDI OUT" — the menubar shows that label separately when
        // MIDI passthrough is active.
        let label = String(format: "%@ %03d", abbrev, safeIdx + 1)
        let alpha: CGFloat = hovered ? 1.0 : 0.82
        let attrs: [NSAttributedString.Key: Any] = [
            .font: processingFont(size: 10.0),
            .foregroundColor: NSColor.labelColor.withAlphaComponent(alpha),
            .kern: 0.2,
        ]
        let str = NSAttributedString(string: label, attributes: attrs)
        let size = str.size()
        str.draw(at: NSPoint(x: rect.midX - size.width / 2,
                             y: rect.midY - size.height / 2))
    }

    private static func drawTypeButton(in rect: NSRect, hoverRect: NSRect,
                                       on: Bool, hovered: Bool) {
        drawHoverBackdrop(in: hoverRect, hovered: hovered)
        let alpha: CGFloat = hovered ? 1.0 : 0.78
        let activeColor = NSColor.controlAccentColor.highlight(withLevel: 0.30)
            ?? NSColor.controlAccentColor
        let color: NSColor = on ? activeColor : NSColor.labelColor.withAlphaComponent(alpha)
        drawQwertyMap(in: rect, color: color)
    }

    private static func drawMIDIButton(in rect: NSRect, hoverRect: NSRect,
                                       on: Bool, hovered: Bool) {
        drawHoverBackdrop(in: hoverRect, hovered: hovered)
        let alpha: CGFloat = hovered ? 1.0 : 0.78
        let activeColor = NSColor.controlAccentColor.highlight(withLevel: 0.30)
            ?? NSColor.controlAccentColor
        let color: NSColor = on ? activeColor : NSColor.labelColor.withAlphaComponent(alpha)

        // Layout: [music note] [tiny "midi"] inline, centered together.
        let iconSize: CGFloat = 11.0
        let textGap: CGFloat = 1.6
        let textAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 6.5, weight: .heavy),
            .foregroundColor: color,
            .kern: 0.2,
        ]
        let textStr = NSAttributedString(string: "midi", attributes: textAttrs)
        let textSize = textStr.size()
        let totalW = iconSize + textGap + textSize.width
        let startX = rect.midX - totalW / 2
        let iconRect = NSRect(x: startX, y: rect.midY - iconSize / 2,
                              width: iconSize, height: iconSize)
        drawTintedSymbol("music.note", in: iconRect, pointSize: 10.0, color: color)
        textStr.draw(at: NSPoint(x: startX + iconSize + textGap,
                                 y: rect.midY - textSize.height / 2))
    }

    /// Tiny stylized QWERTY layout — three staggered rows of small rounded
    /// rects, like a typewriter keyboard from above.
    private static func drawQwertyMap(in rect: NSRect, color: NSColor) {
        color.setFill()
        let rows = [4, 4, 3]
        let rowOffsets: [CGFloat] = [0.0, 0.5, 1.0]  // typewriter stagger
        let keySize: CGFloat = 1.9
        let hSpace: CGFloat = 0.5
        let vSpace: CGFloat = 0.6
        let maxKeys = rows.max() ?? 4
        let gridW = CGFloat(maxKeys) * keySize + CGFloat(maxKeys - 1) * hSpace
        let gridH = CGFloat(rows.count) * keySize + CGFloat(rows.count - 1) * vSpace
        let originX = rect.midX - gridW / 2
        let originY = rect.midY - gridH / 2
        for (rIdx, count) in rows.enumerated() {
            let y = originY + CGFloat(rows.count - 1 - rIdx) * (keySize + vSpace)
            let xOff = rowOffsets[rIdx] * (keySize + hSpace)
            for col in 0..<count {
                let x = originX + xOff + CGFloat(col) * (keySize + hSpace)
                let r = NSRect(x: x, y: y, width: keySize, height: keySize)
                NSBezierPath(roundedRect: r, xRadius: 0.35, yRadius: 0.35).fill()
            }
        }
    }

    /// Render an SF Symbol tinted to an arbitrary color. Templates alone
    /// don't reliably take a fill color when drawn outside a button, so we
    /// composite the symbol into a fresh image and use sourceIn to colour it.
    private static func drawTintedSymbol(_ name: String, in rect: NSRect,
                                         pointSize: CGFloat, color: NSColor) {
        guard let base = NSImage(systemSymbolName: name, accessibilityDescription: nil) else { return }
        let config = NSImage.SymbolConfiguration(pointSize: pointSize, weight: .bold)
        let configured = base.withSymbolConfiguration(config) ?? base
        let size = configured.size

        let tinted = NSImage(size: size)
        tinted.lockFocus()
        configured.draw(in: NSRect(origin: .zero, size: size),
                        from: .zero, operation: .sourceOver, fraction: 1.0)
        color.set()
        NSRect(origin: .zero, size: size).fill(using: .sourceIn)
        tinted.unlockFocus()

        let drawRect = NSRect(
            x: rect.midX - size.width / 2,
            y: rect.midY - size.height / 2,
            width: size.width, height: size.height
        )
        tinted.draw(in: drawRect)
    }
}
