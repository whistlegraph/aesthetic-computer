import AppKit
import GameController

// MARK: - Gamepad configuration

/// The eight physical buttons that play notes: the d-pad diamond (the low
/// tetrachord) and the face-button diamond (the high tetrachord). Read
/// positionally so the mapping is identical on Xbox, DualSense and generic
/// pads — GameController abstracts the face buttons by position
/// (A = South, B = East, X = West, Y = North) regardless of the printed
/// label, so `buttonX` is the left button on every controller.
enum GamepadNoteButton: CaseIterable {
    case dpadLeft, dpadUp, dpadRight, dpadDown
    case faceWest, faceNorth, faceEast, faceSouth
}

/// How the eight C-major scale degrees (0 = C … 7 = +C) sit across the two
/// diamonds. Switchable so the player can find what feels natural.
enum GamepadNoteLayout: String, CaseIterable {
    case clockwise   // each hand ascends clockwise from its left button
    case bottomUp    // lowest at the bottom of each diamond, highest at top
    case leftRight   // leftmost = lowest, rightmost = highest

    /// Scale degree 0...7 for a note button under this layout. The d-pad
    /// always carries the low tetrachord (degrees 0–3) and the face
    /// buttons the high tetrachord (degrees 4–7).
    func degree(for button: GamepadNoteButton) -> Int {
        switch self {
        case .clockwise:
            switch button {
            case .dpadLeft:  return 0   // C
            case .dpadUp:    return 1   // D
            case .dpadRight: return 2   // E
            case .dpadDown:  return 3   // F
            case .faceWest:  return 4   // G
            case .faceNorth: return 5   // A
            case .faceEast:  return 6   // B
            case .faceSouth: return 7   // +C
            }
        case .bottomUp:
            switch button {
            case .dpadDown:  return 0
            case .dpadLeft:  return 1
            case .dpadRight: return 2
            case .dpadUp:    return 3
            case .faceSouth: return 4
            case .faceWest:  return 5
            case .faceEast:  return 6
            case .faceNorth: return 7
            }
        case .leftRight:
            switch button {
            case .dpadLeft:  return 0
            case .dpadUp:    return 1
            case .dpadDown:  return 2
            case .dpadRight: return 3
            case .faceWest:  return 4
            case .faceNorth: return 5
            case .faceSouth: return 6
            case .faceEast:  return 7
            }
        }
    }

    var displayName: String {
        switch self {
        case .clockwise: return "Clockwise climb"
        case .bottomUp:  return "Bottom-up"
        case .leftRight: return "Left-to-right"
        }
    }
}

/// Which spare buttons (the two shoulders, two triggers, two stick-clicks)
/// get the easy, dedicated controls. Switchable live from the popover so
/// the player can feel out which is most natural.
enum GamepadControlScheme: String, CaseIterable {
    case octaveFirst   // triggers = octave, R1 = sustain, L1 = function layer
    case chordFirst    // L1/R1 = chord maj/min, L2 = sustain, R2 = function
    case minimal       // R1 = sustain, triggers = octave, L1 = function, no chords

    var displayName: String {
        switch self {
        case .octaveFirst: return "Octave-first"
        case .chordFirst:  return "Chord-first"
        case .minimal:     return "Minimal"
        }
    }
}

/// UserDefaults-backed gamepad settings. Routing reads these live, so the
/// popover dropdown takes effect on the very next button event.
enum GamepadDefaults {
    static let enabledKey = "notepat.gamepadEnabled"
    static let schemeKey = "notepat.gamepadScheme"
    static let layoutKey = "notepat.gamepadNoteLayout"

    /// Master on/off for gamepad control. OFF by default for the release —
    /// a paired controller stays fully inert (no note routing, no discovery)
    /// until the user opts in. Flip via the Keymap overlay's gamepad config
    /// (or `defaults write … notepat.gamepadEnabled -bool true`).
    static var enabled: Bool {
        get { UserDefaults.standard.bool(forKey: enabledKey) }   // absent → false
        set { UserDefaults.standard.set(newValue, forKey: enabledKey) }
    }

    static var scheme: GamepadControlScheme {
        get { GamepadControlScheme(rawValue:
                UserDefaults.standard.string(forKey: schemeKey) ?? "") ?? .octaveFirst }
        set { UserDefaults.standard.set(newValue.rawValue, forKey: schemeKey) }
    }
    static var layout: GamepadNoteLayout {
        get { GamepadNoteLayout(rawValue:
                UserDefaults.standard.string(forKey: layoutKey) ?? "") ?? .clockwise }
        set { UserDefaults.standard.set(newValue.rawValue, forKey: layoutKey) }
    }
}

extension Notification.Name {
    /// Posted by the popover when the user picks a new gamepad scheme or
    /// layout (informational — routing already reads UserDefaults live).
    static let menuBandGamepadConfigChanged =
        Notification.Name("computer.aestheticcomputer.menuband.gamepadConfigChanged")
}

// MARK: - Manager

/// Bridges a Bluetooth game controller (Xbox / DualSense / generic) into
/// Menu Band. Notes, chords and sustain are synthesized as key events
/// through `MenuBandController.handleLocalKey`, so they inherit every bit
/// of the keyboard's note logic (polyphony channels, per-key pan, menubar
/// lit-state, linger, chord morph, percussion split, MIDI-out). Discrete
/// actions call controller methods directly; the analog sticks drive the
/// global bend / reverb / echo knobs.
///
/// Unlike the keyboard `CGEventTap`, GameController needs no Accessibility
/// permission and (with `shouldMonitorBackgroundEvents`) plays while Menu
/// Band is backgrounded — including in the sandboxed App Store build, where
/// the keyboard tap is disabled.
final class GamepadManager {
    private weak var controller: MenuBandController?

    /// AppDelegate wires this to its status-item show/close logic — the
    /// Menu / Options button opens (and closes) the popover.
    var onTogglePopover: (() -> Void)?

    /// Called on the main thread when a controller connects or disconnects,
    /// so an open popover can refresh its status line.
    var onConnectionChanged: (() -> Void)?

    /// Triggers/shoulders are analog; treat them as "pressed" past this.
    private static let pressThreshold: Float = 0.5
    /// Stick deadzone — ignore tiny resting jitter.
    private static let stickDeadzone: Float = 0.12

    /// The key code we sent for each currently-held note button, so the
    /// matching key-up uses the same code even if the keymap changed
    /// mid-hold — and so a yanked controller can flush its held notes.
    private var heldNoteKeyCodes: [GamepadNoteButton: UInt16] = [:]

    /// Last value pushed per continuous axis, to skip redundant MIDI when
    /// the stick is resting in the deadzone.
    private var lastBend: Float = .nan
    private var lastSpace: Float = .nan
    private var lastEcho: Float = .nan

    init(controller: MenuBandController) {
        self.controller = controller
    }

    deinit { NotificationCenter.default.removeObserver(self) }

    // MARK: - Lifecycle

    func start() {
        // Gamepad control is opt-in (off by default for the release). Until the
        // user enables it, don't observe controllers or bind any input.
        guard GamepadDefaults.enabled else { return }
        // Play over other apps — it's a menubar instrument, rarely frontmost.
        if #available(macOS 11.3, *) {
            GCController.shouldMonitorBackgroundEvents = true
        }

        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(controllerDidConnect(_:)),
                       name: .GCControllerDidConnect, object: nil)
        nc.addObserver(self, selector: #selector(controllerDidDisconnect(_:)),
                       name: .GCControllerDidDisconnect, object: nil)

        // Already-paired Bluetooth controllers are usually present at launch.
        for c in GCController.controllers() { attach(c) }
        // Surface newly-woken wireless controllers (no-op if none pairing).
        GCController.startWirelessControllerDiscovery(completionHandler: nil)
    }

    /// Display name of the first connected controller (for the popover
    /// status line), or nil when nothing is connected.
    var connectedControllerName: String? {
        GCController.controllers().first.flatMap { $0.vendorName }
    }

    @objc private func controllerDidConnect(_ note: Notification) {
        if let c = note.object as? GCController { attach(c) }
        onConnectionChanged?()
    }

    @objc private func controllerDidDisconnect(_ note: Notification) {
        // Flush anything still held so a yanked controller can't strand a
        // sounding note.
        releaseAllHeldNotes()
        onConnectionChanged?()
    }

    // MARK: - Wiring a controller

    private func attach(_ gcController: GCController) {
        guard let gp = gcController.extendedGamepad else { return }
        // Every handler touches the synth / UI — keep them on main.
        gcController.handlerQueue = .main

        // Note buttons: d-pad diamond (low) + face diamond (high).
        bindNote(gp.dpad.left, .dpadLeft, gp)
        bindNote(gp.dpad.up, .dpadUp, gp)
        bindNote(gp.dpad.right, .dpadRight, gp)
        bindNote(gp.dpad.down, .dpadDown, gp)
        bindNote(gp.buttonX, .faceWest, gp)
        bindNote(gp.buttonY, .faceNorth, gp)
        bindNote(gp.buttonB, .faceEast, gp)
        bindNote(gp.buttonA, .faceSouth, gp)

        // Triggers — octave in octave-first / minimal; held modifiers in
        // chord-first (read live there, so no press action).
        gp.leftTrigger.pressedChangedHandler = { [weak self] _, _, pressed in
            self?.handleTrigger(left: true, pressed: pressed)
        }
        gp.rightTrigger.pressedChangedHandler = { [weak self] _, _, pressed in
            self?.handleTrigger(left: false, pressed: pressed)
        }

        // Menu opens/closes the popover; Options toggles MIDI mode.
        gp.buttonMenu.pressedChangedHandler = { [weak self] _, _, pressed in
            if pressed { self?.onTogglePopover?() }
        }
        gp.buttonOptions?.pressedChangedHandler = { [weak self] _, _, pressed in
            if pressed { self?.controller?.toggleMIDIMode() }
        }

        // Sticks — right: pitch bend (Y) + reverb (X); left: echo (X).
        gp.rightThumbstick.valueChangedHandler = { [weak self] _, x, y in
            self?.handleRightStick(x: x, y: y)
        }
        gp.leftThumbstick.valueChangedHandler = { [weak self] _, x, _ in
            self?.handleLeftStick(x: x)
        }

        onConnectionChanged?()
    }

    private func bindNote(_ input: GCControllerButtonInput,
                          _ button: GamepadNoteButton,
                          _ gp: GCExtendedGamepad) {
        // [weak gp] avoids a retain cycle: gp owns the input, the input
        // owns this closure — a strong gp here would keep the controller
        // alive forever after disconnect.
        input.pressedChangedHandler = { [weak self, weak gp] _, _, pressed in
            guard let self = self, let gp = gp else { return }
            self.handleNoteButton(button, pressed: pressed, gp: gp)
        }
    }

    // MARK: - Notes

    private func handleNoteButton(_ button: GamepadNoteButton,
                                  pressed: Bool,
                                  gp: GCExtendedGamepad) {
        guard let controller = controller else { return }

        if !pressed {
            // Always release a note we started — even if the function layer
            // engaged meanwhile — so a sounding note is never stranded.
            if let keyCode = heldNoteKeyCodes.removeValue(forKey: button) {
                controller.handleLocalKey(keyCode: keyCode, isDown: false,
                                          isRepeat: false, flags: [])
            }
            return
        }

        let scheme = GamepadDefaults.scheme
        // While the function layer is held the diamonds become control
        // pads, not notes — run the action and sound nothing.
        if isFunctionLayerHeld(scheme: scheme, gp: gp) {
            performFunctionAction(button, scheme: scheme)
            return
        }

        let degree = GamepadDefaults.layout.degree(for: button)
        let keyCode = MenuBandLayout.cMajorKeyCodes(for: controller.keymap)[degree]
        var flags: NSEvent.ModifierFlags = []
        if isSustainHeld(scheme: scheme, gp: gp) { flags.insert(.shift) }
        flags.formUnion(chordFlags(scheme: scheme, gp: gp))
        heldNoteKeyCodes[button] = keyCode
        controller.handleLocalKey(keyCode: keyCode, isDown: true,
                                  isRepeat: false, flags: flags)
    }

    private func releaseAllHeldNotes() {
        for (_, keyCode) in heldNoteKeyCodes {
            controller?.handleLocalKey(keyCode: keyCode, isDown: false,
                                       isRepeat: false, flags: [])
        }
        heldNoteKeyCodes.removeAll()
    }

    // MARK: - Modifier state (read live from the pad)

    private func isFunctionLayerHeld(scheme: GamepadControlScheme,
                                     gp: GCExtendedGamepad) -> Bool {
        switch scheme {
        case .octaveFirst, .minimal: return gp.leftShoulder.isPressed
        case .chordFirst:            return gp.rightTrigger.value > Self.pressThreshold
        }
    }

    private func isSustainHeld(scheme: GamepadControlScheme,
                               gp: GCExtendedGamepad) -> Bool {
        switch scheme {
        case .octaveFirst, .minimal: return gp.rightShoulder.isPressed
        case .chordFirst:            return gp.leftTrigger.value > Self.pressThreshold
        }
    }

    /// Chord-morph modifiers, mapped onto the same flags the keyboard uses:
    /// `.control` = major, `.option` = minor, both = sus (see `playKeyEvent`).
    private func chordFlags(scheme: GamepadControlScheme,
                            gp: GCExtendedGamepad) -> NSEvent.ModifierFlags {
        var f: NSEvent.ModifierFlags = []
        switch scheme {
        case .minimal:
            break
        case .octaveFirst:
            // Stick-clicks: R3 = major (⌃), L3 = minor (⌥), both = sus.
            if gp.rightThumbstickButton?.isPressed == true { f.insert(.control) }
            if gp.leftThumbstickButton?.isPressed == true { f.insert(.option) }
        case .chordFirst:
            // Shoulders: L1 = major (⌃), R1 = minor (⌥), both = sus.
            if gp.leftShoulder.isPressed { f.insert(.control) }
            if gp.rightShoulder.isPressed { f.insert(.option) }
        }
        return f
    }

    // MARK: - Triggers (octave)

    private func handleTrigger(left: Bool, pressed: Bool) {
        guard pressed else { return }   // step once on the press edge
        switch GamepadDefaults.scheme {
        case .octaveFirst, .minimal:
            controller?.stepOctave(delta: left ? -1 : +1)
        case .chordFirst:
            break   // L2 = sustain, R2 = function layer; both read live
        }
    }

    // MARK: - Function-layer actions

    private func performFunctionAction(_ button: GamepadNoteButton,
                                       scheme: GamepadControlScheme) {
        guard let controller = controller else { return }
        let cols = InstrumentListView.cols
        switch button {
        // D-pad ◄► browses instruments. Up/down browses the instrument grid
        // in octave-first / minimal (octave lives on the triggers there); in
        // chord-first the triggers aren't free, so up/down shifts the octave.
        case .dpadLeft:  controller.stepMelodicProgramWithBlip(delta: -1)
        case .dpadRight: controller.stepMelodicProgramWithBlip(delta: +1)
        case .dpadUp:
            if scheme == .chordFirst { controller.stepOctave(delta: +1) }
            else { controller.stepMelodicProgramWithBlip(delta: -cols) }
        case .dpadDown:
            if scheme == .chordFirst { controller.stepOctave(delta: -1) }
            else { controller.stepMelodicProgramWithBlip(delta: +cols) }
        // Face buttons toggle the per-half percussion split, MIDI mode and
        // tape recording (X / B / A / Y).
        case .faceWest:  controller.togglePercussionLeft()
        case .faceEast:  controller.togglePercussionRight()
        case .faceSouth: controller.toggleMIDIMode()
        case .faceNorth: controller.toggleTapeRecording()
        }
    }

    // MARK: - Sticks (continuous expression)

    private func handleRightStick(x: Float, y: Float) {
        guard let controller = controller else { return }
        // Y → pitch bend, spring-back. ±1 stick = ±1 octave (the engine's
        // announced ±12-semitone range). `allChannels` so every round-robin
        // voice bends together, like Shift + trackpad.
        let bend = deadzoned(y)
        if bend != lastBend {
            controller.setBend(amount: bend, allChannels: true)
            lastBend = bend
        }
        // X (rightward deflection) → reverb 0…1.
        let space = positiveDeflection(x)
        if space != lastSpace {
            controller.setSpace(amount: space)
            lastSpace = space
        }
    }

    private func handleLeftStick(x: Float) {
        guard let controller = controller else { return }
        let echo = positiveDeflection(x)
        if echo != lastEcho {
            controller.setEcho(amount: echo)
            lastEcho = echo
        }
    }

    /// Symmetric deadzone for a bipolar axis (pitch bend), rescaled so the
    /// travel past the deadzone still reaches the full ±1.
    private func deadzoned(_ v: Float) -> Float {
        let dz = Self.stickDeadzone
        guard abs(v) > dz else { return 0 }
        let sign: Float = v < 0 ? -1 : 1
        return sign * (abs(v) - dz) / (1 - dz)
    }

    /// One-sided control (reverb / echo): only positive (rightward)
    /// deflection past the deadzone maps to 0…1; centered/left rests at 0.
    private func positiveDeflection(_ v: Float) -> Float {
        let dz = Self.stickDeadzone
        guard v > dz else { return 0 }
        return (v - dz) / (1 - dz)
    }
}
