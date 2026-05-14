import AppKit
import ApplicationServices
import CoreGraphics

/// Bridges macOS Stickies → Menu Band live note playback. When the
/// user is typing into a sticky whose background color matches the
/// configured trigger color (default: yellow), each keystroke is
/// observed and forwarded through `MenuBandController.handleLocalKey`
/// so the existing keymap turns it into a note — at the same time
/// the keystroke continues unimpeded into Stickies' RTF editor.
///
/// Why a CGEventTap and not Accessibility:
///   • Stickies doesn't expose its window color through AX.
///   • An NSEvent monitor can't see keys delivered to other apps.
///   • CGEventTap in `.listenOnly` mode observes the system event
///     stream without consuming events, so the text entry into the
///     sticky stays exactly as it was.
///
/// Why sample a window pixel for color:
///   • Stickies stores the color enum in the .rtfd package's
///     metadata, but mapping a window → on-disk file is brittle.
///   • A single pixel from the window's title bar is a reliable
///     hue probe; classifier groups RGB by hue family (yellow,
///     blue, pink, green, purple, gray).
final class StickiesBridge {
    /// Stickies' six built-in note colors. Each maps a sticky's
    /// visible background hue to a stable identifier the user can
    /// pick in settings — the trigger color is selected from this
    /// set. Hue thresholds are tuned against light-mode Stickies
    /// on macOS Sequoia; dark-mode Stickies darkens but preserves
    /// hue, so the same family detector works.
    enum ColorFamily: String, CaseIterable {
        case yellow, blue, pink, green, purple, gray

        fileprivate static func detect(rgb: (UInt8, UInt8, UInt8)) -> ColorFamily? {
            let r = CGFloat(rgb.0) / 255
            let g = CGFloat(rgb.1) / 255
            let b = CGFloat(rgb.2) / 255
            let hsb = NSColor(red: r, green: g, blue: b, alpha: 1)
                .usingColorSpace(.deviceRGB)
            guard let hsb = hsb else { return nil }
            var h: CGFloat = 0, s: CGFloat = 0, v: CGFloat = 0, a: CGFloat = 0
            hsb.getHue(&h, saturation: &s, brightness: &v, alpha: &a)
            // Gray: low saturation, mid+ brightness.
            if s < 0.10 && v > 0.35 { return .gray }
            // Below-saturation pixels (likely UI chrome, not sticky
            // paper) are unclassified. The sticky body itself is
            // always a clear pastel.
            if s < 0.12 { return nil }
            let hueDegrees = h * 360
            switch hueDegrees {
            case 40..<70:   return .yellow
            case 70..<165:  return .green
            case 165..<240: return .blue
            case 240..<300: return .purple
            case 300..<360, 0..<40: return .pink
            default: return nil
            }
        }
    }

    private weak var menuBand: MenuBandController?
    private var eventTap: CFMachPort?
    private var runLoopSource: CFRunLoopSource?
    /// Keys we've already forwarded a keyDown for, so a long press's
    /// auto-repeat doesn't pile up extra noteOn callbacks before the
    /// matching keyUp arrives.
    private var heldKeys: Set<UInt16> = []
    /// Cached color of the frontmost Stickies window, refreshed
    /// every `colorRecheckInterval` seconds. Avoids sampling the
    /// screen on every keystroke (a few ms each) — color only
    /// changes when the user switches sticky or recolors one.
    private var lastFrontStickyColor: ColorFamily?
    private var lastColorCheckAt: CFTimeInterval = 0
    private static let colorRecheckInterval: CFTimeInterval = 0.4
    /// Cached prefix-match for the focused sticky's TEXT content,
    /// read via AX. Bridges via a prefix instead of color when
    /// the sticky body begins with `triggerPrefix` (case-
    /// insensitive). Refreshed on the same cadence as color.
    private var lastFrontStickyPrefixMatch = false
    /// If the focused sticky's body matches `menuband:N`, this is
    /// the GM program (0-based, derived from 1-based N). nil when
    /// the sticky just says `menuband` with no instrument suffix
    /// (use whatever instrument is currently active). Cached so
    /// we only call `setMelodicProgram` when the value changes —
    /// avoids re-attacking held notes on every recheck.
    private var lastFrontStickyProgram: UInt8?
    private var lastAppliedProgram: UInt8?
    /// Short, no-separator prefix: `mb` followed directly by the
    /// 1-based GM program number (`mb1` … `mb128`). Plain `mb`
    /// with no digits is also accepted and leaves the instrument
    /// at whatever the user has currently selected.
    private static let triggerPrefix = "mb"

    /// The color a sticky must be for typing in it to play notes.
    /// Persisted across launches so once set it stays set. Default
    /// is GRAY — least likely color to be used for normal note-
    /// taking, so accidental triggers from a casual yellow sticky
    /// don't get hijacked.
    var triggerColor: ColorFamily {
        get {
            let raw = UserDefaults.standard.string(forKey: "stickies.triggerColor")
                ?? ColorFamily.gray.rawValue
            return ColorFamily(rawValue: raw) ?? .gray
        }
        set {
            UserDefaults.standard.set(newValue.rawValue,
                                      forKey: "stickies.triggerColor")
        }
    }

    /// Whether the bridge is actively listening. Persisted so the
    /// user's choice survives a relaunch. Defaults to ON — the
    /// feature is opt-out, not opt-in, since it requires no user
    /// configuration beyond granting Accessibility permission.
    var isEnabled: Bool {
        get {
            if UserDefaults.standard.object(forKey: "stickies.bridgeEnabled") == nil {
                return true
            }
            return UserDefaults.standard.bool(forKey: "stickies.bridgeEnabled")
        }
        set {
            UserDefaults.standard.set(newValue, forKey: "stickies.bridgeEnabled")
            if newValue { start() } else { stop() }
        }
    }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
    }

    /// Begin listening for system keyboard events. No-op if already
    /// running. Silently fails (and logs) if Accessibility permission
    /// hasn't been granted — the user must grant it in System
    /// Settings → Privacy & Security → Accessibility for the event
    /// tap to succeed.
    ///
    /// Tap is `.defaultTap` (NOT listenOnly) so the bridge can
    /// SWALLOW the system's keyboard auto-repeat events when the
    /// trigger color is active. Without that, holding a key to
    /// sustain a note would also keep typing the same character
    /// into the sticky ("aaaaaa…"). The very first keyDown still
    /// passes through (so the user types one character per press)
    /// and keyUp always passes through; only auto-repeat keyDowns
    /// during a matching gesture are intercepted.
    func start() {
        guard eventTap == nil else { return }
        let mask = (1 << CGEventType.keyDown.rawValue)
            | (1 << CGEventType.keyUp.rawValue)
        let userInfo = Unmanaged.passUnretained(self).toOpaque()
        guard let tap = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .defaultTap,
            eventsOfInterest: CGEventMask(mask),
            callback: { _, type, event, refcon in
                guard let refcon = refcon else {
                    return Unmanaged.passUnretained(event)
                }
                let bridge = Unmanaged<StickiesBridge>
                    .fromOpaque(refcon).takeUnretainedValue()
                if bridge.handle(event: event, type: type) {
                    // Swallow — auto-repeat during a matching
                    // sticky session would spam characters.
                    return nil
                }
                return Unmanaged.passUnretained(event)
            },
            userInfo: userInfo
        ) else {
            NSLog("StickiesBridge: failed to create event tap — Accessibility permission missing?")
            return
        }
        eventTap = tap
        let source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)
        runLoopSource = source
        CFRunLoopAddSource(CFRunLoopGetMain(), source, .commonModes)
        CGEvent.tapEnable(tap: tap, enable: true)
        NSLog("StickiesBridge: started (trigger=\(triggerColor.rawValue))")
    }

    func stop() {
        if let tap = eventTap {
            CGEvent.tapEnable(tap: tap, enable: false)
        }
        if let source = runLoopSource {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), source, .commonModes)
        }
        eventTap = nil
        runLoopSource = nil
        // Release any keys we still think are held — if the bridge
        // is killed while the user has a key down, no keyUp will
        // arrive to balance the noteOn.
        releaseAllHeldKeys()
    }

    private func releaseAllHeldKeys() {
        let stuck = heldKeys
        heldKeys.removeAll()
        guard !stuck.isEmpty, let menuBand = menuBand else { return }
        DispatchQueue.main.async {
            for kc in stuck {
                _ = menuBand.handleLocalKey(
                    keyCode: kc, isDown: false,
                    isRepeat: false, flags: [])
            }
        }
    }

    deinit { stop() }

    /// Hardware key codes for editing / navigation keys that
    /// should keep their normal auto-repeat behavior even in a
    /// trigger-color sticky — the user expects to hold backspace
    /// to delete a run of characters, arrow keys to glide the
    /// cursor, etc. Note-mapped keys (letters / digits / symbols)
    /// fall outside this set and have their repeats suppressed
    /// while the synth voice sustains.
    private static let nonNoteEditingKeycodes: Set<UInt16> = [
        51,   // delete (backspace)
        117,  // forward delete
        123, 124, 125, 126, // arrow keys (←↑↓→ — kVK_LeftArrow…RightArrow)
        53,   // escape
        48,   // tab
        36,   // return
        76,   // numpad enter
    ]

    /// Handle one event. Returns `true` when the event should be
    /// SWALLOWED (so Stickies doesn't see it) — used for auto-
    /// repeat suppression while a sustained note is held.
    private func handle(event: CGEvent, type: CGEventType) -> Bool {
        guard let frontBundle = NSWorkspace.shared
            .frontmostApplication?.bundleIdentifier,
              frontBundle == "com.apple.Stickies" else {
            if !heldKeys.isEmpty { releaseAllHeldKeys() }
            return false
        }
        // Color cache refresh stays on its 400ms TTL — the sample
        // is a screen pixel read, too expensive to do per
        // keystroke. The inline parse below reads AX text +
        // cursor on every event, which is cheap (a few
        // microseconds) and lets `mb<N>` switches mid-line take
        // effect on the very next note.
        let now = CACurrentMediaTime()
        if now - lastColorCheckAt > Self.colorRecheckInterval {
            recheckFocusedSticky()
            lastColorCheckAt = now
        }

        // Inline parse — read text + cursor from AX and look at
        // the most recent `mb<N>` token before the cursor.
        let state = readFocusedStickyState()
        let bodyText = state.text ?? ""
        let cursor = state.cursor
        let ctx = inlineContext(for: bodyText, cursorUTF16: cursor)

        // Engagement rules:
        //   • PREFIX MODE (any `mbN` in the body) — engage iff
        //     the cursor isn't inside a still-being-typed mb
        //     token AND at least one completed mb token has
        //     appeared. Lets the user type `mb10 note-keys`
        //     without the "1" / "0" leaking through.
        //   • COLOR MODE (no mb prefix in body) — engage when
        //     the sticky's background color matches the user's
        //     configured trigger color.
        let bodyHasMB = bodyText.lowercased().contains("mb")
        let armed: Bool
        if bodyHasMB {
            armed = ctx.armed
        } else {
            armed = lastFrontStickyColor == triggerColor
        }
        guard armed else { return false }

        // Apply the inline instrument switch. lastAppliedProgram
        // memoizes the value so we only re-trigger
        // setMelodicProgram on actual changes (which gracefully
        // morphs held notes onto the new patch).
        if let prog = ctx.program,
           prog != lastAppliedProgram {
            lastAppliedProgram = prog
            DispatchQueue.main.async { [weak menuBand] in
                menuBand?.setMelodicProgram(prog)
            }
        }

        let keyCode = UInt16(event.getIntegerValueField(.keyboardEventKeycode))
        let autorepeat = event.getIntegerValueField(.keyboardEventAutorepeat) != 0
        let modifierFlags = nsModifierFlags(from: event.flags)

        // Editing keys (backspace, arrows, etc) skip the bridge
        // entirely — they aren't note triggers and the user
        // expects their normal auto-repeat behavior (hold
        // backspace to delete a run of characters).
        if Self.nonNoteEditingKeycodes.contains(keyCode) { return false }

        switch type {
        case .keyDown:
            if autorepeat {
                // The user is holding a note-mapped key for
                // sustain — swallow the auto-repeat so Stickies
                // doesn't get a stream of duplicate characters
                // typed into the note.
                return heldKeys.contains(keyCode)
            }
            if heldKeys.contains(keyCode) { return false }
            heldKeys.insert(keyCode)
            forwardKey(keyCode: keyCode, isDown: true,
                       flags: modifierFlags)
            return false
        case .keyUp:
            if heldKeys.remove(keyCode) != nil {
                forwardKey(keyCode: keyCode, isDown: false,
                           flags: modifierFlags)
            }
            return false
        default:
            return false
        }
    }

    /// Translate `CGEventFlags` (low-level Carbon-era bitmask used
    /// by event taps) to `NSEvent.ModifierFlags` (the cocoa enum
    /// MenuBandController's `handleLocalKey` expects). Shift +
    /// caps-lock are what matter — both engage the linger
    /// envelope in Menu Band, so capital letters typed into a
    /// matching sticky play sustained notes.
    private func nsModifierFlags(from cg: CGEventFlags) -> NSEvent.ModifierFlags {
        var out: NSEvent.ModifierFlags = []
        if cg.contains(.maskShift) { out.insert(.shift) }
        if cg.contains(.maskAlphaShift) { out.insert(.capsLock) }
        if cg.contains(.maskCommand) { out.insert(.command) }
        if cg.contains(.maskControl) { out.insert(.control) }
        if cg.contains(.maskAlternate) { out.insert(.option) }
        return out
    }

    private func forwardKey(keyCode: UInt16, isDown: Bool,
                            flags: NSEvent.ModifierFlags) {
        guard let menuBand = menuBand else { return }
        DispatchQueue.main.async {
            _ = menuBand.handleLocalKey(
                keyCode: keyCode, isDown: isDown,
                isRepeat: false, flags: flags)
        }
    }

    /// Re-poll the focused sticky's BACKGROUND COLOR. Cheap
    /// only when the cache TTL hasn't expired — the underlying
    /// screen sample takes a few ms, so it doesn't fire on
    /// every keystroke. Prefix parsing is its own per-keystroke
    /// path (see `inlineContext(for:cursor:)`) and doesn't
    /// touch the screen.
    private func recheckFocusedSticky() {
        lastFrontStickyColor = sampleFrontStickyColor()
    }

    /// Inline parse result for the focused sticky's body at the
    /// cursor: which instrument (if any) is currently selected
    /// by the most recent `mbN` token to the LEFT of the cursor,
    /// and whether the keystroke about to land is part of a
    /// config token (and therefore should NOT play a note).
    private struct InlineContext {
        /// `true` when the keystroke should reach the synth.
        let armed: Bool
        /// `true` when the cursor is inside a still-being-typed
        /// `mb…` token; the keystroke goes into the sticky as
        /// plain text and is NOT forwarded.
        let inConfig: Bool
        /// GM program (0-based) selected by the most recent
        /// completed `mbN` token before the cursor. nil if no
        /// completed token has appeared yet.
        let program: UInt8?
    }

    /// Compute the inline context — which instrument is active,
    /// whether the cursor is in a config token, and whether the
    /// bridge should engage — given the sticky's text body and
    /// the cursor's UTF-16 position. Driven by `mb<N>` tokens
    /// inline in the body, so a sticky like
    ///   `mb5 hhg mb6 hhih`
    /// plays the first chunk in instrument 5 and the second in
    /// instrument 6.
    private func inlineContext(for text: String,
                                cursorUTF16: Int) -> InlineContext {
        let lower = text.lowercased()
        let utf16 = lower.utf16
        let cursorOffset = max(0, min(cursorUTF16, utf16.count))
        guard let cursorIdx = utf16.index(utf16.startIndex,
                                          offsetBy: cursorOffset,
                                          limitedBy: utf16.endIndex),
              let cursorStr = String.Index(cursorIdx, within: lower)
        else { return InlineContext(armed: false, inConfig: false, program: nil) }
        let head = lower[..<cursorStr]

        // Walk backwards from cursor to find the start of the
        // current word (last whitespace before cursor or start).
        var tokenStart = head.endIndex
        while tokenStart > head.startIndex {
            let prev = head.index(before: tokenStart)
            if head[prev].isWhitespace { break }
            tokenStart = prev
        }
        let currentToken = head[tokenStart..<head.endIndex]
        let completedHead = head[..<tokenStart]

        // Is the cursor in a still-being-typed `mb…` token?
        // We say yes when the token starts with `mb` followed by
        // zero-or-more digits — the user could be in any state
        // along the way from `m` → `mb` → `mb1` → `mb10`.
        let inConfig: Bool
        if currentToken.hasPrefix("mb") {
            let rest = currentToken.dropFirst(2)
            inConfig = rest.allSatisfy { $0.isNumber }
        } else if currentToken == "m" {
            // Catches the in-between state right after pressing
            // `m` but before `b` lands — without this the `m`
            // would forward as a note before the user finishes
            // typing the config token.
            inConfig = true
        } else {
            inConfig = false
        }

        // Scan completed tokens for the LAST `mb<digits>` — that's
        // the active instrument for any subsequent note input.
        var program: UInt8?
        var sawAnyMB = false
        for token in completedHead.split(whereSeparator: { $0.isWhitespace }) {
            if let prog = Self.parseMBToken(token) {
                program = prog
                sawAnyMB = true
            }
        }

        // Armed = there's at least one completed `mbN` before
        // the cursor, and the cursor is NOT inside a fresh
        // config token. The first `mbN` arms the bridge; every
        // subsequent `mbN` switches instruments mid-stream.
        let armed = sawAnyMB && !inConfig
        return InlineContext(armed: armed,
                             inConfig: inConfig,
                             program: program)
    }

    /// `mbN` token parser — returns the 0-based GM program for
    /// `mb1`…`mb128`, nil for any other token. Tokens that have
    /// non-digit characters after `mb` (like `mbox`) or whose
    /// number is out of the GM range fall through.
    private static func parseMBToken(_ token: Substring) -> UInt8? {
        guard token.hasPrefix("mb") else { return nil }
        let rest = token.dropFirst(2)
        guard !rest.isEmpty, rest.allSatisfy({ $0.isNumber }) else { return nil }
        guard let n = Int(rest), n >= 1, n <= 128 else { return nil }
        return UInt8(n - 1)
    }

    /// Read the focused sticky's body text + the cursor's UTF-16
    /// offset within it. Used by the inline-mb parser to decide
    /// which instrument is active at the current cursor position
    /// and whether the cursor is inside an in-progress config
    /// token. Returns nil for both when Stickies isn't focused
    /// or the AX query fails.
    private func readFocusedStickyState()
        -> (text: String?, cursor: Int) {
        guard let stickies = NSWorkspace.shared.runningApplications.first(
            where: { $0.bundleIdentifier == "com.apple.Stickies" }
        ) else { return (nil, 0) }
        let app = AXUIElementCreateApplication(stickies.processIdentifier)
        var focusedRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(
            app, kAXFocusedUIElementAttribute as CFString,
            &focusedRef) == .success,
              let focused = focusedRef else { return (nil, 0) }
        let element = focused as! AXUIElement
        var valueRef: CFTypeRef?
        var rangeRef: CFTypeRef?
        _ = AXUIElementCopyAttributeValue(
            element, kAXValueAttribute as CFString, &valueRef)
        _ = AXUIElementCopyAttributeValue(
            element, kAXSelectedTextRangeAttribute as CFString,
            &rangeRef)
        let text = valueRef as? String
        var range = CFRange()
        if let r = rangeRef {
            AXValueGetValue(r as! AXValue, .cfRange, &range)
        }
        return (text, range.location)
    }

    /// Read the focused element's text in the Stickies process
    /// via Accessibility. Returns nil if Stickies isn't running,
    /// there's no focused element, or the focused element has no
    /// readable string value (e.g. the user clicked the title bar
    /// instead of the body).
    private func readFocusedStickyText() -> String? {
        guard let stickies = NSWorkspace.shared.runningApplications.first(
            where: { $0.bundleIdentifier == "com.apple.Stickies" }
        ) else { return nil }
        let app = AXUIElementCreateApplication(stickies.processIdentifier)
        var focusedRef: CFTypeRef?
        let r1 = AXUIElementCopyAttributeValue(
            app, kAXFocusedUIElementAttribute as CFString, &focusedRef)
        guard r1 == .success, let focused = focusedRef else { return nil }
        let element = focused as! AXUIElement
        // Try kAXValueAttribute first (TextFields / TextAreas) —
        // if the focused element is the title bar or a button,
        // we'll get a non-string value and fall through to nil.
        var valueRef: CFTypeRef?
        let r2 = AXUIElementCopyAttributeValue(
            element, kAXValueAttribute as CFString, &valueRef)
        if r2 == .success, let s = valueRef as? String { return s }
        // Fallback: walk up to the focused window and try its
        // first descendant text content. Stickies' AX tree puts
        // the body in an AXTextArea under the window root.
        var windowRef: CFTypeRef?
        let r3 = AXUIElementCopyAttributeValue(
            app, kAXFocusedWindowAttribute as CFString, &windowRef)
        guard r3 == .success, let win = windowRef else { return nil }
        return firstTextAreaValue(in: win as! AXUIElement)
    }

    /// Depth-first search a Stickies window's AX subtree for the
    /// first element whose kAXValueAttribute resolves to a
    /// String. Bounded depth so a pathological tree can't hang.
    private func firstTextAreaValue(in element: AXUIElement,
                                     depth: Int = 0) -> String? {
        if depth > 6 { return nil }
        var valueRef: CFTypeRef?
        if AXUIElementCopyAttributeValue(
            element, kAXValueAttribute as CFString, &valueRef) == .success,
           let s = valueRef as? String, !s.isEmpty {
            return s
        }
        var childrenRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(
            element, kAXChildrenAttribute as CFString, &childrenRef) == .success,
              let children = childrenRef as? [AXUIElement] else { return nil }
        for child in children {
            if let s = firstTextAreaValue(in: child, depth: depth + 1) {
                return s
            }
        }
        return nil
    }

    /// Parse the `mbN\n` config line off the head of a sticky's
    /// text body. Returns:
    ///   • `hasPrefix` — the body starts with `mb` followed by
    ///     digits (or by whitespace / newline). When true the
    ///     sticky is in PREFIX MODE, which suppresses the COLOR
    ///     trigger path so the user can edit `mb` and the number
    ///     without their keystrokes leaking through to Menu Band.
    ///   • `armed`     — the config line is COMPLETE: terminated
    ///     by a newline. Only when armed does the bridge actually
    ///     forward keystrokes. Without the newline gate, typing
    ///     the "0" in `mb10` would forward through and (digit
    ///     keys toggle MIDI mode in Menu Band's keymap) flip the
    ///     app into MIDI mode mid-edit.
    ///   • `program`   — parsed GM program (0-based, from 1-based
    ///     N). nil for plain `mb` with no instrument number.
    private func parseTriggerPrefix(_ text: String)
        -> (hasPrefix: Bool, armed: Bool, program: UInt8?) {
        let lower = text.lowercased()
        // Allow only leading horizontal whitespace before the
        // prefix — the config line has to be the first line of
        // the sticky body, not buried mid-paragraph.
        let head = lower.drop { $0 == " " || $0 == "\t" }
        guard head.hasPrefix(Self.triggerPrefix) else {
            return (false, false, nil)
        }
        var after = head.dropFirst(Self.triggerPrefix.count)
        var program: UInt8?
        let numPart = after.prefix { $0.isNumber }
        if !numPart.isEmpty {
            if let n = Int(numPart), n >= 1, n <= 128 {
                program = UInt8(n - 1)
            }
            after = after.dropFirst(numPart.count)
        } else if let first = after.first,
                  !first.isWhitespace && !first.isNewline {
            // `mbX…` where X isn't a digit and isn't whitespace —
            // not a real prefix line (could be a word like "mbox").
            return (false, false, nil)
        }
        // Armed once the config line is closed with a newline.
        // Optional trailing horizontal whitespace tolerated.
        let postConfig = after.drop { $0 == " " || $0 == "\t" }
        let armed = postConfig.first == "\n" || postConfig.first == "\r"
        return (true, armed, program)
    }

    /// Find the frontmost Stickies window and classify its
    /// background hue. Returns nil if Stickies has no windows or
    /// the sample point can't be read.
    private func sampleFrontStickyColor() -> ColorFamily? {
        guard let infos = CGWindowListCopyWindowInfo(
            [.optionOnScreenOnly, .excludeDesktopElements],
            kCGNullWindowID
        ) as? [[String: Any]] else { return nil }
        // First Stickies window in the on-screen list is z-order
        // top — that's the one the user is typing into.
        guard let info = infos.first(where: {
            ($0[kCGWindowOwnerName as String] as? String) == "Stickies"
        }) else { return nil }
        guard let boundsDict = info[kCGWindowBounds as String]
                as? [String: CGFloat],
              let w = boundsDict["Width"],
              let h = boundsDict["Height"],
              w > 20, h > 20,
              let windowID = info[kCGWindowNumber as String] as? CGWindowID
        else { return nil }
        // Crop to a 2×2 region near the top-center of the sticky.
        // The title bar is the cleanest color sample — body text
        // ink can pollute the average otherwise.
        let cropRect = CGRect(
            x: (boundsDict["X"] ?? 0) + w / 2 - 1,
            y: (boundsDict["Y"] ?? 0) + 6,
            width: 2, height: 2
        )
        guard let cgImage = CGWindowListCreateImage(
            cropRect,
            .optionIncludingWindow,
            windowID,
            .nominalResolution
        ) else { return nil }
        guard let data = cgImage.dataProvider?.data,
              let bytes = CFDataGetBytePtr(data) else { return nil }
        let bpr = cgImage.bytesPerRow
        let bpp = cgImage.bitsPerPixel / 8
        // CGImage from CGWindowListCreateImage is BGRA on Apple
        // Silicon; pick from the first pixel.
        let idx = 0
        let b = bytes[idx]
        let g = bytes[idx + 1]
        let r = bytes[idx + 2]
        _ = (bpr, bpp)
        return ColorFamily.detect(rgb: (r, g, b))
    }
}
