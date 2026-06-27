import AppKit

// LLMGuideWindow — the "LLMs" panel reachable from the keymap overlay.
//
// Menu Band exposes its whole performance surface as macOS DistributedNotifications
// (see AppDelegate's play / engine.* / say hooks). That means an LLM — Claude,
// say — can DRIVE Menu Band: autoplay melodies, conduct the live engine, run a
// whole composition, and (on the direct-download build) relay parts across
// machines over the peer-to-peer fleet. No keyboard automation, no Accessibility
// — just one posted notification per gesture.
//
// This window hands the user a single copy-paste block: paste it into Claude and
// the model knows the entire protocol. The text is the source of truth and is
// kept in lockstep with the real hook payloads in AppDelegate.handlePlayNotification
// / handleEngineNotification / handleSayNotification.
final class LLMGuideWindowController: NSWindowController, NSWindowDelegate {

    private static var active: LLMGuideWindowController?

    /// Open (or re-focus) the single guide window.
    @discardableResult
    static func show() -> LLMGuideWindowController {
        if let live = active {
            live.window?.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
            return live
        }
        let ctrl = LLMGuideWindowController()
        ctrl.present()
        return ctrl
    }

    init() {
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 560, height: 620),
            styleMask: [.titled, .closable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        window.title = "Menu Band for LLMs"
        window.titlebarAppearsTransparent = true
        window.isMovableByWindowBackground = true
        super.init(window: window)
        window.delegate = self
        buildContent()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) { nil }

    func present() {
        guard let window = window else { return }
        LLMGuideWindowController.active = self
        window.center()
        window.level = NSWindow.Level(rawValue: NSWindow.Level.popUpMenu.rawValue + 1)
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    func windowWillClose(_ notification: Notification) {
        if LLMGuideWindowController.active === self { LLMGuideWindowController.active = nil }
    }

    // MARK: - UI

    private func buildContent() {
        guard let content = window?.contentView else { return }

        let heading = NSTextField(labelWithString: "Play Menu Band with an LLM")
        heading.font = .systemFont(ofSize: 16, weight: .bold)
        heading.translatesAutoresizingMaskIntoConstraints = false

        let blurb = NSTextField(wrappingLabelWithString:
            "Copy the guide below and paste it into Claude (or any LLM that can run "
            + "shell commands on this Mac). It teaches the model Menu Band's whole "
            + "remote-control protocol — autoplay, the live conductible engine, "
            + "speech, and the peer-to-peer fleet — so it can perform and compose "
            + "for you.")
        blurb.font = .systemFont(ofSize: 11)
        blurb.textColor = .secondaryLabelColor
        blurb.translatesAutoresizingMaskIntoConstraints = false

        // Monospaced, selectable, scrollable guide text.
        let textView = NSTextView()
        textView.string = Self.guide
        textView.isEditable = false
        textView.isSelectable = true
        textView.drawsBackground = true
        textView.backgroundColor = .textBackgroundColor
        textView.font = .monospacedSystemFont(ofSize: 10.5, weight: .regular)
        textView.textContainerInset = NSSize(width: 10, height: 10)

        let scroll = NSScrollView()
        scroll.documentView = textView
        scroll.hasVerticalScroller = true
        scroll.borderType = .bezelBorder
        scroll.translatesAutoresizingMaskIntoConstraints = false

        let copyButton = NSButton(title: "Copy guide", target: self,
                                  action: #selector(copyGuide(_:)))
        copyButton.bezelStyle = .rounded
        copyButton.keyEquivalent = "c"
        copyButton.keyEquivalentModifierMask = [.command, .shift]
        copyButton.translatesAutoresizingMaskIntoConstraints = false

        copiedLabel.font = .systemFont(ofSize: 11, weight: .semibold)
        copiedLabel.textColor = .systemGreen
        copiedLabel.isHidden = true
        copiedLabel.translatesAutoresizingMaskIntoConstraints = false

        for v in [heading, blurb, scroll, copyButton, copiedLabel] { content.addSubview(v) }
        let m: CGFloat = 18
        NSLayoutConstraint.activate([
            heading.topAnchor.constraint(equalTo: content.topAnchor, constant: 30),
            heading.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: m),

            blurb.topAnchor.constraint(equalTo: heading.bottomAnchor, constant: 6),
            blurb.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: m),
            blurb.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -m),

            scroll.topAnchor.constraint(equalTo: blurb.bottomAnchor, constant: 12),
            scroll.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: m),
            scroll.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -m),
            scroll.bottomAnchor.constraint(equalTo: copyButton.topAnchor, constant: -12),

            copyButton.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: m),
            copyButton.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -m),
            copiedLabel.centerYAnchor.constraint(equalTo: copyButton.centerYAnchor),
            copiedLabel.leadingAnchor.constraint(equalTo: copyButton.trailingAnchor, constant: 12),
        ])
    }

    private let copiedLabel = NSTextField(labelWithString: "Copied — paste into Claude")

    @objc private func copyGuide(_ sender: Any?) {
        let pb = NSPasteboard.general
        pb.clearContents()
        pb.setString(Self.guide, forType: .string)
        copiedLabel.isHidden = false
    }

    // MARK: - The guide (copy-paste payload)

    /// The full protocol, written for an LLM. Derived 1:1 from the hook payloads
    /// in AppDelegate; keep them in sync when the hooks change.
    static let guide = #"""
    # Driving Menu Band (macOS menu-bar instrument) from an LLM

    Menu Band turns posted macOS DistributedNotifications into music: it plays
    notes, runs a live conductible engine, speaks, and can relay parts to other
    Macs over a peer-to-peer fleet. You drive it by POSTING notifications. No
    keyboard automation, no Accessibility — one post per gesture.

    ## The poster (run once)

    Write this tiny helper, then call it for every gesture:

        cat > /tmp/mbpost.swift <<'EOF'
        import Foundation
        let env = ProcessInfo.processInfo.environment
        var ui: [String: String] = [:]
        if let kv = env["MB_KV"] {
          for pair in kv.split(separator: ";") {
            let p = pair.split(separator: "=", maxSplits: 1)
            if p.count == 2 { ui[String(p[0])] = String(p[1]) }
          }
        }
        DistributedNotificationCenter.default().postNotificationName(
          NSNotification.Name(env["MB_NAME"]!), object: nil,
          userInfo: ui.isEmpty ? nil : ui, deliverImmediately: true)
        RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.06))
        EOF
        swiftc -O /tmp/mbpost.swift -o /tmp/mbpost   # ~once; posts in ~20ms after

    Post a gesture with two env vars — MB_NAME (the hook) and MB_KV
    (semicolon-separated key=value userInfo):

        MB_NAME=computer.aestheticcomputer.menuband.play \
        MB_KV='program=78;notes=67:1,72:1,76:2,r:1' /tmp/mbpost

    ## Note syntax

    A track is comma-separated `token:beats`. Token is a MIDI note number
    (60 = middle C) or a drum letter; `r` = rest. Example: `67:1,72:1,76:2,r:1`.
    Drum letters: k kick · s snare · h closed-hat · ho open-hat · c clap ·
    rd ride · cr crash.

    ## Hooks

    ### …menuband.play — autoplay a melody (one-shot)
    Keys (all optional): program (GM 0–127, default 78 Whistle) · bpm (132) ·
    velocity (100) · notes (the melody) · notes2/notes3/notes4 (up to 3 more
    parallel tracks sharing the start — e.g. a drum kit under a tune) ·
    velocity2/3/4 · startEpoch (UTC seconds to begin at; for syncing machines).
    A bare post (no notes) plays a default whistle refrain.

        # melody on Flute (73), 120 bpm
        MB_NAME=…menuband.play MB_KV='program=73;bpm=120;notes=72:1,74:1,76:1,79:2' /tmp/mbpost
        # drum kit: kick+snare on one track, hats on another
        MB_NAME=…menuband.play MB_KV='bpm=120;notes=k:1,s:1,k:1,s:1;notes2=h:0.5,h:0.5,h:0.5,h:0.5' /tmp/mbpost

    ### …menuband.engine.start — a live, conductible loop (runs until stopped)
    Keys: bpm (110) · chord ("60,64,67") · program (89) · arp ("0,1,2,1" =
    chord-tone indices, -1 = rest) · step (beats per arp step, 0.5) ·
    drums ("k,h,s,h" per step, blank = rest).

        MB_NAME=…menuband.engine.start MB_KV='bpm=100;chord=60,64,67;arp=0,1,2,1;drums=k,h,s,h' /tmp/mbpost

    ### …menuband.engine.chord — morph the held chord (engine keeps playing)
    Keys: chord · glide (seconds, 1.5). The arpeggio follows the new tones.

        MB_NAME=…menuband.engine.chord MB_KV='chord=62,65,69;glide=2' /tmp/mbpost

    ### …menuband.engine.pattern — reshape the loop live
    Any of: arp · drums · bpm · step. Changes without stopping the sound.

    ### …menuband.engine.stop — Keys: fade (seconds, 0.6).
    ### …menuband.engine.listen — mic tempo-follow. Keys: on (0/1) · kick (0/1) · kickvel.

    ### …menuband.say — speak text (AVSpeech)
    Keys: text (required) · voice ("Daniel"/"Samantha"/a BCP-47 lang) · rate
    (0–1) · pitch (0.5–2) · volume (0–1) · startEpoch.

        MB_NAME=…menuband.say MB_KV='text=Two machines, one beat;voice=Samantha' /tmp/mbpost

    ## Composing
    Sequence posts on a clock: hold a chord with engine.start, evolve it with
    engine.chord every few bars, drop melodic fills with play, narrate with say.
    For tight timing across many notes, prefer the engine (it owns the loop)
    over scheduling hundreds of play posts.

    ## The fleet — playing across machines (direct-download build only)
    Each Mac running Menu Band reachable over SSH is a voice. Drive a remote one
    by posting THERE (ssh in and run /tmp/mbpost). To start several together,
    compute a shared startEpoch a second or two in the future and pass the SAME
    value to every machine's post — they lock to it:

        EPOCH=$(python3 -c 'import time;print(time.time()+1.5)')
        MB_NAME=…menuband.play MB_KV="program=73;notes=60:1,64:1,67:2;startEpoch=$EPOCH" /tmp/mbpost
        ssh othermac "MB_NAME=…menuband.play MB_KV='program=42;notes=48:1,52:1,55:2;startEpoch=$EPOCH' /tmp/mbpost"

    Replace `…` with the full prefix `computer.aestheticcomputer.menuband`.
    Menu Band must be running. Have fun — make something that surprises you.
    """#
}
