// WizardController.swift — the window: a roll to drag, a transport, and
// the two ways out (save the numbers, or render them for real).
import AppKit

final class WizardController: NSWindowController, RollViewDelegate {
    private let model: ChartModel
    private let engine = WarpEngine()
    private let roll = RollView()
    private let status = NSTextField(labelWithString: "")
    private let phrasePicker = NSPopUpButton()
    private var buffer: AVAudioPCMBufferBox?
    private var ticker: Timer?
    private var renderTask: Process?

    init(model: ChartModel) {
        self.model = model
        let win = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1180, height: 460),
                           styleMask: [.titled, .closable, .miniaturizable, .resizable],
                           backing: .buffered, defer: false)
        win.title = "ChartWizard — \(model.doc.lane) · \(model.name)"
        win.center()
        super.init(window: win)
        build()
        loadPhrase()
    }
    required init?(coder: NSCoder) { fatalError() }

    // ── layout ───────────────────────────────────────────────────────
    private func build() {
        guard let win = window else { return }
        let root = NSView(frame: win.contentLayoutRect)
        root.autoresizingMask = [.width, .height]

        let scroll = NSScrollView()
        scroll.hasHorizontalScroller = true
        scroll.hasVerticalScroller = false
        scroll.drawsBackground = false
        roll.delegate = self
        roll.frame = NSRect(x: 0, y: 0, width: 3000, height: 380)
        scroll.documentView = roll
        scroll.translatesAutoresizingMaskIntoConstraints = false
        root.addSubview(scroll)

        let bar = NSStackView()
        bar.orientation = .horizontal
        bar.spacing = 8
        bar.translatesAutoresizingMaskIntoConstraints = false

        for (title, sel) in [("▶ Play", #selector(playAll)), ("■ Stop", #selector(stopAll)),
                             ("Revert", #selector(revert)), ("Save", #selector(save)),
                             ("Render", #selector(render))] {
            let b = NSButton(title: title, target: self, action: sel)
            b.bezelStyle = .rounded
            bar.addArrangedSubview(b)
        }
        phrasePicker.addItems(withTitles: model.doc.phrases.keys.sorted())
        phrasePicker.selectItem(withTitle: model.name)
        phrasePicker.target = self
        phrasePicker.action = #selector(pickPhrase)
        bar.addArrangedSubview(phrasePicker)
        status.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
        status.textColor = .secondaryLabelColor
        bar.addArrangedSubview(status)
        root.addSubview(bar)

        NSLayoutConstraint.activate([
            bar.leadingAnchor.constraint(equalTo: root.leadingAnchor, constant: 12),
            bar.trailingAnchor.constraint(lessThanOrEqualTo: root.trailingAnchor, constant: -12),
            bar.topAnchor.constraint(equalTo: root.topAnchor, constant: 10),
            scroll.leadingAnchor.constraint(equalTo: root.leadingAnchor),
            scroll.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            scroll.topAnchor.constraint(equalTo: bar.bottomAnchor, constant: 10),
            scroll.bottomAnchor.constraint(equalTo: root.bottomAnchor),
        ])
        win.contentView = root
    }

    private func loadPhrase() {
        roll.model = model
        try? engine.load(wav: URL(fileURLWithPath: model.phrase.wav))
        roll.invalidateIntrinsicContentSize()
        roll.frame.size = roll.intrinsicContentSize
        window?.title = "ChartWizard — \(model.doc.lane) · \(model.name)"
        window?.makeFirstResponder(roll)
        say("\(model.units.count) words · \(model.phrase.beats.clean) beats @ \(model.bpm.clean) BPM")
    }

    private func say(_ s: String) { status.stringValue = s }

    // ── transport ────────────────────────────────────────────────────
    @objc private func playAll() { play(from: 0) }

    private func play(from beat: Double) {
        guard let buf = engine.assemble(units: model.units,
                                        spb: model.secondsPerBeat,
                                        leadIn: model.phrase.leadIn,
                                        totalBeats: model.phrase.beats) else { return }
        engine.play(buf, fromBeat: beat, spb: model.secondsPerBeat)
        let base = beat * model.secondsPerBeat
        ticker?.invalidate()
        ticker = Timer.scheduledTimer(withTimeInterval: 1.0 / 30, repeats: true) { [weak self] t in
            guard let self else { return t.invalidate() }
            guard self.engine.isPlaying else {
                self.roll.playhead = nil; t.invalidate(); return
            }
            self.roll.playhead = base + self.engine.elapsed
        }
    }

    @objc private func stopAll() {
        engine.stop()
        ticker?.invalidate()
        roll.playhead = nil
    }

    // ── the two ways out ─────────────────────────────────────────────
    @objc private func revert() {
        model.revert()
        roll.needsDisplay = true
        say("reverted to the built chart")
    }

    @objc private func save() {
        do {
            try model.save()
            say("saved → \(model.editsURL.lastPathComponent)")
        } catch { say("save failed: \(error.localizedDescription)") }
    }

    /// Save, then hand it to the real renderer. study.sh rebuilds the bank
    /// through WORLD, re-scores the C engine and redraws the timeline —
    /// what the preview approximates, done properly.
    @objc private func render() {
        save()
        guard renderTask == nil else { say("already rendering…"); return }
        let script = model.laneDir.appendingPathComponent("bin/study.sh")
        guard FileManager.default.fileExists(atPath: script.path) else {
            say("no bin/study.sh in \(model.laneDir.lastPathComponent)"); return
        }
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/bin/bash")
        p.arguments = [script.path, model.name]
        p.terminationHandler = { [weak self] proc in
            DispatchQueue.main.async {
                self?.renderTask = nil
                self?.say(proc.terminationStatus == 0
                          ? "rendered — study.sh rebuilt the bank and the video"
                          : "study.sh failed (\(proc.terminationStatus))")
            }
        }
        do { try p.run(); renderTask = p; say("rendering through WORLD…") }
        catch { say("could not run study.sh: \(error.localizedDescription)") }
    }

    @objc private func pickPhrase() {
        guard let t = phrasePicker.titleOfSelectedItem else { return }
        stopAll()
        model.select(phrase: t)
        loadPhrase()
        roll.needsDisplay = true
    }

    // ── RollViewDelegate ─────────────────────────────────────────────
    func rollDidEdit(_ view: RollView) {
        say(model.dirty ? "edited — Save writes chart-edits.json" : "")
    }

    func rollDidSelect(_ view: RollView, unit: Int?) {
        guard let i = unit else { return say("") }
        let u = model.units[i]
        let owner: String
        switch u.cut {
        case nil: owner = u.pin.map { "times[\($0)]" } ?? "—"
        case .syllable(let k): owner = u.pin.map { "sylls[\($0)][\(k - 1)]" } ?? "—"
        case .auto: owner = "halo3 finds this edge — not draggable"
        }
        say("\(u.t)  ·  bar \(Int(u.beat / 4)) beat \((u.beat.truncatingRemainder(dividingBy: 4) + 1).clean)"
            + "  ·  \(u.dur.clean) beats  ·  src \(u.src0.clean3)–\(u.src1.clean3)s"
            + "  ·  \(((u.src1 - u.src0) / (u.dur * model.secondsPerBeat)).clean2)×  ·  \(owner)")
    }

    func rollRequestsPlay(_ view: RollView, fromBeat b: Double) { play(from: b) }
}

// AVAudioPCMBuffer isn't Sendable; this box only exists so the controller
// can hold one without fighting the concurrency checker.
final class AVAudioPCMBufferBox { }

private extension Double {
    var clean: String { self == rounded() ? String(Int(self)) : String(format: "%.2f", self) }
    var clean2: String { String(format: "%.2f", self) }
    var clean3: String { String(format: "%.3f", self) }
}
